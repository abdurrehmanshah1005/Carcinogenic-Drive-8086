ORG 0x100
BITS 16

; =============================================================
; MAIN ENTRY POINT
; =============================================================
start:
    ; --- 1. MEMORY MANAGEMENT ---
    mov ah, 0x4A
    mov bx, 0x1000      ; Keep 64KB
    int 0x21

    ; Allocate Back Buffer (64KB)
    mov ah, 0x48
    mov bx, 0x0FA0      
    int 0x21
    jnc .mem_ok
    jmp exit_to_dos

.mem_ok:
    mov [back_buffer_seg], ax

    ; --- 2. VIDEO SETUP ---
    mov ah, 0Fh
    int 10h
    push ax             ; Save old mode

    mov ax, 0x0013      ; Mode 13h (320x200)
    int 10h

    call setup_game_palette

    ; Hide Cursor
    mov ah, 01h
    mov ch, 32
    int 10h

    call reset_game_variables

; =============================================================
; GAME LOOP
; =============================================================
game_loop:
    cmp byte [game_active], 0
    je show_game_over_message

    mov ax, [back_buffer_seg]
    mov es, ax

    ; --- DRAWING ---
    call draw_optimized_background
    call draw_player_car
    
    ; Draw Objects
    cmp word [current_object_type], 1
    je .draw_coin_obj
    cmp word [current_object_type], 2
    je .draw_fuel_obj
    
    call DrawCar        ; Enemy Car
    jmp .ui_draw
    
.draw_coin_obj:
    call DrawCoin
    jmp .ui_draw
.draw_fuel_obj:
    call DrawJerryCan
    
.ui_draw:
    call DrawGameUI

    ; --- LOGIC ---
    call update_game
    call check_collisions   ; Standard Front/Rear collision

    ; --- RENDER ---
    call vsync
    call flip_buffer

    ; =========================================================
    ; INPUT HANDLING (Fixed "Stickiness")
    ; =========================================================
    ; We check the buffer. If keys exist, we process the LAST one
    ; and discard the rest to prevent "lag" movement.
    
    xor bx, bx          ; BX will store the last key pressed
.drain_buffer:
    mov ah, 01h
    int 16h
    jz .process_input   ; Buffer empty? Go process.
    
    mov ah, 00h
    int 16h             ; Get the key
    mov bl, ah          ; Store Scan Code in BL
    mov bh, al          ; Store ASCII in BH
    jmp .drain_buffer   ; Check if there are more keys (Drain it!)

.process_input:
    cmp bl, 0           ; Did we find a key?
    je game_loop        ; No key pressed this frame

    cmp bl, 1           ; ESC Scan Code is usually 1 (or check ASCII 27)
    je pause_game_popup
    cmp bh, 27          ; Check ASCII for ESC
    je pause_game_popup
    
    cmp bl, 0x4B        ; Left Arrow
    je .try_move_left
    cmp bl, 0x4D        ; Right Arrow
    je .try_move_right
    cmp bl, 0x48        ; Up Arrow
    je .move_up
    cmp bl, 0x50        ; Down Arrow
    je .move_down

    jmp game_loop

.try_move_left:
    cmp word [current_lane], 0
    jle game_loop       ; Already at left wall, do nothing
    
    ; Check if turning LEFT causes a crash
    mov dx, [current_lane]
    dec dx              ; Target Lane = Current - 1
    call check_side_collision
    cmp al, 1           ; AL=1 means crash
    je .crash_side
    
    dec word [current_lane]
    call update_player_x_from_lane
    jmp game_loop

.try_move_right:
    cmp word [current_lane], 2
    jge game_loop       ; Already at right wall, do nothing
    
    ; Check if turning RIGHT causes a crash
    mov dx, [current_lane]
    inc dx              ; Target Lane = Current + 1
    call check_side_collision
    cmp al, 1           ; AL=1 means crash
    je .crash_side
    
    inc word [current_lane]
    call update_player_x_from_lane
    jmp game_loop

.crash_side:
    call draw_spark_animation
    mov byte [game_active], 0
    jmp game_loop

.move_up:
    mov ax, [player_y]
    sub ax, [player_vel] 
    cmp ax, 10
    jl game_loop
    mov [player_y], ax
    jmp game_loop

.move_down:
    mov ax, [player_y]
    add ax, [player_vel] 
    cmp ax, 199
    jg game_loop
    mov [player_y], ax
    jmp game_loop

; =============================================================
; LOGIC SUBROUTINES
; =============================================================

; INPUT: DX = Target Lane Index (0, 1, 2)
; OUTPUT: AL = 1 if Collision, 0 if Safe
check_side_collision:
    ; 1. Is there an enemy car? (Type 0)
    cmp word [current_object_type], 0
    jne .safe
    
    ; 2. Is the enemy car in the TARGET lane?
    ; We have to check X coordinates.
    push bx
    mov bx, dx              ; Get Target Lane
    shl bx, 1               ; *2 for word table
    mov ax, [lane_x_table + bx] ; Get X of target lane
    pop bx
    
    cmp ax, [X_START]       ; Does Target X == Enemy X?
    jne .safe               ; If enemy is in a different lane, safe.

    ; 3. Is the enemy alongside us? (Y overlap)
    mov ax, [player_y]
    mov bx, [Y_START]
    
    ; Check if Player Top is below Enemy Bottom
    mov cx, bx
    add cx, car_height
    cmp ax, cx
    jg .safe
    
    ; Check if Player Bottom is above Enemy Top
    add ax, car_height
    cmp ax, bx
    jl .safe
    
    ; If here, we are moving INTO the car
    mov al, 1
    ret

.safe:
    mov al, 0
    ret

check_collisions:
    pusha
    ; Standard Front/Back Collision
    ; 1. Y Overlap
    mov ax, [player_y]
    mov bx, [Y_START]
    mov cx, bx
    add cx, car_height
    cmp ax, cx
    jg .no_col
    add ax, car_height
    cmp ax, bx
    jl .no_col

    ; 2. X Overlap
    mov ax, [player_x]
    mov bx, [X_START]
    mov cx, bx
    add cx, car_width
    cmp ax, cx
    jg .no_col
    add ax, car_width
    cmp ax, bx
    jl .no_col

    ; Collision Detected
    cmp word [current_object_type], 0
    je .crash_front
    cmp word [current_object_type], 1
    je .get_coin
    cmp word [current_object_type], 2
    je .get_fuel
    jmp .no_col

.crash_front:
    call draw_spark_animation
    mov byte [game_active], 0
    jmp .col_done

.get_coin:
    inc word [coins_collected]
    call reset_object_offscreen
    jmp .col_done

.get_fuel:
    add word [fuel_amount], 25   ; REWARD: +25 Fuel (Needs to be high to match decay)
    cmp word [fuel_amount], 100
    jle .fuel_limit
    mov word [fuel_amount], 100  ; Cap at 100%
.fuel_limit:
    call reset_object_offscreen
    jmp .col_done

.col_done:
.no_col:
    popa
    ret

reset_object_offscreen:
    mov word [Y_START], 210
    ret

draw_spark_animation:
    ; EXPLOSION EFFECT: Expands over 30 frames
    mov bx, 0           ; Frame counter
    
.anim_loop:
    mov ax, [back_buffer_seg]
    mov es, ax
    
    ; We draw directly to buffer then flip
    ; Draw 50 random debris pixels per frame
    mov cx, 50
    
.debris_loop:
    push cx
    
    ; Random X offset (-20 to +20)
    call get_random_small
    add ax, [player_x]
    add ax, 20          ; Center of car
    mov cx, ax          ; X coordinate
    
    ; Random Y offset (-20 to +20)
    call get_random_small
    add ax, [player_y]
    add ax, 30          ; Center of car height
    mov dx, ax          ; Y coordinate
    
    ; Color Cycling: Red -> Orange -> Yellow -> Gray
    mov al, 40          ; Red base
    add al, bl          ; Shift color as animation progresses
    and al, 00000111b   ; Keep it in red/orange range (roughly)
    add al, 40          ; Offset to Red palette
    
    call draw_pixel
    pop cx
    loop .debris_loop
    
    call flip_buffer
    call vsync          ; Wait for screen refresh (Timing)
    
    inc bx
    cmp bx, 30          ; Run for 30 frames (0.5 seconds)
    jl .anim_loop
    
    ret

; Helper for randomness (Keep this if you have it, or paste it too)
get_random_small:
    mov ax, [fuel_counter] ; Use a changing variable as seed
    add ax, bx             ; Add loop counter
    add ax, cx             ; Add inner loop counter
    and ax, 63             ; 0-63
    sub ax, 32             ; -32 to +32 range
    ret


update_game:
    mov ax, [npc_speed]
    add [Y_START], ax

    ; --- FUEL DECAY LOGIC ---
    inc word [fuel_counter]
    cmp word [fuel_counter], 10  ; DECAY RATE: Lower = Harder (10 is fast)
    jl .skip_fuel
    
    mov word [fuel_counter], 0   ; Reset timer
    
    cmp word [fuel_amount], 0
    jle .fuel_die
    dec word [fuel_amount]       ; Burn fuel
    jmp .skip_fuel

.fuel_die:
    mov byte [game_active], 0    ; Fuel empty = Game Over

.skip_fuel:
    cmp word [Y_START], 200
    jl .road_upd
    call spawn_next_object

.road_upd:
    mov ax, [road_speed]
    add [road_scroll_y], ax
    ret

spawn_next_object:
    mov word [Y_START], -60
    call randomize_npc_x
    inc word [spawn_counter]
    mov ax, [spawn_counter]
    and ax, 7
    cmp ax, 2
    je .mk_coin
    cmp ax, 5
    je .mk_jerry
    mov word [current_object_type], 0
    ret
.mk_coin:
    mov word [current_object_type], 1
    ret
.mk_jerry:
    mov word [current_object_type], 2
    ret

; =============================================================
; UI DRAWING (FIXED)
; =============================================================
DrawGameUI:
    pusha
    ; Coins Text
    mov cx, 5
    mov dx, 5
    call DrawLabelCoins
    mov ax, [coins_collected]
    mov cx, 5
    mov dx, 12
    call DrawNumber
    
    ; Fuel Text
    mov cx, 260
    mov dx, 5
    call DrawLabelFuel
    
    ; Draw the Fuel Gauge
    call draw_fuel_bar
    popa
    ret

draw_fuel_bar:
    pusha
    ; 1. Draw Border (White Box)
    ; X: 258 to 312 (Width 54), Y: 14 to 20 (Height 6)
    mov al, 15    ; White
    mov di, 258
    mov bp, 14
    mov bx, 54    ; Width
    mov dx, 6     ; Height
    
    ; Top Line
    mov cx, bx
    mov si, di
.top_b:
    push cx
    mov cx, si
    mov dx, bp
    call draw_pixel
    pop cx
    inc si
    loop .top_b
    
    ; Bottom Line
    mov cx, bx
    mov si, di
    mov dx, bp
    add dx, 5
.bot_b:
    push cx
    push dx
    mov cx, si
    call draw_pixel
    pop dx
    pop cx
    inc si
    loop .bot_b
    
    ; Left Line
    mov cx, 6
    mov dx, bp
.left_b:
    push cx
    mov cx, di
    call draw_pixel
    inc dx
    pop cx
    loop .left_b

    ; Right Line
    mov cx, 6
    mov dx, bp
    mov si, di
    add si, 54
.right_b:
    push cx
    mov cx, si
    call draw_pixel
    inc dx
    pop cx
    loop .right_b

    ; 2. Draw Fuel Fill
    mov ax, [fuel_amount]
    cmp ax, 0
    jle .done_bar
    
    ; Divide by 2 to fit in 50 pixel width (since max is 100)
    shr ax, 1 
    mov cx, ax    ; Width of bar
    
    mov si, 260   ; Start X (Inside border)
    mov di, 15    ; Start Y (Inside border)
    
    ; Logic: Color
    mov al, 40    ; Red (Low)
    cmp word [fuel_amount], 30
    jl .color_pick
    mov al, 44    ; Yellow (Medium)
    cmp word [fuel_amount], 60
    jl .color_pick
    mov al, 48    ; Green (High)

.color_pick:
    mov bx, 4     ; Height (4 pixels)
.fill_row:
    push cx
    push si
    
    mov dx, di    ; Current Y
.fill_col:
    push cx
    mov cx, si
    call draw_pixel
    pop cx
    inc si
    loop .fill_col
    
    pop si
    pop cx
    inc di
    dec bx
    jnz .fill_row

.done_bar:
    popa
    ret



; =============================================================
; MENUS
; =============================================================
pause_game_popup:
    call draw_confirmation_box
    call flip_buffer
.p_loop:
    mov ah, 00h
    int 16h
    cmp al, 'y'
    je exit_to_dos
    cmp al, 'n'
    je game_loop
    cmp al, 27
    je game_loop
    jmp .p_loop

show_game_over_message:
    call draw_game_over_box
    call flip_buffer
.go_loop:
    mov ah, 00h
    int 16h
    cmp al, 27
    je exit_to_dos
    cmp al, 13
    je exit_to_dos
    jmp .go_loop

draw_confirmation_box:
    ; Simple Blue Box
    pusha
    mov di, 100 ; X
    mov bp, 80  ; Y
    mov bx, 40  ; Height
.cbox_row:
    mov cx, 120 ; Width
    mov si, di
.cbox_col:
    mov al, 1   ; Blue
    mov dx, bp
    push cx
    mov cx, si
    call draw_pixel
    pop cx
    inc si
    loop .cbox_col
    inc bp
    dec bx
    jnz .cbox_row
    popa
    ret

draw_game_over_box:
    ; Simple Red Box
    pusha
    mov di, 90  ; X
    mov bp, 70  ; Y
    mov bx, 60  ; Height
.gbox_row:
    mov cx, 140 ; Width
    mov si, di
.gbox_col:
    mov al, 40  ; Red
    mov dx, bp
    push cx
    mov cx, si
    call draw_pixel
    pop cx
    inc si
    loop .gbox_col
    inc bp
    dec bx
    jnz .gbox_row
    popa
    ret

exit_to_dos:
    mov ah, 00h
    int 10h
    mov es, [back_buffer_seg]
    mov ah, 0x49
    int 0x21
    mov ax, 0x4C00
    int 0x21

; =============================================================
; HELPERS (Existing ones, ensure these are pasted/kept)
; =============================================================
reset_game_variables:
    call randomize_npc_x
    mov word [Y_START], -60 
    mov word [coins_collected], 0
    mov word [fuel_amount], 100
    mov word [fuel_counter], 0
    mov byte [game_active], 1
    mov word [current_lane], 1
    call update_player_x_from_lane
    mov word [player_y], 140
    ret

update_player_x_from_lane:
    push bx
    mov bx, [current_lane]
    shl bx, 1
    mov ax, [lane_x_table + bx]
    mov [player_x], ax
    pop bx
    ret

randomize_npc_x:
    push es
    pusha
    mov ax, 0040h
    mov es, ax
    mov ax, [es:006Ch]
    xor dx, dx
    mov bx, 3
    div bx
    mov bx, dx
    shl bx, 1
    mov ax, [lane_x_table + bx]
    mov [X_START], ax
    popa
    pop es
    ret

vsync:
    mov dx, 0x3DA
.w1: in al, dx
    test al, 8
    jz .w1
.w2: in al, dx
    test al, 8
    jnz .w2
    ret

flip_buffer:
    push ds
    push es
    push si
    push di
    mov ds, [back_buffer_seg]
    xor si, si
    mov ax, 0xA000
    mov es, ax
    xor di, di
    mov cx, 32000
    rep movsw
    pop di
    pop si
    pop es
    pop ds
    ret

draw_optimized_background:
    pusha
    mov di, 0
    mov dx, 200
.scanline_loop:
    mov al, 120 ; Green
    mov cx, 50
    rep stosb
    mov al, 82  ; Gray
    mov cx, 220
    rep stosb
    mov al, 120 ; Green
    mov cx, 50
    rep stosb
    dec dx
    jnz .scanline_loop
    call draw_fast_barriers
    call draw_lane_dividers
    popa
    ret

draw_fast_barriers:
    pusha
    mov dx, 0
    mov di, 0
.barrier_row:
    mov ax, dx
    sub ax, [road_scroll_y]
    test ax, 32
    jz .is_white
    mov al, 94 ; Red
    jmp .draw_sides
.is_white:
    mov al, 153 ; White
.draw_sides:
    mov [es:di + 40], al
    mov [es:di + 41], al
    mov [es:di + 42], al
    mov [es:di + 43], al
    mov [es:di + 44], al
    mov [es:di + 45], al
    mov [es:di + 46], al
    mov [es:di + 47], al
    mov [es:di + 48], al
    mov [es:di + 49], al
    mov [es:di + 270], al
    mov [es:di + 271], al
    mov [es:di + 272], al
    mov [es:di + 273], al
    mov [es:di + 274], al
    mov [es:di + 275], al
    mov [es:di + 276], al
    mov [es:di + 277], al
    mov [es:di + 278], al
    mov [es:di + 279], al
    add di, 320
    inc dx
    cmp dx, 200
    jl .barrier_row
    popa
    ret

draw_lane_dividers:
    pusha
    mov cx, 123
    call draw_thick_dashed_line
    mov cx, 197
    call draw_thick_dashed_line
    popa
    ret

draw_thick_dashed_line:
    pusha
    mov si, cx
    mov di, 0
.dy:
    mov ax, di
    sub ax, [road_scroll_y]
    test ax, 32
    jnz .skip_pixel
    mov bx, di
    shl bx, 8
    mov dx, di
    shl dx, 6
    add bx, dx
    add bx, si
    mov al, 89
    mov [es:bx], al
    mov [es:bx+1], al
    mov [es:bx+2], al
    mov [es:bx+3], al
    mov [es:bx+4], al
.skip_pixel:
    inc di
    cmp di, 200
    jl .dy
    popa
    ret

draw_pixel:
    push di
    push dx
    cmp cx, 0
    jl .sk
    cmp cx, 320
    jge .sk
    cmp dx, 0
    jl .sk
    cmp dx, 200
    jge .sk
    mov di, dx
    shl di, 8
    shl dx, 6
    add di, dx
    add di, cx
    mov [es:di], al
.sk:
    pop dx
    pop di
    ret

draw_player_car:
    pusha
    mov si, img_reed_car_large
    mov bp, [player_y]
    mov di, 0
.p_row:
    mov bx, [player_x]
    mov cx, 0
.p_col:
    mov al, [si]
    cmp al, 0
    je .p_skip
    push cx
    mov cx, bx
    mov dx, bp
    call draw_pixel
    pop cx
.p_skip:
    inc si
    inc bx
    inc cx
    cmp cx, car_width
    jl .p_col
    inc bp
    inc di
    cmp di, car_height
    jl .p_row
    popa
    ret

DrawCar:
    pusha
    mov si, img_blue_car_large
    mov bp, [Y_START]
    mov di, 0
.n_row:
    mov bx, [X_START]
    mov cx, 0
.n_col:
    mov al, [si]
    cmp al, 0
    je .n_skip
    push cx
    mov cx, bx
    mov dx, bp
    call draw_pixel
    pop cx
.n_skip:
    inc si
    inc bx
    inc cx
    cmp cx, car_width
    jl .n_col
    inc bp
    inc di
    cmp di, car_height
    jl .n_row
    popa
    ret

DrawCoin:
    pusha
    mov si, img_coin
    mov bp, [Y_START]
    mov bx, [X_START]
    add bx, 12
    mov di, 0
.c_row:
    push bx
    mov cx, 0
.c_col:
    mov al, [si]
    cmp al, 0
    je .c_skip
    push cx
    mov cx, bx
    mov dx, bp
    call draw_pixel
    pop cx
.c_skip:
    inc si
    inc bx
    inc cx
    cmp cx, coin_width
    jl .c_col
    pop bx
    inc bp
    inc di
    cmp di, coin_height
    jl .c_row
    popa
    ret

DrawJerryCan:
    pusha
    mov si, img_jerry_can
    mov bp, [Y_START]
    mov bx, [X_START]
    add bx, 9
    mov di, 0
.j_row:
    push bx
    mov cx, 0
.j_col:
    mov al, [si]
    cmp al, 0
    je .j_skip
    push cx
    mov cx, bx
    mov dx, bp
    call draw_pixel
    pop cx
.j_skip:
    inc si
    inc bx
    inc cx
    cmp cx, jerry_can_width
    jl .j_col
    pop bx
    inc bp
    inc di
    cmp di, jerry_can_height
    jl .j_row
    popa
    ret

; Use your existing DrawLabelCoins, DrawLabelFuel, DrawChar..., DrawNumber
; Paste them below if you haven't kept them in a separate file.
; I am assuming you have the text drawing routines from previous code.
; Please ensure DrawLabelCoins/Fuel/Number/CharX are present here.

; Helper: Calculate Screen Offset (ES:DI) from CX(X), DX(Y)
GetScreenOffset:
    mov di, dx
    shl di, 8
    shl dx, 6
    add di, dx
    add di, cx
    ret

DrawLabelCoins:
    push cx
    push dx
    mov al, 153
    call GetScreenOffset
    call DrawCharC
    add di, 6
    call DrawCharO
    add di, 6
    call DrawCharI
    add di, 4
    call DrawCharN
    add di, 6
    call DrawCharS
    pop dx
    pop cx
    ret

DrawLabelFuel:
    push cx
    push dx
    mov al, 153
    call GetScreenOffset
    call DrawCharF
    add di, 6
    call DrawCharU
    add di, 6
    call DrawCharE
    add di, 6
    call DrawCharL
    pop dx
    pop cx
    ret

DrawCharC:
    push di
    mov [es:di+1], al
    mov [es:di+2], al
    add di, 320
    mov [es:di], al
    add di, 320
    mov [es:di], al
    add di, 320
    mov [es:di], al
    add di, 320
    mov [es:di+1], al
    mov [es:di+2], al
    pop di
    ret

DrawCharO:
    push di
    mov [es:di+1], al
    add di, 320
    mov [es:di], al
    mov [es:di+2], al
    add di, 320
    mov [es:di], al
    mov [es:di+2], al
    add di, 320
    mov [es:di], al
    mov [es:di+2], al
    add di, 320
    mov [es:di+1], al
    pop di
    ret

DrawCharI:
    push di
    mov bx, 5
.i_loop:
    mov [es:di], al
    add di, 320
    dec bx
    jnz .i_loop
    pop di
    ret

DrawCharN:
    push di
    mov bx, 5
.n_loop:
    mov [es:di], al
    mov [es:di+3], al
    add di, 320
    dec bx
    jnz .n_loop
    pop di
    push di
    add di, 320
    mov [es:di+1], al
    add di, 320
    mov [es:di+2], al
    pop di
    ret

DrawCharS:
    push di
    mov [es:di], al
    mov [es:di+1], al
    mov [es:di+2], al
    add di, 320
    mov [es:di], al
    add di, 320
    mov [es:di], al
    mov [es:di+1], al
    mov [es:di+2], al
    add di, 320
    mov [es:di+2], al
    add di, 320
    mov [es:di], al
    mov [es:di+1], al
    mov [es:di+2], al
    pop di
    ret

DrawCharF:
    push di
    mov bx, 5
.f_loop:
    mov [es:di], al
    add di, 320
    dec bx
    jnz .f_loop
    pop di
    push di
    mov [es:di+1], al
    mov [es:di+2], al
    add di, 640
    mov [es:di+1], al
    pop di
    ret

DrawCharU:
    push di
    mov bx, 4
.u_loop:
    mov [es:di], al
    mov [es:di+2], al
    add di, 320
    dec bx
    jnz .u_loop
    mov [es:di+1], al
    pop di
    ret

DrawCharE:
    push di
    mov bx, 5
.e_loop:
    mov [es:di], al
    add di, 320
    dec bx
    jnz .e_loop
    pop di
    push di
    mov [es:di+1], al
    mov [es:di+2], al
    add di, 640
    mov [es:di+1], al
    add di, 640
    mov [es:di+1], al
    mov [es:di+2], al
    pop di
    ret

DrawCharL:
    push di
    mov bx, 5
.l_loop:
    mov [es:di], al
    add di, 320
    dec bx
    jnz .l_loop
    sub di, 320
    mov [es:di+1], al
    mov [es:di+2], al
    pop di
    ret

DrawNumber:
    pusha
    call GetScreenOffset
    mov bx, 10
    xor cx, cx
.div_loop:
    xor dx, dx
    div bx
    push dx
    inc cx
    cmp ax, 0
    jnz .div_loop
.draw_digit_loop:
    pop ax
    push cx
    call DrawDigitBitmap
    add di, 4
    pop cx
    loop .draw_digit_loop
    popa
    ret

DrawDigitBitmap:
    pusha
    push ds
    push cs
    pop ds
    mov bx, digit_table
    xor ah, ah
    mov cx, ax
    shl ax, 2
    add ax, cx
    add bx, ax
    mov cx, 5
.d_row:
    mov al, [bx]
    push bx
    push di
    mov bx, 3
.d_col:
    shl al, 1
    jnc .no_pixel
    mov byte [es:di], 153
.no_pixel:
    inc di
    dec bx
    jnz .d_col
    pop di
    add di, 320
    pop bx
    inc bx
    loop .d_row
    pop ds
    popa
    ret










; =============================================================
; PALETTE ROUTINES
; =============================================================

setup_palette_main:
    mov dx, 03C8h
    xor al, al
    out dx, al
    mov dx, 03C9h
    mov al, 0
    out dx, al 
    out dx, al 
    out dx, al 
    mov al, 5
    out dx, al
    out dx, al
    out dx, al
    mov al, 7
    out dx, al
    out dx, al
    out dx, al
    mov al, 10
    out dx, al
    out dx, al
    out dx, al
    mov al, 12
    out dx, al
    out dx, al
    out dx, al
    mov al, 15
    out dx, al
    out dx, al
    out dx, al
    mov al, 17
    out dx, al
    out dx, al
    out dx, al
    mov al, 0
    out dx, al
    mov al, 20
    out dx, al
    mov al, 0
    out dx, al
    mov al, 5
    out dx, al
    mov al, 40
    out dx, al
    mov al, 5
    out dx, al
    mov al, 15
    out dx, al
    mov al, 63
    out dx, al
    mov al, 15
    out dx, al
    mov al, 35
    out dx, al
    mov al, 0
    out dx, al
    mov al, 0
    out dx, al
    mov al, 55
    out dx, al
    mov al, 8
    out dx, al
    mov al, 8
    out dx, al
    mov al, 0
    out dx, al
    mov al, 5
    out dx, al
    mov al, 30
    out dx, al
    mov al, 5
    out dx, al
    mov al, 15
    out dx, al
    mov al, 55
    out dx, al
    mov al, 10
    out dx, al
    mov al, 20
    out dx, al
    mov al, 10
    out dx, al
    mov al, 20
    out dx, al
    out dx, al
    out dx, al
    mov al, 2
    out dx, al
    out dx, al
    out dx, al
    mov al, 3
    out dx, al
    out dx, al
    out dx, al
    mov al, 3
    out dx, al
    out dx, al
    out dx, al
    mov al, 4
    out dx, al
    out dx, al
    out dx, al
    mov al, 20
    out dx, al
    mov al, 60
    out dx, al
    mov al, 20
    out dx, al
    mov al, 60
    out dx, al
    mov al, 15
    out dx, al
    mov al, 15
    out dx, al
    mov al, 15
    out dx, al
    mov al, 15
    out dx, al
    mov al, 60
    out dx, al
    mov cx, 9
    xor al, al
pal_zeros:
    out dx, al
    out dx, al
    out dx, al
    loop pal_zeros
    mov al, 63
    out dx, al
    out dx, al
    out dx, al
    ret

setup_game_palette:
    pusha
    mov dx, 0x3C8
    mov al, 1
    out dx, al
    mov dx, 0x3C9
    mov cx, 3
.b1: mov al, 0
    out dx, al
    out dx, al
    out dx, al
    loop .b1

    mov dx, 0x3C8
    mov al, 4
    out dx, al
    mov dx, 0x3C9
    mov cx, 3
.b2: mov al, 0
    out dx, al
    out dx, al
    out dx, al
    loop .b2

    mov al, 15
    mov dx, 0x3C8
    out dx, al
    mov dx, 0x3C9
    mov al, 0
    out dx, al
    out dx, al
    out dx, al

    mov dx, 0x3C8
    mov al, 16
    out dx, al
    mov dx, 0x3C9
    mov cx, 4       
.b3: mov al, 0
    out dx, al
    out dx, al
    out dx, al
    loop .b3

    mov dx, 0x3C8
    mov al, 25
    out dx, al
    mov dx, 0x3C9
    mov al, 20      
    out dx, al
    mov al, 20      
    out dx, al
    mov al, 20      
    out dx, al

    mov dx, 0x3C8
    mov al, 66
    out dx, al
    mov dx, 0x3C9
    mov al, 63      
    out dx, al
    mov al, 63      
    out dx, al
    mov al, 20      
    out dx, al

    mov dx, 0x3C8
    mov al, 74
    out dx, al
    mov dx, 0x3C9
    mov al, 8       
    out dx, al
    mov al, 8       
    out dx, al
    mov al, 8       
    out dx, al
    
    mov dx, 0x3C8
    mov al, 75
    out dx, al
    mov dx, 0x3C9
    mov al, 10
    out dx, al
    mov al, 10
    out dx, al
    mov al, 10
    out dx, al

    mov dx, 0x3C8
    mov al, 76
    out dx, al
    mov dx, 0x3C9
    mov al, 12      
    out dx, al
    mov al, 12      
    out dx, al
    mov al, 12      
    out dx, al
    
    mov dx, 0x3C8
    mov al, 78
    out dx, al
    mov dx, 0x3C9
    mov al, 5
    out dx, al
    mov al, 5
    out dx, al
    mov al, 25
    out dx, al

    mov dx, 0x3C8
    mov al, 90
    out dx, al
    mov dx, 0x3C9
    mov al, 15      
    out dx, al
    mov al, 0       
    out dx, al
    mov al, 0       
    out dx, al

    mov dx, 0x3C8
    mov al, 207
    out dx, al
    mov dx, 0x3C9
    mov al, 45      
    out dx, al
    mov al, 30      
    out dx, al
    mov al, 15      
    out dx, al

    mov dx, 0x3C8
    mov al, 253
    out dx, al
    mov dx, 0x3C9
    mov al, 63      
    out dx, al
    mov al, 51      
    out dx, al
    mov al, 0       
    out dx, al
    
    mov dx, 0x3C8
    mov al, 94
    out dx, al
    mov dx, 0x3C9
    mov al, 28      
    out dx, al
    mov al, 0       
    out dx, al
    mov al, 0       
    out dx, al
    
    mov dx, 0x3C8
    mov al, 96
    out dx, al
    mov dx, 0x3C9
    mov al, 35      
    out dx, al
    mov al, 0       
    out dx, al
    mov al, 0       
    out dx, al
    
    mov dx, 0x3C8
    mov al, 104
    out dx, al
    mov dx, 0x3C9
    mov al, 48      
    out dx, al
    mov al, 0       
    out dx, al
    mov al, 0       
    out dx, al
    
    mov dx, 0x3C8
    mov al, 105
    out dx, al
    mov dx, 0x3C9
    mov al, 60      
    out dx, al
    mov al, 15      
    out dx, al
    mov al, 15      
    out dx, al
    
    mov al, 10
    mov dx, 0x3C8
    out dx, al
    mov dx, 0x3C9
    mov al, 45
    out dx, al
    mov al, 0
    out dx, al
    mov al, 0
    out dx, al

    mov al, 11
    mov dx, 0x3C8
    out dx, al
    mov dx, 0x3C9
    mov al, 60
    out dx, al
    mov al, 20
    out dx, al
    mov al, 20
    out dx, al

    mov al, 13
    mov dx, 0x3C8
    out dx, al
    mov dx, 0x3C9
    mov al, 0
    out dx, al
    mov al, 0
    out dx, al
    mov al, 25
    out dx, al

    mov al, 22
    mov dx, 0x3C8
    out dx, al
    mov dx, 0x3C9
    mov al, 0
    out dx, al
    mov al, 25
    out dx, al
    mov al, 55
    out dx, al

    mov al, 82
    mov dx, 0x3C8
    out dx, al
    mov dx, 0x3C9
    mov al, 36
    out dx, al
    mov al, 36
    out dx, al
    mov al, 36
    out dx, al

    mov al, 89
    mov dx, 0x3C8
    out dx, al
    mov dx, 0x3C9
    mov al, 63
    out dx, al
    mov al, 63
    out dx, al
    mov al, 63
    out dx, al

    mov al, 94
    mov dx, 0x3C8
    out dx, al
    mov dx, 0x3C9
    mov al, 45
    out dx, al
    mov al, 0
    out dx, al
    mov al, 0
    out dx, al

    mov al, 120
    mov dx, 0x3C8
    out dx, al
    mov dx, 0x3C9
    mov al, 0
    out dx, al
    mov al, 40
    out dx, al
    mov al, 0
    out dx, al
    
    mov al, 153
    mov dx, 0x3C8
    out dx, al
    mov dx, 0x3C9
    mov al, 60
    out dx, al
    mov al, 60
    out dx, al
    mov al, 60
    out dx, al

    popa
    ret


; =============================================================
; DATA SECTION
; =============================================================
back_buffer_seg dw 0
game_active     db 1        ; 1 = Running, 0 = Game Over

car_width   EQU 45      
car_height  EQU 59      
coin_width  EQU 20
coin_height EQU 20
jerry_can_width  EQU 27
jerry_can_height EQU 27

; --- LOGIC VARIABLES ---
X_START     dw 138
Y_START     dw -60
npc_speed   dw 6
road_speed  dw 16        
road_scroll_y dw 0

player_x     dw 138     
player_y     dw 140
current_lane dw 1        
player_vel   dw 3 

lane_x_table: dw 64, 138, 211 

spawn_counter       dw 0    
current_object_type dw 0    ; 0=Car, 1=Coin, 2=Jerry

; --- STATS ---
coins_collected dw 0
fuel_amount     dw 100
fuel_counter    dw 0

digit_table:
    db 11100000b, 10100000b, 10100000b, 10100000b, 11100000b ; 0
    db 01000000b, 11000000b, 01000000b, 01000000b, 11100000b ; 1
    db 11100000b, 00100000b, 11100000b, 10000000b, 11100000b ; 2
    db 11100000b, 00100000b, 11100000b, 00100000b, 11100000b ; 3
    db 10100000b, 10100000b, 11100000b, 00100000b, 00100000b ; 4
    db 11100000b, 10000000b, 11100000b, 00100000b, 11100000b ; 5
    db 11100000b, 10000000b, 11100000b, 10100000b, 11100000b ; 6
    db 11100000b, 00100000b, 00100000b, 00100000b, 00100000b ; 7
    db 11100000b, 10100000b, 11100000b, 10100000b, 11100000b ; 8
    db 11100000b, 10100000b, 11100000b, 00100000b, 11100000b ; 9

; =============================================================
; PASTE SPRITES HERE
; =============================================================
; --- SPRITES ---

img_reed_car_large:
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 17, 1, 2, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 2, 1, 18, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 19, 1, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 1, 1, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 18, 1, 2, 2, 10, 11, 10, 10, 11, 10, 10, 11, 11, 11, 11, 11, 11, 11, 10, 10, 11, 10, 10, 11, 10, 2, 2, 1, 18, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 16, 1, 2, 2, 3, 10, 10, 10, 11, 11, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 11, 11, 10, 10, 10, 3, 2, 2, 1, 16, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 16, 10, 10, 10, 10, 11, 10, 10, 11, 11, 11, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 11, 11, 11, 10, 10, 11, 10, 10, 10, 10, 16, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 16, 10, 11, 11, 11, 11, 10, 10, 11, 11, 11, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 11, 11, 11, 10, 10, 11, 11, 11, 11, 10, 16, 0, 0, 0, 0
    DB 0, 0, 0, 0, 2, 11, 11, 11, 11, 11, 10, 11, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 10, 10, 11, 10, 11, 11, 11, 11, 11, 2, 0, 0, 0, 0
    DB 0, 0, 0, 18, 10, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 10, 18, 0, 0, 0
    DB 0, 0, 0, 2, 11, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 2, 0, 0, 0
    DB 0, 0, 16, 10, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 10, 16, 0, 0
    DB 0, 0, 17, 10, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 10, 17, 0, 0
    DB 0, 0, 17, 10, 11, 11, 11, 11, 10, 10, 11, 11, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 11, 11, 10, 10, 11, 11, 11, 11, 10, 17, 0, 0
    DB 0, 0, 17, 10, 11, 11, 11, 11, 10, 11, 11, 11, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 11, 11, 11, 10, 11, 11, 11, 11, 10, 17, 0, 0
    DB 0, 0, 17, 10, 11, 11, 11, 11, 10, 11, 11, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 11, 11, 10, 11, 11, 11, 11, 10, 17, 0, 0
    DB 0, 0, 16, 10, 11, 11, 11, 10, 10, 11, 11, 10, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 10, 11, 11, 10, 10, 11, 11, 11, 10, 16, 0, 0
    DB 0, 0, 0, 3, 11, 11, 11, 10, 10, 11, 11, 11, 10, 10, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 10, 10, 11, 11, 11, 10, 10, 11, 11, 11, 3, 0, 0, 0
    DB 0, 0, 0, 19, 10, 11, 11, 10, 10, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 10, 10, 11, 11, 10, 19, 0, 0, 0
    DB 0, 0, 0, 18, 10, 11, 11, 10, 10, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 10, 10, 11, 11, 10, 18, 0, 0, 0
    DB 0, 0, 0, 18, 10, 11, 11, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 11, 11, 10, 18, 0, 0, 0
    DB 0, 0, 0, 18, 10, 11, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 11, 10, 18, 0, 0, 0
    DB 0, 0, 0, 18, 10, 11, 11, 11, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 11, 11, 11, 10, 18, 0, 0, 0
    DB 0, 0, 0, 18, 10, 11, 11, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 11, 11, 10, 18, 0, 0, 0
    DB 0, 0, 0, 18, 10, 11, 11, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 11, 11, 10, 18, 0, 0, 0
    DB 16, 1, 10, 10, 10, 11, 10, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 10, 11, 10, 10, 10, 1, 16
    DB 3, 11, 11, 11, 10, 11, 3, 10, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 10, 3, 11, 10, 10, 11, 11, 3
    DB 1, 2, 1, 19, 10, 11, 3, 2, 11, 11, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 11, 2, 3, 11, 10, 1, 1, 2, 1
    DB 0, 0, 0, 17, 10, 11, 10, 2, 10, 11, 11, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 10, 2, 10, 11, 10, 17, 0, 0, 0
    DB 0, 0, 0, 16, 10, 11, 10, 3, 3, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 3, 3, 10, 11, 10, 16, 0, 0, 0
    DB 0, 0, 0, 17, 10, 11, 10, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 10, 11, 10, 16, 0, 0, 0
    DB 0, 0, 0, 17, 10, 11, 10, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 10, 11, 10, 16, 0, 0, 0
    DB 0, 0, 0, 17, 10, 11, 10, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 10, 11, 10, 16, 0, 0, 0
    DB 0, 0, 0, 17, 10, 11, 10, 3, 3, 3, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 3, 3, 3, 10, 11, 10, 16, 0, 0, 0
    DB 0, 0, 0, 17, 10, 11, 10, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 10, 11, 10, 16, 0, 0, 0
    DB 0, 0, 0, 17, 10, 11, 10, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 10, 11, 10, 17, 0, 0, 0
    DB 0, 0, 0, 18, 10, 11, 10, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 10, 11, 10, 18, 0, 0, 0
    DB 0, 0, 0, 18, 10, 11, 10, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 10, 11, 10, 18, 0, 0, 0
    DB 0, 0, 0, 1, 10, 11, 3, 2, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 2, 3, 11, 10, 1, 0, 0, 0
    DB 0, 0, 0, 1, 10, 11, 3, 1, 2, 1, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 1, 2, 1, 3, 11, 10, 1, 0, 0, 0
    DB 0, 0, 0, 2, 11, 11, 3, 3, 3, 2, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 2, 3, 3, 3, 11, 11, 2, 0, 0, 0
    DB 0, 0, 0, 2, 11, 11, 10, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 10, 11, 11, 2, 0, 0, 0
    DB 0, 0, 0, 3, 11, 11, 10, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 10, 11, 11, 3, 0, 0, 0
    DB 0, 0, 16, 10, 11, 11, 10, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 10, 11, 11, 3, 16, 0, 0
    DB 0, 0, 16, 10, 11, 11, 11, 10, 3, 2, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 2, 3, 10, 11, 11, 11, 10, 16, 0, 0
    DB 0, 0, 16, 10, 11, 11, 11, 11, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 11, 11, 11, 11, 10, 16, 0, 0
    DB 0, 0, 16, 10, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 10, 16, 0, 0
    DB 0, 0, 16, 10, 11, 11, 11, 11, 10, 10, 10, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 10, 10, 10, 11, 11, 11, 11, 10, 16, 0, 0
    DB 0, 0, 16, 10, 11, 11, 11, 11, 10, 10, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 10, 10, 11, 11, 11, 11, 10, 16, 0, 0
    DB 0, 0, 16, 10, 11, 11, 11, 11, 10, 10, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 10, 10, 11, 11, 11, 11, 10, 16, 0, 0
    DB 0, 0, 16, 10, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 10, 16, 0, 0
    DB 0, 0, 0, 2, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 2, 0, 0, 0
    DB 0, 0, 0, 1, 10, 11, 11, 11, 11, 11, 11, 11, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 11, 11, 11, 11, 11, 11, 11, 10, 1, 0, 0, 0
    DB 0, 0, 0, 17, 10, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 10, 17, 0, 0, 0
    DB 0, 0, 0, 16, 10, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 10, 0, 0, 0, 0
    DB 0, 0, 0, 0, 2, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 2, 0, 0, 0, 0
    DB 0, 0, 0, 0, 16, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 16, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 3, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 2, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 19, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 19, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 2, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 10, 2, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 16, 16, 18, 1, 2, 2, 3, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 3, 2, 2, 1, 19, 16, 16, 0, 0, 0, 0, 0, 0, 0

img_blue_car_large:
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 1, 3, 3, 4, 5, 5, 5, 4, 3, 2, 19, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 18, 5, 13, 13, 13, 22, 22, 22, 22, 22, 22, 22, 22, 13, 13, 13, 13, 5, 17, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 1, 1, 2, 22, 22, 22, 22, 13, 22, 22, 22, 22, 22, 22, 22, 13, 22, 22, 22, 22, 2, 1, 19, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 1, 2, 4, 13, 13, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 13, 22, 22, 22, 13, 13, 4, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 15, 15, 13, 22, 13, 22, 22, 22, 13, 22, 22, 22, 22, 22, 22, 22, 22, 22, 13, 22, 22, 22, 13, 22, 13, 15, 15, 3, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 4, 22, 22, 22, 22, 13, 22, 22, 22, 13, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 13, 22, 22, 22, 22, 3, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 1, 13, 22, 22, 22, 22, 13, 22, 13, 22, 13, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 13, 22, 13, 22, 13, 22, 22, 22, 22, 13, 18, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 15, 22, 22, 22, 22, 13, 13, 15, 13, 13, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 13, 13, 15, 13, 13, 22, 22, 22, 22, 6, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 18, 13, 22, 22, 22, 22, 13, 15, 13, 13, 13, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 13, 13, 13, 15, 13, 22, 22, 22, 22, 13, 16, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 2, 22, 22, 22, 22, 22, 13, 13, 13, 13, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 13, 13, 13, 13, 22, 22, 22, 22, 22, 2, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 5, 22, 22, 22, 22, 13, 13, 13, 13, 13, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 13, 13, 13, 13, 13, 22, 22, 22, 22, 3, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 5, 22, 22, 22, 22, 13, 13, 22, 22, 13, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 13, 22, 22, 13, 13, 22, 22, 22, 22, 4, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 5, 22, 22, 22, 22, 13, 22, 22, 13, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 13, 22, 22, 13, 22, 22, 22, 22, 4, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 5, 22, 22, 22, 22, 13, 22, 22, 13, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 13, 22, 22, 13, 22, 22, 22, 22, 4, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 2, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 13, 13, 15, 15, 15, 15, 6, 15, 15, 15, 15, 13, 13, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 2, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 13, 22, 22, 13, 22, 22, 22, 13, 6, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 6, 13, 22, 22, 22, 22, 22, 22, 13, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 6, 22, 22, 13, 22, 13, 4, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 5, 13, 22, 13, 22, 22, 5, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 6, 22, 22, 22, 15, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 13, 22, 22, 22, 5, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 6, 22, 22, 22, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 22, 22, 22, 5, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 6, 22, 22, 13, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 22, 22, 22, 5, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 6, 22, 22, 22, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 6, 22, 22, 22, 5, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 5, 22, 22, 22, 15, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 15, 22, 22, 22, 5, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 17, 3, 13, 22, 13, 22, 13, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 13, 22, 13, 22, 13, 3, 16, 0, 0, 0
    DB 0, 0, 0, 0, 6, 22, 22, 22, 22, 15, 13, 22, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 22, 13, 15, 22, 22, 22, 22, 5, 0, 0
    DB 0, 0, 0, 0, 5, 15, 3, 15, 22, 15, 5, 22, 13, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 13, 22, 4, 15, 22, 15, 4, 15, 4, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 5, 22, 15, 2, 13, 22, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 22, 13, 2, 15, 22, 5, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 5, 22, 15, 2, 15, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 15, 2, 15, 22, 5, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 5, 22, 15, 3, 5, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 5, 3, 13, 22, 5, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 5, 22, 13, 3, 4, 13, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 13, 4, 3, 13, 22, 5, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 5, 22, 13, 3, 3, 13, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 13, 3, 3, 13, 22, 5, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 5, 22, 13, 3, 3, 15, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 15, 3, 3, 13, 22, 5, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 5, 22, 13, 3, 4, 15, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 15, 4, 3, 13, 22, 5, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 5, 22, 15, 3, 4, 6, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 6, 4, 3, 13, 22, 5, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 6, 22, 15, 3, 4, 5, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 5, 4, 3, 15, 22, 6, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 15, 22, 15, 3, 4, 5, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 5, 4, 3, 15, 22, 15, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 15, 22, 15, 2, 3, 4, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 4, 4, 2, 15, 22, 15, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 13, 22, 15, 2, 2, 3, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 3, 2, 2, 15, 22, 15, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 13, 22, 15, 3, 4, 5, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 4, 4, 3, 15, 22, 13, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 16, 13, 22, 15, 3, 4, 5, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 5, 4, 3, 15, 22, 13, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 18, 13, 22, 13, 3, 4, 6, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 5, 4, 3, 13, 22, 13, 16, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 1, 22, 22, 22, 15, 2, 6, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 6, 2, 15, 22, 22, 22, 18, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 2, 22, 22, 22, 22, 15, 15, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 15, 15, 22, 22, 22, 22, 1, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 2, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 2, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 3, 22, 22, 22, 22, 22, 13, 22, 15, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 15, 22, 13, 22, 22, 22, 22, 22, 2, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 3, 22, 22, 22, 22, 13, 15, 13, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 13, 15, 13, 22, 22, 22, 22, 2, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 2, 22, 22, 22, 22, 13, 15, 13, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 13, 15, 13, 22, 22, 22, 22, 2, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 1, 22, 22, 22, 22, 22, 22, 22, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 22, 22, 22, 22, 22, 22, 22, 18, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 16, 13, 22, 22, 22, 22, 22, 22, 15, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 15, 22, 22, 22, 22, 22, 22, 13, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 13, 22, 22, 22, 22, 22, 22, 13, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 13, 22, 22, 22, 22, 22, 22, 15, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 6, 22, 22, 22, 22, 22, 22, 22, 4, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 4, 22, 22, 22, 22, 22, 22, 22, 5, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 2, 22, 22, 22, 22, 22, 22, 22, 13, 6, 4, 3, 3, 4, 4, 4, 4, 4, 4, 4, 3, 3, 4, 15, 13, 22, 22, 22, 22, 22, 22, 22, 1, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 16, 13, 22, 22, 22, 22, 22, 22, 22, 22, 13, 13, 15, 5, 5, 4, 4, 4, 5, 5, 15, 13, 13, 22, 22, 22, 22, 22, 22, 22, 22, 13, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 6, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 13, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 5, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 2, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 19, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 15, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 15, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 4, 15, 13, 13, 13, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 13, 13, 13, 15, 5, 16, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 18, 1, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 18, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 130, 130, 141, 0, 0, 0, 0, 0, 0, 0

img_coin:
    DB   0,   0,   0,   0,   0,   0,  74,  25, 207, 207, 207, 207,  25,  74,   0,   0,   0,   0,   0,   0
    DB   0,   0,   0,   0,  90, 207, 253, 253, 253, 253, 253, 253, 253,  66, 207,  90,   0,   0,   0,   0
    DB   0,   0,   0,  76, 253, 253, 253, 253, 253, 253, 253,  66, 253, 253, 253, 253,  76,   0,   0,   0
    DB   0,   0,  76, 253, 253, 253, 253,  66, 253,  66,  66,  66,  66, 253, 253, 253, 253,  76,   0,   0
    DB   0,  90, 253, 253, 253,  66, 253, 253,  66,  66,  66, 253, 253, 253,  66, 253, 253, 253,  90,   0
    DB   0, 207, 253, 253,  66, 253,  66,  66,  66,  66, 253, 253, 253, 253, 253,  66, 253, 253, 207,   0
    DB  74, 253, 253, 253, 253,  66,  66,  66, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253,  74
    DB  25, 253, 253, 253,  66,  66,  66, 253, 253, 253, 253, 253, 253, 253, 253, 253,  66, 253, 253,  25
    DB 207, 253, 253,  66,  66,  66, 253, 253, 253, 253, 253, 253, 253, 253, 253,  66,  66, 253, 253, 207
    DB 207, 253,  66,  66, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253,  66,  66,  66, 253, 253, 207
    DB 207, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253,  66,  66,  66,  66,  66, 253, 253, 207
    DB 207, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253,  66,  66,  66,  66,  66,  66, 253, 253, 207
    DB  25, 253, 253, 253, 253, 253, 253, 253, 253,  66,  66,  66,  66,  66,  66,  66,  66, 253, 253,  25
    DB  74, 253, 253, 253, 253, 253, 253, 253,  66,  66,  66,  66,  66,  66,  66,  66, 253, 253, 253,  74
    DB   0, 207, 253, 253, 253, 253,  66,  66,  66,  66,  66,  66,  66,  66, 253, 253, 253, 253, 207,   0
    DB   0,  90, 253, 253, 253,  66,  66,  66,  66,  66,  66,  66,  66, 253, 253, 253, 253, 253,  90,   0
    DB   0,   0,  76, 253, 253, 253,  66,  66,  66,  66,  66,  66, 253, 253, 253, 253, 253,  76,   0,   0
    DB   0,   0,   0,  76,  66, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253,  76,   0,   0,   0
    DB   0,   0,   0,   0,  74, 207,  66, 253, 253, 253, 253, 253, 253, 253, 207,  90,   0,   0,   0,   0
    DB   0,   0,   0,   0,   0,   0,  74,  25, 207, 207, 207, 207,  25,  74,   0,   0,   0,   0,   0,   0

img_jerry_can:
    DB  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0, 74,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0, 74, 74, 74,  0,  0,  0, 90, 90, 90, 90, 90, 90, 90, 90, 90,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0, 74, 74, 74,  0, 90, 104, 104, 104, 104, 104, 104, 104, 104, 78,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0,  0, 74, 74, 90, 104, 104, 104, 104, 104, 105, 105, 105, 104, 76,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0, 90, 104, 104, 96, 96, 96, 96, 96, 96, 96, 96, 96, 104, 94,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0, 90, 104, 96, 104, 104, 105, 105, 105, 105, 105, 105, 105, 96, 94,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0, 90, 104, 96, 104, 105, 105, 105, 105, 105, 105, 104, 104, 96, 94,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0, 90, 104, 96, 96, 96, 105, 105, 105, 104, 104, 96, 96, 96, 94,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0, 90, 104, 96, 104, 96, 96, 104, 104, 104, 96, 96, 104, 96, 94,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0, 90, 104, 96, 104, 104, 96, 96, 104, 96, 96, 104, 104, 96, 94,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0, 90, 104, 96, 104, 104, 104, 96, 96, 96, 104, 104, 104, 96, 94,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0, 90, 104, 96, 104, 104, 104, 96, 96, 96, 104, 104, 104, 96, 94,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0, 90, 104, 96, 104, 96, 96, 104, 104, 104, 96, 96, 104, 96, 94,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0, 90, 104, 96, 104, 104, 96, 96, 104, 96, 96, 104, 104, 96, 94,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0, 90, 104, 96, 96, 96, 104, 104, 104, 104, 104, 96, 96, 96, 94,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0, 90, 104, 96, 104, 104, 104, 104, 104, 104, 104, 104, 104, 96, 94,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0, 90, 104, 104, 96, 96, 96, 96, 96, 96, 96, 96, 96, 104, 94,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0, 90, 94, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 94, 90,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0,  0, 90, 75, 75, 75, 75, 75, 75, 75, 75, 75, 76, 90,  0,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
	var0: 
	var1:
	
	var2:
	var3:
	var4: