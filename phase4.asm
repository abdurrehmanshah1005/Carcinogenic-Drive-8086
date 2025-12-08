BITS 16
ORG 0x100

; =========================================================
; MEMORY MANAGEMENT & SETUP
; =========================================================
start:
    ; --- 1. Allocate Back Buffer ---
    ; Shrink program memory to keep 64KB for code/data
    mov ah, 0x4A
    mov bx, 0x1000      ; 4096 paragraphs = 64KB
    int 0x21

    ; Allocate 64KB for the back buffer
    mov ah, 0x48
    mov bx, 0x0FA0      ; 4000 paragraphs = 64000 bytes
    int 0x21
    jnc .mem_ok
    jmp exit_to_dos     ; Fail if no memory

.mem_ok:
    mov [back_buffer_seg], ax

    ; --- 2. Video Mode ---
    mov ah, 0Fh
    int 10h
    push ax             ; Save original mode
    
    mov ah, 0
    mov al, 13h         ; Mode 13h (320x200, 256 colors)
    int 10h

    ; --- 3. Game Setup ---
    call setup_palette
    call setup_palette_main
    call install_keyboard_isr
    call install_confirm_isr
    
    ; --- 4. Menus ---
    ; Note: Intro screens draw directly to video memory (0xA000)
    ; because double buffering isn't needed for static menus.
    call draw_intro_screen
    call wait_for_menu_key
    call collect_player_details
    call show_instruction_screen
    call show_start_screen
    call wait_for_game_start
    
    ; Hide Cursor
    mov ah, 01h
    mov ch, 32
    int 10h
    
    call reset_gameplay_state

; =========================================================
; MAIN GAME LOOP (DOUBLE BUFFERED)
; =========================================================
main_loop:
    ; Check Game Over
    cmp word [game_over_flag], 1
    je handle_game_over_label

    ; --- A. PREPARE BUFFER ---
    ; Point ES to our Back Buffer. All drawing goes here.
    mov ax, [back_buffer_seg]
    mov es, ax

    ; --- B. DRAW FRAME ---
    ; 1. Background (Fast Fill)
    call draw_fast_background
    
    ; 2. Scrolling Elements (Barriers/Lanes)
    call draw_scrolling_elements
    
    ; 3. Entities
    call draw_enemy_car
    
    cmp word [coin_active], 0
    je .skip_coin_draw
    call draw_coin
.skip_coin_draw:

    cmp word [jerry_active], 0
    je .skip_jerry_draw
    call draw_jerry
.skip_jerry_draw:

    call draw_player_car

    ; --- C. UPDATE LOGIC ---
    call handle_input       ; Handle keys
    call update_physics     ; Move cars, scroll, spawn items
    call check_collisions   ; Collision detection

    ; --- D. RENDER TO SCREEN ---
    call vsync              ; Wait for refresh
    call flip_buffer        ; Copy Back Buffer -> Video Memory (0xA000)

    ; --- E. DRAW HUD (Direct to Video Mem) ---
    ; We draw text AFTER flipping so it sits on top of the graphics
    ; BIOS text functions write to 0xA000 automatically.
    call draw_hud

    jmp main_loop

handle_game_over_label:
    call show_game_over_screen
    call reset_gameplay_state
    call show_start_screen
    call wait_for_game_start
    jmp main_loop

; =========================================================
; INPUT HANDLING
; =========================================================
handle_input:
.input_loop:
    mov ah, 01h
    int 16h
    jz .done_input      ; Buffer empty, exit
    
    mov ah, 0
    int 16h
    
    cmp ah, 01h         ; ESC
    je .pause
    cmp ah, 4Bh         ; Left Arrow
    je .left
    cmp ah, 4Dh         ; Right Arrow
    je .right
    cmp ah, 48h         ; Up Arrow
    je .up
    cmp ah, 50h         ; Down Arrow
    je .down
    
    jmp .input_loop     ; Unknown key, check next

.left:
    mov ax, [player_lane]
    cmp ax, 0
    je .input_loop      ; Already at left edge, check next key
    dec word [player_lane]
    jmp .input_loop

.right:
    mov ax, [player_lane]
    cmp ax, 2
    je .input_loop      ; Already at right edge, check next key
    inc word [player_lane]
    jmp .input_loop

.up:
    mov ax, [player_bottom_y]
    sub ax, PLAYER_VERTICAL_STEP
    cmp ax, PLAYER_TOP_LIMIT + reed_car_large_height
    jge .set_up
    mov ax, PLAYER_TOP_LIMIT + reed_car_large_height
.set_up:
    mov [player_bottom_y], ax
    jmp .input_loop

.down:
    mov ax, [player_bottom_y]
    add ax, PLAYER_VERTICAL_STEP
    cmp ax, PLAYER_BOTTOM_LIMIT
    jle .set_down
    mov ax, PLAYER_BOTTOM_LIMIT
.set_down:
    mov [player_bottom_y], ax
    jmp .input_loop

.pause:
    call handle_pause_logic
    ret                 ; Exit after pause

.done_input:
    ret

handle_pause_logic:
    mov word [game_paused_flag], 1
    mov word [confirm_active], 1
    int 60h                 ; Call custom ISR
    cmp word [quit_requested], 1
    je .quit_to_menu
    mov word [quit_requested], 0
    mov word [game_paused_flag], 0
    mov word [confirm_active], 0
    ret
.quit_to_menu:
    mov word [quit_requested], 0
    mov word [game_paused_flag], 0
    mov word [confirm_active], 0
    call show_start_screen
    mov word [game_started_flag], 0
    call wait_for_game_start
    mov ah, 01h
    mov ch, 32
    int 10h
    call reset_gameplay_state
    ret

; =========================================================
; PHYSICS & LOGIC
; =========================================================
update_physics:
    ; 1. Update Scroll Offset
    mov ax, [scroll_offset]
    add ax, 10              ; Scroll Speed
    cmp ax, 40
    jl .save_scroll
    xor ax, ax
.save_scroll:
    mov [scroll_offset], ax

    ; 2. Update Enemy Position
    mov ax, [enemy_y]
    add ax, 3               ; Enemy Speed
    cmp ax, 200
    jl .save_enemy
    
    ; Enemy Respawn
    mov ax, -60
    call get_random_lane_in_dx
    mov [enemy_lane], dx
    
    ; Spawn Logic (Coin/Jerry)
    call handle_spawning

.save_enemy:
    mov [enemy_y], ax

    ; 3. Update Coin
    cmp word [coin_active], 1
    jne .check_jerry
    mov ax, [coin_y]
    add ax, 6               ; Move with scroll speed
    cmp ax, 200
    jl .save_coin
    mov word [coin_active], 0
.save_coin:
    mov [coin_y], ax

.check_jerry:
    cmp word [jerry_active], 1
    jne .done_physics
    mov ax, [jerry_y]
    add ax, 6               ; Move with scroll speed
    cmp ax, 200
    jl .save_jerry
    mov word [jerry_active], 0
.save_jerry:
    mov [jerry_y], ax

.done_physics:
    ret

handle_spawning:
    ; Increment counters
    inc word [coin_spawn_counter]
    inc word [jerry_spawn_counter]

    ; Try Spawn Coin
    cmp word [coin_active], 1
    je .try_jerry
    mov ax, [coin_spawn_counter]
    cmp ax, COIN_SPAWN_THRESHOLD
    jl .try_jerry
    
    mov word [coin_spawn_counter], 0
    call get_random_lane_in_dx
    mov cx, dx
    
    ; Don't spawn on enemy
    cmp cx, [enemy_lane]
    je .try_jerry
    
    mov [coin_lane], cx
    mov word [coin_active], 1
    mov word [coin_y], -60

.try_jerry:
    cmp word [jerry_active], 1
    je .done_spawn
    mov ax, [jerry_spawn_counter]
    cmp ax, JERRY_SPAWN_THRESHOLD
    jl .done_spawn
    
    mov word [jerry_spawn_counter], 0
    call get_random_lane_in_dx
    mov cx, dx
    
    ; Check conflicts
    cmp cx, [enemy_lane]
    je .done_spawn
    cmp word [coin_active], 1
    jne .spawn_jerry_ok
    cmp cx, [coin_lane]
    je .done_spawn

.spawn_jerry_ok:
    mov [jerry_lane], cx
    mov word [jerry_active], 1
    mov word [jerry_y], -60

.done_spawn:
    ret

; =========================================================
; GRAPHICS CORE (OPTIMIZED)
; =========================================================

; Copies the back buffer (ES) to video memory (0xA000)
flip_buffer:
    push ds
    push es
    push si
    push di
    push cx

    mov ds, [back_buffer_seg]   ; Source
    xor si, si
    mov ax, 0xA000              ; Destination
    mov es, ax
    xor di, di
    mov cx, 32000               ; 64000 bytes / 2
    rep movsw

    pop cx
    pop di
    pop si
    pop es
    pop ds
    ret

; Draws Background using fast memory fills (rep stosb)
; Replaces slow pixel-by-pixel drawing
draw_fast_background:
    pusha
    
    ; 1. Fill entire screen with Green (Grass)
    xor di, di
    mov al, 120     ; Green Color
    mov cx, 64000
    rep stosb

    ; 2. Draw Road (Gray Box in middle)
    ; Road X: 50 to 270 (Width 220)
    mov dx, 0       ; Y counter
    mov di, 50      ; Start offset (Y=0, X=50)
.road_loop:
    push di
    mov al, 82      ; Gray Color
    mov cx, 220
    rep stosb
    pop di
    add di, 320     ; Next line
    inc dx
    cmp dx, 200
    jl .road_loop

    ; 3. Draw Trees (Pixel plotting is okay here, sparse)
    call draw_left_trees
    call draw_right_trees
    
    popa
    ret

; Optimized Pixel Plotter for Double Buffering
; Writes to [ES:DI]. ES must be set to back buffer segment.
; Input: CX = X, DX = Y, AL = Color
draw_pixel:
    push di
    push dx
    push bx
    push ax                 ; Preserve AL (color value)
    
    ; Clipping
    cmp cx, 0
    jl .dp_skip
    cmp cx, 320
    jge .dp_skip
    cmp dx, 0
    jl .dp_skip
    cmp dx, 200
    jge .dp_skip

    ; Offset = (Y * 320) + X
    ;      = (Y * 256) + (Y * 64) + X
    mov di, dx
    shl di, 8       ; Y * 256
    mov bx, dx      ; Save Y in BX temporarily
    shl bx, 6       ; Y * 64
    add di, bx
    add di, cx
    
    pop ax                  ; Restore AL (color value)
    mov [es:di], al
    jmp .dp_done

.dp_skip:
    pop ax                  ; Balance stack if we skipped

.dp_done:
    pop bx
    pop dx
    pop di
    ret

; =========================================================
; DRAWING OBJECTS
; =========================================================

draw_player_car:
    pusha
    
    ; 1. Calculate Destination Address (ES:DI)
    ; DI = (Y * 320) + X
    mov ax, [player_bottom_y]
    sub ax, reed_car_large_height ; Top Y
    inc ax
    
    ; Multiply Y by 320 (Y*256 + Y*64)
    mov di, ax
    shl di, 8       ; Y * 256
    mov bx, ax
    shl bx, 6       ; Y * 64
    add di, bx
    
    ; Add X offset
    mov ax, [player_lane]
    shl ax, 1
    mov bx, ax
    mov bx, [lane_positions + bx] ; BX = Screen X
    add di, bx      ; DI points to top-left corner of car in buffer
    
    ; 2. Setup Source (DS:SI)
    mov si, img_reed_car_large
    
    ; 3. Draw Loop
    mov cx, reed_car_large_height ; Height loop
    
.row_loop:
    push cx
    push di         ; Save start of this screen line
    
    mov cx, reed_car_large_width ; Width loop
    
.col_loop:
    mov al, [si]    ; Load pixel from sprite
    cmp al, 0       ; Transparent?
    je .skip_pixel
    mov [es:di], al ; Write directly to buffer (FAST!)
    
.skip_pixel:
    inc si          ; Next sprite byte
    inc di          ; Next screen byte
    loop .col_loop
    
    pop di          ; Restore start of line
    add di, 320     ; Move DI down one full screen line (320 pixels)
    pop cx
    loop .row_loop
    
    popa
    ret

draw_enemy_car:
    pusha
    
    mov ax, [enemy_y]
    mov bx, blue_car_large_height ; Default height to draw
    mov si, img_blue_car_large    ; Source sprite
    
    ; --- 1. TOP CLIPPING (Smooth entry) ---
    cmp ax, 0
    jge .y_positive
    
    ; If Y is negative (e.g. -10):
    ; We skip 10 rows of the sprite source
    ; We start drawing at Screen Y = 0
    neg ax                  ; AX = 10 (Rows to skip)
    sub bx, ax              ; DrawHeight = Height - 10
    cmp bx, 0
    jle .done               ; Entirely off screen
    
    ; Advance SI by (RowsSkipped * Width)
    push bx
    mov bx, blue_car_large_width
    mul bx                  ; AX = Offset bytes
    add si, ax              ; Move SI forward
    pop bx
    
    xor ax, ax              ; Screen Y = 0
    jmp .calc_addr

.y_positive:
    ; Check Bottom Clipping
    cmp ax, 200
    jge .done
    ; (Optional: Calculate if bottom cuts off, but Y=200 check usually enough)

.calc_addr:
    ; --- 2. Calculate Memory Address ---
    ; DI = (ScreenY * 320) + ScreenX
    mov di, ax      ; Y
    shl di, 8       ; Y*256
    push dx
    mov dx, ax
    shl dx, 6       ; Y*64
    add di, dx
    pop dx
    
    ; Add Lane X
    mov ax, [enemy_lane]
    shl ax, 1
    push bx
    mov bx, ax
    mov bx, [lane_positions + bx]
    add di, bx      ; DI is now at the start pixel
    pop bx

    ; BX currently holds "Height to Draw"
    mov cx, bx      ; Loop counter = Height
    
.row_loop:
    push cx
    push di         ; Save Start of Screen Line
    
    mov cx, blue_car_large_width
    
.col_loop:
    mov al, [si]
    cmp al, 0
    je .skip
    mov [es:di], al ; WRITE PIXEL FAST
.skip:
    inc si
    inc di
    loop .col_loop
    
    pop di
    add di, 320     ; Jump to next line on screen
    pop cx
    loop .row_loop

.done:
    popa
    ret

draw_coin:
    pusha
    mov si, coin_sprite
    mov bp, [coin_y]
    mov di, 0
.c_row:
    mov ax, [coin_lane]
    shl ax, 1
    mov bx, ax
    mov bx, [coin_positions + bx]
    mov cx, 0
.c_col:
    mov al, [si]
    cmp al, 0
    je .c_skip
    push cx
    add cx, bx
    mov dx, bp
    call draw_pixel
    pop cx
.c_skip:
    inc si
    inc cx
    cmp cx, coin_width
    jl .c_col
    inc bp
    inc di
    cmp di, coin_height
    jl .c_row
    popa
    ret

draw_jerry:
    pusha
    mov si, img_jerry_can
    mov bp, [jerry_y]
    mov di, 0
.j_row:
    mov ax, [jerry_lane]
    shl ax, 1
    mov bx, ax
    mov bx, [jerry_positions + bx]
    mov cx, 0
.j_col:
    mov al, [si]
    cmp al, 0
    je .j_skip
    push cx
    add cx, bx
    mov dx, bp
    call draw_pixel
    pop cx
.j_skip:
    inc si
    inc cx
    cmp cx, jerry_width
    jl .j_col
    inc bp
    inc di
    cmp di, jerry_height
    jl .j_row
    popa
    ret

draw_scrolling_elements:
    push ax
    push bx
    push cx
    push dx
    call draw_left_barrier_scroll
    call draw_right_barrier_scroll
    call draw_lanes_scroll
    pop dx
    pop cx
    pop bx
    pop ax
    ret

draw_left_barrier_scroll:
    pusha
    mov dx, 0       ; Y
.lb_loop:
    mov ax, dx
    sub ax, [scroll_offset]
    ; Modulo logic for pattern
.lb_mod:
    cmp ax, 0
    jge .lb_pos
    add ax, 20
    jmp .lb_mod
.lb_pos:
    cmp ax, 20
    jl .lb_check
    sub ax, 20
    jmp .lb_pos
.lb_check:
    cmp ax, 10
    mov al, 89      ; White
    jl .lb_draw
    mov al, 104     ; Red
.lb_draw:
    ; Draw row from X=30 to X=50
    mov cx, 30
.lb_x:
    call draw_pixel
    inc cx
    cmp cx, 50
    jl .lb_x
    inc dx
    cmp dx, 200
    jl .lb_loop
    popa
    ret

draw_right_barrier_scroll:
    pusha
    mov dx, 0
.rb_loop:
    mov ax, dx
    sub ax, [scroll_offset]
.rb_mod:
    cmp ax, 0
    jge .rb_pos
    add ax, 20
    jmp .rb_mod
.rb_pos:
    cmp ax, 20
    jl .rb_check
    sub ax, 20
    jmp .rb_pos
.rb_check:
    cmp ax, 10
    mov al, 89
    jl .rb_draw
    mov al, 104
.rb_draw:
    mov cx, 270
.rb_x:
    call draw_pixel
    inc cx
    cmp cx, 290
    jl .rb_x
    inc dx
    cmp dx, 200
    jl .rb_loop
    popa
    ret

draw_lanes_scroll:
    pusha
    mov cx, 118
    call draw_thick_dashed_line_scroll
    mov cx, 198
    call draw_thick_dashed_line_scroll
    popa
    ret

draw_thick_dashed_line_scroll:
    pusha
    mov si, cx      ; Base X
    mov di, 0       ; Y counter
.dash_loop:
    mov ax, di
    sub ax, [scroll_offset]
.d_mod:
    cmp ax, 0
    jge .d_pos
    add ax, 40
    jmp .d_mod
.d_pos:
    cmp ax, 40
    jl .d_check
    sub ax, 40
    jmp .d_pos
.d_check:
    mov al, 82      ; Road Color (Gap)
    cmp ax, 20
    jge .d_draw
    mov al, 89      ; White (Dash)
.d_draw:
    mov bx, 0
.d_wid:
    mov cx, si
    add cx, bx
    mov dx, di
    call draw_pixel
    inc bx
    cmp bx, 5
    jl .d_wid
    inc di
    cmp di, 200
    jl .dash_loop
    popa
    ret

draw_left_trees:
    push ax
    push bx
    push cx
    push dx
    mov bx, 30
    call draw_tree_left
    mov bx, 80
    call draw_tree_left
    mov bx, 130
    call draw_tree_left
    mov bx, 180
    call draw_tree_left
    pop dx
    pop cx
    pop bx
    pop ax
    ret

draw_right_trees:
    push ax
    push bx
    push cx
    push dx
    mov bx, 30
    call draw_tree_right
    mov bx, 80
    call draw_tree_right
    mov bx, 130
    call draw_tree_right
    mov bx, 180
    call draw_tree_right
    pop dx
    pop cx
    pop bx
    pop ax
    ret

draw_tree_left:
    pusha
    mov dx, bx
    sub dx, 8
    mov si, 0       ; Tree row count
.tl_row:
    mov cx, 8       ; Start X
.tl_col:
    mov al, 120
    call draw_pixel
    inc cx
    cmp cx, 22
    jl .tl_col
    inc dx
    inc si
    cmp si, 8
    jl .tl_row
    popa
    ret

draw_tree_right:
    pusha
    mov dx, bx
    sub dx, 8
    mov si, 0
.tr_row:
    mov cx, 298
.tr_col:
    mov al, 120
    call draw_pixel
    inc cx
    cmp cx, 312
    jl .tr_col
    inc dx
    inc si
    cmp si, 8
    jl .tr_row
    popa
    ret

; =========================================================
; EXIT & CLEANUP
; =========================================================
exit_to_dos:
    call restore_confirm_isr
    call restore_keyboard_isr
    
    ; Restore Cursor
    mov ah, 01h
    mov ch, 6
    mov cl, 7
    int 10h
    
    ; Free Back Buffer
    mov es, [back_buffer_seg]
    mov ah, 0x49
    int 0x21

    ; Restore Video Mode
    pop ax
    mov ah, 00h
    int 10h
    
    mov ah, 4Ch
    int 21h

; =========================================================
; HELPER SUBROUTINES (HUD, COLLISION, ETC)
; =========================================================

; Re-added HUD drawing using BIOS (Writes to Screen 0xA000)
; Called AFTER flip_buffer so text appears on top
draw_hud:
    pusha
    ; Reset ES to 0 (BIOS calls use DS/ES implicitly or internal buffers)
    ; Actually BIOS writes to Active Page. 
    
    mov dh, HUD_SCORE_ROW
    mov dl, HUD_SCORE_COL
    mov si, hud_score_label
    call print_string_at
    
    mov ax, [coin_collected_count]
    call print_number
    
    mov dh, HUD_FUEL_ROW
    mov dl, HUD_FUEL_COL
    mov si, hud_fuel_label
    call print_string_at
    
    mov ax, [fuel_collected_count]
    call print_number
    popa
    ret

print_string_at:
    pusha
    mov ah, 02h         ; Set Cursor Position
    mov bh, 0           ; Page 0
    int 10h
    
    mov ah, 0Eh         ; Teletype Output
    mov bl, 15          ; White color
.ps_loop:
    lodsb
    cmp al, 0
    je .ps_done
    int 10h
    jmp .ps_loop
.ps_done:
    popa
    ret

print_number:
    pusha
    mov cx, 0
    mov bx, 10
.pn_div:
    xor dx, dx
    div bx
    push dx
    inc cx
    cmp ax, 0
    jne .pn_div
.pn_print:
    pop ax
    add al, '0'
    mov ah, 0Eh
    mov bh, 0
    mov bl, 15
    int 10h
    loop .pn_print
    popa
    ret

check_collisions:
    pusha
    ; 1. Enemy
    mov ax, [player_lane]
    cmp ax, [enemy_lane]
    jne .chk_coin
    
    ; Vertical overlap check
    mov bx, [player_bottom_y]
    sub bx, reed_car_large_height ; Player Top
    add bx, COLLISION_MARGIN
    
    mov cx, [enemy_y]
    add cx, blue_car_large_height ; Enemy Bottom
    sub cx, COLLISION_MARGIN
    
    cmp bx, cx
    jge .chk_coin ; Player Top >= Enemy Bottom (Safe if below)
    
    mov bx, [player_bottom_y]
    sub bx, COLLISION_MARGIN
    mov cx, [enemy_y]
    add cx, COLLISION_MARGIN
    
    cmp bx, cx
    jle .chk_coin ; Player Bottom <= Enemy Top (Safe if above)
    
    mov word [game_over_flag], 1
    jmp .coll_done

.chk_coin:
    cmp word [coin_active], 1
    jne .chk_jerry
    
    mov ax, [player_lane]
    cmp ax, [coin_lane]
    jne .chk_jerry
    
    mov bx, [player_bottom_y]
    sub bx, reed_car_large_height
    mov cx, [coin_y]
    add cx, coin_height
    cmp bx, cx
    jge .chk_jerry
    
    mov bx, [player_bottom_y]
    mov cx, [coin_y]
    cmp bx, cx
    jle .chk_jerry
    
    inc word [coin_collected_count]
    mov word [coin_active], 0

.chk_jerry:
    cmp word [jerry_active], 1
    jne .coll_done
    
    mov ax, [player_lane]
    cmp ax, [jerry_lane]
    jne .coll_done
    
    mov bx, [player_bottom_y]
    sub bx, reed_car_large_height
    mov cx, [jerry_y]
    add cx, jerry_height
    cmp bx, cx
    jge .coll_done
    
    mov bx, [player_bottom_y]
    mov cx, [jerry_y]
    cmp bx, cx
    jle .coll_done
    
    inc word [fuel_collected_count]
    mov word [jerry_active], 0

.coll_done:
    popa
    ret

vsync:
    mov dx, 0x3DA
.v1: in al, dx
    test al, 8
    jnz .v1
.v2: in al, dx
    test al, 8
    jz .v2
    ret

get_random_lane_in_dx:
    push ax
    push bx
    push es
    mov ax, 0040h
    mov es, ax
    mov ax, [es:006Ch]
    mov bx, 3
    xor dx, dx
    div bx
    pop es
    pop bx
    pop ax
    ret

reset_gameplay_state:
    pusha
    mov word [coin_active], 0
    mov word [jerry_active], 0
    mov word [coin_collected_count], 0
    mov word [fuel_collected_count], 0
    mov word [game_over_flag], 0
    mov word [player_lane], 1
    mov word [player_bottom_y], PLAYER_BOTTOM_LIMIT
    mov word [enemy_y], -60
    popa
    ret

; -------------------------
; INTRO / MENU ROUTINES (Keep original Int 10h logic for menus)
; -------------------------
draw_intro_screen:
    pusha
    mov ax, 0A000h
    mov es, ax
    mov di, 40  
    add di, 8320
    mov word [ptr_0], var_0
    mov word [ptr_1], var_1
    mov word [ptr_2], var_2
    mov word [ptr_3], var_3
    mov word [ptr_4], var_4
    mov dx, IMG_HEIGHT
.draw_row_loop:
    mov si, [ptr_0]
    mov cx, CHUNK_WIDTH
    rep movsb
    mov [ptr_0], si
    mov si, [ptr_1]
    mov cx, CHUNK_WIDTH
    rep movsb
    mov [ptr_1], si
    mov si, [ptr_2]
    mov cx, CHUNK_WIDTH
    rep movsb
    mov [ptr_2], si
    mov si, [ptr_3]
    mov cx, CHUNK_WIDTH
    rep movsb
    mov [ptr_3], si
    mov si, [ptr_4]
    mov cx, CHUNK_WIDTH
    rep movsb
    mov [ptr_4], si
    add di, PADDING
    dec dx
    jnz .draw_row_loop
    popa
    ret

wait_for_menu_key:
    mov ah, 0
    int 16h
    ret

show_start_screen:
    pusha
    mov ax, 0A000h
    mov es, ax
    xor di, di
    mov al, 0
    mov cx, 64000
    rep stosb
    
    mov dh, 8
    mov dl, 10
    mov si, start_msg1
    call print_string_at
    mov dh, 12
    mov dl, 6
    mov si, start_msg2
    call print_string_at
    mov dh, 14
    mov dl, 2
    mov si, start_msg3
    call print_string_at
    popa
    ret

wait_for_game_start:
    pusha
.wloop:
    cmp word [game_started_flag], 1
    je .wdone
    jmp .wloop
.wdone:
    mov word [game_started_flag], 0
    popa
    ret

clear_screen_with_color:
    pusha
    mov ax, 0A000h
    mov es, ax
    xor di, di
    mov cx, 64000
    rep stosb
    popa
    ret

show_instruction_screen:
    mov al, 82
    call clear_screen_with_color
    mov dh, 3
    mov dl, 10
    mov si, instr_title
    call print_string_at
    mov dh, 6
    mov dl, 2
    mov si, instr_line1
    call print_string_at
    mov dh, 8
    mov dl, 2
    mov si, instr_line2
    call print_string_at
    mov dh, 10
    mov dl, 2
    mov si, instr_line3
    call print_string_at
    mov dh, 12
    mov dl, 2
    mov si, instr_line4
    call print_string_at
    call wait_for_menu_key
    ret

show_game_over_screen:
    mov al, 40 ; Red
    call clear_screen_with_color
    mov dh, 10
    mov dl, 8
    mov si, game_over_msg
    call print_string_at
    call wait_for_menu_key
    ret

collect_player_details:
    pusha
    mov al, 0
    call clear_screen_with_color
    mov dh, 4
    mov dl, 4
    mov si, input_name_prompt
    call print_string_at
    mov dh, 6
    mov dl, 4
    mov si, input_roll_prompt
    call print_string_at
    ; Simple wait key to bypass typing logic for brevity/safety in this merge
    call wait_for_menu_key
    popa
    ret

; ISR Stubs (Minimal implementation for stability)
install_keyboard_isr:
    pusha
    push ds
    push es
    mov ax, cs
    mov ds, ax
    xor ax, ax
    mov es, ax
    mov bx, 9 * 4
    mov ax, [es:bx]
    mov [old_keyboard_isr_offset], ax
    mov ax, [es:bx+2]
    mov [old_keyboard_isr_segment], ax
    cli
    mov word [es:bx], keyboard_isr
    mov ax, cs
    mov word [es:bx+2], ax
    sti
    pop es
    pop ds
    popa
    ret

restore_keyboard_isr:
    pusha
    push ds
    push es
    mov ax, cs
    mov ds, ax
    xor ax, ax
    mov es, ax
    mov bx, 9 * 4
    cli
    mov ax, [old_keyboard_isr_offset]
    mov [es:bx], ax
    mov ax, [old_keyboard_isr_segment]
    mov [es:bx+2], ax
    sti
    pop es
    pop ds
    popa
    ret

keyboard_isr:
    push ax
    push ds
    mov ax, cs
    mov ds, ax
    mov word [game_started_flag], 1
    pop ds
    pop ax
    jmp far [old_keyboard_isr_offset]

install_confirm_isr:
    pusha
    push ds
    push es
    mov ax, cs
    mov ds, ax
    xor ax, ax
    mov es, ax
    mov bx, 60h * 4
    mov ax, [es:bx]
    mov [old_confirm_isr_offset], ax
    mov ax, [es:bx+2]
    mov [old_confirm_isr_segment], ax
    cli
    mov word [es:bx], confirm_screen_handler
    mov ax, cs
    mov word [es:bx+2], ax
    sti
    pop es
    pop ds
    popa
    ret

restore_confirm_isr:
    pusha
    push ds
    push es
    mov ax, cs
    mov ds, ax
    xor ax, ax
    mov es, ax
    mov bx, 60h * 4
    cli
    mov ax, [old_confirm_isr_offset]
    mov [es:bx], ax
    mov ax, [old_confirm_isr_segment]
    mov [es:bx+2], ax
    sti
    pop es
    pop ds
    popa
    ret

confirm_screen_handler:
    pusha
    ; Simplistic handler: assume N (resume) to avoid graphical glitches in double buffered mode for now
    mov word [quit_requested], 0
    popa
    iret

setup_palette:
    pusha
    
    ; --- ORIGINAL GAME COLORS (unchanged) ---
    
    ; Color 2 - Green (for grass)
    mov dx, 0x03C8
    mov al, 2
    out dx, al
    inc dx
    mov al, 0       ; R
    out dx, al
    mov al, 10      ; G (bright green)
    out dx, al
    mov al, 0       ; B
    out dx, al
    
    ; Color 4 - Red (for barriers) - IMPROVED
    mov dx, 0x03C8
    mov al, 4
    out dx, al
    inc dx
    mov al, 52      ; R (reduced from 63)
    out dx, al
    mov al, 0       ; G
    out dx, al
    mov al, 0       ; B
    out dx, al
    
    ; Color 8 - Gray (for road)
    mov dx, 0x03C8
    mov al, 8
    out dx, al
    inc dx
    mov al, 32      ; R
    out dx, al
    mov al, 32      ; G
    out dx, al
    mov al, 32      ; B
    out dx, al
    
    ; Color 9 - Dark Cyan/Blue (for enemy car body/windows)
    mov dx, 0x03C8
    mov al, 9
    out dx, al
    inc dx
    mov al, 0       ; R
    out dx, al
    mov al, 40      ; G
    out dx, al
    mov al, 40      ; B
    out dx, al
    
    ; Color 12 - Dark Blue (for enemy car body/windows)
    mov dx, 0x03C8
    mov al, 12
    out dx, al
    inc dx
    mov al, 0       ; R
    out dx, al
    mov al, 0       ; G
    out dx, al
    mov al, 32      ; B
    out dx, al
    
    ; Color 15 - White
    mov dx, 0x03C8
    mov al, 15
    out dx, al
    inc dx
    mov al, 63      ; R
    out dx, al
    mov al, 63      ; G
    out dx, al
    mov al, 63      ; B
    out dx, al
    
    ; Color 17 - Medium Blue (for enemy car body/windows)
    mov dx, 0x03C8
    mov al, 17
    out dx, al
    inc dx
    mov al, 0       ; R
    out dx, al
    mov al, 20      ; G
    out dx, al
    mov al, 60      ; B
    out dx, al
    
    ; Color 25 - Dark gray
    mov dx, 0x03C8
    mov al, 25
    out dx, al
    inc dx
    mov al, 20      ; R
    out dx, al
    mov al, 20      ; G
    out dx, al
    mov al, 20      ; B
    out dx, al
    
    ; Color 28 - Medium gray
    mov dx, 0x03C8
    mov al, 28
    out dx, al
    inc dx
    mov al, 28      ; R
    out dx, al
    mov al, 28      ; G
    out dx, al
    mov al, 28      ; B
    out dx, al
    
    ; Color 39 - Yellow (for red car headlights) - IMPROVED
    mov dx, 0x03C8
    mov al, 39
    out dx, al
    inc dx
    mov al, 0      ; R
    out dx, al
    mov al, 0      ; G (slightly reduced)
    out dx, al
    mov al, 0       ; B
    out dx, al
    
    ; Color 74 - Very Dark gray (for car shadows) - IMPROVED
    mov dx, 0x03C8
    mov al, 74
    out dx, al
    inc dx
    mov al, 8       ; R (reduced from 10)
    out dx, al
    mov al, 8       ; G
    out dx, al
    mov al, 8       ; B
    out dx, al

    ; Color 75 - Dark gray
    mov dx, 0x03C8
    mov al, 75
    out dx, al
    inc dx
    mov al, 8
    out dx, al
    mov al, 8
    out dx, al
    mov al, 8
    out dx, al
    
    ; Color 76 - Dark gray
    mov dx, 0x03C8
    mov al, 76
    out dx, al
    inc dx
    mov al, 12
    out dx, al
    mov al, 12
    out dx, al
    mov al, 12
    out dx, al

    ; Color 77 - Medium gray
    mov dx, 0x03C8
    mov al, 77
    out dx, al
    inc dx
    mov al, 25      ; R
    out dx, al
    mov al, 25      ; G
    out dx, al
    mov al, 25      ; B
    out dx, al
    
    ; Color 82 - Light gray (for road) - KEY COLOR
    mov dx, 0x03C8
    mov al, 82
    out dx, al
    inc dx
    mov al, 36      ; R
    out dx, al
    mov al, 36      ; G
    out dx, al
    mov al, 36      ; B
    out dx, al
    
    ; Color 87 - Light gray - IMPROVED
    mov dx, 0x03C8
    mov al, 87
    out dx, al
    inc dx
    mov al, 48      ; R (reduced from 56)
    out dx, al
    mov al, 48      ; G
    out dx, al
    mov al, 48      ; B
    out dx, al
    
    ; Color 89 - White (for lanes)
    mov dx, 0x03C8
    mov al, 89
    out dx, al
    inc dx
    mov al, 63      ; R
    out dx, al
    mov al, 63      ; G
    out dx, al
    mov al, 63      ; B
    out dx, al
    
    ; --- UPDATED RED CAR COLORS (PRIMARY CHANGES) ---
    
    ; Color 90 - Medium Red (car body base) - IMPROVED
    mov dx, 0x03C8
    mov al, 90
    out dx, al
    inc dx
    mov al, 10      ; R (balanced mid-tone)
    out dx, al
    mov al, 0       ; G
    out dx, al
    mov al, 0       ; B
    out dx, al
    
    ; Color 94 - Dark Red (shadows) - IMPROVED
    mov dx, 0x03C8
    mov al, 94
    out dx, al
    inc dx
    mov al, 28      ; R (slightly brighter for visibility)
    out dx, al
    mov al, 0       ; G
    out dx, al
    mov al, 0       ; B
    out dx, al
    
    ; Color 96 - Medium Dark Red - IMPROVED
    mov dx, 0x03C8
    mov al, 96
    out dx, al
    inc dx
    mov al, 0      ; R (better mid-tone)
    out dx, al
    mov al, 0       ; G
    out dx, al
    mov al, 0       ; B
    out dx, al
    
    ; Color 104 - Bright Red (car body main) - IMPROVED
    mov dx, 0x03C8
    mov al, 104
    out dx, al
    inc dx
    mov al, 40      ; R (vibrant main body color)
    out dx, al
    mov al, 0       ; G
    out dx, al
    mov al, 0       ; B
    out dx, al
    
    ; Color 105 - Light red/pink (highlights) - IMPROVED
    mov dx, 0x03C8
    mov al, 105
    out dx, al
    inc dx
    mov al, 63      ; R (full brightness for highlights)
    out dx, al
    mov al, 15      ; G (slight warmth)
    out dx, al
    mov al, 15      ; B (slight warmth)
    out dx, al
    
    ; Color 109 - Cyan (for enemy car body/highlights)
    mov dx, 0x03C8
    mov al, 109
    out dx, al
    inc dx
    mov al, 20      ; R
    out dx, al
    mov al, 50      ; G
    out dx, al
    mov al, 60      ; B
    out dx, al
    
    ; Color 110 - Bright yellow
    mov dx, 0x03C8
    mov al, 110
    out dx, al
    inc dx
    mov al, 63      ; R
    out dx, al
    mov al, 63      ; G
    out dx, al
    mov al, 10      ; B
    out dx, al
    
    ; Color 120 - Green (for grass)
    mov dx, 0x03C8
    mov al, 120
    out dx, al
    inc dx
    mov al, 0       ; R
    out dx, al
    mov al, 36      ; G (bright green)
    out dx, al
    mov al, 0       ; B
    out dx, al
    
    ; Color 121 - Light green
    mov dx, 0x03C8
    mov al, 121
    out dx, al
    inc dx
    mov al, 20      ; R
    out dx, al
    mov al, 60      ; G
    out dx, al
    mov al, 20      ; B
    out dx, al

    ; Color 122 - Medium Blue (for enemy car body/highlights)
    mov dx, 0x03C8
    mov al, 122
    out dx, al
    inc dx
    mov al, 0       ; R
    out dx, al
    mov al, 15      ; G
    out dx, al
    mov al, 55      ; B
    out dx, al
    
    ; Color 140 - Bright Cyan (for enemy car body/highlights)
    mov dx, 0x03C8
    mov al, 140
    out dx, al
    inc dx
    mov al, 0       ; R
    out dx, al
    mov al, 60      ; G
    out dx, al
    mov al, 63      ; B
    out dx, al
    
    ; Color 146 - Dark Blue/Purple (for enemy car shadows)
    mov dx, 0x03C8
    mov al, 146
    out dx, al
    inc dx
    mov al, 0       ; R
    out dx, al
    mov al, 45      ; G
    out dx, al
    mov al, 50      ; B
    out dx, al
    
    ; Color 152 - Light Cyan (for enemy car highlights)
    mov dx, 0x03C8
    mov al, 152
    out dx, al
    inc dx
    mov al, 10      ; R
    out dx, al
    mov al, 63      ; G
    out dx, al
    mov al, 63      ; B
    out dx, al
    
    ; Color 153 - Light gray/white
    mov dx, 0x03C8
    mov al, 153
    out dx, al
    inc dx
    mov al, 50      ; R
    out dx, al
    mov al, 50      ; G
    out dx, al
    mov al, 50      ; B
    out dx, al
    
    ; Color 207 - Brown/tan
    mov dx, 0x03C8
    mov al, 207
    out dx, al
    inc dx
    mov al, 45      ; R
    out dx, al
    mov al, 30      ; G
    out dx, al
    mov al, 15      ; B
    out dx, al
    
    ; Color 221 - Pink/light red (for player car highlights)
    mov dx, 0x03C8
    mov al, 221
    out dx, al
    inc dx
    mov al, 63      ; R
    out dx, al
    mov al, 35      ; G
    out dx, al
    mov al, 35      ; B
    out dx, al

    ; Color 226 - Very light gray/off-white
    mov dx, 0x03C8
    mov al, 226
    out dx, al
    inc dx
    mov al, 45      ; R
    out dx, al
    mov al, 45      ; G
    out dx, al
    mov al, 45      ; B
    out dx, al
    
    ; Color 253 - GOLD (for coin)
    mov dx, 0x03C8
    mov al, 253
    out dx, al
    inc dx
    mov al, 63      ; R
    out dx, al
    mov al, 51      ; G
    out dx, al
    mov al, 0       ; B
    out dx, al
    
    ; Color 254 - Bright yellow (for headlights and coin)
    mov dx, 0x03C8
    mov al, 254
    out dx, al
    inc dx
    mov al, 63      ; R
    out dx, al
    mov al, 63      ; G
    out dx, al
    mov al, 0       ; B
    out dx, al

    ; --- BLUE CAR COLORS (unchanged) ---
    
    ; Color 42 - Dark Grey (for tires)
    mov dx, 0x03C8
    mov al, 42
    out dx, al
    inc dx
    mov al, 8       ; R
    out dx, al
    mov al, 8       ; G
    out dx, al
    mov al, 8       ; B
    out dx, al

    ; Color 66 - Bright Yellow (headlights)
    mov dx, 0x03C8
    mov al, 66
    out dx, al
    inc dx
    mov al, 63      ; R
    out dx, al
    mov al, 63      ; G
    out dx, al
    mov al, 20      ; B
    out dx, al

    ; Color 78 - Very Dark Blue (windshield)
    mov dx, 0x03C8
    mov al, 78
    out dx, al
    inc dx
    mov al, 0       ; R
    out dx, al
    mov al, 0       ; G
    out dx, al
    mov al, 20      ; B
    out dx, al

    ; Color 126 - BLACK (middle stripe)
    mov dx, 0x03C8
    mov al, 126
    out dx, al
    inc dx
    mov al, 0       ; R
    out dx, al
    mov al, 0       ; G
    out dx, al
    mov al, 0       ; B
    out dx, al

    ; Color 128 - Very Dark Blue (body shadow)
    mov dx, 0x03C8
    mov al, 128
    out dx, al
    inc dx
    mov al, 0       ; R
    out dx, al
    mov al, 50       ; G
    out dx, al
    mov al, 0       ; B
    out dx, al

    ; Color 130 - Dark Blue (main body)
    mov dx, 0x03C8
    mov al, 130
    out dx, al
    inc dx
    mov al, 0       ; R
    out dx, al
    mov al, 20      ; G
    out dx, al
    mov al, 40      ; B
    out dx, al

    ; Color 132 - Medium Blue (windshield light)
    mov dx, 0x03C8
    mov al, 132
    out dx, al
    inc dx
    mov al, 0       ; R
    out dx, al
    mov al, 0      ; G
    out dx, al
    mov al, 0      ; B
    out dx, al

    ; Color 134 - Medium Bright Blue (body highlight)
    mov dx, 0x03C8
    mov al, 134
    out dx, al
    inc dx
    mov al, 0      ; R
    out dx, al
    mov al, 30      ; G
    out dx, al
    mov al, 0      ; B
    out dx, al

    ; Color 136 - Bright Blue (edges/highlights)
    mov dx, 0x03C8
    mov al, 136
    out dx, al
    inc dx
    mov al, 0      ; R
    out dx, al
    mov al, 40      ; G
    out dx, al
    mov al, 50      ; B
    out dx, al

    ; Color 137 - Almost Black (bumper)
    mov dx, 0x03C8
    mov al, 137
    out dx, al
    inc dx
    mov al, 5       ; R
    out dx, al
    mov al, 5       ; G
    out dx, al
    mov al, 35       ; B
    out dx, al

    ; Color 141 - Light Blue (trim)
    mov dx, 0x03C8
    mov al, 141
    out dx, al
    inc dx
    mov al, 0      ; R
    out dx, al
    mov al, 0      ; G
    out dx, al
    mov al, 10      ; B
    out dx, al
    
    popa
    ret

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

; =========================================================
; DATA SECTION
; =========================================================
section .data
    back_buffer_seg dw 0
    
    CHUNK_WIDTH      EQU 48          
    NUM_VARS         EQU 5
    TOTAL_IMG_W      EQU CHUNK_WIDTH * NUM_VARS  
    SCREEN_W         EQU 320
    PADDING          EQU SCREEN_W - TOTAL_IMG_W  
    IMG_HEIGHT       EQU 148

    reed_car_large_width  EQU 45
    reed_car_large_height EQU 59
    blue_car_large_width  EQU 45
    blue_car_large_height EQU 59
    coin_width            EQU 20
    coin_height           EQU 20
    jerry_width           EQU 27
    jerry_height          EQU 27

    PLAYER_VERTICAL_STEP  EQU 8
    PLAYER_TOP_LIMIT      EQU 30
    PLAYER_BOTTOM_LIMIT   EQU 199
    COIN_SPAWN_THRESHOLD  EQU 2    
    JERRY_SPAWN_THRESHOLD EQU 3    
    COLLISION_MARGIN      EQU 4   

    HUD_SCORE_ROW         EQU 1
    HUD_SCORE_COL         EQU 6
    HUD_FUEL_ROW          EQU 1
    HUD_FUEL_COL          EQU 26

    ptr_0 dw 0
    ptr_1 dw 0
    ptr_2 dw 0
    ptr_3 dw 0
    ptr_4 dw 0

    player_lane        dw 1
    player_bottom_y    dw 199
    player_coin_total  dw 0
    
    scroll_offset      dw 0
    lane_positions:    dw 62, 138, 212
    coin_positions:    dw 72, 145, 218
    jerry_positions:   dw 74, 147, 220
        
    enemy_lane         dw 0
    enemy_y            dw -60
    
    coin_y             dw 0
    coin_lane          dw 0
    coin_active        dw 0
    coin_collected_count dw 0
    coin_spawn_counter dw 0

    jerry_y            dw 0
    jerry_lane         dw 0
    jerry_active       dw 0 
    fuel_collected_count dw 0
    jerry_spawn_counter dw 0

    game_started_flag  dw 0
    game_paused_flag   dw 0
    confirm_active     dw 0
    quit_requested     dw 0
    game_over_flag     dw 0

    old_keyboard_isr_offset  dw 0
    old_keyboard_isr_segment dw 0
    old_confirm_isr_offset   dw 0
    old_confirm_isr_segment  dw 0
    
    start_msg1         db 'COAL Racing', 0
    start_msg2         db 'Press any key to start', 0
    start_msg3         db 'Arrow keys to drive, ESC to pause', 0
    input_name_prompt  db 'Enter Player Name:',0
    input_roll_prompt  db 'Enter Roll Number:',0
    instr_title        db 'Instructions',0
    instr_line1        db '- Arrow keys move red car',0
    instr_line2        db '- Collect coins & fuel cans',0
    instr_line3        db '- ESC: Pause game',0
    instr_line4        db '- Avoid blue cars',0
    
    hud_score_label    db 'Coins: ', 0
    hud_fuel_label     db 'Fuel: ', 0
    game_over_msg      db 'GAME OVER! Press Key...', 0

    CONFIRM_LEFT   EQU 80
    CONFIRM_RIGHT  EQU 240
    CONFIRM_TOP    EQU 80
    CONFIRM_BOTTOM EQU 120

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
   sprite_data:
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

coin_sprite:
   DB  0,  0,  0,  0,  0,  0, 74, 25, 207, 207, 207, 207, 25, 74,  0,  0,  0,  0,  0,  0
    DB 0, 0, 0, 0, 90, 207, 253, 253, 253, 253, 253, 253, 253, 66, 207, 90,  0,  0,  0, 0
    DB 0, 0, 0, 76, 253, 253, 253, 253, 253, 253, 253, 66, 253, 253, 253, 253, 76,  0,  0,  0
    DB 0, 0, 76, 253, 253, 253, 253, 66, 253, 66, 66, 66, 66, 253, 253, 253, 253, 76,  0,  0
    DB 0, 90, 253, 253, 253, 66, 253, 253, 66, 66, 66, 253, 253, 253, 66, 253, 253, 253, 90,  0
    DB 0, 207, 253, 253, 66, 253, 66, 66, 66, 66, 253, 253, 253, 253, 253, 66, 253, 253, 207,  0
    DB 74, 253, 253, 253, 253, 66, 66, 66, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 74
    DB 25, 253, 253, 253, 66, 66, 66, 253, 253, 253, 253, 253, 253, 253, 253, 253, 66, 253, 253, 25
    DB 207, 253, 253, 66, 66, 66, 253, 253, 253, 253, 253, 253, 253, 253, 253, 66, 66, 253, 253, 207
    DB 207, 253, 66, 66, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 66, 66, 66, 253, 253, 207
    DB 207, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 66, 66, 66, 66, 66, 253, 253, 207
    DB 207, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 66, 66, 66, 66, 66, 66, 253, 253, 207
    DB 25, 253, 253, 253, 253, 253, 253, 253, 253, 66, 66, 66, 66, 66, 66, 66, 66, 253, 253, 25
    DB 74, 253, 253, 253, 253, 253, 253, 253, 66, 66, 66, 66, 66, 66, 66, 66, 253, 253, 253, 74
    DB  0, 207, 253, 253, 253, 253, 66, 66, 66, 66, 66, 66, 66, 66, 253, 253, 253, 253, 207,  0
    DB  0, 90, 253, 253, 253, 66, 66, 66, 66, 66, 66, 66, 66, 253, 253, 253, 253, 253, 90, 0
    DB  0,  0, 76, 253, 253, 253, 66, 66, 66, 66, 66, 66, 253, 253, 253, 253, 253, 76, 0, 0
    DB  0,  0,  0, 76, 66, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 253, 76, 0, 0,  0
    DB  0,  0,  0,  0, 74, 207, 66, 253, 253, 253, 253, 253, 253, 253, 207, 90, 0, 0, 0,  0
    DB  0,  0,  0,  0,  0,  0, 74, 25, 207, 207, 207, 207, 25, 74,  0, 0, 0,  0,  0,  0

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

var_0:
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 16, 1, 17, 0, 0, 10, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0
    DB 0, 4, 21, 11, 21, 3, 0, 21, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 19, 21, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 19, 21, 3, 0, 1, 21, 3, 0, 0, 0, 0, 0, 0
    DB 0, 11, 21, 16, 0, 0, 0, 21, 10, 10, 10, 1, 0, 3, 10, 10, 10, 16, 16, 10, 2, 0, 1, 10, 10, 4, 0, 0, 2, 10, 10, 3, 0, 0, 0, 1, 21, 3, 0, 2, 21, 3, 0, 10, 10, 10, 3, 0
    DB 0, 1, 11, 21, 10, 16, 0, 21, 11, 3, 21, 11, 0, 3, 10, 10, 21, 10, 0, 21, 4, 16, 21, 11, 10, 21, 10, 2, 21, 10, 10, 21, 2, 0, 0, 1, 21, 11, 11, 11, 21, 3, 0, 4, 10, 11, 21, 17
    DB 0, 0, 0, 1, 21, 11, 0, 21, 10, 0, 11, 11, 0, 10, 10, 10, 21, 10, 0, 21, 3, 2, 21, 10, 10, 10, 3, 10, 21, 10, 10, 10, 2, 0, 0, 1, 21, 10, 19, 3, 21, 3, 16, 10, 10, 10, 21, 1
    DB 0, 10, 11, 10, 21, 10, 0, 21, 10, 0, 11, 11, 0, 21, 11, 10, 21, 10, 0, 21, 4, 16, 21, 11, 10, 10, 16, 2, 21, 10, 10, 10, 0, 0, 0, 1, 21, 3, 0, 1, 21, 3, 2, 21, 10, 10, 21, 1
    DB 0, 18, 4, 10, 3, 0, 0, 3, 1, 0, 2, 2, 0, 1, 10, 3, 2, 1, 16, 21, 10, 0, 16, 4, 10, 4, 16, 0, 19, 10, 10, 3, 0, 0, 0, 16, 3, 18, 0, 16, 3, 18, 0, 2, 10, 2, 3, 16
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 21, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 19, 18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 16, 1, 16, 0, 0, 0, 0, 17, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 1, 16, 0, 0, 16, 18, 19, 16, 0, 16, 18, 19, 16, 0, 0, 1, 18, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 3, 15, 6, 15, 3, 0, 0, 6, 15, 4, 0, 15, 3, 0, 0, 0, 0, 0, 16, 15, 6, 15, 4, 0, 15, 6, 5, 3, 16, 15, 5, 5, 2, 18, 15, 6, 6, 5, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 1, 18, 0, 15, 5, 0, 4, 15, 6, 5, 0, 15, 3, 0, 0, 16, 16, 0, 4, 15, 0, 3, 15, 18, 15, 6, 4, 16, 1, 15, 5, 4, 0, 4, 15, 0, 2, 15, 2, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 4, 15, 3, 0, 2, 15, 3, 15, 6, 16, 15, 3, 0, 1, 6, 6, 2, 5, 15, 0, 2, 15, 18, 17, 19, 15, 15, 0, 19, 1, 15, 15, 16, 5, 5, 5, 15, 2, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 4, 15, 5, 4, 2, 3, 5, 4, 15, 15, 17, 15, 15, 4, 17, 0, 0, 0, 1, 15, 4, 15, 6, 16, 4, 3, 15, 5, 18, 4, 3, 15, 5, 0, 3, 4, 15, 6, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 19, 2, 2, 3, 1, 0, 0, 0, 1, 17, 0, 1, 2, 3, 16, 0, 0, 0, 0, 18, 4, 3, 0, 16, 3, 4, 2, 0, 16, 3, 4, 1, 0, 0, 2, 4, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

var_1:
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 2, 10, 4, 10, 0, 2, 10, 10, 3, 0, 0, 2, 10, 10, 3, 0, 17, 10, 10, 10, 10, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 10, 21, 10, 10, 4, 21, 10, 10, 21, 3, 2, 21, 10, 4, 21, 10, 17, 21, 10, 3, 21, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 10, 21, 0, 0, 10, 21, 0, 0, 21, 10, 10, 21, 0, 0, 21, 11, 16, 21, 3, 0, 21, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 10, 21, 16, 0, 2, 21, 10, 10, 21, 2, 1, 21, 10, 10, 21, 2, 18, 21, 4, 0, 21, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 18, 3, 0, 0, 0, 18, 10, 10, 18, 0, 0, 18, 4, 10, 1, 0, 16, 3, 18, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 16, 16, 16, 0, 16, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 16, 17, 16, 16, 18, 16, 16, 18, 16
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 16, 18, 16, 19, 1, 1, 19, 1, 1, 1, 17, 19
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 17, 18, 18, 19, 2, 2, 1, 4, 4, 2, 2, 3, 2, 2
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 18, 18, 18, 2, 19, 2, 3, 5, 12, 12, 12, 13, 12, 16, 2, 2
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 18, 2, 1, 2, 2, 3, 4, 5, 12, 12, 12, 12, 12, 12, 3, 0, 0, 1
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 16, 19, 19, 2, 3, 14, 14, 6, 12, 12, 13, 12, 12, 3, 2, 0, 0, 2, 14, 20, 20
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 16, 18, 17, 1, 2, 3, 14, 5, 6, 12, 13, 13, 12, 12, 2, 16, 0, 18, 14, 15, 20, 20, 20, 20
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 18, 16, 1, 1, 2, 14, 5, 12, 12, 13, 12, 12, 12, 17, 0, 16, 14, 15, 20, 20, 20, 20, 20, 20, 20
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 18, 1, 18, 1, 2, 4, 5, 12, 12, 12, 13, 12, 2, 0, 0, 3, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 18, 18, 1, 1, 2, 3, 4, 12, 12, 12, 12, 12, 12, 16, 0, 18, 14, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 17, 16, 19, 2, 3, 5, 12, 12, 13, 12, 12, 2, 0, 0, 2, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 18, 1, 2, 3, 12, 13, 13, 13, 12, 2, 0, 0, 14, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 18, 17, 1, 1, 12, 13, 13, 12, 1, 0, 16, 14, 20, 20, 20, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 19, 2, 0, 12, 12, 1, 0, 17, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 16, 18, 3, 18, 0, 0, 18, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 14, 2
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 17, 16, 19, 2, 19, 0, 16, 14, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 14, 0, 0, 18
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 18, 1, 1, 17, 16, 14, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 1, 0, 16, 10, 10, 10
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 19, 17, 0, 0, 15, 20, 20, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 14, 16, 0, 2, 10, 11, 10, 2, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 19, 18, 14, 15, 0, 2, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 15, 2, 0, 0, 10, 11, 11, 10, 16, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 18, 1, 20, 20, 15, 0, 3, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 14, 16, 0, 17, 10, 11, 11, 2, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 19, 1, 14, 20, 20, 15, 0, 1, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 14, 16, 0, 2, 10, 11, 11, 10, 0, 0, 0, 0, 0, 0, 0, 16
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 17, 16, 19, 1, 14, 20, 20, 15, 16, 2, 20, 20, 15, 20, 20, 20, 20, 20, 14, 0, 0, 3, 11, 21, 10, 10, 10, 18, 0, 0, 0, 0, 0, 0, 0, 12
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 17, 2, 2, 15, 20, 20, 15, 0, 2, 20, 20, 20, 20, 15, 14, 0, 0, 3, 11, 21, 21, 10, 10, 10, 19, 0, 0, 0, 0, 0, 0, 0, 12, 13
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 18, 2, 2, 15, 20, 20, 15, 0, 2, 20, 20, 14, 0, 0, 10, 11, 21, 11, 11, 10, 10, 11, 1, 0, 0, 0, 0, 0, 0, 0, 12, 12, 12
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 17, 19, 2, 2, 15, 20, 20, 15, 0, 16, 0, 0, 1, 0, 3, 11, 11, 10, 10, 11, 21, 2, 0, 0, 0, 0, 0, 0, 0, 1, 12, 12, 12
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 18, 1, 19, 2, 15, 15, 14, 16, 16, 14, 14, 14, 14, 0, 3, 10, 10, 11, 21, 3, 0, 0, 0, 0, 0, 0, 0, 16, 12, 12, 12, 12
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 17, 18, 1, 3, 14, 3, 0, 3, 14, 14, 14, 14, 14, 3, 0, 10, 21, 11, 10, 0, 0, 0, 0, 0, 0, 0, 0, 12, 13, 12, 12, 12
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 19, 18, 3, 14, 16, 16, 14, 14, 14, 14, 14, 14, 18, 2, 21, 11, 11, 0, 0, 0, 0, 0, 0, 0, 0, 2, 12, 12, 12, 12, 12
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 17, 1, 2, 1, 2, 0, 14, 14, 14, 14, 14, 17, 0, 11, 11, 11, 1, 0, 0, 0, 0, 0, 0, 0, 0, 12, 13, 13, 12, 12, 12
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 17, 18, 2, 19, 10, 10, 0, 14, 14, 14, 14, 1, 0, 19, 21, 21, 10, 0, 0, 0, 0, 0, 0, 0, 0, 1, 13, 13, 12, 12, 12, 12
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 18, 1, 1, 19, 11, 2, 0, 14, 14, 14, 3, 0, 0, 4, 21, 11, 2, 0, 0, 0, 0, 0, 0, 0, 0, 3, 12, 12, 13, 12, 13, 13
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 18, 1, 2, 2, 10, 11, 1, 0, 14, 14, 14, 16, 1, 0, 10, 21, 10, 0, 0, 0, 0, 0, 0, 0, 0, 16, 12, 12, 12, 12, 12, 12, 12
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 19, 1, 2, 2, 11, 11, 2, 0, 14, 14, 16, 14, 1, 0, 11, 21, 10, 0, 0, 0, 0, 0, 0, 0, 0, 1, 12, 12, 12, 12, 13, 12, 13
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 17, 19, 2, 1, 10, 11, 11, 2, 0, 14, 3, 16, 14, 16, 16, 11, 11, 18, 0, 0, 0, 0, 0, 0, 0, 0, 12, 12, 12, 12, 12, 12, 12, 12
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 17, 2, 2, 2, 11, 11, 11, 3, 0, 14, 17, 14, 14, 0, 18, 11, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12, 12, 12, 12, 12, 12, 12, 12
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 17, 1, 2, 2, 4, 11, 11, 11, 10, 0, 16, 1, 14, 14, 0, 18, 11, 10, 0, 0, 0, 0, 0, 0, 0, 0, 18, 12, 12, 12, 12, 12, 12, 12, 19
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 1, 2, 2, 10, 11, 11, 11, 10, 0, 0, 14, 14, 14, 16, 18, 21, 3, 0, 0, 0, 0, 0, 0, 0, 0, 2, 13, 13, 12, 12, 12, 12, 12, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 19, 1, 2, 2, 11, 11, 11, 11, 10, 0, 0, 14, 14, 14, 16, 17, 11, 3, 0, 0, 0, 0, 0, 0, 0, 0, 2, 12, 12, 12, 12, 12, 12, 3, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 17, 18, 2, 1, 1, 10, 11, 11, 11, 11, 16, 0, 2, 15, 8, 17, 0, 11, 2, 0, 0, 0, 0, 0, 0, 0, 0, 2, 12, 12, 12, 12, 12, 13, 1, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 17, 18, 2, 1, 2, 11, 11, 11, 11, 11, 2, 0, 1, 11, 8, 2, 0, 10, 2, 0, 0, 0, 0, 0, 0, 0, 0, 2, 12, 12, 12, 12, 13, 12, 0, 18
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 17, 18, 19, 2, 2, 11, 11, 11, 11, 21, 10, 0, 16, 11, 15, 19, 0, 10, 2, 0, 0, 0, 0, 0, 0, 0, 0, 2, 12, 12, 12, 12, 12, 12, 0, 3
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 17, 18, 2, 1, 3, 11, 11, 11, 11, 11, 11, 16, 0, 10, 11, 1, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 18, 12, 12, 13, 12, 13, 3, 0, 10
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 19, 1, 2, 2, 3, 11, 11, 11, 11, 11, 11, 2, 0, 2, 11, 11, 16, 16, 3, 0, 0, 0, 0, 0, 0, 0, 0, 16, 12, 12, 13, 12, 13, 2, 0, 10
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 18, 17, 2, 2, 3, 11, 11, 11, 11, 11, 11, 10, 0, 0, 10, 11, 10, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12, 12, 12, 12, 13, 2, 0, 11
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 1, 19, 3, 14, 2, 11, 11, 11, 11, 11, 11, 11, 18, 0, 10, 11, 21, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 12, 12, 12, 12, 19, 16, 11
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 17, 17, 19, 3, 3, 17, 11, 21, 11, 11, 11, 11, 11, 10, 0, 17, 11, 11, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 19, 12, 12, 12, 12, 18, 16, 11
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 18, 2, 3, 14, 0, 10, 21, 11, 11, 11, 11, 11, 11, 1, 0, 3, 11, 11, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12, 13, 12, 12, 18, 16, 10
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 1, 17, 2, 14, 14, 16, 16, 15, 11, 11, 11, 11, 11, 21, 11, 0, 0, 10, 11, 11, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 13, 12, 13, 2, 16, 10
    DB 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 16, 1, 17, 0, 14, 14, 2, 0, 14, 15, 15, 11, 21, 15, 11, 21, 3, 0, 18, 11, 11, 11, 1, 0, 0, 0, 0, 0, 0, 0, 0, 16, 12, 12, 12, 3, 16, 3
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 18, 2, 1, 14, 18, 3, 14, 16, 19, 20, 21, 15, 11, 20, 21, 11, 11, 18, 0, 3, 11, 21, 11, 16, 0, 0, 0, 0, 0, 0, 0, 0, 3, 12, 12, 12, 0, 1
    DB 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 19, 1, 16, 15, 14, 2, 14, 2, 0, 14, 20, 21, 15, 21, 15, 11, 21, 8, 0, 0, 10, 11, 11, 11, 16, 0, 0, 0, 0, 0, 0, 0, 0, 12, 12, 12, 16, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 18, 1, 16, 15, 20, 2, 2, 14, 16, 0, 20, 20, 15, 15, 15, 20, 11, 15, 15, 0, 0, 10, 11, 21, 11, 16, 0, 0, 0, 0, 0, 0, 0, 2, 12, 12, 2, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 16, 1, 18, 14, 20, 20, 17, 14, 14, 0, 2, 20, 20, 15, 15, 20, 15, 11, 20, 15, 0, 0, 10, 21, 21, 11, 16, 0, 0, 0, 0, 0, 0, 0, 12, 13, 12, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 18, 1, 19, 2, 20, 20, 20, 18, 14, 14, 0, 14, 20, 15, 20, 20, 20, 15, 11, 20, 15, 0, 16, 10, 11, 21, 11, 19, 0, 0, 0, 0, 0, 0, 0, 12, 13, 18
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 17, 18, 1, 1, 20, 20, 20, 20, 17, 2, 1, 0, 14, 20, 15, 20, 20, 20, 15, 21, 20, 15, 0, 0, 10, 21, 11, 10, 2, 0, 0, 0, 0, 0, 0, 17, 13, 12
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 17, 18, 2, 1, 3, 20, 20, 20, 15, 2, 16, 0, 0, 14, 20, 20, 20, 20, 20, 20, 15, 20, 15, 16, 0, 10, 11, 11, 11, 4, 0, 0, 0, 0, 0, 0, 2, 13
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 17, 2, 14, 17, 2, 20, 20, 20, 20, 14, 0, 0, 0, 15, 20, 20, 20, 20, 20, 20, 20, 20, 15, 2, 0, 2, 11, 11, 11, 11, 18, 0, 0, 0, 0, 0, 2
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 19, 19, 14, 14, 0, 2, 20, 20, 20, 20, 14, 16, 0, 0, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 14, 0, 16, 10, 11, 11, 11, 10, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 19, 1, 2, 3, 16, 0, 1, 15, 20, 20, 20, 15, 3, 0, 0, 14, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 16, 0, 2, 10, 21, 21, 11, 2, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 1, 19, 2, 18, 0, 0, 0, 14, 20, 20, 20, 20, 15, 1, 0, 2, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 14, 0, 16, 10, 11, 21, 21, 10, 18, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 18, 17, 2, 2, 1, 1, 0, 0, 14, 20, 20, 20, 20, 20, 14, 16, 0, 15, 20, 20, 20, 20, 20, 20, 15, 20, 20, 15, 2, 0, 19, 10, 11, 21, 21, 10
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 1, 19, 2, 2, 12, 2, 0, 0, 1, 15, 20, 20, 20, 20, 20, 14, 18, 14, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 1, 0, 18, 10, 11, 21
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 19, 16, 2, 14, 2, 13, 12, 18, 0, 0, 14, 15, 20, 20, 20, 20, 20, 14, 2, 14, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 14, 2, 0, 0, 10
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 18, 18, 2, 2, 12, 13, 12, 3, 16, 0, 0, 14, 20, 20, 20, 20, 20, 20, 15, 14, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 14, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 18, 1, 14, 17, 12, 13, 12, 12, 12, 16, 0, 16, 14, 20, 20, 20, 20, 20, 20, 15, 20, 15, 20, 20, 20, 20, 20, 20, 20, 20, 15, 20, 15
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 17, 1, 2, 2, 18, 12, 12, 12, 12, 12, 12, 17, 0, 0, 14, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 17, 19, 1, 2, 2, 12, 12, 12, 12, 12, 12, 12, 1, 0, 0, 1, 14, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 17, 1, 14, 18, 2, 12, 12, 12, 12, 12, 12, 12, 12, 4, 18, 0, 0, 18, 14, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 18, 1, 3, 2, 3, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 3, 18, 0, 0, 16, 2, 14, 15, 15, 20, 20, 20, 20, 20, 20
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 18, 1, 3, 17, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 1, 16, 0, 0, 0, 18, 3, 14, 14, 15, 15
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 17, 1, 1, 2, 19, 3, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 4, 4, 1, 16, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 17, 17, 2, 2, 1, 3, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 12, 12, 12, 12, 12, 12, 12, 4
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 18, 17, 2, 14, 0, 1, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 12, 12, 12, 12, 12, 4, 12, 12, 12, 12
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 18, 1, 1, 2, 0, 0, 0, 19, 1, 3, 12, 12, 12, 13, 12, 13, 13, 13, 12, 12, 12, 12, 12, 12, 12, 12
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 17, 17, 1, 2, 2, 16, 0, 0, 0, 0, 0, 16, 1, 3, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 18, 2, 2, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 19, 1, 1, 2, 2, 3, 4, 12
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 19, 19, 1, 2, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 18, 1, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 18, 19, 2, 3, 3, 18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 16, 19, 19, 2, 14, 2, 19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 18, 18, 1, 1, 3, 2, 1, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 17, 17, 19, 2, 3, 14, 2, 3, 10, 10, 11, 11, 10, 10, 10
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 18, 1, 2, 2, 14, 14, 3, 10, 11, 11, 21, 11
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 16, 18, 1, 1, 2, 2, 2, 1, 1, 3, 10
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 16, 17, 1, 19, 1, 1, 2, 19, 1
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 16, 17, 18, 16, 18, 18, 2
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 17
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 14, 15, 15, 14, 15, 15, 14, 0, 0, 0, 14, 15, 15, 1, 0, 0, 0, 6, 15, 15, 5, 15, 15, 5, 18, 0, 18, 6, 15, 15, 14, 15, 15, 3, 14, 15, 15, 3, 3, 15, 15
    DB 0, 0, 0, 0, 0, 0, 14, 21, 21, 21, 15, 21, 21, 15, 0, 0, 16, 15, 21, 21, 6, 0, 0, 0, 15, 21, 11, 15, 21, 21, 21, 15, 1, 15, 21, 11, 21, 15, 21, 21, 14, 6, 21, 21, 6, 15, 21, 21
    DB 0, 0, 0, 0, 0, 0, 15, 11, 21, 15, 3, 4, 4, 1, 0, 0, 14, 21, 21, 11, 15, 18, 0, 0, 15, 11, 21, 14, 2, 15, 11, 21, 14, 15, 11, 21, 14, 2, 3, 14, 19, 6, 21, 21, 6, 6, 21, 11
    DB 0, 0, 0, 0, 0, 0, 15, 11, 21, 14, 0, 0, 0, 0, 0, 0, 15, 21, 15, 11, 21, 14, 0, 0, 15, 11, 21, 3, 16, 15, 11, 11, 14, 15, 11, 21, 2, 0, 0, 0, 0, 6, 21, 21, 6, 6, 11, 11
    DB 0, 0, 0, 0, 0, 0, 15, 11, 21, 14, 0, 16, 0, 0, 0, 3, 21, 21, 15, 15, 11, 15, 16, 0, 15, 11, 11, 15, 15, 11, 21, 21, 14, 21, 11, 21, 3, 0, 0, 16, 0, 6, 21, 21, 6, 6, 21, 21
    DB 0, 0, 0, 0, 0, 0, 15, 11, 21, 14, 0, 0, 0, 0, 0, 15, 21, 21, 14, 15, 11, 21, 14, 0, 15, 11, 21, 15, 11, 11, 15, 2, 1, 21, 11, 21, 2, 0, 0, 0, 0, 15, 21, 21, 6, 6, 11, 21
    DB 0, 0, 0, 0, 0, 0, 15, 11, 21, 14, 0, 16, 16, 0, 19, 21, 21, 21, 21, 21, 11, 21, 15, 16, 15, 11, 11, 14, 15, 11, 21, 2, 16, 15, 11, 21, 2, 16, 18, 18, 0, 6, 21, 21, 6, 6, 11, 21
    DB 0, 0, 0, 0, 0, 0, 15, 21, 21, 21, 15, 15, 11, 14, 14, 21, 21, 15, 5, 5, 15, 11, 21, 3, 15, 21, 21, 3, 2, 21, 21, 15, 1, 15, 11, 11, 21, 15, 21, 21, 14, 6, 21, 21, 6, 6, 21, 21
    DB 0, 0, 0, 0, 0, 0, 19, 15, 21, 21, 15, 21, 21, 15, 15, 21, 21, 2, 0, 0, 14, 21, 11, 15, 15, 21, 21, 2, 0, 15, 11, 21, 14, 2, 15, 21, 21, 15, 21, 21, 3, 5, 21, 21, 14, 6, 21, 21
    DB 0, 0, 0, 0, 0, 0, 0, 0, 18, 19, 17, 1, 1, 16, 17, 1, 18, 0, 0, 0, 0, 1, 1, 18, 17, 1, 19, 0, 0, 0, 19, 18, 17, 0, 0, 19, 19, 17, 19, 19, 0, 16, 1, 19, 0, 16, 1, 19
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 6, 15, 5, 18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 20, 20, 15, 15, 20, 2, 0, 0, 0, 0, 0, 0, 0, 17, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 17, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 20, 15, 0, 1, 20, 5, 5, 15, 15, 20, 1, 4, 15, 15, 15, 3, 0, 15, 15, 15, 15, 0, 4, 15, 15, 15, 3
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 20, 15, 3, 15, 20, 1, 15, 20, 15, 3, 3, 20, 15, 3, 20, 20, 16, 20, 20, 14, 2, 0, 15, 20, 6, 3, 16
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 20, 20, 15, 15, 3, 0, 15, 20, 17, 0, 14, 20, 15, 15, 15, 15, 0, 1, 15, 15, 15, 1, 16, 5, 15, 20, 4
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 20, 15, 0, 0, 0, 0, 15, 20, 1, 0, 18, 20, 20, 6, 15, 4, 0, 15, 15, 15, 20, 2, 6, 15, 15, 20, 15
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 2, 0, 0, 0, 0, 2, 4, 16, 0, 0, 16, 14, 15, 6, 2, 0, 3, 6, 5, 1, 0, 2, 6, 6, 3, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

var_2:
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 16, 16
    DB 17, 16, 17, 16, 16, 17, 16, 0, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 16, 16, 16, 17, 16, 16
    DB 1, 17, 1, 1, 18, 19, 19, 17, 16, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 16, 19, 18, 18, 19, 1, 1
    DB 2, 2, 2, 2, 1, 2, 2, 1, 2, 16, 17, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 17, 1, 2, 2, 1, 1, 2, 3
    DB 0, 0, 16, 0, 14, 20, 20, 14, 1, 2, 1, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 19, 1, 1, 14, 20, 20, 1, 0, 18
    DB 14, 15, 20, 14, 16, 15, 20, 20, 14, 1, 2, 1, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 18, 18, 1, 19, 14, 20, 20, 15, 0, 15, 20
    DB 20, 20, 20, 20, 1, 0, 15, 20, 20, 14, 2, 2, 19, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 17, 18, 1, 2, 14, 20, 20, 15, 0, 2, 20, 20
    DB 20, 20, 20, 20, 15, 0, 2, 20, 20, 15, 14, 14, 1, 17, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 19, 18, 1, 14, 20, 20, 20, 18, 16, 15, 20, 20
    DB 20, 20, 20, 20, 20, 14, 0, 14, 20, 20, 15, 2, 2, 2, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 19, 1, 3, 15, 20, 20, 14, 0, 15, 20, 20, 20
    DB 20, 20, 20, 20, 20, 20, 14, 0, 15, 20, 20, 14, 2, 1, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 1, 1, 14, 20, 20, 15, 0, 14, 20, 20, 20, 20
    DB 20, 20, 20, 20, 20, 20, 20, 2, 16, 20, 20, 20, 2, 1, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 16, 0, 16, 0, 0, 16, 0, 17, 16, 17, 2, 15, 20, 15, 0, 2, 20, 15, 20, 20, 20
    DB 20, 20, 20, 15, 20, 20, 20, 20, 17, 2, 15, 14, 3, 1, 18, 17, 17, 16, 16, 16, 17, 17, 16, 16, 16, 17, 17, 17, 16, 16, 16, 18, 18, 16, 18, 17, 1, 2, 14, 14, 1, 1, 20, 20, 20, 20, 20, 20
    DB 20, 20, 20, 20, 20, 20, 15, 14, 16, 0, 16, 3, 3, 2, 2, 18, 1, 18, 1, 19, 19, 1, 1, 18, 19, 1, 19, 1, 1, 1, 2, 1, 2, 1, 1, 2, 1, 3, 2, 16, 0, 18, 14, 20, 20, 20, 20, 20
    DB 20, 20, 20, 20, 14, 17, 0, 0, 14, 14, 14, 14, 2, 2, 2, 3, 2, 2, 14, 2, 2, 2, 2, 2, 1, 1, 18, 2, 2, 2, 14, 2, 14, 14, 14, 14, 2, 14, 14, 14, 14, 14, 0, 0, 2, 14, 20, 20
    DB 20, 14, 1, 0, 0, 10, 3, 0, 14, 14, 14, 14, 14, 14, 3, 14, 14, 2, 2, 2, 2, 3, 2, 2, 3, 3, 3, 3, 3, 2, 17, 16, 19, 1, 2, 2, 2, 2, 2, 14, 14, 14, 0, 3, 3, 0, 0, 1
    DB 0, 0, 1, 10, 11, 21, 21, 2, 0, 14, 14, 14, 2, 2, 2, 3, 4, 12, 12, 12, 12, 12, 13, 12, 12, 12, 2, 18, 0, 0, 0, 0, 0, 0, 0, 0, 16, 17, 1, 3, 14, 0, 2, 21, 21, 11, 10, 19
    DB 10, 11, 11, 21, 21, 11, 11, 10, 0, 1, 3, 3, 12, 12, 12, 12, 12, 12, 12, 12, 13, 12, 12, 2, 16, 0, 0, 0, 0, 0, 0, 17, 2, 10, 10, 11, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 11, 11
    DB 11, 10, 10, 10, 10, 3, 3, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 18, 0, 0, 0, 0, 0, 18, 3, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 21, 11, 11, 10, 10, 10
    DB 0, 0, 0, 18, 12, 12, 12, 13, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 2, 16, 0, 0, 0, 0, 19, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11
    DB 0, 0, 4, 12, 12, 12, 12, 12, 13, 12, 12, 12, 12, 12, 12, 12, 1, 0, 0, 0, 0, 0, 2, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11
    DB 0, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 12, 13, 12, 3, 16, 0, 0, 0, 16, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 21, 11, 11, 11, 11, 11, 11, 11
    DB 12, 12, 12, 12, 12, 12, 13, 12, 12, 12, 12, 12, 12, 1, 0, 0, 0, 16, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 10, 3, 3, 2, 1, 1, 18
    DB 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 3, 0, 0, 0, 16, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 10, 2, 17, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 1, 0, 0, 0, 3, 11, 21, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 13, 12, 12, 12, 12, 12, 12, 12, 12, 0, 0, 0, 16, 10, 21, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 12, 12, 12, 12, 12, 12, 12, 12, 0, 0, 0, 2, 11, 21, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 2, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 12, 13, 12, 12, 12, 12, 4, 0, 0, 0, 10, 21, 11, 11, 11, 11, 11, 11, 11, 11, 21, 11, 10, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 18, 2, 1, 3, 3, 4, 12, 12, 12, 12, 12, 12
    DB 13, 12, 12, 12, 12, 3, 0, 0, 16, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 17, 1, 3, 12, 12, 12, 12, 13, 13, 12, 12, 13, 12, 12, 12, 13, 13, 12
    DB 13, 12, 12, 12, 3, 0, 0, 18, 11, 21, 11, 11, 11, 11, 11, 11, 11, 10, 2, 0, 0, 0, 0, 0, 0, 0, 19, 3, 12, 12, 13, 12, 12, 12, 13, 12, 13, 12, 12, 13, 12, 12, 12, 13, 12, 13, 13, 12
    DB 12, 12, 12, 12, 0, 0, 18, 11, 11, 11, 11, 11, 11, 11, 11, 11, 3, 0, 0, 0, 0, 0, 16, 2, 12, 12, 13, 13, 12, 12, 12, 12, 12, 2, 1, 19, 19, 17, 16, 19, 18, 1, 2, 2, 1, 3, 4, 4
    DB 12, 12, 12, 0, 0, 18, 11, 11, 11, 11, 11, 11, 11, 11, 10, 18, 0, 0, 0, 0, 2, 12, 12, 12, 12, 12, 1, 16, 0, 0, 16, 17, 19, 1, 1, 2, 2, 14, 14, 14, 14, 14, 3, 1, 18, 16, 0, 0
    DB 12, 12, 18, 0, 16, 11, 21, 11, 11, 11, 11, 11, 11, 2, 0, 0, 0, 1, 4, 12, 12, 12, 2, 19, 1, 14, 14, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15
    DB 12, 3, 0, 0, 10, 11, 10, 21, 11, 11, 11, 10, 0, 0, 0, 1, 12, 12, 12, 1, 18, 2, 14, 14, 14, 14, 15, 14, 14, 14, 14, 15, 15, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20
    DB 12, 16, 0, 3, 11, 11, 11, 11, 11, 11, 3, 0, 0, 19, 12, 12, 2, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 1, 2, 2, 3, 14, 14, 15, 15, 20, 20, 20, 20, 20, 20, 20
    DB 2, 0, 19, 11, 11, 11, 11, 11, 11, 18, 0, 16, 2, 3, 19, 0, 0, 0, 0, 18, 16, 0, 0, 0, 0, 0, 0, 0, 0, 16, 19, 16, 0, 16, 1, 2, 3, 3, 2, 16, 16, 2, 14, 15, 20, 20, 20, 20
    DB 0, 0, 11, 11, 11, 11, 11, 10, 17, 0, 18, 2, 16, 0, 0, 0, 0, 14, 14, 2, 18, 0, 0, 0, 0, 0, 0, 0, 0, 16, 1, 14, 14, 16, 0, 0, 0, 19, 4, 10, 10, 10, 17, 0, 16, 14, 15, 20
    DB 0, 3, 21, 11, 11, 21, 10, 16, 0, 0, 16, 0, 0, 0, 0, 14, 15, 14, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 15, 14, 0, 0, 0, 0, 0, 2, 10, 11, 11, 10, 16, 0, 2
    DB 0, 11, 11, 11, 21, 10, 16, 0, 0, 0, 0, 0, 0, 16, 15, 20, 2, 0, 0, 0, 0, 0, 18, 10, 10, 10, 10, 18, 0, 0, 0, 0, 0, 0, 20, 15, 16, 0, 0, 0, 0, 0, 18, 10, 21, 21, 10, 1
    DB 3, 11, 11, 21, 11, 16, 0, 0, 0, 0, 0, 0, 1, 20, 20, 14, 1, 10, 0, 0, 0, 0, 16, 21, 21, 11, 21, 19, 0, 0, 0, 0, 10, 10, 18, 20, 20, 1, 0, 0, 0, 0, 0, 0, 0, 10, 11, 21
    DB 10, 11, 11, 11, 16, 0, 0, 0, 0, 0, 0, 16, 20, 20, 20, 16, 11, 21, 10, 0, 0, 18, 1, 10, 10, 10, 10, 4, 1, 18, 0, 1, 21, 21, 2, 14, 20, 20, 18, 0, 0, 0, 0, 0, 0, 0, 17, 10
    DB 11, 11, 11, 1, 0, 0, 0, 0, 0, 0, 0, 15, 20, 20, 14, 19, 11, 11, 10, 10, 11, 11, 11, 18, 0, 0, 16, 11, 11, 10, 10, 10, 10, 11, 10, 14, 20, 20, 15, 0, 0, 0, 0, 0, 0, 0, 0, 16
    DB 11, 11, 10, 0, 0, 0, 0, 0, 0, 0, 1, 20, 20, 20, 15, 16, 1, 0, 0, 10, 11, 11, 11, 1, 0, 0, 16, 11, 11, 11, 10, 0, 0, 19, 18, 14, 20, 20, 20, 2, 0, 0, 0, 0, 0, 0, 0, 2
    DB 11, 11, 0, 0, 0, 0, 0, 0, 0, 0, 14, 20, 20, 20, 20, 16, 0, 0, 0, 0, 10, 3, 4, 14, 14, 14, 14, 4, 3, 10, 17, 0, 0, 0, 0, 15, 20, 20, 20, 14, 0, 0, 0, 0, 0, 0, 0, 0
    DB 11, 3, 0, 0, 0, 0, 0, 0, 0, 0, 14, 20, 20, 20, 20, 14, 0, 16, 1, 14, 8, 20, 20, 20, 20, 20, 20, 20, 20, 8, 14, 14, 18, 0, 14, 20, 20, 20, 20, 14, 0, 0, 0, 0, 0, 0, 0, 0
    DB 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 20, 20, 20, 20, 20, 3, 18, 20, 20, 20, 20, 15, 14, 14, 14, 14, 15, 20, 20, 20, 20, 2, 1, 20, 20, 20, 20, 20, 3, 0, 0, 0, 0, 0, 0, 0, 0
    DB 10, 0, 0, 0, 0, 0, 0, 0, 3, 14, 15, 20, 20, 20, 20, 20, 20, 14, 3, 15, 14, 17, 0, 0, 0, 0, 0, 0, 16, 14, 14, 2, 14, 20, 20, 20, 20, 20, 20, 15, 14, 3, 16, 0, 0, 0, 0, 0
    DB 1, 0, 0, 0, 0, 16, 14, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 15, 20, 20, 20, 20, 15, 20, 20, 20, 20, 20, 20, 14, 16, 0, 0, 0
    DB 0, 0, 0, 0, 2, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 14, 14, 3, 18, 17, 2, 3, 14, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 3, 0, 0
    DB 0, 0, 0, 14, 20, 20, 20, 20, 20, 15, 15, 14, 15, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 14, 4, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 15, 14, 14, 15, 15, 20, 20, 20, 20, 14, 0
    DB 0, 0, 1, 20, 20, 20, 14, 2, 0, 0, 0, 0, 2, 14, 3, 14, 15, 20, 20, 20, 20, 20, 20, 14, 0, 16, 3, 15, 20, 20, 20, 20, 20, 15, 14, 2, 14, 14, 0, 0, 0, 0, 2, 14, 20, 20, 20, 3
    DB 0, 0, 20, 20, 14, 10, 11, 10, 0, 0, 0, 0, 15, 20, 20, 0, 0, 2, 15, 20, 20, 20, 2, 0, 0, 0, 0, 16, 20, 20, 20, 15, 2, 0, 0, 15, 20, 20, 16, 0, 0, 0, 10, 11, 10, 14, 20, 20
    DB 0, 14, 20, 2, 19, 10, 10, 10, 10, 10, 11, 1, 8, 20, 20, 1, 0, 0, 0, 14, 20, 15, 0, 0, 0, 0, 0, 0, 15, 20, 15, 16, 0, 0, 0, 20, 20, 20, 1, 10, 10, 10, 10, 10, 10, 19, 1, 20
    DB 0, 15, 14, 0, 0, 0, 0, 17, 11, 11, 21, 10, 14, 20, 20, 14, 0, 16, 0, 0, 2, 3, 14, 1, 0, 0, 18, 14, 14, 3, 0, 0, 0, 0, 16, 20, 20, 15, 3, 21, 11, 21, 2, 0, 0, 0, 0, 14
    DB 0, 15, 0, 0, 0, 0, 0, 0, 10, 11, 11, 10, 2, 20, 20, 20, 0, 0, 0, 0, 0, 15, 20, 20, 20, 20, 20, 20, 20, 0, 0, 0, 0, 0, 15, 20, 20, 14, 2, 11, 11, 11, 0, 0, 0, 0, 0, 0
    DB 0, 14, 0, 0, 0, 0, 17, 3, 10, 17, 0, 0, 0, 15, 20, 20, 15, 0, 0, 0, 0, 2, 20, 20, 20, 20, 20, 20, 14, 0, 0, 0, 0, 3, 20, 20, 20, 0, 0, 0, 18, 10, 3, 1, 0, 0, 0, 0
    DB 0, 2, 0, 0, 0, 3, 11, 21, 11, 16, 0, 0, 0, 19, 20, 20, 20, 15, 16, 0, 0, 16, 20, 20, 20, 20, 20, 20, 18, 0, 0, 0, 14, 20, 20, 20, 2, 0, 0, 0, 0, 11, 11, 11, 10, 0, 0, 0
    DB 2, 0, 0, 0, 0, 16, 11, 21, 21, 10, 19, 10, 11, 11, 2, 15, 20, 20, 20, 14, 18, 16, 20, 20, 20, 20, 20, 20, 18, 0, 3, 15, 20, 20, 15, 2, 10, 11, 10, 1, 10, 21, 21, 21, 2, 0, 0, 0
    DB 12, 0, 0, 0, 0, 0, 2, 11, 10, 1, 3, 21, 11, 21, 11, 19, 14, 20, 20, 20, 14, 17, 20, 20, 20, 20, 20, 20, 19, 14, 20, 20, 20, 15, 16, 10, 21, 21, 21, 10, 18, 3, 10, 10, 0, 0, 0, 0
    DB 2, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 11, 11, 10, 16, 0, 1, 14, 20, 16, 14, 20, 20, 20, 20, 20, 20, 14, 18, 20, 15, 2, 0, 0, 3, 11, 11, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 3, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 0, 0, 0, 0, 0, 0, 3, 20, 20, 20, 20, 20, 20, 20, 20, 14, 0, 0, 0, 0, 0, 0, 10, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16
    DB 0, 0, 18, 18, 0, 0, 17, 0, 0, 0, 19, 11, 21, 21, 10, 2, 0, 0, 0, 14, 20, 20, 20, 20, 20, 20, 15, 20, 20, 20, 14, 0, 0, 0, 2, 11, 21, 11, 11, 2, 0, 0, 0, 16, 0, 0, 0, 10
    DB 16, 0, 0, 0, 0, 0, 1, 14, 2, 16, 0, 10, 11, 11, 11, 10, 16, 1, 15, 20, 20, 20, 20, 20, 15, 15, 20, 20, 20, 20, 20, 15, 2, 0, 10, 11, 11, 11, 10, 16, 0, 2, 14, 1, 0, 0, 1, 11
    DB 11, 10, 1, 0, 0, 0, 0, 16, 3, 14, 14, 14, 14, 6, 14, 14, 15, 20, 20, 20, 20, 20, 20, 14, 0, 0, 14, 15, 20, 20, 20, 20, 20, 20, 15, 14, 14, 14, 14, 14, 14, 14, 17, 0, 0, 0, 11, 11
    DB 11, 11, 21, 11, 2, 0, 0, 0, 0, 0, 2, 14, 15, 20, 20, 20, 20, 20, 20, 15, 14, 2, 0, 0, 0, 0, 0, 0, 1, 14, 15, 15, 20, 20, 20, 20, 20, 15, 14, 2, 0, 0, 0, 0, 0, 10, 21, 11
    DB 0, 1, 10, 11, 21, 11, 10, 18, 0, 0, 0, 0, 0, 0, 0, 16, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 10, 21, 11, 11
    DB 14, 2, 0, 0, 1, 10, 21, 11, 11, 4, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 3, 17, 0, 0, 10, 11, 11, 11, 11
    DB 20, 20, 20, 14, 2, 16, 0, 18, 10, 10, 10, 10, 3, 1, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 4, 2, 0, 0, 0, 10, 21, 11, 11, 11, 11
    DB 20, 20, 20, 20, 20, 20, 15, 14, 3, 17, 0, 16, 1, 2, 2, 2, 1, 19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 12, 12, 17, 0, 0, 0, 10, 21, 11, 11, 11, 11, 11
    DB 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 14, 14, 14, 3, 3, 2, 1, 19, 18, 18, 1, 1, 2, 14, 14, 14, 14, 1, 0, 16, 4, 12, 12, 2, 0, 0, 0, 0, 10, 21, 11, 11, 11, 11, 11, 10
    DB 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 14, 14, 2, 1, 3, 12, 12, 12, 2, 0, 0, 0, 0, 17, 10, 11, 11, 11, 11, 11, 11, 11, 0
    DB 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 15, 14, 14, 19, 16, 0, 17, 2, 12, 13, 12, 12, 2, 0, 0, 0, 0, 0, 1, 11, 21, 11, 11, 11, 11, 11, 11, 19, 0
    DB 0, 17, 1, 2, 2, 14, 14, 14, 14, 14, 14, 14, 14, 2, 1, 18, 0, 0, 0, 0, 17, 2, 12, 12, 13, 13, 13, 12, 3, 17, 0, 0, 0, 0, 0, 0, 10, 11, 11, 11, 11, 11, 11, 11, 21, 2, 0, 0
    DB 3, 1, 17, 17, 16, 16, 0, 0, 16, 16, 16, 16, 18, 1, 2, 12, 12, 12, 12, 12, 12, 12, 13, 13, 12, 12, 2, 0, 0, 0, 0, 0, 0, 0, 2, 10, 11, 11, 11, 11, 11, 11, 11, 11, 3, 0, 0, 1
    DB 13, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 12, 13, 13, 13, 12, 12, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 17, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 3, 0, 0, 18, 12
    DB 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 12, 13, 12, 12, 12, 12, 12, 2, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 2, 0, 0, 17, 12, 12
    DB 12, 13, 12, 13, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 3, 1, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 1, 0, 0, 16, 12, 12, 12
    DB 3, 3, 2, 12, 3, 3, 2, 2, 1, 18, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 18, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 16, 0, 0, 16, 12, 12, 12, 12
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 2, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 3, 0, 0, 0, 18, 12, 12, 12, 12, 12
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 1, 0, 0, 0, 1, 12, 12, 12, 12, 12, 12
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 18, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 0, 0, 0, 0, 2, 12, 13, 12, 12, 12, 12, 12
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 18, 2, 10, 10, 11, 21, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 18, 0, 0, 0, 0, 12, 12, 13, 12, 12, 12, 12, 12, 12
    DB 0, 0, 0, 0, 0, 16, 18, 1, 3, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 1, 0, 0, 0, 0, 2, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12
    DB 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 21, 11, 10, 1, 0, 0, 0, 0, 17, 12, 13, 12, 12, 12, 12, 12, 12, 12, 12, 13, 12
    DB 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 21, 11, 10, 10, 17, 0, 0, 0, 0, 0, 4, 12, 12, 12, 13, 12, 12, 12, 12, 13, 12, 12, 12, 1
    DB 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 2, 0, 0, 0, 0, 0, 0, 3, 12, 12, 12, 12, 12, 12, 12, 13, 13, 12, 12, 3, 1, 1, 1
    DB 19, 2, 4, 10, 11, 11, 11, 21, 11, 11, 11, 11, 11, 11, 11, 11, 11, 21, 11, 11, 10, 10, 1, 0, 0, 0, 0, 0, 0, 16, 2, 12, 13, 12, 12, 12, 12, 12, 13, 12, 12, 3, 2, 1, 2, 2, 1, 2
    DB 19, 2, 2, 2, 2, 3, 3, 10, 10, 10, 10, 11, 11, 11, 11, 11, 10, 10, 2, 17, 0, 0, 0, 0, 0, 0, 16, 2, 12, 12, 13, 12, 13, 12, 12, 12, 3, 2, 2, 1, 2, 2, 2, 3, 1, 1, 18, 18
    DB 18, 1, 2, 2, 2, 14, 2, 1, 1, 19, 18, 2, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 17, 2, 12, 12, 12, 4, 4, 3, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1, 18, 18, 18, 16, 0
    DB 16, 17, 16, 19, 17, 17, 19, 1, 19, 2, 1, 2, 1, 2, 2, 2, 2, 2, 2, 1, 18, 1, 18, 1, 2, 2, 2, 1, 2, 3, 2, 2, 2, 2, 2, 2, 2, 19, 18, 18, 17, 16, 17, 16, 16, 0, 0, 0
    DB 0, 16, 0, 16, 16, 0, 16, 16, 16, 17, 17, 18, 18, 2, 2, 3, 2, 3, 2, 1, 2, 2, 2, 2, 2, 1, 2, 1, 19, 2, 2, 2, 18, 1, 17, 17, 16, 18, 16, 0, 0, 16, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 16, 16, 18, 18, 18, 18, 19, 18, 19, 18, 19, 18, 17, 18, 18, 18, 16, 17, 16, 17, 17, 16, 16, 17, 16, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 16, 16, 0, 0, 0, 16, 16, 16, 16, 16, 16, 0, 0, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 3, 0, 0, 3, 15, 15, 14, 0, 14, 15, 15, 6, 15, 6, 3, 0, 16, 14, 15, 15, 14, 15, 15, 14, 0, 14, 15, 15, 15, 15, 6, 15, 3, 14, 15, 15, 2, 0, 0, 14, 15, 15, 2, 14, 15, 15, 2, 16
    DB 15, 19, 0, 6, 21, 21, 6, 14, 21, 21, 21, 15, 21, 21, 21, 3, 15, 21, 21, 21, 15, 21, 21, 15, 0, 15, 21, 21, 21, 21, 21, 21, 15, 15, 21, 21, 15, 16, 0, 15, 21, 21, 14, 15, 21, 21, 14, 15
    DB 21, 15, 0, 14, 21, 21, 6, 15, 21, 21, 15, 2, 15, 11, 21, 14, 15, 21, 21, 14, 2, 3, 3, 1, 0, 15, 11, 21, 14, 2, 2, 3, 1, 6, 11, 11, 15, 6, 0, 15, 11, 21, 14, 15, 11, 21, 14, 15
    DB 21, 21, 14, 14, 11, 21, 6, 6, 11, 21, 14, 0, 15, 21, 21, 14, 15, 11, 21, 1, 0, 16, 16, 16, 16, 15, 21, 11, 5, 3, 3, 16, 0, 15, 11, 21, 15, 21, 14, 6, 11, 21, 14, 15, 21, 21, 14, 15
    DB 15, 11, 21, 15, 21, 21, 6, 6, 21, 21, 14, 0, 15, 21, 21, 14, 15, 11, 21, 14, 15, 21, 11, 15, 3, 15, 11, 11, 21, 21, 21, 14, 0, 15, 21, 21, 15, 11, 11, 15, 11, 21, 14, 15, 21, 21, 14, 15
    DB 6, 15, 11, 21, 11, 21, 5, 15, 21, 21, 14, 0, 15, 11, 21, 14, 15, 11, 21, 2, 15, 21, 11, 21, 14, 15, 11, 11, 6, 5, 5, 1, 0, 15, 21, 21, 14, 15, 11, 15, 11, 21, 14, 15, 21, 21, 14, 15
    DB 6, 1, 15, 11, 11, 11, 6, 15, 11, 21, 14, 0, 15, 11, 21, 14, 15, 21, 21, 2, 0, 6, 21, 21, 14, 15, 21, 21, 2, 0, 0, 16, 0, 15, 11, 21, 14, 2, 21, 21, 11, 11, 14, 15, 21, 21, 14, 15
    DB 6, 0, 3, 15, 11, 21, 6, 6, 21, 11, 15, 15, 11, 11, 21, 14, 15, 21, 21, 15, 15, 21, 11, 21, 14, 15, 11, 21, 11, 11, 11, 11, 5, 15, 21, 21, 14, 0, 14, 21, 11, 21, 14, 15, 21, 21, 14, 15
    DB 14, 0, 0, 14, 11, 21, 14, 19, 15, 21, 21, 15, 11, 15, 14, 0, 2, 15, 11, 21, 15, 21, 21, 15, 16, 15, 21, 11, 11, 21, 11, 21, 6, 15, 21, 21, 14, 0, 0, 15, 11, 21, 14, 15, 21, 21, 3, 2
    DB 16, 0, 0, 0, 18, 18, 0, 0, 0, 18, 19, 17, 19, 16, 0, 0, 0, 0, 18, 18, 16, 18, 16, 0, 0, 16, 18, 18, 18, 18, 18, 18, 16, 16, 19, 18, 0, 0, 0, 0, 19, 18, 0, 16, 1, 19, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 20, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0
    DB 0, 0, 0, 0, 16, 16, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 20, 4, 0, 0, 16, 0, 0, 0, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 20, 0, 0
    DB 0, 0, 0, 15, 15, 15, 15, 2, 0, 15, 15, 15, 20, 15, 19, 15, 15, 0, 2, 20, 4, 0, 0, 3, 20, 2, 4, 20, 3, 0, 15, 15, 15, 15, 1, 6, 15, 18, 0, 15, 15, 0, 0, 4, 20, 20, 15, 4
    DB 0, 0, 0, 2, 4, 5, 20, 15, 0, 20, 20, 1, 6, 20, 2, 15, 20, 18, 15, 20, 17, 0, 0, 3, 20, 15, 20, 5, 0, 5, 20, 15, 4, 20, 15, 19, 20, 15, 2, 20, 5, 0, 0, 16, 15, 20, 3, 16
    DB 0, 0, 18, 15, 15, 6, 20, 15, 0, 15, 15, 0, 4, 20, 3, 0, 20, 15, 20, 5, 0, 0, 0, 3, 20, 20, 20, 2, 0, 15, 20, 15, 15, 15, 6, 0, 15, 20, 15, 15, 0, 0, 0, 0, 15, 20, 0, 0
    DB 0, 0, 2, 20, 15, 6, 20, 15, 0, 20, 20, 0, 5, 20, 4, 0, 4, 20, 15, 0, 0, 0, 0, 3, 20, 3, 15, 20, 3, 2, 20, 15, 6, 15, 2, 0, 0, 15, 20, 3, 0, 0, 0, 0, 15, 20, 15, 6
    DB 0, 0, 0, 2, 6, 3, 3, 2, 0, 3, 3, 0, 19, 4, 18, 0, 4, 20, 4, 0, 0, 0, 0, 17, 4, 17, 0, 4, 3, 0, 18, 5, 15, 5, 1, 0, 16, 20, 15, 0, 0, 0, 0, 0, 16, 14, 15, 3
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15, 15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 20, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

var_3:
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 17, 11, 1, 0, 0, 0, 0, 0, 0, 10, 10, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 11, 16, 0, 18, 11, 3, 2, 16, 0, 0, 18, 1, 10, 11, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 17, 11, 11, 10, 0, 18, 11, 10, 10, 11, 18, 3, 11, 10, 11, 10, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 11, 2, 11, 2, 0, 11, 1, 0, 11, 10, 11, 10, 0, 10, 11, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 11, 10, 10, 11, 11, 1, 11, 10, 10, 11, 1, 10, 11, 10, 11, 11, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 0, 3, 19, 2, 2, 10, 1, 0, 0, 2, 10, 2, 1, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 16, 0, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 17, 16, 17, 17, 17, 16, 16, 16, 16, 0, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 18, 1, 1, 1, 2, 1, 1, 17, 18, 18, 18, 17, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 14, 3, 2, 3, 2, 16, 2, 3, 2, 3, 2, 1, 1, 1, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 16, 17, 2, 1, 17, 10, 11, 11, 11, 10, 5, 3, 2, 2, 18, 18, 16, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 15, 3, 16, 0, 0, 10, 10, 11, 11, 11, 11, 11, 10, 4, 2, 1, 2, 19, 18, 17, 16, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 20, 20, 20, 15, 14, 17, 0, 16, 2, 10, 11, 11, 11, 11, 11, 6, 14, 2, 2, 1, 19, 18, 16, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 20, 20, 20, 20, 20, 20, 15, 14, 16, 0, 16, 2, 10, 21, 21, 21, 21, 10, 5, 14, 2, 1, 1, 18, 17, 16, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 14, 17, 0, 17, 10, 11, 11, 11, 21, 10, 5, 4, 1, 1, 1, 1, 16, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 14, 0, 0, 2, 10, 11, 21, 11, 11, 10, 4, 1, 1, 1, 1, 17, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 14, 16, 0, 19, 10, 11, 11, 11, 11, 10, 5, 2, 2, 1, 1, 19, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 2, 0, 16, 10, 11, 11, 21, 21, 10, 5, 3, 2, 19, 18, 17, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 20, 20, 20, 20, 20, 20, 20, 20, 14, 0, 0, 3, 10, 21, 21, 11, 11, 3, 2, 2, 17, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 14, 0, 0, 3, 11, 21, 11, 11, 2, 2, 18, 18, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 14, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 16, 0, 2, 11, 10, 18, 2, 2, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 2, 14, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 14, 17, 0, 0, 19, 3, 1, 18, 16, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 11, 10, 18, 0, 16, 14, 15, 20, 20, 20, 20, 20, 20, 20, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 14, 17, 0, 1, 2, 19, 19, 17, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 10, 10, 11, 10, 3, 16, 0, 2, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 14, 16, 16, 2, 2, 19, 18, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 11, 10, 10, 10, 10, 10, 10, 1, 0, 16, 14, 15, 20, 20, 20, 20, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 16, 0, 17, 19, 18, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 11, 11, 11, 11, 11, 10, 10, 10, 10, 3, 0, 0, 2, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 3, 0, 14, 3, 1, 19, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 11, 11, 11, 21, 21, 11, 11, 10, 10, 10, 10, 10, 16, 0, 1, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 20, 20, 20, 20, 3, 0, 15, 20, 20, 2, 18, 18, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0
    DB 18, 19, 18, 1, 1, 2, 2, 2, 3, 1, 18, 10, 11, 10, 1, 0, 16, 14, 20, 20, 20, 20, 20, 20, 20, 20, 15, 20, 20, 1, 0, 15, 20, 20, 14, 2, 19, 17, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 10, 21, 11, 2, 0, 0, 14, 20, 20, 20, 20, 20, 20, 20, 20, 2, 0, 15, 20, 20, 15, 2, 1, 1, 18, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 18, 10, 21, 11, 3, 0, 0, 14, 20, 20, 20, 20, 20, 2, 0, 15, 20, 20, 14, 2, 2, 1, 19, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 11, 21, 11, 3, 0, 0, 14, 15, 20, 2, 0, 15, 20, 20, 15, 14, 2, 19, 18, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 11, 10, 0, 3, 16, 16, 18, 16, 20, 20, 20, 15, 2, 3, 18, 16, 17, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 12, 12, 12, 4, 3, 3, 2, 19, 17, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 14, 15, 14, 14, 17, 18, 14, 15, 14, 2, 1, 1, 19, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 12, 12, 12, 12, 12, 12, 12, 12, 13, 12, 12, 12, 3, 1, 17, 0, 0, 0, 0, 0, 16, 14, 14, 14, 14, 14, 14, 19, 1, 17, 1, 17, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 12, 12, 12, 12, 12, 12, 12, 12, 13, 12, 12, 12, 13, 12, 12, 12, 12, 1, 17, 0, 0, 0, 1, 14, 14, 3, 2, 1, 1, 16, 16, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 12, 12, 12, 12, 13, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 12, 3, 1, 0, 1, 14, 2, 1, 1, 19, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 1, 3, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 12, 12, 16, 14, 2, 18, 18, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 15, 14, 3, 2, 0, 0, 0, 16, 1, 4, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 12, 13, 12, 16, 2, 3, 16, 18, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 20, 20, 20, 20, 20, 15, 14, 2, 16, 0, 0, 16, 3, 12, 12, 12, 13, 12, 12, 12, 12, 12, 12, 12, 4, 18, 3, 2, 19, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 14, 1, 0, 0, 19, 3, 12, 12, 12, 12, 12, 12, 12, 12, 13, 12, 16, 14, 2, 18, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 14, 19, 0, 0, 2, 12, 12, 12, 12, 12, 12, 12, 12, 3, 17, 14, 2, 19, 18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 3, 0, 0, 18, 4, 12, 12, 12, 12, 12, 12, 2, 2, 2, 2, 18, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 14, 0, 0, 18, 12, 12, 12, 13, 12, 12, 19, 3, 3, 19, 17, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 16, 14, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 15, 20, 20, 20, 20, 20, 20, 14, 0, 0, 1, 12, 12, 12, 12, 12, 17, 3, 1, 18, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 11, 2, 0, 0, 14, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 14, 20, 20, 20, 20, 20, 20, 3, 0, 0, 2, 12, 12, 12, 1, 2, 2, 2, 18, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 11, 21, 11, 2, 0, 16, 15, 20, 20, 20, 20, 20, 20, 20, 20, 15, 14, 14, 20, 20, 20, 20, 20, 15, 1, 0, 16, 3, 12, 12, 17, 14, 1, 17, 18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 2, 11, 11, 21, 10, 1, 0, 3, 20, 20, 20, 20, 20, 20, 20, 20, 20, 14, 1, 15, 20, 20, 20, 20, 20, 14, 0, 0, 2, 15, 12, 1, 3, 1, 18, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 2, 0, 3, 11, 21, 11, 10, 0, 0, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 19, 1, 15, 20, 20, 20, 20, 15, 16, 0, 17, 12, 1, 2, 2, 18, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 3, 12, 0, 18, 11, 11, 21, 11, 1, 0, 3, 20, 20, 20, 20, 20, 20, 20, 20, 20, 3, 0, 14, 20, 20, 20, 20, 20, 1, 0, 16, 17, 19, 3, 1, 18, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 12, 12, 2, 0, 10, 11, 11, 11, 3, 0, 18, 15, 20, 20, 20, 20, 20, 20, 20, 20, 14, 0, 1, 15, 20, 20, 20, 20, 1, 0, 0, 16, 2, 1, 18, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 16, 13, 13, 4, 0, 2, 11, 11, 11, 10, 0, 0, 14, 20, 20, 20, 20, 20, 20, 20, 20, 14, 0, 0, 14, 20, 20, 20, 20, 2, 0, 16, 2, 1, 1, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 2, 12, 13, 12, 0, 19, 10, 11, 11, 10, 16, 0, 15, 20, 15, 20, 20, 20, 20, 20, 20, 14, 0, 0, 3, 20, 20, 20, 20, 2, 0, 2, 1, 1, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 12, 12, 12, 12, 0, 17, 11, 11, 11, 10, 0, 0, 14, 20, 15, 20, 20, 20, 20, 20, 20, 3, 0, 0, 19, 20, 20, 20, 20, 18, 17, 14, 19, 1, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 19, 12, 12, 13, 12, 0, 18, 11, 11, 11, 10, 0, 0, 15, 20, 11, 20, 20, 20, 20, 20, 20, 16, 16, 1, 18, 20, 20, 20, 20, 16, 2, 1, 18, 17, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 18, 0, 12, 12, 12, 13, 12, 0, 18, 11, 11, 21, 10, 0, 16, 20, 20, 11, 20, 15, 20, 15, 20, 15, 0, 3, 3, 1, 20, 20, 20, 15, 17, 2, 1, 18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 2, 19, 2, 12, 12, 12, 12, 12, 0, 19, 11, 11, 11, 10, 0, 1, 20, 15, 21, 20, 20, 20, 15, 20, 3, 0, 15, 2, 3, 20, 20, 20, 1, 1, 18, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 16, 0, 2, 4, 16, 12, 12, 12, 13, 12, 4, 0, 2, 11, 11, 21, 3, 0, 3, 15, 11, 11, 20, 20, 15, 20, 15, 0, 1, 15, 19, 14, 20, 20, 1, 2, 18, 17, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 14, 0, 3, 10, 0, 12, 12, 12, 12, 13, 13, 2, 0, 10, 21, 11, 11, 16, 0, 10, 15, 11, 15, 21, 15, 21, 20, 14, 0, 14, 15, 0, 15, 20, 1, 1, 1, 18, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 15, 0, 3, 10, 0, 3, 12, 12, 12, 13, 13, 12, 16, 0, 10, 11, 11, 10, 0, 2, 11, 15, 11, 20, 21, 15, 21, 20, 0, 1, 14, 3, 19, 20, 2, 2, 19, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 14, 16, 10, 11, 0, 2, 12, 12, 12, 12, 12, 13, 2, 0, 17, 11, 11, 11, 16, 0, 10, 11, 11, 15, 21, 15, 21, 20, 1, 0, 14, 14, 16, 14, 2, 19, 17, 17, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0
    DB 14, 16, 10, 11, 0, 1, 13, 12, 12, 12, 12, 12, 3, 0, 0, 10, 11, 11, 10, 0, 1, 11, 11, 11, 11, 11, 21, 20, 14, 0, 2, 3, 14, 0, 16, 1, 18, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 7, 1, 11, 11, 0, 1, 12, 12, 12, 12, 12, 12, 12, 0, 0, 16, 11, 11, 11, 16, 0, 10, 11, 11, 11, 11, 11, 15, 15, 0, 1, 3, 2, 18, 16, 17, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 10, 21, 11, 0, 1, 12, 12, 12, 12, 12, 12, 12, 0, 0, 0, 3, 11, 11, 3, 0, 3, 21, 21, 11, 11, 11, 11, 11, 1, 18, 1, 19, 1, 17, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 11, 21, 10, 0, 1, 12, 12, 12, 12, 12, 13, 12, 16, 0, 0, 0, 11, 21, 10, 0, 18, 11, 11, 11, 11, 11, 11, 21, 3, 18, 2, 1, 18, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 2, 21, 21, 10, 0, 2, 12, 12, 12, 12, 12, 12, 12, 18, 0, 16, 16, 10, 11, 11, 16, 0, 10, 11, 11, 11, 10, 11, 11, 10, 1, 2, 1, 19, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 11, 11, 11, 10, 0, 3, 12, 12, 12, 12, 12, 12, 12, 18, 0, 16, 3, 18, 11, 21, 2, 0, 10, 11, 11, 11, 11, 11, 11, 3, 2, 3, 1, 19, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 21, 21, 11, 2, 0, 12, 12, 12, 12, 12, 12, 12, 12, 19, 0, 0, 12, 16, 1, 11, 2, 0, 1, 11, 11, 11, 11, 11, 11, 3, 1, 2, 1, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 11, 11, 11, 16, 0, 12, 12, 12, 12, 12, 12, 12, 12, 1, 0, 0, 12, 19, 0, 10, 10, 0, 16, 11, 11, 11, 11, 11, 11, 2, 2, 1, 19, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 11, 11, 10, 0, 17, 12, 12, 12, 12, 12, 12, 12, 12, 17, 0, 0, 12, 1, 0, 3, 10, 0, 0, 11, 11, 11, 11, 11, 11, 1, 2, 18, 19, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 10, 11, 3, 0, 2, 12, 12, 12, 12, 12, 12, 12, 12, 0, 0, 0, 12, 2, 0, 14, 1, 0, 0, 10, 21, 11, 11, 11, 10, 18, 2, 1, 17, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 11, 10, 16, 0, 12, 12, 12, 12, 13, 12, 12, 12, 12, 0, 0, 0, 12, 2, 0, 14, 14, 0, 0, 10, 11, 11, 11, 21, 10, 1, 2, 17, 18, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 21, 4, 0, 16, 12, 12, 12, 12, 12, 12, 12, 12, 3, 0, 0, 16, 13, 2, 0, 14, 14, 0, 0, 10, 11, 10, 11, 11, 1, 14, 1, 19, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 11, 16, 0, 2, 12, 12, 12, 12, 12, 12, 12, 12, 2, 0, 0, 19, 13, 2, 0, 14, 14, 16, 0, 10, 11, 11, 21, 10, 1, 2, 1, 19, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 1, 0, 0, 12, 12, 12, 12, 12, 12, 12, 12, 12, 0, 0, 0, 3, 13, 2, 0, 14, 14, 16, 0, 10, 11, 11, 11, 2, 1, 2, 1, 17, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 3, 12, 12, 12, 12, 12, 12, 12, 12, 4, 0, 0, 0, 12, 13, 1, 0, 15, 14, 0, 0, 10, 11, 11, 10, 16, 2, 1, 19, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 19, 12, 12, 12, 12, 12, 12, 12, 12, 13, 1, 0, 0, 16, 12, 12, 16, 18, 14, 0, 0, 16, 11, 11, 11, 2, 2, 2, 17, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 0, 0, 0, 2, 12, 12, 0, 17, 17, 2, 1, 16, 11, 21, 10, 1, 1, 1, 18, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 4, 12, 12, 12, 12, 12, 12, 12, 12, 12, 2, 0, 0, 0, 12, 13, 12, 0, 0, 16, 14, 18, 1, 21, 11, 2, 3, 19, 18, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 0, 0, 0, 2, 12, 13, 1, 0, 0, 2, 14, 0, 3, 21, 2, 2, 1, 1, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 12, 12, 12, 12, 12, 12, 12, 12, 12, 1, 0, 0, 0, 12, 13, 12, 16, 0, 2, 14, 2, 0, 10, 4, 19, 2, 1, 18, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 12, 12, 12, 12, 12, 12, 12, 13, 12, 0, 0, 0, 2, 13, 12, 12, 0, 2, 14, 2, 19, 0, 18, 1, 1, 18, 17, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 12, 12, 12, 13, 12, 12, 12, 12, 0, 0, 0, 16, 12, 12, 12, 1, 2, 14, 2, 2, 16, 0, 16, 18, 16, 16, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 12, 12, 12, 12, 12, 12, 12, 19, 0, 0, 0, 4, 12, 12, 2, 18, 14, 1, 18, 18, 16, 17, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 12, 12, 12, 12, 12, 12, 2, 0, 0, 0, 2, 12, 12, 1, 1, 2, 2, 1, 18, 16, 16, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 12, 12, 12, 12, 12, 2, 0, 0, 0, 18, 12, 12, 18, 2, 2, 1, 16, 17, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 12, 12, 12, 12, 3, 0, 0, 0, 18, 12, 3, 1, 2, 1, 18, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 12, 12, 12, 3, 0, 0, 0, 17, 12, 2, 1, 2, 19, 17, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 12, 13, 3, 0, 0, 0, 0, 1, 2, 2, 2, 19, 19, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 12, 2, 0, 0, 0, 16, 1, 1, 2, 1, 16, 17, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 16, 1, 2, 2, 1, 1, 2, 1, 19, 17, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 2, 3, 2, 1, 2, 19, 18, 17, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 1, 19, 18, 18, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 16, 16, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 6, 15, 6, 14, 15, 15, 1, 0, 0, 0, 0, 14, 15, 15, 14, 6, 15, 6, 1, 0, 6, 6, 6, 14, 15, 6, 14, 16, 16, 15, 15, 6, 14, 15, 15, 2, 0, 0, 1, 15, 15, 14, 6, 15, 15, 15, 6, 6
    DB 21, 21, 21, 15, 21, 21, 14, 0, 0, 0, 0, 15, 21, 21, 15, 21, 21, 21, 21, 2, 15, 21, 21, 15, 21, 21, 21, 15, 1, 21, 21, 21, 14, 21, 21, 15, 0, 0, 15, 21, 21, 14, 11, 21, 21, 21, 21, 21
    DB 21, 21, 14, 2, 3, 4, 18, 0, 0, 0, 0, 15, 21, 11, 14, 2, 15, 11, 21, 14, 15, 11, 21, 14, 4, 21, 11, 11, 1, 15, 11, 21, 19, 6, 21, 21, 2, 17, 15, 11, 15, 7, 15, 11, 11, 14, 3, 3
    DB 11, 21, 1, 0, 0, 0, 0, 0, 0, 0, 0, 6, 11, 21, 4, 0, 15, 11, 21, 14, 15, 21, 21, 7, 19, 15, 11, 11, 1, 15, 11, 21, 18, 14, 21, 21, 6, 14, 21, 21, 5, 18, 21, 21, 11, 2, 2, 2
    DB 21, 21, 2, 0, 0, 0, 0, 0, 0, 0, 0, 6, 21, 21, 14, 0, 15, 11, 21, 14, 15, 11, 21, 15, 11, 21, 11, 15, 1, 15, 11, 21, 19, 0, 15, 11, 15, 15, 11, 21, 1, 1, 21, 11, 21, 21, 21, 21
    DB 11, 21, 1, 0, 0, 0, 0, 0, 0, 0, 0, 6, 21, 21, 14, 0, 15, 11, 21, 14, 15, 11, 21, 15, 21, 11, 15, 2, 16, 15, 11, 21, 1, 0, 14, 21, 21, 15, 11, 15, 0, 1, 21, 11, 21, 6, 5, 5
    DB 21, 21, 2, 0, 16, 16, 0, 0, 0, 0, 0, 6, 21, 21, 14, 0, 15, 11, 21, 14, 15, 21, 11, 14, 21, 11, 15, 18, 16, 15, 11, 21, 1, 0, 1, 21, 11, 21, 21, 3, 0, 17, 15, 21, 21, 18, 0, 16
    DB 21, 11, 15, 15, 21, 21, 3, 0, 0, 0, 0, 15, 21, 21, 15, 15, 21, 11, 21, 14, 15, 11, 21, 1, 5, 21, 11, 15, 18, 15, 11, 21, 2, 0, 0, 15, 11, 21, 15, 0, 0, 18, 21, 11, 11, 11, 21, 21
    DB 15, 11, 15, 15, 21, 21, 14, 0, 0, 0, 0, 6, 21, 11, 15, 21, 11, 21, 5, 16, 15, 11, 15, 1, 16, 15, 21, 15, 14, 15, 21, 15, 2, 0, 0, 2, 15, 21, 14, 0, 0, 18, 15, 11, 21, 21, 21, 21
    DB 0, 19, 18, 16, 18, 18, 0, 0, 0, 0, 0, 16, 19, 19, 17, 19, 19, 16, 0, 0, 17, 19, 18, 0, 0, 16, 1, 19, 16, 18, 1, 18, 0, 0, 0, 0, 19, 1, 0, 0, 0, 0, 19, 1, 1, 19, 1, 19
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 17, 0, 0, 0, 15, 15, 0, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 4, 20, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 15, 15, 15, 15, 2, 0, 0, 0, 4, 15, 15, 15, 2, 6, 20, 20, 15, 18, 4, 15, 15, 20, 15, 0, 5, 15, 15, 20, 15, 20, 20, 15, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 15, 20, 4, 19, 20, 20, 0, 0, 0, 15, 20, 6, 3, 0, 18, 20, 15, 2, 0, 17, 4, 4, 15, 20, 0, 15, 20, 15, 3, 18, 15, 20, 3, 18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 15, 20, 0, 0, 15, 20, 16, 0, 0, 16, 5, 15, 20, 14, 0, 20, 15, 0, 0, 6, 15, 6, 15, 20, 0, 6, 20, 17, 0, 0, 15, 20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 2, 20, 15, 15, 20, 15, 0, 0, 0, 15, 15, 15, 20, 15, 0, 15, 20, 15, 3, 15, 20, 6, 15, 20, 0, 15, 20, 2, 0, 0, 6, 20, 15, 15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 19, 5, 6, 2, 0, 0, 0, 0, 2, 6, 6, 3, 0, 0, 19, 6, 6, 1, 16, 6, 5, 2, 4, 0, 1, 4, 16, 0, 0, 16, 14, 15, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

var_4:
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 10, 10, 10, 16, 0, 0, 0, 0, 0, 0, 10, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 18, 16, 0, 19, 0, 16, 17, 17, 1, 0, 0, 10, 11, 2, 10, 11, 0, 0, 19, 2, 16, 0, 10, 11, 1, 1, 0, 0, 18, 17, 1, 18, 0, 2, 17, 0, 0, 1, 2, 17, 0, 16, 18, 18, 2, 0, 0, 0
    DB 11, 10, 16, 21, 1, 3, 11, 10, 10, 0, 0, 10, 11, 0, 10, 11, 0, 10, 11, 10, 11, 16, 10, 11, 10, 11, 10, 0, 11, 11, 10, 11, 10, 10, 11, 1, 19, 10, 10, 11, 2, 1, 11, 10, 11, 11, 0, 0
    DB 11, 10, 0, 11, 1, 3, 11, 16, 0, 0, 0, 10, 11, 11, 11, 18, 16, 21, 10, 3, 11, 2, 10, 11, 0, 10, 11, 0, 11, 10, 0, 11, 10, 0, 11, 3, 17, 10, 10, 11, 10, 1, 11, 0, 2, 11, 0, 0
    DB 11, 11, 10, 21, 1, 3, 11, 0, 0, 0, 0, 10, 11, 2, 11, 10, 0, 11, 11, 10, 10, 0, 10, 11, 0, 10, 11, 0, 11, 10, 0, 11, 10, 0, 21, 3, 10, 11, 10, 11, 10, 1, 21, 16, 3, 11, 0, 0
    DB 17, 10, 2, 2, 16, 17, 2, 0, 0, 0, 0, 18, 2, 0, 1, 3, 0, 0, 3, 10, 3, 0, 19, 2, 0, 1, 2, 0, 2, 1, 0, 2, 18, 0, 2, 17, 16, 10, 2, 2, 18, 16, 2, 0, 17, 2, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 16, 3, 5, 3, 0, 0, 0, 0, 3, 3, 16, 0, 3, 17, 0, 0, 0, 0, 0, 0, 0, 3, 5, 3, 0, 16, 4, 3, 3, 3, 16, 17, 2, 0, 16, 4, 5, 2, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 15, 15, 3, 15, 5, 0, 0, 6, 15, 15, 19, 16, 15, 2, 0, 0, 0, 0, 0, 0, 6, 15, 3, 15, 6, 16, 5, 5, 15, 15, 0, 6, 15, 0, 15, 6, 3, 15, 5, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 16, 0, 19, 15, 5, 0, 2, 15, 2, 15, 16, 0, 15, 2, 0, 0, 18, 18, 17, 16, 15, 4, 0, 4, 15, 16, 0, 0, 15, 4, 0, 6, 15, 0, 16, 0, 1, 15, 4, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 17, 6, 15, 2, 0, 17, 15, 15, 3, 15, 3, 16, 15, 1, 0, 16, 6, 6, 5, 17, 15, 4, 0, 4, 15, 16, 0, 4, 15, 16, 0, 6, 15, 0, 19, 6, 15, 2, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 15, 15, 4, 4, 3, 1, 5, 4, 6, 15, 4, 17, 15, 6, 4, 19, 0, 0, 0, 0, 5, 15, 4, 15, 6, 0, 16, 15, 5, 0, 0, 15, 15, 0, 15, 15, 4, 4, 3, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 2, 3, 3, 4, 2, 0, 0, 0, 0, 3, 0, 0, 3, 3, 4, 18, 0, 0, 0, 0, 0, 3, 4, 3, 0, 0, 18, 3, 16, 0, 0, 2, 2, 0, 2, 3, 3, 4, 2, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 15, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 21, 14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 4, 18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 21, 14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 21, 14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0