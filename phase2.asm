BITS 16
ORG 0x100

section .data
    scroll_offset: dw 0
    player_lane: dw 1
    
    ; Fixed lane X positions for car centering (road is X=50 to X=270)
    lane_positions:
        dw 70     ; Lane 0 (left)
        dw 143    ; Lane 1 (middle)
        dw 216    ; Lane 2 (right)
        
    enemy_lane: dw 0
    enemy_y: dw 0
    previous_enemy_y: dw -1
    previous_enemy_lane: dw 0

    ; --- COIN DATA ---
    coin_y: dw 0
    previous_coin_y: dw -1
    coin_lane: dw 0
    previous_coin_lane: dw 0
    coin_active: dw 0     ; 0 = inactive, 1 = active
    
    coin_width EQU 30
    coin_height EQU 30
    
    coin_positions:
        dw 72     ; Lane 0
        dw 145    ; Lane 1
        dw 218    ; Lane 2
        
    ; --- NEW: COIN SPAWN COUNTER ---
    coin_spawn_counter: dw 0
    COIN_SPAWN_THRESHOLD EQU 2   ; Spawn coin every 2 enemies

    
    ; --- JERRY CAN DATA ---
    jerry_y: dw 0
    previous_jerry_y: dw -1
    jerry_lane: dw 0
    previous_jerry_lane: dw 0
    jerry_active: dw 0     ; 0 = inactive, 1 = active
    
    jerry_width EQU 27
    jerry_height EQU 27
    
    jerry_positions:
        dw 74     ; Lane 0
        dw 147    ; Lane 1
        dw 220    ; Lane 2

    ; --- JERRY CAN SPAWN COUNTER ---
    jerry_spawn_counter: dw 0
    JERRY_SPAWN_THRESHOLD EQU 3   ; Spawn jerry can every 3 enemies

section .text
start:
    mov ah, 0Fh
    int 10h
    push ax
    
    mov ah, 0
    mov al, 13h
    int 10h
    
    call setup_palette
    
    mov ah, 01h
    mov ch, 32
    int 10h
    
    call draw_static_background
    call initialize_enemy_car

    ; --- Initialize coin & jerry can states ---
    mov word [coin_active], 0
    mov word [previous_coin_y], -1
    mov word [jerry_active], 0
    mov word [previous_jerry_y], -1
    
    ; --- Initialize new counters ---
    mov word [jerry_spawn_counter], 0
    mov word [coin_spawn_counter], 0

    call draw_scrolling_elements
    
main_loop:
    mov ah, 01h
    int 16h
    jz no_key
    
    mov ah, 0
    int 16h
    
    cmp ah, 01h
    je exit_game
    
    cmp ah, 4Bh
    je move_left
    
    cmp ah, 4Dh
    je move_right
    
    jmp no_key

move_left:
    mov ax, [player_lane]
    cmp ax, 0
    je no_key
    
    call erase_car_at_lane
    
    mov ax, [player_lane]
    dec ax
    mov [player_lane], ax
    
    call draw_player_car
    
    jmp no_key

move_right:
    mov ax, [player_lane]
    cmp ax, 2
    je no_key
    
    call erase_car_at_lane
    
    mov ax, [player_lane]
    inc ax
    mov [player_lane], ax
    
    call draw_player_car
    
    jmp no_key

no_key:
    mov ax, [scroll_offset]
    add ax, 10   ;background speed
    cmp ax, 40
    jl .no_reset_scroll
    xor ax, ax
.no_reset_scroll:
    mov [scroll_offset], ax

    call draw_scrolling_elements
    
    mov ax, [enemy_y]
    add ax, 6  ;blue car speed
    
    cmp ax, 200
    jl .enemy_still_visible
    
    ; --- Enemy is off-screen ---
    mov ax, [previous_enemy_y]
    cmp ax, 0
    jl .skip_final_erase
    call erase_enemy_at_y
.skip_final_erase:
    mov ax, -46     ; Reset above screen (use largest height, blue car is 46)
    mov [enemy_y], ax
    mov word [previous_enemy_y], ax

    ; --- ENEMY RANDOMIZE LOGIC ---
    call get_random_lane_in_dx
    mov [enemy_lane], dx
    mov [previous_enemy_lane], dx
    ; --- END ENEMY RANDOMIZE LOGIC ---

    
    ; --- NEW COIN SPAWN LOGIC (Based on counter) ---
    cmp word [coin_active], 1        ; Is one already active?
    je .skip_coin_spawn_check       ; Yes, don't spawn another

    ; Increment counter
    mov ax, [coin_spawn_counter]
    inc ax
    mov [coin_spawn_counter], ax

    ; Check if counter hit the threshold
    cmp ax, COIN_SPAWN_THRESHOLD
    jl .skip_coin_spawn_check      ; Not time to spawn yet
    
    ; --- Time to spawn a coin! ---
    mov word [coin_spawn_counter], 0 ; Reset counter
    
    call get_random_lane_in_dx  ; DX = new random lane
    
    mov cx, dx                  ; CX = potential coin lane
    mov ax, [enemy_lane]        ; AX = enemy's new lane
    
    cmp cx, ax                  ; Do they match?
    jne .coin_lane_ok           ; No, lane is fine
    
    ; --- CLASH ---
    inc cx
    cmp cx, 2
    jle .coin_lane_ok           ; If 1 or 2, it's ok
    xor cx, cx                  ; Wrap 3 back to 0

.coin_lane_ok:
    mov [coin_lane], cx
    mov [previous_coin_lane], cx
    mov word [coin_active], 1
    mov word [coin_y], -30
    mov word [previous_coin_y], -1
    ; --- END COIN SPAWN LOGIC ---

.skip_coin_spawn_check:
    ; --- JERRY CAN SPAWN LOGIC (Based on counter) ---
    cmp word [jerry_active], 1        ; Is one already active?
    je .skip_jerry_spawn_check      ; Yes, don't spawn another

    ; Increment counter
    mov ax, [jerry_spawn_counter]
    inc ax
    mov [jerry_spawn_counter], ax

    ; Check if counter hit the threshold
    cmp ax, JERRY_SPAWN_THRESHOLD
    jl .skip_jerry_spawn_check      ; Not time to spawn yet
    
    ; --- Time to spawn a jerry can! ---
    mov word [jerry_spawn_counter], 0 ; Reset counter

    call get_random_lane_in_dx  ; DX = new random lane
    
    mov cx, dx                  ; CX = potential jerry lane
    
.jerry_check_loop:
    ; Check against ACTIVE enemy car (which we just spawned)
    mov ax, [enemy_lane]
    cmp cx, ax
    je .jerry_clash           ; Clashes with enemy

    ; Check against ACTIVE coin
    cmp word [coin_active], 0   ; Is there an active coin?
    je .jerry_lane_ok           ; No coin, so we're fine
    mov bx, [coin_lane]
    cmp cx, bx
    je .jerry_clash           ; Clashes with *active* coin
    
    jmp .jerry_lane_ok          ; No clashes, lane is good

.jerry_clash:
    ; A clash with either car or coin occurred. Bump to next lane.
    inc cx
    cmp cx, 2
    jle .jerry_check_loop       ; If 1 or 2, check that new lane
    xor cx, cx                  ; Wrap 3 to 0, and check *that* lane
    jmp .jerry_check_loop

.jerry_lane_ok:
    mov [jerry_lane], cx
    mov [previous_jerry_lane], cx
    
    mov word [jerry_active], 1
    mov word [jerry_y], -27   ; Start above screen (height=27)
    mov word [previous_jerry_y], -1

.skip_jerry_spawn_check:
    jmp .enemy_done
    
.enemy_still_visible:
    mov [enemy_y], ax
    
    mov ax, [previous_enemy_y]
    call erase_enemy_at_y
.skip_erase:
    
    mov ax, [enemy_y]
    mov [previous_enemy_y], ax
    mov dx, [enemy_lane]
    mov [previous_enemy_lane], dx
    
.enemy_done:

    ; --- COIN UPDATE ---
    cmp word [coin_active], 0
    je .skip_coin_update
    
    mov ax, [coin_y]
    add ax, 3         ; Move coin
    
    cmp ax, 200
    jl .coin_still_visible
    
    ; --- Coin is off-screen (below) ---
    mov ax, [previous_coin_y]
    push ax
    add ax, coin_height
    pop ax
    cmp ax, 0
    jl .skip_final_coin_erase
    cmp ax, 200
    jge .skip_final_coin_erase
    call erase_coin
.skip_final_coin_erase:
    
    mov word [coin_active], 0
    mov word [previous_coin_y], -1
    
    jmp .coin_update_done
    
.coin_still_visible:
    mov [coin_y], ax
    
    mov ax, [previous_coin_y]
    push ax
    add ax, coin_height
    cmp ax, 0
    jle .skip_coin_erase
    pop ax
    push ax
    cmp ax, 200
    jge .skip_coin_erase
    pop ax
    call erase_coin
    jmp .coin_erase_done
.skip_coin_erase:
    pop ax
.coin_erase_done:
    
    mov ax, [coin_y]
    mov [previous_coin_y], ax
    mov dx, [coin_lane]
    mov [previous_coin_lane], dx
    
.coin_update_done:
.skip_coin_update:
    ; --- END COIN UPDATE ---

    ; --- JERRY CAN UPDATE ---
    cmp word [jerry_active], 0
    je .skip_jerry_update
    
    mov ax, [jerry_y]
    add ax, 3         ; Move jerry can at same speed
    
    cmp ax, 200
    jl .jerry_still_visible
    
    ; --- Jerry can is off-screen (below) ---
    mov ax, [previous_jerry_y]
    push ax
    add ax, jerry_height
    pop ax
    cmp ax, 0
    jl .skip_final_jerry_erase
    cmp ax, 200
    jge .skip_final_jerry_erase
    call erase_jerry
.skip_final_jerry_erase:
    
    mov word [jerry_active], 0
    mov word [previous_jerry_y], -1
    jmp .jerry_update_done
    
.jerry_still_visible:
    mov [jerry_y], ax
    
    mov ax, [previous_jerry_y]
    push ax
    add ax, jerry_height
    cmp ax, 0
    jle .skip_jerry_erase
    pop ax
    push ax
    cmp ax, 200
    jge .skip_jerry_erase
    pop ax
    call erase_jerry
    jmp .jerry_erase_done
.skip_jerry_erase:
    pop ax
.jerry_erase_done:
    
    mov ax, [jerry_y]
    mov [previous_jerry_y], ax
    mov dx, [jerry_lane]
    mov [previous_jerry_lane], dx
    
.jerry_update_done:
.skip_jerry_update:
    ; --- END JERRY CAN UPDATE ---

    call draw_enemy_car
    
    ; --- Draw coin if active ---
    cmp word [coin_active], 0
    je .skip_draw_coin
    call draw_coin
.skip_draw_coin:

    ; --- Draw jerry can if active ---
    cmp word [jerry_active], 0
    je .skip_draw_jerry
    call draw_jerry
.skip_draw_jerry:

    call draw_player_car
    call wait_for_retrace
    
    jmp main_loop

exit_game:
    mov ah, 01h
    mov ch, 6
    mov cl, 7
    int 10h
    
    pop ax
    and al, 0x7F
    mov ah, 0
    int 10h
    
    mov ah, 4Ch
    int 21h

;--------------------------------------
initialize_enemy_car:
    push ax
    push bx
    push dx
    
    mov ax, -46     ; Start above screen (car height = 46)
    mov [enemy_y], ax
    mov word [previous_enemy_y], -1
    
    ; --- Randomize initial enemy lane ---
    call get_random_lane_in_dx  ; DX = random lane
    mov [enemy_lane], dx
    mov [previous_enemy_lane], dx
    ; --- End Randomize ---
    
    pop dx
    pop bx
    pop ax
    ret

;--------------------------------------
; get_random_lane_in_dx
; Returns a random lane (0, 1, or 2) in DX
; Trashes: AX, BX, ES
;--------------------------------------
get_random_lane_in_dx:
    push ax
    push bx
    push es
    
    mov ax, 0040h
    mov es, ax
    mov ax, [es:006Ch]  ; Get BIOS timer tick
    mov bx, 3
    xor dx, dx
    div bx              ; DX = random lane (0, 1, or 2)
    
    pop es
    pop bx
    pop ax
    ret

erase_enemy_at_y:
    push ax
    push bx
    push cx
    push dx
    push si
    
    mov dx, ax  ; DX holds Y (top of car)
    
    mov ax, dx
    add ax, blue_car_large_height
    
    cmp ax, 0
    jle .erase_done
    cmp dx, 200
    jge .erase_done
    
    mov ax, [previous_enemy_lane]
    shl ax, 1
    mov bx, ax
    mov bx, [lane_positions + bx]
    
    mov cx, 0 ; Row counter
.erase_row:
    cmp cx, blue_car_large_height
    jge .erase_done
    
    cmp dx, 0
    jl .skip_row
    cmp dx, 199
    jg .skip_row
    
    mov ax, bx
    mov si, 0 ; Col counter
.erase_col:
    cmp si, blue_car_large_width
    jge .next_erase_row
    
    push cx
    push dx
    push si
    push ax
    
    mov cx, ax
    mov ah, 0Ch
    mov al, 82
    mov bh, 0
    int 10h
    
    pop ax
    pop si
    pop dx
    pop cx
    
    inc ax
    inc si
    jmp .erase_col
    
.skip_row:
    inc dx
    inc cx
    jmp .erase_row
    
.next_erase_row:
    inc dx
    inc cx
    jmp .erase_row
    
.erase_done:
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

;--------------------------------------
draw_static_background:
    push ax
    push bx
    push cx
    push dx
    call draw_grass
    call draw_road
    call draw_left_trees
    call draw_right_trees
    call draw_right_grass_fill
    pop dx
    pop cx
    pop bx
    pop ax
    ret

;--------------------------------------
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

;--------------------------------------
draw_grass:
    push ax
    push bx
    push cx
    push dx
    mov dx, 0
.grass_row:
    mov cx, 0
.grass_col:
    mov ah, 0Ch
    mov al, 120
    mov bh, 0
    int 10h
    inc cx
    cmp cx, 320
    jl .grass_col
    inc dx
    cmp dx, 200
    jl .grass_row
    pop dx
    pop cx
    pop bx
    pop ax
    ret
    
;--------------------------------------
draw_road:
    push ax
    push bx
    push cx
    push dx
    mov dx, 0
.road_row:
    mov cx, 50
.road_col:
    mov ah, 0Ch
    mov al, 82
    mov bh, 0
    int 10h
    inc cx
    cmp cx, 270
    jl .road_col
    inc dx
    cmp dx, 200
    jl .road_row
    pop dx
    pop cx
    pop bx
    pop ax
    ret

;--------------------------------------
draw_left_barrier_scroll:
    push ax
    push bx
    push cx
    push dx
    push si
    mov dx, 0
.left_bar_row:
    mov ax, dx
    sub ax, [scroll_offset]
.wrap_check:
    cmp ax, 0
    jge .wrap_done
    add ax, 20
    jmp .wrap_check
.wrap_done:
    cmp ax, 20
    jl .in_range
    sub ax, 20
    jmp .wrap_done
.in_range:
    cmp ax, 10
    jl .left_red
    mov al, 89
    jmp .left_draw
.left_red:
    mov al, 104
.left_draw:
    mov si, 30
.left_bar_col:
    push cx
    push dx
    mov cx, si
    mov ah, 0Ch
    mov bh, 0
    int 10h
    pop dx
    pop cx
    inc si
    cmp si, 50
    jl .left_bar_col
    inc dx
    cmp dx, 200
    jl .left_bar_row
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

;--------------------------------------
draw_right_barrier_scroll:
    push ax
    push bx
    push cx
    push dx
    push si
    mov dx, 0
.right_bar_row:
    mov ax, dx
    sub ax, [scroll_offset]
.wrap_check:
    cmp ax, 0
    jge .wrap_done
    add ax, 20
    jmp .wrap_check
.wrap_done:
    cmp ax, 20
    jl .in_range
    sub ax, 20
    jmp .wrap_done
.in_range:
    cmp ax, 10
    jl .right_red
    mov al, 89
    jmp .right_draw
.right_red:
    mov al, 104
.right_draw:
    mov si, 270
.right_bar_col:
    push cx
    push dx
    mov cx, si
    mov ah, 0Ch
    mov bh, 0
    int 10h
    pop dx
    pop cx
    inc si
    cmp si, 290
    jl .right_bar_col
    inc dx
    cmp dx, 200
    jl .right_bar_row
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
;--------------------------------------
draw_right_grass_fill:
    push ax
    push bx
    push cx
    push dx
    mov dx, 0
.right_grass_row:
    mov cx, 290
.right_grass_col:
    mov ah, 0Ch
    mov al, 120
    mov bh, 0
    int 10h
    inc cx
    cmp cx, 320
    jl .right_grass_col
    inc dx
    cmp dx, 200
    jl .right_grass_row
    pop dx
    pop cx
    pop bx
    pop ax
    ret

;--------------------------------------
draw_lanes_scroll:
    push ax
    push bx
    push cx
    push dx
    mov cx, 118
    call draw_thick_dashed_line_scroll
    mov cx, 198
    call draw_thick_dashed_line_scroll
    pop dx
    pop cx
    pop bx
    pop ax
    ret

;--------------------------------------
draw_thick_dashed_line_scroll:
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    mov si, cx
    mov di, [scroll_offset]
    mov bx, 0
.column_loop:
    mov cx, si
    add cx, bx
    mov dx, 0
.dash_loop:
    mov ax, dx
    sub ax, di
.mod_loop:
    cmp ax, 0
    jge .check_upper
    add ax, 40
    jmp .mod_loop
.check_upper:
    cmp ax, 40
    jl .mod_done
    sub ax, 40
    jmp .check_upper
.mod_done:
    cmp ax, 20
    jge .skip_dash_pixel
    push cx
    push dx
    mov ah, 0Ch
    mov al, 89
    mov bh, 0
    int 10h
    pop dx
    pop cx
    jmp .pixel_done
.skip_dash_pixel:
    push cx
    push dx
    mov ah, 0Ch
    mov al, 82
    mov bh, 0
    int 10h
    pop dx
    pop cx
.pixel_done:
    inc dx
    cmp dx, 200
    jl .dash_loop
    inc bx
    cmp bx, 5
    jl .column_loop
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

;--------------------------------------
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

;--------------------------------------
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

;--------------------------------------
draw_tree_left:
    push ax
    push cx
    push dx
    push si
    mov dx, bx
    sub dx, 8
.tree_l_row:
    mov si, 8
.tree_l_col:
    push cx
    push dx
    mov cx, si
    mov ah, 0Ch
    mov al, 120
    mov bh, 0
    int 10h
    pop dx
    pop cx
    inc si
    cmp si, 22
    jl .tree_l_col
    inc dx
    mov ax, dx
    sub ax, bx
    cmp ax, 8
    jl .tree_l_row
    pop si
    pop dx
    pop cx
    pop ax
    ret

;--------------------------------------
draw_tree_right:
    push ax
    push cx
    push dx
    push si
    mov dx, bx
    sub dx, 8
.tree_r_row:
    mov si, 298
.tree_r_col:
    push cx
    push dx
    mov cx, si
    mov ah, 0Ch
    mov al, 120
    mov bh, 0
    int 10h
    pop dx
    pop cx
    inc si
    cmp si, 312
    jl .tree_r_col
    inc dx
    mov ax, dx
    sub ax, bx
    cmp ax, 8
    jl .tree_r_row
    pop si
    pop dx
    pop cx
    pop ax
    ret

;--------------------------------------
draw_pixel:
    push ax
    push bx
    push cx
    push dx
    mov ah, 0Ch
    mov bh, 0
    int 10h
    pop dx
    pop cx
    pop bx
    pop ax
    ret

;--------------------------------------
; DRAWS PLAYER CAR (CHANGED: NOW DRAWS RED CAR)
;--------------------------------------
draw_player_car:
    pusha
    
    mov si, img_reed_car_large
    
    mov bp, 199
    mov di, 0
.car_row_loop:
    mov ax, [player_lane]
    shl ax, 1
    mov bx, ax
    mov bx, [lane_positions + bx]
    mov cx, 0
.car_col_loop:
    mov al, [si]
    cmp al, 0
    je .skip_car_pixel
    pusha
    mov cx, bx
    mov dx, bp
    call draw_pixel
    popa
.skip_car_pixel:
    inc si
    inc bx
    inc cx
    
    cmp cx, reed_car_large_width
    jl .car_col_loop
    dec bp
    inc di
    
    cmp di, reed_car_large_height
    jl .car_row_loop
    popa
    ret

;--------------------------------------
; DRAWS ENEMY CAR (CHANGED: NOW DRAWS BLUE CAR)
;--------------------------------------
draw_enemy_car:
    pusha
    
    mov ax, [enemy_y]
    cmp ax, 200
    jge .draw_done
    
    add ax, blue_car_large_height
    cmp ax, 0
    jle .draw_done
    
    mov si, img_blue_car_large
    
    mov ax, [enemy_y]
    add ax, blue_car_large_height
    add ax, -1
    mov bp, ax
    
    mov di, 0
    
.enemy_row_loop:
    cmp bp, 0
    jl .skip_entire_row
    cmp bp, 199
    jg .skip_entire_row
    
    mov ax, [enemy_lane]
    shl ax, 1
    mov bx, ax
    mov bx, [lane_positions + bx]
    
    mov cx, 0
    
.enemy_col_loop:
    mov al, [si]
    cmp al, 0
    je .skip_enemy_pixel
    
    pusha
    mov cx, bx
    mov dx, bp
    call draw_pixel
    popa
    
.skip_enemy_pixel:
    inc si
    inc bx
    inc cx
    cmp cx, blue_car_large_width
    jl .enemy_col_loop
    jmp .next_enemy_row
    
.skip_entire_row:
    add si, blue_car_large_width
    
.next_enemy_row:
    dec bp
    inc di
    cmp di, blue_car_large_height
    jl .enemy_row_loop
    
.draw_done:
    popa
    ret

;--------------------------------------
erase_car_at_lane:
    push ax
    push bx
    push cx
    push dx
    push si
    mov ax, [player_lane]
    shl ax, 1
    mov bx, ax
    mov bx, [lane_positions + bx]
    mov dx, 154
    mov cx, 0
.erase_row:
    mov ax, bx
    mov si, 0
.erase_col:
    push cx
    push dx
    push si
    push ax
    mov cx, ax
    mov ah, 0Ch
    mov al, 82
    mov bh, 0
    int 10h
    pop ax
    pop si
    pop dx
    pop cx
    inc ax
    inc si
    cmp si, reed_car_large_width
    jl .erase_col
    inc dx
    inc cx
    cmp cx, reed_car_large_height
    jl .erase_row
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

;--------------------------------------
wait_for_retrace:
    mov dx, 0x3DA
.wait_v:
    in al, dx
    test al, 0x08
    jnz .wait_v
.wait_h:
    in al, dx
    test al, 0x08
    jz .wait_h
    ret

;--------------------------------------
; PALETTE PROCEDURE (USING YOUR ORIGINAL)
;--------------------------------------
; Replace the setup_palette procedure in your game code with this updated version:

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
    mov al, 45      ; G (bright green)
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
    mov al, 63      ; R
    out dx, al
    mov al, 60      ; G (slightly reduced)
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
    mov al, 48      ; R (reduced from 63 for better depth)
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
    mov al, 56      ; R (slightly reduced from 63)
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
    mov al, 12      ; G (reduced from 20)
    out dx, al
    mov al, 12      ; B (reduced from 20)
    out dx, al
    
    ; --- REMAINING COLORS (unchanged from your original) ---
    
    ; Color 94 - Dark Red
    mov dx, 0x03C8
    mov al, 94
    out dx, al
    inc dx
    mov al, 24
    out dx, al
    mov al, 0
    out dx, al
    mov al, 0
    out dx, al
    
    ; Color 96 - Medium Dark Red
    mov dx, 0x03C8
    mov al, 96
    out dx, al
    inc dx
    mov al, 32
    out dx, al
    mov al, 0
    out dx, al
    mov al, 0
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
    mov al, 50      ; G (bright green)
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
    mov al, 0       ; B
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
    mov al, 0       ; G
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
    mov al, 0       ; G
    out dx, al
    mov al, 0       ; B
    out dx, al

    ; Color 132 - Medium Blue (windshield light)
    mov dx, 0x03C8
    mov al, 132
    out dx, al
    inc dx
    mov al, 0       ; R
    out dx, al
    mov al, 0       ; G
    out dx, al
    mov al, 0       ; B
    out dx, al

    ; Color 134 - Medium Bright Blue (body highlight)
    mov dx, 0x03C8
    mov al, 134
    out dx, al
    inc dx
    mov al, 0       ; R
    out dx, al
    mov al, 0       ; G
    out dx, al
    mov al, 0       ; B
    out dx, al

    ; Color 136 - Bright Blue (edges/highlights)
    mov dx, 0x03C8
    mov al, 136
    out dx, al
    inc dx
    mov al, 20      ; R
    out dx, al
    mov al, 45      ; G
    out dx, al
    mov al, 63      ; B
    out dx, al

    ; Color 137 - Almost Black (bumper)
    mov dx, 0x03C8
    mov al, 137
    out dx, al
    inc dx
    mov al, 0       ; R
    out dx, al
    mov al, 0       ; G
    out dx, al
    mov al, 20      ; B
    out dx, al

    ; Color 141 - Light Blue (trim)
    mov dx, 0x03C8
    mov al, 141
    out dx, al
    inc dx
    mov al, 0       ; R
    out dx, al
    mov al, 20      ; G
    out dx, al
    mov al, 55      ; B
    out dx, al
    
    popa
    ret

;--------------------------------------
; Draw coin (with Y-Clipping)
;--------------------------------------
draw_coin:
    pusha

    mov si, coin_sprite
    mov ax, [coin_y]
    add ax, coin_height
    dec ax
    mov bp, ax      ; BP = bottom row Y

    mov di, 0       ; DI = row counter
.coin_row_loop:
    cmp bp, 0
    jl .skip_entire_row
    cmp bp, 199
    jg .skip_entire_row

    mov ax, [coin_lane]
    shl ax, 1
    mov bx, ax
    mov bx, [coin_positions + bx]

    mov cx, 0
.coin_col_loop:
    mov al, [si]
    cmp al, 0
    je .skip_coin_pixel

    pusha
    mov cx, bx
    mov dx, bp
    call draw_pixel
    popa

.skip_coin_pixel:
    inc si
    inc bx
    inc cx
    cmp cx, coin_width
    jl .coin_col_loop
    jmp .next_coin_row

.skip_entire_row:
    add si, coin_width

.next_coin_row:
    dec bp
    inc di
    cmp di, coin_height
    jl .coin_row_loop

    popa
    ret

;--------------------------------------
; Erase coin (with Y-Clipping)
;--------------------------------------
erase_coin:
    pusha
    
    ; AX holds Y (top of coin)
    cmp ax, 200
    jge .erase_done
    
    mov dx, ax
    add ax, coin_height
    cmp ax, 0
    jle .erase_done
    
    mov ax, [previous_coin_lane]
    shl ax, 1
    mov bx, ax
    mov bx, [coin_positions + bx]
    
    mov cx, 0 ; Row counter
.erase_row:
    cmp cx, coin_height
    jge .erase_done
    
    cmp dx, 0
    jl .skip_row
    cmp dx, 199
    jg .skip_row
    
    mov ax, bx
    mov si, 0
.erase_col:
    cmp si, coin_width
    jge .next_erase_row
    
    pusha
    mov cx, ax
    mov ah, 0Ch
    mov al, 82
    mov bh, 0
    int 10h
    popa
    
    inc ax
    inc si
    jmp .erase_col
    
.skip_row:
    
.next_erase_row:
    inc dx
    inc cx
    jmp .erase_row
    
.erase_done:
    popa
    ret

;--------------------------------------
; NEW: Draw jerry can (FIXED: Draws top-down)
;--------------------------------------
draw_jerry:
    pusha

    mov si, img_jerry_can    ; Use jerry can sprite
    
    mov ax, [jerry_y]
    mov bp, ax          ; BP = top row Y

    mov di, 0           ; DI = row counter
.jerry_row_loop:
    cmp bp, 0
    jl .skip_entire_row     ; Clip if above screen
    cmp bp, 199
    jg .skip_entire_row     ; Clip if below screen

    mov ax, [jerry_lane]
    shl ax, 1
    mov bx, ax
    mov bx, [jerry_positions + bx] ; Use jerry can X positions

    mov cx, 0
.jerry_col_loop:
    mov al, [si]
    cmp al, 0
    je .skip_jerry_pixel

    pusha
    mov cx, bx
    mov dx, bp
    call draw_pixel
    popa

.skip_jerry_pixel:
    inc si
    inc bx
    inc cx
    cmp cx, jerry_width   ; Use jerry can width
    jl .jerry_col_loop
    jmp .next_jerry_row ; Go to next row logic

.skip_entire_row:
    add si, jerry_width   ; Skip this row's data

.next_jerry_row:
    inc bp
    inc di
    cmp di, jerry_height  ; Use jerry can height
    jl .jerry_row_loop

    popa
    ret

;--------------------------------------
; NEW: Erase jerry can
;--------------------------------------
erase_jerry:
    pusha
    
    ; AX holds Y (top of can)
    cmp ax, 200
    jge .erase_done
    
    mov dx, ax
    add ax, jerry_height
    cmp ax, 0
    jle .erase_done
    
    mov ax, [previous_jerry_lane]
    shl ax, 1
    mov bx, ax
    mov bx, [jerry_positions + bx]
    
    mov cx, 0 ; Row counter
.erase_row:
    cmp cx, jerry_height
    jge .erase_done
    
    cmp dx, 0
    jl .skip_row
    cmp dx, 199
    jg .skip_row
    
    mov ax, bx
    mov si, 0
.erase_col:
    cmp si, jerry_width
    jge .next_erase_row
    
    pusha
    mov cx, ax
    mov ah, 0Ch
    mov al, 82
    mov bh, 0
    int 10h
    popa
    
    inc ax
    inc si
    jmp .erase_col
    
.skip_row:
    
.next_erase_row:
    inc dx
    inc cx
    jmp .erase_row
    
.erase_done:
    popa
    ret

;--------------------------------------
; CAR SPRITE DATA
;--------------------------------------
reed_car_large_width EQU 35
reed_car_large_height EQU 46
img_reed_car_large:
    DB 0, 0, 0, 74, 74, 90, 90, 90, 90, 90, 90, 90, 90, 74, 74, 74, 74, 74, 74, 74, 90, 90, 90, 90, 90, 90, 90, 90, 74, 74, 0, 0, 0, 0, 0
    DB 0, 0, 74, 74, 74, 82, 74, 74, 87, 87, 87, 74, 87, 90, 90, 90, 90, 90, 90, 90, 87, 74, 74, 87, 87, 87, 74, 87, 74, 74, 74, 0, 0, 0, 0
    DB 0, 0, 90, 90, 82, 87, 74, 74, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 74, 87, 87, 87, 90, 0, 0, 0, 0, 0, 0
    DB 0, 0, 90, 90, 82, 74, 87, 87, 105, 87, 87, 87, 82, 82, 82, 82, 82, 82, 82, 82, 82, 87, 87, 87, 105, 105, 87, 74, 87, 87, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 82, 74, 87, 87, 105, 87, 87, 87, 82, 82, 82, 82, 82, 82, 82, 82, 82, 87, 87, 87, 105, 105, 87, 74, 87, 87, 90, 0, 0, 0, 0
    DB 0, 0,90, 90, 90, 87, 105, 105, 105, 90, 90, 90, 82, 82, 82, 82, 82, 82, 82, 82, 82, 90, 90, 90, 105, 105, 105, 87, 90, 90, 90, 0, 0, 0, 0
    DB 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90
    DB 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90
    DB 82, 77, 77, 77, 77, 77, 77, 82, 77, 77, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 77, 77, 82, 82, 82, 90, 90, 90
    DB 87, 82, 82, 87, 87, 87, 87, 74, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 74, 74, 87, 87, 87, 87, 82, 87, 87, 0, 0, 0
    DB 90, 90, 90, 90, 90, 90, 90, 74, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 74, 74, 90, 90, 90, 90, 90, 90, 90, 0, 0, 0
    DB 90, 90, 90, 90, 90, 90, 90, 74, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 74, 74, 90, 90, 90, 90, 90, 90, 90, 0, 0, 0
    DB 0, 74, 74, 90, 87, 90, 90, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 90, 87, 90, 90, 74, 0, 0, 0, 0, 0, 0
    DB 0, 90, 90, 74, 90, 74, 74, 90, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 90, 90, 74, 90, 87, 87, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 74, 90, 74, 74, 90, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 90, 90, 74, 90, 87, 87, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 74, 77, 87, 87, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 74, 77, 74, 74, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 74, 77, 90, 90, 82, 77, 77, 77, 77, 77, 77, 82, 82, 77, 82, 82, 87, 87, 87, 82, 90, 90, 90, 77, 74, 74, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 74, 77, 90, 90, 74, 74, 74, 74, 74, 74, 74, 105, 105, 105, 105, 105, 74, 74, 74, 74, 74, 74, 90, 77, 74, 74, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 74, 77, 90, 90, 74, 74, 74, 74, 74, 74, 74, 105, 105, 105, 105, 105, 74, 74, 74, 74, 74, 74, 90, 77, 74, 74, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 74, 77, 90, 90, 74, 74, 74, 74, 74, 74, 74, 105, 254, 105, 105, 105, 74, 74, 74, 74, 87, 87, 90, 77, 74, 74, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 74, 77, 90, 90, 74, 74, 74, 74, 74, 74, 74, 105, 105, 105, 105, 105, 74, 74, 74, 74, 74, 74, 90, 77, 74, 74, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 74, 77, 90, 90, 74, 74, 74, 74, 74, 74, 74, 105, 254, 105, 105, 105, 74, 74, 74, 74, 74, 74, 90, 77, 74, 74, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 74, 77, 90, 90, 74, 74, 74, 74, 74, 74, 74, 105, 254, 105, 105, 105, 74, 74, 74, 74, 74, 74, 90, 77, 74, 74, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 74, 77, 90, 90, 74, 74, 74, 74, 74, 74, 74, 105, 254, 105, 105, 105, 74, 74, 74, 74, 74, 74, 90, 77, 74, 74, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 74, 77, 90, 90, 74, 87, 87, 74, 74, 74, 74, 254, 254, 105, 105, 105, 74, 74, 74, 74, 74, 74, 90, 77, 74, 74, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 74, 77, 90, 90, 74, 87, 87, 74, 74, 74, 74, 254, 254, 105, 105, 105, 74, 74, 74, 74, 74, 74, 90, 77, 74, 74, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 74, 77, 90, 90, 74, 87, 87, 87, 74, 74, 74, 254, 254, 254, 105, 105, 74, 74, 74, 74, 74, 74, 90, 77, 90, 90, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 74, 77, 90, 90, 74, 87, 87, 87, 74, 74, 74, 254, 254, 254, 105, 105, 74, 74, 74, 74, 74, 74, 90, 77, 90, 90, 90, 0, 0, 0, 0
    DB 0, 0, 90, 77, 77, 82, 90, 90, 90, 74, 87, 87, 87, 74, 74, 74, 254, 254, 254, 254, 254, 74, 74, 74, 74, 74, 74, 90, 90, 82, 82, 77, 90, 90, 0
    DB 0, 0, 90, 87, 87, 87, 74, 87, 87, 74, 87, 87, 87, 74, 74, 74, 254, 254, 254, 254, 254, 74, 74, 74, 74, 74, 74, 87, 74, 87, 87, 87, 90, 90, 0
    DB 0, 0, 90, 87, 87, 87, 74, 87, 87, 74, 87, 87, 87, 74, 74, 74, 254, 254, 254, 254, 254, 74, 74, 74, 74, 74, 74, 87, 74, 87, 87, 87, 90, 90, 0
    DB 0, 0, 74, 90, 90, 74, 74, 77, 77, 74, 74, 74, 74, 74, 74, 74, 105, 105, 105, 105, 105, 74, 74, 74, 74, 74, 74, 77, 74, 74, 74, 90, 74, 74, 0
    DB 0, 0, 0, 90, 90, 74, 74, 90, 90, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 90, 74, 74, 74, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 90, 74, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 74, 90, 90, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 90, 74, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 74, 90, 90, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 90, 87, 74, 74, 87, 82, 82, 82, 82, 82, 82, 87, 82, 82, 87, 87, 82, 82, 82, 87, 87, 87, 90, 74, 90, 90, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 90, 87, 74, 74, 90, 74, 74, 74, 74, 74, 74, 105, 105, 105, 105, 105, 74, 74, 74, 74, 90, 90, 74, 87, 90, 90, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 90, 87, 74, 74, 90, 74, 74, 74, 74, 74, 74, 105, 105, 105, 105, 105, 74, 74, 74, 74, 90, 90, 74, 87, 90, 90, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 90, 87, 74, 74, 90, 74, 74, 87, 74, 74, 74, 105, 105, 105, 105, 105, 74, 74, 74, 74, 90, 90, 74, 87, 90, 90, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 90, 87, 74, 74, 90, 74, 74, 74, 74, 74, 74, 105, 105, 105, 105, 105, 74, 74, 74, 74, 90, 90, 74, 87, 90, 90, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 74, 87, 74, 74, 90, 74, 74, 87, 74, 74, 74, 105, 254, 105, 105, 105, 74, 74, 74, 74, 90, 90, 74, 87, 74, 74, 90, 0, 0, 0, 0
    DB 0, 0, 90, 90, 90, 87, 74, 74, 74, 87, 87, 87, 74, 74, 74, 105, 254, 254, 105, 105, 87, 74, 74, 74, 74, 74, 74, 87, 90, 90, 90, 0, 0, 0, 0
    DB 0, 0, 0, 0, 90, 90, 74, 74, 90, 74, 74, 87, 87, 74, 74, 105, 105, 105, 105, 105, 74, 74, 74, 74, 90, 90, 74, 90, 90, 90, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 90, 90, 74, 74, 90, 74, 74, 87, 87, 74, 74, 105, 105, 105, 105, 105, 74, 74, 74, 74, 90, 90, 74, 90, 90, 90, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 74, 94, 94, 90, 90, 90, 74, 74, 74, 74, 87, 87, 87, 87, 87, 74, 74, 74, 90, 90, 90, 94, 74, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 87, 87, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 87, 0, 0, 0, 0, 0, 0, 0, 0, 0
    
;    NPC CAR SPRITE DATA    
    blue_car_large_width EQU 35
    blue_car_large_height EQU 46
    img_blue_car_large:
         DB 0, 0, 0, 0, 141, 141, 130, 130, 130, 130, 130, 130, 130, 130, 141, 141, 141, 141, 141, 141, 141, 130, 130, 130, 130, 130, 130, 130, 130, 141, 141, 0, 0, 0, 0, 0, 0, 0, 141, 141
         DB 141, 136, 141, 141, 141, 141, 141, 141, 141, 130, 130, 130, 130, 130, 130, 130, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 0, 0, 0, 0, 0, 0, 130, 130, 136, 141, 141, 141, 130
         DB 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 141, 141, 141, 141, 130, 0, 0, 0, 0, 0, 0, 130, 130, 136, 141, 141, 141, 132, 141, 141, 141, 136, 136
         DB 136, 136, 136, 136, 136, 136, 136, 141, 141, 141, 132, 132, 141, 141, 141, 141, 130, 0, 0, 0, 0, 0, 0, 130, 130, 136, 141, 141, 141, 132, 141, 141, 141, 136, 136, 136, 136, 136, 136, 136
         DB 136, 136, 141, 141, 141, 132, 132, 141, 141, 141, 141, 130, 0, 0, 0, 0, 0, 0, 130, 130, 130, 141, 78, 78, 132, 130, 130, 130, 136, 136, 136, 136, 136, 136, 136, 136, 136, 130, 130, 130
         DB 132, 132, 78, 141, 130, 130, 130, 0, 0, 0, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130
         DB 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130
         DB 130, 130, 136, 137, 137, 137, 137, 137, 137, 136, 137, 137, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 137, 137, 136, 136, 136, 130, 130, 130, 141, 136, 136
         DB 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 136, 141, 141, 130, 0, 0, 130, 130, 130, 130, 130, 130, 130, 141
         DB 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 141, 141, 130, 130, 130, 130, 130, 130, 130, 0, 0, 0, 130, 130, 130, 130, 130, 130, 130, 141, 130, 130, 130, 130, 130
         DB 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 141, 141, 130, 130, 130, 130, 130, 130, 130, 0, 0, 0, 0, 141, 141, 130, 141, 130, 130, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126
         DB 126, 126, 126, 126, 126, 126, 126, 130, 141, 130, 130, 141, 0, 0, 0, 0, 0, 0, 130, 130, 141, 130, 141, 141, 130, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126
         DB 130, 130, 141, 130, 141, 141, 130, 0, 0, 0, 0, 0, 0, 130, 130, 141, 130, 141, 141, 130, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 130, 130, 141, 130, 141
         DB 141, 130, 0, 0, 0, 0, 0, 0, 130, 130, 141, 126, 141, 141, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 141, 126, 141, 141, 130, 0, 0, 0
         DB 0, 0, 0, 130, 130, 141, 126, 130, 130, 136, 137, 137, 137, 137, 137, 137, 136, 136, 137, 136, 136, 141, 141, 141, 136, 130, 130, 130, 126, 141, 141, 130, 0, 0, 0, 0, 0, 0, 130, 130
         DB 141, 126, 130, 130, 141, 141, 141, 141, 141, 141, 141, 134, 134, 134, 134, 134, 141, 141, 141, 141, 141, 141, 130, 126, 141, 141, 130, 0, 0, 0, 0, 0, 0, 130, 130, 141, 126, 130, 130, 141
         DB 141, 141, 141, 141, 141, 141, 134, 134, 134, 134, 134, 141, 141, 141, 141, 141, 141, 130, 126, 141, 141, 130, 0, 0, 0, 0, 0, 0, 130, 130, 141, 126, 130, 130, 141, 141, 141, 141, 141, 141
         DB 141, 134, 128, 134, 134, 134, 141, 141, 141, 141, 141, 141, 130, 126, 141, 141, 130, 0, 0, 0, 0, 0, 0, 130, 130, 141, 126, 130, 130, 141, 141, 141, 141, 141, 141, 141, 134, 134, 134, 134
         DB 134, 141, 141, 141, 141, 141, 141, 130, 126, 141, 141, 130, 0, 0, 0, 0, 0, 0, 130, 130, 141, 126, 130, 130, 141, 141, 141, 141, 141, 141, 141, 134, 128, 134, 134, 134, 141, 141, 141, 141
         DB 141, 141, 130, 126, 141, 141, 130, 0, 0, 0, 0, 0, 0, 130, 130, 141, 126, 130, 130, 141, 141, 141, 141, 141, 141, 141, 134, 128, 134, 134, 134, 141, 141, 141, 141, 141, 141, 130, 126, 141
         DB 141, 130, 0, 0, 0, 0, 0, 0, 130, 130, 141, 126, 130, 130, 141, 141, 141, 141, 141, 141, 141, 134, 128, 134, 134, 134, 141, 141, 141, 141, 141, 141, 130, 126, 141, 141, 130, 0, 0, 0
         DB 0, 0, 0, 130, 130, 141, 126, 130, 130, 141, 141, 141, 141, 141, 141, 141, 128, 128, 134, 134, 134, 141, 141, 141, 141, 141, 141, 130, 126, 141, 141, 130, 0, 0, 0, 0, 0, 0, 130, 130
         DB 141, 126, 130, 130, 141, 141, 141, 141, 141, 141, 141, 128, 128, 134, 134, 134, 141, 141, 141, 141, 141, 141, 130, 126, 141, 141, 130, 0, 0, 0, 0, 0, 0, 130, 130, 141, 126, 130, 130, 141
         DB 141, 141, 141, 141, 141, 141, 128, 128, 128, 134, 134, 141, 141, 141, 141, 141, 141, 130, 126, 130, 130, 130, 0, 0, 0, 0, 0, 0, 130, 130, 141, 126, 130, 130, 141, 141, 141, 141, 141, 141
         DB 141, 128, 128, 128, 134, 134, 141, 141, 141, 141, 141, 141, 130, 126, 130, 130, 130, 0, 0, 0, 0, 0, 130, 137, 137, 136, 130, 130, 130, 141, 141, 141, 141, 141, 141, 141, 128, 128, 128, 128
         DB 128, 141, 141, 141, 141, 141, 141, 130, 130, 136, 136, 137, 130, 130, 0, 0, 0, 130, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 128, 128, 128, 128, 128, 141, 141, 141, 141
         DB 141, 141, 141, 141, 141, 141, 141, 130, 130, 0, 0, 0, 130, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 128, 128, 128, 128, 128, 141, 141, 141, 141, 141, 141, 141, 141, 141
         DB 141, 141, 130, 130, 0, 0, 0, 141, 130, 130, 141, 141, 78, 78, 141, 141, 141, 141, 141, 141, 141, 134, 134, 134, 134, 134, 141, 141, 141, 141, 141, 141, 78, 141, 141, 141, 130, 141, 141, 0
         DB 0, 0, 0, 130, 130, 141, 141, 130, 130, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 130, 141, 141, 141, 130, 0, 0, 0, 0, 0, 0, 130, 130
         DB 130, 141, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 141, 130, 130, 130, 0, 0, 0, 0, 0, 0, 130, 130, 130, 141, 130, 130, 130
         DB 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 141, 130, 130, 130, 0, 0, 0, 0, 0, 0, 130, 130, 130, 141, 130, 130, 141, 136, 136, 136, 136, 136
         DB 136, 141, 136, 136, 141, 141, 136, 136, 136, 141, 141, 141, 130, 141, 130, 130, 130, 0, 0, 0, 0, 0, 0, 130, 130, 130, 141, 141, 141, 130, 141, 141, 141, 141, 141, 141, 134, 134, 134, 134
         DB 134, 141, 141, 141, 141, 130, 130, 141, 141, 130, 130, 130, 0, 0, 0, 0, 0, 0, 130, 130, 130, 141, 141, 141, 130, 141, 141, 141, 141, 141, 141, 134, 134, 134, 134, 134, 141, 141, 141, 141
         DB 130, 130, 141, 141, 130, 130, 130, 0, 0, 0, 0, 0, 0, 130, 130, 130, 141, 141, 141, 130, 141, 141, 141, 141, 141, 141, 134, 134, 134, 134, 134, 141, 141, 141, 141, 130, 130, 141, 141, 130
         DB 130, 130, 0, 0, 0, 0, 0, 0, 130, 130, 130, 141, 141, 141, 130, 141, 141, 141, 141, 141, 141, 134, 134, 134, 134, 134, 141, 141, 141, 141, 130, 130, 141, 141, 130, 130, 130, 0, 0, 0
         DB 0, 0, 0, 130, 130, 141, 141, 141, 141, 130, 141, 141, 141, 141, 141, 141, 134, 128, 134, 134, 134, 141, 141, 141, 141, 130, 130, 141, 141, 141, 141, 130, 0, 0, 0, 0, 0, 0, 130, 130
         DB 130, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 134, 128, 128, 134, 134, 141, 141, 141, 141, 141, 141, 141, 141, 130, 130, 130, 0, 0, 0, 0, 0, 0, 0, 0, 130, 78, 141, 141, 130
         DB 141, 141, 141, 141, 141, 141, 134, 134, 134, 134, 134, 141, 141, 141, 141, 130, 130, 141, 78, 130, 130, 0, 0, 0, 0, 0, 0, 0, 0, 0, 130, 78, 141, 141, 130, 141, 141, 141, 141, 141
         DB 141, 134, 134, 134, 134, 134, 141, 141, 141, 141, 130, 130, 141, 78, 130, 130, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 141, 66, 66, 78, 130, 130, 141, 141, 141, 141, 141, 141, 141, 141
         DB 141, 141, 141, 141, 130, 78, 78, 66, 141, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 141, 141, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130
         DB 130, 130, 141, 0, 0, 0, 0, 0, 0, 0
  
  
  palette_data:
    DB    0,   0,   0     ; 0 - Black/Transparent
    DB   11,   7,   0     ; 1
    DB   15,  11,   0     ; 2
    DB   23,  19,   0     ; 3
    DB   19,  15,   0     ; 4
    DB   27,  23,   0     ; 5
    DB   35,  27,   0     ; 6
    DB   31,  23,   0     ; 7
    DB   35,  31,   0     ; 8
    DB   43,  35,   0     ; 9
    DB   39,  31,   0     ; 10
    DB   47,  39,   0     ; 11
    DB   47,  43,   0     ; 12
    DB   51,  43,   0     ; 13
    DB   51,  47,   0     ; 14
    DB   55,  47,   0     ; 15
    DB   55,  51,   0     ; 16
    DB   59,  51,   0     ; 17
    DB   59,  55,   0     ; 18
    DB   63,  55,   0     ; 19
    DB   63,  59,   0     ; 20
    DB   63,  63,   0     ; 21
    DB   63,  59,   7     ; 22
    DB   63,  63,  11     ; 23
    DB   63,  59,  15     ; 24
    DB   63,  63,  19     ; 25
    DB   63,  59,  23     ; 26
    DB   63,  63,  27     ; 27
    DB   63,  59,  31     ; 28
    DB   63,  63,  35     ; 29
    DB   63,  59,  39     ; 30
    DB   63,  63,  43     ; 31
    DB   63,  59,  47     ; 32
    DB   63,  63,  51     ; 33
    DB   63,  59,  55     ; 34
    DB   63,  63,  59     ; 35
    DB   59,  55,  51     ; 36
    DB   59,  51,  43     ; 37
    DB   59,  47,  35     ; 38
    DB   59,  43,  27     ; 39
    DB   55,  43,  23     ; 40
    DB   55,  39,  15     ; 41
    DB   55,  35,   7     ; 42
    DB   51,  35,   0     ; 43
    DB   51,  31,   0     ; 44
    DB   47,  31,   0     ; 45
    DB   47,  27,   0     ; 46
    DB   43,  27,   0     ; 47
    DB   43,  23,   0     ; 48
    DB   39,  23,   0     ; 49
    DB   39,  19,   0     ; 50
    DB   35,  19,   0     ; 51
    DB   35,  15,   0     ; 52
    DB   31,  15,   0     ; 53
    DB   31,  11,   0     ; 54
    DB   27,  11,   0     ; 55
    DB   27,   7,   0     ; 56
    DB   23,   7,   0     ; 57
    DB   23,   3,   0     ; 58
    DB   19,   3,   0     ; 59
    DB   19,   0,   0     ; 60
    DB   15,   0,   0     ; 61
    DB   47,  35,   0     ; 62
    DB   43,  31,   0     ; 63
    DB   43,  35,   0     ; 64
    DB   39,  35,   0     ; 65
    DB   39,  27,   0     ; 66
    DB   35,  23,   0     ; 67
    DB   31,  19,   0     ; 68
    DB   27,  15,   0     ; 69
    DB   47,  39,   0     ; 70
    DB   43,  35,   0     ; 71
    DB   39,  31,   0     ; 72
    DB   35,  27,   0     ; 73
    ; Colors 74-89: Grayscale (16 shades)
    DB    4,   4,   4     ; 74 - Very dark gray
    DB    8,   8,   8     ; 75
    DB   12,  12,  12     ; 76
    DB   16,  16,  16     ; 77
    DB   20,  20,  20     ; 78
    DB   24,  24,  24     ; 79
    DB   28,  28,  28     ; 80
    DB   32,  32,  32     ; 81
    DB   36,  36,  36     ; 82
    DB   40,  40,  40     ; 83
    DB   44,  44,  44     ; 84
    DB   48,  48,  48     ; 85
    DB   52,  52,  52     ; 86
    DB   56,  56,  56     ; 87
    DB   60,  60,  60     ; 88
    DB   63,  63,  63     ; 89 - White
    ; Colors 90-105: Red gradient (16 shades)
    DB    8,   0,   0     ; 90 - Dark red
    DB   12,   0,   0     ; 91
    DB   16,   0,   0     ; 92
    DB   20,   0,   0     ; 93
    DB   24,   0,   0     ; 94
    DB   28,   0,   0     ; 95
    DB   32,   0,   0     ; 96
    DB   36,   0,   0     ; 97
    DB   40,   0,   0     ; 98
    DB   44,   0,   0     ; 99
    DB   48,   0,   0     ; 100
    DB   52,   0,   0     ; 101
    DB   56,   0,   0     ; 102
    DB   60,   0,   0     ; 103
    DB   63,   0,   0     ; 104 - Bright red
    DB   63,   8,   8     ; 105 - Light red
    ; Colors 106-121: Green gradient (16 shades)
    DB    0,   8,   0     ; 106 - Dark green
    DB    0,  12,   0     ; 107
    DB    0,  16,   0     ; 108
    DB    0,  20,   0     ; 109
    DB    0,  24,   0     ; 110
    DB    0,  28,   0     ; 111
    DB    0,  32,   0     ; 112
    DB    0,  36,   0     ; 113
    DB    0,  40,   0     ; 114
    DB    0,  44,   0     ; 115
    DB    0,  48,   0     ; 116
    DB    0,  52,   0     ; 117
    DB    0,  56,   0     ; 118
    DB    0,  60,   0     ; 119
    DB    0,  63,   0     ; 120 - Bright green
    DB    8,  63,   8     ; 121 - Light green
    ; Colors 122-137: Blue gradient (16 shades)
    DB    0,   0,   8     ; 122 - Dark blue
    DB    0,   0,  12     ; 123
    DB    0,   0,  16     ; 124
    DB    0,   0,  20     ; 125
    DB    0,   0,  24     ; 126
    DB    0,   0,  28     ; 127
    DB    0,   0,  32     ; 128
    DB    0,   0,  36     ; 129
    DB    0,   0,  40     ; 130
    DB    0,   0,  44     ; 131
    DB    0,   0,  48     ; 132
    DB    0,   0,  52     ; 133
    DB    0,   0,  56     ; 134
    DB    0,   0,  60     ; 135
    DB    0,   0,  63     ; 136 - Bright blue
    DB    8,   8,  63     ; 137 - Light blue
    ; Colors 138-153: Cyan gradient (16 shades)
    DB    0,   8,   8     ; 138
    DB    0,  12,  12     ; 139
    DB    0,  16,  16     ; 140
    DB    0,  20,  20     ; 141
    DB    0,  24,  24     ; 142
    DB    0,  28,  28     ; 143
    DB    0,  32,  32     ; 144
    DB    0,  36,  36     ; 145
    DB    0,  40,  40     ; 146
    DB    0,  44,  44     ; 147
    DB    0,  48,  48     ; 148
    DB    0,  52,  52     ; 149
    DB    0,  56,  56     ; 150
    DB    0,  60,  60     ; 151
    DB    0,  63,  63     ; 152 - Bright cyan
    DB    8,  63,  63     ; 153
    ; Colors 154-169: Magenta gradient (16 shades)
    DB    8,   0,   8     ; 154
    DB   12,   0,  12     ; 155
    DB   16,   0,  16     ; 156
    DB   20,   0,  20     ; 157
    DB   24,   0,  24     ; 158
    DB   28,   0,  28     ; 159
    DB   32,   0,  32     ; 160
    DB   36,   0,  36     ; 161
    DB   40,   0,  40     ; 162
    DB   44,   0,  44     ; 163
    DB   48,   0,  48     ; 164
    DB   52,   0,  52     ; 165
    DB   56,   0,  56     ; 166
    DB   60,   0,  60     ; 167
    DB   63,   0,  63     ; 168 - Bright magenta
    DB   63,   8,  63     ; 169
    ; Colors 170-185: Orange gradient (16 shades)
    DB   16,   8,   0     ; 170
    DB   20,  10,   0     ; 171
    DB   24,  12,   0     ; 172
    DB   28,  14,   0     ; 173
    DB   32,  16,   0     ; 174
    DB   36,  18,   0     ; 175
    DB   40,  20,   0     ; 176
    DB   44,  22,   0     ; 177
    DB   48,  24,   0     ; 178
    DB   52,  26,   0     ; 179
    DB   56,  28,   0     ; 180
    DB   60,  30,   0     ; 181
    DB   63,  31,   0     ; 182
    DB   63,  35,   0     ; 183
    DB   63,  39,   0     ; 184
    DB   63,  43,   8     ; 185
    ; Colors 186-201: Purple gradient (16 shades)
    DB   12,   0,  16     ; 186
    DB   16,   0,  20     ; 187
    DB   20,   0,  24     ; 188
    DB   24,   0,  28     ; 189
    DB   28,   0,  32     ; 190
    DB   32,   0,  36     ; 191
    DB   36,   0,  40     ; 192
    DB   40,   0,  44     ; 193
    DB   44,   0,  48     ; 194
    DB   48,   0,  52     ; 195
    DB   52,   0,  56     ; 196
    DB   56,   0,  60     ; 197
    DB   60,   0,  63     ; 198
    DB   63,   0,  63     ; 199
    DB   63,   8,  63     ; 200
    DB   63,  16,  63     ; 201
    ; Colors 202-217: Brown gradient (16 shades)
    DB   12,   8,   4     ; 202
    DB   16,  12,   6     ; 203
    DB   20,  14,   7     ; 204
    DB   24,  16,   8     ; 205
    DB   28,  18,   9     ; 206
    DB   32,  20,  10     ; 207
    DB   36,  22,  11     ; 208
    DB   40,  24,  12     ; 209
    DB   44,  26,  13     ; 210
    DB   48,  28,  14     ; 211
    DB   52,  30,  15     ; 212
    DB   56,  32,  16     ; 213
    DB   60,  34,  17     ; 214
    DB   63,  36,  18     ; 215
    DB   63,  40,  20     ; 216
    DB   63,  44,  22     ; 217
    ; Colors 218-233: Pink gradient (16 shades)
    DB   16,   8,  12     ; 218
    DB   20,  10,  15     ; 219
    DB   24,  12,  18     ; 220
    DB   28,  14,  21     ; 221
    DB   32,  16,  24     ; 222
    DB   36,  18,  27     ; 223
    DB   40,  20,  30     ; 224
    DB   44,  22,  33     ; 225
    DB   48,  24,  36     ; 226
    DB   52,  26,  39     ; 227
    DB   56,  28,  42     ; 228
    DB   60,  30,  45     ; 229
    DB   63,  32,  48     ; 230
    DB   63,  36,  51     ; 231
    DB   63,  40,  55     ; 232
    DB   63,  48,  59     ; 233
    ; Colors 234-249: Sky blue gradient (16 shades)
    DB    4,  12,  20     ; 234
    DB    6,  16,  24     ; 235
    DB    8,  20,  28     ; 236
    DB   10,  24,  32     ; 237
    DB   12,  28,  36     ; 238
    DB   14,  32,  40     ; 239
    DB   16,  36,  44     ; 240
    DB   18,  40,  48     ; 241
    DB   20,  44,  52     ; 242
    DB   22,  48,  56     ; 243
    DB   24,  52,  60     ; 244
    DB   26,  56,  63     ; 245
    DB   30,  60,  63     ; 246
    DB   35,  63,  63     ; 247
    DB   40,  63,  63     ; 248
    DB   48,  63,  63     ; 249
    ; Colors 250-255: Lime green gradient (6 shades)
    DB   20,  32,   8     ; 250
    DB   28,  40,   8     ; 251
    DB   36,  48,   8     ; 252
    DB   44,  56,  12     ; 253 ; <--- THIS IS THE NEW GOLD
    DB   52,  63,  16     ; 254 ; <--- THIS IS THE BRIGHT YELLOW
    DB   60,  63,  20     ; 255
        
; --- 30x30 COIN SPRITE ---
coin_sprite:
    DB  0,  0,  0,  0,  0,  0,  0,  0,  0, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0,  0, 74, 74, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 74, 74,  0,  0,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0, 74, 74, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 74, 74,  0,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0, 74, 74, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 74, 74,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0, 74, 74, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 74, 74,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0, 74, 74, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 74, 74,  0,  0,  0,  0
    DB  0,  0,  0,  0, 74, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 74,  0,  0,  0,  0
    DB  0,  0,  0, 74, 254, 254, 254, 254, 254, 254, 254, 254, 74, 74, 74, 74, 74, 74, 254, 254, 254, 254, 254, 254, 254, 254, 74,  0,  0,  0
    DB  0,  0,  0, 74, 254, 254, 254, 254, 254, 254, 254, 74, 253, 253, 253, 253, 253, 253, 74, 254, 254, 254, 254, 254, 254, 254, 74,  0,  0,  0
    DB  0,  0,  0, 74, 254, 254, 254, 254, 254, 254, 74, 253, 253, 253, 253, 253, 253, 253, 253, 74, 254, 254, 254, 254, 254, 254, 74,  0,  0,  0
    DB  0,  0,  0, 74, 254, 254, 254, 254, 254, 254, 74, 253, 253, 253, 253, 253, 253, 253, 253, 74, 254, 254, 254, 254, 254, 254, 74,  0,  0,  0
    DB  0,  0,  0, 74, 254, 254, 254, 254, 254, 254, 74, 253, 253, 253, 253, 253, 253, 253, 253, 74, 254, 254, 254, 254, 254, 254, 74,  0,  0,  0
    DB  0,  0,  0, 74, 254, 254, 254, 254, 254, 254, 74, 253, 253, 253, 253, 253, 253, 253, 253, 74, 254, 254, 254, 254, 254, 254, 74,  0,  0,  0
    DB  0,  0,  0, 74, 254, 254, 254, 254, 254, 254, 74, 253, 253, 253, 253, 253, 253, 253, 253, 74, 254, 254, 254, 254, 254, 254, 74,  0,  0,  0
    DB  0,  0,  0, 74, 254, 254, 254, 254, 254, 254, 74, 253, 253, 253, 253, 253, 253, 253, 253, 74, 254, 254, 254, 254, 254, 254, 74,  0,  0,  0
    DB  0,  0,  0, 74, 254, 254, 254, 254, 254, 254, 74, 253, 253, 253, 253, 253, 253, 253, 253, 74, 254, 254, 254, 254, 254, 254, 74,  0,  0,  0
    DB  0,  0,  0, 74, 254, 254, 254, 254, 254, 254, 254, 74, 253, 253, 253, 253, 253, 253, 74, 254, 254, 254, 254, 254, 254, 254, 74,  0,  0,  0
    DB  0,  0,  0, 74, 254, 254, 254, 254, 254, 254, 254, 254, 74, 74, 74, 74, 74, 74, 254, 254, 254, 254, 254, 254, 254, 254, 74,  0,  0,  0
    DB  0,  0,  0, 74, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 74,  0,  0,  0
    DB  0,  0,  0, 74, 74, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 74, 74,  0,  0,  0
    DB  0,  0,  0,  0, 74, 74, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 74, 74,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0, 74, 74, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 74, 74,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0, 74, 74, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 74, 74,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0, 74, 74, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 74, 74,  0,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0,  0, 74, 74, 254, 254, 254, 254, 254, 254, 254, 254, 254, 254, 74, 74,  0,  0,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
    DB  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0

; --- 27x27 JERRY CAN SPRITE ---
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
