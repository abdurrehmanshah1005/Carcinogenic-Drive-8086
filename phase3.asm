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
    jmp exit_game

.mem_ok:
    mov [back_buffer_seg], ax

    ; --- 2. VIDEO SETUP ---
    mov ah, 0Fh
    int 10h
    push ax             ; Save old mode

    mov ax, 0x0013      ; Mode 13h (320x200)
    int 10h

    ; =========================================================
    ; MAIN MENU PHASE
    ; =========================================================
    
    ; 1. Load MENU Palette
    call setup_palette_main
    
    ; 2. Draw Menu Screen (Direct to Video Memory 0xA000)
    ; Note: We draw direct to screen for the menu to save complexity,
    ; since it's a static image.
    mov ax, 0xA000
    mov es, ax
    call draw_main_menu
    
    ; 3. Wait for ANY Key Press
    xor ax, ax
    int 16h             ; Halts here until key pressed
    
    ; =========================================================
    ; GAME START PHASE
    ; =========================================================
    
    ; 1. Load GAME Palette (Switching from Menu colors to Game colors)
    call setup_game_palette

    ; 2. Hide Cursor
    mov ah, 01h
    mov ch, 32
    int 10h

    ; 3. Initialize Game Variables
    call randomize_npc_x
    mov word [Y_START], -60 

; =============================================================
; GAME LOOP
; =============================================================
game_loop:
    ; Point ES to Back Buffer for all drawing
    mov ax, [back_buffer_seg]
    mov es, ax

    ; --- DRAWING ---
    call draw_optimized_background
    
    ; Draw Player
    call draw_player_car
    
    ; Draw Object (Car, Coin, or Jerry Can)
    cmp word [current_object_type], 1
    je .draw_the_coin
    cmp word [current_object_type], 2
    je .draw_the_jerry
    
    call DrawCar        ; Type 0 = Car
    jmp .draw_done
    
.draw_the_coin:
    call DrawCoin       ; Type 1 = Coin
    jmp .draw_done
    
.draw_the_jerry:
    call DrawJerryCan   ; Type 2 = Jerry Can
    
.draw_done:

    ; --- LOGIC ---
    call update_game

    ; --- RENDER ---
    call vsync
    call flip_buffer

    ; =========================================================
    ; INPUT HANDLING (BUFFER DRAINING FOR NO LAG)
    ; =========================================================
    xor bx, bx          ; Clear BX. BL will store the *last* key pressed.

.drain_buffer:
    mov ah, 01h         ; Peek at the buffer
    int 16h
    jz .process_input   ; If Zero Flag is set, buffer is empty. Go process.
    
    ; If we are here, there is a key waiting. Let's eat it.
    mov ah, 00h         
    int 16h             ; AL = ASCII, AH = Scan Code
    
    cmp al, 27          ; Check for ESC immediately
    je exit_game
    
    mov bl, ah          ; Save this Scan Code into BL
    jmp .drain_buffer   ; Loop back to check if there are MORE keys

.process_input:
    cmp bl, 0           ; Did we find any key during the drain?
    je game_loop        ; If BL is still 0, no keys were pressed. Loop.

    ; --- MOVEMENT LOGIC (Using BL instead of AH) ---
    
    cmp bl, 0x4B        ; Left Arrow
    je .snap_left
    cmp bl, 0x4D        ; Right Arrow
    je .snap_right
    
    push bx             ; Save BX (Move logic uses BX for speed)
    mov bx, [player_vel] 
    
    cmp byte [esp], 0x48 ; Check saved BL (Up Arrow)
    je .move_up
    cmp byte [esp], 0x50 ; Check saved BL (Down Arrow)
    je .move_down
    
    pop bx              ; Restore stack
    jmp game_loop       ; Ignore other keys

.snap_left:
    cmp word [current_lane], 0  
    jle game_loop               
    dec word [current_lane]     
    call update_player_x_from_lane
    jmp game_loop

.snap_right:
    cmp word [current_lane], 2  
    jge game_loop               
    inc word [current_lane]     
    call update_player_x_from_lane
    jmp game_loop

.move_up:
    pop bx              
    mov ax, [player_y]
    sub ax, [player_vel] 
    cmp ax, 0         
    jl game_loop
    mov [player_y], ax
    jmp game_loop

.move_down:
    pop bx              
    mov ax, [player_y]
    add ax, [player_vel] 
    cmp ax, 145         
    jg game_loop
    mov [player_y], ax
    jmp game_loop

; =============================================================
; EXIT
; =============================================================
exit_game:
    pop ax
    mov ah, 00h
    int 10h
    
    mov es, [back_buffer_seg]
    mov ah, 0x49
    int 0x21

    mov ax, 0x4C00
    int 0x21

; =============================================================
; SUBROUTINES
; =============================================================

; --- DRAW MAIN MENU (INTERLEAVED) ---
draw_main_menu:
    pusha
    
    ; 1. Initialize the temporary pointers for the 5 chunks
    mov ax, var0
    mov [curr_ptrs], ax
    mov ax, var1
    mov [curr_ptrs + 2], ax
    mov ax, var2
    mov [curr_ptrs + 4], ax
    mov ax, var3
    mov [curr_ptrs + 6], ax
    mov ax, var4
    mov [curr_ptrs + 8], ax

    ; 2. Setup centering
    ; Screen W (320) - Total W (240) = 80. Offset X = 40.
    ; Screen H (200) - Img H (148) = 52. Offset Y = 26.
    mov bp, 26          ; Start Y
    mov dx, 0           ; Loop Counter: Current Image Row

.row_loop:
    mov di, 40          ; Reset Screen X to 40 for the start of this row
    xor bx, bx          ; Chunk Index (0, 2, 4, 6, 8)

    ; --- Draw the 5 chunks for this single row ---
.chunk_loop:
    mov si, [curr_ptrs + bx] ; Load current address for this chunk
    
    mov cx, 48          ; CHUNK_WIDTH
.pixel_loop:
    mov al, [si]
    ; Main menu is opaque, so no transparency check
    push dx             
    push cx
    mov cx, di          ; Screen X
    mov dx, bp          ; Screen Y
    call draw_pixel     
    pop cx
    pop dx
    
    inc si              ; Move next pixel in data
    inc di              ; Move next pixel on screen
    loop .pixel_loop    

    ; Update pointer for this chunk
    mov [curr_ptrs + bx], si
    
    add bx, 2           ; Next chunk
    cmp bx, 10          ; 5 chunks * 2 bytes
    jl .chunk_loop

    ; --- End of Row ---
    inc bp              ; Move Screen Y down 1
    inc dx              ; Increment Row Counter
    cmp dx, 148         ; IMG_HEIGHT
    jl .row_loop

    popa
    ret

; =============================================================
; UPDATE LOGIC (Now with Collision Detection)
; =============================================================
update_game:
    ; --- 1. MOVE OBJECT DOWN ---
    mov ax, [npc_speed]
    add [Y_START], ax

    ; =========================================================
    ; 2. COLLISION DETECTION
    ; =========================================================
    
    ; Only collect Coins (1) and Jerry Cans (2)
    cmp word [current_object_type], 0
    je .check_reset             ; If it's a car, skip logic

    ; --- A. HORIZONTAL CHECK (With Tolerance) ---
    ; Calculate Distance = ABS(PlayerX - ObjectX)
    mov ax, [player_x]
    sub ax, [X_START]
    
    ; Absolute Value Logic
    cmp ax, 0
    jge .pos_diff
    neg ax              ; Make positive if negative
.pos_diff:
    
    ; Check if distance < 25 pixels (Allows for width differences)
    cmp ax, 25
    jg .check_reset     ; If too far apart, no hit.

    ; --- B. VERTICAL CHECK (Y Intersection) ---
    ; Check 1: Is Object Bottom below Player Top?
    mov ax, [Y_START]
    add ax, 27                  ; Height of item (approx)
    cmp ax, [player_y]
    jl .check_reset             ; Object is too high

    ; Check 2: Is Object Top above Player Bottom?
    mov ax, [Y_START]
    mov bx, [player_y]
    add bx, 55                  ; Player Car Height (reed_car_length)
    cmp ax, bx
    jg .check_reset             ; Object is too low

    ; --- HIT! COLLECT ITEM ---
    ; Move off-screen to vanish immediately
    mov word [Y_START], 201     
    
    ; (Add score code here later)

    ; =========================================================
    ; 3. SPAWNING & RESET
    ; =========================================================
.check_reset:
    cmp word [Y_START], 200
    jl .update_road_lines
    
    inc word [spawn_counter]
    
    cmp word [spawn_counter], 2
    je .spawn_coin
    
    cmp word [spawn_counter], 5
    je .spawn_jerry
    
    cmp word [spawn_counter], 6
    jge .reset_cycle
    
    ; Spawn Car
    mov word [current_object_type], 0 
    jmp .reset_position

.spawn_coin:
    mov word [current_object_type], 1
    jmp .reset_position

.spawn_jerry:
    mov word [current_object_type], 2
    jmp .reset_position

.reset_cycle:
    mov word [spawn_counter], 0
    mov word [current_object_type], 0 

.reset_position:
    mov word [Y_START], -60     
    call randomize_npc_x        

.update_road_lines:
    ; --- 4. SLIPPERY SCROLL (Infinite) ---
    mov ax, [road_speed]    
    add [road_scroll_y], ax     
    ; No reset logic here for smooth infinite scrolling

.done:
    ret
	
update_player_x_from_lane:
    push bx
    mov bx, [current_lane]
    shl bx, 1               
    mov ax, [reed_lane_x_table + bx] 
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
    mov dx, 200         ; Height (200 lines)

.scanline_loop:
    ; 1. Draw Left Grass (0 to 50) -> 50 pixels
    mov al, 120         ; Green
    mov cx, 50
    rep stosb

    ; 2. Draw Road (50 to 270) -> 220 pixels
    mov al, 82          ; Road Gray
    mov cx, 220
    rep stosb

    ; 3. Draw Right Grass (270 to 320) -> 50 pixels
    mov al, 120         ; Green
    mov cx, 50
    rep stosb

    dec dx
    jnz .scanline_loop

    ; Draw details on top
    call draw_fast_barriers
    call draw_lane_dividers
    
    popa
    ret

draw_fast_barriers:
    pusha
    mov dx, 0            ; Y Counter
    mov di, 0            ; Screen Offset
    
.barrier_row:
    ; --- OPTIMIZED MATH ---
    mov ax, dx
    sub ax, [road_scroll_y] ; SUB = Moves Road DOWN (Forward)
    
    ; Check Bit 5 (Value 32) for 32-pixel tall blocks
    test ax, 32          
    jz .is_white
    
    mov al, 94           ; Red
    jmp .draw_sides
    
.is_white:
    mov al, 153          ; White

.draw_sides:
    ; --- DRAW LEFT STRIP (MOVED RIGHT TO X=40) ---
    ; Previously 30-39. Now 40-49 to touch the road at 50.
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

    ; --- DRAW RIGHT STRIP (At X=270) ---
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

    add di, 320          ; Next Line
    inc dx
    cmp dx, 200
    jl .barrier_row
    popa
    ret

draw_lane_dividers:
    pusha
    ; Adjusted for equal lanes
    mov cx, 123
    call draw_thick_dashed_line
    mov cx, 197
    call draw_thick_dashed_line
    popa
    ret

draw_thick_dashed_line:
    pusha
    mov si, cx          ; Base X
    mov di, 0           ; Y Counter
    
.dy:
    mov ax, di
    sub ax, [road_scroll_y] ; SUB = Moves Road DOWN (Forward)
    
    ; Check Bit 5 (32). 
    ; 32 pixels of Paint, 32 pixels of Gap.
    ; Matches the barrier size for a synchronized look.
    test ax, 32
    jnz .skip_pixel     ; If bit is set, it's a gap
    
    ; Draw 5 pixels manually
    mov bx, di
    shl bx, 8           
    mov dx, di
    shl dx, 6           
    add bx, dx          ; BX = Y * 320
    add bx, si          ; Add X
    
    mov al, 89          ; White
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
    cmp cx, reed_car_width   
    jl .p_col
    inc bp              ; Draw UP to DOWN (Corrected)
    inc di
    cmp di, reed_car_length  
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
    inc bp              ; Draw UP to DOWN (Corrected)
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
    add bx, 12          ; Center offset
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
    add bx, 9           ; Center offset
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
    
    ; --- BLACKOUTS ---
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

    ; --- SHARED ---
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
    
    ; --- JERRY CAN ---
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
    
    ; --- CAR & ENV ---
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

reed_car_width EQU 41
reed_car_length EQU 55

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

player_x     dw 142     
player_y     dw 140
current_lane dw 1       
player_vel   dw 3 

reed_lane_x_table: dw 66, 142, 215

lane_x_table: dw 65, 139, 212 

spawn_counter       dw 0    
current_object_type dw 0    

curr_ptrs: dw 0, 0, 0, 0, 0 

; --- SPRITES ---

img_reed_car_large:
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 1, 2, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 2, 1, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 0, 0, 17, 1, 2, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 10, 10, 10, 10, 10, 2, 1, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 18, 2, 2, 2, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 2, 2, 2, 18, 0, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 1, 2, 3, 10, 10, 10, 10, 11, 11, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 11, 11, 10, 10, 10, 10, 3, 2, 1, 0, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 16, 3, 10, 10, 11, 11, 10, 10, 11, 11, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 11, 11, 10, 10, 11, 11, 10, 10, 3, 16, 0, 0, 0, 0
    DB 0, 0, 0, 0, 2, 11, 11, 11, 11, 10, 10, 11, 11, 11, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 11, 11, 11, 10, 10, 11, 11, 11, 11, 2, 0, 0, 0, 0
    DB 0, 0, 0, 1, 10, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 10, 19, 0, 0, 0
    DB 0, 0, 0, 10, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 10, 0, 0, 0
    DB 0, 0, 17, 10, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 10, 17, 0, 0
    DB 0, 0, 1, 10, 11, 11, 11, 11, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 10, 10, 10, 11, 11, 11, 11, 10, 1, 0, 0
    DB 0, 0, 1, 11, 11, 11, 11, 10, 10, 11, 11, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 11, 11, 10, 10, 11, 11, 11, 11, 1, 0, 0
    DB 0, 0, 1, 11, 11, 11, 11, 10, 10, 11, 11, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 11, 11, 10, 10, 11, 11, 11, 11, 1, 0, 0
    DB 0, 0, 1, 11, 11, 11, 11, 10, 11, 11, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 11, 11, 10, 11, 11, 11, 11, 1, 0, 0
    DB 0, 0, 1, 10, 11, 11, 11, 10, 11, 11, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 10, 11, 11, 11, 10, 1, 0, 0
    DB 0, 0, 16, 10, 11, 11, 11, 10, 11, 11, 11, 10, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 10, 11, 11, 11, 10, 11, 11, 11, 10, 16, 0, 0
    DB 0, 0, 0, 2, 11, 11, 10, 10, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 10, 10, 11, 11, 2, 0, 0, 0
    DB 0, 0, 0, 2, 11, 11, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 11, 11, 2, 0, 0, 0
    DB 0, 0, 0, 2, 11, 11, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 11, 11, 2, 0, 0, 0
    DB 0, 0, 0, 2, 11, 11, 11, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 11, 11, 11, 2, 0, 0, 0
    DB 0, 0, 0, 2, 11, 11, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 11, 11, 2, 0, 0, 0
    DB 0, 0, 0, 2, 11, 11, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 11, 11, 2, 0, 0, 0
    DB 0, 16, 18, 10, 11, 10, 11, 11, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 11, 11, 10, 11, 10, 19, 16, 0
    DB 2, 10, 11, 11, 11, 10, 10, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 10, 10, 11, 11, 11, 10, 2
    DB 2, 10, 2, 3, 11, 10, 2, 10, 11, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 11, 10, 2, 10, 11, 3, 2, 10, 2
    DB 0, 0, 0, 1, 11, 10, 2, 10, 11, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 10, 2, 10, 11, 2, 0, 0, 0
    DB 0, 0, 0, 2, 11, 10, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 10, 11, 2, 0, 0, 0
    DB 0, 0, 0, 2, 11, 10, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 10, 11, 1, 0, 0, 0
    DB 0, 0, 0, 2, 11, 10, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 10, 11, 1, 0, 0, 0
    DB 0, 0, 0, 2, 11, 10, 3, 3, 3, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 3, 3, 3, 10, 11, 1, 0, 0, 0
    DB 0, 0, 0, 2, 11, 10, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 10, 11, 1, 0, 0, 0
    DB 0, 0, 0, 1, 11, 10, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 10, 11, 1, 0, 0, 0
    DB 0, 0, 0, 2, 11, 10, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 10, 11, 2, 0, 0, 0
    DB 0, 0, 0, 2, 11, 10, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 10, 11, 2, 0, 0, 0
    DB 0, 0, 0, 2, 11, 10, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 10, 11, 2, 0, 0, 0
    DB 0, 0, 0, 3, 11, 10, 1, 2, 2, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 2, 2, 1, 10, 11, 3, 0, 0, 0
    DB 0, 0, 0, 10, 11, 10, 2, 2, 2, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 2, 2, 2, 10, 11, 10, 0, 0, 0
    DB 0, 0, 16, 10, 11, 10, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 10, 11, 10, 16, 0, 0
    DB 0, 0, 16, 10, 11, 10, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 10, 11, 10, 16, 0, 0
    DB 0, 0, 16, 10, 11, 11, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 11, 11, 10, 16, 0, 0
    DB 0, 0, 17, 10, 11, 11, 10, 3, 2, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 2, 3, 10, 11, 11, 10, 17, 0, 0
    DB 0, 0, 19, 10, 11, 11, 11, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 10, 11, 11, 11, 10, 18, 0, 0
    DB 0, 0, 19, 10, 11, 11, 11, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 10, 19, 0, 0
    DB 0, 0, 19, 10, 11, 11, 11, 10, 10, 10, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 10, 10, 10, 11, 11, 11, 10, 19, 0, 0
    DB 0, 0, 19, 10, 11, 11, 11, 10, 10, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 10, 10, 11, 11, 11, 10, 19, 0, 0
    DB 0, 0, 19, 10, 11, 11, 11, 10, 10, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 10, 10, 11, 11, 11, 10, 18, 0, 0
    DB 0, 0, 16, 10, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 10, 16, 0, 0
    DB 0, 0, 16, 10, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 10, 16, 0, 0
    DB 0, 0, 0, 2, 11, 11, 11, 11, 11, 11, 11, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 11, 11, 11, 11, 11, 11, 11, 2, 0, 0, 0
    DB 0, 0, 0, 19, 10, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 10, 19, 0, 0, 0
    DB 0, 0, 0, 16, 10, 11, 11, 11, 11, 11, 11, 11, 11, 10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 10, 11, 11, 11, 11, 11, 11, 11, 11, 10, 16, 0, 0, 0
    DB 0, 0, 0, 0, 2, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 2, 0, 0, 0, 0
    DB 0, 0, 0, 0, 18, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 18, 0, 0, 0, 0
    DB 0, 0, 0, 0, 16, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 16, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 19, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 10, 10, 10, 19, 0, 0, 0, 0, 0
    DB 0, 0, 0, 0, 0, 0, 0, 16, 19, 1, 2, 3, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 3, 2, 1, 19, 16, 0, 0, 0, 0, 0, 0, 0


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


; MAIN MENU IMAGE DATA (48x148 pixels per chunk)
; =============================================================

var0:
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

var1:
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

var2:
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

var3:
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

var4:
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