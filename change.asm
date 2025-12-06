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

restart_game:
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
    
    ; 2. Draw Menu Screen
    mov dx, filename_menu
    call draw_320x200_image 
    
    ; 3. Wait for Key Press
    xor ax, ax
    int 16h               

    ; =========================================================
    ; INFO ENTRY PHASE
    ; =========================================================
    
    ; 1. Draw Info Screen
    mov dx, filename_info
    call draw_320x200_image
    
    ; 2. Input Name (Inside Gray Box 1)
    mov cx, 118      
    mov dx, 78       
    call get_user_input
    
    ; 3. Input Roll No (Inside Gray Box 2)
    mov cx, 133      
    mov dx, 124      
    call get_user_input

    ; =========================================================
    ; INSTRUCTIONS PHASE
    ; =========================================================
    
    ; 1. Draw Instructions Screen
    mov dx, filename_inst
    call draw_320x200_image
    
    ; 2. Wait for Key Press to Start Game
    xor ax, ax
    int 16h

    ; =========================================================
    ; GAME START PHASE
    ; =========================================================
    
    ; 1. Load GAME Palette
    call setup_game_palette

    ; 2. Hide Cursor
    mov ah, 01h
    mov ch, 32
    int 10h

    ; 3. Initialize Game Variables
    call init_objects            
    mov word [player_score], 0  
    mov word [fuel_seconds], 100
    
    ; --- DIFFICULTY RESET ---
    mov word [npc_speed], 6       
    mov word [road_speed], 16     
    mov word [difficulty_timer], 0
    mov word [spawn_timer], 0     
    mov word [spawn_counter], 0   
    
    ; Initialize Timer
    push es
    mov ax, 0040h
    mov es, ax
    mov ax, [es:006Ch]            
    mov [last_timer_tick], ax
    pop es

; =============================================================
; GAME LOOP
; =============================================================
game_loop:
    mov ax, [back_buffer_seg]
    mov es, ax

    call draw_optimized_background
    call draw_player_car
    call draw_objects
    call draw_hud
    
    call update_game
    call update_timer_logic
	
	call update_sound_system

    call vsync
    call flip_buffer

    ; --- INPUT ---
    xor bx, bx          
.drain_buffer:
    mov ah, 01h
    int 16h
    jz .process_input
    mov ah, 00h         
    int 16h             
    cmp al, 27          ; ESC
    je pause_game_handler
    mov bl, ah          
    jmp .drain_buffer   

.process_input:
    cmp bl, 0           
    je game_loop        

    cmp bl, 0x4B        ; Left
    je .snap_left
    cmp bl, 0x4D        ; Right
    je .snap_right
    
    push bx             
    mov bx, [player_vel] 
    cmp byte [esp], 0x48 ; Up
    je .move_up
    cmp byte [esp], 0x50 ; Down
    je .move_down
    pop bx              
    jmp game_loop        

.snap_left:
    cmp word [current_lane], 0
    jle .hit_barrier_left       ; Trigger crash if trying to leave lane 0
    dec word [current_lane]
    call update_player_x_from_lane
    jmp game_loop

.hit_barrier_left:
    ; Adjust X slightly to look like we are embedded in the wall
    sub word [player_x], 10     
    jmp trigger_crash_sequence  ; Go to the new sequence!

.snap_right:
    cmp word [current_lane], 2
    jge .hit_barrier_right      ; Trigger crash if trying to leave lane 2
    inc word [current_lane]
    call update_player_x_from_lane
    jmp game_loop

.hit_barrier_right:
    ; Adjust X slightly to look like we are embedded in the wall
    add word [player_x], 10     
    jmp trigger_crash_sequence  ; Go to the new sequence!

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
    cmp ax, 130           
    jg game_loop
    mov [player_y], ax
    jmp game_loop

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
; LOGIC SUBROUTINES
; =============================================================

; =============================================================
; PAUSE MENU LOGIC
; =============================================================
pause_game_handler:
	call speaker_off
    call draw_pause_image_overlay

.pause_input_loop:
    xor ax, ax
    int 16h                 ; Wait for key

    ; Check 'Y' or 'y' (Yes -> Quit)
    cmp al, 'y'
    je .quit_to_menu
    cmp al, 'Y'
    je .quit_to_menu

    ; Check 'N' or 'n' (No -> Resume)
    cmp al, 'n'
    je .resume_game
    cmp al, 'N'
    je .resume_game

    jmp .pause_input_loop   

.quit_to_menu:
    jmp restart_game        

.resume_game:
    call flip_buffer 
    call countdown_3_seconds
    jmp game_loop

; -----------------------------------------------------------
; Draw 230x122 image at X=45, Y=39 directly to Video RAM
; -----------------------------------------------------------
draw_pause_image_overlay:
    pusha
    
    ; Open File
    mov ax, 3D00h
    mov dx, filename_pause
    int 21h
    jnc .open_ok
    popa
    ret                     

.open_ok:
    mov bx, ax              ; File Handle
    
    ; Setup ES to VGA for drawing
    mov ax, 0xA000
    mov es, ax

    ; Calculate Start Offset: (39 * 320) + 45 = 12525
    mov di, 12525
    
    mov cx, 122             ; Height (Rows)

.row_loop:
    push cx
    push di                 ; Save start of current screen row

    ; Read 230 bytes (one row) from file into buffer
    mov ah, 3Fh
    mov cx, 230             ; Width
    mov dx, pause_line_buf  ; Read into data segment buffer
    int 21h

    ; Copy buffer to Video RAM
    pop di                  ; Restore Screen Offset
    push di                 ; Save it again for next line calc
    
    mov si, pause_line_buf
    mov cx, 230
    rep movsb               ; Copy DS:SI (buffer) to ES:DI (screen)

    pop di                  ; Restore Offset
    add di, 320             ; Move Screen Offset down one full line
    pop cx                  ; Restore Height Counter
    loop .row_loop

    ; Close File
    mov ah, 3Eh
    int 21h

    popa
    ret

; -----------------------------------------------------------
; Countdown Logic (3..2..1)
; -----------------------------------------------------------
countdown_3_seconds:
    pusha
    call .draw_and_wait_3
    call flip_buffer        
    call .draw_and_wait_2
    call flip_buffer        
    call .draw_and_wait_1
    call flip_buffer        
    popa
    ret

.draw_and_wait_3:
    mov ax, 3               
    call .draw_count_num
    call .wait_one_sec
    ret

.draw_and_wait_2:
    mov ax, 2
    call .draw_count_num
    call .wait_one_sec
    ret

.draw_and_wait_1:
    mov ax, 1
    call .draw_count_num
    call .wait_one_sec
    ret

.draw_count_num:
    mov cx, 160
    mov dx, 190
    mov bl, 40              ; Color Red
    call draw_number_scaled 
    ret

.wait_one_sec:
    push es
    mov ax, 0040h
    mov es, ax
    mov ax, [es:006Ch]      
    add ax, 18              
    
.wait_loop:
    mov dx, [es:006Ch]
    cmp dx, ax
    jl .wait_loop           
    pop es
    ret

update_timer_logic:
    push es
    pusha
    mov ax, 0040h
    mov es, ax
    mov ax, [es:006Ch]
    mov bx, [last_timer_tick]
    sub ax, bx
    
    call spawn_manager

    cmp ax, 18           
    jl .timer_done
    
    mov ax, [es:006Ch]
    mov [last_timer_tick], ax
    
    inc word [difficulty_timer]
    cmp word [difficulty_timer], 10
    jl .skip_diff
    
    mov word [difficulty_timer], 0  
    cmp word [npc_speed], 12
    jge .skip_diff
    inc word [npc_speed]            
    add word [road_speed], 2        
    
.skip_diff:
    dec word [fuel_seconds]
    cmp word [fuel_seconds], 0
    jle .fuel_empty
    jmp .timer_done

.fuel_empty:
    popa
    pop es
    ; Jump to specific fuel handler
    jmp game_over_fuel   

.timer_done:
    popa
    pop es
    ret

; =============================================================
; OBJECT LOGIC
; =============================================================

init_objects:
    mov cx, MAX_OBJECTS
    mov bx, 0
.init_loop:
    mov word [obj_active + bx], 0
    add bx, 2
    loop .init_loop
    ret

draw_objects:
    pusha
    mov cx, MAX_OBJECTS
    xor bx, bx          
.draw_loop:
    cmp word [obj_active + bx], 1
    jne .next_draw

    push bx 
    mov ax, [obj_x + bx]
    mov [X_START], ax
    mov ax, [obj_y + bx]
    mov [Y_START], ax
    
    mov dx, [obj_type + bx]
    cmp dx, 0
    je .do_car
    cmp dx, 1
    je .do_coin
    jmp .do_jerry

.do_car:
    call DrawCar
    jmp .restore_idx
.do_coin:
    call DrawCoin
    jmp .restore_idx
.do_jerry:
    call DrawJerryCan

.restore_idx:
    pop bx 
.next_draw:
    add bx, 2
    loop .draw_loop
    popa
    ret

update_game:
    pusha
    mov cx, MAX_OBJECTS
    xor bx, bx
.update_loop:
    cmp word [obj_active + bx], 1
    jne .next_update

    mov ax, [npc_speed]
    add [obj_y + bx], ax
    call check_player_collision 

    cmp word [obj_y + bx], 200
    jl .next_update
    mov word [obj_active + bx], 0 

.next_update:
    add bx, 2
    loop .update_loop

    mov ax, [road_speed]      
    add [road_scroll_y], ax
    popa
    ret

check_player_collision:
    mov dx, [obj_type + bx]
    cmp dx, 0
    je .bounds_car
    mov si, 22 
    mov di, 25 
    jmp .check_bounds
.bounds_car:
    mov si, 38 
    mov di, 50 

.check_bounds:
    mov ax, [player_x]
    sub ax, [obj_x + bx]
    cmp ax, 0
    jge .pos_x
    neg ax
.pos_x:
    cmp ax, si
    jg .no_hit

    mov ax, [obj_y + bx]
    add ax, di 
    cmp ax, [player_y] 
    jl .no_hit

    mov ax, [obj_y + bx] 
    mov dx, [player_y]
    add dx, 46 
    cmp ax, dx
    jg .no_hit

    mov word [obj_active + bx], 0 

    mov dx, [obj_type + bx]
    cmp dx, 0
    je .hit_car
    cmp dx, 1
    je .hit_coin
    jmp .hit_jerry

.hit_car:
    pop ax    
    popa      
    ; --- FIXED: JUMP TO THE CORRECT CRASH HANDLER ---
	jmp trigger_crash_sequence
    ;jmp game_over_crash
.hit_coin:
    inc word [player_score]
    
    ; --- COIN SOUND EFFECT (High Pitch "Ding") ---
    mov word [sfx_priority], 900    ; Divisor 900 (~1325 Hz) - High pitch
    mov word [sfx_timer], 3         ; Play for 3 frames (Short & Crisp)
    ret

.hit_jerry:
    add word [fuel_seconds], 20
    cmp word [fuel_seconds], 100
    jle .play_fuel_sound
    mov word [fuel_seconds], 100

.play_fuel_sound:

    mov word [sfx_priority], 1500   
    
    ; Timer 6 = Plays slightly longer than the coin (which is 3).
    ; This gives it more "weight".
    mov word [sfx_timer], 6         
    ret
.ret:
    ret
.no_hit:
    ret

spawn_manager:
    inc word [spawn_timer]
    cmp word [spawn_timer], SPAWN_DELAY
    jl .no_spawn
    mov word [spawn_timer], 0

    mov cx, MAX_OBJECTS
    xor bx, bx
.find_slot:
    cmp word [obj_active + bx], 0
    je .found_slot
    add bx, 2
    loop .find_slot
    jmp .no_spawn 

.found_slot:
    call get_next_spawn_type 
    push ax 

    call get_random_lane_x 
    mov dx, -60 

    call check_spawn_overlap 
    jc .abort_spawn

    mov [obj_x + bx], ax
    mov [obj_y + bx], dx
    pop ax 
    mov [obj_type + bx], ax
    mov word [obj_active + bx], 1
    ret

.abort_spawn:
    pop ax 
.no_spawn:
    ret

get_next_spawn_type:
    inc word [spawn_counter]
    cmp word [spawn_counter], 2
    je .coin
    cmp word [spawn_counter], 5
    je .jerry
    cmp word [spawn_counter], 6
    jge .reset_counter
    mov ax, 0 
    ret
.coin:
    mov ax, 1
    ret
.jerry:
    mov ax, 2
    ret
.reset_counter:
    mov word [spawn_counter], 0
    mov ax, 0 
    ret

get_random_lane_x:
    push dx
    push es
    mov ax, 0040h
    mov es, ax
    mov ax, [es:006Ch]
    xor dx, dx
    mov cx, 3
    div cx
    mov bx, dx
    shl bx, 1
    mov ax, [lane_x_table + bx]
    pop es
    pop dx
    ret

check_spawn_overlap:
    pusha
    mov cx, MAX_OBJECTS
    xor bx, bx
    mov si, ax 
    mov di, dx 
    mov bp, 70 

.overlap_loop:
    cmp word [obj_active + bx], 1
    jne .next_check

    cmp [obj_x + bx], si
    jne .next_check

    mov ax, [obj_y + bx]
    sub ax, di
    cmp ax, 0
    jge .pos_y
    neg ax
.pos_y:
    cmp ax, bp
    jl .overlap_found

.next_check:
    add bx, 2
    loop .overlap_loop

    clc 
    popa
    ret

.overlap_found:
    stc 
    popa
    ret

; =============================================================
; USER INPUT ROUTINE
; =============================================================
get_user_input:
    pusha
    mov di, cx          ; Current X position

.input_loop:
    ; 1. Wait for Key
    xor ax, ax
    int 16h             ; AL = ASCII

    ; 2. Check for ENTER
    cmp al, 0x0D
    je .input_done
    
    ; 3. Check for Space
    cmp al, 0x20
    je .is_space

    ; 4. Draw Logic
    mov bh, al          ; ASCII to BH
    mov cx, di          ; X
    ; DX already has Y from call arguments
    mov al, 44          ; Yellow Color
    
    push ax
    push cx
    push dx
    
    ; Check Digit or Letter
    cmp bh, '0'
    jl .check_alpha_input
    cmp bh, '9'
    jg .check_alpha_input
    
    ; Is Digit
    sub bh, '0'
    mov bl, bh
    xor bh, bh
    mov ax, bx
    mov si, font_data_nums
    mov bx, 5
    mul bx
    add si, ax
    jmp .draw_it

.check_alpha_input:
    ; Is Upper Case?
    cmp bh, 'a'
    jl .is_upper
    sub bh, 32      ; Convert lowercase to uppercase
.is_upper:
    sub bh, 'A'
    cmp bh, 0
    jl .bad_char    ; Not a letter
    cmp bh, 25
    jg .bad_char
    
    mov bl, bh
    xor bh, bh
    mov ax, bx      ; Index 0-25
    mov si, font_data_alpha
    mov bx, 5
    mul bx
    add si, ax
    jmp .draw_it

.is_space:
    add di, 12
    jmp .input_loop

.bad_char:
    pop dx
    pop cx
    pop ax
    jmp .input_loop ; Ignore invalid char

.draw_it:
    pop dx
    pop cx
    pop ax
    
    ; Use SCALED drawer for bigger text in input box
    call draw_bitmap_char_scaled
    
    add di, 12      ; Advance X (8px char + spacing)
    jmp .input_loop

.input_done:
    popa
    ret

; =============================================================
; HUD & TEXT DRAWING
; =============================================================
draw_hud:
    pusha
    
    mov ax, 188
    mov di, ax
    shl ax, 8
    shl di, 6
    add di, ax      
    mov al, 0       
    mov cx, 3840    
    rep stosb

    mov si, txt_coins_str
    mov cx, 5       
    mov dx, 192     
    mov al, 44      
    call draw_text_string

    mov ax, [player_score]
    mov cx, 40
    mov dx, 192
    mov bl, 44      
    call draw_number

    mov si, txt_fuel_str
    mov cx, 220     
    mov dx, 192     
    mov al, 40      
    call draw_text_string

    call draw_fuel_bar
    popa
    ret

draw_fuel_bar:
    pusha
    mov al, 15    
    mov di, 258   
    mov bp, 192   
    mov bx, 54    
    mov dx, 6     
    
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
    
    mov cx, 6
    mov dx, bp
.left_b:
    push cx
    mov cx, di
    call draw_pixel
    inc dx
    pop cx
    loop .left_b

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

    mov ax, [fuel_seconds]
    cmp ax, 0
    jle .done_bar
    
    shr ax, 1     
    mov cx, ax    
    
    mov si, 260   
    mov di, 193   
    
    mov al, 40    
    cmp word [fuel_seconds], 30
    jl .color_pick
    mov al, 44    
    cmp word [fuel_seconds], 60
    jl .color_pick
    mov al, 48    

.color_pick:
    mov bx, 4     
.fill_row:
    push cx
    push si
    mov dx, di    
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

draw_text_string:
.char_loop:
    mov bh, [si]
    cmp bh, 0
    je .done_str
    
    push ax
    push cx
    push dx
    push si           
    
    ; Use Old Helper for HUD (Only needs COINS/FUEL)
    call get_font_offset_hud 
    call draw_bitmap_char
    
    pop si            
    pop dx
    pop cx
    pop ax
    
    add cx, 6         
    inc si            
    jmp .char_loop
.done_str:
    ret

draw_number:
    pusha             
    mov di, cx        
    mov bp, dx        
    xor bh, bh
    mov si, bx        
    mov bx, 10
    xor cx, cx        
.calc_digits:
    xor dx, dx
    div bx            
    push dx           
    inc cx
    test ax, ax
    jz .print_loop
    jmp .calc_digits
.print_loop:
    pop ax            
    push cx           
    push si           
    push ax           
    mov dx, 5
    mul dx            
    mov bx, ax        
    pop ax            
    mov si, font_data_nums
    add si, bx        
    mov cx, di        
    mov dx, bp        
    mov bx, [esp]     
    mov al, bl        
    call draw_bitmap_char
    pop si            
    pop cx            
    add di, 6         
    loop .print_loop
    popa
    ret

draw_number_scaled:
    pusha
    mov di, cx        
    mov bp, dx        
    xor bh, bh
    mov si, bx        
    mov bx, 10
    xor cx, cx        
.calc_s_digits:
    xor dx, dx
    div bx
    push dx
    inc cx
    test ax, ax
    jz .print_s_loop
    jmp .calc_s_digits
.print_s_loop:
    pop ax
    push cx
    push si
    push ax
    mov dx, 5
    mul dx
    mov bx, ax
    pop ax
    mov si, font_data_nums
    add si, bx        
    mov cx, di        
    mov dx, bp        
    mov bx, sp
    mov bx, [bx]      
    mov al, bl
    call draw_bitmap_char_scaled 
    pop si
    pop cx
    add di, 12        
    loop .print_s_loop
    popa
    ret

; OLD HUD FONT HELPER (Specific)
get_font_offset_hud:
    mov si, font_data_letters
    cmp bh, 'C'
    je .is_0
    cmp bh, 'O'
    je .is_1
    cmp bh, 'I'
    je .is_2
    cmp bh, 'N'
    je .is_3
    cmp bh, 'S'
    je .is_4
    cmp bh, 'F'
    je .is_5
    cmp bh, 'U'
    je .is_6
    cmp bh, 'E'
    je .is_7
    cmp bh, 'L'
    je .is_8
    cmp bh, ':'
    je .is_9
    ret 
.is_0: add si, 0  
       ret
.is_1: add si, 5
       ret
.is_2: add si, 10
       ret
.is_3: add si, 15
       ret
.is_4: add si, 20
       ret
.is_5: add si, 25
       ret
.is_6: add si, 30
       ret
.is_7: add si, 35
       ret
.is_8: add si, 40
       ret
.is_9: add si, 45
       ret

draw_bitmap_char:
    push di
    push bx
    push cx
    push dx
    mov bl, 5
.row_l:
    mov bh, [si]    
    push cx         
    test bh, 0x80
    jz .s1
    call draw_pixel_hud
.s1: inc cx
    test bh, 0x40
    jz .s2
    call draw_pixel_hud
.s2: inc cx
    test bh, 0x20
    jz .s3
    call draw_pixel_hud
.s3: inc cx
    test bh, 0x10
    jz .s4
    call draw_pixel_hud
.s4: 
    pop cx          
    inc dx          
    inc si          
    dec bl
    jnz .row_l
    pop dx
    pop cx
    pop bx
    pop di
    ret

draw_bitmap_char_scaled:
    push di
    push bx
    push cx
    push dx
    mov bl, 5        
.rows_scaled:
    mov bh, [si]    
    push cx         
    test bh, 0x80
    jz .skip1
    call draw_pixel_2x
.skip1: add cx, 2
    test bh, 0x40
    jz .skip2
    call draw_pixel_2x
.skip2: add cx, 2
    test bh, 0x20
    jz .skip3
    call draw_pixel_2x
.skip3: add cx, 2
    test bh, 0x10
    jz .skip4
    call draw_pixel_2x
.skip4:
    pop cx          
    add dx, 2        
    inc si          
    dec bl
    jnz .rows_scaled
    pop dx
    pop cx
    pop bx
    pop di
    ret

draw_pixel_2x:
    push cx
    push dx
    call draw_pixel_hud ; 0,0
    inc cx
    call draw_pixel_hud ; 1,0
    pop dx
    push dx
    inc dx              
    dec cx              
    call draw_pixel_hud ; 0,1
    inc cx
    call draw_pixel_hud ; 1,1
    pop dx
    pop cx
    ret

draw_pixel_hud:
    push di
    push dx
    mov di, dx
    shl di, 8
    shl dx, 6
    add di, dx
    add di, cx
    mov [es:di], al
    pop dx
    pop di
    ret

; =============================================================
; NEW: FULL SCREEN 320x200 DRAWER
; =============================================================
draw_320x200_image:
    pusha
    push dx  
    call setup_palette_main
    pop dx   
    mov ax, [back_buffer_seg]
    mov es, ax              
    call load_bin_file
    jc .draw_error          
    push ds
    mov ax, [back_buffer_seg]
    mov ds, ax              
    xor si, si              
    mov ax, 0xA000
    mov es, ax              
    xor di, di
    mov cx, 32000
    rep movsw
    pop ds
    popa
    ret
.draw_error:
    mov ax, 0xA000
    mov es, ax
    xor di, di
    mov al, 40      
    mov cx, 32000
    rep stosb
    popa
    ret

load_bin_file:
    pusha
    push ds
    mov ax, 3D00h
    int 21h
    jc .load_fail
    mov bx, ax           
    push es
    pop ds               
    xor dx, dx           
    mov cx, 64000        
    mov ah, 3Fh
    int 21h
    mov ah, 3Eh
    int 21h
    pop ds
    popa
    clc
    ret
.load_fail:
    pop ds
    popa
    stc
    ret

; =============================================================
; GAME OVER HANDLERS
; =============================================================

; =============================================================
; CRASH SEQUENCE COORDINATOR
; =============================================================
trigger_crash_sequence:
    ; 1. Draw the explosion over the car in the back buffer
    call draw_explosion_effect

    ; 2. FORCE the buffer to screen immediately so player SEES it
    call flip_buffer

    ; 3. Play sound (This loop acts as the DELAY you wanted)
    ;    The screen stays frozen on the explosion while this plays.
    call play_crash_sound

    ; 4. NOW load the final Game Over screen
    jmp game_over_crash
; Entry point 1: Collision
game_over_crash:
    mov dx, filename_crash      ; Load Crash Image
    call draw_320x200_image     ; Draw screen first
    call play_crash_sound       ; Then play the CRASH sound (Blocking)
    jmp common_game_over_logic  ; Jump to score drawing

; Entry point 2: Empty Tank
game_over_fuel:
    mov dx, filename_ranf       ; Load Fuel Image
    call draw_320x200_image     ; Draw screen first
    call play_fuel_empty_sound  ; Play the NEW Fuel Empty sound (Blocking)
    jmp common_game_over_logic

; Shared Logic (Renamed label for clarity)
common_game_over_logic:
    ; Note: Removed draw_320x200_image from here because we did it above
    
    ; 2. Set ES to Video Memory for Score Drawing
    mov ax, 0xA000
    mov es, ax
    
    ; 3. Draw Score
    mov ax, [player_score]
    mov cx, 210             ; X Position
    mov dx, 48              ; Y Position
    mov bl, 44              ; Color Yellow
    call draw_number_scaled

    ; 4. Reset Stack & Input Loop
    mov sp, 0xFFFE          ; Reset stack pointer
.drain_keys:
    mov ah, 01h
    int 16h
    jz .wait_restart_key
    mov ah, 00h
    int 16h
    jmp .drain_keys

.wait_restart_key:
    xor ax, ax
    int 16h                 ; Wait for any key press
    jmp restart_game

update_player_x_from_lane:
    push bx
    mov bx, [current_lane]
    shl bx, 1                 
    mov ax, [reed_lane_x_table + bx] 
    mov [player_x], ax       
    pop bx
    ret

vsync:
    mov dx, 0x3DA
.w1: in al, dx
    test al, 8
    jz .w1
.w2: in al, dx
    test al, 8
    jz .w2
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
    mov al, 120          
    mov cx, 50
    rep stosb
    mov al, 82           
    mov cx, 220
    rep stosb
    mov al, 120          
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
    mov al, 94            
    jmp .draw_sides
.is_white:
    mov al, 153           
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
	


update_sound_system:
    pusha

    ; --- 1. PRIORITY: SFX ---
    cmp word [sfx_timer], 0
    jle .play_music
    
    mov bx, [sfx_priority]
    call speaker_on_freq
    dec word [sfx_timer]
    
    cmp word [sfx_timer], 0
    jg .done
    call speaker_off
    jmp .done

.play_music:
    ; --- 2. MUSIC ENGINE ---
    
    cmp word [note_timer], 0
    jg .sustain_note

    ; --- LOAD NEXT NOTE ---
    mov si, bg_music_data
    add si, [music_cursor]
    
    mov bx, [si]                ; Frequency
    mov cx, [si+2]              ; Duration
    
    cmp bx, 0xFFFF              ; End of song?
    je .reset_song
    
    mov [note_timer], cx
    add word [music_cursor], 4
    
    cmp bx, 0
    je .play_silence
    
    call speaker_on_freq
    jmp .done

.play_silence:
    call speaker_off
    jmp .done

.sustain_note:
    dec word [note_timer]
    
    ; --- STACCATO CUT ---
    ; For very short notes (<= 3 frames), we cut the sound
    ; when the timer hits 0 to maximize playtime.
    ; For longer notes (> 3), we cut at 1 to create separation.
    
    mov ax, [note_timer]
    cmp ax, 1
    jg .done        ; If timer > 1, keep playing
    
    ; If we are here, timer is 1 or 0. Cut the speaker.
    call speaker_off
    jmp .done

.reset_song:
    mov word [music_cursor], 0
    jmp .play_music

.done:
    popa
    ret

; -----------------------------------------------------------
; Play Crash Sound (Blocking - Stops game for effect)
; -----------------------------------------------------------
play_crash_sound:
    pusha
    mov cx, 1000        ; Start Frequency Divisor (High pitch)
.slide_down:
    mov bx, cx
    call speaker_on_freq
    
    ; Delay loop
    push cx
    mov cx, 1000        ; Loop delay
.d: loop .d
    pop cx

    inc cx              ; Increase divisor (Lowers pitch)
    cmp cx, 4000        ; End frequency
    jl .slide_down

    call speaker_off
    popa
    ret


; -----------------------------------------------------------
; Play Engine Failure Sound (Slower slide down)
; -----------------------------------------------------------
play_fuel_empty_sound:
    pusha
    mov cx, 2000        ; Start Frequency (Mid-Low)
.engine_die_loop:
    mov bx, cx
    call speaker_on_freq
    
    ; Long Delay (Slower fade out than crash)
    push cx
    mov cx, 3000        
.d_fuel: loop .d_fuel
    pop cx

    add cx, 20          ; Increment divisor slowly
    cmp cx, 8000        ; End at very low rumble
    jl .engine_die_loop

    call speaker_off
    popa
    ret
; -----------------------------------------------------------
; Speaker Low-Level Helpers
; BX = Frequency Divisor (1193180 / Hz)
; -----------------------------------------------------------
speaker_on_freq:
    push ax
    push dx
    
    ; 1. Set PIT to Mode 3
    mov al, 0B6h
    out 43h, al
    
    ; 2. Send Frequency (BX)
    mov ax, bx
    out 42h, al     ; Low byte
    mov al, ah
    out 42h, al     ; High byte
    
    ; 3. Turn Speaker ON
    in al, 61h
    or al, 3
    out 61h, al
    
    pop dx
    pop ax
    ret

speaker_off:
    push ax
    in al, 61h
    and al, 0FCh    ; Clear bits 0 and 1
    out 61h, al
    pop ax
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
    inc bp              
    inc di
    cmp di, reed_car_length   
    jl .p_row
    popa
    ret

; =============================================================
; VISUAL EFFECTS
; =============================================================
draw_explosion_effect:
    pusha
    
    ; Setup center of explosion based on player position
    mov cx, [player_x]
    add cx, 10          ; Offset X to center
    mov dx, [player_y]
    add dx, 20          ; Offset Y to center

    ; Draw a jagged Red Square (Outer Blast)
    mov si, 40          ; Size
    mov al, 40          ; Red
    ;call .draw_messy_box

    ; Draw a smaller Yellow Square (Inner Core)
    add cx, 5
    add dx, 5
    mov si, 25          ; Size
    mov al, 44          ; Yellow
    ;call .draw_messy_box
    
    popa
    ret

.draw_messy_box:
    ; CX=X, DX=Y, SI=Size, AL=Color
    pusha
    mov di, dx          ; Y Start
    add si, dx          ; Y End (using SI as limit)
    
.exp_row:
    push cx
    mov bx, cx          ; X Start
    add bx, si          ; X End (approx)
    sub bx, dx          ; Adjust width relative to current loop size
    
.exp_col:
    push cx
    push dx
    mov dx, di          ; Current Y
    call draw_pixel     ; Draw pixel at CX, DX
    pop dx
    pop cx
    
    inc cx
    cmp cx, bx
    jl .exp_col
    
    pop cx
    inc di
    cmp di, si
    jl .exp_row
    
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
pause_line_buf  db 230 dup(0)

reed_car_width EQU 41
reed_car_length EQU 55

car_width   EQU 45      
car_height  EQU 59      
coin_width  EQU 20
coin_height EQU 20
jerry_can_width  EQU 27
jerry_can_height EQU 27

; --- LOGIC VARIABLES ---
MAX_OBJECTS EQU 3
obj_x       dw 0, 0, 0
obj_y       dw 0, 0, 0
obj_type    dw 0, 0, 0    
obj_active  dw 0, 0, 0    
spawn_timer dw 0
SPAWN_DELAY EQU 30        

X_START     dw 0
Y_START     dw 0

npc_speed   dw 6
road_speed  dw 16       
road_scroll_y dw 0

player_x     dw 142     
player_y     dw 130
current_lane dw 1        
player_vel   dw 3 

reed_lane_x_table: dw 66, 142, 215
lane_x_table: dw 65, 139, 212 

spawn_counter       dw 0    

; --- HUD VARIABLES ---
player_score    dw 0
fuel_seconds    dw 30
last_timer_tick dw 0
difficulty_timer dw 0
txt_coins_str   db "COINS", 0
txt_fuel_str    db "FUEL", 0

; --- FILES ---
filename_menu   db "Masmain.bin", 0
filename_crash  db "crashco.bin", 0
filename_intro  db "INTRO.BIN", 0
filename_win    db "WIN.BIN", 0
filename_level2 db "LEVEL2.BIN", 0
filename_info   db "Infott.bin", 0
filename_inst   db "Instr.bin", 0
filename_pause  db "Pauses.bin", 0
filename_ranf   db "Ranf.bin", 0

; =============================================================
; NEW SOUND VARIABLES
; =============================================================
sfx_timer       dw 0        ; SFX Override Timer
sfx_priority    dw 0        ; SFX Frequency

; --- MUSIC ENGINE VARS ---
music_cursor    dw 0        ; Current position in music array
note_timer      dw 0        ; How long to keep playing current note

; --- THE TRACK: "Highway Blues" (Freq Divisor, Duration) ---
; 0 Frequency = Silence (Rest)
; =============================================================
; TRACK: "TOP GEAR" (HIGH SPEED / TURBO TEMPO)
; =============================================================
bg_music_data:
    ; --- MAIN THEME PART A ---
    ; E4
    dw 3619, 9      ; Was 18 -> Now 9 (Fast beat)
    dw 3619, 5      ; Was 9  -> Now 5
    dw 3619, 5
    ; G4
    dw 3043, 9
    ; F#4
    dw 3224, 9
    ; E4
    dw 3619, 9
    ; D4
    dw 4063, 9
    ; E4 (Sustain)
    dw 3619, 18     ; Was 36 -> Now 18

    ; --- REPEAT WITH VARIATION ---
    dw 3619, 5
    dw 3619, 5
    ; G4
    dw 3043, 9
    ; F#4
    dw 3224, 9
    ; E4
    dw 3619, 9
    ; B4 (High)
    dw 2415, 9
    ; A4
    dw 2711, 18
    
    ; --- PART B ---
    dw 3043, 9
    dw 3224, 9
    dw 3619, 9
    dw 4063, 9
    ; E4 (Long Sustain before run)
    dw 3619, 30     ; Was 72 -> Now 30
    
    ; --- FAST ARPEGGIO RUN (BLISTERING SPEED) ---
    ; These are set to 3 frames (approx 0.05 seconds per note)
    ; This creates that "blur" effect of the original song.
    
    dw 3619, 3      ; E
    dw 3043, 3      ; G
    dw 2415, 3      ; B
    dw 2280, 3      ; C
    dw 2415, 3      ; B
    dw 3043, 3      ; G
    
    dw 3619, 3      ; E
    dw 3043, 3      ; G
    dw 2415, 3      ; B
    dw 2280, 3      ; C
    dw 2415, 3      ; B
    dw 3043, 3      ; G

    ; --- LOOP ---
    dw 0xFFFF, 0

; --- INPUT BUFFERS ---
input_name_buffer db 10 dup(0)
input_roll_buffer db 10 dup(0)

; --- TINY 4x5 FONT DATA ---
font_data_nums:
    db 0xF0, 0x90, 0x90, 0x90, 0xF0 ; 0
    db 0x40, 0xC0, 0x40, 0x40, 0xE0 ; 1
    db 0xF0, 0x10, 0xF0, 0x80, 0xF0 ; 2
    db 0xF0, 0x10, 0xF0, 0x10, 0xF0 ; 3
    db 0x90, 0x90, 0xF0, 0x10, 0x10 ; 4
    db 0xF0, 0x80, 0xF0, 0x10, 0xF0 ; 5
    db 0xF0, 0x80, 0xF0, 0x90, 0xF0 ; 6
    db 0xF0, 0x10, 0x20, 0x40, 0x40 ; 7
    db 0xF0, 0x90, 0xF0, 0x90, 0xF0 ; 8
    db 0xF0, 0x90, 0xF0, 0x10, 0xF0 ; 9

font_data_letters:
    db 0xF0, 0x80, 0x80, 0x80, 0xF0 ; C
    db 0xF0, 0x90, 0x90, 0x90, 0xF0 ; O
    db 0xE0, 0x40, 0x40, 0x40, 0xE0 ; I
    db 0x90, 0xD0, 0xB0, 0x90, 0x90 ; N
    db 0xF0, 0x80, 0xF0, 0x10, 0xF0 ; S
    db 0xF0, 0x80, 0xE0, 0x80, 0x80 ; F
    db 0x90, 0x90, 0x90, 0x90, 0xF0 ; U
    db 0xF0, 0x80, 0xF0, 0x80, 0xF0 ; E
    db 0x80, 0x80, 0x80, 0x80, 0xF0 ; L
    db 0x00, 0x60, 0x00, 0x60, 0x00 ; :

; NEW FULL ALPHABET (A-Z)
font_data_alpha:
    db 0x60, 0x90, 0xF0, 0x90, 0x90 ; A
    db 0xE0, 0x90, 0xE0, 0x90, 0xE0 ; B
    db 0xF0, 0x80, 0x80, 0x80, 0xF0 ; C
    db 0xE0, 0x90, 0x90, 0x90, 0xE0 ; D
    db 0xF0, 0x80, 0xF0, 0x80, 0xF0 ; E
    db 0xF0, 0x80, 0xE0, 0x80, 0x80 ; F
    db 0xF0, 0x80, 0xB0, 0x90, 0xF0 ; G
    db 0x90, 0x90, 0xF0, 0x90, 0x90 ; H
    db 0x70, 0x20, 0x20, 0x20, 0x70 ; I
    db 0x70, 0x20, 0x20, 0xA0, 0x40 ; J
    db 0x90, 0xA0, 0xC0, 0xA0, 0x90 ; K
    db 0x80, 0x80, 0x80, 0x80, 0xF0 ; L
    db 0x90, 0xF0, 0x90, 0x90, 0x90 ; M
    db 0x90, 0xD0, 0xB0, 0x90, 0x90 ; N
    db 0xF0, 0x90, 0x90, 0x90, 0xF0 ; O
    db 0xF0, 0x90, 0xF0, 0x80, 0x80 ; P
    db 0xF0, 0x90, 0x90, 0xF0, 0x10 ; Q
    db 0xE0, 0x90, 0xE0, 0x90, 0x90 ; R
    db 0xF0, 0x80, 0xF0, 0x10, 0xF0 ; S
    db 0xF0, 0x20, 0x20, 0x20, 0x20 ; T
    db 0x90, 0x90, 0x90, 0x90, 0xF0 ; U
    db 0x90, 0x90, 0x90, 0x90, 0x60 ; V
    db 0x90, 0x90, 0x90, 0xF0, 0x90 ; W
    db 0x90, 0x60, 0x00, 0x60, 0x90 ; X
    db 0x90, 0x90, 0x60, 0x20, 0x20 ; Y
    db 0xF0, 0x10, 0x20, 0x40, 0xF0 ; Z





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