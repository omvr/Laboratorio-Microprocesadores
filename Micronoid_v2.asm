;##############################################
;Ejemplo#5 - Captura inmediata de teclas
;EL4313 - Laboratorio de Estructura de Microprocesadores
;2S2016-LCRA
;##############################################
;Este programa contiene una serie de funciones y utilidades para
;apagar/encender los diferentes modos de operacion del teclado
;en Linux.
;Cada función se explica en detalle abajo
;
;NOTA IMPORTANTE: Al final del archivo se muestra el uso basico de las funciones
;
;##############################################

;--------------------Segmento de datos--------------------

section .data
	cons_banner: db 'Modo Canonico Apagado - Presione una tecla, y luego Enter:  '		; Banner para el usuario
	cons_tamano_banner: equ $-cons_banner					; Longitud del banner

	cons_salida: db 'Usted presiono la tecla:  '					; Banner para el usuario
	cons_tamano_salida: equ $-cons_salida					; Longitud del banner

	cons_final: db 0xa,'Fin del programa. ',0xa					; Banner para el usuario
	cons_tamano_final: equ $-cons_final						; Longitud del banner

	variable: db '0000'				;Almacenamiento de la tecla capturada

	termios:  times 36 db 0		        ;Estructura de 36bytes que contiene el modo de operacion de la consola
	stdin:    equ 0			;Standard Input (se usa stdin en lugar de escribir manualmente los valores)
	ICANON:   equ 1<<1				;ICANON: Valor de control para encender/apagar el modo canonico
	ECHO:     equ 1<<3		;ECHO: Valor de control para encender/apagar el modo de eco 

        Vtime: equ 4
        Vmin: equ 5 
        CC_C: equ 18


        ;--------------------------------------------------------------------------------------------
        wall_: db '====',0xa
                wall_len: equ $-wall_

        ball_: db '@',0xa
                ball_len: equ $-ball_

        block_: db '[][][][]',0xa
                block_len: equ $-block_

        plat_: db '====',0xa
                plat_len: equ $-plat_

        roof_: db '===',0xa
                roof_len: equ $-roof_

        floor_: db '^',0xa
                floor_len: equ $-floor_

        space_: db ' ',0xa
                space_len: equ $-space_

        spaceplat_: db '    ',0xa
                spaceplat_len: equ $-space_

        spaceblock_: db '        ',0xa
                spaceblock_len: equ $-spaceblock_

        escClear db 27,"[2J"    ;eSC[2J
                escClearLen: equ 4      

        escCursor db 27,"[00;00H"        
                escCursorLen: equ 8

        botRight_ db "c"                                ;boton derecho
                botRight_len: equ $-botRight_     

        botLeft_ db "z"                                 ;boton izquierdo
                botLeft_len: equ $-botLeft_

        UpDown_: db 1,0xa                               ;constante utilizada para la direccion vertical de la pelota
                UpDown_len: equ $-UpDown_

        RightLeft_: db 1,0xa                            ;constante utilizada para la direccion horizontal de la pelota
                RightLeft_len: equ $-RightLeft_

        ballposition_: db 27,"[19;17H",0xa              ;almacena la direccion de la pelota
                ballposition_len: equ $-ballposition_

        platposition_: db 27,"[22;16H",0xa              ;almacena la direccion de la plataforma
                platposition_len: equ $-platposition_

        blockposition_: db 27,"[00;00H",0xa              ;almacena la direccion de la plataforma
                blockposition_len: equ $-blockposition_

        block1_: db 1
        block2_: db 1
        block3_: db 1
        block4_: db 1
        block5_: db 1 
        block6_: db 1
        block7_: db 1
        block8_: db 1
        block9_: db 1


;--------------------Declaracion de funciones y utilidades ---------------------------------------------

;####################################################
;canonical_off
;Esta es una funcion que sirve para apagar el modo canonico en Linux
;Cuando el modo canonico se apaga, Linux NO espera un ENTER para
;procesar lo que se captura desde el teclado, sino que se hace de forma
;directa e inmediata
;
;Para apagar el modo canonico, simplemente use: call canonical_off
;###################################################
canonical_off:

	;Se llama a la funcion que lee el estado actual del TERMIOS en STDIN
	;TERMIOS son los parametros de configuracion que usa Linux para STDIN
        call read_stdin_termios

	;Se escribe el nuevo valor de ICANON en EAX, para apagar el modo canonico
        push rax
        mov eax, ICANON
        not eax
        and [termios+12], eax
        mov byte [termios+CC_C+Vmin],0
        mov byte [termios+CC_C+Vtime],1
        pop rax

	;Se escribe la nueva configuracion de TERMIOS
        call write_stdin_termios
        ret
        ;Final de la funcion
;###################################################


;####################################################
;echo_off
;Esta es una funcion que sirve para apagar el modo echo en Linux
;Cuando el modo echo se apaga, Linux NO muestra en la pantalla la tecla que
;se acaba de presionar.
;
;Para apagar el modo echo, simplemente use: call echo_off
;###################################################
echo_off:

	;Se llama a la funcion que lee el estado actual del TERMIOS en STDIN
	;TERMIOS son los parametros de configuracion que usa Linux para STDIN
        call read_stdin_termios

        ;Se escribe el nuevo valor de ECHO en EAX para apagar el echo
        push rax
        mov eax, ECHO
        not eax
        and [termios+12], eax
        pop rax

	;Se escribe la nueva configuracion de TERMIOS
        call write_stdin_termios
        ret
        ;Final de la funcion
;###################################################


;####################################################
;canonical_on
;Esta es una funcion que sirve para encender el modo canonico en Linux
;Cuando el modo canonico se enciende, Linux espera un ENTER para
;procesar lo que se captura desde el teclado
;
;Para encender el modo canonico, simplemente use: call canonical_on
;###################################################
canonical_on:

	;Se llama a la funcion que lee el estado actual del TERMIOS en STDIN
	;TERMIOS son los parametros de configuracion que usa Linux para STDIN
        call read_stdin_termios

        ;Se escribe el nuevo valor de modo Canonico
        or dword [termios+12], ICANON

	;Se escribe la nueva configuracion de TERMIOS
        call write_stdin_termios
        ret
        ;Final de la funcion
;###################################################


;####################################################
;echo_on
;Esta es una funcion que sirve para encender el echo en Linux
;Cuando el echo se enciende, Linux muestra en la pantalla (stdout) cada tecla
;que se recibe del teclado (stdin)
;
;Para encender el modo echo, simplemente use: call echo_on
;###################################################
echo_on:

	;Se llama a la funcion que lee el estado actual del TERMIOS en STDIN
	;TERMIOS son los parametros de configuracion que usa Linux para STDIN
        call read_stdin_termios

        ;Se escribe el nuevo valor de modo echo
        or dword [termios+12], ECHO

	;Se escribe la nueva configuracion de TERMIOS
        call write_stdin_termios
        ret
        ;Final de la funcion
;###################################################



;####################################################
;read_stdin_termios
;Esta es una funcion que sirve para leer la configuracion actual del stdin o 
;teclado directamente de Linux
;Esta configuracion se conoce como TERMIOS (Terminal Input/Output Settings)
;Los valores del stdin se cargan con EAX=36h y llamada a la interrupcion 80h
;
;Para utilizarlo, simplemente se usa: call read_stdin_termios
;###################################################
read_stdin_termios:
        push rax
        push rbx
        push rcx
        push rdx

        mov eax, 36h
        mov ebx, stdin
        mov ecx, 5401h
        mov edx, termios
        int 80h

        pop rdx
        pop rcx
        pop rbx
        pop rax
        ret
        ;Final de la funcion
;###################################################


;####################################################
;write_stdin_termios
;Esta es una funcion que sirve para escribir la configuracion actual del stdin o 
;teclado directamente de Linux
;Esta configuracion se conoce como TERMIOS (Terminal Input/Output Settings)
;Los valores del stdin se cargan con EAX=36h y llamada a la interrupcion 80h
;
;Para utilizarlo, simplemente se usa: call write_stdin_termios
;###################################################
write_stdin_termios:
        push rax
        push rbx
        push rcx
        push rdx

        mov eax, 36h
        mov ebx, stdin
        mov ecx, 5402h
        mov edx, termios
        int 80h

        pop rdx
        pop rcx
        pop rbx
        pop rax
        ret
        ;Final de la funcion
;###################################################
;                 VTIME & VMIN


;####################################################

;---------------------------------------------------------------------------------------------------
section .text
        global _start           ;Definicion de la etiqueta inicia
        global _ret
        global _freesourse
        global _stop1
        global _return


_Cero:                                    ;Coloca el cursor en "[00;00H"
        mov word [escCursor+2],'00'      
        mov word [escCursor+5],'00'      
        ret
;############################# Limpia la pantalla ####################################
_clearscreen:
        mov rax,4
        mov rbx,1
        mov rcx,escClear
        mov rdx,escClearLen
        
        int 80h
        ret

;############################# MANEJO DEL CURSOR ###################################
_movcursor:                                          ;coloca el cursor en la posicion escCursor (esc[YY;XXH)
        mov rax,4
        mov rbx,1
        mov rcx,escCursor
        mov rdx,escCursorLen
        int 80h
        ret

;### incrementa la posicion vertical del cursor
_incposY:                                         ;le incrementa 1 a esc[+1;XXH
        cmp byte [escCursor+3],'9'
        je _incDecY
        _incUniY:
                inc byte [escCursor+3]                                          ;esc[+1;XXH) solo unidades
                ret
        _incDecY:                                                               ;esc[+1;XXH) solo decenas
                mov byte [escCursor+3],'0'      ;Pone un cero en las unidades
                inc byte [escCursor+2]          ;incrementa un +1 en la decenas
                ret

;### incrementa la posicion horizontal del cursor
_incposX:                                  ;le incrementa 1 a esc[YY;+1H)
        cmp byte [escCursor+6],'9'
        je _incDecX
        _incUniX:
                inc byte [escCursor+6]    ;Coloca el cursor en esc[07;07H [y,x]
                ret
        _incDecX:
                mov byte [escCursor+6],'0'     ;Pone un cero en las unidades
                inc byte [escCursor+5]         ;incrementa un +1 en la decenas
                ret

;### decrementa la posicion vertical del cursor
_decposY:                                                                                                                                                               ;le decrementa 1 a esc[-1;XXH)
        cmp byte [escCursor+3],'0'
        je _decDecY

        _decUniY:
                dec byte [escCursor+3]    ;Coloca el cursor en esc[07;07H [y,x]
                ret
        _decDecY:
                mov byte [escCursor+3],'9'      ;Coloca el cursor en "[00;07H"
                dec byte [escCursor+2]    ;Coloca el cursor en esc[07;07H [y,x]
                ret

;### decrementa la posicion horizontal del cursor
_decposX:                                                                                                                                                               ;le Decrementa 1 a esc[YY;-1H)
        cmp byte [escCursor+6],'0'
        je _decDecX

        _decUniX:
                dec byte [escCursor+6]    ;Coloca el cursor en esc[07;07H [y,x]
                ret
        _decDecX:
                mov byte [escCursor+6],'9'      ;Coloca el cursor en "[00;07H"
                dec byte [escCursor+5]    ;Coloca el cursor en esc[07;07H [y,x]
                ret

;############################# FIN MANEJO DEL CURSOR ###################################

;################################Dibujo de algunos caracteres #############################

_delete:     
        ;implimir la primera linea
        mov rax,1                               ;rax = syst_write (1)
        mov rdi,1                               ;rdi = 1
        mov rsi,space_                             ;rsi = linea_uno
        mov rdx,space_len                              ;rdx = tamano de linea_uno

        syscall
        ret

_deleteplat:     
        ;implimir la primera linea
        mov rax,1                               ;rax = syst_write (1)
        mov rdi,1                               ;rdi = 1
        mov rsi,spaceplat_                             ;rsi = linea_uno
        mov rdx,spaceplat_len                              ;rdx = tamano de linea_uno

        syscall
        ret


_deleteblock:                                           ;implimir la primera linea
        mov rax,1                               ;rax = syst_write (1)
        mov rdi,1                               ;rdi = 1
        mov rsi,spaceblock_                             ;rsi = linea_uno
        mov rdx,spaceblock_len                              ;rdx = tamano de linea_uno

        ;int 80h
        syscall
        ret

_printplat:     
        ;implimir la primera linea
        mov rax,1                               ;rax = syst_write (1)
        mov rdi,1                               ;rdi = 1
        mov rsi,plat_                   ;rsi = linea_uno
        mov rdx,plat_len                ;rdx = tamano de linea_uno

        syscall
        ret


_printball:     
        ;implimir la primera linea
        mov rax,1                               ;rax = syst_write (1)
        mov rdi,1                               ;rdi = 1
        mov rsi,ball_                   ;rsi = linea_uno
        mov rdx,ball_len                ;rdx = tamano de linea_uno

        syscall
        ret

_printblock:     
        ;implimir la primera linea
        mov rax,1                               ;rax = syst_write (1)
        mov rdi,1                               ;rdi = 1
        mov rsi,block_                   ;rsi = linea_uno
        mov rdx,block_len                ;rdx = tamano de linea_uno

        syscall
        ret

_printwall:     
        ;implimir la primera linea
        mov rax,1                               ;rax = syst_write (1)
        mov rdi,1                               ;rdi = 1
        mov rsi,wall_                   ;rsi = linea_uno
        mov rdx,wall_len                ;rdx = tamano de linea_uno

        syscall
        ret

_printroof:     
        ;implimir la primera linea
        mov rax,1                               ;rax = syst_write (1)
        mov rdi,1                               ;rdi = 1
        mov rsi,roof_                   ;rsi = linea_uno
        mov rdx,roof_len                ;rdx = tamano de linea_uno

        syscall                 ;llamado al sistema
        ret

_printfloor:    
        ;implimir la primera linea
        mov rax,1                               ;rax = syst_write (1)
        mov rdi,1                               ;rdi = 1
        mov rsi,floor_                  ;rsi = linea_uno
        mov rdx,floor_len               ;rdx = tamano de linea_uno

        syscall                 ;llamado al sistema
        ret

;#################################### Dibuja interface ##############################################
_firstwall:
        mov word [escCursor+2],'02'            
        mov word [escCursor+5],'00'         ;Coloca el cursor en "[02;10H"

        mov rcx,22
        _loopWall1:
                push rcx

                call _incposY

                call _movcursor
                call _printwall

                pop rcx
        loop _loopWall1
        ret

_secondwall:
        mov word [escCursor+2],'02'      
        mov word [escCursor+5],'31'      ;Coloca el cursor en "[02;02H"

        mov rcx,22
        _loopWall2:
                push rcx

                call _incposY
                call _movcursor
                call _printwall

                pop rcx
        loop _loopWall2
        ret

_floor:
        mov word [escCursor+2],'24'            
        mov word [escCursor+5],'04'        ;Coloca el cursor en "[33;00H"

        mov rcx,26
        _loopfloor:
                push rcx
                call _incposX

                call _movcursor
                call _printfloor

                pop rcx
        loop _loopfloor
        ret

_roof:
        mov word [escCursor+2],'03'       
        mov word [escCursor+5],'04'      ;Coloca el cursor en "[02;00H"

        mov rcx,26
        _looproof:
                push rcx
                call _incposX

                call _movcursor
                call _printroof

                pop rcx
        loop _looproof
        ret

;#################################### Predice direccion de movimiento de la pelota ############################
_predmovUD:
        cmp byte [UpDown_],0
        je _movDown

        _movUp:
                mov byte [UpDown_],1
                call _decposY
                jmp _validmovUD

        _movDown:
                mov byte [UpDown_],0 
                call _incposY
                jmp _validmovUD


_validmovUD:
        _validup:
                cmp word [escCursor+2],'03'
                        je _movDown
                call _Ball_Plat
                call _Ball_block
        _validdown:
                cmp word [escCursor+2],'24'
                        je _movUp                    ;pierde una vida
                call _Ball_Plat                      ;compara la nextpos del la pelota con la pisicion del plato
                call _Ball_block
        ret


_Ball_Plat:
        mov r10,qword [escCursor]         ;guarda la nextpos de la pelota r10
        mov r11,qword [platposition_]     ;guarda la pos actual del plato en r11
        mov [escCursor],r11               ;le da a esCursor la pos actual del plato

        mov rcx,4
        _cmpposplat:
                cmp r10,qword [escCursor]         ;compara la posicion 1 del plato con la nextpos de la pelota
                        je _platbounce            ;si es igual, la pelota debe moverse hacia arriba
                call _incposX
        loop _cmpposplat
        mov qword [escCursor],r10           ;escCUsor recopera la nextpos de la pelota
        ret

_platbounce:
        mov qword [escCursor],r10           ;escCUsor recopera la nextpos de la pelota
        cmp byte [UpDown_],0                ;cambia el bit de direccion up-down
                je _movUp
        jmp _movDown


_movbyte:
        cmp byte [UpDown_],0
        je _posbyte
                mov byte [UpDown_],0
                ret
        _posbyte:
                mov byte [UpDown_],1
                ret

_predmovLR:
        cmp byte [RightLeft_],0
        je _revmovRight

        _revmovLeft:
                mov byte [RightLeft_],1
                call _decposX
                jmp _validmovLR

        _revmovRight:
                mov byte [RightLeft_],0
                call _incposX
                jmp _validmovLR
        

_validmovLR:
        _validleft:
                cmp word [escCursor+5],'05'
                        je _revmovRight
        _validright:
                cmp word [escCursor+5],'30'
                        je _revmovLeft
        ret


_Ball_block:
        mov r10,qword [escCursor]         ;guarda la nextpos de la pelota r10
        ;mov r11,qword [platposition_]     ;guarda la pos actual del plato en r11
        ;mov [escCursor],r11               ;le da a esCursor la pos actual de la pelota
        
        _block_ball1:
                cmp byte [block1_],0
                        je _block_ball2
                        mov word [escCursor+2],'04'       
                        mov word [escCursor+5],'05'
                        mov r11, block1_
                        call _revblock_ball
        _block_ball2:
                cmp byte [block2_],0
                        je _block_ball3
                        mov word [escCursor+2],'04'       
                        mov word [escCursor+5],'14'
                        mov r11, block2_
                        call _revblock_ball
        _block_ball3:
                cmp byte [block3_],0
                        je _block_ball4
                        mov word [escCursor+2],'04'       
                        mov word [escCursor+5],'23'
                        mov r11, block3_
                        call _revblock_ball
        _block_ball4:
                cmp byte [block4_],0
                        je _block_ball5
                        mov word [escCursor+2],'05'       
                        mov word [escCursor+5],'05'
                        mov r11, block4_
                        call _revblock_ball
        _block_ball5:
                cmp byte [block5_],0
                        je _block_ball6
                        mov word [escCursor+2],'05'       
                        mov word [escCursor+5],'14'
                        mov r11, block5_
                        call _revblock_ball
        _block_ball6:
                cmp byte [block6_],0
                        je _block_ball7
                        mov word [escCursor+2],'05'       
                        mov word [escCursor+5],'23'
                        mov r11, block6_
                        call _revblock_ball

        _block_ball7:
                cmp byte [block7_],0
                        je _block_ball8
                        mov word [escCursor+2],'06'       
                        mov word [escCursor+5],'05'
                        mov r11, block7_
                        call _revblock_ball
        _block_ball8:
                cmp byte [block8_],0
                        je _block_ball9
                        mov word [escCursor+2],'06'       
                        mov word [escCursor+5],'14'
                        mov r11, block8_
                        call _revblock_ball
        _block_ball9:
                cmp byte [block9_],0
                        je _ret
                        mov word [escCursor+2],'06'       
                        mov word [escCursor+5],'23'
                        mov r11, block9_
                        call _revblock_ball
        mov qword [escCursor],r10                               ;escCUsor recopera la nextpos de la pelota
        ret

_revblock_ball:
        mov rcx,8
        _cmpposblock:
                cmp r10,qword [escCursor]         ;compara la posicion 1 del plato con la nextpos de la pelota
                        je _blockbounce            ;si es igual, la pelota debe moverse hacia arriba
                call _incposX
        loop _cmpposblock
        mov qword [escCursor],r10           ;escCUsor recopera la nextpos de la pelota
        ret

_blockbounce:
        mov qword [escCursor],r10           ;escCUsor recopera la nextpos de la pelota
        mov byte [r11],0                    ;ELIMINA EL BLOQUE
        cmp byte [UpDown_],0                ;cambia el bit de direccion up-down
                je _movUp
        jmp _movDown

                        

;##################################################################################
_movBall:
        mov r8,[ballposition_]      
        mov [escCursor],r8

        call _movcursor
        call _delete   
                   
        call _predmovUD
        call _predmovLR
        call _movcursor
        call _printball
                                
        mov r8,[escCursor]      
        mov [ballposition_],r8
        ret

_movPlat:
        mov r8,[platposition_]      
        mov [escCursor],r8

        call _movcursor
        call _deleteplat
                   
        call _predmovplatLR
        call _movcursor
        call _printplat
                                
        mov r8,[escCursor]      
        mov [platposition_],r8
        ret
        
_blocks:
        _block1:
                ;cmp byte [block1_],0
                 ;       jz _block2
                        mov word [escCursor+2],'04'       
                        mov word [escCursor+5],'05'
                        call _movcursor
                        call _printblock
        _block2:
                ;cmp byte [block2_],0
                 ;       jz _block3
                        mov word [escCursor+2],'04'       
                        mov word [escCursor+5],'14'
                        call _movcursor
                        call _printblock
        _block3:
                ;cmp byte [block3_],0
                 ;       jz _block4
                        mov word [escCursor+2],'04'       
                        mov word [escCursor+5],'23'
                        call _movcursor
                        call _printblock
        _block4:
                ;cmp byte [block4_],0
                 ;       jz _block5
                        mov word [escCursor+2],'05'       
                        mov word [escCursor+5],'05'
                        call _movcursor
                        call _printblock
        _block5:
                ;cmp byte [block5_],0
                 ;       jz _block6
                        mov word [escCursor+2],'05'       
                        mov word [escCursor+5],'14'
                        call _movcursor
                        call _printblock

        _block6:
                ;cmp byte [block6_],0
                 ;       jz _block7
                        mov word [escCursor+2],'05'       
                        mov word [escCursor+5],'23'
                        call _movcursor
                        call _printblock

        _block7:
                ;cmp byte [block7_],0
                 ;       jz _block8
                        mov word [escCursor+2],'06'       
                        mov word [escCursor+5],'05'
                        call _movcursor
                        call _printblock
        _block8:
                ;cmp byte [block8_],0
                 ;       jz _block9
                        mov word [escCursor+2],'06'       
                        mov word [escCursor+5],'14'
                        call _movcursor
                        call _printblock
        _block9:
                ;cmp byte [block9_],0
                 ;       jz _ret
                        mov word [escCursor+2],'06'       
                        mov word [escCursor+5],'23'
                        call _movcursor
                        call _printblock
        ret



;###################################################################################
;###################################################################################
_nblocks:
        push qword [escCursor]
        _nblock1:
                cmp byte [block1_],1
                        je _nblock2
                        mov word [escCursor+2],'04'       
                        mov word [escCursor+5],'05'
                        call _movcursor
                        call _deleteblock
        _nblock2:
                cmp byte [block2_],1
                        je _nblock3
                        mov word [escCursor+2],'04'       
                        mov word [escCursor+5],'14'
                        call _movcursor
                        call _deleteblock
        _nblock3:
                cmp byte [block3_],1
                        je _nblock4
                        mov word [escCursor+2],'04'       
                        mov word [escCursor+5],'23'
                        call _movcursor
                        call _deleteblock
        _nblock4:
                cmp byte [block4_],1
                        je _nblock5
                        mov word [escCursor+2],'05'       
                        mov word [escCursor+5],'05'
                        call _movcursor
                        call _deleteblock
        _nblock5:
                cmp byte [block5_],1
                        je _nblock6
                        mov word [escCursor+2],'05'       
                        mov word [escCursor+5],'14'
                        call _movcursor
                        call _deleteblock

        _nblock6:
                cmp byte [block6_],1
                        je _nblock7
                        mov word [escCursor+2],'05'       
                        mov word [escCursor+5],'23'
                        call _movcursor
                        call _deleteblock

        _nblock7:
                cmp byte [block7_],1
                        je _nblock8
                        _stop1:
                        mov word [escCursor+2],'06'       
                        mov word [escCursor+5],'05'
                        call _movcursor
                        call _deleteblock
        _nblock8:
                cmp byte [block8_],1
                        je _nblock9
                        mov word [escCursor+2],'06'       
                        mov word [escCursor+5],'14'
                        call _movcursor
                        call _deleteblock
        _nblock9:
                cmp byte [block9_],1
                        je _return
                        mov word [escCursor+2],'06'       
                        mov word [escCursor+5],'23'
                        call _movcursor
                        call _deleteblock
        _return:
                pop qword [escCursor]
                ret
;###################################################################################
;###################################################################################



;--------------------Declaracion de codigo para pruebas--------------------------------------------



;###################################################
; AQUI EMPIEZA EL CODIGO DE PRUEBAS
;En este codigo de pruebas, solamente se va a apagar el modo canonico
;para demostrar la captura inmediata de las teclas, sin esperar el ENTER
;###################################################



_start:

        call canonical_off
        call echo_off
        
        call _clearscreen
        call _firstwall
        call _secondwall
        call _roof
        call _floor
        call _blocks

        _startgame:
                mov rcx,1000
                _looball:
                        push rcx

                                _paso2:
                                       mov rax,0                                                       ;rax = "sys_read"
                                        mov rdi,0                                                       ;rdi = 0 (standard input = teclado)
                                        mov rsi,variable                   ;rsi = direccion de memoria donde se almacena la tecla capturada
                                        mov rdx,1                           ;rdx=1 (cuantos eventos o teclazos capturar)
                                        syscall

                                call _nblocks

                                call _movPlat
                                call _movBall

                                _stop3:
                      
                                mov dword [variable],0
                                call _Cero
                                call _movcursor

                        pop rcx
                dec rcx
                cmp rcx,0
                jne _looball

        call _Cero
        call _movcursor

        ;Segundo paso: Capturar una tecla presionada en el teclado

;########Predice movimiento de la lataforma

        _predmovplatLR:
                movsx r15,byte [variable]
                movsx r14,byte [botRight_]
                movsx r13,byte [botLeft_]

                cmp r15,r14
                        je _revmovplatRight
                cmp r15,r13
                        je _revmovplatLeft
                _ret:
                        ret

        _revmovplatLeft:
                cmp word [escCursor+5],'05'
                        je _ret
                _movplatLeft:
                        call _decposX
                        ret

        _revmovplatRight:
                cmp word [escCursor+5],'27'
                        je _ret
                _movplatRight:
                        call _incposX
                        ret



;Primer paso: Apagar el modo canonico y luego imprimir el banner de bienvenida
;_paso1:
	;call canonical_off
	;mov rax,1							;rax = "sys_write"
	;mov rdi,1							;rdi = 1 (standard output = pantalla)
	;mov rsi,cons_banner				;rsi = mensaje a imprimir
	;mov rdx,cons_tamano_banner	;rdx=tamano del string
	;syscall								;Llamar al sistema

							;Llamar al sistema

;Tercer paso: Imprimir el banner de salida
;_paso3:
	;mov rax,1							;rax = "sys_write"
	;mov rdi,1							;rdi = 1 (standard output = pantalla)
	;mov rsi,cons_salida				;rsi = mensaje a imprimir
	;mov rdx,cons_tamano_salida	;rdx=tamano del string
	;syscall								;Llamar al sistema

;Cuarto paso: Imprimir la tecla capturada
;_paso4:
	;mov rax,1							;rax = "sys_write"
	;mov rdi,1							;rdi = 1 (standard output = pantalla)
	;mov rsi,variable					;rsi = mensaje a imprimir
	;mov rdx,1							;rdx=solo se imprime 1 byte
	;syscall								;Llamar al sistema

;Quinto paso: Recuperar el modo canonico a su estado original y finalizacion del programa
	call canonical_on
        call echo_on
	mov rax,60				;se carga la llamada 60d (sys_exit) en rax
	mov rdi,0					;en rdi se carga un 0
	syscall						;se llama al sistema.
