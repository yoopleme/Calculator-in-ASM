			;=================================================================================;
			;Copyright (c) 2015  CASSION Pierre Robentz (pierrerobentz.cassion@gmail.com);
			;This is a program writing to evaluate an arithmetic expression																						;
			;This code is under Yoople licence		
			;this programme is writed on 16 bits that mean we can use signed number from -32768 to 32767								;
			;iwe can calculate the factorial of 0 to 8. 
			;=================================================================================;

DATA SEGMENT
	welcome db  10, 13, 10, 13, "  ------BIENVENUE DANS LE PROGRAMME CALCULATEUR ET FACTORIEL------",10,13,"$"
	msg_01 db "   [--]Copyright (c) 2015, FACULTE DES SCIENCES(FSD)", 10, 13,10,13 ,"   CASSION PIERRE ROBENTZ ", 10,13,"   VILLIARD ELISEE", 10,13,"   JEAN ONES", 10,13, "  ----------------------------------------------------------------", 10, 13, 10, 13,"$"
	welcome2 db"  CE PROGRAMME EST UN CALCULATEUR QUI PERMET D'EVALUER", 10, 13, "  UNE EXPRESSION MATH AVEC LES OPERATEURS(*, /, +, -)",10,13, "  ET LE FACTORIEL D'UN ENTIER(COMPRIS ENTRE 0 ET 8)", 10, 13, "$"
	bienvenue db 10, 13, "  A- EVALUER UNE EXPRESSION MATH(PAS DE PARENTHESE) ",10,13, "  B- CALCUL DE FACTORIEL", 10, 13,"  Q- SORTIR",10,13,"Q- OU -R POUR REVENIR AU MENU", 10, 13, 10,13,"$"
    welcome3 db  10, 13, "  (Appuyer sur une lettre pour faire un choix)",10, 13,"$"
	invite db ">>","$"
	expr db 100,?, 100 DUP(0),"$"
	exprF db 7,?,9 DUP(?),"$"
	epost db 120 DUP(0),"$"
	m_epost db ">>Voici l'expression EPOST",10,13,"$"
	bon db ">>Ok",10,13,"$"
	entrer db 10,13,"$"
	bonString db ">>Nous sommes a binToString",10,13,"$"
	result db "Reponse:", 10 DUP(?),"$"
	m_evaluate db ">>Nous evaluons la chaine d'operaction",10,13,"$"
	jemul db">>Je multiplie",10,13,"$"
	jepile db ">>J'empile",10,13,"$"
	jedepile  db ">>Je depile ",10,13,"$"
	badexpr db ">>MAUVAISE EXPRESSION!",10,13,"$"
	stack_h dw ?
	dix dw 10
	bonexpre db 1
	bv_fact db "ICI ON CALCULE LE FACTORIEL D'UN COMPRIS ENTRE 0 ET 8",10,13,"$"
	ms_fact db 10,13,"fact:$"
    ms_epress db "Expression: ", 10, 13, "$"	
DATA ENDS


PILE SEGMENT
	dw 256 DUP(?)
PILE ENDS


CODE SEGMENT
    assume cs:CODE,ds:DATA, ss:PILE
			;=====================================================================;
			;								MACRO for printing a string to the screen									     ;
			;=====================================================================;
	print MACRO string
		push dx; push dx value
		push ax;
		push bx
		push cx
		lea dx, string
		mov ah , 09h; appel systeme qui s'occupe de l'affichage de la chaine de caracteres dans dx
		int 21h; interruption logicielle / interruption DOS qui declenche l'execution de l'appel systeme 09h
		pop cx
		pop bx
		pop ax
		pop  dx; initiate dx to it initiale value
	endm; end of the macro
			;=====================================================================;
			;											The program Main procedure  										 ;
			;=====================================================================;
			
	Begin  PROC
		_main:
			mov ax,DATA
			mov ds,ax
			mov ax,PILE
			mov ss,ax
			print welcome
		_prog:
		    print msg_01
			print welcome2
			print welcome3
			print bienvenue
				
		_read_exprB:
			xor bx,bx
			xor dx,dx
			print invite
			mov cx,100
			mov ah,07h
			int 21h
			cmp al,10
			je _read_exprB
			cmp al,13
			je _read_exprB
			
			cmp al,"A"
			je _calcu
			cmp al,"a"
			je _calcu
			cmp al,"B"
			je _factor
			cmp al,"b"
			je _factor
			cmp al,"q"
			je _endB
			cmp al,"Q"
			je _endB
			jmp _read_exprB
		_calcu:
			CALL Calculator_entry
			jmp _prog
		_factor:
			CALL Factorial_entry
			jmp _prog
		_endB:
			mov ah,4Ch
			int 21h
	Begin	ENDP
	;proc that manage factorial
	Factorial_entry PROC
		_read_facto:
			lea dx,ms_fact
			mov ah,9
			int 21h
			lea si,exprF
			mov dx,si
			mov ah,0Ch
			mov al,0Ah
			int 21h
			add si,2
			mov bl,[si]
			test bl,bl
			je _read_facto
			cmp bl,0h
			je _read_facto
			cmp bl,"$"
			je _read_facto
			cmp bl,10
			je _read_facto
			cmp bl,13
			je _read_facto
			cmp bl,"R"
			je _endF
			cmp bl,"r"
			je _endF
			cmp bl,"q"
			je _endF
			cmp bl,"Q"
			je _endF
			xor ax,ax
		whileF:
			mov bl,[si]
			inc si
			test bl,bl
			je _read_facto
			cmp bl,0
			je _read_facto
			cmp bl,"$"
			je _doFact
			cmp bl,10
			je _doFact
			cmp bl,13
			je _doFact
			cmp bl,"0"
			jge _continueF
			jl _read_facto
		_continueF:
			cmp bl ,"8"
			jg _read_facto
			jle _binF

		_endF:
			ret
		_doFact:
			cmp ax,9
			jge _read_facto
			CALL factoriel
			print entrer
			lea dx, result
			CALL binToString
			print entrer
			print result
			xor dx,dx
			xor si,si
			jmp _read_facto
		_binF:
			sub bl, "0"             				; on soustrait la valeur de zero de la code ascii/substract 30h to the ascii code
			mul dix
			mov bh,0h
			add ax,bx
			jmp whileF

	Factorial_entry ENDP
	
	RESETEXPR proc near
			push dx
			push si
			push cx
			mov cx,100
			mov si , 0
		_reset:
			mov epost[si],0
			inc si
		loop _reset
			pop cx
			pop si
			pop dx
			ret
		
	RESETEXPR ENDP
	; Entry point to compute an expression
	Calculator_entry PROC

			print ms_epress
		_read_expr:
			xor bx,bx
			xor dx,dx
			print invite
			mov bonexpre,1
			CALL RESETEXPR
			lea dx,expr
			mov ah,0Ah
			int 21h
			add dx,2
			mov bx,dx
			mov bl,[bx]
			cmp bl,10
			je _read_expr
			cmp bl,13
			je _read_expr
			cmp bl,"r"
			je _end
			cmp bl,"R"
			je _end
			cmp bl,"q"
			je _end
			cmp bl,"Q"
			je _end
			
			;print bon
			lea si,expr; SI contienti l'adresse de la chaine entrer par l'utilisateur
			add si,2
			lea di,epost; DI l'adresse de l'expression POSTFIXE
			CALL Analyseur
			cmp bonexpre,0 
			je _read_expr
			CALL Einf_To_Epost
			;print m_epost;
			;print epost
			;mov ax,40430
			;print expr
			lea  di, epost
			CALL Eval_Epost
			lea dx, result
			;mov ax,40430
			CALL binToStringS
			print entrer
			print result
			print entrer
			;CALL Parser
			jmp _read_expr
		_end:
			ret
	Calculator_entry ENDP
			;=============================================================;
			;    procedure writed to convert an infixexpression in the adresses begining by SI  		   ;
			;   				to a postfixexpression  in the adresse starting by DI					   				   ;
			;=============================================================;
	Einf_To_Epost proc near
			push bp
			push ax
			push bx
			push cx
			push dx
			push si
			push di
			sub sp,50
			xor ax,ax
			xor bx,bx
			xor cx,cx
			xor dx,dx
		_while4:
			mov bl, byte ptr[si]
			inc si
			test bl,bl
			je _end2
			cmp bl,"$"
			je _end2
			cmp bl,13
			je _end2 
			cmp bl,10
			je _end2 
			cmp bl,"/"
			je _mul1
			cmp bl,"*"
			je _mul1
			cmp bl,"+"
			je _add1
			cmp bl ,"-"
			je _add1
			cmp bl,"9"
			jle _continue
		_continue:
			cmp bl,"0"
			jge _digit
			;cmp 
		_digit:	mov byte ptr[di],bl
			inc di
			jmp _while4

		_end2:
			mov byte ptr[di],"|"
			inc di
		_end2_1:	
			cmp cx,0 
			je _true_end
			pop ax
			mov byte ptr[di], al
			inc di
			mov byte ptr[di],"|"
			inc di
			loop _end2_1
		_true_end:
			add sp, 50
			inc di
			mov byte ptr[di], 10
			inc di 
			mov byte ptr[di],13
			inc di
			mov byte ptr[di],"$"
			pop di
			pop si
			pop dx
			pop cx
			pop bx
			pop ax
			pop bp
			ret
		_pile_post:
			push bx
			inc cx
			jmp _while4
		_mul1:		mov byte ptr[di],"|"
			inc di
			cmp cx,0
			je _pile_post
			pop stack_h
			push stack_h
			cmp stack_h , "*"
			je _depile_post
			cmp stack_h,"/"
			je _depile_post
			jmp _pile_post
		_depile_post:  pop dx
			dec cx
			;push bx
			mov byte ptr[di],dl
			inc di
			mov byte ptr[di],"|"
			inc di
			cmp cx,0
			je _pile_post
			cmp bl,"+"
			je _depile_post
			cmp bl,"-"
			je _depile_post
			jmp _pile_post
		_add1:
			mov byte ptr[di],"|"
			inc di
			cmp cx,0
			je _pile_post
			jmp _depile_post
			;jmp _while4
	Einf_To_Epost ENDP
			;===========================================================;
			;= 			To evaluate a postfixe expression loaded in DI adresse				 				=;
			;===========================================================;
	
	Eval_Epost PROC near
			push bp
			push bx
			push cx
			push dx 
			sub sp ,50
			mov dl,10;
			xor ax,ax
			xor cx,cx 
			xor bx,bx
		_while5:
			;print bon
			mov bl, byte ptr[di]				;read a caracter in the postfixe string
			inc di									; we go to the following  char
			test bl,bl								; test if the caracter read is equal to zero
			je _end3								;if true we end the precedure
			cmp bl,"$"							; test if we are in the string end
			je _end3
			cmp bl,13							; if the user press enter
			je _end3 
			cmp bl,10							; if the user press enter
			je _end3 
			cmp bl,0							; if the user press enter
			je _end3 
			cmp bl,"*"							; for the multiiplication char
			je _mul2							
			cmp bl, "/"							;for divide char
			je _div2								
			cmp bl,"+"							; addition
			je _add2
			cmp bl,"-"							;subtraction
			je _sub2
			cmp bl,"|"							; to see if we meet a separator, it is a separator
			je _pile
		_is_digit:									; to know if the char is a digit
			cmp bl,"9"
			jbe _continu1
		_continu1:
			cmp bl,"0"
			jae _bin1
			jmp _end3
		_bin1 :										;that convert an integer string into a binary number
			xor dx,dx
			sub bl,"0"             				; on soustrait la valeur de zero de la code ascii/substract 30h to the ascii code
			mul dix
			mov bh,0
			add ax,bx
			jmp _while5
		_pile :										; pile a binary number in the stack in other to evaluate the postfixe expression
			inc cx
			push ax
			xor ax,ax	
			jmp _while5
				;=============================================================================;
				; on the following label or operation we will pop twice the stack;
				;to respect our evaluated postfixe algorithm    ;
				;=============================================================================;
		_mul2 :									
			xor dx,dx
			pop ax
			pop bx
			mul bx
			sub cx,2
			jmp _while5
		_div2:
			xor dx,dx
			sub cx,2
			pop bx
			pop ax
			div bx
			jmp _while5
		_add2:
			sub cx,2
			pop bx
			pop ax
			add ax,bx
			jmp _while5
		_sub2:
			sub cx,2
			pop bx
			pop ax
			sub ax,bx
			jmp _while5
		_end3 :
			pop ax
			add sp ,50
			pop dx
			pop cx
			pop bx
			pop bp
			ret
	Eval_Epost ENDP
	
			;procedure permettant de convertir un entier en chaine de caractere(l'adresse de la chaine se trouvera dans dx et l'entier dans ax)
			;====================================================================;
			; 							To convert a binary(16bits) number to an integer string									      ;
			;====================================================================;
	binToString proc near
			;print bonString
			push bp
			push si
			push dx
			push ax
			push cx
			lea si, result
			add si,17
			mov cx,9
			xor bx,bx
			mov bx,10
			;mov bh, 0
			xor dx,dx 
		while2:
			div bx
			add dl, '0'
			mov byte ptr[si],dl
			xor dx,dx
			dec si
			dec cx
			cmp ax, 0
			je fin2
			jmp while2
		fin2:
			mov bx,cx
		fin2_1:
			cmp cx,0
			je finF
			mov byte ptr[si],0
			dec si
			loop fin2_1
		finF:
			pop cx
		    pop ax
			pop dx
			pop si
			pop bp
			ret
		
    ENDP binToString
	;//Affichage des nombres negatifs //printing of negatif number
	binToStringS proc
		push si
		push ax
		cmp ax,32768
		ja addminus
		jb _nominus
	   addminus:
		neg ax
		CALL binToString
		lea si, result
		add si,8
		add si,bx
		mov byte ptr[si],"-"
		jmp retB
		_nominus:
			call binToString
		retB:
		pop ax
		pop si
			ret

	binToStringS ENDP
	
	;Analyseur is a procedure that make us know if an expression is correct or not
	Analyseur proc near
			push bp
			push di
			push si
			push ax
			push cx
			xor cx,cx
			mov ah,[si]
			cmp ah,"*"
			je _bad_expr
			cmp ah,"/"
			je _bad_expr
			cmp ah,"$"
			je _endAnal
			cmp ah,13
			je _endAnal
			cmp ah,0
			je _endAnal
			cmp ah,10
			je _endAnal
		_whileCar:
			mov ah,[si]
			inc si
			mov al,[si]
			cmp ah,"*"
			je _is_char_after
			cmp ah,"/"
			je _is_char_after
			cmp ah,"+"
			je _is_char_after
			cmp ah,"-"
			je _is_char_after
			cmp ah,"$"
			je _befor_end
			cmp ah,13
			je _befor_end
			cmp ah,0
			je _befor_end
			cmp ah,10
			je _befor_end
			_is_digitA:									; to know if the char is a digit
			cmp ah,"9"
			jle _continuA1
			jge _bad_expr
		_continuA1:
			cmp ah,"0"
			jge _whileCar
			jne _bad_expr
		jmp _bad_expr
	
		_bad_expr:
			print badexpr
		 mov bonexpre,0
		_endAnal:
			pop cx
			pop ax
			pop si
			pop di
			pop bp
			ret
			
		_is_char_after:
			cmp al,"*"
			je _bad_expr
			cmp al,"/"
			je _bad_expr
			cmp al,"+"
			je _bad_expr
			cmp al,"-"
			je _bad_expr
			cmp al,"$"
			je _bad_expr
			cmp al,13
			je _bad_expr
			cmp al,0
			je _bad_expr
			cmp al,10
			je _bad_expr
			jmp _whileCar
		_befor_end:
			cmp cx,0
			jne _bad_expr
			je _endAnal

	Analyseur ENDP
	
	;This procedure compute the factorial  of a number
	factoriel proc
			mov cx,ax
			cmp ax,0
			je fin4
			mov ax ,1
			cmp cx,0
			je fin3
		fact1:
			mul cx
			loop fact1
		fin3:
			ret 
		fin4:
		mov ax,1
		ret 
	ENDP factoriel

	
	
CODE ENDS
END _main
