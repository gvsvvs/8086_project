; This file detects the pressed key on a 4x4 keypad
; and displays the character pressed on the seven segment display.



MYDATA   SEGMENT 
    
 
         ;Bit pattern for Dispaying Numerals on SSD
         KCODE0	     DB	 06H,5BH,4FH       
         KCODE1	     DB	 66H,6DH,0FDH       
         KCODE2	     DB	 07H,7FH,0EFH       
         KCODE3	     DB	 00H,3FH,00H 
         
         SSPA EQU 0000H
         SSPB EQU 0002H
         SSPC EQU 0004H
         SSCR EQU 0006H 
         KBPA EQU 0020H
         KBPB EQU 0022H
         KBPC EQU 0024H
         KBCR EQU 0026H
         
         MSC EQU 3003H
         MSB EQU 3002H
         MSA EQU 3001H 
          
         PORTD EQU 00E0H 
         PORTE EQU 00E2H
         PORTF EQU 00E4H
         CONTROLB EQU 00E6H 
         PORTG EQU 0060H 
         PORTH EQU 0062H
         PORTI EQU 0064H
         CONTROLC EQU 0066H
         ;Bit pattern for Dispaying Numerals on SSD
         SSD_TABLE DB 3FH, 06H, 5BH, 4FH, 66H, 6DH, 7DH, 07H, 7FH, 6FH
        
         
         
          
	        
          
MYDATA   ENDS 


MYSTACK  SEGMENT STACK
    
         DW 100 DUP(0)
         STACK_TOP  LABEL WORD
       
MYSTACK  ENDS

MYCODE   SEGMENT  
         
         ASSUME CS:MYCODE, DS:MYDATA, SS:MYSTACK 
         
         ; Initialize Segment Registers
         
         MOV AX, MYDATA
         MOV DS, AX
         
         MOV AX, MYSTACK
         MOV SS, AX
         LEA SP, STACK_TOP
                                                
                                                
         ; Initialize the 8255 peripheral in I/O MODE
         ; PORTA as OUTPUT, PORTB as OUTPUT and PORTC as INPUT           
    
START:   MOV AL, 90H 
         MOV DX, CONTROLB
         OUT DX, AL
         
         MOV AL, 80H 
         MOV DX, CONTROLC
         OUT DX, AL 
         
         MOV AL, 90H 
         MOV DX, KBCR
         OUT DX, AL   
                    
                    
         MOV AL, 80H 
         MOV DX, SSCR
         OUT DX, AL
         ; Initialize SSD by displayong 0
         
         MOV AL, 3FH
         MOV BX, MSC
         MOV [BX], 3FH
         MOV BX, MSB                                 
         MOV [BX], 3FH
         MOV BX, MSA
         MOV [BX], 3FH
         MOV DX, SSPC
         OUT DX, AL
         MOV DX, SSPB
         OUT DX, AL
         MOV DX, SSPA
         OUT DX, AL  
         
         JMP FIRST
         
         
AGAIN:   CALL KEY_WRITE     
         
         CALL DELAY_S
         CALL CONVERT_TO_BINAY
         
         MOV BX,MSA
         MOV [BX],AL 
         ;MOV AL,[BX]
         ;MOV DX,SSPC
         ;OUT DX,AL
         MOV [4000H],AL         
         
         CALL KEY_WRITE     
         
         CALL DELAY_S
         CALL CONVERT_TO_BINAY
         MOV BX,MSB
         MOV [BX],AL
         ;MOV AL,[BX]
         ;MOV DX,SSPB
         ;OUT DX,AL
         MOV BL,10 
         MOV CL,AL
         MOV AL,[4000H]
         MOV AH,00H
         MUL BL
         ADD AL,CL
         MOV [4000H],AL
         
         CALL KEY_WRITE     
         
         CALL DELAY_S
         CALL CONVERT_TO_BINAY
         MOV BX,MSC
         MOV [BX],AL
         ;MOV AL,[BX]
         ;MOV DX,SSPA
         ;OUT DX,AL
         MOV BL,10 
         MOV CL,AL
         MOV AL,[4000H]
         MOV AH,00H
         MUL BL
         ADD AL,CL
         MOV [4000H],AL 

         ;CALL DELAY_S
         ;CALL DELAY_S
         ;CALL DELAY_S
         ;CALL DELAY_S
         ;CALL DELAY_S
         
         
         
                 
         RET 
         
         FIRST:CALL AGAIN
         MOV [1001H],AL
         MOV DX,SSPC
         OUT DX,AL
         SECOND:CALL AGAIN
         MOV [1002H],AL
         MOV DX,SSPB
         OUT DX,AL 
         
         CALL TEMPSENS 
         
         JMP FIRST
; PROCEDURE: KEY_WRITE - Write the key pressed on the SSD
; INPUT : NONE
; OUTPUT: NONE

KEY_WRITE PROC  NEAR       
             
          
         
         ; Make all rows as 0
K1:      MOV DX, KBPB 
         MOV AL, 0F0H
         OUT DX, AL  
         
         ; Read Columns
         MOV DX, KBPA
         IN AL, DX
         AND AL, 0FH
         
         ; Compare to see all keys open in the beginning
         CMP AL, 0FH
         JNE K1 
            

         CALL DELAY_MS      
                        
         
K2:      ; Read Columns
         MOV DX, KBPA
         IN AL, DX
         AND AL,0FH
         
         ; Compare to see if any key is pressed
         CMP AL, 0FH
         JNE NEXT
         JMP K2
          
NEXT:    ; CALL 20ms Delay (Debounce Delay)
         CALL DELAY_MS
           
         
         ; Read Columns again
         MOV DX, KBPA
         IN AL, DX
         AND AL,0FH
         
         ; Compare to see if key is still pressed
         CMP AL, 0FH
         JNE NEXT1
         JMP K2 
         
         ; Keypress is detected, now do the row scanning   
                 
         ; Make Row0 only 0 rest of the rows 1
         
NEXT1:   MOV DX, KBPB 
         MOV AL, 0FEH
         OUT DX, AL  
         
                 
         ; Read Columns
         MOV DX, KBPA
         IN AL, DX
         AND AL,0FH
         
         ; Compare to see if key is pressed
         CMP AL, 0FH
         JNE ROW_0   
         
         ; Make Row1 only 0 rest of the rows 1
         
         MOV DX, KBPB 
         MOV AL, 0FDH
         OUT DX, AL  
         
         ; Read Columns
         MOV DX, KBPA
         IN AL, DX
         AND AL,0FH
         
         ; Compare to see if key is pressed
         CMP AL, 0FH
         JNE ROW_1
         
         ; Make Row2 only 0 rest of the rows 1
         
         MOV DX, KBPB
         MOV AL, 0FBH
         OUT DX, AL  
         
         ; Read Columns
         MOV DX, KBPA
         IN AL, DX
         AND AL,0FH
         
         ; Compare to see if key is pressed
         CMP AL, 0FH
         JNE ROW_2
         
         ; Make Row0 only 0 rest of the rows 1
         
         MOV DX, KBPB 
         MOV AL, 0F7H
         OUT DX, AL  
         
         ; Read Columns
         MOV DX, KBPA
         IN AL, DX
         AND AL,0FH
         
         ; Compare to see if key is pressed
         CMP AL, 0FH
         JNE ROW_3   
         
ROW_0:   MOV BX, OFFSET KCODE0
         JMP FIND_COL

ROW_1:   MOV BX, OFFSET KCODE1
         JMP FIND_COL

ROW_2:   MOV BX, OFFSET KCODE2
         JMP FIND_COL

ROW_3:   MOV BX, OFFSET KCODE3
         JMP FIND_COL
         
FIND_COL: RCR AL,1
          JNC MATCH
          INC BX
          ;MOV CX,BX
          JMP FIND_COL
          
MATCH:   ;MOVING SSG DISPLAY

         
         MOV CX,BX; 
         
                                
         ;MOV BX,MSB
         ;MOV AL,[BX]
         ;MOV DX,SSPA
         ;OUT DX,AL
         ;MOV BX,MSA
         ;MOV [BX],AL 
         
         ;MOV BX,MSC
         ;MOV AL,[BX]
         ;MOV DX,SSPB
         ;OUT DX,AL
         ;MOV BX,MSB
         ;MOV [BX],AL
                                                                
         ;DECODING KEY AND DISPLAING IN SS C
         ;MOV BX, CX
         MOV AL, [BX]
         MOV DX, KBPC
         OUT DX, AL
         ;MOV BX,MSC
         ;MOV [BX],AL
         
          
         
         RET  
         
KEY_WRITE ENDP


; PROCEDURE: DELAY_MS : Generates approx 20 ms delay
; INPUT : NONE
; OUTPUT: NONE  

DELAY_MS PROC  NEAR  
         PUSH CX  
         
          
    
         MOV CX, 09C4H
RPT1:    LOOP RPT1

         POP CX  
         
         RET
DELAY_MS ENDP    


; PROCEDURE: DELAY_S : Generates approx 0.5 s delay
; INPUT : NONE
; OUTPUT: NONE  


CONVERT_TO_BINAY PROC NEAR
        ;MOV BX,MSC
        ;MOV AL,[BX]
        
         
        MOV CL,00H
        CMP AL,3FH
        
        JE STORE_B
        
        MOV CL,01H
        CMP AL,06H
        JE STORE_B
        
        MOV CL,02H
        CMP AL,5BH
        JE STORE_B
        
        MOV CL,03H
        CMP AL,4FH
        JE STORE_B
        
        MOV CL,04H
        CMP AL,66H
        JE STORE_B
        
        MOV CL,05H
        CMP AL,6DH
        JE STORE_B
        
        MOV CL,06H
        CMP AL,0FDH
        JE STORE_B
        
        MOV CL,07H
        CMP AL,07H
        JE STORE_B
        
        MOV CL,08H
        CMP AL,7FH
        JE STORE_B
        
        MOV CL,09H
        CMP AL,4FH
        JE STORE_B
        
        
        
STORE_B:NOP
        MOV AL,CL
        
        RET    
CONVERT_TO_BINAY ENDP 



               
               
TEMPSENS:      
         MOV DX, PORTD
         IN AL, DX
         
         MOV BH,00
         MOV BL,AL  
         
         MOV CX,500
         XOR AH,AH
         MUL CX
         MOV CX,256
         DIV CX
         MOV BL,AL 
         CALL DELAY_S  
         CALL DELAY_S
         MOV [2000H], BX
         MOV AX,BX
         MOV CL,10
         DIV CL
         MOV BL,AH 
         MOV BH,00H
         MOV AH,00H
         
         
         MOV AL, 80H 
         MOV DX, SSCR
         OUT DX, AL   
         
         ; Initialize SSD by displayong 0
         
        ; MOV AL, 3FH
         ;MOV DX, PORTA
         ;OUT DX, AL 
         
         
         ; Get pointer to the SSD Table
         
         MOV SI, OFFSET SSD_TABLE 
         
        ; MOV BL, 0AH    ; A counter for 10 digits
         
         
AGAIN1:  ADD SI, BX 
        MOV AL, [SI]   ; get the byte from table
         
         MOV DX, SSPC  ; display on SSD
         OUT DX, AL
          
         MOV BX,[2000H]
         MOV AX,BX
         MOV CL,10
         DIV CL 
         MOV AH,00H
         DIV CL
         MOV BL,AH 
         MOV BH,00H
         MOV AH,00H
         
         MOV SI, OFFSET SSD_TABLE 
         
        ; MOV BL, 0AH    ; A counter for 10 digits
         
         
        ADD SI, BX 
        MOV AL, [SI] 
         
         MOV DX, SSPB  ; display on SSD
         OUT DX, AL 
         
         MOV BX,[2000H]
         MOV AX,BX
         MOV CL,10
         DIV CL 
         MOV AH,00H
         DIV CL
         MOV AH,00H
         DIV CL
         MOV BL,AH 
         MOV BH,00H
         MOV AH,00H
         
         MOV SI, OFFSET SSD_TABLE 
         
        ; MOV BL, 0AH    ; A counter for 10 digits
         
         
        ADD SI, BX 
        MOV AL, [SI] 
         
        MOV DX, SSPA  ; display on SSD
        OUT DX, AL
        
        
        MOV DX,KBPC
        MOV AL,[1002H]
        OUT DX,AL 
        
        MOV AX,[2000H]
        MOV BX,50        
        CMP AX,BX
        JNC CAG
        ;JMP START
        
        MOV DX,KBPC
        MOV AL,01H
        OUT DX,AL
        JMP R  
        
        CAG:   
        MOV DX,KBPC
        MOV AL,02H
        OUT DX,AL 
        
        
        MOV AX,[2000H] 
        MOV BX,100
        CMP AX,BX
        JC CONTI
        MOV AL,0AH
        MOV DX,PORTI
        OUT DX,AL
        NOP  
        CALL  DELAY_S
        JMP R
        
        CONTI: 
        MOV DX,KBPC
        MOV AL,07H
        OUT DX,AL 
        
        
        MOV AL,05H
        MOV DX,PORTI
        OUT DX,AL
         
        
        
        R:
        JMP TEMPSENS 
         
         

; PROCEDURE: DELAY_S : Generates approx 0.5 s delay
; INPUT : NONE
; OUTPUT: NONE  

DELAY_S PROC  NEAR 
    
        PUSH CX
    
        MOV CX, 0FFFFH
RPT2:   LOOP RPT2 

        POP CX
         
        RET
DELAY_S ENDP          
   

MYCODE   ENDS
        
         END  START   
         
;----------------- END OF FILE -------------------------------------------;