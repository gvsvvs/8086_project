MYDATA   SEGMENT 
    
 
         ;Bit pattern for Dispaying Numerals on SSD
         KCODE0	     DB	 06H,5BH,4FH       
         KCODE1	     DB	 66H,6DH,0FDH       
         KCODE2	     DB	 07H,7FH,0EFH       
         KCODE3	     DB	 00H,3FH,00H 
         
         SSPA EQU 0000H
         SSPB EQU 0002H
         SSPC EQU 0004H    ;port addresses for SSDs interfacing
         SSCR EQU 0006H 
         
         KBPA EQU 0020H
         KBPB EQU 0022H    ;port addresses for Keypad  interfacing
         KBPC EQU 0024H
         KBCR EQU 0026H
         
         MSC EQU 3003H
         MSB EQU 3002H     ;temporary usage for memory
         MSA EQU 3001H 
          
         PORTD EQU 00E0H 
         PORTE EQU 00E2H   ;port addresses for temperature sensor interfacing
         PORTF EQU 00E4H
         CONTROLB EQU 00E6H  
         
         PORTG EQU 0060H 
         PORTH EQU 0062H
         PORTI EQU 0064H      ;port addresses for buzzer interfacing
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
         MOV DX, CONTROLB    ;setting the port a to input(temperature sensor)
         OUT DX, AL
         
         MOV AL, 80H 
         MOV DX, CONTROLC       ;  setting all ports to output(buzzer and led lights)
         OUT DX, AL 
         
         MOV AL, 90H        ;seting port a to input for keypad interfacing
         MOV DX, KBCR
         OUT DX, AL   
                    
                    
         MOV AL, 80H 
         MOV DX, SSCR       ;  setting all ports to output for SSDs
         OUT DX, AL
         ; Initialize SSD by displayong 0
         
         MOV AL, 3FH
         MOV BX, MSC
         MOV [BX], 3FH
         MOV BX, MSB                                 
         MOV [BX], 3FH          ;initializing all SSDs to 0
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
         MOV [BX],AL      ;taking the hundreds digit in and storing in 4000h
         MOV [4000H],AL         
         
         CALL KEY_WRITE     
         
         CALL DELAY_S
         CALL CONVERT_TO_BINAY
         MOV BX,MSB
         MOV [BX],AL
         MOV BL,10             ;taking the tens digit in and storing in 4000h by multiplying the number in 4000h by 10 and adding this tens digit
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
         MOV BL,10 
         MOV CL,AL
         MOV AL,[4000H]             ;taking the ones digit in and storing in 4000h by multiplying the number in 4000h by 10 and adding this ones digit
         MOV AH,00H
         MUL BL
         ADD AL,CL
         MOV [4000H],AL 
   
         RET 
         
         FIRST:CALL AGAIN
         MOV [1000H],AL
         MOV DX,SSPC           ;storing the first threshold in 1000h
         OUT DX,AL
         SECOND:CALL AGAIN
         MOV [1002H],AL
         MOV DX,SSPB                    ;storing the second threshold in 1002h
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
         
         ; Make Row3 only 0 rest of the rows 1
         
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
          
MATCH:   ;MOVING SSD DISPLAY

         MOV CX,BX
         MOV AL, [BX]
         MOV DX, KBPC  ;displaying the pressed key on SSD which is interfaced with the one the keypad is also interfaced
         OUT DX, AL          
         
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
        CMP AL,3FH           ;comparing the offsets to map to digits 
        
        JE STORE_B           ;if matched jumping to storing binary(digit)
        
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
        MOV AL,CL       ;instead of LED's display mumber storing the original value in AL 
        
        RET    
CONVERT_TO_BINAY ENDP 



               
               
TEMPSENS:      
         MOV DX, PORTD
         IN AL, DX            ;reading the input from temperature sensor
         
         MOV BH,00
         MOV BL,AL  
         
         MOV CX,500
         XOR AH,AH
         MUL CX                   ;multiplying with 500 and divide with 256 to get original tempe
         MOV CX,256
         DIV CX
         MOV BL,AL 
         CALL DELAY_S  
         CALL DELAY_S
         MOV [2000H], BX          ;store the read temperature in 2000h
         MOV AX,BX
         MOV CL,10
         DIV CL
         MOV BL,AH 
         MOV BH,00H
         MOV AH,00H
         
         
         MOV AL, 80H              ;set the SSDs interfaced 8255's ports to output 
         MOV DX, SSCR
         OUT DX, AL   
         
         
         ; Get pointer to the SSD Table
         
         MOV SI, OFFSET SSD_TABLE          ;loading the offset of the SSD_TABLE

         
         
AGAIN1:  ADD SI, BX                                ;adding to the base address by the key pressed value to display in SSD
        MOV AL, [SI]   ; get the byte from table
         
         MOV DX, SSPC  ; display on SSD
         OUT DX, AL                         ;display ones digit in LAST SSD
          
         MOV BX,[2000H]
         MOV AX,BX
         MOV CL,10
         DIV CL 
         MOV AH,00H                            ;calculate the tens digit by dividing the number in 2000h by 10
         DIV CL
         MOV BL,AH 
         MOV BH,00H
         MOV AH,00H
                                                             ;increase the base address of SSD by the remainder
         MOV SI, OFFSET SSD_TABLE 
         
        ; MOV BL, 0AH    ; A counter for 10 digits
         
         
        ADD SI, BX 
        MOV AL, [SI] 
         
         MOV DX, SSPB  ; display tens in second SSD
         OUT DX, AL 
         
         MOV BX,[2000H]
         MOV AX,BX
         MOV CL,10
         DIV CL 
         MOV AH,00H
         DIV CL
         MOV AH,00H
         DIV CL                             ;calculate hundreds by dividing the number in 2000h by 100 and store the remainder 
         MOV BL,AH 
         MOV BH,00H
         MOV AH,00H
         
         MOV SI, OFFSET SSD_TABLE           ; increase the base address by the remainder to map to the correct LED display
         
         
        ADD SI, BX 
        MOV AL, [SI] 
         
        MOV DX, SSPA  ; display the hundreds number in the top SSD
        OUT DX, AL
        
        
        
        MOV AX,[2000H]   ;load the temperature which is read in AX
        MOV BX,[1000H]   ;load the first threshold in BX
        XOR BH,BH      
        CMP AX,BX        ;comapre AX and BX
        JNC CAG          ;if the temperature read is greater than the first threshold then jump to subroutine CAG
        
        MOV AL,00H
        MOV DX,PORTI     ;if it is not greater then no buzzer and no light
        OUT DX,AL

        JMP R            ;jump to R
        
        CAG:   
        
        MOV AX,[2000H] 
        MOV BX,[1002H]
        XOR BH,BH          ;load the second threshold into bx 
        CMP AX,BX
        
        JC CONTI           ;if the temperaure read is greater than the second threshold the go for high frequency Buzzer
        
        MOV AL,0AH
        MOV DX,PORTI       ;if greater than the first threshold and less than second threshold 
        OUT DX,AL          ;low frequency buzzer and yellow light
        CALL  DELAY_S
        JMP R
        
        CONTI: 
        MOV AL,05H
        MOV DX,PORTI       ;if temp read is greater than both the thresholds the high frequency and red light
        OUT DX,AL
         

        
        R:JMP TEMPSENS    ;jump to tempsens until stopped read the modified temperature(if any) and act accordingly
         
         

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
         
 ;END  