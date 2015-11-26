CONST EXE = "splitpem"
CONST VER = "v0.1"

CONST DQ = CHR(34)
CONST LF = CHR(10), CR = CHR(13), CRLF = CR + LF

'--------------------------------------------------------------------------------
FUNCTION STRREPLACE (buf AS STRING, src AS STRING, tgt AS STRING) AS STRING
' Return a string with all occurences of 'src' in initial string 'buf' replaced with 'tgt'
    DIM AS ZSTRING PTR iStr, iFind, iRep
    DIM AS INTEGER icor = 0
    DIM AS INTEGER i, code

    IF buf="" OR src="" THEN RETURN buf
    IF tgt="" THEN
        tgt="¿"
        icor=1
    ENDIF
    iStr  = STRPTR(buf)
    iFind = STRPTR(src)
    iRep  = STRPTR(tgt)

    DIM AS INTEGER iSize    = LEN(buf) - LEN(src)
    DIM AS ZSTRING PTR dStr = CALLOCATE(LEN(buf) * 20)
    DIM AS STRING s

     ASM
        mov esi,[iStr]
        ADD [iSize],esi
        mov ebx,[iFind]
        inc dword PTR[iSize]
        mov edi,[dStr]
     END ASM
     ASM SUB esi,1
     ASM
        jmp Start1
Start2: ADD esi,ecx
Start1: ADD esi,1
        cmp [iSize],esi
        jle Done
        movzx eax,BYTE PTR[esi]
        cmp al,[ebx]
        je Match
        mov [edi],al
        ADD edi,1
        jmp Start1
Match:  mov ecx,-1
        mov edx,ebx
B1:     ADD ecx,1
        movzx eax,BYTE PTR[edx]
        TEST eax,eax
        jz Change
        ADD edx,1
        cmp [esi+ecx],al
        je B1
        movzx eax,BYTE PTR[esi]
        mov [edi],al
        ADD edi,1
        jmp Start1
Change: mov edx,[iRep]
     END ASM
saute:  ASM SUB ecx,1
     ASM
B2:     movzx eax,BYTE PTR[edx]
        TEST eax,eax
        jz Start2
        ADD edx,1
        mov [edi],al
        ADD edi,1
        jmp B2
Done:   mov ecx,-1
B3:     ADD ecx,1
        movzx eax,BYTE PTR[esi+ecx]
        mov [edi+ecx],al
        TEST eax,eax
        jnz B3
       ' mov eax,[dStr]
       ' mov [toto],eax
    END ASM

    s = *dStr
    IF icor=0 THEN
        FUNCTION = s
        DEALLOCATE dStr
        EXIT FUNCTION
    ENDIF

    code = ASC("¿") ' change it here if you want
    ASM
        mov esi, [dStr]
        mov edx, -1       ' "get" pointer
        XOR ecx, ecx      ' "put" pointer
        TEST esi, esi     ' in case passed null string
        jz  1f
      0:
        inc edx
        movzx eax, BYTE PTR [esi+edx]
        TEST eax, eax
        jz  1f
        cmp eax,[code]  '
        je  0b
        mov [esi+ecx], al
        inc ecx
        jmp 0b
      1:
        mov [i], ecx
    END ASM

    s = LEFT(*dStr, i)
    FUNCTION = s
    DEALLOCATE dStr

END FUNCTION
'--------------------------------------------------------------------------------

'--------------------------------------------------------------------------------
FUNCTION TALLY (BYREF buf AS STRING, BYREF chunk AS STRING) AS INTEGER
' Return the number of occurences of 'chunk' inside 'buf'
    VAR n = 0, i = INSTR(buf, chunk), lc = LEN(chunk)
    WHILE i
        n += 1
        i = INSTR(i + lc, buf, chunk)
    WEND
    RETURN n
END FUNCTION
'--------------------------------------------------------------------------------

DIM AS STRING e, pem, buf
DIM AS LONG   i, ff

pem = TRIM(COMMAND(-1), ANY " " + DQ)
IF pem = "" THEN
    PRINT "<" + EXE + "> Usage: splitpem ""certificate.pem"""
    END(1)
END IF

IF LCASE(pem) = "-v" OR LCASE(pem) = "--v" OR LCASE(pem) = "-version" THEN
    PRINT EXE + " " + VER + " is Open Source GNU/GPL software written in FreeBasic by mougino"
    END(0)
END IF

ff = FREEFILE
IF OPEN (pem FOR BINARY ACCESS READ AS #ff) <> 0 THEN
    PRINT "<" + EXE + "> Fatal error: " + DQ + pem + DQ + " does not exist"
    END(1)
END IF
IF LOF(ff) > 0 THEN
    buf = STRING(LOF(ff), 0)
    IF GET( #ff, ,buf ) <> 0 THEN buf = ""
END IF
CLOSE #ff

IF INSTR(buf, "-----BEGIN PRIVATE KEY-----") = 0 _
OR INSTR(buf, "-----END PRIVATE KEY-----") = 0 _
OR INSTR(buf, "-----BEGIN CERTIFICATE-----") = 0 _
OR INSTR(buf, "-----END CERTIFICATE-----") = 0 _
THEN
    PRINT "<" + EXE + "> Fatal error: " + DQ + pem + DQ + " is incorrect"
    PRINT "           It should contain a certificate and a private key"
    END(2)
END IF

buf = STRREPLACE (buf, CRLF, LF)
FOR i = 1 TO TALLY(buf, LF)
    e = LEFT(buf, INSTR(buf, LF) - 1)
    buf = MID(buf, INSTR(buf, LF) + 1)
    IF e = "-----BEGIN PRIVATE KEY-----" THEN
        ff = FREEFILE
        OPEN "private.rsa.pem" FOR OUTPUT AS #ff
    ELSEIF e = "-----BEGIN CERTIFICATE-----" THEN
        ff = FREEFILE
        OPEN "cert.x509.pem" FOR OUTPUT AS #ff
    ELSEIF e = "-----END PRIVATE KEY-----" _
        OR e = "-----END CERTIFICATE-----" THEN
        PRINT #ff, e
        CLOSE #ff
        ff = 0
    END IF
    IF ff <> 0 THEN PRINT #ff, e
NEXT i

PRINT "<" + EXE + "> Created private key ""private.rsa.pem"""
PRINT "<" + EXE + "> Created certificate ""cert.x509.pem"""

END(0)