.586
.MODEL FLAT, C
.CODE

; this is all copied from i686.S
__cupid_cpuid_0_2 PROC
    PUSH EBP
    MOV EBP, ESP

    PUSH EBX
    PUSH ESI

    MOV EAX, DWORD PTR [EBP + 8]
    XOR ECX, ECX

    CPUID

    MOV ESI, DWORD PTR [EBP + 12]
    MOV DWORD PTR [ESI], EAX
    MOV DWORD PTR [ESI + 4], EBX
    MOV DWORD PTR [ESI + 8], ECX
    MOV DWORD PTR [ESI + 12], EDX

    POP ESI
    POP EBX

    POP EBP
    RET
__cupid_cpuid_0_2 ENDP

END
