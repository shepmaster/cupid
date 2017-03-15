_text SEGMENT

; copied from x86_64.S
__cupid_cpuid_0_2 PROC
    PUSH RBX
    PUSH RSI

    MOV RSI, RDX

    MOV EAX, ECX
    XOR RCX, RCX

    CPUID

    MOV DWORD PTR [RSI], EAX
    MOV DWORD PTR [RSI + 4], EBX
    MOV DWORD PTR [RSI + 8], ECX
    MOV DWORD PTR [RSI + 12], EDX

    POP RSI
    POP RBX

    RET

__cupid_cpuid_0_2 ENDP

END
