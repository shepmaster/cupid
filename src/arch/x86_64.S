#include <asm.h>

.text

#if defined(WINDOWS)
// convention: https://msdn.microsoft.com/en-us/library/zthk2dkh.aspx
// saved registers: https://msdn.microsoft.com/en-us/library/6t169e9c.aspx
#define ARG1 %ecx
#define ARG2 %rdx
#else
// conventions: http://stackoverflow.com/questions/18024672
#define ARG1 %edi
#define ARG2 %rsi
#endif


GLOBAL(__cupid_cpuid_0_2):
    // save callee saved registers that we'll clobber
    push %rbx
    push %rsi

    // save off our return pointer on all platforms
    mov ARG2, %rsi

    // set up inputs to cpuid
    mov ARG1, %eax
    xor %rcx, %rcx

    cpuid

    // Store return values of cpuid
    mov %eax, (%rsi)
    mov %ebx, 4(%rsi)
    mov %ecx, 8(%rsi)
    mov %edx, 12(%rsi)

    // Restore registers
    pop %rsi
    pop %rbx

    ret
