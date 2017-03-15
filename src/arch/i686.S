#include <asm.h>

.text

// windows conventions: ??
// unix conventions: http://wiki.osdev.org/Calling_Conventions

GLOBAL(__cupid_cpuid_0_2):
    // frame prologue
    push %ebp
    mov %esp, %ebp

    // save registers
    push %ebx
    push %esi

    // Set up arguments to cpuid
    mov 8(%ebp), %eax
    xor %ecx, %ecx

    cpuid

    // Store all return values
    mov 12(%ebp), %esi
    mov %eax, (%esi)
    mov %ebx, 4(%esi)
    mov %ecx, 8(%esi)
    mov %edx, 12(%esi)

    // restore registers
    pop %esi
    pop %ebx

    // epilogue
    pop %ebp
    ret
