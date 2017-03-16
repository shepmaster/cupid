#ifdef MSVC

#include <stdint.h>
#include <intrin.h>

void __cupid_cpuid_shim_0_3(uint32_t code, uint32_t code2, uint32_t res[4]) {
  __cpuidex(res, code, code2);
}

#else

#include <stdint.h>

void __cupid_cpuid_shim_0_3(uint32_t code, uint32_t code2, uint32_t res[4]) {
  asm("cpuid"
      : // output operands
        "=a"(res[0]),
        "=b"(res[1]),
        "=c"(res[2]),
        "=d"(res[3])
      : // input operands
        "a"(code),
        "c"(code2)
  );
}

#endif
