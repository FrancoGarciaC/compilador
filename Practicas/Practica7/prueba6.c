#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

void* fd4_final;
uint64_t* fd4main() {
  fd4_final = (void *)(({
    uint64_t  fd4_res = 4 + 5;
    fd4_res;
  }));
  fd4_printn((uint64_t)fd4_final)
  ;
  return 0;
}