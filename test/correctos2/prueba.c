#include <inttypes.h>
#include <wchar.h>
extern void** fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern uint64_t fd4_sub(uint64_t, uint64_t);

uint64_t fd4_prog;
uint64_t* fd4main() {
  fd4_prog = (void *)(24);
  fd4_printn((uint64_t)fd4_prog)
  ;
  return 0;
}