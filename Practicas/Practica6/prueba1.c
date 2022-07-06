#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

void* fd4___put1 (void** fd4_clo1, void** fd4_t) {
  return (void *)(({
    void** fd4_z = (fd4_clo1)[1];
    ({
      wprintf(L":=");
      fd4_printn((uint64_t)(uint64_t) fd4_t + (uint64_t) fd4_z);
    });
  }));
}
void* fd4_put (void** fd4_z) {
  return (void *)(({
    void** fd4_p = fd4_mkclosure(fd4___put1, 1, fd4_z);
    ((void* (*) (void*, void*)) (fd4_p)[0])((void *)fd4_p, (void *)3);
  }));
}
void* fd4_prog;
uint64_t* fd4main() {
  fd4_prog = (void *)(((void* (*) (void*, void*)) fd4_put)((void *)5));
  fd4_printn((uint64_t)fd4_prog)
  ;
  return 0;
}