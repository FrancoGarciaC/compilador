#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

void* fd4_app (void** fd4_clo, void** fd4_f) {
  return (void *)(({
    void** fd4___var0 = ((void* (*) (void*, void*)) (fd4_f)[0])( (void *)fd4_f
    , (void *)12 );
    ((void* (*) (void*, void*)) (fd4___var0)[0])((void *)fd4___var0, (void *)2);
  }));
}
void* fd4___final1 (void** fd4_clo, void** fd4_y) {
  return (void *)(({
    void** fd4_x = (fd4_clo)[1];
    (uint64_t) fd4_x + (uint64_t) fd4_y;
  }));
}
void* fd4___final0 (void** fd4_clo, void** fd4_x) {
  return (void *)(fd4_mkclosure(fd4___final1, 1, fd4_x));
}
void* fd4_final;
uint64_t* fd4main() {
  fd4_final = (void *)(({
    void** fd4_suma = fd4_mkclosure(fd4___final0, 0);
    ((void* (*) (void*, void*)) fd4_app)( (void *)fd4_mkclosure(fd4_app, 0)
    , (void *)fd4_suma );
  }));
  fd4_printn((uint64_t)fd4_final)
  ;
  return 0;
}