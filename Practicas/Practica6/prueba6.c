#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

void* fd4___suma0 (void** fd4_clo, void** fd4_y) {
  return (void *)(({
    void** fd4_x = (fd4_clo)[1];
    fd4_y
    ? (void *)({
        void** fd4___var1 = ((void* (*) (void*, void*)) (fd4_suma)[0])( (void *)fd4_suma
        , (void *)(uint64_t) fd4_x + (uint64_t) 1 );
        ((void* (*) (void*, void*)) (fd4___var1)[0])( (void *)fd4___var1
        , (void *)({
          fd4_sub((uint64_t) fd4_y, (uint64_t) 1);
        }) );
      })
    : (void *)fd4_x;
  }));
}
void* fd4_suma (void** fd4_clo, void** fd4_x) {
  return (void *)(fd4_mkclosure(fd4___suma0, 1, fd4_x));
}

void* fd4_final;
uint64_t* fd4main() {
  fd4_final = (void *)(({
    void** fd4___var1 = ((void* (*) (void*, void*)) fd4_suma)( (void *)fd4_mkclosure( fd4_suma
    , 0 )
    , (void *)12 );
    ((void* (*) (void*, void*)) (fd4___var1)[0])( (void *)fd4___var1
    , (void *)25 );
  }));
  fd4_printn((uint64_t)fd4_final)
  ;
  return 0;
}