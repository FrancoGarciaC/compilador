#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

uint64_t fd4___suma0 (void**  fd4_clo, uint64_t  fd4_n) {
  return (void *)(({
    void** fd4_m = (fd4_clo)[2];
    ({
      void** fd4_suma = (fd4_clo)[1];
      fd4_n
      ? (void *)(uint64_t) ({
          void** fd4___var1 = ((void* (*) (void*, void*)) (fd4_suma)[0])( (void *)fd4_suma
          , (void *)fd4_m );
          ((void* (*) (void*, void*)) (fd4___var1)[0])( (void *)fd4___var1
          , (void *)({
            fd4_sub((uint64_t) fd4_n, (uint64_t) fd4_n);
          }) );
        }) + (uint64_t) ({
          void** fd4___var2 = ((void* (*) (void*, void*)) (fd4_suma)[0])( (void *)fd4_suma
          , (void *)fd4_m );
          ((void* (*) (void*, void*)) (fd4___var2)[0])( (void *)fd4___var2
          , (void *)({
            fd4_sub((uint64_t) fd4_n, (uint64_t) fd4_n);
          }) );
        })
      : (void *)fd4_m;
    });
  }));
}
void* fd4_suma (void**  fd4_suma, uint64_t  fd4_m) {
  return (fd4_mkclosure(fd4___suma0, 2, fd4_suma, fd4_m));
}
void* fd4_final;
uint64_t* fd4main() {
  fd4_final = (void *)(({
    void* fd4___var1 = ((void* (*) (void*, void*)) fd4_suma)( (void *)fd4_mkclosure( fd4_suma
    , 0 )
    , (void *)2 );
    ((void* (*) (void*, void*)) (fd4___var1)[0])((void *)fd4___var1, (void *)3);
  }));
  fd4_printn((uint64_t)fd4_final)
  ;
  return 0;
}