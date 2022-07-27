#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

void* fd4___prod0 (void** fd4_clo, void** fd4_n) {
  return (void *)(({
    void** fd4_m = (fd4_clo)[2];
    ({
      void** fd4_prod = (fd4_clo)[1];
      fd4_n
      ? (void *)(uint64_t) fd4_m + (uint64_t) ({
          void** fd4___var1 = ((void* (*) (void*, void*)) (fd4_prod)[0])( (void *)fd4_prod
          , (void *)fd4_m );
          ((void* (*) (void*, void*)) (fd4___var1)[0])( (void *)fd4___var1
          , (void *)({
            fd4_sub((uint64_t) fd4_n, (uint64_t) 1);
          }) );
        })
      : (void *)0;
    });
  }));
}
void* fd4_prod (void** fd4_prod, void** fd4_m) {
  return (void *)(fd4_mkclosure(fd4___prod0, 2, fd4_prod, fd4_m));
}
void* fd4_fact (void** fd4_fact, void** fd4_x) {
  return (void *)(fd4_x
  ? (void *)({
      void** fd4___var1 = ((void* (*) (void*, void*)) fd4_prod)( (void *)fd4_mkclosure( fd4_prod
      , 0 )
      , (void *)fd4_x );
      ((void* (*) (void*, void*)) (fd4___var1)[0])( (void *)fd4___var1
      , (void *)((void* (*) (void*, void*)) (fd4_fact)[0])( (void *)fd4_fact
      , (void *)({
        fd4_sub((uint64_t) fd4_x, (uint64_t) 1);
      }) ) );
    })
  : (void *)1);
}
void* fd4_final;
uint64_t* fd4main() {
  fd4_final = (void *)(((void* (*) (void*, void*)) fd4_fact)( (void *)fd4_mkclosure( fd4_fact
  , 0 )
  , (void *)5 ));
  fd4_printn((uint64_t)fd4_final)
  ;
  return 0;
}