#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

void* fd4___suma0 (void** fd4_clo, void** fd4_y) {
  return (void *)(({
    void** fd4_x = (fd4_clo)[2];
    ({
      void** fd4_suma = (fd4_clo)[1];
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
    });
  }));
}
void* fd4_suma (void** fd4_suma, void** fd4_x) {
  return (void *)(fd4_mkclosure(fd4___suma0, 2, fd4_suma, fd4_x));
}
void* fd4___resta0 (void** fd4_clo, void** fd4_y) {
  return (void *)(({
    void** fd4_x = (fd4_clo)[2];
    ({
      void** fd4_resta = (fd4_clo)[1];
      fd4_y
      ? (void *)({
          void** fd4___var1 = ((void* (*) (void*, void*)) (fd4_resta)[0])( (void *)fd4_resta
          , (void *)({
            fd4_sub((uint64_t) fd4_x, (uint64_t) 1);
          }) );
          ((void* (*) (void*, void*)) (fd4___var1)[0])( (void *)fd4___var1
          , (void *)({
            fd4_sub((uint64_t) fd4_y, (uint64_t) 1);
          }) );
        })
      : (void *)fd4_x;
    });
  }));
}
void* fd4_resta (void** fd4_resta, void** fd4_x) {
  return (void *)(fd4_mkclosure(fd4___resta0, 2, fd4_resta, fd4_x));
}
void* fd4_final;
uint64_t* fd4main() {
  fd4_final = (void *)(({
    void** fd4___var3 = ((void* (*) (void*, void*)) fd4_suma)( (void *)fd4_mkclosure( fd4_suma
    , 0 )
    , (void *)({
      void** fd4___var1 = ((void* (*) (void*, void*)) fd4_resta)( (void *)fd4_mkclosure( fd4_resta
      , 0 )
      , (void *)200 );
      ((void* (*) (void*, void*)) (fd4___var1)[0])( (void *)fd4___var1
      , (void *)100 );
    }) );
    ((void* (*) (void*, void*)) (fd4___var3)[0])( (void *)fd4___var3
    , (void *)500 );
  }));
  fd4_printn((uint64_t)fd4_final)
  ;
  return 0;
}