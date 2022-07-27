#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

void* fd4___map0 (void** fd4_clo, void** fd4_f) {
  return (void *)(({
    void** fd4_x = (fd4_clo)[2];
    ({
      void** fd4_map = (fd4_clo)[1];
      fd4_x
      ? (void *)({
          void** fd4_res = ((void* (*) (void*, void*)) (fd4_f)[0])( (void *)fd4_f
          , (void *)fd4_x );
          ({
            void** fd4___var1 = ((void* (*) (void*, void*)) (fd4_map)[0])( (void *)fd4_map
            , (void *)({
              fd4_sub((uint64_t) fd4_x, (uint64_t) 1);
            }) );
            ((void* (*) (void*, void*)) (fd4___var1)[0])( (void *)fd4___var1
            , (void *)fd4_f );
          });
        })
      : (void *)fd4_x;
    });
  }));
}
void* fd4_map (void** fd4_map, void** fd4_x) {
  return (void *)(fd4_mkclosure(fd4___map0, 2, fd4_map, fd4_x));
}
void* fd4___mapF0 (void** fd4_clo, void** fd4_y) {
  return (void *)(({
    void** fd4_x = (fd4_clo)[1];
    ({
      wprintf(L"x+y:=");
      fd4_printn((uint64_t)(uint64_t) fd4_x + (uint64_t) fd4_y);
    });
  }));
}
void* fd4_mapF (void** fd4_mapF, void** fd4_x) {
  return (void *)(fd4_mkclosure(fd4___mapF0, 1, fd4_x));
}
void* fd4_final;
uint64_t* fd4main() {
  fd4_final = (void *)(({
    void** fd4___var2 = ((void* (*) (void*, void*)) fd4_map)( (void *)fd4_mkclosure( fd4_map
    , 0 )
    , (void *)20 );
    ((void* (*) (void*, void*)) (fd4___var2)[0])( (void *)fd4___var2
    , (void *)((void* (*) (void*, void*)) fd4_mapF)( (void *)fd4_mkclosure( fd4_mapF
    , 0 )
    , (void *)5 ) );
  }));
  fd4_printn((uint64_t)fd4_final)
  ;
  return 0;
}