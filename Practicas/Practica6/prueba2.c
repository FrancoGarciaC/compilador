#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

void* fd4___put3 (void** fd4_clo3, void** fd4_t) {
  return (void *)(({
    void** fd4_x = (fd4_clo3)[3];
    ({
      void** fd4_y = (fd4_clo3)[2];
      ({
        void** fd4_p = (fd4_clo3)[1];
        (uint64_t) (uint64_t) fd4_p + (uint64_t) fd4_x + (uint64_t) fd4_y;
      });
    });
  }));
}
void* fd4___put2 (void** fd4_clo2, void** fd4_p) {
  return (void *)(({
    void** fd4_x = (fd4_clo2)[2];
    ({
      void** fd4_y = (fd4_clo2)[1];
      fd4_mkclosure(fd4___put3, 3, fd4_p, fd4_y, fd4_x);
    });
  }));
}
void* fd4___put1 (void** fd4_clo1, void** fd4_y) {
  return (void *)(({
    void** fd4_x = (fd4_clo1)[1];
    ({
      void** fd4_f = fd4_mkclosure(fd4___put2, 2, fd4_y, fd4_x);
      ((void* (*) (void*, void*)) (fd4_f)[0])( (void *)fd4_f
      , (void *)1
      , (void *)2 );
    });
  }));
}
void* fd4_put (void** fd4_x, void** fd4_y) {
  return (void *)(fd4_mkclosure(fd4___put1, 1, fd4_x));
}
void* fd4_prog;
uint64_t* fd4main() {
  fd4_prog = (void *)(((void* (*) (void*, void*)) fd4_put)( (void *)5
  , (void *)8 ));
  fd4_printn((uint64_t)fd4_prog)
  ;
  return 0;
}