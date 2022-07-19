#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

void* fd4_const;
void* fd4___sum3 (void** fd4_clo, void** fd4_z) {
  return (void *)(({
    void** fd4_v = (fd4_clo)[4];
    ({
      void** fd4_w = (fd4_clo)[3];
      ({
        void** fd4_x = (fd4_clo)[2];
        ({
          void** fd4_y = (fd4_clo)[1];
          (uint64_t) (uint64_t) (uint64_t) (uint64_t) (uint64_t) fd4_v + (uint64_t) fd4_w + (uint64_t) fd4_x + (uint64_t) fd4_y + (uint64_t) fd4_z + (uint64_t) fd4_const;
        });
      });
    });
  }));
}
void* fd4___sum2 (void** fd4_clo, void** fd4_y) {
  return (void *)(({
    void** fd4_v = (fd4_clo)[3];
    ({
      void** fd4_w = (fd4_clo)[2];
      ({
        void** fd4_x = (fd4_clo)[1];
        fd4_mkclosure(fd4___sum3, 4, fd4_y, fd4_x, fd4_w, fd4_v);
      });
    });
  }));
}
void* fd4___sum1 (void** fd4_clo, void** fd4_x) {
  return (void *)(({
    void** fd4_v = (fd4_clo)[2];
    ({
      void** fd4_w = (fd4_clo)[1];
      fd4_mkclosure(fd4___sum2, 3, fd4_x, fd4_w, fd4_v);
    });
  }));
}
void* fd4___sum0 (void** fd4_clo, void** fd4_w) {
  return (void *)(({
    void** fd4_v = (fd4_clo)[1];
    fd4_mkclosure(fd4___sum1, 2, fd4_w, fd4_v);
  }));
}
void* fd4_sum (void** fd4_clo, void** fd4_v) {
  return (void *)(fd4_mkclosure(fd4___sum0, 1, fd4_v));
}
void* fd4_sumatemp1 (void** fd4_clo, void** fd4_dummy) {
  return (void *)(((void* (*) (void*, void*)) fd4_sum)( (void *)fd4_mkclosure( fd4_sum
  , 0 )
  , (void *)3 ));
}
void* fd4_sumatemp2 (void** fd4_clo, void** fd4_dummy) {
  return (void *)(({
    void** fd4___var0 = ((void* (*) (void*, void*)) fd4_sumatemp1)( (void *)fd4_mkclosure( fd4_sumatemp1
    , 0 )
    , (void *)0 );
    ((void* (*) (void*, void*)) (fd4___var0)[0])((void *)fd4___var0, (void *)2);
  }));
}
void* fd4_sumatemp3 (void** fd4_clo, void** fd4_dummy) {
  return (void *)(({
    void** fd4___var0 = ((void* (*) (void*, void*)) fd4_sumatemp2)( (void *)fd4_mkclosure( fd4_sumatemp2
    , 0 )
    , (void *)0 );
    ((void* (*) (void*, void*)) (fd4___var0)[0])((void *)fd4___var0, (void *)4);
  }));
}
void* fd4_sumatemp4 (void** fd4_clo, void** fd4_dummy) {
  return (void *)(({
    void** fd4___var0 = ((void* (*) (void*, void*)) fd4_sumatemp3)( (void *)fd4_mkclosure( fd4_sumatemp3
    , 0 )
    , (void *)0 );
    ((void* (*) (void*, void*)) (fd4___var0)[0])((void *)fd4___var0, (void *)6);
  }));
}
void* fd4_final;
uint64_t* fd4main() {
  fd4_const = (void *)(205);
  fd4_final = (void *)(({
    void** fd4___var0 = ((void* (*) (void*, void*)) fd4_sumatemp4)( (void *)fd4_mkclosure( fd4_sumatemp4
    , 0 )
    , (void *)0 );
    ((void* (*) (void*, void*)) (fd4___var0)[0])((void *)fd4___var0, (void *)7);
  }));
  fd4_printn((uint64_t)fd4_final)
  ;
  return 0;
}