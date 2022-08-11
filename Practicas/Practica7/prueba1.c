#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

uint64_t fd4___suma0 (void**  fd4_clo, uint64_t  fd4_n) {
  return ({
    uint64_t  fd4_m = fd4_clo[2];
    ({
      void**  fd4_suma_clo = fd4_clo[1];
      ({
        void**  (*  fd4_suma ) (void**,  uint64_t  ) = fd4_suma_clo[0];
        fd4_n
        ? ({
            void**  fd4___clo1 = fd4_suma(fd4_suma_clo, fd4_m);
            ({
              uint64_t  (*  fd4___var1 ) (void**,  uint64_t  ) = fd4___clo1[0];
              fd4___var1(fd4___clo1, ({ fd4_sub(fd4_n, 1); }));
            });
          }) + 1
        : fd4_m;
      });
    });
  });
}
void** fd4_suma (void**  fd4_suma, uint64_t  fd4_m) {
  return fd4_mkclosure(fd4___suma0, 2, fd4_suma, fd4_m);
}
void* fd4_final;
uint64_t* fd4main() {
  fd4_final = (void *)(({
    void**  fd4___clo1 = fd4_suma(fd4_mkclosure(fd4_suma, 0), 200);
    ({
      uint64_t  (*  fd4___var1 ) (void**,  uint64_t  ) = fd4___clo1[0];
      fd4___var1(fd4___clo1, 300);
    });
  }));
  fd4_printn((uint64_t)fd4_final)
  ;
  return 0;
}