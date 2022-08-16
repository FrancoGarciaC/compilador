#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

uint64_t fd4___temp1 (void**  fd4_clo, uint64_t  fd4_n) {
  return ({
    uint64_t  fd4_m = fd4_clo[1];
    fd4_m + fd4_n;
  });
}
void** fd4___temp0 (void**  fd4_clo, uint64_t  fd4_m) {
  return fd4_mkclosure(fd4___temp1, 1, fd4_m);
}
void** fd4_temp (void**  fd4_temp_clo, uint64_t  fd4_dummy) {
  return ({
    void**  fd4_suma_clo = fd4_mkclosure(fd4___temp0, 0);
    ({
      void**  (* fd4_suma) (void**,  uint64_t  ) = fd4_suma_clo[0];
      fd4_suma;
    });
  });
}
void** fd4_suma15 (void**  fd4_suma15_clo, uint64_t  fd4_dummy) {
  return ({
    void**  fd4___clo0 = fd4_temp(fd4_mkclosure(fd4_temp, 0), 0);
    ({
      void**  (* fd4___var0) (void**,  uint64_t  ) = fd4___clo0[0];
      fd4___var0(fd4___clo0, 15);
    });
  });
}
void* fd4_final;
uint64_t* fd4main() {
  fd4_final = (void *)(({
    void**  fd4___clo0 = fd4_suma15(fd4_mkclosure(fd4_suma15, 0), 0);
    ({
      uint64_t  (* fd4___var0) (void**,  uint64_t  ) = fd4___clo0[0];
      fd4___var0(fd4___clo0, 3);
    });
  }));
  fd4_printn((uint64_t)fd4_final)
  ;
  return 0;
}