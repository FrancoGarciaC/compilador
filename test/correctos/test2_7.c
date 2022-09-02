#include <inttypes.h>
#include <wchar.h>
extern void** fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern uint64_t fd4_sub(uint64_t, uint64_t);

uint64_t fd4_x;
uint64_t fd4_y;
uint64_t fd4_f (void**  fd4_f_clo, uint64_t  fd4_y) {
  return 1 + fd4_x;
}
uint64_t fd4___suma0 (void**  fd4_clo, uint64_t  fd4_y) {
  return ({
    uint64_t  fd4_x = fd4_clo[1];
    fd4_x + fd4_y;
  });
}
void** fd4_suma (void**  fd4_suma_clo, uint64_t  fd4_x) {
  return fd4_mkclosure(fd4___suma0, 1, fd4_x);
}
void** fd4_suma5 (void**  fd4_suma5_clo, uint64_t  fd4_dummy) {
  return fd4_suma(fd4_mkclosure(fd4_suma, 0), 5);
}
uint64_t fd4_res;
uint64_t* fd4main() {
  fd4_x = (void *)(({
    wprintf(L"Def x = ");
    fd4_printn((uint64_t)1);
  }));
  fd4_y = (void *)(({
    wprintf(L"Def y = ");
    fd4_printn((uint64_t)2 + fd4_x);
  }));
  fd4_res = (void *)(({
    wprintf(L"Resultado es ");
    fd4_printn((uint64_t)({
      void**  fd4___clo0 = fd4_suma5(fd4_mkclosure(fd4_suma5, 0), 0);
      ({
        uint64_t  (* fd4___var0) (void**,  uint64_t  ) = fd4___clo0[0];
        fd4___var0(fd4___clo0, 2);
      });
    }));
  }));
  fd4_printn((uint64_t)fd4_res)
  ;
  return 0;
}