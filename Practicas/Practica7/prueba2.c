#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

uint64_t fd4___map0 (void**  fd4_clo, void**  fd4_f_clo) {
  return ({
    uint64_t  fd4_n = fd4_clo[1];
    ({
      uint64_t  (* fd4_f) (void**,  uint64_t  ) = fd4_f_clo[0];
      fd4_f(fd4_f_clo, fd4_n);
    });
  });
}
void** fd4_map (void**  fd4_map_clo, uint64_t  fd4_n) {
  return fd4_mkclosure(fd4___map0, 1, fd4_n);
}
uint64_t fd4_duplicador (void**  fd4_duplicador_clo, uint64_t  fd4_n) {
  return fd4_n + fd4_n;
}
void* fd4_final;
uint64_t* fd4main() {
  fd4_final = (void *)(({
    void**  fd4___clo1 = fd4_map(fd4_mkclosure(fd4_map, 0), 3);
    ({
      uint64_t  (* fd4___var1) (void**,  uint64_t  (* ) (void**,  uint64_t  ) ) = fd4___clo1[0];
      fd4___var1(fd4___clo1, fd4_mkclosure(fd4_duplicador, 0));
    });
  }));
  fd4_printn((uint64_t)fd4_final)
  ;
  return 0;
}