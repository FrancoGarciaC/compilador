#include <inttypes.h>
#include <wchar.h>
extern void** fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern uint64_t fd4_sub(uint64_t, uint64_t);

uint64_t fd4___gcd0 (void**  fd4_clo, uint64_t  fd4_m) {
  return ({
    void**  fd4_gcd_clo = fd4_clo[3];
    ({
      uint64_t  fd4_n = fd4_clo[2];
      ({
        void**  (* fd4_gcd) (void**,  uint64_t  ) = fd4_gcd_clo[0];
        fd4_n
        ? fd4_m
          ? ({
              fd4_sub(fd4_n, fd4_m);
            })
            ? ({
                void**  fd4___clo2 = fd4_gcd(fd4_gcd_clo, fd4_m);
                ({
                  uint64_t  (* fd4___var2) (void**,  uint64_t  ) = fd4___clo2[0];
                  fd4___var2(fd4___clo2, ({ fd4_sub(fd4_n, fd4_m); }));
                });
              })
            : ({
                void**  fd4___clo1 = fd4_gcd( fd4_gcd_clo
                , ({
                  fd4_sub(fd4_m, fd4_n);
                }) );
                ({
                  uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
                  fd4___var1(fd4___clo1, fd4_n);
                });
              })
          : fd4_n
        : fd4_m;
      });
    });
  });
}
void** fd4_gcd (void**  fd4_gcd_clo, uint64_t  fd4_n) {
  return fd4_mkclosure(fd4___gcd0, 3, fd4_gcd, fd4_n, fd4_gcd_clo);
}
uint64_t fd4_res;
uint64_t* fd4main() {
  fd4_res = (void *)(({
    void**  fd4___clo1 = fd4_gcd(fd4_mkclosure(fd4_gcd, 0), 12);
    ({
      uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
      fd4___var1(fd4___clo1, 1234);
    });
  }));
  fd4_printn((uint64_t)fd4_res)
  ;
  return 0;
}