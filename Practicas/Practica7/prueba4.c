#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

uint64_t fd4___prod0 (void**  fd4_clo, uint64_t  fd4_n) {
  return ({
    void**  fd4_prod_clo = fd4_clo[3];
    ({
      uint64_t  fd4_m = fd4_clo[2];
      ({
        void**  (* fd4_prod) (void**,  uint64_t  ) = fd4_prod_clo[0];
        fd4_n
        ? fd4_m + ({
            void**  fd4___clo1 = fd4_prod(fd4_prod_clo, fd4_m);
            ({
              uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
              fd4___var1(fd4___clo1, ({ fd4_sub(fd4_n, 1); }));
            });
          })
        : 0;
      });
    });
  });
}
void** fd4_prod (void**  fd4_prod_clo, uint64_t  fd4_m) {
  return fd4_mkclosure(fd4___prod0, 3, fd4_prod, fd4_m, fd4_prod_clo);
}
uint64_t fd4_fact (void**  fd4_fact_clo, uint64_t  fd4_x) {
  return fd4_x
  ? ({
      void**  fd4___clo1 = fd4_prod(fd4_mkclosure(fd4_prod, 0), fd4_x);
      ({
        uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
        fd4___var1( fd4___clo1
        , fd4_fact(fd4_fact_clo, ({ fd4_sub(fd4_x, 1); })) );
      });
    })
  : 1;
}
void* fd4_final;
uint64_t* fd4main() {
  fd4_final = (void *)(fd4_fact(fd4_mkclosure(fd4_fact, 0), 5));
  fd4_printn((uint64_t)fd4_final)
  ;
  return 0;
}