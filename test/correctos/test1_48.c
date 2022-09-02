#include <inttypes.h>
#include <wchar.h>
extern void** fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern uint64_t fd4_sub(uint64_t, uint64_t);

uint64_t fd4_pred (void**  fd4_pred_clo, uint64_t  fd4_x) {
  return ({
    fd4_sub(fd4_x, 1);
  });
}
uint64_t fd4___mult0 (void**  fd4_clo, uint64_t  fd4_n) {
  return ({
    void**  fd4_mult_clo = fd4_clo[3];
    ({
      uint64_t  fd4_m = fd4_clo[2];
      ({
        void**  (* fd4_mult) (void**,  uint64_t  ) = fd4_mult_clo[0];
        fd4_m
        ? fd4_n
          ? fd4_pred(fd4_mkclosure(fd4_pred, 0), fd4_n)
            ? fd4_m + ({
                void**  fd4___clo3 = fd4_mult(fd4_mult_clo, fd4_m);
                ({
                  uint64_t  (* fd4___var3) (void**,  uint64_t  ) = fd4___clo3[0];
                  fd4___var3( fd4___clo3
                  , fd4_pred(fd4_mkclosure(fd4_pred, 0), fd4_n) );
                });
              })
            : fd4_m
          : 0
        : 0;
      });
    });
  });
}
void** fd4_mult (void**  fd4_mult_clo, uint64_t  fd4_m) {
  return fd4_mkclosure(fd4___mult0, 3, fd4_mult, fd4_m, fd4_mult_clo);
}
uint64_t fd4_b;
uint64_t fd4_d;
uint64_t fd4_e;
uint64_t fd4_c;
uint64_t fd4___g1 (void**  fd4_clo, uint64_t  fd4_m) {
  return ({
    uint64_t  fd4_k = fd4_clo[2];
    ({
      uint64_t  fd4_l = fd4_clo[1];
      fd4_k + 2 + fd4_l + fd4_m + 3 + fd4_b + ({
        void**  fd4___clo3 = fd4_mult(fd4_mkclosure(fd4_mult, 0), 4);
        ({
          uint64_t  (* fd4___var3) (void**,  uint64_t  ) = fd4___clo3[0];
          fd4___var3(fd4___clo3, 5);
        });
      });
    });
  });
}
void** fd4___g0 (void**  fd4_clo, uint64_t  fd4_l) {
  return ({
    uint64_t  fd4_k = fd4_clo[1];
    fd4_mkclosure(fd4___g1, 2, fd4_l, fd4_k);
  });
}
void** fd4_g (void**  fd4_g_clo, uint64_t  fd4_k) {
  return fd4_mkclosure(fd4___g0, 1, fd4_k);
}
uint64_t fd4___f0 (void**  fd4_clo, uint64_t  fd4_y) {
  return ({
    uint64_t  fd4_x = fd4_clo[1];
    ({
      void**  fd4___clo3 = ({
        void**  fd4___clo2 = fd4_g(fd4_mkclosure(fd4_g, 0), fd4_x + 1);
        ({
          void**  (* fd4___var2) (void**,  uint64_t  ) = fd4___clo2[0];
          fd4___var2(fd4___clo2, fd4_y);
        });
      });
      ({
        uint64_t  (* fd4___var3) (void**,  uint64_t  ) = fd4___clo3[0];
        fd4___var3(fd4___clo3, 0 ? 3 : 5);
      });
    });
  });
}
void** fd4_f (void**  fd4_f_clo, uint64_t  fd4_x) {
  return fd4_mkclosure(fd4___f0, 1, fd4_x);
}
uint64_t fd4_a;
uint64_t* fd4main() {
  fd4_b = (void *)(8 + 2);
  fd4_d = (void *)(0);
  fd4_e = (void *)(0);
  fd4_c = (void *)(0 ? 3 : 5);
  fd4_a = (void *)(({
    void**  fd4___clo1 = fd4_f(fd4_mkclosure(fd4_f, 0), 3);
    ({
      uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
      fd4___var1(fd4___clo1, 4);
    });
  }));
  fd4_printn((uint64_t)fd4_a)
  ;
  return 0;
}