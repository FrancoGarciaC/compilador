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
uint64_t fd4___mult0 (void**  fd4_clo, uint64_t  fd4_y) {
  return ({
    void**  fd4_mult_clo = fd4_clo[3];
    ({
      uint64_t  fd4_x = fd4_clo[2];
      ({
        void**  (* fd4_mult) (void**,  uint64_t  ) = fd4_mult_clo[0];
        fd4_y
        ? fd4_x + ({
            void**  fd4___clo2 = fd4_mult(fd4_mult_clo, fd4_x);
            ({
              uint64_t  (* fd4___var2) (void**,  uint64_t  ) = fd4___clo2[0];
              fd4___var2( fd4___clo2
              , fd4_pred(fd4_mkclosure(fd4_pred, 0), fd4_y) );
            });
          })
        : 0;
      });
    });
  });
}
void** fd4_mult (void**  fd4_mult_clo, uint64_t  fd4_x) {
  return fd4_mkclosure(fd4___mult0, 3, fd4_mult, fd4_x, fd4_mult_clo);
}
uint64_t fd4___exp0 (void**  fd4_clo, uint64_t  fd4_y) {
  return ({
    void**  fd4_exp_clo = fd4_clo[3];
    ({
      uint64_t  fd4_x = fd4_clo[2];
      ({
        void**  (* fd4_exp) (void**,  uint64_t  ) = fd4_exp_clo[0];
        fd4_y
        ? ({
            void**  fd4___clo4 = fd4_mult(fd4_mkclosure(fd4_mult, 0), fd4_x);
            ({
              uint64_t  (* fd4___var4) (void**,  uint64_t  ) = fd4___clo4[0];
              fd4___var4( fd4___clo4
              , ({
                void**  fd4___clo2 = fd4_exp(fd4_exp_clo, fd4_x);
                ({
                  uint64_t  (* fd4___var2) (void**,  uint64_t  ) = fd4___clo2[0];
                  fd4___var2( fd4___clo2
                  , fd4_pred(fd4_mkclosure(fd4_pred, 0), fd4_y) );
                });
              }) );
            });
          })
        : 1;
      });
    });
  });
}
void** fd4_exp (void**  fd4_exp_clo, uint64_t  fd4_x) {
  return fd4_mkclosure(fd4___exp0, 3, fd4_exp, fd4_x, fd4_exp_clo);
}
uint64_t fd4_res;
uint64_t* fd4main() {
  fd4_res = (void *)(({
    void**  fd4___clo1 = fd4_exp(fd4_mkclosure(fd4_exp, 0), 2);
    ({
      uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
      fd4___var1(fd4___clo1, 3);
    });
  }));
  fd4_printn((uint64_t)fd4_res)
  ;
  return 0;
}