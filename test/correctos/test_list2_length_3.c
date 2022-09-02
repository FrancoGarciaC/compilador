#include <inttypes.h>
#include <wchar.h>
extern void** fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern uint64_t fd4_sub(uint64_t, uint64_t);

uint64_t fd4_base;
uint64_t fd4_pred (void**  fd4_pred_clo, uint64_t  fd4_x) {
  return ({
    fd4_sub(fd4_x, 1);
  });
}
uint64_t fd4_op (void**  fd4_op_clo, uint64_t  fd4_x) {
  return fd4_x ? 0 : 1;
}
uint64_t fd4___eq0 (void**  fd4_clo, uint64_t  fd4_y) {
  return ({
    uint64_t  fd4_x = fd4_clo[1];
    ({ fd4_sub(fd4_x, fd4_y); }) + ({ fd4_sub(fd4_y, fd4_x); }) ? 1 : 0;
  });
}
void** fd4_eq (void**  fd4_eq_clo, uint64_t  fd4_x) {
  return fd4_mkclosure(fd4___eq0, 1, fd4_x);
}
uint64_t fd4___leq0 (void**  fd4_clo, uint64_t  fd4_y) {
  return ({
    uint64_t  fd4_x = fd4_clo[1];
    ({
      fd4_sub(fd4_x, fd4_y);
    });
  });
}
void** fd4_leq (void**  fd4_leq_clo, uint64_t  fd4_x) {
  return fd4_mkclosure(fd4___leq0, 1, fd4_x);
}
uint64_t fd4___div0 (void**  fd4_clo, uint64_t  fd4_y) {
  return ({
    void**  fd4_div_clo = fd4_clo[3];
    ({
      uint64_t  fd4_x = fd4_clo[2];
      ({
        void**  (* fd4_div) (void**,  uint64_t  ) = fd4_div_clo[0];
        ({
          void**  fd4___clo2 = fd4_eq(fd4_mkclosure(fd4_eq, 0), fd4_x);
          ({
            uint64_t  (* fd4___var2) (void**,  uint64_t  ) = fd4___clo2[0];
            fd4___var2(fd4___clo2, fd4_y);
          });
        })
        ? ({
            fd4_sub(fd4_x, fd4_y);
          })
          ? 1 + ({
              void**  fd4___clo3 = fd4_div( fd4_div_clo
              , ({
                fd4_sub(fd4_x, fd4_y);
              }) );
              ({
                uint64_t  (* fd4___var3) (void**,  uint64_t  ) = fd4___clo3[0];
                fd4___var3(fd4___clo3, fd4_y);
              });
            })
          : 0
        : 1;
      });
    });
  });
}
void** fd4_div (void**  fd4_div_clo, uint64_t  fd4_x) {
  return fd4_mkclosure(fd4___div0, 3, fd4_div, fd4_x, fd4_div_clo);
}
uint64_t fd4___mod0 (void**  fd4_clo, uint64_t  fd4_y) {
  return ({
    void**  fd4_mod_clo = fd4_clo[3];
    ({
      uint64_t  fd4_x = fd4_clo[2];
      ({
        void**  (* fd4_mod) (void**,  uint64_t  ) = fd4_mod_clo[0];
        ({
          void**  fd4___clo2 = fd4_eq(fd4_mkclosure(fd4_eq, 0), fd4_x);
          ({
            uint64_t  (* fd4___var2) (void**,  uint64_t  ) = fd4___clo2[0];
            fd4___var2(fd4___clo2, fd4_y);
          });
        })
        ? ({
            fd4_sub(fd4_x, fd4_y);
          })
          ? ({
              void**  fd4___clo3 = fd4_mod( fd4_mod_clo
              , ({
                fd4_sub(fd4_x, fd4_y);
              }) );
              ({
                uint64_t  (* fd4___var3) (void**,  uint64_t  ) = fd4___clo3[0];
                fd4___var3(fd4___clo3, fd4_y);
              });
            })
          : fd4_x
        : 0;
      });
    });
  });
}
void** fd4_mod (void**  fd4_mod_clo, uint64_t  fd4_x) {
  return fd4_mkclosure(fd4___mod0, 3, fd4_mod, fd4_x, fd4_mod_clo);
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
uint64_t fd4_head (void**  fd4_head_clo, uint64_t  fd4_xs) {
  return ({
    void**  fd4___clo1 = fd4_mod(fd4_mkclosure(fd4_mod, 0), fd4_xs);
    ({
      uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
      fd4___var1(fd4___clo1, fd4_base);
    });
  });
}
uint64_t fd4_tail (void**  fd4_tail_clo, uint64_t  fd4_xs) {
  return ({
    void**  fd4___clo1 = fd4_div(fd4_mkclosure(fd4_div, 0), fd4_xs);
    ({
      uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
      fd4___var1(fd4___clo1, fd4_base);
    });
  });
}
uint64_t fd4___add0 (void**  fd4_clo, uint64_t  fd4_xs) {
  return ({
    uint64_t  fd4_n = fd4_clo[1];
    ({
      void**  fd4___clo2 = fd4_mult(fd4_mkclosure(fd4_mult, 0), fd4_xs);
      ({
        uint64_t  (* fd4___var2) (void**,  uint64_t  ) = fd4___clo2[0];
        fd4___var2(fd4___clo2, fd4_base);
      });
    }) + fd4_n;
  });
}
void** fd4_add (void**  fd4_add_clo, uint64_t  fd4_n) {
  return fd4_mkclosure(fd4___add0, 1, fd4_n);
}
uint64_t fd4_len (void**  fd4_len_clo, uint64_t  fd4_xs) {
  return fd4_xs
  ? 1 + fd4_len(fd4_len_clo, fd4_tail(fd4_mkclosure(fd4_tail, 0), fd4_xs))
  : 0;
}
uint64_t fd4_l;
uint64_t fd4_length;
uint64_t* fd4main() {
  fd4_base = (void *)(10);
  fd4_l = (void *)(({
    void**  fd4___clo5 = fd4_add(fd4_mkclosure(fd4_add, 0), 6);
    ({
      uint64_t  (* fd4___var5) (void**,  uint64_t  ) = fd4___clo5[0];
      fd4___var5( fd4___clo5
      , ({
        void**  fd4___clo3 = fd4_add(fd4_mkclosure(fd4_add, 0), 7);
        ({
          uint64_t  (* fd4___var3) (void**,  uint64_t  ) = fd4___clo3[0];
          fd4___var3( fd4___clo3
          , ({
            void**  fd4___clo1 = fd4_add(fd4_mkclosure(fd4_add, 0), 8);
            ({
              uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
              fd4___var1(fd4___clo1, 0);
            });
          }) );
        });
      }) );
    });
  }));
  fd4_length = (void *)(fd4_len(fd4_mkclosure(fd4_len, 0), fd4_l));
  fd4_printn((uint64_t)fd4_length)
  ;
  return 0;
}