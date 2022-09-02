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
uint64_t fd4_l;
uint64_t fd4___foldr1 (void**  fd4_clo, void**  fd4_f_clo) {
  return ({
    void**  fd4_foldr_clo = fd4_clo[4];
    ({
      uint64_t  fd4_xs = fd4_clo[3];
      ({
        void**  (* fd4_foldr) (void**,  uint64_t  ) = fd4_foldr_clo[0];
        ({
          uint64_t  fd4_b = fd4_clo[1];
          ({
            void**  (* fd4_f) (void**,  uint64_t  ) = fd4_f_clo[0];
            fd4_xs
            ? ({
                void**  fd4___clo6 = fd4_f( fd4_f_clo
                , fd4_head(fd4_mkclosure(fd4_head, 0), fd4_xs) );
                ({
                  uint64_t  (* fd4___var6) (void**,  uint64_t  ) = fd4___clo6[0];
                  fd4___var6( fd4___clo6
                  , ({
                    void**  fd4___clo4 = ({
                      void**  fd4___clo3 = fd4_foldr( fd4_foldr_clo
                      , fd4_tail(fd4_mkclosure(fd4_tail, 0), fd4_xs) );
                      ({
                        void**  (* fd4___var3) (void**,  uint64_t  ) = fd4___clo3[0];
                        fd4___var3(fd4___clo3, fd4_b);
                      });
                    });
                    ({
                      uint64_t  (* fd4___var4) (void**,  void**  (* ) (void**,  uint64_t  ) ) = fd4___clo4[0];
                      fd4___var4(fd4___clo4, fd4_f_clo);
                    });
                  }) );
                });
              })
            : fd4_b;
          });
        });
      });
    });
  });
}
void** fd4___foldr0 (void**  fd4_clo, uint64_t  fd4_b) {
  return ({
    void**  fd4_foldr_clo = fd4_clo[3];
    ({
      uint64_t  fd4_xs = fd4_clo[2];
      ({
        void**  (* fd4_foldr) (void**,  uint64_t  ) = fd4_foldr_clo[0];
        fd4_mkclosure(fd4___foldr1, 4, fd4_b, fd4_foldr, fd4_xs, fd4_foldr_clo);
      });
    });
  });
}
void** fd4_foldr (void**  fd4_foldr_clo, uint64_t  fd4_xs) {
  return fd4_mkclosure(fd4___foldr0, 3, fd4_foldr, fd4_xs, fd4_foldr_clo);
}
uint64_t fd4_f;
uint64_t* fd4main() {
  fd4_base = (void *)(10);
  fd4_l = (void *)(({
    void**  fd4___clo5 = fd4_add(fd4_mkclosure(fd4_add, 0), 3);
    ({
      uint64_t  (* fd4___var5) (void**,  uint64_t  ) = fd4___clo5[0];
      fd4___var5( fd4___clo5
      , ({
        void**  fd4___clo3 = fd4_add(fd4_mkclosure(fd4_add, 0), 4);
        ({
          uint64_t  (* fd4___var3) (void**,  uint64_t  ) = fd4___clo3[0];
          fd4___var3( fd4___clo3
          , ({
            void**  fd4___clo1 = fd4_add(fd4_mkclosure(fd4_add, 0), 5);
            ({
              uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
              fd4___var1(fd4___clo1, 0);
            });
          }) );
        });
      }) );
    });
  }));
  fd4_f = (void *)(({
    void**  fd4___clo2 = ({
      void**  fd4___clo1 = fd4_foldr(fd4_mkclosure(fd4_foldr, 0), fd4_l);
      ({
        void**  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
        fd4___var1(fd4___clo1, 1);
      });
    });
    ({
      uint64_t  (* fd4___var2) (void**,  void**  (* ) (void**,  uint64_t  ) ) = fd4___clo2[0];
      fd4___var2(fd4___clo2, fd4_mkclosure(fd4_mult, 0));
    });
  }));
  fd4_printn((uint64_t)fd4_f)
  ;
  return 0;
}