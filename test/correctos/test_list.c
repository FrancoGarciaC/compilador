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
uint64_t fd4_l;
uint64_t fd4_h;
uint64_t fd4_len (void**  fd4_len_clo, uint64_t  fd4_xs) {
  return fd4_xs
  ? 1 + fd4_len(fd4_len_clo, fd4_tail(fd4_mkclosure(fd4_tail, 0), fd4_xs))
  : 0;
}
uint64_t fd4_sum (void**  fd4_sum_clo, uint64_t  fd4_xs) {
  return fd4_xs
  ? fd4_head(fd4_mkclosure(fd4_head, 0), fd4_xs) + fd4_sum( fd4_sum_clo
    , fd4_tail(fd4_mkclosure(fd4_tail, 0), fd4_xs) )
  : 0;
}
uint64_t fd4___map0 (void**  fd4_clo, uint64_t  fd4_xs) {
  return ({
    void**  fd4_map_clo = fd4_clo[3];
    ({
      void**  fd4_f_clo = fd4_clo[2];
      ({
        uint64_t  (* fd4_f) (void**,  uint64_t  ) = fd4_f_clo[0];
        ({
          void**  (* fd4_map) (void**,  uint64_t  (* ) (void**,  uint64_t  ) ) = fd4_map_clo[0];
          fd4_xs
          ? ({
              void**  fd4___clo5 = fd4_add( fd4_mkclosure(fd4_add, 0)
              , fd4_f( fd4_f_clo
              , fd4_head(fd4_mkclosure(fd4_head, 0), fd4_xs) ) );
              ({
                uint64_t  (* fd4___var5) (void**,  uint64_t  ) = fd4___clo5[0];
                fd4___var5( fd4___clo5
                , ({
                  void**  fd4___clo2 = fd4_map(fd4_map_clo, fd4_f_clo);
                  ({
                    uint64_t  (* fd4___var2) (void**,  uint64_t  ) = fd4___clo2[0];
                    fd4___var2( fd4___clo2
                    , fd4_tail(fd4_mkclosure(fd4_tail, 0), fd4_xs) );
                  });
                }) );
              });
            })
          : 0;
        });
      });
    });
  });
}
void** fd4_map ( void**  fd4_map_clo
, uint64_t  (* fd4_f) (void**,  uint64_t  ) ) {
  return fd4_mkclosure(fd4___map0, 3, fd4_map, fd4_f, fd4_map_clo);
}
uint64_t fd4_sm;
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
uint64_t fd4___splitOddAux0 (void**  fd4_clo, uint64_t  fd4_p) {
  return ({
    void**  fd4_splitOddAux_clo = fd4_clo[3];
    ({
      uint64_t  fd4_xs = fd4_clo[2];
      ({
        void**  (* fd4_splitOddAux) (void**,  uint64_t  ) = fd4_splitOddAux_clo[0];
        fd4_xs
        ? fd4_p
          ? ({
              void**  fd4___clo9 = fd4_splitOddAux( fd4_splitOddAux_clo
              , fd4_tail(fd4_mkclosure(fd4_tail, 0), fd4_xs) );
              ({
                uint64_t  (* fd4___var9) (void**,  uint64_t  ) = fd4___clo9[0];
                fd4___var9(fd4___clo9, fd4_op(fd4_mkclosure(fd4_op, 0), fd4_p));
              });
            })
          : ({
              void**  fd4___clo6 = fd4_add( fd4_mkclosure(fd4_add, 0)
              , fd4_head(fd4_mkclosure(fd4_head, 0), fd4_xs) );
              ({
                uint64_t  (* fd4___var6) (void**,  uint64_t  ) = fd4___clo6[0];
                fd4___var6( fd4___clo6
                , ({
                  void**  fd4___clo3 = fd4_splitOddAux( fd4_splitOddAux_clo
                  , fd4_tail(fd4_mkclosure(fd4_tail, 0), fd4_xs) );
                  ({
                    uint64_t  (* fd4___var3) (void**,  uint64_t  ) = fd4___clo3[0];
                    fd4___var3( fd4___clo3
                    , fd4_op(fd4_mkclosure(fd4_op, 0), fd4_p) );
                  });
                }) );
              });
            })
        : 0;
      });
    });
  });
}
void** fd4_splitOddAux (void**  fd4_splitOddAux_clo, uint64_t  fd4_xs) {
  return fd4_mkclosure( fd4___splitOddAux0
  , 3
  , fd4_splitOddAux
  , fd4_xs
  , fd4_splitOddAux_clo );
}
uint64_t fd4_splitOdd (void**  fd4_splitOdd_clo, uint64_t  fd4_xs) {
  return ({
    void**  fd4___clo1 = fd4_splitOddAux( fd4_mkclosure(fd4_splitOddAux, 0)
    , fd4_xs );
    ({
      uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
      fd4___var1(fd4___clo1, 0);
    });
  });
}
uint64_t fd4_splitEven (void**  fd4_splitEven_clo, uint64_t  fd4_xs) {
  return ({
    void**  fd4___clo1 = fd4_splitOddAux( fd4_mkclosure(fd4_splitOddAux, 0)
    , fd4_xs );
    ({
      uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
      fd4___var1(fd4___clo1, 1);
    });
  });
}
uint64_t fd4___join0 (void**  fd4_clo, uint64_t  fd4_xs) {
  return ({
    void**  fd4_join_clo = fd4_clo[3];
    ({
      uint64_t  fd4_ys = fd4_clo[2];
      ({
        void**  (* fd4_join) (void**,  uint64_t  ) = fd4_join_clo[0];
        fd4_ys
        ? fd4_xs
          ? ({
              void**  fd4___clo4 = fd4_leq( fd4_mkclosure(fd4_leq, 0)
              , fd4_head(fd4_mkclosure(fd4_head, 0), fd4_xs) );
              ({
                uint64_t  (* fd4___var4) (void**,  uint64_t  ) = fd4___clo4[0];
                fd4___var4( fd4___clo4
                , fd4_head(fd4_mkclosure(fd4_head, 0), fd4_ys) );
              });
            })
            ? ({
                void**  fd4___clo14 = fd4_add( fd4_mkclosure(fd4_add, 0)
                , fd4_head(fd4_mkclosure(fd4_head, 0), fd4_ys) );
                ({
                  uint64_t  (* fd4___var14) (void**,  uint64_t  ) = fd4___clo14[0];
                  fd4___var14( fd4___clo14
                  , ({
                    void**  fd4___clo11 = fd4_join( fd4_join_clo
                    , fd4_tail(fd4_mkclosure(fd4_tail, 0), fd4_ys) );
                    ({
                      uint64_t  (* fd4___var11) (void**,  uint64_t  ) = fd4___clo11[0];
                      fd4___var11(fd4___clo11, fd4_xs);
                    });
                  }) );
                });
              })
            : ({
                void**  fd4___clo9 = fd4_add( fd4_mkclosure(fd4_add, 0)
                , fd4_head(fd4_mkclosure(fd4_head, 0), fd4_xs) );
                ({
                  uint64_t  (* fd4___var9) (void**,  uint64_t  ) = fd4___clo9[0];
                  fd4___var9( fd4___clo9
                  , ({
                    void**  fd4___clo6 = fd4_join(fd4_join_clo, fd4_ys);
                    ({
                      uint64_t  (* fd4___var6) (void**,  uint64_t  ) = fd4___clo6[0];
                      fd4___var6( fd4___clo6
                      , fd4_tail(fd4_mkclosure(fd4_tail, 0), fd4_xs) );
                    });
                  }) );
                });
              })
          : fd4_ys
        : fd4_xs;
      });
    });
  });
}
void** fd4_join (void**  fd4_join_clo, uint64_t  fd4_ys) {
  return fd4_mkclosure(fd4___join0, 3, fd4_join, fd4_ys, fd4_join_clo);
}
uint64_t fd4_mergeSort (void**  fd4_mergeSort_clo, uint64_t  fd4_xs) {
  return fd4_xs
  ? ({
      void**  fd4___clo2 = fd4_eq( fd4_mkclosure(fd4_eq, 0)
      , fd4_len(fd4_mkclosure(fd4_len, 0), fd4_xs) );
      ({
        uint64_t  (* fd4___var2) (void**,  uint64_t  ) = fd4___clo2[0];
        fd4___var2(fd4___clo2, 1);
      });
    })
    ? ({
        void**  fd4___clo6 = fd4_join( fd4_mkclosure(fd4_join, 0)
        , fd4_mergeSort( fd4_mergeSort_clo
        , fd4_splitOdd(fd4_mkclosure(fd4_splitOdd, 0), fd4_xs) ) );
        ({
          uint64_t  (* fd4___var6) (void**,  uint64_t  ) = fd4___clo6[0];
          fd4___var6( fd4___clo6
          , fd4_mergeSort( fd4_mergeSort_clo
          , fd4_splitEven(fd4_mkclosure(fd4_splitEven, 0), fd4_xs) ) );
        });
      })
    : fd4_xs
  : 0;
}
uint64_t fd4_l2;
uint64_t fd4_xs1;
uint64_t fd4_xs2;
uint64_t fd4_xs22;
uint64_t fd4_xs3;
uint64_t fd4_xs4;
uint64_t fd4___append0 (void**  fd4_clo, uint64_t  fd4_ys) {
  return ({
    void**  fd4_append_clo = fd4_clo[3];
    ({
      uint64_t  fd4_xs = fd4_clo[2];
      ({
        void**  (* fd4_append) (void**,  uint64_t  ) = fd4_append_clo[0];
        fd4_xs
        ? ({
            void**  fd4___clo5 = fd4_add( fd4_mkclosure(fd4_add, 0)
            , fd4_head(fd4_mkclosure(fd4_head, 0), fd4_xs) );
            ({
              uint64_t  (* fd4___var5) (void**,  uint64_t  ) = fd4___clo5[0];
              fd4___var5( fd4___clo5
              , ({
                void**  fd4___clo2 = fd4_append( fd4_append_clo
                , fd4_tail(fd4_mkclosure(fd4_tail, 0), fd4_xs) );
                ({
                  uint64_t  (* fd4___var2) (void**,  uint64_t  ) = fd4___clo2[0];
                  fd4___var2(fd4___clo2, fd4_ys);
                });
              }) );
            });
          })
        : fd4_ys;
      });
    });
  });
}
void** fd4_append (void**  fd4_append_clo, uint64_t  fd4_xs) {
  return fd4_mkclosure(fd4___append0, 3, fd4_append, fd4_xs, fd4_append_clo);
}
uint64_t fd4___reverseAux0 (void**  fd4_clo, uint64_t  fd4_ys) {
  return ({
    void**  fd4_reverseAux_clo = fd4_clo[3];
    ({
      uint64_t  fd4_xs = fd4_clo[2];
      ({
        void**  (* fd4_reverseAux) (void**,  uint64_t  ) = fd4_reverseAux_clo[0];
        fd4_xs
        ? ({
            void**  fd4___clo5 = fd4_reverseAux( fd4_reverseAux_clo
            , fd4_tail(fd4_mkclosure(fd4_tail, 0), fd4_xs) );
            ({
              uint64_t  (* fd4___var5) (void**,  uint64_t  ) = fd4___clo5[0];
              fd4___var5( fd4___clo5
              , ({
                void**  fd4___clo3 = fd4_add( fd4_mkclosure(fd4_add, 0)
                , fd4_head(fd4_mkclosure(fd4_head, 0), fd4_xs) );
                ({
                  uint64_t  (* fd4___var3) (void**,  uint64_t  ) = fd4___clo3[0];
                  fd4___var3(fd4___clo3, fd4_ys);
                });
              }) );
            });
          })
        : fd4_ys;
      });
    });
  });
}
void** fd4_reverseAux (void**  fd4_reverseAux_clo, uint64_t  fd4_xs) {
  return fd4_mkclosure( fd4___reverseAux0
  , 3
  , fd4_reverseAux
  , fd4_xs
  , fd4_reverseAux_clo );
}
uint64_t fd4_reverse (void**  fd4_reverse_clo, uint64_t  fd4_xs) {
  return ({
    void**  fd4___clo1 = fd4_reverseAux( fd4_mkclosure(fd4_reverseAux, 0)
    , fd4_xs );
    ({
      uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
      fd4___var1(fd4___clo1, 0);
    });
  });
}
uint64_t fd4_sorted;
uint64_t* fd4main() {
  fd4_base = (void *)(10);
  fd4_l = (void *)(({
    void**  fd4___clo5 = fd4_add(fd4_mkclosure(fd4_add, 0), 1);
    ({
      uint64_t  (* fd4___var5) (void**,  uint64_t  ) = fd4___clo5[0];
      fd4___var5( fd4___clo5
      , ({
        void**  fd4___clo3 = fd4_add(fd4_mkclosure(fd4_add, 0), 2);
        ({
          uint64_t  (* fd4___var3) (void**,  uint64_t  ) = fd4___clo3[0];
          fd4___var3( fd4___clo3
          , ({
            void**  fd4___clo1 = fd4_add(fd4_mkclosure(fd4_add, 0), 3);
            ({
              uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
              fd4___var1(fd4___clo1, 0);
            });
          }) );
        });
      }) );
    });
  }));
  fd4_h = (void *)(fd4_head( fd4_mkclosure(fd4_head, 0)
  , fd4_tail(fd4_mkclosure(fd4_tail, 0), fd4_l) ));
  fd4_sm = (void *)(fd4_sum( fd4_mkclosure(fd4_sum, 0)
  , ({
    void**  fd4___clo2 = fd4_map( fd4_mkclosure(fd4_map, 0)
    , fd4_mult(fd4_mkclosure(fd4_mult, 0), 4) );
    ({
      uint64_t  (* fd4___var2) (void**,  uint64_t  ) = fd4___clo2[0];
      fd4___var2(fd4___clo2, fd4_l);
    });
  }) ));
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
  fd4_l2 = (void *)(({
    void**  fd4___clo5 = fd4_add(fd4_mkclosure(fd4_add, 0), 5);
    ({
      uint64_t  (* fd4___var5) (void**,  uint64_t  ) = fd4___clo5[0];
      fd4___var5( fd4___clo5
      , ({
        void**  fd4___clo3 = fd4_add(fd4_mkclosure(fd4_add, 0), 4);
        ({
          uint64_t  (* fd4___var3) (void**,  uint64_t  ) = fd4___clo3[0];
          fd4___var3( fd4___clo3
          , ({
            void**  fd4___clo1 = fd4_add(fd4_mkclosure(fd4_add, 0), 3);
            ({
              uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
              fd4___var1(fd4___clo1, 0);
            });
          }) );
        });
      }) );
    });
  }));
  fd4_xs1 = (void *)(({
    void**  fd4___clo9 = fd4_add(fd4_mkclosure(fd4_add, 0), 5);
    ({
      uint64_t  (* fd4___var9) (void**,  uint64_t  ) = fd4___clo9[0];
      fd4___var9( fd4___clo9
      , ({
        void**  fd4___clo7 = fd4_add(fd4_mkclosure(fd4_add, 0), 4);
        ({
          uint64_t  (* fd4___var7) (void**,  uint64_t  ) = fd4___clo7[0];
          fd4___var7( fd4___clo7
          , ({
            void**  fd4___clo5 = fd4_add(fd4_mkclosure(fd4_add, 0), 3);
            ({
              uint64_t  (* fd4___var5) (void**,  uint64_t  ) = fd4___clo5[0];
              fd4___var5( fd4___clo5
              , ({
                void**  fd4___clo3 = fd4_add(fd4_mkclosure(fd4_add, 0), 2);
                ({
                  uint64_t  (* fd4___var3) (void**,  uint64_t  ) = fd4___clo3[0];
                  fd4___var3( fd4___clo3
                  , ({
                    void**  fd4___clo1 = fd4_add(fd4_mkclosure(fd4_add, 0), 1);
                    ({
                      uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
                      fd4___var1(fd4___clo1, 0);
                    });
                  }) );
                });
              }) );
            });
          }) );
        });
      }) );
    });
  }));
  fd4_xs2 = (void *)(({
    void**  fd4___clo9 = fd4_add(fd4_mkclosure(fd4_add, 0), 3);
    ({
      uint64_t  (* fd4___var9) (void**,  uint64_t  ) = fd4___clo9[0];
      fd4___var9( fd4___clo9
      , ({
        void**  fd4___clo7 = fd4_add(fd4_mkclosure(fd4_add, 0), 5);
        ({
          uint64_t  (* fd4___var7) (void**,  uint64_t  ) = fd4___clo7[0];
          fd4___var7( fd4___clo7
          , ({
            void**  fd4___clo5 = fd4_add(fd4_mkclosure(fd4_add, 0), 2);
            ({
              uint64_t  (* fd4___var5) (void**,  uint64_t  ) = fd4___clo5[0];
              fd4___var5( fd4___clo5
              , ({
                void**  fd4___clo3 = fd4_add(fd4_mkclosure(fd4_add, 0), 7);
                ({
                  uint64_t  (* fd4___var3) (void**,  uint64_t  ) = fd4___clo3[0];
                  fd4___var3( fd4___clo3
                  , ({
                    void**  fd4___clo1 = fd4_add(fd4_mkclosure(fd4_add, 0), 9);
                    ({
                      uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
                      fd4___var1(fd4___clo1, 0);
                    });
                  }) );
                });
              }) );
            });
          }) );
        });
      }) );
    });
  }));
  fd4_xs22 = (void *)(({
    void**  fd4___clo7 = fd4_add(fd4_mkclosure(fd4_add, 0), 23);
    ({
      uint64_t  (* fd4___var7) (void**,  uint64_t  ) = fd4___clo7[0];
      fd4___var7( fd4___clo7
      , ({
        void**  fd4___clo5 = fd4_add(fd4_mkclosure(fd4_add, 0), 12);
        ({
          uint64_t  (* fd4___var5) (void**,  uint64_t  ) = fd4___clo5[0];
          fd4___var5( fd4___clo5
          , ({
            void**  fd4___clo3 = fd4_add(fd4_mkclosure(fd4_add, 0), 4);
            ({
              uint64_t  (* fd4___var3) (void**,  uint64_t  ) = fd4___clo3[0];
              fd4___var3( fd4___clo3
              , ({
                void**  fd4___clo1 = fd4_add(fd4_mkclosure(fd4_add, 0), 2);
                ({
                  uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
                  fd4___var1(fd4___clo1, 0);
                });
              }) );
            });
          }) );
        });
      }) );
    });
  }));
  fd4_xs3 = (void *)(({
    void**  fd4___clo5 = fd4_add(fd4_mkclosure(fd4_add, 0), 3);
    ({
      uint64_t  (* fd4___var5) (void**,  uint64_t  ) = fd4___clo5[0];
      fd4___var5( fd4___clo5
      , ({
        void**  fd4___clo3 = fd4_add(fd4_mkclosure(fd4_add, 0), 2);
        ({
          uint64_t  (* fd4___var3) (void**,  uint64_t  ) = fd4___clo3[0];
          fd4___var3( fd4___clo3
          , ({
            void**  fd4___clo1 = fd4_add(fd4_mkclosure(fd4_add, 0), 1);
            ({
              uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
              fd4___var1(fd4___clo1, 0);
            });
          }) );
        });
      }) );
    });
  }));
  fd4_xs4 = (void *)(({
    void**  fd4___clo3 = fd4_add(fd4_mkclosure(fd4_add, 0), 2);
    ({
      uint64_t  (* fd4___var3) (void**,  uint64_t  ) = fd4___clo3[0];
      fd4___var3( fd4___clo3
      , ({
        void**  fd4___clo1 = fd4_add(fd4_mkclosure(fd4_add, 0), 1);
        ({
          uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
          fd4___var1(fd4___clo1, 0);
        });
      }) );
    });
  }));
  fd4_sorted = (void *)(fd4_mergeSort( fd4_mkclosure(fd4_mergeSort, 0)
  , fd4_xs2 ));
  fd4_printn((uint64_t)fd4_sorted)
  ;
  return 0;
}