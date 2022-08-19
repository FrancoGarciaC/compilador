#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

uint64_t fd4___suma0 (void**  fd4_clo, uint64_t  fd4_n) {
  return ({
    uint64_t  fd4_const = fd4_clo[5];
    ({
      uint64_t  fd4_const1 = fd4_clo[4];
      ({
        void**  fd4_suma_clo = fd4_clo[3];
        ({
          uint64_t  fd4_m = fd4_clo[2];
          ({
            void**  (* fd4_suma) (void**,  uint64_t  ) = fd4_suma_clo[0];
            fd4_n
            ? ({
                void**  fd4___clo1 = fd4_suma(fd4_suma_clo, fd4_m);
                ({
                  uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
                  fd4___var1(fd4___clo1, ({ fd4_sub(fd4_n, 1); }));
                });
              }) + 1
            : fd4_m;
          });
        });
      });
    });
  });
}
void** fd4_suma (void**  fd4_suma_clo, uint64_t  fd4_m) {
  return ({
    uint64_t  fd4_const = fd4_suma_clo[2];
    ({
      uint64_t  fd4_const1 = fd4_suma_clo[1];
      fd4_mkclosure( fd4___suma0
      , 5
      , fd4_suma
      , fd4_m
      , fd4_suma_clo
      , fd4_const1
      , fd4_const );
    });
  });
}
uint64_t fd4___prod2 (void**  fd4_clo, uint64_t  fd4_n) {
  return ({
    uint64_t  fd4_const = fd4_clo[7];
    ({
      uint64_t  fd4_const1 = fd4_clo[6];
      ({
        void**  fd4_suma_clo = fd4_clo[5];
        ({
          void**  (* fd4_suma) (void**,  uint64_t  ) = fd4_suma_clo[0];
          ({
            void**  fd4_prod_clo = fd4_clo[3];
            ({
              uint64_t  fd4_m = fd4_clo[2];
              ({
                void**  (* fd4_prod) (void**,  uint64_t  ) = fd4_prod_clo[0];
                fd4_n
                ? ({
                    void**  fd4___clo4 = fd4_suma(fd4_suma_clo, fd4_m);
                    ({
                      uint64_t  (* fd4___var4) (void**,  uint64_t  ) = fd4___clo4[0];
                      fd4___var4( fd4___clo4
                      , ({
                        void**  fd4___clo3 = fd4_prod(fd4_prod_clo, fd4_m);
                        ({
                          uint64_t  (* fd4___var3) (void**,  uint64_t  ) = fd4___clo3[0];
                          fd4___var3(fd4___clo3, ({ fd4_sub(fd4_n, 1); }));
                        });
                      }) );
                    });
                  })
                : 0;
              });
            });
          });
        });
      });
    });
  });
}
void** fd4_prod (void**  fd4_prod_clo, uint64_t  fd4_m) {
  return ({
    uint64_t  fd4_const = fd4_prod_clo[4];
    ({
      uint64_t  fd4_const1 = fd4_prod_clo[3];
      ({
        void**  fd4_suma_clo = fd4_prod_clo[2];
        ({
          void**  (* fd4_suma) (void**,  uint64_t  ) = fd4_suma_clo[0];
          fd4_mkclosure( fd4___prod2
          , 7
          , fd4_prod
          , fd4_m
          , fd4_prod_clo
          , fd4_suma
          , fd4_suma_clo
          , fd4_const1
          , fd4_const );
        });
      });
    });
  });
}
void** fd4_temp (void**  fd4_temp_clo, uint64_t  fd4_dummy) {
  return ({
    uint64_t  fd4_const = 20;
    ({
      uint64_t  fd4_const1 = 30;
      ({
        void**  fd4_suma_clo = fd4_mkclosure( fd4_suma
        , 2
        , fd4_const1
        , fd4_const );
        ({
          void**  (* fd4_suma) (void**,  uint64_t  ) = fd4_suma_clo[0];
          ({
            void**  fd4_prod_clo = fd4_mkclosure( fd4_prod
            , 4
            , fd4_suma
            , fd4_suma_clo
            , fd4_const1
            , fd4_const );
            ({
              void**  (* fd4_prod) (void**,  uint64_t  ) = fd4_prod_clo[0];
              fd4_prod_clo;
            });
          });
        });
      });
    });
  });
}
void** fd4_temp2 (void**  fd4_temp2_clo, uint64_t  fd4_dummy) {
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
    void**  fd4___clo0 = fd4_temp2(fd4_mkclosure(fd4_temp2, 0), 0);
    ({
      uint64_t  (* fd4___var0) (void**,  uint64_t  ) = fd4___clo0[0];
      fd4___var0(fd4___clo0, 3);
    });
  }));
  fd4_printn((uint64_t)fd4_final)
  ;
  return 0;
}