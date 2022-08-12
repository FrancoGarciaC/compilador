#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

uint64_t fd4___map3 (void**  fd4_clo, void**  fd4_f_clo) {
  return ({
    uint64_t  fd4_n = fd4_clo[4];
    ({
      uint64_t  fd4_m = fd4_clo[3];
      ({
        uint64_t  fd4_o = fd4_clo[2];
        ({
          uint64_t  fd4_p = fd4_clo[1];
          ({
            void**  (* fd4_f) (void**,  uint64_t  ) = fd4_f_clo[0];
            ({
              void**  fd4___clo6 = ({
                void**  fd4___clo5 = ({
                  void**  fd4___clo4 = fd4_f(fd4_f_clo, fd4_n);
                  ({
                    void**  (* fd4___var4) (void**,  uint64_t  ) = fd4___clo4[0];
                    fd4___var4(fd4___clo4, fd4_m);
                  });
                });
                ({
                  void**  (* fd4___var5) (void**,  uint64_t  ) = fd4___clo5[0];
                  fd4___var5(fd4___clo5, fd4_o);
                });
              });
              ({
                uint64_t  (* fd4___var6) (void**,  uint64_t  ) = fd4___clo6[0];
                fd4___var6(fd4___clo6, fd4_p);
              });
            });
          });
        });
      });
    });
  });
}
void** fd4___map2 (void**  fd4_clo, uint64_t  fd4_p) {
  return ({
    uint64_t  fd4_n = fd4_clo[3];
    ({
      uint64_t  fd4_m = fd4_clo[2];
      ({
        uint64_t  fd4_o = fd4_clo[1];
        fd4_mkclosure(fd4___map3, 4, fd4_p, fd4_o, fd4_m, fd4_n);
      });
    });
  });
}
void** fd4___map1 (void**  fd4_clo, uint64_t  fd4_o) {
  return ({
    uint64_t  fd4_n = fd4_clo[2];
    ({
      uint64_t  fd4_m = fd4_clo[1];
      fd4_mkclosure(fd4___map2, 3, fd4_o, fd4_m, fd4_n);
    });
  });
}
void** fd4___map0 (void**  fd4_clo, uint64_t  fd4_m) {
  return ({
    uint64_t  fd4_n = fd4_clo[1];
    fd4_mkclosure(fd4___map1, 2, fd4_m, fd4_n);
  });
}
void** fd4_map (void**  fd4_map_clo, uint64_t  fd4_n) {
  return fd4_mkclosure(fd4___map0, 1, fd4_n);
}
uint64_t fd4___sumador2 (void**  fd4_clo, uint64_t  fd4_p) {
  return ({
    uint64_t  fd4_n = fd4_clo[3];
    ({
      uint64_t  fd4_m = fd4_clo[2];
      ({
        uint64_t  fd4_o = fd4_clo[1];
        fd4_n + fd4_m + fd4_p + fd4_o;
      });
    });
  });
}
void** fd4___sumador1 (void**  fd4_clo, uint64_t  fd4_o) {
  return ({
    uint64_t  fd4_n = fd4_clo[2];
    ({
      uint64_t  fd4_m = fd4_clo[1];
      fd4_mkclosure(fd4___sumador2, 3, fd4_o, fd4_m, fd4_n);
    });
  });
}
void** fd4___sumador0 (void**  fd4_clo, uint64_t  fd4_m) {
  return ({
    uint64_t  fd4_n = fd4_clo[1];
    fd4_mkclosure(fd4___sumador1, 2, fd4_m, fd4_n);
  });
}
void** fd4_sumador (void**  fd4_sumador_clo, uint64_t  fd4_n) {
  return fd4_mkclosure(fd4___sumador0, 1, fd4_n);
}
void* fd4_final;
uint64_t* fd4main() {
  fd4_final = (void *)(({
    void**  fd4___clo4 = ({
      void**  fd4___clo3 = ({
        void**  fd4___clo2 = ({
          void**  fd4___clo1 = fd4_map(fd4_mkclosure(fd4_map, 0), 3);
          ({
            void**  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
            fd4___var1(fd4___clo1, 4);
          });
        });
        ({
          void**  (* fd4___var2) (void**,  uint64_t  ) = fd4___clo2[0];
          fd4___var2(fd4___clo2, 5);
        });
      });
      ({
        void**  (* fd4___var3) (void**,  uint64_t  ) = fd4___clo3[0];
        fd4___var3(fd4___clo3, 30);
      });
    });
    ({
      uint64_t  (* fd4___var4) (void**,  void**  (* ) (void**,  uint64_t  ) ) = fd4___clo4[0];
      fd4___var4(fd4___clo4, fd4_mkclosure(fd4_sumador, 0));
    });
  }));
  fd4_printn((uint64_t)fd4_final)
  ;
  return 0;
}