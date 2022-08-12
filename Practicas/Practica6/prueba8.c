#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

uint64_t fd4___map1 ( void**  fd4_clo
, uint64_t  (* fd4_g) (void**,  uint64_t  ) ) {
  return ({
    void**  fd4_map_clo = fd4_clo[4];
    ({
      uint64_t  fd4_x = fd4_clo[3];
      ({
        void**  (* fd4_map) (void**,  uint64_t  ) = fd4_map_clo[0];
        ({
          void**  fd4_f_clo = fd4_clo[1];
          ({
            uint64_t  (* fd4_f) (void**,  uint64_t  ) = fd4_f_clo[0];
            ({
              void**  fd4_g_clo = fd4_mkclosure(fd4_g, 0);
              fd4_x
              ? ({
                  uint64_t  fd4_res = fd4_f(fd4_f_clo, fd4_x);
                  ({
                    uint64_t  fd4_res2 = fd4_g(fd4_g_clo, fd4_x);
                    ({
                      void**  fd4___clo3 = ({
                        void**  fd4___clo2 = fd4_map( fd4_map_clo
                        , ({
                          fd4_sub(fd4_x, 1);
                        }) );
                        ({
                          void**  (* fd4___var2) (void**,  uint64_t  (* ) (void**,  uint64_t  ) ) = fd4___clo2[0];
                          fd4___var2(fd4___clo2, fd4_f);
                        });
                      });
                      ({
                        uint64_t  (* fd4___var3) (void**,  uint64_t  (* ) (void**,  uint64_t  ) ) = fd4___clo3[0];
                        fd4___var3(fd4___clo3, fd4_g);
                      });
                    });
                  });
                })
              : fd4_x;
            });
          });
        });
      });
    });
  });
}
void** fd4___map0 (void**  fd4_clo, uint64_t  (* fd4_f) (void**,  uint64_t  )) {
  return ({
    void**  fd4_map_clo = fd4_clo[3];
    ({
      uint64_t  fd4_x = fd4_clo[2];
      ({
        void**  (* fd4_map) (void**,  uint64_t  ) = fd4_map_clo[0];
        ({
          void**  fd4_f_clo = fd4_mkclosure(fd4_f, 0);
          fd4_mkclosure(fd4___map1, 4, fd4_f, fd4_map, fd4_x, fd4_map_clo);
        });
      });
    });
  });
}
void** fd4_map (void**  fd4_map_clo, uint64_t  fd4_x) {
  return fd4_mkclosure(fd4___map0, 3, fd4_map, fd4_x, fd4_map_clo);
}
uint64_t fd4___mapF0 (void**  fd4_clo, uint64_t  fd4_y) {
  return ({
    uint64_t  fd4_x = fd4_clo[1];
    ({
      wprintf(L"x+y:=");
      fd4_printn((uint64_t)fd4_x + fd4_y);
    });
  });
}
void** fd4_mapF (void**  fd4_mapF_clo, uint64_t  fd4_x) {
  return fd4_mkclosure(fd4___mapF0, 1, fd4_x);
}
uint64_t fd4___mapP0 (void**  fd4_clo, uint64_t  fd4_j) {
  return ({
    uint64_t  fd4_h = fd4_clo[1];
    ({
      wprintf(L"h-j:=");
      fd4_printn((uint64_t)({
        fd4_sub(fd4_h, fd4_j);
      }));
    });
  });
}
void** fd4_mapP (void**  fd4_mapP_clo, uint64_t  fd4_h) {
  return fd4_mkclosure(fd4___mapP0, 1, fd4_h);
}
void* fd4_final;
uint64_t* fd4main() {
  fd4_final = (void *)(({
    void**  fd4___clo4 = ({
      void**  fd4___clo3 = fd4_map(fd4_mkclosure(fd4_map, 0), 20);
      ({
        void**  (* fd4___var3) (void**,  uint64_t  (* ) (void**,  uint64_t  ) ) = fd4___clo3[0];
        fd4___var3(fd4___clo3, fd4_mapF(fd4_mkclosure(fd4_mapF, 0), 5));
      });
    });
    ({
      uint64_t  (* fd4___var4) (void**,  uint64_t  (* ) (void**,  uint64_t  ) ) = fd4___clo4[0];
      fd4___var4(fd4___clo4, fd4_mapP(fd4_mkclosure(fd4_mapP, 0), 100));
    });
  }));
  fd4_printn((uint64_t)fd4_final)
  ;
  return 0;
}