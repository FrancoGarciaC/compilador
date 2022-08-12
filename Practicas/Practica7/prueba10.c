#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

uint64_t fd4___map0 (void**  fd4_clo, void**  fd4_f_clo) {
  return ({
    void**  fd4_map_clo = fd4_clo[3];
    ({
      uint64_t  fd4_x = fd4_clo[2];
      ({
        void**  (* fd4_map) (void**,  uint64_t  ) = fd4_map_clo[0];
        ({
          uint64_t  (* fd4_f) (void**,  uint64_t  ) = fd4_f_clo[0];
          fd4_f(fd4_f_clo, fd4_x);
        });
      });
    });
  });
}
void** fd4_map (void**  fd4_map_clo, uint64_t  fd4_x) {
  return fd4_mkclosure(fd4___map0, 3, fd4_map, fd4_x, fd4_map_clo);
}
uint64_t fd4___final1 (void**  fd4_clo, uint64_t  fd4_y) {
  return ({
    uint64_t  fd4_x = fd4_clo[1];
    fd4_x + fd4_y;
  });
}
void** fd4___final0 (void**  fd4_clo, uint64_t  fd4_x) {
  return fd4_mkclosure(fd4___final1, 1, fd4_x);
}
uint64_t fd4___final3 (void**  fd4_clo, uint64_t  fd4_j) {
  return ({
    void**  fd4_mapF_clo = fd4_clo[3];
    ({
      void**  (* fd4_mapF) (void**,  uint64_t  ) = fd4_mapF_clo[0];
      ({
        uint64_t  fd4_h = fd4_clo[1];
        ({
          wprintf(L"h-j:=");
          fd4_printn((uint64_t)({
            void**  fd4___clo4 = fd4_mapF(fd4_mapF_clo, fd4_h);
            ({
              uint64_t  (* fd4___var4) (void**,  uint64_t  ) = fd4___clo4[0];
              fd4___var4(fd4___clo4, fd4_j);
            });
          }));
        });
      });
    });
  });
}
void** fd4___final2 (void**  fd4_clo, uint64_t  fd4_h) {
  return ({
    void**  fd4_mapF_clo = fd4_clo[2];
    ({
      void**  (* fd4_mapF) (void**,  uint64_t  ) = fd4_mapF_clo[0];
      fd4_mkclosure(fd4___final3, 3, fd4_h, fd4_mapF, fd4_mapF_clo);
    });
  });
}
uint64_t fd4___final6 (void**  fd4_clo, uint64_t  fd4_b) {
  return ({
    void**  fd4_mapF_clo = fd4_clo[5];
    ({
      void**  (* fd4_mapF) (void**,  uint64_t  ) = fd4_mapF_clo[0];
      ({
        void**  fd4_mapP_clo = fd4_clo[3];
        ({
          void**  (* fd4_mapP) (void**,  uint64_t  ) = fd4_mapP_clo[0];
          ({
            uint64_t  fd4_a = fd4_clo[1];
            ({
              wprintf(L"a-b:=");
              fd4_printn((uint64_t)({
                void**  fd4___clo7 = fd4_mapP(fd4_mapP_clo, fd4_a);
                ({
                  uint64_t  (* fd4___var7) (void**,  uint64_t  ) = fd4___clo7[0];
                  fd4___var7(fd4___clo7, fd4_b);
                });
              }));
            });
          });
        });
      });
    });
  });
}
void** fd4___final5 (void**  fd4_clo, uint64_t  fd4_a) {
  return ({
    void**  fd4_mapF_clo = fd4_clo[4];
    ({
      void**  (* fd4_mapF) (void**,  uint64_t  ) = fd4_mapF_clo[0];
      ({
        void**  fd4_mapP_clo = fd4_clo[2];
        ({
          void**  (* fd4_mapP) (void**,  uint64_t  ) = fd4_mapP_clo[0];
          fd4_mkclosure( fd4___final6
          , 5
          , fd4_a
          , fd4_mapP
          , fd4_mapP_clo
          , fd4_mapF
          , fd4_mapF_clo );
        });
      });
    });
  });
}
uint64_t fd4___final9 (void**  fd4_clo, uint64_t  fd4_m) {
  return ({
    void**  fd4_mapF_clo = fd4_clo[7];
    ({
      void**  (* fd4_mapF) (void**,  uint64_t  ) = fd4_mapF_clo[0];
      ({
        void**  fd4_mapP_clo = fd4_clo[5];
        ({
          void**  (* fd4_mapP) (void**,  uint64_t  ) = fd4_mapP_clo[0];
          ({
            void**  fd4_mapX_clo = fd4_clo[3];
            ({
              void**  (* fd4_mapX) (void**,  uint64_t  ) = fd4_mapX_clo[0];
              ({
                uint64_t  fd4_v = fd4_clo[1];
                ({
                  wprintf(L"b+m:=");
                  fd4_printn((uint64_t)({
                    void**  fd4___clo10 = fd4_mapX(fd4_mapX_clo, fd4_v);
                    ({
                      uint64_t  (* fd4___var10) (void**,  uint64_t  ) = fd4___clo10[0];
                      fd4___var10(fd4___clo10, fd4_m);
                    });
                  }));
                });
              });
            });
          });
        });
      });
    });
  });
}
void** fd4___final8 (void**  fd4_clo, uint64_t  fd4_v) {
  return ({
    void**  fd4_mapF_clo = fd4_clo[6];
    ({
      void**  (* fd4_mapF) (void**,  uint64_t  ) = fd4_mapF_clo[0];
      ({
        void**  fd4_mapP_clo = fd4_clo[4];
        ({
          void**  (* fd4_mapP) (void**,  uint64_t  ) = fd4_mapP_clo[0];
          ({
            void**  fd4_mapX_clo = fd4_clo[2];
            ({
              void**  (* fd4_mapX) (void**,  uint64_t  ) = fd4_mapX_clo[0];
              fd4_mkclosure( fd4___final9
              , 7
              , fd4_v
              , fd4_mapX
              , fd4_mapX_clo
              , fd4_mapP
              , fd4_mapP_clo
              , fd4_mapF
              , fd4_mapF_clo );
            });
          });
        });
      });
    });
  });
}
uint64_t fd4___final12 (void**  fd4_clo, uint64_t  fd4_i) {
  return ({
    void**  fd4_mapF_clo = fd4_clo[9];
    ({
      void**  (* fd4_mapF) (void**,  uint64_t  ) = fd4_mapF_clo[0];
      ({
        void**  fd4_mapP_clo = fd4_clo[7];
        ({
          void**  (* fd4_mapP) (void**,  uint64_t  ) = fd4_mapP_clo[0];
          ({
            void**  fd4_mapX_clo = fd4_clo[5];
            ({
              void**  (* fd4_mapX) (void**,  uint64_t  ) = fd4_mapX_clo[0];
              ({
                void**  fd4_mapZ_clo = fd4_clo[3];
                ({
                  void**  (* fd4_mapZ) (void**,  uint64_t  ) = fd4_mapZ_clo[0];
                  ({
                    uint64_t  fd4_k = fd4_clo[1];
                    ({
                      wprintf(L"k+i:=");
                      fd4_printn((uint64_t)({
                        void**  fd4___clo13 = fd4_mapZ(fd4_mapZ_clo, fd4_k);
                        ({
                          uint64_t  (* fd4___var13) (void**,  uint64_t  ) = fd4___clo13[0];
                          fd4___var13(fd4___clo13, fd4_i);
                        });
                      }));
                    });
                  });
                });
              });
            });
          });
        });
      });
    });
  });
}
void** fd4___final11 (void**  fd4_clo, uint64_t  fd4_k) {
  return ({
    void**  fd4_mapF_clo = fd4_clo[8];
    ({
      void**  (* fd4_mapF) (void**,  uint64_t  ) = fd4_mapF_clo[0];
      ({
        void**  fd4_mapP_clo = fd4_clo[6];
        ({
          void**  (* fd4_mapP) (void**,  uint64_t  ) = fd4_mapP_clo[0];
          ({
            void**  fd4_mapX_clo = fd4_clo[4];
            ({
              void**  (* fd4_mapX) (void**,  uint64_t  ) = fd4_mapX_clo[0];
              ({
                void**  fd4_mapZ_clo = fd4_clo[2];
                ({
                  void**  (* fd4_mapZ) (void**,  uint64_t  ) = fd4_mapZ_clo[0];
                  fd4_mkclosure( fd4___final12
                  , 9
                  , fd4_k
                  , fd4_mapZ
                  , fd4_mapZ_clo
                  , fd4_mapX
                  , fd4_mapX_clo
                  , fd4_mapP
                  , fd4_mapP_clo
                  , fd4_mapF
                  , fd4_mapF_clo );
                });
              });
            });
          });
        });
      });
    });
  });
}
void* fd4_final;
uint64_t* fd4main() {
  fd4_final = (void *)(({
    void**  fd4_mapF_clo = fd4_mkclosure(fd4___final0, 0);
    ({
      void**  (* fd4_mapF) (void**,  uint64_t  ) = fd4_mapF_clo[0];
      ({
        void**  fd4_mapP_clo = fd4_mkclosure( fd4___final2
        , 2
        , fd4_mapF
        , fd4_mapF_clo );
        ({
          void**  (* fd4_mapP) (void**,  uint64_t  ) = fd4_mapP_clo[0];
          ({
            void**  fd4_mapX_clo = fd4_mkclosure( fd4___final5
            , 4
            , fd4_mapP
            , fd4_mapP_clo
            , fd4_mapF
            , fd4_mapF_clo );
            ({
              void**  (* fd4_mapX) (void**,  uint64_t  ) = fd4_mapX_clo[0];
              ({
                void**  fd4_mapZ_clo = fd4_mkclosure( fd4___final8
                , 6
                , fd4_mapX
                , fd4_mapX_clo
                , fd4_mapP
                , fd4_mapP_clo
                , fd4_mapF
                , fd4_mapF_clo );
                ({
                  void**  (* fd4_mapZ) (void**,  uint64_t  ) = fd4_mapZ_clo[0];
                  ({
                    void**  fd4_mapL_clo = fd4_mkclosure( fd4___final11
                    , 8
                    , fd4_mapZ
                    , fd4_mapZ_clo
                    , fd4_mapX
                    , fd4_mapX_clo
                    , fd4_mapP
                    , fd4_mapP_clo
                    , fd4_mapF
                    , fd4_mapF_clo );
                    ({
                      void**  (* fd4_mapL) (void**,  uint64_t  ) = fd4_mapL_clo[0];
                      ({
                        void**  fd4___clo15 = fd4_map( fd4_mkclosure(fd4_map, 0)
                        , 20 );
                        ({
                          uint64_t  (* fd4___var15) (void**,  uint64_t  (* ) (void**,  uint64_t  ) ) = fd4___clo15[0];
                          fd4___var15(fd4___clo15, fd4_mapL(fd4_mapL_clo, 2));
                        });
                      });
                    });
                  });
                });
              });
            });
          });
        });
      });
    });
  }));
  fd4_printn((uint64_t)fd4_final)
  ;
  return 0;
}