#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

uint64_t fd4___map1 (void**  fd4_clo, void**  fd4_f_clo) {
  return ({
    void**  fd4_map_clo = fd4_clo[4];
    ({
      uint64_t  fd4_x = fd4_clo[3];
      ({
        void**  (* fd4_map) (void**,  uint64_t  ) = fd4_map_clo[0];
        ({
          uint64_t  fd4_y = fd4_clo[1];
          ({
            void**  (* fd4_f) (void**,  uint64_t  ) = fd4_f_clo[0];
            ({
              void**  fd4___clo2 = fd4_f(fd4_f_clo, fd4_x);
              ({
                uint64_t  (* fd4___var2) (void**,  uint64_t  ) = fd4___clo2[0];
                fd4___var2(fd4___clo2, fd4_y);
              });
            });
          });
        });
      });
    });
  });
}
void** fd4___map0 (void**  fd4_clo, uint64_t  fd4_y) {
  return ({
    void**  fd4_map_clo = fd4_clo[3];
    ({
      uint64_t  fd4_x = fd4_clo[2];
      ({
        void**  (* fd4_map) (void**,  uint64_t  ) = fd4_map_clo[0];
        fd4_mkclosure(fd4___map1, 4, fd4_y, fd4_map, fd4_x, fd4_map_clo);
      });
    });
  });
}
void** fd4_map (void**  fd4_map_clo, uint64_t  fd4_x) {
  return fd4_mkclosure(fd4___map0, 3, fd4_map, fd4_x, fd4_map_clo);
}
uint64_t fd4___final2 (void**  fd4_clo, uint64_t  fd4_z) {
  return ({
    uint64_t  fd4_x = fd4_clo[2];
    ({
      uint64_t  fd4_y = fd4_clo[1];
      fd4_x + fd4_y + fd4_z;
    });
  });
}
void** fd4___final1 (void**  fd4_clo, uint64_t  fd4_y) {
  return ({
    uint64_t  fd4_x = fd4_clo[1];
    fd4_mkclosure(fd4___final2, 2, fd4_y, fd4_x);
  });
}
void** fd4___final0 (void**  fd4_clo, uint64_t  fd4_x) {
  return fd4_mkclosure(fd4___final1, 1, fd4_x);
}
uint64_t fd4___final5 (void**  fd4_clo, uint64_t  fd4_q) {
  return ({
    void**  fd4_mapF_clo = fd4_clo[4];
    ({
      void**  (* fd4_mapF) (void**,  uint64_t  ) = fd4_mapF_clo[0];
      ({
        uint64_t  fd4_h = fd4_clo[2];
        ({
          uint64_t  fd4_j = fd4_clo[1];
          ({
            wprintf(L"h-j:=");
            fd4_printn((uint64_t)({
              void**  fd4___clo7 = ({
                void**  fd4___clo6 = fd4_mapF(fd4_mapF_clo, fd4_h);
                ({
                  void**  (* fd4___var6) (void**,  uint64_t  ) = fd4___clo6[0];
                  fd4___var6(fd4___clo6, fd4_j);
                });
              });
              ({
                uint64_t  (* fd4___var7) (void**,  uint64_t  ) = fd4___clo7[0];
                fd4___var7(fd4___clo7, fd4_q);
              });
            }));
          });
        });
      });
    });
  });
}
void** fd4___final4 (void**  fd4_clo, uint64_t  fd4_j) {
  return ({
    void**  fd4_mapF_clo = fd4_clo[3];
    ({
      void**  (* fd4_mapF) (void**,  uint64_t  ) = fd4_mapF_clo[0];
      ({
        uint64_t  fd4_h = fd4_clo[1];
        fd4_mkclosure(fd4___final5, 4, fd4_j, fd4_h, fd4_mapF, fd4_mapF_clo);
      });
    });
  });
}
void** fd4___final3 (void**  fd4_clo, uint64_t  fd4_h) {
  return ({
    void**  fd4_mapF_clo = fd4_clo[2];
    ({
      void**  (* fd4_mapF) (void**,  uint64_t  ) = fd4_mapF_clo[0];
      fd4_mkclosure(fd4___final4, 3, fd4_h, fd4_mapF, fd4_mapF_clo);
    });
  });
}
uint64_t fd4___final10 (void**  fd4_clo, uint64_t  fd4_q) {
  return ({
    void**  fd4_mapF_clo = fd4_clo[6];
    ({
      void**  (* fd4_mapF) (void**,  uint64_t  ) = fd4_mapF_clo[0];
      ({
        void**  fd4_mapP_clo = fd4_clo[4];
        ({
          void**  (* fd4_mapP) (void**,  uint64_t  ) = fd4_mapP_clo[0];
          ({
            uint64_t  fd4_a = fd4_clo[2];
            ({
              uint64_t  fd4_b = fd4_clo[1];
              ({
                wprintf(L"a-b:=");
                fd4_printn((uint64_t)({
                  void**  fd4___clo12 = ({
                    void**  fd4___clo11 = fd4_mapP(fd4_mapP_clo, fd4_a);
                    ({
                      void**  (* fd4___var11) (void**,  uint64_t  ) = fd4___clo11[0];
                      fd4___var11(fd4___clo11, fd4_b);
                    });
                  });
                  ({
                    uint64_t  (* fd4___var12) (void**,  uint64_t  ) = fd4___clo12[0];
                    fd4___var12(fd4___clo12, fd4_q);
                  });
                }));
              });
            });
          });
        });
      });
    });
  });
}
void** fd4___final9 (void**  fd4_clo, uint64_t  fd4_b) {
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
            fd4_mkclosure( fd4___final10
            , 6
            , fd4_b
            , fd4_a
            , fd4_mapP
            , fd4_mapP_clo
            , fd4_mapF
            , fd4_mapF_clo );
          });
        });
      });
    });
  });
}
void** fd4___final8 (void**  fd4_clo, uint64_t  fd4_a) {
  return ({
    void**  fd4_mapF_clo = fd4_clo[4];
    ({
      void**  (* fd4_mapF) (void**,  uint64_t  ) = fd4_mapF_clo[0];
      ({
        void**  fd4_mapP_clo = fd4_clo[2];
        ({
          void**  (* fd4_mapP) (void**,  uint64_t  ) = fd4_mapP_clo[0];
          fd4_mkclosure( fd4___final9
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
uint64_t fd4___final15 (void**  fd4_clo, uint64_t  fd4_q) {
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
                uint64_t  fd4_v = fd4_clo[2];
                ({
                  uint64_t  fd4_m = fd4_clo[1];
                  ({
                    wprintf(L"b+m:=");
                    fd4_printn((uint64_t)({
                      void**  fd4___clo17 = ({
                        void**  fd4___clo16 = fd4_mapX(fd4_mapX_clo, fd4_v);
                        ({
                          void**  (* fd4___var16) (void**,  uint64_t  ) = fd4___clo16[0];
                          fd4___var16(fd4___clo16, fd4_m);
                        });
                      });
                      ({
                        uint64_t  (* fd4___var17) (void**,  uint64_t  ) = fd4___clo17[0];
                        fd4___var17(fd4___clo17, fd4_q);
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
}
void** fd4___final14 (void**  fd4_clo, uint64_t  fd4_m) {
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
                fd4_mkclosure( fd4___final15
                , 8
                , fd4_m
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
  });
}
void** fd4___final13 (void**  fd4_clo, uint64_t  fd4_v) {
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
              fd4_mkclosure( fd4___final14
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
uint64_t fd4___final20 (void**  fd4_clo, uint64_t  fd4_q) {
  return ({
    void**  fd4_mapF_clo = fd4_clo[10];
    ({
      void**  (* fd4_mapF) (void**,  uint64_t  ) = fd4_mapF_clo[0];
      ({
        void**  fd4_mapP_clo = fd4_clo[8];
        ({
          void**  (* fd4_mapP) (void**,  uint64_t  ) = fd4_mapP_clo[0];
          ({
            void**  fd4_mapX_clo = fd4_clo[6];
            ({
              void**  (* fd4_mapX) (void**,  uint64_t  ) = fd4_mapX_clo[0];
              ({
                void**  fd4_mapZ_clo = fd4_clo[4];
                ({
                  void**  (* fd4_mapZ) (void**,  uint64_t  ) = fd4_mapZ_clo[0];
                  ({
                    uint64_t  fd4_k = fd4_clo[2];
                    ({
                      uint64_t  fd4_i = fd4_clo[1];
                      ({
                        wprintf(L"k+i:=");
                        fd4_printn((uint64_t)5 + ({
                          void**  fd4___clo22 = ({
                            void**  fd4___clo21 = fd4_mapZ(fd4_mapZ_clo, fd4_k);
                            ({
                              void**  (* fd4___var21) (void**,  uint64_t  ) = fd4___clo21[0];
                              fd4___var21(fd4___clo21, fd4_i);
                            });
                          });
                          ({
                            uint64_t  (* fd4___var22) (void**,  uint64_t  ) = fd4___clo22[0];
                            fd4___var22(fd4___clo22, fd4_q);
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
  });
}
void** fd4___final19 (void**  fd4_clo, uint64_t  fd4_i) {
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
                    fd4_mkclosure( fd4___final20
                    , 10
                    , fd4_i
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
  });
}
void** fd4___final18 (void**  fd4_clo, uint64_t  fd4_k) {
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
                  fd4_mkclosure( fd4___final19
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
        void**  fd4_mapP_clo = fd4_mkclosure( fd4___final3
        , 2
        , fd4_mapF
        , fd4_mapF_clo );
        ({
          void**  (* fd4_mapP) (void**,  uint64_t  ) = fd4_mapP_clo[0];
          ({
            void**  fd4_mapX_clo = fd4_mkclosure( fd4___final8
            , 4
            , fd4_mapP
            , fd4_mapP_clo
            , fd4_mapF
            , fd4_mapF_clo );
            ({
              void**  (* fd4_mapX) (void**,  uint64_t  ) = fd4_mapX_clo[0];
              ({
                void**  fd4_mapZ_clo = fd4_mkclosure( fd4___final13
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
                    void**  fd4_mapL_clo = fd4_mkclosure( fd4___final18
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
                        void**  fd4___clo25 = ({
                          void**  fd4___clo24 = fd4_map( fd4_mkclosure( fd4_map
                          , 0 )
                          , 20 );
                          ({
                            void**  (* fd4___var24) (void**,  uint64_t  ) = fd4___clo24[0];
                            fd4___var24(fd4___clo24, 23);
                          });
                        });
                        ({
                          uint64_t  (* fd4___var25) (void**,  void**  (* ) (void**,  uint64_t  ) ) = fd4___clo25[0];
                          fd4___var25(fd4___clo25, fd4_mapL(fd4_mapL_clo, 5));
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