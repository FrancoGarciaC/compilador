#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

uint64_t fd4___map3 (void**  fd4_clo, void**  fd4_h_clo) {
  return ({
    void**  fd4_map_clo = fd4_clo[9];
    ({
      uint64_t  fd4_x = fd4_clo[8];
      ({
        void**  (* fd4_map) (void**,  uint64_t  ) = fd4_map_clo[0];
        ({
          void**  fd4_f_clo = fd4_clo[6];
          ({
            uint64_t  (* fd4_f) (void**,  uint64_t  ) = fd4_f_clo[0];
            ({
              void**  fd4_g_clo = fd4_clo[4];
              ({
                uint64_t  (* fd4_g) (void**,  uint64_t  ) = fd4_g_clo[0];
                ({
                  void**  fd4_p_clo = fd4_clo[2];
                  ({
                    uint64_t  (* fd4_p) (void**,  uint64_t  ) = fd4_p_clo[0];
                    ({
                      uint64_t  (* fd4_h) (void**,  uint64_t  ) = fd4_h_clo[0];
                      fd4_x
                      ? ({
                          uint64_t  fd4_res = fd4_f(fd4_f_clo, fd4_x);
                          ({
                            uint64_t  fd4_res2 = fd4_g(fd4_g_clo, fd4_x);
                            ({
                              uint64_t  fd4_res3 = fd4_p(fd4_p_clo, fd4_x);
                              ({
                                uint64_t  fd4_res4 = fd4_h(fd4_h_clo, fd4_x);
                                ({
                                  void**  fd4___clo7 = ({
                                    void**  fd4___clo6 = ({
                                      void**  fd4___clo5 = ({
                                        void**  fd4___clo4 = fd4_map( fd4_map_clo
                                        , ({
                                          fd4_sub(fd4_x, 1);
                                        }) );
                                        ({
                                          void**  (* fd4___var4) (void**,  uint64_t  (* ) (void**,  uint64_t  ) ) = fd4___clo4[0];
                                          fd4___var4(fd4___clo4, fd4_f_clo);
                                        });
                                      });
                                      ({
                                        void**  (* fd4___var5) (void**,  uint64_t  (* ) (void**,  uint64_t  ) ) = fd4___clo5[0];
                                        fd4___var5(fd4___clo5, fd4_g_clo);
                                      });
                                    });
                                    ({
                                      void**  (* fd4___var6) (void**,  uint64_t  (* ) (void**,  uint64_t  ) ) = fd4___clo6[0];
                                      fd4___var6(fd4___clo6, fd4_p_clo);
                                    });
                                  });
                                  ({
                                    uint64_t  (* fd4___var7) (void**,  uint64_t  (* ) (void**,  uint64_t  ) ) = fd4___clo7[0];
                                    fd4___var7(fd4___clo7, fd4_h_clo);
                                  });
                                });
                              });
                            });
                          });
                        })
                      : 1;
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
void** fd4___map2 (void**  fd4_clo, void**  fd4_p_clo) {
  return ({
    void**  fd4_map_clo = fd4_clo[7];
    ({
      uint64_t  fd4_x = fd4_clo[6];
      ({
        void**  (* fd4_map) (void**,  uint64_t  ) = fd4_map_clo[0];
        ({
          void**  fd4_f_clo = fd4_clo[4];
          ({
            uint64_t  (* fd4_f) (void**,  uint64_t  ) = fd4_f_clo[0];
            ({
              void**  fd4_g_clo = fd4_clo[2];
              ({
                uint64_t  (* fd4_g) (void**,  uint64_t  ) = fd4_g_clo[0];
                ({
                  uint64_t  (* fd4_p) (void**,  uint64_t  ) = fd4_p_clo[0];
                  fd4_mkclosure( fd4___map3
                  , 9
                  , fd4_p
                  , fd4_p_clo
                  , fd4_g
                  , fd4_g_clo
                  , fd4_f
                  , fd4_f_clo
                  , fd4_map
                  , fd4_x
                  , fd4_map_clo );
                });
              });
            });
          });
        });
      });
    });
  });
}
void** fd4___map1 (void**  fd4_clo, void**  fd4_g_clo) {
  return ({
    void**  fd4_map_clo = fd4_clo[5];
    ({
      uint64_t  fd4_x = fd4_clo[4];
      ({
        void**  (* fd4_map) (void**,  uint64_t  ) = fd4_map_clo[0];
        ({
          void**  fd4_f_clo = fd4_clo[2];
          ({
            uint64_t  (* fd4_f) (void**,  uint64_t  ) = fd4_f_clo[0];
            ({
              uint64_t  (* fd4_g) (void**,  uint64_t  ) = fd4_g_clo[0];
              fd4_mkclosure( fd4___map2
              , 7
              , fd4_g
              , fd4_g_clo
              , fd4_f
              , fd4_f_clo
              , fd4_map
              , fd4_x
              , fd4_map_clo );
            });
          });
        });
      });
    });
  });
}
void** fd4___map0 (void**  fd4_clo, void**  fd4_f_clo) {
  return ({
    void**  fd4_map_clo = fd4_clo[3];
    ({
      uint64_t  fd4_x = fd4_clo[2];
      ({
        void**  (* fd4_map) (void**,  uint64_t  ) = fd4_map_clo[0];
        ({
          uint64_t  (* fd4_f) (void**,  uint64_t  ) = fd4_f_clo[0];
          fd4_mkclosure( fd4___map1
          , 5
          , fd4_f
          , fd4_f_clo
          , fd4_map
          , fd4_x
          , fd4_map_clo );
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
    ({
      wprintf(L"x+y:=");
      fd4_printn((uint64_t)fd4_x + fd4_y);
    });
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
            fd4_sub(fd4_h, fd4_j);
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
uint64_t fd4___final5 (void**  fd4_clo, uint64_t  fd4_j) {
  return ({
    void**  fd4_mapF_clo = fd4_clo[5];
    ({
      void**  (* fd4_mapF) (void**,  uint64_t  ) = fd4_mapF_clo[0];
      ({
        void**  fd4_mapP_clo = fd4_clo[3];
        ({
          void**  (* fd4_mapP) (void**,  uint64_t  ) = fd4_mapP_clo[0];
          ({
            uint64_t  fd4_h = fd4_clo[1];
            ({
              wprintf(L"h**j:=");
              fd4_printn((uint64_t)({
                fd4_sub(fd4_h, fd4_j);
              }));
            });
          });
        });
      });
    });
  });
}
void** fd4___final4 (void**  fd4_clo, uint64_t  fd4_h) {
  return ({
    void**  fd4_mapF_clo = fd4_clo[4];
    ({
      void**  (* fd4_mapF) (void**,  uint64_t  ) = fd4_mapF_clo[0];
      ({
        void**  fd4_mapP_clo = fd4_clo[2];
        ({
          void**  (* fd4_mapP) (void**,  uint64_t  ) = fd4_mapP_clo[0];
          fd4_mkclosure( fd4___final5
          , 5
          , fd4_h
          , fd4_mapP
          , fd4_mapP_clo
          , fd4_mapF
          , fd4_mapF_clo );
        });
      });
    });
  });
}
uint64_t fd4___final7 (void**  fd4_clo, uint64_t  fd4_j) {
  return ({
    void**  fd4_mapF_clo = fd4_clo[7];
    ({
      void**  (* fd4_mapF) (void**,  uint64_t  ) = fd4_mapF_clo[0];
      ({
        void**  fd4_mapP_clo = fd4_clo[5];
        ({
          void**  (* fd4_mapP) (void**,  uint64_t  ) = fd4_mapP_clo[0];
          ({
            void**  fd4_mapH_clo = fd4_clo[3];
            ({
              void**  (* fd4_mapH) (void**,  uint64_t  ) = fd4_mapH_clo[0];
              ({
                uint64_t  fd4_h = fd4_clo[1];
                ({
                  wprintf(L"h....j:=");
                  fd4_printn((uint64_t)({
                    fd4_sub(fd4_h, fd4_j);
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
void** fd4___final6 (void**  fd4_clo, uint64_t  fd4_h) {
  return ({
    void**  fd4_mapF_clo = fd4_clo[6];
    ({
      void**  (* fd4_mapF) (void**,  uint64_t  ) = fd4_mapF_clo[0];
      ({
        void**  fd4_mapP_clo = fd4_clo[4];
        ({
          void**  (* fd4_mapP) (void**,  uint64_t  ) = fd4_mapP_clo[0];
          ({
            void**  fd4_mapH_clo = fd4_clo[2];
            ({
              void**  (* fd4_mapH) (void**,  uint64_t  ) = fd4_mapH_clo[0];
              fd4_mkclosure( fd4___final7
              , 7
              , fd4_h
              , fd4_mapH
              , fd4_mapH_clo
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
            void**  fd4_mapH_clo = fd4_mkclosure( fd4___final4
            , 4
            , fd4_mapP
            , fd4_mapP_clo
            , fd4_mapF
            , fd4_mapF_clo );
            ({
              void**  (* fd4_mapH) (void**,  uint64_t  ) = fd4_mapH_clo[0];
              ({
                void**  fd4_mapJ_clo = fd4_mkclosure( fd4___final6
                , 6
                , fd4_mapH
                , fd4_mapH_clo
                , fd4_mapP
                , fd4_mapP_clo
                , fd4_mapF
                , fd4_mapF_clo );
                ({
                  void**  (* fd4_mapJ) (void**,  uint64_t  ) = fd4_mapJ_clo[0];
                  ({
                    void**  fd4___clo12 = ({
                      void**  fd4___clo11 = ({
                        void**  fd4___clo10 = ({
                          void**  fd4___clo9 = fd4_map( fd4_mkclosure( fd4_map
                          , 0 )
                          , 20 );
                          ({
                            void**  (* fd4___var9) (void**,  uint64_t  (* ) (void**,  uint64_t  ) ) = fd4___clo9[0];
                            fd4___var9(fd4___clo9, fd4_mapF(fd4_mapF_clo, 5));
                          });
                        });
                        ({
                          void**  (* fd4___var10) (void**,  uint64_t  (* ) (void**,  uint64_t  ) ) = fd4___clo10[0];
                          fd4___var10(fd4___clo10, fd4_mapP(fd4_mapP_clo, 12));
                        });
                      });
                      ({
                        void**  (* fd4___var11) (void**,  uint64_t  (* ) (void**,  uint64_t  ) ) = fd4___clo11[0];
                        fd4___var11(fd4___clo11, fd4_mapH(fd4_mapH_clo, 18));
                      });
                    });
                    ({
                      uint64_t  (* fd4___var12) (void**,  uint64_t  (* ) (void**,  uint64_t  ) ) = fd4___clo12[0];
                      fd4___var12(fd4___clo12, fd4_mapJ(fd4_mapJ_clo, 20));
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