#include <inttypes.h>
#include <wchar.h>
extern void** fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern uint64_t fd4_sub(uint64_t, uint64_t);

uint64_t fd4_z;
uint64_t fd4___suma0 (void**  fd4_clo, uint64_t  fd4_y) {
  return ({
    uint64_t  fd4_x = fd4_clo[1];
    fd4_x + fd4_y;
  });
}
void** fd4_suma (void**  fd4_suma_clo, uint64_t  fd4_x) {
  return fd4_mkclosure(fd4___suma0, 1, fd4_x);
}
uint64_t fd4___resta0 (void**  fd4_clo, uint64_t  fd4_y) {
  return ({
    uint64_t  fd4_x = fd4_clo[1];
    ({
      fd4_sub(fd4_x, fd4_y);
    });
  });
}
void** fd4_resta (void**  fd4_resta_clo, uint64_t  fd4_x) {
  return fd4_mkclosure(fd4___resta0, 1, fd4_x);
}
uint64_t fd4_ans;
uint64_t* fd4main() {
  fd4_z = (void *)(4);
  fd4_ans = (void *)(({
    void**  fd4___clo9 = fd4_suma( fd4_mkclosure(fd4_suma, 0)
    , ({
      void**  fd4___clo7 = fd4_resta(fd4_mkclosure(fd4_resta, 0), fd4_z);
      ({
        uint64_t  (* fd4___var7) (void**,  uint64_t  ) = fd4___clo7[0];
        fd4___var7(fd4___clo7, fd4_z);
      });
    }) );
    ({
      uint64_t  (* fd4___var9) (void**,  uint64_t  ) = fd4___clo9[0];
      fd4___var9( fd4___clo9
      , ({
        void**  fd4___clo5 = fd4_suma( fd4_mkclosure(fd4_suma, 0)
        , ({
          void**  fd4___clo3 = fd4_resta(fd4_mkclosure(fd4_resta, 0), fd4_z);
          ({
            uint64_t  (* fd4___var3) (void**,  uint64_t  ) = fd4___clo3[0];
            fd4___var3(fd4___clo3, 3);
          });
        }) );
        ({
          uint64_t  (* fd4___var5) (void**,  uint64_t  ) = fd4___clo5[0];
          fd4___var5( fd4___clo5
          , ({
            void**  fd4___clo1 = fd4_suma(fd4_mkclosure(fd4_suma, 0), 1);
            ({
              uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
              fd4___var1(fd4___clo1, 2);
            });
          }) );
        });
      }) );
    });
  }));
  fd4_printn((uint64_t)fd4_ans)
  ;
  return 0;
}