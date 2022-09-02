#include <inttypes.h>
#include <wchar.h>
extern void** fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern uint64_t fd4_sub(uint64_t, uint64_t);

uint64_t fd4___eq0 (void**  fd4_clo, uint64_t  fd4_y) {
  return ({
    uint64_t  fd4_x = fd4_clo[1];
    ({ fd4_sub(fd4_x, fd4_y); }) + ({ fd4_sub(fd4_y, fd4_x); }) ? 1 : 0;
  });
}
void** fd4_eq (void**  fd4_eq_clo, uint64_t  fd4_x) {
  return fd4_mkclosure(fd4___eq0, 1, fd4_x);
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
uint64_t fd4_mans;
uint64_t* fd4main() {
  fd4_mans = (void *)(({
    void**  fd4___clo1 = fd4_mod(fd4_mkclosure(fd4_mod, 0), 5234);
    ({
      uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
      fd4___var1(fd4___clo1, 10);
    });
  }));
  fd4_printn((uint64_t)fd4_mans)
  ;
  return 0;
}