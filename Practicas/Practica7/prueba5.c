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
          fd4_x
          ? ({
              uint64_t  fd4_res = fd4_f(fd4_f_clo, fd4_x);
              ({
                void**  fd4___clo1 = fd4_map( fd4_map_clo
                , ({
                  fd4_sub(fd4_x, 1);
                }) );
                ({
                  uint64_t  (* fd4___var1) (void**,  uint64_t  (* ) (void**,  uint64_t  ) ) = fd4___clo1[0];
                  fd4___var1(fd4___clo1, fd4_f_clo);
                });
              });
            })
          : fd4_x;
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
void* fd4_final;
uint64_t* fd4main() {
  fd4_final = (void *)(({
    void**  fd4___clo2 = fd4_map(fd4_mkclosure(fd4_map, 0), 20);
    ({
      uint64_t  (* fd4___var2) (void**,  uint64_t  (* ) (void**,  uint64_t  ) ) = fd4___clo2[0];
      fd4___var2(fd4___clo2, fd4_mapF(fd4_mkclosure(fd4_mapF, 0), 5));
    });
  }));
  fd4_printn((uint64_t)fd4_final)
  ;
  return 0;
}