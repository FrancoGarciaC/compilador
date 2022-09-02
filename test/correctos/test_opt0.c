#include <inttypes.h>
#include <wchar.h>
extern void** fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern uint64_t fd4_sub(uint64_t, uint64_t);

uint64_t fd4_pred (void**  fd4_pred_clo, uint64_t  fd4_x) {
  return ({
    fd4_sub(fd4_x, 1);
  });
}
uint64_t fd4_succ (void**  fd4_succ_clo, uint64_t  fd4_x) {
  return fd4_x + 1;
}
uint64_t fd4___ack0 (void**  fd4_clo, uint64_t  fd4_n) {
  return ({
    void**  fd4_ack_clo = fd4_clo[3];
    ({
      uint64_t  fd4_m = fd4_clo[2];
      ({
        void**  (* fd4_ack) (void**,  uint64_t  ) = fd4_ack_clo[0];
        fd4_m
        ? fd4_n
          ? ({
              void**  fd4___clo7 = fd4_ack( fd4_ack_clo
              , fd4_pred(fd4_mkclosure(fd4_pred, 0), fd4_m) );
              ({
                uint64_t  (* fd4___var7) (void**,  uint64_t  ) = fd4___clo7[0];
                fd4___var7( fd4___clo7
                , ({
                  void**  fd4___clo5 = fd4_ack(fd4_ack_clo, fd4_m);
                  ({
                    uint64_t  (* fd4___var5) (void**,  uint64_t  ) = fd4___clo5[0];
                    fd4___var5( fd4___clo5
                    , fd4_pred(fd4_mkclosure(fd4_pred, 0), fd4_n) );
                  });
                }) );
              });
            })
          : ({
              void**  fd4___clo3 = fd4_ack( fd4_ack_clo
              , fd4_pred(fd4_mkclosure(fd4_pred, 0), fd4_m) );
              ({
                uint64_t  (* fd4___var3) (void**,  uint64_t  ) = fd4___clo3[0];
                fd4___var3(fd4___clo3, 1);
              });
            })
        : fd4_succ(fd4_mkclosure(fd4_succ, 0), fd4_n);
      });
    });
  });
}
void** fd4_ack (void**  fd4_ack_clo, uint64_t  fd4_m) {
  return fd4_mkclosure(fd4___ack0, 3, fd4_ack, fd4_m, fd4_ack_clo);
}
uint64_t fd4_big (void**  fd4_big_clo, uint64_t  fd4_x) {
  return fd4_succ( fd4_mkclosure(fd4_succ, 0)
  , fd4_succ( fd4_mkclosure(fd4_succ, 0)
  , fd4_succ( fd4_mkclosure(fd4_succ, 0)
  , fd4_succ( fd4_mkclosure(fd4_succ, 0)
  , fd4_succ(fd4_mkclosure(fd4_succ, 0), fd4_x) ) ) ) );
}
uint64_t fd4_small (void**  fd4_small_clo, uint64_t  fd4_x) {
  return fd4_x + 5;
}
uint64_t fd4_n2;
uint64_t fd4_DEBUG;
uint64_t fd4_f2 (void**  fd4_f2_clo, uint64_t  fd4_x) {
  return fd4_x + 5;
}
uint64_t fd4_f1 (void**  fd4_f1_clo, uint64_t  fd4_x) {
  return fd4_f2(fd4_mkclosure(fd4_f2, 0), fd4_x);
}
uint64_t fd4_x;
uint64_t fd4_n3;
uint64_t* fd4main() {
  fd4_n2 = (void *)(fd4_small( fd4_mkclosure(fd4_small, 0)
  , fd4_small( fd4_mkclosure(fd4_small, 0)
  , fd4_small( fd4_mkclosure(fd4_small, 0)
  , fd4_small( fd4_mkclosure(fd4_small, 0)
  , fd4_small( fd4_mkclosure(fd4_small, 0)
  , 2 ) ) ) ) ) + fd4_big(fd4_mkclosure(fd4_big, 0), 2));
  fd4_DEBUG = (void *)(1);
  fd4_x = (void *)(fd4_DEBUG
  ? fd4_f1(fd4_mkclosure(fd4_f1, 0), 2 + 3)
  : fd4_f2(fd4_mkclosure(fd4_f2, 0), 5));
  fd4_n3 = (void *)(({
    void**  fd4___clo1 = fd4_ack(fd4_mkclosure(fd4_ack, 0), 3);
    ({
      uint64_t  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
      fd4___var1(fd4___clo1, 2);
    });
  }) + fd4_n2 + fd4_x);
  fd4_printn((uint64_t)fd4_n3)
  ;
  return 0;
}