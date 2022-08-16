#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

void* fd4_const;
uint64_t fd4___sum3 (void**  fd4_clo, uint64_t  fd4_z) {
  return ({
    uint64_t  fd4_v = fd4_clo[4];
    ({
      uint64_t  fd4_w = fd4_clo[3];
      ({
        uint64_t  fd4_x = fd4_clo[2];
        ({
          uint64_t  fd4_y = fd4_clo[1];
          fd4_v + fd4_w + fd4_x + fd4_y + fd4_z + fd4_const;
        });
      });
    });
  });
}
void** fd4___sum2 (void**  fd4_clo, uint64_t  fd4_y) {
  return ({
    uint64_t  fd4_v = fd4_clo[3];
    ({
      uint64_t  fd4_w = fd4_clo[2];
      ({
        uint64_t  fd4_x = fd4_clo[1];
        fd4_mkclosure(fd4___sum3, 4, fd4_y, fd4_x, fd4_w, fd4_v);
      });
    });
  });
}
void** fd4___sum1 (void**  fd4_clo, uint64_t  fd4_x) {
  return ({
    uint64_t  fd4_v = fd4_clo[2];
    ({
      uint64_t  fd4_w = fd4_clo[1];
      fd4_mkclosure(fd4___sum2, 3, fd4_x, fd4_w, fd4_v);
    });
  });
}
void** fd4___sum0 (void**  fd4_clo, uint64_t  fd4_w) {
  return ({
    uint64_t  fd4_v = fd4_clo[1];
    fd4_mkclosure(fd4___sum1, 2, fd4_w, fd4_v);
  });
}
void** fd4_sum (void**  fd4_sum_clo, uint64_t  fd4_v) {
  return fd4_mkclosure(fd4___sum0, 1, fd4_v);
}
void** fd4_sumatemp1 (void**  fd4_sumatemp1_clo, uint64_t  fd4_dummy) {
  return fd4_sum(fd4_mkclosure(fd4_sum, 0), 3);
}
void** fd4_sumatemp2 (void**  fd4_sumatemp2_clo, uint64_t  fd4_dummy) {
  return ({
    void**  fd4___clo0 = fd4_sumatemp1(fd4_mkclosure(fd4_sumatemp1, 0), 0);
    ({
      void**  (* fd4___var0) (void**,  uint64_t  ) = fd4___clo0[0];
      fd4___var0(fd4___clo0, 2);
    });
  });
}
void** fd4_sumatemp3 (void**  fd4_sumatemp3_clo, uint64_t  fd4_dummy) {
  return ({
    void**  fd4___clo0 = fd4_sumatemp2(fd4_mkclosure(fd4_sumatemp2, 0), 0);
    ({
      void**  (* fd4___var0) (void**,  uint64_t  ) = fd4___clo0[0];
      fd4___var0(fd4___clo0, 4);
    });
  });
}
void** fd4_sumatemp4 (void**  fd4_sumatemp4_clo, uint64_t  fd4_dummy) {
  return ({
    void**  fd4___clo0 = fd4_sumatemp3(fd4_mkclosure(fd4_sumatemp3, 0), 0);
    ({
      void**  (* fd4___var0) (void**,  uint64_t  ) = fd4___clo0[0];
      fd4___var0(fd4___clo0, 6);
    });
  });
}
void* fd4_final;
void** fd4_sumatemp5 (void**  fd4_sumatemp5_clo, uint64_t  fd4_dummy) {
  return ({
    void**  fd4___clo2 = ({
      void**  fd4___clo1 = fd4_sum(fd4_mkclosure(fd4_sum, 0), 8);
      ({
        void**  (* fd4___var1) (void**,  uint64_t  ) = fd4___clo1[0];
        fd4___var1(fd4___clo1, 9);
      });
    });
    ({
      void**  (* fd4___var2) (void**,  uint64_t  ) = fd4___clo2[0];
      fd4___var2(fd4___clo2, 20);
    });
  });
}
void** fd4_sumatemp6 (void**  fd4_sumatemp6_clo, uint64_t  fd4_dummy) {
  return ({
    void**  fd4___clo0 = fd4_sumatemp5(fd4_mkclosure(fd4_sumatemp5, 0), 0);
    ({
      void**  (* fd4___var0) (void**,  uint64_t  ) = fd4___clo0[0];
      fd4___var0(fd4___clo0, 4);
    });
  });
}
void* fd4_final2;
uint64_t* fd4main() {
  fd4_const = (void *)(205);
  fd4_final = (void *)(({
    void**  fd4___clo0 = fd4_sumatemp4(fd4_mkclosure(fd4_sumatemp4, 0), 0);
    ({
      uint64_t  (* fd4___var0) (void**,  uint64_t  ) = fd4___clo0[0];
      fd4___var0(fd4___clo0, 7);
    });
  }));
  fd4_final2 = (void *)(({
    void**  fd4___clo0 = fd4_sumatemp6(fd4_mkclosure(fd4_sumatemp6, 0), 0);
    ({
      uint64_t  (* fd4___var0) (void**,  uint64_t  ) = fd4___clo0[0];
      fd4___var0(fd4___clo0, 7);
    });
  }));
  fd4_printn((uint64_t)fd4_final);
  fd4_printn((uint64_t)fd4_final2)
  ;
  return 0;
}