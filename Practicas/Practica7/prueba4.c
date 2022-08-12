#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

uint64_t fd4_fact (void**  fd4_fact_clo, uint64_t  fd4_x) {
  return ({
    uint64_t  fd4_n = ({
      wprintf(L"x=");
      fd4_printn((uint64_t)fd4_x);
    });
    fd4_x ? fd4_fact(fd4_fact_clo, ({ fd4_sub(fd4_x, 1); })) : 1;
  });
}
void* fd4_final;
uint64_t* fd4main() {
  fd4_final = (void *)(fd4_fact(fd4_mkclosure(fd4_fact, 0), 5));
  fd4_printn((uint64_t)fd4_final)
  ;
  return 0;
}