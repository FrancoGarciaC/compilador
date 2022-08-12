#include <inttypes.h>
#include <wchar.h>
extern void *fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern void *fd4_sub(uint64_t, uint64_t);

uint64_t fd4___final0 (void**  fd4_clo, uint64_t  fd4_v) {
  return ({
    wprintf(L"x+y+z+w+v:=");
    fd4_printn((uint64_t)fd4_v);
  });
}
void* fd4_final;
uint64_t* fd4main() {
  fd4_final = (void *)(({
    uint64_t  (* fd4_mapF) (void**,  uint64_t  ) = fd4_mkclosure( fd4___final0
    , 0 );
    fd4_mapF(fd4_mapF_clo, 3);
  }));
  fd4_printn((uint64_t)fd4_final)
  ;
  return 0;
}