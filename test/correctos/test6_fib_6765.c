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
uint64_t fd4_fib (void**  fd4_fib_clo, uint64_t  fd4_x) {
  return fd4_x
  ? fd4_pred(fd4_mkclosure(fd4_pred, 0), fd4_x)
    ? fd4_fib( fd4_fib_clo
      , fd4_pred(fd4_mkclosure(fd4_pred, 0), fd4_x) ) + fd4_fib( fd4_fib_clo
      , fd4_pred( fd4_mkclosure(fd4_pred, 0)
      , fd4_pred(fd4_mkclosure(fd4_pred, 0), fd4_x) ) )
    : 1
  : 0;
}
uint64_t fd4_res;
uint64_t* fd4main() {
  fd4_res = (void *)(fd4_fib(fd4_mkclosure(fd4_fib, 0), 20));
  fd4_printn((uint64_t)fd4_res)
  ;
  return 0;
}