#include <inttypes.h>
#include <wchar.h>
extern void** fd4_mkclosure(void*, int, ...);
extern void *fd4_printn(uint64_t);
extern uint64_t fd4_sub(uint64_t, uint64_t);

uint64_t fd4_x;
uint64_t fd4_y;
uint64_t fd4_f (void**  fd4_f_clo, uint64_t  fd4_y) {
  return 1 + fd4_x;
}
uint64_t fd4___suma0 (void**  fd4_clo, uint64_t  fd4_y) {
  return ({
    uint64_t  fd4_x = fd4_clo[1];
    fd4_x + fd4_y;
  });
}
void** fd4_suma (void**  fd4_suma_clo, uint64_t  fd4_x) {
  return fd4_mkclosure(fd4___suma0, 1, fd4_x);
}
void** fd4_suma5 (void**  fd4_suma5_clo, uint64_t  fd4_dummy) {
  return fd4_suma(fd4_mkclosure(fd4_suma, 0), 5);
}
uint64_t fd4_countdown (void**  fd4_countdown_clo, uint64_t  fd4_n) {
  return fd4_n ? fd4_countdown(fd4_countdown_clo, ({ fd4_sub(fd4_n, 1); })) : 0;
}
uint64_t fd4_ans;
uint64_t* fd4main() {
  fd4_x = (void *)(1);
  fd4_y = (void *)(2 + fd4_x);
  fd4_ans = (void *)(fd4_countdown(fd4_mkclosure(fd4_countdown, 0), 345) + 7);
  fd4_printn((uint64_t)fd4_ans)
  ;
  return 0;
}