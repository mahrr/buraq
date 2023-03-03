// Buraq Runtime

#include <stdio.h>
#include <stdint.h>

// this is the entry function to buraq
extern int64_t boot() asm("boot");

int main() {
    printf("%lld\n", boot());
    return 0;
}