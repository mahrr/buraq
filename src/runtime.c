// Buraq Runtime

#include <stdio.h>

// this is the entry function to buraq
extern double boot() asm("boot");

int main() {
    printf("%g\n", boot());
    return 0;
}