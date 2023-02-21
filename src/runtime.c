// Buraq Runtime

#include <stdio.h>

// this is the entry function to buraq
extern int boot() asm("boot");

int main() {
    printf("%d\n", boot());
    return 0;
}