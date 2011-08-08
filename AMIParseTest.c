#include <stdio.h>
#include <stdlib.h>
#include "HsFFI.h"

#ifdef __GLASGOW_HASKELL__
    #include "AMIParse_stub.h"
#endif

#ifdef __GLASGOW_HASKELL__
    extern void __stginit_AMIParse ( void );
#endif

int main(int argc, char *argv[]) {
    hs_init(&argc, &argv);
    #ifdef __GLASGOW_HASKELL__
        hs_add_root(__stginit_AMIParse);
    #endif

    if (argc < 2) {
        printf ("Sorry, need an input number.\n");
        return -1;
    } else {
        printf ("You gave: %d\n", test(atoi(argv[1])));
        return 0;
    }

    hs_exit();
    return 0;
}

