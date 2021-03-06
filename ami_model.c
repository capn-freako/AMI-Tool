#include <stdio.h>
#include <stdlib.h>
#include "HsFFI.h"
//#include <Rts.h>

#ifdef __GLASGOW_HASKELL__
    #include "AMIModel_stub.h"
#endif

#ifdef __GLASGOW_HASKELL__
    extern void __stginit_AMIModel ( void );
#endif

// Helper functions, to start up and shut down the Haskell RTS.
int hs_started = 0;

void HsStart()
{
    int argc = 1;
    char* argv[] = {"ghcDll",
//                    "+RTS",
//                    "-hc",
//                    "-p",
//                    "-K100M",
                    NULL}; // argv must end with NULL

    // Initialize Haskell runtime
    char** args = argv;
    hs_init(&argc, &args);

    // Tell Haskell about all root modules
    #ifdef __GLASGOW_HASKELL__
        hs_add_root(__stginit_AMIModel);
    #endif

    // Set global flag.
    hs_started = 1;
}

void HsEnd()
{
    hs_exit();
    hs_started = 0;
}

// The IBIS AMI interface proper.
long AMI_Init(
    double *    impulse_matrix,
    long        row_size,
    long        aggressors,
    double      sample_interval,
    double      bit_time,
    char *      AMI_parameters_in,
    char **     AMI_parameters_out,
    void **     AMI_memory_handle,
    char **     msg
) {
    long res;

    // Before doing anything else, get the Haskell run-time system going.
    if (!hs_started)
        HsStart();

    // Call the Haskell function.
    res = amiInit(
        impulse_matrix,
        row_size,
        aggressors,
        sample_interval,
        bit_time,
        AMI_parameters_in,
        AMI_parameters_out,
        AMI_memory_handle,
        msg
    );

    return res;
}

long AMI_GetWave(
    double *    wave_in,
    long        wave_size,
    double *    clock_times,
    char **     AMI_parameters_out,
    void *      AMI_memory
) {
    long res;

    // Call the Haskell function.
    res = amiGetWave(
        wave_in,
        wave_size,
        clock_times,
        AMI_parameters_out,
        AMI_memory
    );

    return res;
}

long AMI_Close(
    void *      AMI_memory
) {
    if( AMI_memory ) {
        amiClose( AMI_memory );
    }

    // After cleaning up everything else, shut down the Haskell RTS.
    if (hs_started)
        HsEnd();

    // Nothing should follow this line, except 'return'.
    return 1;
}

