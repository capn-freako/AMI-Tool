// ami_test.c - utility for testing my Haskell IBIS-AMI model
//
// ami_test <AMI file> <# GetWave calls>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>
//#include "HsFFI.h"
//#include "ami_model.h"

#define DEF_AMI_FILE "test.ami"
#define VEC_SIZE     128
#define MAX_LINE_LEN 256
#define SMPL_RATE    40e9
#define BIT_RATE      5e9
#define SAMPS_PER_BIT (SMPL_RATE / BIT_RATE)
#define PRBS_LEN     127
#define PRBS_TAPS    0x10010000 // `[7, 4]' gold code feedback pattern
#define DO_IMPULSE   0

int bitcount (unsigned x) {
    int b;

    for (b = 0; x != 0; x >>= 1)
        if (x & 01)
            b++;
    return b;
}

void gen_prbs (int *res, int len) {
    unsigned     accum = 0xFFFFFFFF;
    int     i;

    for (i=0; i<len; i++) {
        accum = (accum << 1) | (bitcount (accum & PRBS_TAPS) % 2);
        res[i] = (accum % 2) * 2 - 1; // from [0,1] to [-1,1]
    }
}

int main(int argc, char **argv) {
    void *lib_handle;
    char *error;
    long (*AMI_Init)(
        double *    impulse_matrix,
        long        row_size,
        long        aggressors,
        double      sample_interval,
        double      bit_time,
        char *      AMI_parameters_in,
        char **     AMI_parameters_out,
        void **     AMI_memory_handle,
        char **     msg
    );
    long (*AMI_GetWave)(
        double *    wave_in,
        long        wave_size,
        double *    clock_times,
        char **     AMI_parameters_out,
        void *      AMI_memory
    );
    long (*AMI_Close)(
        void *      AMI_memory
    );
    char        ami_filename[256]  = DEF_AMI_FILE;
    FILE *      ami_file           = NULL;
    char        line[MAX_LINE_LEN] = "\n";
    char        ami_str[65536]     = "";
    double      impulse[VEC_SIZE]  = {0., 1., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
                                      0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
                                      0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
                                      0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
                                      0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
                                      0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
                                      0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
                                      0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.
                                     };
    char *      msg_ptr            = NULL;
    char *      parms_out          = NULL;
    void *      mem_ptr            = NULL;
    int         num_bits           = 0;
    int         prbs[PRBS_LEN];
    int         bits_per_vec       = 0;
    double      wave[VEC_SIZE]     = {0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
                                      0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
                                      0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
                                      0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
                                      0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
                                      0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
                                      0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
                                      0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.
                                     };
    int         i, j, k, l;
    int         bit;

    // AMI_Init function parameters
    double *    impulse_matrix     = impulse;
    long        row_size           = VEC_SIZE;
    long        aggressors         = 0;
    double      sample_interval    = 1 / SMPL_RATE;
    double      bit_time           = 1 / BIT_RATE;
    char *      AMI_parameters_in  = ami_str;
    char **     AMI_parameters_out = &parms_out;
    void **     AMI_memory_handle  = &mem_ptr;
    char **     msg                = &msg_ptr;

    // AMI_GetWave function parameters
    double *    wave_in            = wave;
    long        wave_size          = VEC_SIZE;
    double      clock_times[VEC_SIZE];
    void *      AMI_memory         = mem_ptr;

    if (argc > 1)
        strcpy (ami_filename, argv[1]);
    ami_file = fopen (ami_filename, "rt");
    if (!ami_file) {
        fprintf (stderr, "Couldn't open AMI file, '%s'\n", ami_filename);
        return -1;
    }
    while (fgets (line, MAX_LINE_LEN, ami_file)) {
        strcat (ami_str, line);
    }

    if (argc > 2)
        sscanf (argv[2], "%d", &num_bits);

    lib_handle = dlopen("libami.so", RTLD_LAZY);
//    lib_handle = dlopen("arria5_rx.linux.so", RTLD_LAZY);
    if (!lib_handle) {
        fprintf(stderr, "%s\n", dlerror());
        exit(1);
    }

    AMI_Init = dlsym(lib_handle, "AMI_Init");
    if ((error = dlerror()) != NULL)  {
        fprintf(stderr, "%s\n", error);
        exit(1);
    }

    if (num_bits) {
        AMI_GetWave = dlsym(lib_handle, "AMI_GetWave");
        if ((error = dlerror()) != NULL)  {
            fprintf(stderr, "%s\n", error);
            exit(1);
        }
    }

    AMI_Close = dlsym(lib_handle, "AMI_Close");
    if ((error = dlerror()) != NULL)  {
        fprintf(stderr, "%s\n", error);
        exit(1);
    }

    if (!(*AMI_Init)(
        impulse_matrix,
        row_size,
        aggressors,
        sample_interval,
        bit_time,
        AMI_parameters_in,
        AMI_parameters_out,
        AMI_memory_handle,
        msg))
            printf ("Error: AMI_Init call unsuccessful!\n");

    if (msg_ptr)
        printf ("Received this message from AMI_Init: %s\n", msg_ptr);
    else
        printf ("Received no message from AMI_Init.\n");

    if (parms_out)
        printf ("Parameters from AMI_Init:\n%s\n", parms_out);
    else
        printf ("No parameters from AMI_Init.\n");

    if (DO_IMPULSE) {
	printf ("Impulse Response:\n");
	for (i=0; i<VEC_SIZE; i++) {
	    printf ("%6.3e, %6.3e\n", i * sample_interval, impulse[i]);
	}
	printf ("\n");
    }

    if (num_bits) {
        printf ("GetWave Output:\n");
        gen_prbs(prbs, PRBS_LEN);
        bits_per_vec = VEC_SIZE/SAMPS_PER_BIT;
        for (i=0; i<num_bits; i+=bits_per_vec) {
            for (j=0; j<VEC_SIZE; j++)
                wave[j] = 0;
            for (j=0; j<bits_per_vec; j++) {
                bit = prbs[(i + j) % PRBS_LEN];
                for (k=j*SAMPS_PER_BIT, l=0; k<VEC_SIZE; k++, l++)
                    wave[k] += bit * impulse[l];
            }
            if (!(*AMI_GetWave)(
                wave_in,
                wave_size,
                clock_times,
                AMI_parameters_out,
                mem_ptr
            ))
                printf ("Error: AMI_GetWave call unsuccessful!\n");
            for (j=0; j<VEC_SIZE; j++)
                printf ("%6.3e, %6.3e\n", (i*VEC_SIZE + j) * sample_interval, wave[j]);
        }
    }

    (*AMI_Close)(mem_ptr);

    dlclose(lib_handle);
    return 0;
}

