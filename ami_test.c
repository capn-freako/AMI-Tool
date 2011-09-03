#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>
//#include "HsFFI.h"
//#include "ami_model.h"

#define DEF_AMI_FILE "test.ami"
#define VEC_SIZE       8
#define MAX_LINE_LEN 256

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
    long (*AMI_Close)(
        void *      AMI_memory
    );
    char        ami_filename[256]  = DEF_AMI_FILE;
    FILE *      ami_file           = NULL;
    char        line[MAX_LINE_LEN] = "\n";
    char        ami_str[65536]     = "";
    double      impulse[VEC_SIZE]  = {0., 0.1, 0.4, 1.0, 0.9, 0.6, 0.5, 0.0};
    char *      msgPtr             = NULL;
    char *      parmsOut           = NULL;
    void *      memPtr             = NULL;
    // AMI_Init function parameters
    double *    impulse_matrix     = impulse;
    long        row_size           = VEC_SIZE;
    long        aggressors         = 0;
    double      sample_interval    = 1.e-12;
    double      bit_time           = 100.e-12;
    char *      AMI_parameters_in  = ami_str;
    char **     AMI_parameters_out = &parmsOut;
    void **     AMI_memory_handle  = &memPtr;
    char **     msg                = &msgPtr;

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

    lib_handle = dlopen("libami.so", RTLD_LAZY);
    if (!lib_handle) {
        fprintf(stderr, "%s\n", dlerror());
        exit(1);
    }

    AMI_Init = dlsym(lib_handle, "AMI_Init");
    if ((error = dlerror()) != NULL)  {
        fprintf(stderr, "%s\n", error);
        exit(1);
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

    if (msgPtr)
        printf ("Received this message from AMI_Init: %s\n", msgPtr);
    else
        printf ("Received no message from AMI_Init.\n");

    if (parmsOut)
        printf ("Parameters from AMI_Init:\n%s\n", parmsOut);
    else
        printf ("No parameters from AMI_Init.\n");

    (*AMI_Close)(memPtr);

    dlclose(lib_handle);
    return 0;
}

