#include <stdio.h>
#include <stdlib.h>
#include <string.h>
//#include "HsFFI.h"

#define DEF_AMI_FILE "test.ami"
#define VEC_SIZE 8

int main(int argc, char *argv[]) {
    char        ami_filename[256]  = DEF_AMI_FILE;
    FILE *      ami_file           = NULL;
    char        line[256]          = "\n";
    char        ami_str[65536]     = "";
    double      impulse[VEC_SIZE]  = {0., 0.1, 0.4, 1.0, 0.9, 0.6, 0.5, 0.0};
    // AMI_Init function parameters
    double *    impulse_matrix     = impulse;
    long        row_size           = VEC_SIZE;
    long        aggressors         = 0;
    double      sample_interval    = 1.e-12;
    double      bit_time           = 100.e-12;
    char *      AMI_parameters_in  = ami_str;
    char **     AMI_parameters_out = NULL;
    void **     AMI_memory_handle  = NULL;
    char **     msg                = NULL;

    if (argc > 1)
        strcpy (ami_filename, argv[1]);
    ami_file = fopen (ami_filename, "r");
    if (!ami_file) {
        fprintf (stderr, "Couldn't open AMI file, '%s'\n", ami_filename);
        return -1;
    }
    printf ("About to scan AMI file, %s\n", ami_filename);
    while (fscanf (ami_file, "%s\n", line) > 0) {
        strcat (ami_str, line);
    }
    printf ("Finished scanning AMI file.\n");

    printf ("Calling AMI_Init.\n");
    if (!AMI_Init(
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
    printf ("Calling AMI_Close.\n");
    AMI_Close(NULL);

    return 0;
}

