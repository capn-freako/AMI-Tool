#include <stdio.h>
#include <stdlib.h>
#include <string.h>
//#include "HsFFI.h"

#define DEF_AMI_FILE "test.ami"
#define VEC_SIZE       8
#define MAX_LINE_LEN 256

int main(int argc, char *argv[]) {
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

    if (msgPtr)
        printf ("Received this message from AMI_Init: %s\n", msgPtr);
    else
        printf ("Received no message from AMI_Init.\n");

    if (parmsOut)
        printf ("Parameters from AMI_Init:\n%s\n", parmsOut);
    else
        printf ("No parameters from AMI_Init.\n");

    AMI_Close(memPtr);

    return 0;
}

