extern long AMI_Init(
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

extern long AMI_Close(
    void *      AMI_memory
);
