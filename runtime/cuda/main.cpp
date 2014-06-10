#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "cu_runtime.h"

static void nnvm_load_any_kernel(size_t dev, const char *file, const char *kernel)
{
    size_t len = strlen(file);
    if(len > 3 && !strcmp(file+len-3, ".cu")) // hack: nvcc checks for .cu extension, so we just do the same here
        nvvm_load_cuda_kernel(dev, file, kernel);
    else
        nvvm_load_nvvm_kernel(dev, file, kernel);
}

static int num = 1024;

int test_kernelfile(const char *file)
{
    printf("Test file: %s\n", file);

    size_t dev = 1;
    int *host = (int *)thorin_malloc(sizeof(int) * num);

    for (unsigned int i=0; i<num; ++i) {
        host[i] = 0;
    }

    // CODE TO BE GENERATED: BEGIN
    mem_id mem = nvvm_malloc_memory(dev, host);
    nvvm_write_memory(dev, mem, host);

    nnvm_load_any_kernel(dev, file, "simple");
    nvvm_set_kernel_arg_map(dev, mem);
    nvvm_set_problem_size(dev, 1024, 1, 1);
    nvvm_set_config_size(dev, 128, 1, 1);
    nvvm_launch_kernel(dev, "simple");
    nvvm_synchronize(dev); // optional
    nvvm_read_memory(dev, mem, host);
    nvvm_free_memory(dev, mem);
    // CODE TO BE GENERATED: END

    // check result
    for (unsigned int i=0; i<num; ++i) {
        if (host[i] != i) {
            printf("Test failed!\n");
            return EXIT_FAILURE;
        }
    }
    printf("Test passed!\n");

    for (unsigned int i=0; i<num; ++i) {
        host[i] = i;
    }


    // CODE TO BE GENERATED: BEGIN
    CUdeviceptr tex = nvvm_malloc_memory(dev, host);
    nvvm_write_memory(dev, tex, host);

    int *host_out = (int *)thorin_malloc(sizeof(int) * num);
    CUdeviceptr out = nvvm_malloc_memory(dev, host_out);
    for (unsigned int i=0; i<num; ++i) {
        host_out[i] = 0;
    }
    nvvm_write_memory(dev, out, host_out);

    nnvm_load_any_kernel(dev, file, "simple_tex");
    nvvm_set_kernel_arg_tex(dev, tex, "tex", CU_AD_FORMAT_SIGNED_INT32);
    nvvm_set_kernel_arg_map(dev, out);
    nvvm_set_problem_size(dev, 1024, 1, 1);
    nvvm_set_config_size(dev, 128, 1, 1);
    nvvm_launch_kernel(dev, "simple");
    nvvm_synchronize(dev); // optional
    nvvm_read_memory(dev, out, host_out);
    nvvm_free_memory(dev, out);
    nvvm_free_memory(dev, tex);
    // CODE TO BE GENERATED: END

    // check result
    for (unsigned int i=0; i<num; ++i) {
        if (host_out[i] != i) {
            printf("Texture test failed!\n");
            return EXIT_FAILURE;
        }
    }
    printf("Texture test passed!\n");

    thorin_free(host);
    thorin_free(host_out);

    return EXIT_SUCCESS;
}

extern "C" { int main_impala(void); }
int main_impala() {
    int ret = test_kernelfile("simple-gpu64.cu");
    if(ret == EXIT_SUCCESS)
        ret = test_kernelfile("simple-gpu64.nvvm");
    return ret;
}

