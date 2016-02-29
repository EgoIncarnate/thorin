#ifndef OPENCL_2_PLATFORM_H
#define OPENCL_2_PLATFORM_H

#include "platform.h"
#include "runtime.h"
#include "opencl_utils.h"

#include <list>
#include <map>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>
#include <queue>
#include <climits>

#ifdef __APPLE__
#include <OpenCL/cl.h>
#include <OpenCL/cl_ext.h>
#else
#include <CL/cl.h>
#include <CL/cl_ext.h>
#endif

/// OpenCL2 platform. Has the same number of devices as that of the OpenCL2 implementation.
class OpenCL2Platform : public Platform {
public:
    OpenCL2Platform(Runtime* runtime);
    ~OpenCL2Platform();

protected:
    struct dim3 {
        int x, y, z;
        dim3(int x = 1, int y = 1, int z = 1) : x(x), y(y), z(z) {}
    };

    void* alloc(device_id dev, int64_t size) override;
    void* alloc_host(device_id, int64_t) override { platform_error(); return nullptr; }
    void* alloc_unified(device_id, int64_t) override { platform_error(); return nullptr; }
    void* get_device_ptr(device_id, void*) override { platform_error(); return nullptr; }

    cl_mem create_sub_buffer(cl_mem owner_buf, int64_t start_byte, int64_t region_size, cl_mem_flags flags);
    cl_mem get_svm_mem_obj(device_id dev, void* svm_mem, int64_t total_size);
    void* assign_region_to_host(device_id dev, void* svm_mem, int64_t total_size, int64_t start_byte, int64_t region_size) override;
    void* assign_region_to_device(device_id dev, void* svm_mem, int64_t total_size, int64_t start_byte, int64_t region_size) override;

    void release(device_id dev, void* ptr) override;
    void release_host(device_id, void*) override { platform_error(); }

    void set_block_size(device_id dev, int32_t x, int32_t y, int32_t z) override;
    void set_grid_size(device_id dev, int32_t x, int32_t y, int32_t z) override;
    void set_kernel_arg(device_id dev, int32_t arg, void* ptr, int32_t size) override;
    void set_kernel_arg_ptr(device_id dev, int32_t arg, void* ptr) override;
    void set_kernel_arg_struct(device_id dev, int32_t arg, void* ptr, int32_t size) override;
    void load_kernel(device_id dev, const char* file, const char* name) override;
    void launch_kernel(device_id dev) override;
    void synchronize(device_id dev) override;

    void copy(device_id dev_src, const void* src, int64_t offset_src, device_id dev_dst, void* dst, int64_t offset_dst, int64_t size) override;
    void copy_from_host(const void* src, int64_t offset_src, device_id dev_dst, void* dst, int64_t offset_dst, int64_t size) override;
    void copy_to_host(device_id dev_src, const void* src, int64_t offset_src, void* dst, int64_t offset_dst, int64_t size) override;

    int dev_count() override;

    std::string name() override { return "OpenCL 2.0"; }

    typedef std::unordered_map<std::string, cl_kernel> KernelMap;

    struct DeviceData {
        cl_platform_id platform;
        cl_device_id dev;
        cl_command_queue queue;
        cl_context ctx;
        cl_kernel kernel;

        size_t local_work_size[3], global_work_size[3];
        cl_ulong start_kernel, end_kernel;
        std::vector<void*> kernel_args;
        std::vector<void*> kernel_vals;
        std::vector<size_t> kernel_arg_sizes;
        std::list<cl_mem> kernel_structs;

        std::unordered_map<std::string, cl_program> programs;
        std::unordered_map<cl_program, KernelMap> kernels;
    };

    std::vector<DeviceData> devices_;

    std::map<void*, cl_mem> svm_to_mem;
    std::set<const void*> svm_objs;
    std::map<void*, cl_mem> host_region_to_mem;
};

#endif
