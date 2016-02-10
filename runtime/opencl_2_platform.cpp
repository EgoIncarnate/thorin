#include "opencl_2_platform.h"
#include "runtime.h"

#include <algorithm>
#include <atomic>
#include <cassert>
#include <cstdlib>
#include <fstream>
#include <string>
#include <cstring>
#ifndef KERNEL_DIR
#define KERNEL_DIR ""
#endif

OpenCL2Platform::OpenCL2Platform(Runtime* runtime)
    : Platform(runtime)
{
    // get OpenCL platform count
    cl_uint num_platforms, num_devices;
    cl_int err = clGetPlatformIDs(0, NULL, &num_platforms);
    checkErr(err, "clGetPlatformIDs()");

    WLOG("Number of available OpenCL Platforms: %", num_platforms);

    cl_platform_id* platforms = new cl_platform_id[num_platforms];

    err = clGetPlatformIDs(num_platforms, platforms, NULL);
    checkErr(err, "clGetPlatformIDs()");

    // get platform info for each platform
    for (size_t i=0; i<num_platforms; ++i) {
        auto platform = platforms[i];

        char buffer[1024];
        err  = clGetPlatformInfo(platforms[i], CL_PLATFORM_NAME, sizeof(buffer), &buffer, NULL);
        WLOG("  Platform Name: %", buffer);
        err |= clGetPlatformInfo(platforms[i], CL_PLATFORM_VENDOR, sizeof(buffer), &buffer, NULL);
        WLOG("  Platform Vendor: %", buffer);
        err |= clGetPlatformInfo(platforms[i], CL_PLATFORM_VERSION, sizeof(buffer), &buffer, NULL);
        WLOG("  Platform Version: %", buffer);
        checkErr(err, "clGetPlatformInfo()");

        // only consider platforms that support OpenCL 2.0 or higher
        if(strstr(buffer, "OpenCL 2.") != NULL)
        {
            err = clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_ALL, 0, NULL, &num_devices);
            checkErr(err, "clGetDeviceIDs()");

            cl_device_id* devices = new cl_device_id[num_devices];
            err = clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_ALL, num_devices, devices, &num_devices);
            checkErr(err, "clGetDeviceIDs()");

            // get device info for each device
            for (size_t j=0; j<num_devices; ++j) {
                auto device = devices[j];
                cl_device_type dev_type;
                cl_uint device_vendor_id;
            
                err  = clGetDeviceInfo(devices[j], CL_DEVICE_NAME, sizeof(buffer), &buffer, NULL);
                err |= clGetDeviceInfo(devices[j], CL_DEVICE_TYPE, sizeof(dev_type), &dev_type, NULL);
                std::string type_str;
                if (dev_type & CL_DEVICE_TYPE_CPU)         type_str  = "CL_DEVICE_TYPE_CPU";
                if (dev_type & CL_DEVICE_TYPE_GPU)         type_str  = "CL_DEVICE_TYPE_GPU";
                if (dev_type & CL_DEVICE_TYPE_ACCELERATOR) type_str  = "CL_DEVICE_TYPE_ACCELERATOR";
#ifdef CL_VERSION_1_2
                if (dev_type & CL_DEVICE_TYPE_CUSTOM)      type_str  = "CL_DEVICE_TYPE_CUSTOM";
#endif
                if (dev_type & CL_DEVICE_TYPE_DEFAULT)     type_str += "|CL_DEVICE_TYPE_DEFAULT";
                WLOG("  (%) Device Name: % (%)", devices_.size(), buffer, type_str);
                err |= clGetDeviceInfo(devices[j], CL_DEVICE_VENDOR, sizeof(buffer), &buffer, NULL);
                err |= clGetDeviceInfo(devices[j], CL_DEVICE_VENDOR_ID, sizeof(device_vendor_id), &device_vendor_id, NULL);
                WLOG("      Device Vendor: %", buffer, " (ID: ", device_vendor_id, ")");
                err |= clGetDeviceInfo(devices[j], CL_DEVICE_VERSION, sizeof(buffer), &buffer, NULL);
                WLOG("      Device OpenCL Version: %", buffer);

                assert(strstr(buffer, "OpenCL 2.") != NULL);

                err |= clGetDeviceInfo(devices[j], CL_DRIVER_VERSION, sizeof(buffer), &buffer, NULL);
                WLOG("      Device Driver Version: %", buffer);
                err |= clGetDeviceInfo(devices[j], CL_DEVICE_EXTENSIONS, sizeof(buffer), &buffer, NULL);
                //WLOG("      Device Extensions: %", buffer);
                std::string extensions(buffer);
                bool has_spir = extensions.find("cl_khr_spir") != std::string::npos;
                std::string spir_version;
#ifdef CL_DEVICE_SPIR_VERSIONS
                if (has_spir) {
                    err |= clGetDeviceInfo(devices[j], CL_DEVICE_SPIR_VERSIONS , sizeof(buffer), &buffer, NULL);
                    spir_version = "(Version: " + std::string(buffer) + ")";
                }
#endif
                WLOG("      Device SPIR Support: % %", has_spir, spir_version);
                
                std::string svm_caps_str;
                cl_device_svm_capabilities svm_caps;
                err |= clGetDeviceInfo(devices[j], CL_DEVICE_SVM_CAPABILITIES, sizeof(svm_caps), &svm_caps, NULL);
                if (svm_caps & CL_DEVICE_SVM_COARSE_GRAIN_BUFFER) svm_caps_str += " CL_DEVICE_SVM_COARSE_GRAIN_BUFFER";
                else                                              svm_caps_str += " n/a";
                if (svm_caps & CL_DEVICE_SVM_FINE_GRAIN_BUFFER)   svm_caps_str += " CL_DEVICE_SVM_FINE_GRAIN_BUFFER";
                if (svm_caps & CL_DEVICE_SVM_FINE_GRAIN_SYSTEM)   svm_caps_str += " CL_DEVICE_SVM_FINE_GRAIN_SYSTEM";
                if (svm_caps & CL_DEVICE_SVM_ATOMICS)             svm_caps_str += " CL_DEVICE_SVM_ATOMICS";
                WLOG("      Device SVM capabilities:%", svm_caps_str);
                checkErr(err, "clGetDeviceInfo()");

                auto dev = devices_.size();
                devices_.resize(dev + 1);
                devices_[dev].platform = platform;
                devices_[dev].dev = device;

                // create context
                cl_context_properties ctx_props[3] = { CL_CONTEXT_PLATFORM, (cl_context_properties)platform, 0 };
                devices_[dev].ctx = clCreateContext(ctx_props, 1, &devices_[dev].dev, NULL, NULL, &err);
                checkErr(err, "clCreateContext()");

                // create command queue
                cl_queue_properties queue_props[3] = { CL_QUEUE_PROPERTIES, CL_QUEUE_PROFILING_ENABLE, 0 };
                devices_[dev].queue = clCreateCommandQueueWithProperties(devices_[dev].ctx, devices_[dev].dev, queue_props, &err);
                checkErr(err, "clCreateCommandQueueWithProperties()");
            }
            delete[] devices;
        }
        else
        {
            WLOG("OpenCL 2.0 is not supported, skipping platform");
        }
    }
    delete[] platforms;

    latest_kernel_data.is_waiting_kernel = false;
}

OpenCL2Platform::~OpenCL2Platform() {
    for (size_t i = 0; i < devices_.size(); i++) {
        for (auto& map : devices_[i].kernels) {
            for (auto& it : map.second) {
                cl_int err = clReleaseKernel(it.second);
                checkErr(err, "clReleaseKernel()");
            }
        }
        for (auto& it : devices_[i].programs) {
            cl_int err = clReleaseProgram(it.second);
            checkErr(err, "clReleaseProgram()");
        }
    }
}

void* OpenCL2Platform::alloc(device_id dev, int64_t size) {
    if (!size) return 0;
    
    cl_svm_mem_flags flags = CL_MEM_READ_WRITE | CL_MEM_SVM_FINE_GRAIN_BUFFER
        | CL_MEM_SVM_ATOMICS;
    void* mem = clSVMAlloc(devices_[dev].ctx, flags, size, 0);
    checkAllocation(mem);
    
    return mem;
}

void OpenCL2Platform::release(device_id dev, void* ptr) {
    clSVMFree(devices_[dev].ctx, ptr);
}

void OpenCL2Platform::set_block_size(device_id dev, int32_t x, int32_t y, int32_t z) {
    auto& local_work_size = devices_[dev].local_work_size;
    local_work_size[0] = x;
    local_work_size[1] = y;
    local_work_size[2] = z;
}

void OpenCL2Platform::set_grid_size(device_id dev, int32_t x, int32_t y, int32_t z) {
    auto& global_work_size = devices_[dev].global_work_size;
    global_work_size[0] = x;
    global_work_size[1] = y;
    global_work_size[2] = z;
}

void OpenCL2Platform::set_kernel_arg(device_id dev, int32_t arg, void* ptr, int32_t size) {
    
    auto& args = devices_[dev].kernel_args;
    auto& sizs = devices_[dev].kernel_arg_sizes;
    args.resize(std::max(arg + 1, (int32_t)args.size()));
    sizs.resize(std::max(arg + 1, (int32_t)sizs.size()));
    args[arg] = ptr;
    sizs[arg] = size;
}

void OpenCL2Platform::set_kernel_arg_ptr(device_id dev, int32_t arg, void* ptr) {
    
    auto& vals = devices_[dev].kernel_vals;
    auto& args = devices_[dev].kernel_args;
    auto& sizs = devices_[dev].kernel_arg_sizes;
    vals.resize(std::max(arg + 1, (int32_t)vals.size()));
    args.resize(std::max(arg + 1, (int32_t)args.size()));
    sizs.resize(std::max(arg + 1, (int32_t)sizs.size()));
    vals[arg] = ptr;
    args[arg] = nullptr;
    sizs[arg] = sizeof(cl_mem);
}
 
void OpenCL2Platform::set_kernel_arg_struct(device_id dev, int32_t arg, void* ptr, int32_t size) {
    cl_int err = CL_SUCCESS;
    cl_mem_flags flags = CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR;
    cl_mem struct_buf = clCreateBuffer(devices_[dev].ctx, flags, size, ptr, &err);
    checkErr(err, "clCreateBuffer()");
    devices_[dev].kernel_structs.emplace_back(struct_buf);
    cl_mem& buf = devices_[dev].kernel_structs.back();
    set_kernel_arg_ptr(dev, arg, (void*)buf);
}

void OpenCL2Platform::load_kernel(device_id dev, const char* file, const char* name) {
    cl_int err = CL_SUCCESS;
    cl_program program;
    auto& prog_cache = devices_[dev].programs;
    auto prog_it = prog_cache.find(file);
    if (prog_it == prog_cache.end()) {
        std::string spir_bc("spir.bc");
        std::string file_str(file);
        std::ifstream src_file(std::string(KERNEL_DIR) + file);
        bool is_binary = file_str.compare(file_str.length() - spir_bc.length(), spir_bc.length(), spir_bc) == 0;

        if (!src_file.is_open()) {
            ELOG("Can't open % file '%'!", (is_binary ? "SPIR binary" : "OpenCL source"), name);
        }

        WLOG("Compiling '%' on OpenCL device %", file, dev);

        std::string cl_str(std::istreambuf_iterator<char>(src_file), (std::istreambuf_iterator<char>()));
        std::string options = "-cl-fast-relaxed-math";

        const size_t length = cl_str.length();
        const char* c_str = cl_str.c_str();

        if (is_binary) {
            options += " -x spir -spir-std=1.2";
            program = clCreateProgramWithBinary(devices_[dev].ctx, 1, &devices_[dev].dev, &length, (const unsigned char**)&c_str, NULL, &err);
            checkErr(err, "clCreateProgramWithBinary()");
        } else {
            program = clCreateProgramWithSource(devices_[dev].ctx, 1, (const char**)&c_str, &length, &err);
            checkErr(err, "clCreateProgramWithSource()");
        }

        err = clBuildProgram(program, 0, NULL, options.c_str(), NULL, NULL);

        cl_build_status build_status;
        clGetProgramBuildInfo(program, devices_[dev].dev, CL_PROGRAM_BUILD_STATUS, sizeof(build_status), &build_status, NULL);

        if (build_status == CL_BUILD_ERROR || err != CL_SUCCESS) {
            // determine the size of the options and log
            size_t log_size, options_size;
            err |= clGetProgramBuildInfo(program, devices_[dev].dev, CL_PROGRAM_BUILD_OPTIONS, 0, NULL, &options_size);
            err |= clGetProgramBuildInfo(program, devices_[dev].dev, CL_PROGRAM_BUILD_LOG, 0, NULL, &log_size);

            // allocate memory for the options and log
            char* program_build_options = new char[options_size];
            char* program_build_log = new char[log_size];

            // get the options and log
            err |= clGetProgramBuildInfo(program, devices_[dev].dev, CL_PROGRAM_BUILD_OPTIONS, options_size, program_build_options, NULL);
            err |= clGetProgramBuildInfo(program, devices_[dev].dev, CL_PROGRAM_BUILD_LOG, log_size, program_build_log, NULL);
            WLOG("OpenCL build options : %", program_build_options);
            WLOG("OpenCL build log : %", program_build_log);

            // free memory for options and log
            delete[] program_build_options;
            delete[] program_build_log;
        }
        checkErr(err, "clBuildProgram(), clGetProgramBuildInfo()");

        prog_cache[file] = program;
    } else {
        program = prog_it->second;
    }

    // checks that the kernel exists
    auto& kernel_cache = devices_[dev].kernels;
    auto& kernel_map = kernel_cache[program];
    auto kernel_it = kernel_map.find(name);
    if (kernel_it == kernel_map.end()) {
        devices_[dev].kernel = clCreateKernel(program, name, &err);
        checkErr(err, "clCreateKernel()");
        kernel_map.emplace(name, devices_[dev].kernel);
        devices_[dev].kernel = devices_[dev].kernel;
    } else {
        devices_[dev].kernel = kernel_it->second;
    }
}

extern std::atomic_llong thorin_kernel_time;

void OpenCL2Platform::launch_kernel(device_id dev) {
    cl_int err = CL_SUCCESS;
    cl_event event;

    // set up arguments
    auto& args = devices_[dev].kernel_args;
    auto& vals = devices_[dev].kernel_vals;
    auto& sizs = devices_[dev].kernel_arg_sizes;
    for (size_t i = 0; i < args.size(); i++) {

        cl_int err;
        
        // set the arguments pointers
        if (!args[i])
        {
            args[i] = vals[i];
            err = clSetKernelArgSVMPointer(devices_[dev].kernel, i, args[i]);
            checkErr(err, "clSetKernelArgSVMPointer()");
        }
        else
        {
            err = clSetKernelArg(devices_[dev].kernel, i, sizs[i], args[i]);
            checkErr(err, "clSetKernelArg()");
        }
    }
    args.clear();
    vals.clear();
    sizs.clear();

    // launch the kernel
    err = clEnqueueNDRangeKernel(devices_[dev].queue, devices_[dev].kernel, 2, NULL, devices_[dev].global_work_size, devices_[dev].local_work_size, 0, NULL, &event);
    checkErr(err, "clEnqueueNDRangeKernel()");

    latest_kernel_data.is_waiting_kernel = true;
    latest_kernel_data.ndrange_evt = event;
}

void OpenCL2Platform::synchronize(device_id dev) {
    cl_int err = clFinish(devices_[dev].queue);
    checkErr(err, "clFinish()");

    if(latest_kernel_data.is_waiting_kernel)
    {
        cl_event event = latest_kernel_data.ndrange_evt;
        cl_ulong end, start;
        float time;

        err = clWaitForEvents(1, &event);
        checkErr(err, "clWaitForEvents()");
        err = clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_END, sizeof(cl_ulong), &end, 0);
        err |= clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_START, sizeof(cl_ulong), &start, 0);
        checkErr(err, "clGetEventProfilingInfo()");
        time = (end-start)*1.0e-6f;
        thorin_kernel_time.fetch_add(time * 1000);

        err = clReleaseEvent(event);
        checkErr(err, "clReleaseEvent()");

        // release temporary buffers for struct arguments
        for (cl_mem buf : devices_[dev].kernel_structs)
            release(dev, buf);
        devices_[dev].kernel_structs.clear();

        latest_kernel_data.is_waiting_kernel = false;
    }
}

void OpenCL2Platform::copy(device_id dev_src, const void* src, int64_t offset_src, device_id dev_dst, void* dst, int64_t offset_dst, int64_t size) {
    assert(dev_src == dev_dst);

    cl_int err = clEnqueueCopyBuffer(devices_[dev_src].queue, (cl_mem)src, (cl_mem)dst, offset_src, offset_dst, size, 0, NULL, NULL);
    err |= clFinish(devices_[dev_src].queue);
    checkErr(err, "clEnqueueCopyBuffer()");
}

void OpenCL2Platform::copy_from_host(const void* src, int64_t offset_src, device_id dev_dst, void* dst, int64_t offset_dst, int64_t size) {
    std::memcpy((char*)dst + offset_dst, (char*)src + offset_src, size);
}

void OpenCL2Platform::copy_to_host(device_id dev_src, const void* src, int64_t offset_src, void* dst, int64_t offset_dst, int64_t size) {
    std::memcpy((char*)dst + offset_dst, (char*)src + offset_src, size);
}

int OpenCL2Platform::dev_count() {
    return devices_.size();
}
