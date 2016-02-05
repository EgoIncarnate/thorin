#ifndef OPENCL_UTILS_H
#define OPENCL_UTILS_H

#include "thorin/util/log.h"
#include <string>

#ifdef __APPLE__
#include <OpenCL/cl.h>
#include <OpenCL/cl_ext.h>
#else
#include <CL/cl.h>
#include <CL/cl_ext.h>
#endif

std::string get_opencl_error_code_str(int error);

#define checkErr(err, name)  check_opencl_errors (err, name, __FILE__, __LINE__)
#define checkAllocation(ptr) check_allocation(ptr, __FILE__, __LINE__)

void check_opencl_errors(cl_int err, const char* name, const char* file, const int line);
void check_allocation(void* ptr, const char* file, const int line);

#endif
