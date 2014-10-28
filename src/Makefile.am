
bin_PROGRAMS=roseacc


roseacc_SOURCES= \
  roseacc.cpp
roseacc_CXXFLAGS= \
  -g -O0 \
  -DOPENACC_INC_PATH=\"@openacc_inc_path@\" \
  -DOPENACC_LIB_PATH=\"@openacc_lib_path@\" \
  -DOPENCL_INC_PATH=\"@opencl_inc_path@\" \
  -I$(srcdir)/../include \
  -I@sqlite_inc_path@ \
  -I@boost_path@/include \
  -I@rose_path@/include \
  -I@rose_path@/include/rose
roseacc_LDFLAGS= \
  -L@sqlite_lib_path@ \
  -L@boost_lib_path@ \
  -L@rose_path@/lib
roseacc_LDADD= \
  $(builddir)/../lib/dlx/openacc/libDLX-openacc.la \
  $(builddir)/../lib/klt/openacc/libKLT-openacc.la \
  $(builddir)/../lib/mdcg/openacc/libMDCG-openacc.la \
  -lrose -lDLX-core -lMFB-sage -lMDCG -lKLT -lKLT-core \
  -lboost_filesystem -lboost_system -lboost_date_time -lboost_wave -lboost_thread -lboost_program_options -lboost_regex -lboost_iostreams \
  -lsqlite3

clean-local:
	rm -rf host-data.* kernels.cl kernel-openacc.o rose_kernel-openacc.c  versions.db

