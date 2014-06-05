
bin_PROGRAMS=roseacc

CXXFLAGS=-g -O0

roseacc_SOURCES= \
  roseacc.cpp
roseacc_CXXFLAGS= \
  -DLIBOPENACC_INC_PATH=\"@libopenacc_src_path@/include\" \
  -DOPENCL_INC_PATH=\"@opencl_path@/include\" \
  -I$(srcdir)/../include \
  -I@sqlite_path@/include \
  -I@boost_path@/include \
  -I@rose_path@/include
roseacc_LDFLAGS= \
  -L@sqlite_lib_path@ \
  -L@boost_lib_path@ \
  -L@rose_path@/lib
roseacc_LDADD= \
  $(builddir)/../lib/dlx/openacc/libDLX-openacc.la \
  $(builddir)/../lib/klt/openacc/libKLT-openacc.la \
  $(builddir)/../lib/mdcg/openacc/libMDCG-openacc.la \
  -lrose -lDLX-core -lMFB-sage -lMDCG -lKLT -lKLT-core \
  -lboost_filesystem -lboost_system \
  -lsqlite3

check-local: clean-local roseacc
	./roseacc -I$(top_srcdir)/../libopenacc/include -c $(top_srcdir)/../tests/101/kernel-openacc.c

clean-local:
	rm -rf host-data.* kernels.cl kernel-openacc.o rose_kernel-openacc.c  versions.db

