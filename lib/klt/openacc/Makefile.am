
lib_LTLIBRARIES=libKLT-openacc.la

CXXFLAGS=-g -O0

libKLT_openacc_la_SOURCES= \
  dlx-openacc.cpp \
  loop-tiler.cpp \
  runtime-openacc.cpp \
  mfb-openacc.cpp \
  klt-openacc.cpp \
  code-generation.cpp
libKLT_openacc_la_CXXFLAGS= \
  -I$(srcdir)/../../../include \
  -I@sqlite_path@/include \
  -I@rose_path@/include \
  -I@boost_path@/include



