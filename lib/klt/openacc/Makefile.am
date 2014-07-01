
lib_LTLIBRARIES=libKLT-openacc.la

AM_CXXFLAGS=-g -O0

libKLT_openacc_la_SOURCES= \
  dlx-openacc.cpp \
  loop-tiler.cpp \
  data-flow.cpp \
  runtime-openacc.cpp \
  mfb-openacc.cpp \
  klt-openacc.cpp \
  code-generation.cpp
libKLT_openacc_la_CXXFLAGS= \
  -I$(srcdir)/../../../include \
  -I@sqlite_inc_path@ \
  -I@rose_path@/include \
  -I@boost_path@/include



