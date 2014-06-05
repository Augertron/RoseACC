
lib_LTLIBRARIES=libDLX-openacc.la

CXXFLAGS=-g -O0

libDLX_openacc_la_SOURCES= \
  language.cpp \
  directives.cpp constructs.cpp clauses.cpp \
  frontend.cpp \
  compiler.cpp
libDLX_openacc_la_CXXFLAGS= \
  -I$(srcdir)/../../../include \
  -I@sqlite_path@/include \
  -I@rose_path@/include \
  -I@boost_path@/include


