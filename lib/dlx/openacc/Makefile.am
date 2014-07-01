
lib_LTLIBRARIES=libDLX-openacc.la

AM_CXXFLAGS=-g -O0

libDLX_openacc_la_SOURCES= \
  language.cpp \
  directives.cpp constructs.cpp clauses.cpp \
  frontend.cpp \
  compiler.cpp
libDLX_openacc_la_CXXFLAGS= \
  -I$(srcdir)/../../../include \
  -I@sqlite_inc_path@ \
  -I@rose_path@/include \
  -I@boost_path@/include


