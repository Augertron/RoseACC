#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.68])

AC_INIT([RoseACC], [0.1], [vanderbruggentristan@gmail.com])

AC_CONFIG_MACRO_DIR([m4])

LT_INIT

AM_INIT_AUTOMAKE([RoseACC], [0.1])

AC_CONFIG_SRCDIR([include/openacc_spec.hpp])

#AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CXX

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC

#####
# ROSE
#####

AC_ARG_WITH(rose,           [  --with-rose              ROSE source tree])
if test ! -z "$with_rose"
then
  AC_MSG_NOTICE([Path to ROSE intall tree provided])
  AC_SUBST(rose_path, "$with_rose")
else
  AC_MSG_ERROR([Need path to ROSE install tree for RoseACC.])
fi

#####
# libOpenACC
#####

AC_ARG_WITH(openacc-include, [  --with-openacc-include    OpenACC include path])
if test ! -z "$with_openacc_include"
then
	AC_MSG_NOTICE([OpenACC include path provided])
        AC_SUBST(openacc_inc_path, $with_openacc_include)
else
  AC_MSG_ERROR([Missing OpenACC include path.])
fi

AC_ARG_WITH(openacc-libdir,  [  --with-openacc-libdir     OpenACC library path])
if test ! -z "$with_openacc_libdir"
then
	AC_MSG_NOTICE([OpenACC library path provided])
        AC_SUBST(openacc_lib_path, $with_openacc_libdir)
else
  AC_MSG_ERROR([Missing OpenACC library path.])
fi

#####
# OpenCL
#####

AC_ARG_WITH(opencl-include, [  --with-opencl-include    OpenCL include path])

if test ! -z "$with_opencl_include"
then
	AC_MSG_NOTICE([OpenCL include path provided])
        AC_SUBST(opencl_inc_path, $with_opencl_include)
else
  AC_MSG_ERROR([Missing OpenCL include path.])
fi

#####
# SQLite
#####

AC_ARG_WITH(sqlite-include, [  --with-sqlite-include    SQLite include path])
AC_ARG_WITH(sqlite-libdir,  [  --with-sqlite-libdir     SQLite library path])

if test ! -z "$with_sqlite_include"
then
	AC_MSG_NOTICE([SQLite include path provided])
        AC_SUBST(sqlite_inc_path, $with_sqlite_include)
else
  AC_MSG_ERROR([Missing SQLite include path.])
fi
if test ! -z "$with_sqlite_libdir"
then
	AC_MSG_NOTICE([SQLite library path provided])
        AC_SUBST(sqlite_lib_path, $with_sqlite_libdir)
else
  AC_MSG_ERROR([Missing SQLite library path.])
fi
# TODO test SQLite

#####
# Boost (RoseACC depends on ROSE which need Boost)
#####

AC_ARG_WITH(boost,         [  --with-boost             BOOST path])
AC_ARG_WITH(boost-libdir,  [  --with-boost-libdir      BOOST libraries path])

if test ! -z "$with_boost"
then
  AC_MSG_NOTICE([BOOST path provided])
  AC_SUBST(boost_path, "$with_boost")
  AC_SUBST(boost_lib_path, "$with_boost/lib")
else
  AC_MSG_ERROR([Need BOOST path for ROSE])
fi
if test ! -z "$with_boost_libdir"
then
  AC_MSG_NOTICE([BOOST headers path provided])
  AC_SUBST(boost_inc_path, $with_boost_inc)
fi
# TODO test BOOST

AC_CONFIG_FILES([Makefile
                 include/DLX/Makefile
                 include/DLX/OpenACC/Makefile
                 include/KLT/Makefile
                 include/KLT/OpenACC/Makefile
                 include/MDCG/Makefile
                 include/MDCG/OpenACC/Makefile
                 include/Makefile
                 lib/Makefile
                 lib/dlx/Makefile
                 lib/dlx/openacc/Makefile
                 lib/klt/Makefile
                 lib/klt/openacc/Makefile
                 lib/mdcg/Makefile
                 lib/mdcg/openacc/Makefile
                 src/Makefile])
AC_OUTPUT
