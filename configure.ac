#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.68])

AC_INIT([RoseACC], [0.1], [vanderbruggentristan@gmail.com])

AM_INIT_AUTOMAKE([RoseACC], [0.1])

LT_INIT()

AC_CONFIG_SRCDIR([include/openacc_spec.hpp])

#AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CXX

AC_ARG_WITH(rose,           [  --with-rose              ROSE source tree])
if test ! -z "$with_rose"
then
	AC_MSG_NOTICE([Path to ROSE intall tree provided])
        AC_SUBST(rose_path, "$with_rose")
else
  AC_MSG_ERROR([Need path to ROSE install tree for RoseACC.])
fi

AC_ARG_WITH(libopenacc-src, [  --with-libopenacc-src    libOpenACC source tree])
if test ! -z "$with_libopenacc_src"
then
	AC_MSG_NOTICE([Path to libOpenACC source tree provided])
        AC_SUBST(libopenacc_src_path, "$with_libopenacc_src")
else
  AC_MSG_ERROR([Need path to libOpenACC source tree for RoseACC.])
fi

AC_ARG_WITH(sqlite,        [  --with-sqlite            SQLite path])
AC_ARG_WITH(sqlite-libdir, [  --with-sqlite-libdir     SQLite libraries path])

if test ! -z "$with_sqlite"
then
	AC_MSG_NOTICE([SQLite path provided])
        AC_SUBST(sqlite_path, "$with_sqlite")
        AC_SUBST(sqlite_lib_path, "$with_sqlite/lib")
else
  AC_MSG_ERROR([Missing SQLite headers and/or libraries paths.])
fi
if test ! -z "$with_sqlite_libdir"
then
	AC_MSG_NOTICE([SQLite libraries path provided])
        AC_SUBST(sqlite_lib_path, $with_sqlite_libdir)
fi
# TODO test SQLite

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

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC

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
