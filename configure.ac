dnl Autoconf configure script for swagger

dnl Process this file with autoconf to produce a configure script.

AC_INIT(src/swagger.ads)

AC_CANONICAL_SYSTEM
dnl AM_MAINTAINER_MODE

# Current release settings
SWAGGER_MAJOR_VERSION=0
SWAGGER_MINOR_VERSION=1
SWAGGER_MICRO_VERSION=0
SWAGGER_VERSION=$SWAGGER_MAJOR_VERSION.$SWAGGER_MINOR_VERSION.$SWAGGER_MICRO_VERSION
AM_GNAT_CHECK_GPRBUILD

AM_GNAT_CHECK_INSTALL

# checking for local tools
AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_LN_S

# Set the version number of the project
AC_SUBST(SWAGGER_VERSION)
AC_SUBST(SWAGGER_MAJOR_VERSION)
AC_SUBST(SWAGGER_MINOR_VERSION)
AC_SUBST(SWAGGER_MICRO_VERSION)

EXEC_PREFIX="$prefix"
AC_SUBST(EXEC_PREFIX)
SWAGGER_LIBDIR="lib"
AC_SUBST(SWAGGER_LIBDIR)

AC_MSG_CHECKING([number of processors])
NR_CPUS=`getconf _NPROCESSORS_CONF 2>/dev/null || getconf NPROCESSORS_CONF 2>/dev/null || echo 1`
AC_MSG_RESULT($NR_CPUS)
AC_SUBST(NR_CPUS)

##########################################
# Check to enable/disable shared library
##########################################
AM_SHARED_LIBRARY_SUPPORT

##########################################
# Ada Util library
##########################################
AM_GNAT_FIND_PROJECT([ada-util],[Ada Utility Library],[util],
  [git@github.com:stcarrez/ada-util.git],
  [Building swagger requires the Ada Utility Library.],
  [
    UTIL_DIR=${gnat_project_dir_util}
  ])
AC_SUBST(UTIL_DIR)

AC_OUTPUT(
Makefile swagger.gpr swagger_tests.gpr
)
