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
AM_DISTRIB_SUPPORT
AM_COVERAGE_SUPPORT

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

##########################################
# Check for Swagger Server support
##########################################
AC_MSG_CHECKING([server support])
ac_enable_server=yes
AC_ARG_ENABLE(server,
[  --enable-server         Enable the support for Swagger Server (enabled)],
[case "${enableval}" in
  no|none)  ac_enable_server=no ;;
  *)        ac_enable_server=yes ;;
esac])dnl

AC_MSG_RESULT(${ac_enable_server})
SWAGGER_HAVE_SERVER=$ac_enable_server
AC_SUBST(SWAGGER_HAVE_SERVER)

if test T${ac_enable_server} = Tyes; then

##########################################
# Ada Servlet library
##########################################
AM_GNAT_FIND_PROJECT([ada-servlet],[Ada Servlet Library],[servlet],
  [git@github.com:stcarrez/ada-servlet.git],
  [Building swagger requires the Ada Servlet Library.],
  [
    SERVLET_DIR=${gnat_project_dir_servlet}
  ])
AC_SUBST(SERVLET_DIR)

fi

##########################################
# Ada Security library
##########################################
AM_GNAT_FIND_PROJECT([ada-security],[Ada Security Library],[security],
  [git@github.com:stcarrez/ada-security.git],
  [Building Ada Servlet requires the Ada Security Library.],
  [
    SECURITY_DIR=${gnat_project_dir_security}
  ])
AC_SUBST(SECURITY_DIR)

AC_OUTPUT(
Makefile swagger.gpr swagger_server.gpr swagger_tests.gpr testapi_server.gpr testapi_model.gpr
)
