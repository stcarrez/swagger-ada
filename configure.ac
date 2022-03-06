dnl Autoconf configure script for swagger

dnl Process this file with autoconf to produce a configure script.

AC_INIT(src/swagger.ads)

# Current release settings
OPENAPI_MAJOR_VERSION=0
OPENAPI_MINOR_VERSION=6
OPENAPI_MICRO_VERSION=0
OPENAPI_VERSION=$OPENAPI_MAJOR_VERSION.$OPENAPI_MINOR_VERSION.$OPENAPI_MICRO_VERSION

# Set the version number of the project
AC_SUBST(OPENAPI_VERSION)
AC_SUBST(OPENAPI_MAJOR_VERSION)
AC_SUBST(OPENAPI_MINOR_VERSION)
AC_SUBST(OPENAPI_MICRO_VERSION)

AM_GNAT_LIBRARY_PROJECT(openapi)

AC_CHECK_PROG(HAVE_PANDOC,pandoc,yes)
AC_CHECK_PROG(HAVE_DYNAMO,dynamo,yes)

AM_GNAT_FIND_ADA_UTIL([Ada OpenAPI])

##########################################
# Check for OpenAPI Server support
##########################################
AC_MSG_CHECKING([server support])
ac_enable_server=yes
AC_ARG_ENABLE(server,
[  --enable-server         Enable the support for OpenAPI Server (enabled)],
[case "${enableval}" in
  no|none)  ac_enable_server=no ;;
  *)        ac_enable_server=yes ;;
esac])dnl

AC_MSG_RESULT(${ac_enable_server})
OPENAPI_HAVE_SERVER=$ac_enable_server
AC_SUBST(OPENAPI_HAVE_SERVER)

if test T${ac_enable_server} = Tyes; then
  AM_GNAT_FIND_ADA_SERVLET([Ada OpenAPI])
fi

AM_GNAT_FIND_ADA_SECURITY([Ada OpenAPI])

AC_OUTPUT(
Makefile.conf
openapi.gpr
openapi_server.gpr
openapi_tests.gpr
testapi_server.gpr
testapi_model.gpr
)
