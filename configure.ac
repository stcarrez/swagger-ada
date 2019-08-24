dnl Autoconf configure script for swagger

dnl Process this file with autoconf to produce a configure script.

AC_INIT(src/swagger.ads)

# Current release settings
SWAGGER_MAJOR_VERSION=0
SWAGGER_MINOR_VERSION=1
SWAGGER_MICRO_VERSION=0
SWAGGER_VERSION=$SWAGGER_MAJOR_VERSION.$SWAGGER_MINOR_VERSION.$SWAGGER_MICRO_VERSION

# Set the version number of the project
AC_SUBST(SWAGGER_VERSION)
AC_SUBST(SWAGGER_MAJOR_VERSION)
AC_SUBST(SWAGGER_MINOR_VERSION)
AC_SUBST(SWAGGER_MICRO_VERSION)

AM_GNAT_LIBRARY_PROJECT(swagger)

AM_GNAT_FIND_ADA_UTIL([Ada Swagger])

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
  AM_GNAT_FIND_ADA_SERVLET([Ada Swagger])
fi

AM_GNAT_FIND_ADA_SECURITY([Ada Swagger])

AC_OUTPUT(
Makefile.conf
swagger.gpr
swagger_server.gpr
swagger_tests.gpr
testapi_server.gpr
testapi_model.gpr
)
