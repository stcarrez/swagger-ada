dnl Autoconf configure script for swagger

dnl Process this file with autoconf to produce a configure script.

AC_INIT
AC_CONFIG_SRCDIR([src/swagger.ads])

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

AM_GNAT_FIND_ADA_SECURITY([Ada OpenAPI])

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

OPENAPI_HAVE_EWS=no
OPENAPI_HAVE_AWS=no
WITH_SERVLET_EWS=''
WITH_SERVLET_WWS=''
SERVER_EWS=''
SERVER_AWS=''
if test T${ac_enable_server} = Tyes; then
  AM_GNAT_FIND_ADA_SERVLET([Ada OpenAPI])

  AM_GNAT_FIND_PROJECT([ada-servlet],[Servlet Ada with Embedded Web Server],[servletada_ews],
   [https://github.com/simonjwright/ews],
   [],
   [
     OPENAPI_HAVE_EWS=yes
     WITH_SERVLET_EWS="with \"${ac_cv_gnat_project_dir_servletada_ews}servletada_ews\";"
     SERVER_EWS="\"ews\""
   ])

  AM_GNAT_FIND_PROJECT([ada-servlet],[Servlet Ada with Ada Web Server],[servletada_aws],
   [https://libre.adacore.com/libre/tools/aws/],
   [],
   [
     OPENAPI_HAVE_AWS=yes
     WITH_SERVLET_AWS="with \"${ac_cv_gnat_project_dir_servletada_aws}servletada_aws\";"
     SERVER_EWS="\"aws\""
   ])

fi

AC_SUBST(OPENAPI_HAVE_EWS)
AC_SUBST(WITH_SERVLET_EWS)
AC_SUBST(OPENAPI_HAVE_AWS)
AC_SUBST(WITH_SERVLET_AWS)

AC_CONFIG_FILES([Makefile.conf
openapi.gpr
openapi_server.gpr
openapi_tests.gpr
testapi_server.gpr
testapi_model.gpr
])
AC_OUTPUT
