# Installation

This chapter explains how to build and install the OpenAPI Ada library.

## Before Building

Before building OpenAPI Ada, you will need:

* [XML/Ada](https://libre.adacore.com/libre/tools/xmlada/)
* [AWS](https://libre.adacore.com/libre/tools/aws/)
* [Ada Utility Library](https://github.com/stcarrez/ada-util)
* [Ada Security](https://github.com/stcarrez/ada-security)
* [Ada EL](https://github.com/stcarrez/ada-el)
* [Ada Servlet](https://github.com/stcarrez/ada-servlet)

First get, build and install the [XML/Ada](https://libre.adacore.com/libre/tools/xmlada/)
and then get, build and install the [Ada Utility Library](https://github.com/stcarrez/ada-util).



## Configuration

The library uses the `configure` script to detect the build environment.  If some component is missing, the
`configure` script will report an error.  The `configure` script provides several standard options
and you may use:

  * `--prefix=DIR` to control the installation directory,
  * `--with-ada-util=PATH` to control the installation path of [Ada Utility Library](https://github.com/stcarrez/ada-util),
  * `--enable-shared` to enable the build of shared libraries,
  * `--disable-static` to disable the build of static libraries,
  * `--enable-distrib` to build for a distribution and strip symbols,
  * `--disable-distrib` to build with debugging support,
  * `--enable-coverage` to build with code coverage support (`-fprofile-arcs -ftest-coverage`),
  * `--enable-server` to build the OpenAPI server support,
  * `--help` to get a detailed list of supported options.

In most cases you will configure with the following command:
```
./configure
```

## Build

After configuration is successful, you can build the library by running:
```
make
```

After building, it is good practice to run the unit tests before installing the library.
The unit tests are built and executed using:
```
make test
```

## Installation
The installation is done by running the `install` target:

```
make install
```

If you want to install on a specific place, you can change the `prefix` and indicate the installation
direction as follows:

```
make install prefix=/opt
```

## Using

To use the library in an Ada project, add the following line at the beginning of your
GNAT project file:

For a REST client, use:

```
with "openapi";
```

For a REST server, use:

```
with "openapi";
with "openapi_server";
```
