#!/bin/sh
#
# Shell script to launch openapi-generator on Unix systems.  Mostly borrowed from
# ArgoUML which borrowed from the Apache Ant project.
#
# The idea is that you can put a softlink in your "bin" directory back
# to this file in the ArgoUML install directory and this script will
# use the link to find the jars that it needs, e.g.:
#

## resolve links - $0 may be a link to openapi-generator's home
PRG=$0
progname=`basename $0`

while [ -h "$PRG" ] ; do
  ls=`ls -ld "$PRG"`
  link=`expr "$ls" : '.*-> \(.*\)$'`
  if expr "$link" : '.*/.*' > /dev/null; then
      PRG="$link"
  else
      PRG="`dirname $PRG`/$link"
  fi
done

DIR=`dirname $PRG`/../share/openapi-ada/
if [ ! -d $DIR ] ; then
    echo "Cannot find openapi-generator configuration directory: $DIR does not exist"
    exit 1
fi
CONFIG=""

exec java -Xms64m -Xmx512m $CONFIG -jar $DIR/openapi-generator-cli.jar "$@"

