#!/bin/bash

PROJECT_HOME="`dirname $0`/.."

absolutize ()
{
  if [ ! -d "$1" ]; then
    echo
    echo "ERROR: '$1' doesn't exist or not a directory!"
    exit -1
  fi

  cd "$1"
  echo `pwd`
  cd - >/dev/null
}

PROJECT_HOME=`absolutize "$PROJECT_HOME"`

echo "Assumin cl-l10n is in: '$PROJECT_HOME'"

if [ ! -d "$PROJECT_HOME" ]; then
    echo Something is not ok, there is no "$PROJECT_HOME" directory?! Bailing out...
    exit 1
fi

read -p "About to recursively remove everything in $PROJECT_HOME/cldr/, continue (y/n)? "

if [ "$REPLY" = "y" ]; then
  rm -rf "$PROJECT_HOME/cldr/"
  mkdir "$PROJECT_HOME/cldr/"
else
  exit 2
fi

cd "$PROJECT_HOME/cldr/"

wget http://www.unicode.org/cldr/dtd/1.6/ldml.dtd
#wget http://www.unicode.org/cldr/dtd/1.6/cldrTest.dtd

wget http://unicode.org/Public/cldr/1.6.1/core.zip
unzip core.zip
rm core.zip

wget http://unicode.org/Public/cldr/1.6.1/tests.zip
unzip tests.zip
rm tests.zip

echo Done.
