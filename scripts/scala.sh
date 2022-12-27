#!/bin/bash

set -e

prev=$(tar -cf - src target/scala-3.2.1/blockjumper-opt/main.js | md5sum)
echo "Previous hash: $prev"
check_equal_hashes () {
  curr=$(tar -cf - src target/scala-3.2.1/blockjumper-opt/main.js | md5sum)
  echo "New hash: $curr"
  if [ "$curr" != "$prev" ]; then
    echo "Source code changed. Run bash ./scripts/scala.sh"
    exit 1
  fi
}
echo "Compiling Scala, fixing intentation..."
sbt -Dscala.rewrite=indent "fullLinkJS; test:compile; scalafmtAll"
echo "Compiling Scala, fixing syntax..."
sbt -Dscala.rewrite=new-syntax "fullLinkJS; test:compile; scalafmtAll"
check_equal_hashes
