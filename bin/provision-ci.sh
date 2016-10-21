#! /bin/bash

set -e

readonly SCRIPT_DIR=$(dirname "$0")
readonly ROOT_DIR=$(dirname "$SCRIPT_DIR")

install_sbt() {
  wget -q "https://dl.bintray.com/sbt/debian/sbt-0.13.12.deb"
  sudo dpkg -i sbt-0.13.12.deb
}

main() {
  install_sbt
}

main
