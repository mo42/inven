#!/bin/sh

set -eu -o pipefail

FOURMOLU='stack exec -- fourmolu'
exec $FOURMOLU "$@"
