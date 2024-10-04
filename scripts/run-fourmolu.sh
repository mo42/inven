#!/bin/sh

set -eu -o pipefail

FOURMOLU='stack exec -- fourmolu -i app src'
exec $FOURMOLU "$@"
