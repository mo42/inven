#!/bin/sh

set -eu -o pipefail

FOURMOLU='stack exec -- fourmolu -i'
exec $FOURMOLU "$@"
