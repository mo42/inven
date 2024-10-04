#!/bin/sh

set -eu -o pipefail

HLINT='stack exec -- hlint app src'
exec $HLINT "$@"
