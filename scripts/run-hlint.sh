#!/bin/sh

set -eu -o pipefail

HLINT='stack exec -- hlint'
exec $HLINT "$@"
