#!/bin/bash
#
# only used for docker deployment, executed after wait-for-it.sh

echo "$(date) - Starting app"
cabal run todo-app
