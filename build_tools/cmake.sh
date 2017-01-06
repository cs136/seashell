#!/bin/bash
SCRIPT_PATH=$( cd "$(dirname "${BASH_SOURCE}")" ; pwd -P )
if [ -d "$SCRIPT_PATH/../_build/" ]; then rm -r "$SCRIPT_PATH/../_build/"; fi
mkdir "$SCRIPT_PATH/../_build/"
[ -e "$SCRIPT_PATH/../build.log" ] && rm "$SCRIPT_PATH/../build.log"
echo "Cmaking Seashell to \`/_build\`... logging to /build.log"
cd "$SCRIPT_PATH/../_build" && cmake "$SCRIPT_PATH/../" -DCMAKE_INSTALL_PREFIX="$SCRIPT_PATH/../_install"  > ../build.log 2>&1

