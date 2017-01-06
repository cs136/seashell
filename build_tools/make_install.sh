#!/bin/bash
SCRIPT_PATH=$( cd "$(dirname "${BASH_SOURCE}")" ; pwd -P )
if [ -d "$SCRIPT_PATH/../_install" ]; then rm -r "$SCRIPT_PATH/../_install/"; fi
mkdir "$SCRIPT_PATH/../_install/"
[ -e "$SCRIPT_PATH/../install.log" ] && rm "$SCRIPT_PATH/../install.log"
echo "Make_installing Seashell to \`/_install\` with 8 cores... logging to /install.log"
cd "$SCRIPT_PATH/../_build" && make -j8 > ../install.log 2>&1
cd "$SCRIPT_PATH/../_build" && make install -j8  
echo "Setting Symlinks"
ln -s "$SCRIPT_PATH/../_install/cgi-bin" "$SCRIPT_PATH/../_install/share/frontend/cgi-bin"

echo "Updating login2.cgi with correct path"
rm "$SCRIPT_PATH/../_install/cgi-bin/login2.cgi" 
printf "#!/bin/bash\nexec \"$SCRIPT_PATH/../_install/bin/seashell-main\" \"-l\"" > "$SCRIPT_PATH/../_install/cgi-bin/login2.cgi"
chmod +x "$SCRIPT_PATH/../_install/cgi-bin/login2.cgi"

