Seashell React Frontend

Command|Description
--- | ---
*npm run start*|Start application with a live server
*npm run build*|Minified build with offline capabilities

Resources get built to /dist while the index.html file is built to /index.html.

All source files (including uncompiled index.html) are located in /src.

The Seashell Clang.js port is included as a file dependency in package.json.  However
one needs to manually update the file that we download when a new build of the compiler
is released, as the file names are version tagged.

TODO: Point dexie-syncable at upstream source once github.com/dfahlander/Dexie.js/pull/570 is resolved.

