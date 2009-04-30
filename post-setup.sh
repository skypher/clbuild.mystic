# make absolute symlinks relative
cd systems/
symlinks -cs .
symlinks -cs .
cd ..

# update cl-l10n CLDRs
sh source/cl-l10n/bin/update-cldr.sh

