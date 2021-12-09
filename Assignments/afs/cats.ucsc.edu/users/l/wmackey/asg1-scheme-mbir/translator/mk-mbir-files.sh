#!/bin/sh
# $Id: mk-mbir-files.sh,v 1.1 2020-09-06 19:56:47-07 - - $

for mb in mb-programs.d/*.mb
do
   mbir=$(echo $mb | sed -e 's|.*/\(.*\)\.mb$|\1.mbir|')
   echo mbtran $mb mbir-files.d/$mbir
   mb2mbir.d/mbtran $mb >mbir-files.d/$mbir
done
