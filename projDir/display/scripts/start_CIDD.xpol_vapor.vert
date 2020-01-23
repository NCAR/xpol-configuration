#! /bin/csh

$HOME/xpolProjDir/display/scripts/set_font_path >& /dev/null

cd $HOME/xpolProjDir/display/params

setenv DATA_HOST vapor.rap.ucar.edu

CIDD -V -p CIDD.xpol_vapor.vert -i xpol_vapor.vert -font fixed >& /dev/null &

