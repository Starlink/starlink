# Output parameter definitions for the ccdpack_red package
# Created automatically from ccdpack.ifd using ifd2iraf
# 20Jun97
#
set OutputParList(makebias) {}
set OutputParList(debias) {}
set OutputParList(makecal) {}
set OutputParList(calcor) {}
set OutputParList(makeflat) {}
set OutputParList(flatcor) {}

global env
if { [info exists env(CCDPACK_GLOBALS) ] } { 
   if { $env(CCDPACK_GLOBALS) == "yes" } { 
      set DynParList(makebias) {rnoise genvar useext preserve logto logfile}
      set DynParList(debias) {adc rnoise bounds mask useext genvar 
                              direction preserve logto logfile 
                              saturate setsat saturation deferred extent}
      set DynParList(makecal) {useext logto logfile}
      set DynParList(calcor) {useext preserve logto logfile}
      set DynParList(makeflat) {logto logfile}
      set DynParList(flatcor) {preserve logto logfile}
   }
}
