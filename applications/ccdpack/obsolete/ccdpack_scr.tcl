# Output parameter definitions for the ccdpack_scr package
# Created automatically from ccdpack.ifd using ifd2iraf
# 20Jun97
#
set OutputParList(reduce) {}
set OutputParList(ccdalign) {}
set OutputParList(runxreduce) {}

global env
if { [info exists env(CCDPACK_GLOBALS) ] } { 
   if { $env(CCDPACK_GLOBALS) == "yes" } { 
      set DynParList(reduce) {logto logfile}
      set DynParList(ccdalign) {logto logfile}
      set DynParList(runxreduce) {}
   }
}
