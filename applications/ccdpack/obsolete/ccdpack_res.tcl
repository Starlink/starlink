# Output parameter definitions for the ccdpack_res package
# Created automatically from ccdpack.ifd using ifd2iraf
# 20Jun97
#
set OutputParList(ccdsetup) {maskname}
set OutputParList(ccdclear) {}
set OutputParList(ccdnote) {}
set OutputParList(ccdshow) {}
set OutputParList(ccdndfac) {}
set OutputParList(ccdgenerate) {}
set OutputParList(picinfo) {}
set OutputParList(import) {}
set OutputParList(present) {}
set OutputParList(schedule) {}
set OutputParList(ccdimp) {}
set OutputParList(ccdexp) {}

#  CCDSHOW is special, must always get the global values
set DynParList(ccdshow) {logto logfile logtod logfiled adc bounds
                         rnoise mask flat bias cal direction deferred 
                         extent preserve genvar ndfnames saturate 
                         saturation setsat}

global env
if { [info exists env(CCDPACK_GLOBALS) ] } { 
   if { $env(CCDPACK_GLOBALS) == "yes" } { 
      set DynParList(ccdsetup) {maskname}
      set DynParList(ccdclear) {logto logfile}
      set DynParList(ccdnote) {logfile logto}
      set DynParList(ccdndfac) {logto logfile}
      set DynParList(ccdgenerate) {logto logfile}
      set DynParList(picinfo) {}
      set DynParList(import) {logto logfile}
      set DynParList(present) {adc bounds rnoise biasvalue saturation 
                               direction deferred extent logto logfile}
      set DynParList(schedule) {logto logfile}
      set DynParList(ccdimp) {}
      set DynParList(ccdexp) {}
   }
}


