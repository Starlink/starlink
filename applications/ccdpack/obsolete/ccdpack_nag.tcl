# Output parameter definitions for the ccdpack_nag package
# Created automatically from ccdpack.ifd using ifd2iraf
# 20Jun97
#
set OutputParList(nagmakemos) {}
global env
if { [info exists env(CCDPACK_GLOBALS) ] } { 
   if { $env(CCDPACK_GLOBALS) == "yes" } { 
      set DynParList(nagmakemos) {logfile logto preserve}
   }
}
