# Output parameter definitions for the ccdpack_reg package
# Created automatically from ccdpack.ifd using ifd2iraf
# 20Jun97
#
set OutputParList(findcent) {}
set OutputParList(idicurs) {}
set OutputParList(findobj) {}
set OutputParList(register) {}
set OutputParList(tranlist) {}
set OutputParList(plotlist) {}
set OutputParList(findoff) {}
set OutputParList(pairndf) {}
set OutputParList(ccdedit) {}
set OutputParList(makemos) {}
set OutputParList(tranndf) {}

global env
if { [info exists env(CCDPACK_GLOBALS) ] } { 
   if { $env(CCDPACK_GLOBALS) == "yes" } { 
      set DynParList(findcent) {ndfnames logto logfile}
      set DynParList(idicurs) {device logto logfile}
      set DynParList(findobj) {logto logfile}
      set DynParList(register) {ndfnames logto logfile}
      set DynParList(tranlist) {ndfnames logto logfile}
      set DynParList(plotlist) {ndfnames logto logfile}
      set DynParList(findoff) {ndfnames logto logfile}
      set DynParList(pairndf) {logto logfile}
      set DynParList(ccdedit) {logto logfile}
      set DynParList(makemos) {logfile logto preserve}
      set DynParList(tranndf) {logto logfile lbound ubound}
   }
}
