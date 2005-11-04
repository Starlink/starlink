set env TCL_LIBRARY ../../../tcl8.3.1/library
set env TK_LIBRARY  ../../../tk8.3.1/library 
set env TCLX_LIBRARY ../../tcl/unix
set env TKX_LIBRARY ../../tk/unix
set env TCL_PROGRAM ../../tcl/unix/tclXtest

set env LD_LIBRARY_PATH ../../tcl/unix:../../tk/unix:../../../tcl8.3.1/unix:../../../tk8.3.1/unix

set env MALLOC_OPTIONS A

handle SIGTERM SIGHUP SIGKILL SIGCHLD pass nostop
dir ../../tcl/unix ../../tcl/generic ../../tk/unix ../../tk/generic ../../../tcl8.3.1/generic ../../../tcl8.3.1/unix ../../../tk8.3.1/generic ../../../tk8.3.1/unix



