define RADOPT       $AST_BIN/rosxrt_mono
define XRTSORT      $AST_BIN/rosxrt_mono
define XRTCORR      $AST_BIN/rosxrt_mono
define XRTHK        $AST_BIN/rosxrt_mono
define XRTORB       $AST_BIN/rosxrt_mono
define XRTSUB       $AST_BIN/rosxrt_mono
define XRTRESP      $AST_BIN/rosxrt_mono
define XRTHOT       $AST_BIN/rosxrt_mono
define XPSSCORR     $AST_BIN/rosxrt_mono
define XSPOKES      $AST_BIN/rosxrt_mono
define XRAD         $AST_BIN/rosxrt_mono
define XRAD90       $AST_BIN/rosxrt_mono
define SHOWXRT      $AST_BIN/rosxrt_mono
define XRTCONV      $AST_BIN/rosxrt_mono
define XRTINDEX     $AST_BIN/rosxrt_mono
define FITS2HDS     $AST_BIN/rosxrt_mono
define XRTEXPMAP    $AST_BIN/rosxrt_mono

defstring XSORT XRTSORT

define WFCSPEC      $AST_BIN/rosxrt_mono

Defstring READXRT   PRINT "This command has been withdrawn - see help"

Defproc  XRTBOX       $AST_BIN/xrt_procs
Defproc  XRTBCKBOX    $AST_BIN/xrt_procs
Defproc  XRTMSPEC     $AST_BIN/xrtmspec
Defproc  XRTMTIME     $AST_BIN/xrtmtime
Defproc  XPSF         $AST_BIN/xrt_procs
Defproc  WFCOPT       $AST_BIN/xrt_procs

Defstring SASSBIN REBIN OPT=5 CLONE=$AST_ETC/sassbins
