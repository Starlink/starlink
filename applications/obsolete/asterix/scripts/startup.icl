{ ASTERIX ICL startup file

{ switch off load messages
 SET NOMESSAGES
 SET NOCHECKPARS

{ declare a few basic variables
X=0.0
SX='(X)'
Y=0.0
SY='(Y)'
X1=0.0
SX1='(X1)'
Y1=0.0
SY1='(Y1)'
X2=0.0
SX2='(X2)'
Y2=0.0
SY2='(Y2)'


{ ASTERIX system commands

{define ASTHELP }
{  defstring dumphelp dcl asthelp >
{  defhelp asterix ast_help:asthelp help
{  defstring user_guide dcl user_guide
{  defstring docs dcl docs
{  defstring cxmake $cxmake
{  defstring mxmake $mxmake
{  defstring trace $trace

{ load monolith command definitions
load $AST_BIN/grf_shr.icl
load $AST_BIN/hed_shr.icl
load $AST_BIN/interface_shr.icl
load $AST_BIN/spec_shr.icl
load $AST_BIN/src_shr.icl
load $AST_BIN/tim_shr.icl
load $AST_BIN/util_shr.icl
load $AST_BIN/xrt_shr.icl

defstring astdefs !astdefs
defstring asthelp !asthelp

VERSION = GETENV("AST_VERSION")
  PRINT
  PRINT '----------------------------------------------------------------------'
  PRINT ' ASTERIX Version' (VERSION) '          - type ASTHELP for more information'
  PRINT ' '

{ Disable SAVE dump on exit
SET NOSAVE

{ local definitions commands etc if defined
HIDDEN PROC AST_LOCAL
  FNAME = GETENV( "AST_LOCAL_ICL" ) 
  IF FILE_EXISTS(FNAME)
    LOAD (FNAME)
  ENDIF
  EXCEPTION OSERR
  ENDEXCEPTION
ENDPROC
AST_LOCAL

{ user definitions commands etc if defined
HIDDEN PROC AST_USER
  FNAME = GETENV( "AST_USER_ICL" ) 
  IF FILE_EXISTS(FNAME)
    LOAD (FNAME)
  ENDIF
  EXCEPTION OSERR
  ENDEXCEPTION
ENDPROC
AST_USER
