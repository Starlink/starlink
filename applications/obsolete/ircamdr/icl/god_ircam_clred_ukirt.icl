proc startup
  print "Welcome to Unix IRCAM_CLRED !"
  print " "
  set precision 6
  exists = file_exists("$ADAM_USER/GLOBAL.sdf")
  if exists
    ! rm $ADAM_USER/GLOBAL.sdf
  end if
  define glob $ADAM_USER/GLOBAL
  today = getenv("TODAY")
  print (today)
  print " "
  host = variable(host)
  host = upcase(host)
  if host = "VAX"
    print "Loading Vax utilities"
    load $LIRCAMDIR_ICL/utils.icl
    load $LIRCAMDIR_ICL/setfile.icl
  else
    print "Loading Unix utilities"
    load $LIRCAMDIR_ICL/utils_unix.icl
    load $LIRCAMDIR_ICL/setfile_unix.icl
  end if
  load $LIRCAMDIR_ICL/closet.icl
  load $LIRCAMDIR_ICL/closecq.icl
  load $LIRCAMDIR_ICL/closeo.icl
  load $LIRCAMDIR_ICL/formname.icl
  load $LIRCAMDIR_ICL/formname2.icl
  load $LIRCAMDIR_ICL/get_imagename.icl
  load $LIRCAMDIR_ICL/testval.icl
  load $LIRCAMDIR_ICL/define_atasks.icl
  load $LIRCAMDIR_ICL/short_display.icl
  load $LIRCAMDIR_ICL/defprocs.icl
  load $LIRCAMDIR_ICL/setpre.icl
  load $LIRCAMDIR_ICL/exit.icl
  load $LIRCAMDIR/acvt.icl
  load $LIRCAMDIR_ICL/lplt.icl
  load $LIRCAMDIR_ICL/logo.icl
  set prompt "Ircam3-Clred> "
  LPLT
  SETFILE
  SETPS
  LOGO
end proc

load $LIRCAMDIR_ICL/global_vars.icl
load_acvt
send acvt set endcheck false
startup
routd = getenv("ROUTD")
rosuf = "NONE"
POPEN 1
SETPS 0.286
SETMAG 0
SEND plt2d SET NAME_PREFIX (routd)
SEND plt2d SET MAGNIFICATION 2
SEND plt2d SET CUT_AXISRATIO 1.0
SEND plt2d SET CUT_LINETYPE 'B'
SEND plt2d SET CUT_SCALING 'A'
SEND plt2d SET CUT_XTICST 0.0
SEND plt2d SET CUT_XTICINT 10.0
SEND plt2d SET CUT_TITLE 'IRCAM Image'
SEND plt2d SET CUT_ANNOTATION 'FULL'
SEND plt2d SET CUT_MAGNIF 12
SEND plt2d SET CUT_POSITIONING 'KEY'
SEND plt2d SET CUT_XCEN WORKXCEN
SEND plt2d SET CUT_YCEN WORKYCEN
SEND plt2d SET SIGMA_LEVEL 2
GET plt2d WORKXCEN (workxcen)
GET plt2d WORKYCEN (workycen)
LOGO
print " "
print "File prefix defined as " (routd)
print " "
send plt2d set zeroj 23.45
send plt2d set zeroh 22.98
send plt2d set zerok 22.45
send plt2d set zerolp 20.4
print "Nominal zeropoints of J=23.45, H=22.98, K=22.45, Lp=20.4 defined"
print " "

