proc startup
  print "Welcome to Unix IRCAM_CLRED/Portable-IRCAMDR !"
  print " "
  set precision 6
  load $LIRCAMDIR/define_atasks.icl
  delfile $ADAM_USER/GLOBAL.sdf
  today = getenv("TODAY")
  host = variable(host)
  host = upcase(host)
  if host = "VAX"
    print "Loading Vax utilities"
    load $LIRCAMDIR/utils.icl
    load $LIRCAMDIR/setfile.icl
  else
    print "Loading Unix utilities"
    load $LIRCAMDIR/utils_unix.icl
    load $LIRCAMDIR/setfile_unix.icl
  end if
  ndfformats = getenv("NDF_FORMATS_IN")
  if ndfformats <> " "
    setenv NDF_FORMATS_IN ~
'FITS(.fit),~
FIGARO(.dst),~
IRAF(.imh),~
STREAM(.das),~
UNFORMATTED(.unf),~
UNF0(.dat),~
ASCII(.asc),~
TEXT(.txt),~
GIF(.gif),~
TIFF(.tif),~
GASP(.hdr),~
COMPRESSED(.sdf.Z),~
GZIP(.sdf.gz)'
  endif
  load $LIRCAMDIR/fclose_c.icl
  load $LIRCAMDIR/fclose_a.icl
  load $LIRCAMDIR/fclose_b.icl
  load $LIRCAMDIR/fclose_d.icl
  load $LIRCAMDIR/fclose_e.icl
  load $LIRCAMDIR/formname.icl
  load $LIRCAMDIR/formname2.icl
  load $LIRCAMDIR/get_imagename.icl
  load $LIRCAMDIR/testval.icl
  load $LIRCAMDIR/short_display.icl
  load $LIRCAMDIR/defprocs.icl
  load $LIRCAMDIR/setpre.icl
  load $LIRCAMDIR/exit.icl
  load $LIRCAMDIR/lplt.icl
  load $LIRCAMDIR/logo.icl
  set prompt "IrcamDR > "
end proc

load $LIRCAMDIR/global_vars.icl
routd = getenv("ROUTD")
kappa
startup
define glob $ADAM_USER/GLOBAL
popen 1
setps 0.286
setmag 0
send plt2d set name_prefix (routd)
send plt2d set cut_axisratio 1.0
send plt2d set cut_linetype 'B'
send plt2d set cut_scaling 'A'
send plt2d set cut_xticst 0.0
send plt2d set cut_xticint 10.0
send plt2d set cut_title 'IRCAM3 IMAGE'
send plt2d set cut_annotation 'FULL'
send plt2d set cut_magnif 12
send plt2d set cut_positioning 'KEY'
get plt2d workxcen (workxcen)
get plt2d workycen (workycen)
send plt2d set cut_xcen (workxcen)
send plt2d set cut_ycen (workycen)
send plt2d set sigma_level 3
send plt2d set ffj NONE
send plt2d set ffh NONE
send plt2d set ffk NONE
send plt2d set ffnbl NONE
send plt2d set fflp NONE
send plt2d set ffnbm NONE
logo
print " "
print "File prefix defined as " (routd)
print " "
send plt2d set zeroj 22.94
send plt2d set zeroh 22.68
send plt2d set zerok 22.32
send plt2d set zerolp 20.7
print "Nominal zeropoints of J=22.94, H=22.68, K=22.32, Lp=20.7 defined"
print " "
