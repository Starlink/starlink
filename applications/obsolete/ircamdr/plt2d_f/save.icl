PROC startup
  print 'Welcome to Unix IRCAM_CLRED !'
  print ' '
  set precision 6
  EXISTS = file_exists ('$ADAM_USER/GLOBAL.sdf')
  IF EXISTS
    SH rm $ADAM_USER/GLOBAL.sdf
  END IF
  define glob $ADAM_USER/GLOBAL
  TODAY = getenv ('TODAY')
  print (TODAY)
  print ' '
  HOST = variable (HOST)
  HOST = upcase (HOST)
  IF HOST = 'VAX'
    print 'Loading Vax utilities'
    load $LIRCAMDIR/utils.icl
    load $LIRCAMDIR/setfile.icl
  ELSE
    print 'Loading Unix utilities'
    load $LIRCAMDIR/utils_unix.icl
    load $LIRCAMDIR/setfile_unix.icl
  END IF
  load $LIRCAMDIR/closet.icl
  load $LIRCAMDIR/closecq.icl
  load $LIRCAMDIR/closeo.icl
  load $LIRCAMDIR/formname.icl
  load $LIRCAMDIR/formname2.icl
  load $LIRCAMDIR/get_imagename.icl
  load $LIRCAMDIR/testval.icl
  load $LIRCAMDIR/define_atasks.icl
  load $LIRCAMDIR/short_display.icl
  load $LIRCAMDIR/defprocs.icl
  load $LIRCAMDIR/setpre.icl
  load $LIRCAMDIR/exit.icl
  load $LIRCAMDIR/lplt.icl
  load $LIRCAMDIR/logo.icl
  set prompt 'Ircam-Clred> '
  LPLT
  SETFILE
  SETPS
  LOGO
END PROC
