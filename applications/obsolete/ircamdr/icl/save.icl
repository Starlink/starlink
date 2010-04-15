PROC COLL
  print ' '
  asknum (FLATH) 'Enter the current FLAT HORIZONTAL setting \0.0\ '
  asknum (FLATV) 'Enter the current FLAT VERTICAL   setting \0.0\ '
  print ' '
  asknum (COLLH) 'Enter the current COLL HORIZONTAL setting \0.0\ '
  asknum (COLLV) 'Enter the current COLL VERTICAL   setting \0.0\ '
  print ' '
  asknum (INCR) 'Enter increment (in mm) for scan \0.25\ '
  print ' '
  FEXISTS = file_exists ('ircam3_coll.dat')
  IF FEXISTS
    delfile ircam3_coll.dat
  END IF
  create cfile ircam3_coll.dat
  DLINE = '                              **************************'
  write cfile (DLINE)
  DLINE = '                              *IRCAM3 COLLIMATION CHART*'
  write cfile (DLINE)
  DLINE = '                              **************************'
  write cfile (DLINE)
  DLINE = ' '
  write cfile (DLINE)
  DLINE = 'HORIZONTAL COLLIMATION SETTINGS - 0,0 = ' & (FLATH) & ',' & (COLLH)
  write cfile (DLINE)
  DLINE = '-------------------------------------------------------------------------------'
  write cfile (DLINE)
  DLINE = '          HSHIFT       New FLAT HOR    New COLL HOR       VALUE'
  write cfile (DLINE)
  DLINE = '-------------------------------------------------------------------------------'
  write cfile (DLINE)
  LOOP FOR dummy = - 5 TO 5 STEP 1
    HSHIFT = DUMMY * INCR
    NEWFLATH = FLATH + 0.0882000 * HSHIFT
    NEWCOLLH = COLLH + 0.0390000 * HSHIFT
    CHS = REAL (HSHIFT:15:3)
    NFL = REAL (NEWFLATH:15:3)
    NCO = REAL (NEWCOLLH:15:3)
    write cfile (CHS) (NFL) (NCO)
    DLINE = '-------------------------------------------------------------------------------'
    write cfile (DLINE)
  END LOOP
  DLINE = ' '
  write cfile (DLINE)
  DLINE = ' '
  write cfile (DLINE)
  DLINE = 'VERTICAL COLLIMATION SETTINGS - 0,0 = ' & (FLATV) & ',' & (COLLV)
  write cfile (DLINE)
  DLINE = '-------------------------------------------------------------------------------'
  write cfile (DLINE)
  DLINE = '          VSHIFT       New FLAT VER    New COLL VER       VALUE'
  write cfile (DLINE)
  DLINE = '-------------------------------------------------------------------------------'
  write cfile (DLINE)
  LOOP FOR dummy = - 5 TO 5 STEP 1
    VSHIFT = DUMMY * INCR
    NEWFLATV = FLATV - 0.144000 * VSHIFT
    NEWCOLLV = COLLV + 0.0392000 * VSHIFT
    CVS = REAL (VSHIFT:15:3)
    NFL = REAL (NEWFLATV:15:3)
    NCO = REAL (NEWCOLLV:15:3)
    write cfile (CVS) (NFL) (NCO)
    DLINE = '-------------------------------------------------------------------------------'
    write cfile (DLINE)
  END LOOP
  fclose_c
  print 'File ircam3_coll.dat contains user-defined collimation settings'
  print ' '
  sh lp ircam3_coll.dat
END PROC
