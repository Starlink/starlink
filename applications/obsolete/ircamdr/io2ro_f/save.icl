PROC IO2RO istart, iend
  print 'IO2RO converts I and O files to RO files'
  DEF = getenv ('DEFDIR')
   {  def = "''" & def & "''"
  IDIR = getenv ('IDIR')
   {  idir = "''" & idir & "''"
  ODIR = getenv ('ODIR')
   {  odir = "''" & odir & "''"
  RODIR = getenv ('RODIR')
   {  rodir = "''" & rodir & "''"
  get plt2d name_prefix (NAME_PREFIX)
  NAME_PREFIX2 = upcase (NAME_PREFIX)
  IF NAME_PREFIX2 = 'NONE'
    print 'Give UT date of observations to be converted (format 940428) : '
    askname (UTD) 'UT Date ? '
  ELSE
    UTD = substr (NAME_PREFIX, 3, 6)
  END IF
  print ' '
  print 'Current UT date/file prefix = ' (UTD)
  print ' '
  print 'Use SETFILE to change UT date/file prefix'
  print ' '
  print 'Current default directory = ' (DEF)
  print 'Current IDIR              = ' (IDIR)
  print 'Current ODIR              = ' (ODIR)
  print 'Current RODIR             = ' (RODIR)
  print ' '
  print 'Change definitions of IDIR, ODIR, RODIR ? '
  asklog (ICHOICE) 'Change Definitions (Yes or No) \N\ ? '
  IF ICHOICE = 1
    print ' '
    print 'Enter FULL NAME of directory containing the I files : '
    askname (IDIR) 'New IDIR ? '
     {    c1 = substr(idir,1,1)
     {    if c1 = "["
     {      idir = "''"&idir&"''"
     {    end if
    print ' '
    print 'Enter FULL NAME of directory containing the O files : '
    askname (ODIR) 'New ODIR ? '
     {    c1 = substr(odir,1,1)
     {    if c1 = "["
     {      odir = "''"&odir&"''"
     {    end if
    print ' '
    print 'Enter FULL NAME of directory to contain the RO files : '
    askname (RODIR) 'New RODIR ? '
     {    c1 = substr(rodir,1,1)
     {    if c1 = "["
     {      rodir = "''"&rodir&"''"
     {    end if
  END IF
  print ' '
  YN = undefined (ISTART)
  YN2 = undefined (IEND)
  IF YN
    print 'Give start observation for conversion : '
    asknum (OSTART) 'Start Observation Number    \1\ ? '
  ELSE
    OSTART = ISTART
  END IF
  IF YN2
    print 'Give end   observation for conversion : '
    asknum (OEND) 'End Observations Number   \500\ ? '
  ELSE
    OEND = IEND
  END IF
  print ' '
  LOOP FOR jj = (OSTART) TO (OEND) STEP 1
    DATE_OBS = UTD & '_' & JJ
    obeyw io2ro reduce (IDIR) (ODIR) (RODIR) (DATE_OBS)
  END LOOP
END PROC
