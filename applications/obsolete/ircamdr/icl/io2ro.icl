{ PROCEDURE IO2RO : converts a range of RO files from new to old format
proc IO2RO istart iend
  print "IO2RO converts I and O files to RO files"
  def = getenv("DEFDIR")
{  def = "''" & def & "''"
  idir = getenv("IDIR")
{  idir = "''" & idir & "''"
  odir = getenv("ODIR")
{  odir = "''" & odir & "''"
  rodir = getenv("RODIR")
{  rodir = "''" & rodir & "''"
  get plt2d name_prefix (name_prefix)
  name_prefix2 = upcase(name_prefix)
  if name_prefix2 = "NONE"
    print "Give UT date of observations to be converted (format 940428) : "
    askname (utd) "UT Date ? "
  else
    utd = substr(name_prefix,3,6)
  end if
  print " "
  print "Current UT date/file prefix = " (utd)
  print " "
  print "Use SETFILE to change UT date/file prefix"
  print " "
  print "Current default directory = " (def)
  print "Current IDIR              = " (idir)
  print "Current ODIR              = " (odir)
  print "Current RODIR             = " (rodir)
  print " "
  print "Change definitions of IDIR, ODIR, RODIR ? "
  asklog (ichoice) "Change Definitions (Yes or No) \N\ ? "
  if ichoice = 1
    print " "
    print "Enter FULL NAME of directory containing the I files : "
    askname (idir) "New IDIR ? "
{    c1 = substr(idir,1,1)
{    if c1 = "["
{      idir = "''"&idir&"''"
{    end if
    print " "
    print "Enter FULL NAME of directory containing the O files : "
    askname (odir) "New ODIR ? "
{    c1 = substr(odir,1,1)
{    if c1 = "["
{      odir = "''"&odir&"''"
{    end if
    print " "
    print "Enter FULL NAME of directory to contain the RO files : "
    askname (rodir) "New RODIR ? "
{    c1 = substr(rodir,1,1)
{    if c1 = "["
{      rodir = "''"&rodir&"''"
{    end if
  end if
  print " "
  yn = undefined(istart)
  yn2 = undefined(iend)
  if yn
    print "Give start observation for conversion : "
    asknum (ostart) "Start Observation Number    \1\ ? "
  else
    ostart = istart
  end if
  if yn2
    print "Give end   observation for conversion : "
    asknum (oend) "End Observations Number   \500\ ? "
  else
    oend = iend
  end if
  print " "
  loop for jj = (ostart) to (oend)
    date_obs = utd & "_" & jj
    obeyw io2ro reduce (idir) (odir) (rodir) (date_obs)
  end loop
end proc
