#  IRAF initialisation file for the GAIA package.

#  Peter W. Draper 6-May-2003

package gaia

cl < "starlink$irafstar/zzsetenv.def"

#  ----------
#  Procedures
#  ----------
task gaiadisp = "gaia$gaiadisp.cl"
task gaiacut = "gaiacut.cl"
task gaiadispandcut = "gaiadispandcut.cl"
task gaiapercent = "gaiapercent.cl"

clbye()
