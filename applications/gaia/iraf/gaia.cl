#  IRAF initialisation file for the GAIA package.

#  Peter W. Draper 12-February-1999

package gaia

cl < "starlink$irafstar/zzsetenv.def"

#  ----------
#  Procedures
#  ----------
task gaiadisp = "gaia$gaiadisp.cl"

clbye()
