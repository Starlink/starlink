procedure pongo_demo (device)
#+
#  Name:
#     pongo_demo.cl

#  Purpose:
#     Tests PONGO installation.

#  Language:
#     IRAF/CL

#  Type of module:
#     CL procedure

#  Arguments:
#     device = string (read)
#        The plotting device

#  Invocation:
#     pongo_demo device

#  Description:
#     This routine runs the PONGO example scripts one-by-one to 
#     test the correct installation. The tests are reasonably
#     comprehensive.

#  Authors:
#     PDRAPER: P.W. Draper (STARLINK - Durham University)

#  History:
#     14-MAY-1997 (PDRAPER):
#        Original version.

#  Bugs:

#-

string device {"xw",prompt="Display device"}

#.
begin
   string gdname
   gdname = device
   spectrum (gdname)
   errors (gdname)
   histogramtest (gdname)
   ppdotdiag (gdname)
   ellipsetest (gdname)
   projections (gdname)
   radec (gdname)
   vector (gdname)
   cover (gdname)
end
