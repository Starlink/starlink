procedure radec (device)
#+
#  Name:
#     radec.cl

#  Purpose:
#     Plot the ppdotdiag example using RA DEC labels

#  Language:
#     IRAF/CL

#  Type of module:
#     CL procedure

#  Arguments:
#     device = string (read)
#        The plotting device.

#  Invocation:
#     task radec = radec.cl
#     radec device

#  Description:
#     Demonstrate RA and DEC labelling of axis.

#  Authors:
#     PDRAPER: P.W. Draper (STARLINK - Durham University)

#  History:
#     30-MAY-1996 (PDRAPER):
#        Original version.
#     14-MAY-1997 (PDRAPER):
#        Ported to CL.

#  Bugs:

#-

string device {"xw",prompt="Display device"}

#.
begin
   string gdname 

#  Prompt for and read the name of a graphics device.
   gdname = device

#  Begin a PONGO plot.
   begplot (gdname)

#  Reset PONGO.
   resetpongo

#  Set the columns to read from the data file.
   xcolumn RA
   ycolumn Declination
   excolumn 0
   eycolumn 0
   labcolumn 1
   symcolumn 2

#  Read the data file.
   readf (data=envget("PONGO_EXAMPLES")//"/ppdot.dat", 
          selcond="pdot > 0", all=no, quick=no, mode="h")

#  Convert the X- and Y-axis data to radians.
   degtor x
   degtor y

#  Set the plot limits.
   limits (5.5, 4.5, -0.2, 0.2)

#  Draw the box with suitable axes and plot the points.
   lweight 2
   font 3
   boxframe (xopt="bcnstzhg", yopt="bcnstzdg")
   lweight 1
   expand 3
   points INDEF
   expand 1

#  Note how the X-axis label is picked up from the data file.
   lweight 2
   label (columns=yes, title="Pulsar Positions", xlabel="INDEF", ylabel="INDEF")

#  End the PONGO plot.
   endplot
end
