procedure projections (device)

#+
#  Name:
#     projections.cl

#  Purpose:
#     Plot the projections example.

#  Language:
#     IRAF/CL

#  Type of module:
#     CL procedure

#  Arguments:
#     device = string (read)
#        The plotting device.

#  Invocation:
#     LOAD $PONGO_EXAMPLES/projections

#  Description:
#     A PONGO example command file to illustrate different projections.

#  Authors:
#     PDRAPER: Peter W. Draper

#  History:
#     14-MAY-1997 (PDRAPER):
#        Original version

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

#  Set the character height.
   expand 0.7

#  Read the postions of pulsars from the data file.
   readf (data=envget("PONGO_EXAMPLES")//"/ppdot.dat", symcol=2, 
          labcol=1, xcol="RA", ycol="Dec", zcol="Dist", 
          selcond="Dist > 0", all=no, quick=no, mode="h")

#  Convert the XCOL and YCOL data areas from degrees to radians.
   xscale 1.7453292e-2
   yscale 1.7453292e-2

#  Select a viewport in the top left-hand corner.
   vp_tl

#  Get the limits from the data using the AITOFF projection, note that the 
#  RA centre is specified in the form of hh:mm:ss.sss and that below is 
#  understood to mean 12 hours.
   setproj (projection="aitoff", racentre="12", deccentre="0")
   dlimits 

#  Adjust the viewport.
   wnad
   mtext ("t", 1.0, 0.5, 0.5, "Aitoff centre \\\ga=12\\\uh\\\d \\\gd=0\\\(2729)")

#  Draw the coordinate grid.
   grid (mode="h")

#  Plot the points with various symbols.
   pen 6
   points (symbol=INDEF)

#  Plot the same data in the top right-hand corner in a sine projection.
   pen 1
   vp_tr
   setproj (projection="sin", racentre="18", deccentre="30")
   limits (-1.1, 1.1, -1.1, 1.1)
   wnad
   mtext( "t", 1.0, 0.5, 0.5, "SIN centre \\\ga=18\\\uh\\\d \\\gd=30\\\(2729)")
   grid (mode="h")

#  Draw a circle around the projection to make it look prettier.
   arc (1.0, 1.0, 0.0, 0.0, 0.0, 360.0, 0.0, projection="none")
   pen 6
   points (symbol=INDEF)

#  Plot the data in the bottom left-hand corner in a tangent projection.
   pen 1
   vp_bl
   setproj (projection="tan", racentre="0", deccentre="30")
   limits (-4.0, 4.0, -4.0, 4.0)
   wnad
   mtext ("t", 1.0, 0.5, 0.5, "TAN centre \\\ga=0\\\uh\\\d \\\gd=30\\\(2729)")
   grid (mode="h")
   pen 6
   points (symbol=INDEF)

#  Plot the data in the bottom right-hand corner in an arc projection.
   pen 1
   vp_br
   setproj (projection="arc", racentre="12", deccentre="30")
   limits (-3.1, 3.1, -3.1, 3.1)
   wnad
   mtext ("t", 1.0, 0.5, 0.5, "ARC centre \\\ga=12\\\uh\\\d \\\gd=30\\\(2729)")
   grid (mode="h")
   pen 6
   points (symbol=INDEF)

#  This line resets the viewport and the plotting pen, font, point fill style,
#  line style and line width to their defaults.
   vstand
   unlearn change
   change (mode="h")

#  This line resets the character height to 1.0.
   expand 1.0

#  End the PONGO plot.
   endplot
end
