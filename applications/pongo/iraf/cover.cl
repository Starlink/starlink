procedure pongo_cover (device)

#+
#  Name:
#     cover.cl

#  Purpose:
#     Plot the SUN/137 cover example.

#  Language:
#     IRAF/CL

#  Type of module:
#     CL procedure

#  Arguments:
#     device = string (read)
#        Plotting device
#     colour = boolean (read)
#        Add extra colour to plot (use for colour devices only).
#     hardcopy = boolean (read)
#        Whether the output is for hardcopy (sets paper size).

#  Invocation:
#     task cover = cover.cl
#     cover device [colour=?]

#  Description:
#     A PONGO example command file to generate the cover of SUN/137.

#  Authors:
#     PDRAPER: P.W. Draper (STARLINK - Durham University)

#  History:
#     14-MAY-1997 (PDRAPER):
#        Original version.

#  Bugs:

#-

string device {"xw",prompt="Display device"}
bool colour {yes,prompt="Add extra colour to plot"}
bool hardcopy {no,prompt="Is output for hardcopy"}

#.
begin
   string gdname
   bool addcol 
   bool hard
   real xmin
   real xmax

#  Get parameters.
   gdname = device
   addcol = colour
   hard = hardcopy

#  Begin plot.
   begplot (gdname)

#  Reset PONGO.
   resetpongo
   if ( addcol ) {
     pen 2
   } else {
     pen 1
   }

#  Set new paper size, if needed.
   if ( hard ) { 
      paper (6, 1)
   }

#  Set the character size.
   expand 0.7

#  Select a viewport in the top left-hand corner.
   viewport ("ndc", 0.0, 0.6, 0.4, 1.0)
   expand 4
   font 4
   lweight 5
   wtext ("p", 0.65, 0.35, "PONGO", justification=0.5, angle=45)
   pen 1
   lweight 2
   expand 2
   wtext ("p", 0.81, 0.63, "\\\(2291)", justification=0.5, angle=45)
   expand 18
   wtext ("p", 0.62, 0.16, "\\\(0844)", justification=0.5, angle=0)
   font 1
   lweight 1
   expand 0.5
   prim.symbol=17
   prim ("k", 0.254, 0.808)
   prim ("k", 0.551, 0.769)
   prim ("k", 0.707, 0.761)
   prim ("k", 0.864, 0.232)
   prim ("k", 0.938, 0.162)
   prim ("k", 0.265, 0.127)
   prim ("k", 0.258, 0.260)
   prim ("k", 0.903, 0.464)
   prim ("k", 0.329, 0.385)
   expand 1.1
   prim ("k", 0.393, 0.692)
   prim ("k", 0.453, 0.692)
   prim ("k", 0.445, 0.645)
   prim ("k", 0.393, 0.629)
   prim ("k", 0.354, 0.575)
   expand 0.8
   prim ("k", 0.393, 0.711)
   expand 0.5
   prim ("k", 0.343, 0.716)
   prim ("k", 0.370, 0.700)
   prim ("k", 0.346, 0.583)
   prim ("k", 0.443, 0.700)
   expand 0.7

#  Select a viewport in the top right-hand corner.
   viewport ("ndc", 0.6, 0.95, 0.6, 0.95)

#  Read the unbinned data into the XCOL data area, but only if their values
#  are greater than zero. Note that the XCOL column has been specified by 
#  its label which is case-sensitive.
   readf (data=envget("PONGO_EXAMPLES")//"/ppdot.dat", xcol="Age", 
          selcond="Age > 0", all=no, quick=no, mode="h")

#  Take logarithms of the X-axis data.
   xlogarithm

#  Set up the limits "manually".
   limits (2.0, 11.0, 0.0, 100.0)

#  Draw a box, but don't draw the right-hand edge. Reset the X-axis options
#  so that they do not contain an "L" option (which has been automatically 
#  added by XLOGARITHM).
   if ( addcol ) {
      pen 5
   }
   boxframe (xopt="bcnst", yopt="bnst")

#  Change the line style.
   ltype 2

#  Bin the data in the XCOL data area between 2 and 11, with 18 bins.
#  Draw these data using the world coordinate limits already set up.
   histogram (binmin=2.0, binmax=11.0, nbin=18, autoscale=no)

#  Write a label describing the histogram.
   ltype 1
   label ("Log [Characteristic age (yr)]", "Number of old pulsars (dashed)", "")
   mtext ("t", 0.5, 0.5, 0.5, "Pulsar Age Distribution")

#  Read a second data file (the data are already logarithmic).
   readf (data=envget("PONGO_EXAMPLES")//"/cljon.dat", xcol=1, ycol=0,
          zcol=0, eycol=0, excol=0, all=yes, quick=no, mode="h")

#  Set up new limits.
   limit (2.0, 11.0, 0.0, 11.0)

#  Draw a box, but this time do not draw the left-hand edge.
   boxframe (xopt="bcnst", yopt="cmst")
   ltype 4
   histogram (binmin=2.0, binmax=11.0, nbin=18, autoscale=no)

#  Write a label describing the second histogram.
   ltype 1
   mtext ("r", 2.5, 0.5, 0.5, "Number of new pulsars (dotted)")

#  This line resets the plotting pen, font, point fill style, line style 
#  and line width to their defaults.
   unlearn change
   change (mode="h")
   if ( addcol ) { 
      pen 7
   }

#  Select a viewport in the bottom left-hand corner.
   viewport ("ndc", 0.1, 0.45, 0.1, 0.45)

#  Set the columns to read from the data file.
   xcolumn 1
   excolumn 2
   ycolumn 3
   eycolumn 4
   symcolumn 0
   labcolumn 0

#  Read the data file.
   readf (data=envget("PONGO_EXAMPLES")//"/tutorial.dat", all=yes, 
          quick=no, mode="h")

#  Set the plotting limits.
   dlimits

#  Draw the box and plot the data.
   expand 0.7
   boxframe (xopt="bcnst", yopt="bcnvst")
   errorbar ("x", erterm=0.0)
   errorbar ("y", erterm=0.0)

#  Draw the labels.
   wtext ("m", 0.5, 0.5, "Interstellar data", side="t", justification=0.5)
   wtext ("m", 3, 0.5, "Colour excess E\\\dB-V\\\d", side="b", justification=0.5)
   wtext ("m", 3, 0.5, "Equivalent width in m\\\A", side="l", justification=0.5)

#  Perform an unweighted least squares to a straight line.
   if ( addcol ) { 
      pen 5
   }
   ltype 2
   xmin = world.xmin.p_value
   xmax = world.xmax.p_value
   fitline (weight=no, colour=1, xmin=xmin, xmax=xmax)

#  Perform a weighted least squares fit to a polynomial.
   if ( addcol ) { 
      pen 4 
   } 
   ltype 1
   fitcurve ("poly", 2, weight=yes, colour=1, xmin=xmin, xmax=xmax)

#  This line resets the plotting pen, font, point fill style, line style 
#  and line width to their defaults.
   unlearn change
   change (mode="h")
   if ( addcol ) { 
      pen 8
   }

#  Select a viewport in the bottom right-hand corner.
   vp_br

#  Read the postions of pulsars from the data file.
   readf (data=envget("PONGO_EXAMPLES")//"/ppdot.dat", symcol=2, 
          labcol=1, xcol="RA", ycol="Dec", zcol="Dist", 
          selcond="Dist > 0",  all=no, quick=no, mode="h")

#  Convert the XCOL and YCOL data areas from degrees to radians.
   degtor x
   degtor y

#  Plot the data in the bottom right-hand corner in a sine projection.
   setproj ("sin", "18", "30")
   limits (-1.1, 1.1, -1.1, 1.1)
   wnad
   expand 0.7
   mtext ("t", 1.0, 0.5, 0.5, "SIN centre \\\ga=18\\\uh\\\d \\\gd=30\\\(2729)")
   grid (mode="h")

#  Draw a circle around the projection to make it look prettier.
   arc (1.0, 1.0, 0.0, 0.0, 0.0, 360.0, 0.0, projection="none")
   if ( addcol ) { 
      pen 3
   }
   points (symbol=INDEF)

#  This line resets the viewport and the plotting pen, font, point fill style,
#  line style and line width to their defaults.
   vstand
   unlearn change
   change (mode="h")

#  This line resets the character height to 1.0.
   expand 1
   endplot
end
