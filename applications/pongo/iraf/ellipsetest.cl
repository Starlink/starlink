procedure ellipetest (device)

#+
#  Name:
#     ellipsetest.cl

#  Purpose:
#     Plot the ELLIPSES example.

#  Language:
#     IRAF/CL

#  Type of module:
#     CL procedure

#  Arguments:
#     device = string (read)
#        The graphics device to plot in.

#  Invocation:
#     task ellipsetest = ellipsetest.cl
#     ellipsetest device

#  Description:
#     A PONGO example file to plot error ellipses.

#  Authors:
#     PDRAPER: P.W. Draper (STARLINK - Durham University)

#  History:
#     11-JUN-1994 (PDRAPER):
#        Original version.

#  Bugs:

#-

string device {"xw",prompt="Display device"}

#.
begin
   string gdname
   int ndata

#  Prompt for and read the name of a graphics device.
   gdname = device

#  Begin a PONGO plot.
   begplot (gdname)
   vstand

#  Reset PONGO.
   resetpongo

#  Change the character height to half its normal value.
   expand 0.5

#  Change the axis options global parameters to make the x=0 and y=0
#  lines appear.
   boxframe.xopt = "abcnst"
   boxframe.yopt = "abcnst"

#  Set the error scale parameter.
   ellipses.erscale=2.3
   world.erscale=2.3

#  Set up the readf data columns.
   excolumn 5
   eycolumn 7
   labcolumn 1
   symcolumn 2
   xcolumn 4
   ycolumn 6
   zcolumn 8

#  Alter the viewport to the bottom left corner.
   vp_bl

#  Read the data file, selecting only lines where column 3 is equal to 11.
   readf(data=envget("PONGO_EXAMPLES")//"/ellipses.dat", all=no, 
         selcond="3 = 11", mode="h")

#  Set up the world coordinate limits from the data including the origin.
   world (action="data0")

#  Adjust the viewport to make X- and Y-axis scales equal.
   wnad

#  Draw the box and axes
   boxframe (xopt="abcnst", yopt="abcnst")

#  Write some text relative to the viewport.
   mtext ("t", 1.0, 0.5, 0.5, "Defford only", mode="h")

#  Change the plotting pen.
   pen 2

#  Plot the points - note that because symbol is set to INDEF and
#  values have been read into the symbol column, these values are used
#  to determine the symbol type.
   points (symbol=INDEF)

#  Change the pen again
   pen 4

#  Annotate the graph with labels (these happen to be numbers in this case)
   annotate (mode="h")
   pen 1

#  Draw the error ellipses without their principal axes.
   ellipses (axes=no)

#  Find how many data points have been read (this was written out to the
#  parameter ndata of the readf command).
   ndata = readf.ndata

#  Use the radiate command to draw lines from each of the data points to
#  the origin.
   radiate (0.0, 0.0, ndata)

#  Draw the axis labels and the plot title.
   label ("\\\gm\\\d\\\ga\\\u (mas yr\\\u-1\\\d)",
          "\\\gm\\\d\\\gd\\\u (mas yr\\\u-1\\\d)", "1112+50")

#  Produce two more similar plots in different areas of the screen...
   vp_br
   readf (data=envget("PONGO_EXAMPLES")//"/ellipses.dat", all=no, 
          selcond="3 = 12", mode="h")
   world data0
   wnad
   boxframe
   mtext ("t", 1.0, 0.5, 0.5, "Knockin only")
   pen 2
   points (symbol=INDEF)
   pen 4
   annotate (mode="h")
   pen 1
   ellipses (axes=no)
   radiate (0.0, 0.0, readf.ndata)
   label ("\\\gm\\\d\\\ga\\\u (mas yr\\\u-1\\\d)",
          "\\\gm\\\d\\\gd\\\u (mas yr\\\u-1\\\d)", "1112+50")

#  Second plot
   vp_th
   readf (data=envget("PONGO_EXAMPLES")//"/ellipses.dat", all=no, 
          selcond="3 = 13", mode="h")
   world data0
   wnad
   boxframe
   mtext ("t", 1.0, 0.5, 0.5,  "Defford and Knockin")
   pen 2
   points (symbol=INDEF)
   pen 4
   annotate (mode="h")
   pen 1
   ellipses (axes=no)
   radiate (0.0, 0.0, readf.ndata)
   label ("\\\gm\\\d\\\ga\\\u (mas yr\\\u-1\\\d)",
          "\\\gm\\\d\\\gd\\\u (mas yr\\\u-1\\\d)", "1112+50")

#  Change the character height and viewport back to their default sizes.
   expand 1.0
   vstand

#  This line resets the plotting pen, font, point fill style, line style
#  and line width to their defaults.
   unlearn change
   change (mode="h")

#  End the PONGO plot.
   endplot
end
