procedure interactive (device)
#+
#  Name:
#     interactive.cl

#  Purpose:
#     Plot the INTERACTIVE example.

#  Language:
#     IRAF/CL

#  Invocation:
#     task interactive = interactive.cl
#     interactive device

#  Description:
#     Demonstrate interaction with PONGO plots using the CURSE command.

#  Authors:
#     PDRAPER: P.W. Draper (STARLINK - Durham University)

#  History:
#     6-MAY-1997 (PDRAPER):
#       Original version

#  Bugs:

#-

string device {"xw",prompt="Display device"}

#.
begin
   string gdname
   string titlestring 
   string ylabel

#  Prompt for and read the name of a graphics device.
   gdname=device

#  Begin a PONGO plot.
   begplot (gdname)

#  Reset PONGO.
   resetpongo

#  Read the data file.
   readf (data=envget("PONGO_EXAMPLES")//"/ppdot.dat", selcond="pdot > 0", 
          xcol="Period", ycol="pdot", excol="0", eycol="0",
          labcol="1", symcol="2", zcol="Dist", all=no, quick=no)

#  Convert the X- and Y-axis data to logarithms.
   xlogarithm
   ylogarithm

#  Set the plot limits.
   limits (-2.95, 1.0, -20.5, -11.0)

#  Draw the box and plot the points.
   lweight 2
   boxframe
   lweight 1
   points 1

#  Create a title from the number of points that have been read in.
   titlestring = (readf.ndata // " pulsars")

#  The Y-axis label.
   ylabel='''log[Period Derivative (ss\u-1\\\\d)]'''

#  Note how the X-axis label is picked up from the data file.
   lweight 2
   label (columns=yes, ylabel=ylabel, title=titlestring, mode="h")
   lweight 1

#  Invoke the CURSE command.
   print ("Invoking the CURSE command.")
   curse

#  Invoke the WRITEI command.
   writei (action="lablst",mode="h")

#  Redraw the graph using SIZEPLOT.
   print ("Invoking the ADVANCE command.")
   advance 
   print ("Invoking the BOXFRAME command.")
   lweight 2
   boxframe
   print ("Invoking the LABEL command.")
   label (mode="h")
   lweight 1
   print ("Invoking the command CCMATH Z=1/(Z+0.1).")
   ccmath (z="1/(Z+0.1)")
   print ("Invoking the SIZEPLOT command to plot scaled symbols.")
   sizeplot

#  This line resets the plotting pen, font, point fill style, line style 
#  and line width to their defaults.
   unlearn change
   change

#  This line resets the character height to 1.0.
   change (cheight=1.0)

#  End the PONGO plot.
   endplot
end
