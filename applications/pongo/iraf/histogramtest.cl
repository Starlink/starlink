procedure histogramtest (device)
#+
#  Name:
#     histogramtest.cl

#  Purpose:
#     Plot the histogram example.

#  Language:
#     IRAF/CL

#  Type of module:
#     CL procedure

#  Arguments:
#     device = string (read)
#       The display device.

#  Invocation:
#     task histogramtest = histogramtest.cl
#     histogram device

#  Description:
#     A PONGO example command file to plot histograms.

#  Authors:
#     P.W. Draper (STARLINK - Durham University)

#  History:
#     9-MAY-1997 (PDRAPER):
#        Original version.

#  Bugs:

#-

string device {"xw", prompt="Display device"}

#.
begin
    string gdname

#  Prompt for and read the name of a graphics device.
   gdname = device

#  Begin a PONGO plot.
   begplot (gdname)

#  Reset PONGO.
   resetpongo

#  Set the line width to 2.
   lweight 2

#  Read the unbinned data into the XCOL data area, but only if their values
#  are greater than zero. Note that the XCOL column has been specified by 
#  its label which is case-sensitive.
   readf (data=envget("PONGO_EXAMPLES")//"/ppdot.dat", xcol="Age", 
          selcond="Age > 0", all=no, mode=h)

#  Take logarithms of the X-axis data.
   xlogarithm

#  Set up the limits "manually".
   limits (2.0, 11.0, 0.0, 100.0)

#  Draw a box, but don't draw the right-hand edge. Reset the X-axis options
#  so that they do not contain an "L" option (which has been automatically 
#  added by XLOGARITHM).
   boxframe (xopt="bcnst", yopt="bnst")

#  Change the line style.
   ltype 2

#  Bin the data in the XCOL data area between 2 and 11, with 18 bins.
#  Draw these data using the world coordinate limits already set up.
   histogram (binmin=2.0, binmax=11.0, nbin=18, autoscale=no)

#  Write a label describing the histogram.
   ltype 1
   label ("Log [Characteristic age (yr)]", "Number of old pulsars (dashed)", "")
   expand 2
   mtext ("t", 1.0, 0.5, 0.5, "Pulsar Age Distribution")
   expand 1

#  Read a second data file (the data are already logarithmic).
   readf (data=envget("PONGO_EXAMPLES")//"/cljon.dat", xcol="1", 
          ycol="0", zcol="0", eycol="0", excol="0", all=yes,mode="h")

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

#  This line resets the character height to 1.0.
   expand 1

#  End the PONGO plot.
   endplot
end
