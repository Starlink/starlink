procedure ppdotdiag (device)

#+
#  Name:
#     ppdotdiag.cl

#  Purpose:
#     Plot the PPDOTDIAG example.

#  Language:
#     IRAF/CL

#  Type of module:
#     CL procedure

#  Arguments:
#     device = string (read)
#        The display device

#  Invocation:
#     task ppdotdiag = ppdotdiag.cl
#     ppdotdiag device

#  Description:
#     Demonstrate more complex data management using READF and labelling.

#  Authors:
#     PDRAPER: P.W. Draper (STARLINK - Durham University)

#  History:
#     9-MAY-1997 (PDRAPER):
#        Original version

#  Bugs:

#-

string device {"xw", prompt="Display device"}

#.
begin
   string gdname
   string title

#  Prompt for and read the name of a graphics device.
   gdname = device

#  Begin a PONGO plot.
   begplot (gdname)

#  Reset PONGO.
   resetpongo

#  Set the columns to read from the data file.
   xcolumn   Perio
   ycolumn   pdot
   excolumn  0
   eycolumn  0
   labcolumn 1
   symcolumn 2

#  Read the data file.
   readf (data=envget("PONGO_EXAMPLES")//"/ppdot.dat", 
          selcond="pdot > 0", all=no, quick=no, mode="h")

#  Convert the X- and Y-axis data to logarithms.
   xlogarithm
   ylogarithm

#  Set the plot limits.
   limits (-2.95, 1.0, -20.5, -11.0)

#  Draw the box and plot the points.
   lweight 2
   boxframe (yopt="bcnst")
   lweight 1
   points (symbol=INDEF)

#  Note how the X-axis label is picked up from the data file.
   lweight 2
   title = (readf.ndata//" Pulsars")
   label(columns=yes, ylabel="log[Period Derivative (ss\\\u-1\\\d)]",
         title=title, xlabel="INDEF")
   ltype 2
   move (-1.325, -21.0)
   draw ( 1.0,   -14.025)
   ltype 5
   move (-3.0,   -18.9549)
   draw ( 1.0,   -13.626)
   lweight 1
   ltype 1
   expand 0.7
   ptext (-0.7082780, -18.0945511, 0, 0.0, "0655+64")
   ptext (-1.2338796, -17.0440712, 0, 0.0, "1913+16")

   ptext (-0.2164239e+01, -0.1934648e+02, 0, 0.0, "1953+29")
   ptext (-0.2336934e+01, -0.1979958e+02, 0, 1.0, "1855+09")
   ptext (-1.9114622, -17.9879818, 0, 0.0, "1620-26")

   expand 1
   ptext (-1.0945635, -20.1498394, 37, 0.0, "Death Line")
   ptext (-1.4808489, -16.7852573, 19, 0.0, "Spin-up Line")
   endplot

end
