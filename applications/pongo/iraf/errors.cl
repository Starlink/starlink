procedure errors (device)

#+
#  Name:
#     errors.cl

#  Purpose:
#     Plot the errors example.

#  Language:
#     IRAF/CL

#  Type of module:
#     CL procedure

#  Arguments:
#     device = string (read)
#        The plotting device.

#  Invocation:
#     task errors = errors.cl
#     errors device

#  Description:
#     Demonstrate the use of error bars and least squares fitting in PONGO.

#  Authors:
#     PDRAPER: P.W. Draper (STARLINK - Durham University)

#  History:
#     9-MAY-1997 (PDRAPER): 
#        Original version

#  Bugs:

#-

string device {"xw",prompt="Display device"}

#.
begin
   string gdname 
   real xmax = 1.0
   real xmin = 0.0

#  Prompt for and read the name of a graphics device.
   gdname = device

#  Begin a PONGO plot.
   begplot (gdname)

#  Reset PONGO.
   resetpongo

#  Set the columns to read from the data file.
   xcolumn 1
   excolumn 2
   ycolumn 3
   eycolumn 4
   symcolumn 0
   labcolumn 0
                              
#  Read the data file.
   readf (data=envget("PONGO_EXAMPLES")//"/tutorial.dat", all=yes, mode="h")

#  Set the plotting limits.
   world data
   xmin = world.xmin.p_value
   xmax = world.xmax.p_value

#  Draw the box and plot the data.
   lweight 2
   boxframe (xopt="bcnst", yopt="bcnvst")
   lweight 1
   errorbar ("x", erterm=0.0)
   errorbar ("y", erterm=0.0)

#  Draw the labels.
   lweight 2
   expand 2
   wtext ("m", 0.5, 0.5, "Interstellar data", side="t", justification=0.5)
   expand 1
   wtext ("m", 3, 0.5, "Colour excess e\\\Db-v\\\D", side="b", justification=0.5)
   wtext ("m", 3, 0.5, "Equivalent width in m\\\A", side="l", justification=0.5)

#  Perform an unweighted least squares to a straight line.
   ltype 2
   unlearn fitline
   fitline (weight=no, colour=3, xmin=xmin, xmax=xmax )

#  Perform a weighted least squares fit to a straight line.
   ltype 1
   fitline (weight=yes, colour=2, xmin=xmin, xmax=xmax )

#  Perform a weighted least squares fit to a Chebyshev polynomial.
   ltype 3
   fitcurve ("poly", npoly=3, weight=yes, colour=4, xmin=xmin, xmax=xmax)

#  This line resets the plotting pen, font, point fill style, line style 
#  and line width to their defaults.
   unlearn change
   change (mode="h")

#  This line resets the character height to 1.0.
   expand 1

#  End the PONGO plot.
   endplot
end
