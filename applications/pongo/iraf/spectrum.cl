procedure spectrum (device)
#+
#  Name:
#     spectrum.cl

#  Purpose:
#     Plot the spectrum example.

#  Language:
#     IRAF/CL

#  Type of module:
#     CL procedure

#  Arguments:
#     device = string (read)
#        The display device

#  Invocation:
#     task spectrum = spectrum.cl
#     spectrum device

#  Description:
#     Demonstrate plotting simple spectrum data using PONGO.

#  Authors:
#     PDRAPER: P.W. Draper (STARLINK - Durham University)

#  History:
#     14-MAY-1997 
#        Original version.

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
   xcolumn Wave
   ycolumn Flux
   symcolumn 0
   labcolumn 0
   excolumn 0
   eycolumn 0
                              
#  Read the data file.
   readf (data=envget("PONGO_EXAMPLES")//"/swp3196.lap", 
          selcond="Flux > 0", all=no, mode="h")

#  Set the plotting limits.
   dlimits

#  Draw the box and plot the data.
   lweight 2
   pen 1
   boxframe (xopt="bcnst", yopt="bcnst")
   lweight 1
   pen 4
   connect
   pen 1

#  Draw the labels (use the global values set by READF).
   lweight 2
   label (title="SWP3196, LORES, LAP", columns=yes, xlabel="INDEF", ylabel="INDEF")
	
#  This line resets the plotting pen, font, point fill style, line style 
#  and line width to their defaults.
   unlearn change
   change (mode="h")

#  This line resets the character height to 1.0.
   expand 1

#  End the PONGO plot.
   endplot
end
