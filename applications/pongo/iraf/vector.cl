procedure vector (device)

#+
#  Name:
#     vector.cl

#  Purpose:
#     Plot the vector example.

#  Language:
#     IRAF/CL

#  Invocation:
#     task vector = "pongoe$vector.cl"
#     vector xw

#  Description:
#     A PONGO example file to illustrate different the use
#     of the VECT and PVECT commands.

#  Authors:
#     PDRAPER: Peter W. Draper (STARLINK - Durham University)

#  History:
#     6-MAY-1997 (PDRAPER):
#        Original version.

#-

string device {"xw",prompt="Display device"}

#.
begin
   string gdname

   #  Prompt for the name of a graphics device.
   gdname=device

   #  Begin a PONGO plot.
   begplot (gdname)

   #  Reset PONGO.
   resetpongo

   #  Read in the data, selecting objects with proper motions
   #  greater than 0.5 arcseconds in RA per year.
   xcolumn 2
   ycolumn 3
   zcolumn 0
   excolumn 4
   eycolumn 5
   symcolumn 0 
   labcolumn 0
   readf ( data=envget("PONGO_EXAMPLES")//"/highppm.dat", xcol=2,
           ycol=3, excol=4, eycol=5, all=no, selcond="4 > 0.5", mode="h")

   #  Convert the proper motions into radians.
   ccmath (ex="ex/3600.0", ey="ey/3600.0")
   degtor ex
   degtor ey

   #  Select top of plot.
   vp_th

   #  Use the AITOFF projection
   setproj ("aitoff", "12", "0")

   # Set the limits of the plot.
   limits (-3.3, 3.3, -1.5, 1.5)
   wnad

   # Draw the background grid.
   lweight 1
   pen 7
   grid (mode="h")

   #  Add a title.
   pen 1
   expand 0.75
   mtext ("t", 1.0, 0.5, 0.5,
   "Bright stars with high proper motions (AITOFF projection centre \\\\ga=12\\\\uh\\\\d \\\\gd=0\\\\(2729))")

   #  Plot the star positions.
   lweight 2
   pen 2
   points 3

   #  And the proper motion vectors. This gives 100000 years of travel.
   pen 4
   pvect (erscale=1.0e5, mode="h")

   #  Now read in and plot the ra and dec labels and their positions.
   xcolumn 2
   ycolumn 3
   zcolumn 0
   excolumn 0
   eycolumn 0
   symcolumn 0 
   labcolumn 1
   readf (data=envget("PONGO_EXAMPLES")//"/gridlabels.dat", all=yes, mode="h")
   degtor x
   degtor y
   pen 1
   annotate (yoff=0.0, xoff=0.0, justification=1.0, mode="h")

   xcolumn 5
   ycolumn 6
   zcolumn 0
   excolumn 0
   eycolumn 0
   symcolumn 0 
   labcolumn 4
   readf (data=envget("PONGO_EXAMPLES")//"/gridlabels.dat", to=9, all=yes, mode="h")
   degtor x
   degtor y
   annotate (yoff=0.0, xoff=0.0, justification=1.0)

   #  Do normal vector plot for bottom half.
   setproj ("none", "0", "0")
   xcolumn 2
   ycolumn 3
   zcolumn 0
   excolumn 4
   eycolumn 5
   symcolumn 0 
   labcolumn 1
   readf (data=envget("PONGO_EXAMPLES")//"/highppm.dat", all=no, selcond="4 > 0.5", mode="h")
   ccmath (ex="ex/3600.0", ey="ey/3600.0")
   degtor ex
   degtor ey
   vp_bh
   dlimits
   wnad

   #  Draw a box with RA and Dec labels.
   lweight 1
   pen 7
   box (xopt="bcnstzhg", yopt="bcnstzdg")

   #  Plot the star positions.
   lweight 2
   pen 2
   points 3

   #  And the proper motion vectors.
   pen 4
   vect (erscale=1.0e5, mode="h")

   #  Close the plot.
   endplot
end
