procedure ccdexercise ( device )
   string device   {"xw", prompt="Image display device (NONE for none)"}

#+
#  Name:
#     ccdexercise

#  Purpose:
#     Tests basic CCDPACK functionality from CL.

#  Language:
#     IRAF CL script.

#  Description:
#     The script creates a series of test frames using CCDGENERATE. It
#     then executes various of the CCDPACK tasks simulating a
#     reduction sequence. Intermediary results are displayed if
#     required (note this functionality isn't available yet as no
#     KAPPA from CL).

#  Authors:
#     PDRAPER: Peter Draper (STARLINK)
#     {enter_new_authors_here}

#  History:
#     2-APR-1997 (PDRAPER):
#        Original version.
#     {enter_further_changes_here}
#-
#

begin
   string local_dev
   local_dev = device

   #  Parameter mode is hidden by default.
   set mode = h

   #  Make sure CCDPACK is available.
   if ( ! defpac("ccdpack") ) {
     print "!!Sorry cannot exercise CCDPACK as package not initialised"
     bye
   }

   #  If we're going to use a device we need KAPPA.
   if ( local_dev != "NONE" ) {
      if ( ! defpac( "kappa" ) ) {
         print "!!Sorry cannot display images as KAPPA package is not initialised"
         local_dev="NONE"
      } else {
         idset ( device=local_dev )
         gdset ( device=local_dev )
         gdclear ( device=local_dev )
         paldef ( device=local_dev )
         palentry ( device=local_dev, colour="white", palnum=0 )
      }
   }

   #  Try to locate the object specification file. If this does not exist
   #  then exit.
   if ( access ( "ccdtest_obj.dat" ) ) {
     delete ccdtest_obj.dat
   }
   if ( access ( "ccdpackdir$/ccdtest_obj.dat" ) ) {
     copy ( "ccdpackdir$/ccdtest_obj.dat", "ccdtest_obj.dat" )
   } else {
     print "!!Cannot locate file ccdpackdir$/ccdtest_obj.dat - test terminated."
     bye
   }

   #  Establish the current directory as ADAM_USER and clear
   #  any global values already existing
   reset ADAM_USER=osfn( "." )
   if ( access ( "ADAM_USER$/GLOBAL.sdf" ) ) {
     ccdclear byname=no
   }
   reset imdir=osfn( "." )

   #  Try to locate the ARD file which goes with the test. If this is
   #  not found then proceed without it.
   print " "
   print "  Setting the characteristics of the CCD device using the "
   print "  CCDSETUP routine."
   print " "
   if ( access( "ccdtest.ard" ) ) {
     delete ccdtest.ard
   }
   if ( access( "ccdpackdir$/ccdtest.ard" ) ) {
     copy ( "ccdpackdir$/ccdtest.ard", "ccdtest.ard" )
     ccdsetup ( restore=no, adc=1, bounds="1,5,120,128",
                rnoise=9.95, direction="x", deferred=INDEF,
                extent="6,119,1,128", logto="both",
                logfile="ccdtest.log", preserve=yes, genvar=no,
                ndfnames=yes, save=no, saturate=no,
                mask="ccdtest.ard", maskname="ccdtest.ard" )
   } else {
     print "Cannot locate ARD file; mask not applied"
     ccdsetup ( restore=no, adc=1, bounds="1,5,120,128",
                rnoise=9.95, direction="x", deferred=INDEF,
                extent="6,119,1,128", logto="both",
                logfile="ccdtest.log", preserve=yes, genvar=no,
                ndfnames=yes, save=no, saturate=no,
                mask=INDEF, maskname=INDEF )
   }

   #  Create the test frames
   print " "
   print "  Creating the test data. "
   print " "
   print "  The test data consists of :-"
   print "     4 bias frames    "
   print "     4 flatfields     "
   print "     4 object fields. "
   print " "
   print "  The target data is a simulated galaxy cluster in which the"
   print "  telescope position has been moved between exposures."
   print " "

   #  Make the output files of type ".imh"
   reset NDF_FORMATS_OUT="IRAF(.imh),.,*"
   ccdgenerate ( nseq=4, file="ccdtest_obj.dat",
                 ubnds="128,128,166,128,128,201,166,201",
                 lbnds="1,1,39,1,1,74,39,74" )

#  If display capability is enabled then display the DATA frames.
   if ( local_dev != "NONE" ) {
     print " "
     print "  Displaying raw target frames."
     print " "
     picdef ( mode="array", xpic=2, ypic=2, prefix="a" )
     lutheat
     picsel ( label="a1" )
     display ( in=data1.imh, mode=percentiles, percentiles="2,98" )
     picsel ( label="a2" )
     display ( in=data2.imh, mode=percentiles, percentiles="2,98" )
     picsel ( label="a3" )
     display ( in=data3.imh, mode=percentiles , percentiles="2,98" )
     picsel ( label="a4" )
     display ( in=data4.imh, mode=percentiles, percentiles="2,98" )
   }

   #  Add note to logfile.
   print " "
   print "  Adding a note to the logfile. "
   print " "
   ccdnote ( note="userid$ : exercising CCDPACK" )

   #  Make a BIAS frame.
   print " "
   print "  Producing a master bias calibration frame. This is produced"
   print "  by median stacking the ordinary bias frames. This process"
   print "  reduces the noise introduced in debiassing."
   print " "
   makebias ( in="bias*.imh", out="master_bias.imh", method="median" )

   #  DEBIAS all frames (including flatfields)
   debias ( in="data?.imh,ff?.imh", out="debias_*", bias="master_bias.imh" )

   #  Display all the debiassed frames.
   if ( local_dev != "NONE" ) {
     print " "
     print "  Displaying debiassed target frames. Note the absence of the"
     print "  bias strips (which were along the Y edges) and the removal"
     print "  of defective regions. The flatfields have also been debiassed"
     print "  but are not displayed."
     print " "
     gdclear
     picdef ( mode=array, xpic=2, ypic=2, prefix="a" )
     picsel ( label=a1 )
     display ( in=debias_data1.imh, mode=percentiles, percentiles= "2,98" )
     picsel ( label=a2 )
     display ( in=debias_data2.imh, mode=percentiles, percentiles="2,98" )
     picsel ( label=a3 )
     display ( in=debias_data3.imh, mode=percentiles, percentiles="2,98" )
     picsel ( label=a4 )
     display ( in=debias_data4.imh, mode=percentiles, percentiles="2,98" )
   }

   #  Create a flat field master
   print " "
   print "   Producing a master flatfield. This frame will be used to correct"
   print "   for the sensitivity variations in the detector response and"
   print "   any variations in the optical throughput (vignetting)."
   print "   As in the creation of the master bias frame median stacking is"
   print "   used to combine a series of flatfields."
   print " "
   makeflat ( in="debias_ff?.imh", out="master_flat.imh", method="median" )

   #  Display the master flatfield.
   if ( local_dev != "NONE" ) {
     print " "
     print "  Displaying master flatfield (the flatfield used in this case"
     print "  is a ramp, normal flatfields are not like this). "
     print " "
     gdclear
     display ( in=master_flat, mode=percentiles, percentiles="2,98" )
   }

   #  Flatfield all the DATA frames
   print " "
   print "  Flatfielding all the target frames."
   print " "
   flatcor ( in="debias_data?.imh", out="*|debias|reduced|",
             flat="master_flat.imh" )
   if ( local_dev != "NONE" ) {
     print " "
     print "  Displaying flatfielded target frames."
     print " "
     gdclear
     picdef ( mode=array, xpic=2, ypic=2, prefix="a" )
     lutheat
     picsel ( label=a1 )
     display ( in=reduced_data1.imh, mode=percentiles, percentiles="2,98" )
     picsel ( label=a2 )
     display ( in=reduced_data2.imh, mode=percentiles, percentiles="2,98" )
     picsel ( label=a3 )
     display ( in=reduced_data3.imh, mode=percentiles, percentiles="2,98" )
     picsel ( label=a4 )
     display ( in=reduced_data4.imh, mode=percentiles, percentiles="2,98" )
   }

   #  Now proceed to test out some alignment functionality.
   print " "
   print "  CCDPACK will now attempt to realign the target frames to"
   print "  produce a complete mosaic of whole of the target region."
   print " "

   #  Locate all the objects on the frames
   print " "
   print "  The first stage of the automated registration process is"
   print "  to detect the positions of objects (stars and galaxies)."
   print "  "
   findobj ( in="reduced_data?.imh", outlist="*|.imh|.find|",
             percentile=95, minpix=6 )
   if ( local_dev != "NONE" ) {
     #  Display the objects located.
     print " "
     print "  Displaying the positions of the object which have been"
     print "  detected."
     print " "
     picsel ( label=a1 )
     plotlist ( inlist="reduced_data1.imh", palnum=4, mtype=23 )
     picsel ( label=a2 )
     plotlist ( inlist="reduced_data2.imh" )
     picsel ( label=a3 )
     plotlist ( inlist="reduced_data3.imh" )
     picsel ( label=a4 )
     plotlist ( inlist="reduced_data4.imh" )
   }

   #  Determine matches between the positions.
   print " "
   print "  After locating the objects it is now necessary to determine"
   print "  which objects correspond."
   print " "
   findoff ( inlist="reduced_data?.imh", ndfnames=yes, 
             outlist="*|.imh|.off|", error=1, fast=yes, failsafe=yes )
   if ( local_dev != "NONE" ) {
     #  Display the objects located.
     print " "
     print "  Displaying the labels of objects which have been matched."
     print " "
     picsel ( label=a1 )
     plotlist ( inlist="reduced_data1.imh", palnum=3, mtype=-1,
                thick=2, msize=1.5 )
     picsel ( label=a2 )
     plotlist ( inlist="reduced_data2.imh" )
     picsel ( label=a3 )
     plotlist ( inlist="reduced_data3.imh" )
     picsel ( label=a4 )
     plotlist ( inlist="reduced_data4.imh" )
   }

   #  Set the registration structures.
   print " "
   print "  Now that the object-object correspondence is known it is"
   print "  possible to work out the inter-image transformations."
   print "  The next routine does this for a range of different"
   print "  transformation types. It also writes the information into the"
   print "  images so that other routines may use it."
   print " "
   register ( inlist="reduced_data?.imh", fittype=1 )

   #  Resample the data.
   print " "
   print "  The reduced images will now be resampled to the same coordinate"
   print "  system. After this is performed they can then be combined"
   print "  (after determining normalising scale and zero points which "
   print "  take into account any variations in sky transparency and "
   print "  exposure time) into a single frame which shows the complete"
   print "  data coverage for the target area."
   print " "
   tranndf ( in="reduced_data?.imh", out="*|reduced|resamp|" )

   #  Normalise it.
   print " "
   print "  Normalising and combining the aligned datasets."
   print " "
   makemos ( in="resamp_data?.imh", scale=yes, zero=yes, out="mosaic.imh" )

   #  Display the final mosaic.
   if ( local_dev != "NONE" ) {
     print " "
     print "  Displaying the final mosaic."
     print " "
     gdclear
     display ( in=mosaic.imh, mode=percentiles, percentiles="2,98" )
   }

   #  Exercise is completed.
   ccdnote ( note = "Exercise completed" )
end
