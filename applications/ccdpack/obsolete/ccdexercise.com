$!+
$!  Name:
$!     CCDEXERCISE
$!
$!  Purpose:
$!     Tests CCDPACK functionality.
$!
$!  Language:
$!     DCL
$!
$!  Description:
$!     This procedure creates a series of test frames using CCDGENERATE. It
$!     then executes various of the CCDPACK Atasks simulating a
$!     reduction sequence. Intermediary results are displayed (if requested)
$!
$!  Authors:
$!     PDRAPER: Peter Draper (STARLINK)
$!     {enter_new_authors_here}
$!
$!  History:
$!     4-APR-1992 (PDRAPER):
$!        Original version.
$!     29-JUN-1993 (PDRAPER):
$!     	  Changed to include automated alignment.
$!     5-JUL-1993 (PDRAPER):
$!        DCL version
$!     {enter_further_changes_here}
$!-
$      ECHO := WRITE SYS$OUTPUT
$!
$!  Try to locate the object specification file. If this does not exists
$!  then exit.
$     IF ( F$SEARCH( "CCDPACK_DIR:CCDTEST_OBJ.DAT" ) .EQS. "" )
$     THEN
$        ECHO "Cannot locate file CCDPACK_DIR:CCDTEST_OBJ.DAT"
$        ECHO "- test terminated."
$        GOTO EXIT
$     ENDIF
$!
$!  Establish the current directory as ADAM_USER.
$     ON CONTROL_Y THEN GOTO EXIT
$     DEFINE/NOLOG OLD_USER  'F$TRNLNM( "ADAM_USER" )
$     DEFINE/NOLOG ADAM_USER 'F$ENVIRONMENT( "DEFAULT" )
$     ADAMSTART
$!
$!  Does the user want to see image display activity?
$     ECHO " "
$     ECHO "  Give the name of an image display device. Not using an image
$     ECHO "  display considerably speeds the exercise."
$     ECHO " "
$     INQUIRE/NOPUNC DEVICE "DISPLAY - Display device (<CR> for none) > "
$     IF ( DEVICE .EQS. "!!" ) 
$     THEN
$        GOTO EXIT
$     ELSE
$        IF ( DEVICE .EQS. "" )
$        THEN
$           DEVICE = "NONE"
$        ENDIF
$     ENDIF
$!
$!  If we're going to use a device we need KAPPA.
$     IF ( DEVICE .NES. "NONE" )
$     THEN
$        KAPPA
$        IDSET 'DEVICE ACCEPT
$        GDSET 'DEVICE ACCEPT
$        GDCLEAR ACCEPT
$        PALDEF ACCEPT
$        PALENTRY COLOUR=WHITE PALNUM=0 ACCEPT
$     ENDIF
$!
$!  Initialise CCDPACK.
$     CCDPACK
$!
$!  Clear any existing CCDPACK parameters.
$     CCDCLEAR ACCEPT 
$!
$!  Try to locate the ARD file which goes with the test. If this is 
$!  not found then proceed without it.
$     ECHO " "
$     ECHO "  Setting the characteristics of the CCD device using the "
$     ECHO "  CCDSETUP routine."
$     ECHO " "
$     IF ( F$SEARCH( "CCDPACK_DIR:CCDTEST_ARD.DAT" ) .NES. "" )
$     THEN
$!
$!  Found the ARD file setup CCDPACK appropriately.
$        CCDSETUP -
            BOUNDS=[1,5,120,128] -
            EXTENT=[6,119,1,128] -
            ADC=1 -
            RNOISE=9.95 -
            LOGTO=BOTH -
            LOGFILE=CCDTEST.LOG -
            PRESERVE=TRUE -
            DIRECTION=X -
            MASK=CCDPACK_DIR:CCDTEST_ARD.DAT -
            RESET ACCEPT
$!       END CCDSETUP
$     ELSE
$        ECHO "Cannot locate ARD file; mask not applied"
$
$!  Do the "device" setup without an ARD file.
$        CCDSETUP -
            BOUNDS=[1,4,120,128] -
            EXTENT=[6,119,1,128] -
            ADC=1 -
            RNOISE=10.0 -
            LOGTO=BOTH -
            LOGFILE=CCDTEST.LOG -
            PRESERVE=TRUE -
            DIRECTION=X -
            RESET ACCEPT
$!       END CCDSETUP
$      ENDIF
$!
$!  Create the test frames
$     ECHO " "
$     ECHO "  Creating the test data. "
$     ECHO " "
$     ECHO "  The test data consists of :-"
$     ECHO "     4 bias frames    "
$     ECHO "     4 flatfields     "
$     ECHO "     4 object fields. "
$     ECHO " "
$     ECHO "  The target data is a simulated galaxy cluster in which the"
$     ECHO "  telescope position has been moved between exposures."
$     ECHO " "
$     CCDGENERATE -
         NSEQ=4 -
         FILE=CCDPACK_DIR:CCDTEST_OBJ.DAT -
         UBNDS=[128,128,166,128,128,201,166,201] -
         LBNDS=[1,1,39,1,1,74,39,74] -
         ACCEPT
$!    END CCDGENERATE
$!
$!  If display capability is enabled then display the DATA frames.
$     IF ( DEVICE .NES. "NONE" )
$     THEN
$        ECHO " "
$        ECHO "  Displaying raw target frames."
$        ECHO " "
$        PICDEF -
            MODE=ARRAY -
            XPIC=2 -
            YPIC=2 -
            PREFIX=A -
            ACCEPT
$!       END PICDEF
$        LUTHEAT -
            ACCEPT
$!       END LUTHEAT
$        PICSEL -
            LABEL=A1 -
            ACCEPT
$!       END PICSEL
$        DISPLAY -
            IN=DATA1 -
            MODE=PERCENTILES -
            PERCENTILES=[2,98] -
            ACCEPT
$!       END DISPLAY
$        PICSEL -
            LABEL=A2 -
            ACCEPT
$!       END PICSEL
$        DISPLAY -
            IN=DATA2 -
            MODE=PERCENTILES -
            PERCENTILES=[2,98] -
            ACCEPT
$!       END DISPLAY
$        PICSEL -
            LABEL=A3 -
            ACCEPT
$!       END PICSEL
$        DISPLAY -
            IN=DATA3 -
            MODE=PERCENTILES -
            PERCENTILES=[2,98] -
            ACCEPT
$!       END DISPLAY
$        PICSEL -
            LABEL=A4 -
            ACCEPT
$!       END PICSEL
$        DISPLAY -
            IN=DATA4 -
            MODE=PERCENTILES -
            PERCENTILES=[2,98] -
            ACCEPT
$!       END DISPLAY
$     ENDIF
$! 
$!  Add note to logfile.
$     ECHO " "
$     ECHO "  Adding a note to the logfile. "
$     ECHO " "
$     CCDNOTE -
         "''f$process()': exercising CCDPACK" -
         ACCEPT
$!    END CCDNOTE
$!
$!  Make a BIAS frame.
$     ECHO " "
$     ECHO "  Producing a master bias calibration frame. This is produced"
$     ECHO "  by median stacking the ordinary bias frames. This process"
$     ECHO "  reduces the noise introduced in debiassing."
$     ECHO " "
$     MAKEBIAS -
         IN=BIAS* -
         OUT=MASTER_BIAS -
         ACCEPT
$!    END MAKEBIAS
$!
$!  DEBIAS all frames (including flatfields)
$     DEBIAS -
         IN="DATA%,FF%" -
         OUT=DEBIAS_* -
         ACCEPT
$!    END DEBIAS
$!
$!  Display all the debiassed frames.
$     IF ( DEVICE .NES. "NONE" )
$     THEN
$        ECHO " "
$        ECHO "  Displaying debiassed target frames. Note the absence of the"
$        ECHO "  bias strips (which were along the Y edges) and the removal"
$        ECHO "  of defective regions. The flatfields have also been debiassed"
$        ECHO "  but are not displayed."
$        ECHO " "
$        GDCLEAR ACCEPT
$        PICDEF -
            MODE=ARRAY -
            XPIC=2 -
            YPIC=2 -
            PREFIX=A -
            ACCEPT
$!       END PICDEF
$        PICSEL -
            LABEL=A1 -
            ACCEPT
$!       END PICSEL
$        DISPLAY -
            IN=DEBIAS_DATA1 -
            MODE=PERCENTILES -
            PERCENTILES=[2,98] -
            ACCEPT
$!       END DISPLAY
$        PICSEL -
            LABEL=A2 -
            ACCEPT
$!       END PICSEL
$        DISPLAY -
            IN=DEBIAS_DATA2 -
            MODE=PERCENTILES -
            PERCENTILES=[2,98] -
            ACCEPT
$!       END DISPLAY
$        PICSEL -
            LABEL=A3 -
            ACCEPT
$!       END PICSEL
$        DISPLAY -
            IN=DEBIAS_DATA3 -
            MODE=PERCENTILES -
            PERCENTILES=[2,98] -
            ACCEPT
$!       END DISPLAY
$        PICSEL -
            LABEL=A4 -
            ACCEPT
$!       END PICSEL
$        DISPLAY -
            IN=DEBIAS_DATA4 -
            MODE=PERCENTILES -
            PERCENTILES=[2,98] -
            ACCEPT
$!       END DISPLAY
$     ENDIF
$!
$!  Create a flat field master
$     ECHO " "
$     ECHO "   Producing a master flatfield. This frame will be used to correct"
$     ECHO "   for the sensitivity variations in the detector response and"
$     ECHO "   any variations in the optical throughput (vignetting)."
$     ECHO "   As in the creation of the master bias frame median stacking is"
$     ECHO "   used to combine a series of flatfields."
$     ECHO " "
$     MAKEFLAT -
         IN=DEBIAS_FF% -
         OUT=MASTER_FLAT -
         ACCEPT
$!    END MAKEFLAT
$!  Display the master flatfield.
$     IF ( DEVICE .NES. "NONE" )
$     THEN 
$        ECHO " "
$        ECHO "  Displaying master flatfield (the flatfield used in this case"
$        ECHO "  is a ramp, normal flatfields are not like this). "
$        ECHO " "
$        GDCLEAR ACCEPT
$        DISPLAY -
            IN=MASTER_FLAT -
            MODE=PERCENTILES -
            PERCENTILES=[2,98] -
            ACCEPT
$!       END DISPLAY
$     ENDIF
$!
$!  Flatfield all the DATA frames
$     ECHO " "
$     ECHO "  Flatfielding all the target frames."
$     ECHO " "
$     FLATCOR -
         IN=DEBIAS_DATA% -
         OUT=*|DEBIAS|REDUCED| -
         ACCEPT
$!    END FLATCOR
$     IF ( DEVICE .NES. "NONE" )
$     THEN
$        ECHO " "
$        ECHO "  Displaying flatfielded target frames."
$        ECHO " "
$        GDCLEAR ACCEPT
$        PICDEF -
            MODE=ARRAY -
            XPIC=2 -
            YPIC=2 -
            PREFIX=A -
            ACCEPT
$!       END PICDEF
$        LUTHEAT -
            ACCEPT
$!       END LUTHEAT
$        PICSEL -
            LABEL=A1 -
            ACCEPT
$!       END PICSEL
$        DISPLAY -
            IN=REDUCED_DATA1 -
            MODE=PERCENTILES -
            PERCENTILES=[2,98] -
            ACCEPT
$!       END DISPLAY
$        PICSEL -
            LABEL=A2 -
            ACCEPT
$!       END PICSEL
$        DISPLAY -
            IN=REDUCED_DATA2 -
            MODE=PERCENTILES -
            PERCENTILES=[2,98] -
            ACCEPT
$!       END DISPLAY
$        PICSEL -
            LABEL=A3 -
            ACCEPT
$!       END PICSEL
$        DISPLAY -
            IN=REDUCED_DATA3 -
            MODE=PERCENTILES -
            PERCENTILES=[2,98] -
            ACCEPT
$!       END DISPLAY
$        PICSEL -
            LABEL=A4 -
            ACCEPT
$!       END PICSEL
$        DISPLAY -
            IN=REDUCED_DATA4 -
            MODE=PERCENTILES -
            PERCENTILES=[2,98] -
            ACCEPT
$!       END DISPLAY
$     ENDIF
$!
$!  Now proceed to test out some alignment functionality.
$     ECHO " "
$     ECHO "  CCDPACK will now attempt to realign the target frames to"
$     ECHO "  produce a complete mosaic of whole of the target region."
$     ECHO " "
$!
$!  Locate all the objects on the frames
$     ECHO " " 
$     ECHO "  The first stage of the automated registration process is"
$     ECHO "  to detect the positions of objects (stars and galaxies)."
$     ECHO "  "
$     FINDOBJ -
         IN=REDUCED_DATA% -
         OUTLIST=*.FIND -
         PERCENTILE=95 -
         ACCEPT
$!    END FINDOBJ
$     IF ( DEVICE .NES. "NONE" )
$     THEN
$!
$!  Display the objects located.
$        ECHO " "
$        ECHO "  Displaying the positions of the object which have been"
$        ECHO "  detected."
$        ECHO " "
$        PICSEL -
            LABEL=A1 -
            ACCEPT
$!       END PICSEL
$        PLOTLIST -
            INLIST=REDUCED_DATA1 -
            PALNUM=4 -
            MTYPE=23 -
            ACCEPT
$!       END PLOTLIST
$        PICSEL -
            LABEL=A2 -
            ACCEPT
$!       END PICSEL
$        PLOTLIST -
            INLIST=REDUCED_DATA2 -
            ACCEPT
$!       END PLOTLIST
$        PICSEL -
            LABEL=A3 -
            ACCEPT
$!       END PICSEL
$        PLOTLIST -
            INLIST=REDUCED_DATA3 -
            ACCEPT
$!       END PLOTLIST
$        PICSEL -
            LABEL=A4 -
            ACCEPT
$!       END PICSEL
$        PLOTLIST -
            INLIST=REDUCED_DATA4 -
            ACCEPT
$!       END PLOTLIST
$     ENDIF
$
$!  Determine matches between the positions.
$     ECHO " "
$     ECHO "  After locating the objects it is now necessary to determine"
$     ECHO "  which objects correspond."
$     ECHO " "
$     FINDOFF -
         INLIST=REDUCED_DATA% -
         NDFNAMES=TRUE -
         OUTLIST=*.OFF -
         ACCEPT
$!    END FINDOFF
$     IF ( DEVICE .NES "NONE" )
$     THEN
$!
$!  Display the objects located.
$        ECHO " "
$        ECHO "  Displaying the labels of objects which have been matched."
$        ECHO " "
$        PICSEL -
            LABEL=A1 -
            ACCEPT
$!       END PICSEL
$        PLOTLIST -
            INLIST=REDUCED_DATA1 -
            PALNUM=3 -
            MTYPE=-1 -
            THICK=2 -
            MSIZE=1.5 -
            ACCEPT
$!       END PLOTLIST
$        PICSEL -
            LABEL=A2 -
            ACCEPT
$!       END PICSEL
$        PLOTLIST -
            INLIST=REDUCED_DATA2 -
            ACCEPT
$!       END PLOTLIST
$        PICSEL -
            LABEL=A3 -
            ACCEPT
$!       END PICSEL
$        PLOTLIST -
            INLIST=REDUCED_DATA3 -
            ACCEPT
$!       END PLOTLIST
$        PICSEL -
            LABEL=A4 -
            ACCEPT
$!       END PICSEL
$        PLOTLIST -
            INLIST=REDUCED_DATA4 -
            ACCEPT
$!       END PLOTLIST
$     ENDIF
$!
$!  Set the registration structures.
$     ECHO " "
$     ECHO "  Now that the object-object correspondence is known it is"
$     ECHO "  possible to work out the inter-NDF transformations."
$     ECHO "  The next routine does this for a range of different"
$     ECHO "  transformation types. It also writes the information into the"
$     ECHO "  NDFs so that other routines may use it."
$     ECHO " "
$     REGISTER -
         INLIST=REDUCED_DATA% -
         FITTYPE=1 -
         ACCEPT
$!    END REGISTER
$!
$!  Resample the data.
$     ECHO " "
$     ECHO "  The reduced NDFs will now be resampled to the same coordinate"
$     ECHO "  system. After this is performed they can then be combined"
$     ECHO "  (after determining normalising scale and zero points which "
$     ECHO "  take into account any variations in sky transparency and "
$     ECHO "  exposure time) into a single frame which shows the complete"
$     ECHO "  data coverage for the target area."
$     ECHO " "
$     TRANNDF -
         IN=REDUCED_DATA% -
         OUT=*|REDUCED|RESAMP| -
         ACCEPT
$!    END TRANNDF
$!
$!  Normalise it.
$     ECHO " "
$     ECHO "  Normalising and combining the aligned datasets."
$     ECHO " "
$     MAKEMOS -
         IN=RESAMP_DATA% -
         SCALE=TRUE -
         ZERO=TRUE -
         OUT=mosaic -
         ACCEPT
$!    END MAKEMOS
$!
$!  Display the final mosaic.
$     IF ( DEVICE .NES "NONE" )
$     THEN 
$        ECHO " "
$        ECHO "  Displaying the final mosaic."
$        ECHO " "
$        GDCLEAR ACCEPT
$        DISPLAY -
            IN=mosaic -
            MODE=PERCENTILES -
            PERCENTILES=[2,98] -
            ACCEPT
$!       END DISPLAY
$     ENDIF
$!
$!  Exercise is completed.
$     CCDNOTE -
         "Exercise completed" -
          ACCEPT
$!    END CCDNOTE
$!
$!  Exit label
$EXIT:
$     DEFINE/NOLOG ADAM_USER 'F$TRNLNM( "OLD_USER" )
$     EXIT
$! $Id$
