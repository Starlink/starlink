hidden proc ccdexercise
{+
{  Name:
{     CCDEXERCISE
{
{  Purpose:
{     Tests CCDPACK functionality.
{
{  Language:
{     ICL
{
{  Description:
{     This procedure creates a series of test frames using CCDGENERATE. It
{     then executes various of the CCDPACK Atasks simulating a
{     reduction sequence. Intermediary results are displayed (if requested)
{
{  Authors:
{     PDRAPER: Peter Draper (STARLINK)
{     {enter_new_authors_here}
{
{  History:
{     4-APR-1992 (PDRAPER):
{        Original version.
{     29-JUN-1993 (PDRAPER):
{     	  Changed to include automated alignment.
{     5-JUL-1993 (PDRAPER):
{        DCL version
{     11-SEP-1995 (PDRAPER):
{        UNIX version
{     {enter_further_changes_here}
{-
      defstring echo print
      CCDdir=getenv("CCDPACK_DIR")

{
{  Try to locate the object specification file. If this does not exists
{  then exit.
     object_file =  (CCDdir) & "/ccdtest_obj.dat"
     if not file_exists(object_file)
        echo "Cannot locate file " (object_file)
        echo "- test terminated."
        signal escape
     endif
{
{  Does the user want to see image display activity?
     input "DISPLAY - Display device (! for none) > " (device)
     if device = "!!"
        goto exit
     else
        if device = "!"
           device = "none"
        else
           device = '@' & (device)
        endif
     endif
{
{  If we're going to use a device we need KAPPA.
     if device <> "none"
        kappa
        idset (device) accept
        gdset (device) accept
        gdclear accept
        paldef accept
        palentry colour=white palnum=0 accept
     endif
{
{  Initialise CCDPACK.
      ccdpack
{
{  Clear any existing global parameters.
     ccdclear accept
{
{  Try to locate the ARD file which goes with the test. If this is
{  not found then proceed without it.
     echo " "
     echo "  Setting the characteristics of the CCD device using the "
     echo "  ccdsetup routine."
     echo " "
     if file_exists( (CCDdir) & "/ccdtest_ard.dat" )
{
{  Found the ARD file setup CCDPACK appropriately.
        mask = (CCDdir) & '/ccdtest_ard.dat'
        ccdsetup ~
            bounds=[1,5,120,128] ~
            extent=[6,119,1,128] ~
            adc=1 ~
            rnoise=9.95 ~
            logto=both ~
            logfile=ccdtest.log ~
            preserve=true ~
            direction=x ~
            mask=(mask) ~
            reset accept
{       end ccdsetup
     else
        echo "Cannot locate ARD file; mask not applied"

{  Do the "device" setup without an ARD file.
        ccdsetup ~
            bounds=[1,4,120,128] ~
            extent=[6,119,1,128] ~
            adc=1 ~
            rnoise=10.0 ~
            logto=both ~
            logfile=ccdtest.log ~
            preserve=true ~
            direction=x ~
            reset accept
{       end ccdsetup
      endif
{
{  Create the test frames
     echo " "
     echo "  Creating the test data. "
     echo " "
     echo "  The test data consists of :-"
     echo "     4 bias frames    "
     echo "     4 flatfields     "
     echo "     4 object fields. "
     echo " "
     echo "  The target data is a simulated galaxy cluster in which the"
     echo "  telescope position has been moved between exposures."
     echo " "
     ccdgenerate ~
         nseq=4 ~
         file=(object_file) ~
         ubnds=[128,128,166,128,128,201,166,201] ~
         lbnds=[1,1,39,1,1,74,39,74] ~
         accept
{    end ccdgenerate
{
{  If display capability is enabled then display the data frames.
     if device <> "none"
        echo " "
        echo "  Displaying raw target frames."
        echo " "
        picdef ~
            mode=array ~
            xpic=2 ~
            ypic=2 ~
            prefix=a ~
            accept
{       end picdef
        lutheat
{       end lutheat
        picsel ~
            label=a1 ~
            accept
{       end picsel
        display ~
            in=data1 ~
            mode=percentiles ~
            percentiles=[2,98] ~
            accept
{       end display
        picsel ~
            label=a2 ~
            accept
{       end picsel
        display ~
            in=data2 ~
            mode=percentiles ~
            percentiles=[2,98] ~
            accept
{       end display
        picsel ~
            label=a3 ~
            accept
{       end picsel
        display ~
            in=data3 ~
            mode=percentiles ~
            percentiles=[2,98] ~
            accept
{       end display
        picsel ~
            label=a4 ~
            accept
{       end picsel
        display ~
            in=data4 ~
            mode=percentiles ~
            percentiles=[2,98] ~
            accept
{       end display
     endif
{
{  Add note to logfile.
     echo " "
     echo "  Adding a note to the logfile. "
     echo " "
     ccdnote ~
         "Exercising CCDPACK" ~
         accept
{    end ccdnote
{
{  Make a bias frame.
     echo " "
     echo "  Producing a master bias calibration frame. This is produced"
     echo "  by median stacking the ordinary bias frames. This process"
     echo "  reduces the noise introduced in debiassing."
     echo " "
     makebias ~
         in=bias* ~
         out=master_bias ~
         accept
{    end makebias
{
{  DEbias all frames (including flatfields)
     debias ~
         in="data?,ff?" ~
         out=debias_* ~
         accept
{    end debias
{
{  Display all the debiassed frames.
     if device <> "none"
        echo " "
        echo "  Displaying debiassed target frames. Note the absence of the"
        echo "  bias strips (which were along the Y edges) and the removal"
        echo "  of defective regions. The flatfields have also been debiassed"
        echo "  but are not displayed."
        echo " "
        gdclear accept
        picdef ~
            mode=array ~
            xpic=2 ~
            ypic=2 ~
            prefix=a ~
            accept
{       end picdef
        picsel ~
            label=a1 ~
            accept
{       end picsel
        display ~
            in=debias_data1 ~
            mode=percentiles ~
            percentiles=[2,98] ~
            accept
{       end display
        picsel ~
            label=a2 ~
            accept
{       end picsel
        display ~
            in=debias_data2 ~
            mode=percentiles ~
            percentiles=[2,98] ~
            accept
{       end display
        picsel ~
            label=a3 ~
            accept
{       end picsel
        display ~
            in=debias_data3 ~
            mode=percentiles ~
            percentiles=[2,98] ~
            accept
{       end display
        picsel ~
            label=a4 ~
            accept
{       end picsel
        display ~
            in=debias_data4 ~
            mode=percentiles ~
            percentiles=[2,98] ~
            accept
{       end display
     endif
{
{  Create a flat field master
     echo " "
     echo "   Producing a master flatfield. This frame will be used to correct"
     echo "   for the sensitivity variations in the detector response and"
     echo "   any variations in the optical throughput (vignetting)."
     echo "   As in the creation of the master bias frame median stacking is"
     echo "   used to combine a series of flatfields."
     echo " "
     makeflat ~
         in=debias_ff? ~
         out=master_flat ~
         accept
{    end makeflat
{  Display the master flatfield.
     if device <> "none"
        echo " "
        echo "  Displaying master flatfield (the flatfield used in this case"
        echo "  is a ramp, normal flatfields are not like this). "
        echo " "
        gdclear accept
        display ~
            in=master_flat ~
            mode=percentiles ~
            percentiles=[2,98] ~
            accept
{       end display
     endif
{
{  Flatfield all the data frames
     echo " "
     echo "  Flatfielding all the target frames."
     echo " "
     flatcor ~
         in=debias_data? ~
         out=*|debias|reduced| ~
         accept
{    end flatcor
     if device <> "none"
        echo " "
        echo "  Displaying flatfielded target frames."
        echo " "
        gdclear accept
        picdef ~
            mode=array ~
            xpic=2 ~
            ypic=2 ~
            prefix=a ~
            accept
{       end picdef
        lutheat
{       end lutheat
        picsel ~
            label=a1 ~
            accept
{       end picsel
        display ~
            in=reduced_data1 ~
            mode=percentiles ~
            percentiles=[2,98] ~
            accept
{       end display
        picsel ~
            label=a2 ~
            accept
{       end picsel
        display ~
            in=reduced_data2 ~
            mode=percentiles ~
            percentiles=[2,98] ~
            accept
{       end display
        picsel ~
            label=a3 ~
            accept
{       end picsel
        display ~
            in=reduced_data3 ~
            mode=percentiles ~
            percentiles=[2,98] ~
            accept
{       end display
        picsel ~
            label=a4 ~
            accept
{       end picsel
        display ~
            in=reduced_data4 ~
            mode=percentiles ~
            percentiles=[2,98] ~
            accept
{       end display
     endif
{
{  Now proceed to test out some alignment functionality.
     echo " "
     echo "  CCDPACK will now attempt to realign the target frames to"
     echo "  produce a complete mosaic of whole of the target region."
     echo " "
{
{  Locate all the objects on the frames
     echo " "
     echo "  The first stage of the automated registration process is"
     echo "  to detect the positions of objects (stars and galaxies)."
     echo "  "
     findobj ~
         in=reduced_data? ~
         outlist=*.find ~
         percentile=95 ~
         accept
{    end findobj
     if device <> "none"
{
{  Display the objects located.
        echo " "
        echo "  Displaying the positions of the object which have been"
        echo "  detected."
        echo " "
        picsel ~
            label=a1 ~
            accept
{       end picsel
        plotlist ~
            inlist='reduced_data1' ~
            palnum=4 ~
            mtype=23 ~
            accept
{       end plotlist
        picsel ~
            label=a2 ~
            accept
{       end picsel
        plotlist ~
            inlist='reduced_data2' ~
            accept
{       end plotlist
        picsel ~
            label=a3 ~
            accept
{       end picsel
        plotlist ~
            inlist='reduced_data3' ~
            accept
{       end plotlist
        picsel ~
            label=a4 ~
            accept
{       end picsel
        plotlist ~
            inlist='reduced_data4' ~
            accept
{       end plotlist
     endif

{  Determine matches between the positions.
     echo " "
     echo "  After locating the objects it is now necessary to determine"
     echo "  which objects correspond."
     echo " "
     findoff ~
         inlist=reduced_data? ~
         ndfnames=true ~
         outlist=*.off ~
         accept
{    end findoff
     if device <> "none"
{
{  Display the objects located.
        echo " "
        echo "  Displaying the labels of objects which have been matched."
        echo " "
        picsel ~
            label=a1 ~
            accept
{       end picsel
        plotlist ~
            inlist='reduced_data1' ~
            palnum=3 ~
            mtype=-1 ~
            thick=2 ~
            msize=1.5 ~
            accept
{       end plotlist
        picsel ~
            label=a2 ~
            accept
{       end picsel
        plotlist ~
            inlist='reduced_data2' ~
            accept
{       end plotlist
        picsel ~
            label=a3 ~
            accept
{       end picsel
        plotlist ~
            inlist='reduced_data3' ~
            accept
{       end plotlist
        picsel ~
            label=a4 ~
            accept
{       end picsel
        plotlist ~
            inlist='reduced_data4' ~
            accept
{       end plotlist
     endif
{
{  Set the registration structures.
     echo " "
     echo "  Now that the object-object correspondence is known it is"
     echo "  possible to work out the inter-NDF transformations."
     echo "  The next routine does this for a range of different"
     echo "  transformation types. It also writes the information into the"
     echo "  NDFs so that other routines may use it."
     echo " "
     register ~
         inlist=reduced_data? ~
         fittype=1 ~
         accept
{    end register
{
{  Resample the data.
     echo " "
     echo "  The reduced NDFs will now be resampled to the same coordinate"
     echo "  system. After this is performed they can then be combined"
     echo "  (after determining normalising scale and zero points which "
     echo "  take into account any variations in sky transparency and "
     echo "  exposure time) into a single frame which shows the complete"
     echo "  data coverage for the target area."
     echo " "
     tranndf ~
         in=reduced_data? ~
         out=*|reduced|resamp| ~
         accept
{    end tranndf
{
{  Normalise it.
     echo " "
     echo "  Normalising and combining the aligned datasets."
     ECHO " "
     makemos ~
         in=resamp_data? ~
         scale=true ~
         zero=true ~
         out=MOSAIC ~
         accept
{    end makemos
{
{  Display the final mosaic.
     if device <> "none"
        echo " "
        echo "  Displaying the final mosaic."
        echo " "
        gdclear accept
        display ~
            in=MOSAIC ~
            mode=percentiles ~
            percentiles=[2,98] ~
            accept
{       end display
     endif
{
{  Exercise is completed.
     ccdnote ~
         "Exercise completed" ~
          accept
{    end ccdnote
{
{  Exit in a hurry exception
     exception escape
     end exception
end proc
{ ?W?     ?G?     ?R?
