#!/bin/tcsh 

#  Check a list of observations has been supplied on the command line.
if( $# < 1 ) then
   echo "Usage: distortion <inlist>"
   echo
   echo "   <inlist> is a text file in which each line contains the path"
   echo "   to a directory holding the raw time series cubes for a single"
   echo "   observation."
   exit
endif

#  Ensure SMURF tasks assume there is no focal plane distortion.
setenv SMURF_DISTORTION NONE

#  Ensure ADAM tasks fail rather than prompt for parameter values
setenv ADAM_NOPROMPT 1
   
#  Expand the supplied list of directories into absolute paths and check
#  they exist.
set dir_list = " "
foreach dir (`cat $1`)
   set indir = `readlink -f $dir`
   if( ! -d "$indir" ) then
      echo " "
      echo ">>>>  Cannot find directory $indir \!\!\!\!\!"
      echo " "
      exit
   endif
   set dir_list = "$dir_list $indir"
end

#  Create a temporary directory to hold all the intermediate files, and
#  move into it.
mkdir distortion-tmp
cd distortion-tmp

#  Create a temporary ADAM directory within the above directory.
mkdir adam
setenv ADAM_USER $PWD/adam 
   
#  For each observation, use the iterative mapmaker to create a set of
#  maps for each individual bolometer. An accurate position for the source 
#  in each bolomap is found using kappa:beamfit. This is compared to the 
#  position implied by the telescope base position (assuming no distortion).
#  A catalogue holding these positions is created for each observation. The 
#  focal plane X and Y offsets are found for each bolometer and stored in a 
#  catalogue (total.txt), in TOPCAT "ascii" format. This catalogue is then 
#  used to create a pair of 2D NDFs, one holding the focal Plane X offset 
#  for each bolometer, and the other holding the Y offsets (both in units 
#  of mm). These are called outdx.sdf and outdy.sdf. Each observation is 
#  processed in a separate directory.
set idir = 0
foreach indir ($dir_list)

#  Create a new directory in which to process this observation, and move
#  into it.
   @ idir = $idir + 1
   set pdir = "obs$idir"
   mkdir $pdir
   cd $pdir

#  Save the name of the output catalogue for this observation.
   set cat = "$PWD/total.txt"
   
#  Make a temporary sub-directory, and move into it
   set t = "$PWD/distortion-tmp"
   mkdir $t 
   cd $t

#  Ensure NDG does not try to fork processes to expand shell meta-characters 
#  for output NDFs ("fork" can fail when creating the bolomaps due to lack 
#  of memory on big data sets).
   setenv NDG_NOSHELL 1
   
#  Tell the user what's happening
   echo "-----------------------------------------------"
   echo "Processing directory $indir ....."
   echo " "
   
#  Use the iterative mapmaker to create map for each individual bolometers.
   $SMURF_DIR/makemap "$indir/*" method=iter out=junk system=AZEL \
             alignsys=t config="'^/star/share/smurf/dimmconfig.lis,bolomap=1'" | tee makemap.log
   
#  See if the bolomaps have a chunk number in the NDF name or not. If so, get 
#  the highest chunk number (only check up to 5 chunks).
   grep "Writing single bolo map" makemap.log | grep CH05 > /dev/null
   if( $status == 0 ) then
      set top_chunk = 5
   else
      grep "Writing single bolo map" makemap.log | grep CH04 > /dev/null
      if( $status == 0 ) then
         set top_chunk = 4
      else
         grep "Writing single bolo map" makemap.log | grep CH03 > /dev/null
         if( $status == 0 ) then
            set top_chunk = 3
         else
            grep "Writing single bolo map" makemap.log | grep CH02 > /dev/null
            if( $status == 0 ) then
               set top_chunk = 2
            else
               grep "Writing single bolo map" makemap.log | grep CH01 > /dev/null
               if( $status == 0 ) then
                  set top_chunk = 1
               else
                  set top_chunk = 0
               endif
            endif
         endif
      endif
   endif
   
#  Get the date.
   set date = `$KAPPA_DIR/fitsmod junk edit=print keyword=UTDATE`
   
#  Get the middle elevation
   set el0 = `$KAPPA_DIR/fitsmod junk edit=print keyword=ELSTART`
   set el1 = `$KAPPA_DIR/fitsmod junk edit=print keyword=ELEND`
   set elev = `$KAPPA_DIR/calc exp="0.5*(pa+pb)" pa=$el0 pb=$el1`
   
#  Get the observation number.
   set obsnum = `$KAPPA_DIR/fitsmod junk edit=print keyword=OBSNUM`
   
#  Get the subarray name, and its corresponding integer index.
   set subarray = `$KAPPA_DIR/fitsmod junk edit=print keyword=SUBARRAY`
   if( $subarray == "s8a" ) then
      set array = 0
   else if( $subarray == "s8b" ) then
      set array = 1
   else if( $subarray == "s8c" ) then
      set array = 2
   else if( $subarray == "s8d" ) then
      set array = 3
   else if( $subarray == "s4a" ) then
      set array = 4
   else if( $subarray == "s4b" ) then
      set array = 5
   else if( $subarray == "s4c" ) then
      set array = 6
   else if( $subarray == "s4d" ) then
      set array = 7
   else 
      echo "Unknown subarray - $subarray"
      exit
   endif
   
#  Need to identify a non dark subscan. Loop round all subscans in the 
#  current directory.
   set nondark = " "
   foreach n ($indir/s*_*_*.sdf)
   
#  Check the file has a SHUTTER keyword in its FITS extension
      set there = `$KAPPA_DIR/fitsmod $n edit=exist keyword=SHUTTER`
      if( "$there" == "TRUE" ) then      
   
#  If the value of SHUTTER is zero, the scan is a dark. If not, store the name 
#  of the scan for future use.
         set val = `$KAPPA_DIR/fitsmod $n edit=print keyword=SHUTTER`
         if( "$val" != "0" ) then
            set nondark = "$n"
         endif
      endif
   end
   
   if( "$nondark" == " " ) then
      echo "All scans are darks"
      exit
   endif
   
#  Get a Mapping from bolometer array pixel offsets from the reference point, 
#  to (Az,El) offsets from the reference point, for a typical time slice, 
#  in the non-dark scan found above, and invert it so it goes the other way. 
#  Using this bolomaps approach, we need to assume that all the time slices 
#  are rotated in the same way on the sky. The timeslices approach does not 
#  suffer from this assumption.
   $SMURF_DIR/dsutils in=$nondark bmap=bmap.ast
   $ATOOLS_DIR/astinvert bmap.ast bmap.ast
   
#  Write the header for the output catalogue. 
   echo "# SCUBA-2 focal plane distortion data" > $cat
   echo "#" >> $cat
   echo "# ICH - zero-based makemap chunk index" >> $cat
   echo "# BC1 - array pixel X coord at centre of source (produced by beamfit)" >> $cat
   echo "# BC2 - array pixel Y coord at centre of source (produced by beamfit)" >> $cat
   echo "# BF1 - array pixel X coord at bolometer centre" >> $cat
   echo "# BF2 - - array pixel Y coord at bolometer centre" >> $cat
   echo "# DBC1 - error in BC1" >> $cat
   echo "# DBC2 - error in BC2" >> $cat
   echo "# AMP - source amplitude (produced by beamfit)" >> $cat
   echo "# DAMP - error in AMP" >> $cat
   echo "# RMS - RMS error of beamfit solution" >> $cat
   echo "# OBS - observation number" >> $cat
   echo "# DATE - observation date" >> $cat
   echo "# ARRAY - array index (s8a=0, s8b=1, s8c=2, s8d=3, s4a=4, s4b=5, s4c=6, s4d=7)" >> $cat
   echo "# ELEV - telescope elevation (degrees)" >> $cat
   echo "#" >> $cat
   echo "# ICH BC1 BC2 BF1 BF2 DBC1 DBC2 AMP DAMP RMS OBS DATE ARRAY ELEV" >> $cat
   
#  Loop round all GRID X columns (1 to 32)
   set ix = 0
   while( $ix < 32 ) 
      @ ix = $ix + 1
   
#  In the catalogue, the "true" source position (columns BF1 and BF2) is 
#  given by the pixel coords of the centre of the bolometer used to make 
#  the bolomap. Store the X pixel coord for the centre of bolometers in the 
#  current column. This assumes the lower pixel index bound is zero on the
#  first axis.
      set bf1 = `$KAPPA_DIR/calc exp="pa-1.5" pa=$ix`
   
#  Loop round all GRID Y rows (1 to 40).
      set iy = 0
      while( $iy < 40 ) 
         @ iy = $iy + 1
   
#  In the catalogue, the "true" source position (columns BF1 and BF2) is 
#  given by the pixel coords of the centre of the bolometer used to make 
#  the bolomap. Store the Y pixel coord for the centre of bolometers in the 
#  current row. This assumes the lower pixel index bound is zero on the second
#  axis.
         set bf2 = `$KAPPA_DIR/calc exp="pa-1.5" pa=$iy`
   
#  Loop round all chunks (if any)
         set ich = 0
         while( $ich <= $top_chunk ) 
   
#  Construct the name of the bolometer map.
            if( $top_chunk > 0 ) then
               set bolo_name = "CH0${ich}"
            else
               set bolo_name = ""
            endif
   
            if( $ix < 10 ) then
               set bolo_name = "${bolo_name}C0${ix}"
            else
               set bolo_name = "${bolo_name}C${ix}"
            endif
   
            if( $iy < 10 ) then
               set bolo_name = "${bolo_name}R0${iy}"
            else
               set bolo_name = "${bolo_name}R${iy}"
            endif
      
#  Skip if the bolomap was not created by makemap.
            grep -i $bolo_name makemap.log > /dev/null
            if( $status == 1 ) then
               echo "Skipping bolomap $bolo_name"
            else
               echo "Doing bolomap $bolo_name"
      
#  Get the full path to the bolomap NDF.
               set ndf = "junk.more.smurf.bolomaps.$bolo_name"
      
#  From here on, use pixel coords to refer to positions within the bolomap,
#  increasing the format precision from the one decimal place provided by 
#  the NDF library. NDF does not allow us to change the Format attributes for 
#  the PIXEL Frame directly, so take a copy of the PIXEL Frame first, and set 
#  the Format attributes of the copy.
               $KAPPA_DIR/wcsadd $ndf frame=pixel domain=newpix maptype=unit accept 
               $KAPPA_DIR/wcsattrib $ndf set 'format(1)' "%5.3f"
               $KAPPA_DIR/wcsattrib $ndf set 'format(2)' "%5.3f"
      
#  Find the pixel coords at the centre of the feature. The feature is 
#  nominally at the reference point and so should, in the absence of errors 
#  and incorrect polymaps, have pixel coords (0.5,0.5).
               $KAPPA_DIR/beamfit $ndf mode=inter pos=\"0.5 0.5\" | grep \!\! >& /dev/null
      
#  Check a peak was fitted succesfully.
               if( $status == 1 ) then
      
# Get the parameters of the fit.
                  set amp = `$KAPPA_DIR/parget amp beamfit`
                  set posxy = `$KAPPA_DIR/parget centre beamfit`
                  set rms = `$KAPPA_DIR/parget rms beamfit`
      
#  Get the Mapping from bolomap pixel coords to sky offsets from the 
#  reference point, and combine with the mapping from sky offsets to focal 
#  plane pixel coordinate offsets.
                  $ATOOLS_DIR/astgetmapping $ndf PIXEL SKY smap.ast
                  $ATOOLS_DIR/astcmpmap smap.ast bmap.ast yes \! sbmap.ast
      
#  Use this CmpMap to transform the beam centre from bolomap pixel coords
#  to offsets from the nominal reference point in pixel coords within the 
#  bolometer array.
                  set dfp = `$ATOOLS_DIR/asttran2 sbmap.ast $posxy[1] $posxy[2] y`
   
#  Check for AST__BAD values from the above astTran2 call.    
                  echo $dfp | grep -i "e+308" > /dev/null
                  if( $status == 0 ) then
                     echo "  astTran failure"
   
#  In the catalogue, the "base" source position (columns BC1 and BC2) 
#  corresponds to the bolometer array pixel coords of the source after 
#  distortion (columns BF1 and BF2 give the bolometer array pixel coords 
#  of the source before distortion). In the absence of any distortion, the 
#  beamfit centre found above would be at bolometer array pixel offsets (0,0).
#  Any discrepancy is caused by distortion (and noise). So modify the known 
#  bolometer position ($bf1,$bf2) by subtracting off the discrepancies in 
#  the beamfit position to get the distorted source position.
                  else
                     set bc1 = `$KAPPA_DIR/calc exp="pa-pb" pa=$bf1 pb=$dfp[1]`
                     set bc2 = `$KAPPA_DIR/calc exp="pa-pb" pa=$bf2 pb=$dfp[2]`
      
#  Display the parameters of the fit, and copy them into the output text
#  file. 
                     echo "$ich $bc1 $bc2 $bf1 $bf2 $posxy[3] $posxy[4] $amp $rms $obsnum $date $array $elev " >> $cat
                  endif   
               else
                  echo "  beamfit failure"
               endif
            endif
   
#  Increment the chunk index
            @ ich = $ich + 1
         end
      end
   end 
   
#  Move into the parent directory
   cd ..

#  Bin the offsets in the "total.txt" catalogue to create two images, one 
#  containing FplaneX offsets and the other containing FplaneY offsets, 
#  both in units of mm. The current WCS Frame in these images is FPLANE 
#  (also in units of mm). The GRID->FPLANE mapping includes no polynomial 
#  distortion.
   $SMURF_DIR/dsutils infitx=\! incat=$cat subarray=$subarray feature=beam \
                  outdx=outdx outdy=outdy outcat=\!

#  Move back to the original directory.
   cd ..

#  Construct a list of all the outdx.sdf files, and another of all the 
#  outdy.sdf files.
   echo "$PWD/$pdir/outdx" >> outdx.lis
   echo "$PWD/$pdir/outdy" >> outdy.lis

#  Construct a list of all the aligned outdx.sdf files, and another of all the 
#  aligned outdy.sdf files. 
   echo "$PWD/$pdir/outdx_A" >> outdx_A.lis
   echo "$PWD/$pdir/outdy_A" >> outdy_A.lis

end

#  Align all the outdx and outdy files. Alignment occurs in focal plane 
#  X and Y coords.
$KAPPA_DIR/wcsalign in=^outdx.lis out=^outdx_A.lis method=sincsinc wlim=0.4 \
                    rebin=no ref=! lbnd=!
$KAPPA_DIR/wcsalign in=^outdy.lis out=^outdy_A.lis method=sincsinc wlim=0.4 \
                    rebin=no ref=! lbnd=!

#  Mosaic them into a single image, adjusting the zero points to make 
#  them agree as far as possible.
$CCDPACK_DIR/makemos in=^outdx_A.lis out=outdx-total zero scale \
                     logto=term skysup=0 method=median optov=$idir
$CCDPACK_DIR/makemos in=^outdy_A.lis out=outdy-total zero scale \
                     logto=term skysup=0 method=median optov=$idir

#  Ensure that the mean residual (after sigma clipping) is zero.
$KAPPA_DIR/stats outdx-total quiet clip=\[3,3,3\]
set mean = `$KAPPA_DIR/parget mean stats`
$KAPPA_DIR/csub outdx-total $mean a.sdf
mv a.sdf outdx-total.sdf

$KAPPA_DIR/stats outdy-total quiet clip=\[3,3,3\]
set mean = `$KAPPA_DIR/parget mean stats`
$KAPPA_DIR/csub outdy-total $mean a.sdf
mv a.sdf outdy-total.sdf

#  Save the names of the total focal plane offset images
set outdx = outdx-total
set outdy = outdy-total

#  Ensure focal plane Y is upwards so that we can use setaxis to transfer WCS 
#  FPLANE coords into the NDF AXIS structures (as required by FITSURFACE)
$KAPPA_DIR/rotate $outdx out=outdxr angle=!

#  Set up AXIS structures holding the FPLANE X and Y positions in mm, and 
#  make it the current Frame
$KAPPA_DIR/setaxis outdxr 1 wcs
$KAPPA_DIR/setaxis outdxr 2 wcs
$KAPPA_DIR/wcsframe outdxr axis

#  Fit a cubic surface to the values in the X error image, doing 
#  sigma-clipping to get rid of aberrant points. 
$KAPPA_DIR/surfit outdxr out=surfitx fittype=poly order=3 bindim=4 \
               estimator=mode fitclip=\[3,3\] evaluate=a

#  Surfit does not report the coefficients of the polynomial fit, so we now 
#  use fitsurface to fit a surface to the cubic surface created by surfit.
#  Hopefully this fit will be exact since the surface is accurately cubic 
#  by definition. The resulting polynomial coefficients are put into a 
#  SURFACEFIT extension in the NDF. Select "cosys=data" so that the 
#  polynomial transforms (Fx,Fy) in mm, into delta_Fx, also in mm.
$KAPPA_DIR/fitsurface surfitx variance=no fittype=poly nxpar=4 \
               nypar=4 overwrite cosys=data

#  Now do exactly the same with the Y correction image...
$KAPPA_DIR/rotate $outdy out=outdyr angle=!
$KAPPA_DIR/setaxis outdyr 1 wcs
$KAPPA_DIR/setaxis outdyr 2 wcs
$KAPPA_DIR/wcsframe outdyr axis
$KAPPA_DIR/surfit outdyr out=surfity fittype=poly order=3 bindim=4 \
               estimator=mode fitclip=\[3,3\] evaluate=a
$KAPPA_DIR/fitsurface surfity variance=no fittype=poly nxpar=4 \
               nypar=4 overwrite cosys=data


#  Produce the description of the forward transformation. C source code 
#  defining the coefficients of the forward transformation of the PolyMap 
#  are put in fwd.c
$SMURF_DIR/dsutils subarray=s8d infitx=surfitx infity=surfity outcode=fwd.c \
                            outdx=invdx outdy=invdy forward=yes


#  Now generate the inverse transformation. Allow the inverse polynomial to 
#  be one degree higher than the forward polynomial, in order to achieve an 
#  accurate inverse.
$KAPPA_DIR/rotate invdx out=invdxr angle=!
$KAPPA_DIR/setaxis invdxr 1 wcs
$KAPPA_DIR/setaxis invdxr 2 wcs
$KAPPA_DIR/wcsframe invdxr axis
$KAPPA_DIR/fitsurface invdxr variance=no fittype=poly nxpar=6 \
               nypar=6 overwrite cosys=data

$KAPPA_DIR/rotate invdy out=invdyr angle=!
$KAPPA_DIR/setaxis invdyr 1 wcs
$KAPPA_DIR/setaxis invdyr 2 wcs
$KAPPA_DIR/wcsframe invdyr axis
$KAPPA_DIR/fitsurface invdyr variance=no fittype=poly nxpar=6 \
               nypar=6 overwrite cosys=data

$SMURF_DIR/dsutils subarray=s8d infitx=invdxr infity=invdyr outcode=inv.c \
                            outdx=! outdy=! forward=no

#  Conbine both transformations into one file
cat fwd.c inv.c > code.c

#  Now find the systematic offset unique to each individual directory.
foreach n (`cat outdx_A.lis`)

#  Find the mean residual between the X corrections for the current directory 
#  and the overall X corrections.
   $KAPPA_DIR/sub $n outdx-total diff
   $KAPPA_DIR/stats diff quiet clip=\[3,3,3\]
   set mean = `$KAPPA_DIR/parget mean stats`

#  Get the subarray name, and store the result in a file for the subarray
   set words = `$KAPPA_DIR/setext $n DSUTILS get SUBARRAY loop=no`
   set subarray = $words[3]
   echo $mean >> $subarray.fpcx

end

#  Do the same for the Y offsets.
foreach n (`cat outdy_A.lis`)
   $KAPPA_DIR/sub $n outdy-total diff
   $KAPPA_DIR/stats diff quiet clip=\[3,3,3\]
   set mean = `$KAPPA_DIR/parget mean stats`
   set words = `$KAPPA_DIR/setext $n DSUTILS get SUBARRAY loop=no`
   set subarray = $words[3]
   echo $mean >> $subarray.fpcy
end

echo "" >> code.c

#  Find the median Y offset for each subarray, converting from mm to pixels.
foreach n (s*.fpcx)
   set w = `wc $n`
   $KAPPA_DIR/trandat freename=$n auto shape=$w[1] ndf=tmp
   $KAPPA_DIR/stats tmp order quiet
   set median = `$KAPPA_DIR/parget median stats`
   set median = `$KAPPA_DIR/calc exp='pa/1.135' pa=$median`
   set subarray = `basename $n .fpcx`
   echo "Add $median pixels to the focal plane X centre of subarray $subarray" | tee -a code.c
end

#  Find the median Y offset for each subarray, converting from mm to pixels.
foreach n (s*.fpcy)
   set w = `wc $n`
   $KAPPA_DIR/trandat freename=$n auto shape=$w[1] ndf=tmp
   $KAPPA_DIR/stats tmp order quiet
   set median = `$KAPPA_DIR/parget median stats`
   set median = `$KAPPA_DIR/calc exp='pa/1.135' pa=$median`
   set subarray = `basename $n .fpcy`
   echo "Add $median pixels to the focal plane Y centre of subarray $subarray" | tee -a code.c
end

#  Move the final result ("code.c") into the parent directory

echo
if( -e code.c ) then
   mv code.c ..
   echo "Finished - results are in file code.c"
else
   echo "Finished - something went wrong so there are no results :-("
endif
echo




