#!/bin/csh
#+
#  Name:
#     peakmap.csh
#
#  Purpose:
#     Builds a map of emission line emission line strength from a 3D IFU NDF
#
#  Type of Module:
#     C shell script.
#
#  Usage:
#     peakmap [-i filename] [-o filename] [-r number] [-f] [-p] [-v] [-z/+z]
#
#  Description:
#     This shell script sits onto of a collection of A-tasks from the KAPPA
#     FIGARO and DATACUBE packages. It reads a 3D IFU NDF datacube as input
#     and presents the user with a white light image of the cube. The user
#     can then select and x,y position using the cursor. The script will
#     extract and display this spectra. The user will then be prompted to
#     specify various fitting parameters, eg peak position, using the cursor.
#     The script will then attempt to fit the emission line. The fit will be
#     displayed and the user consulted to the goodness of fit. If the fit is
#     considered good enough by the user the script will attempt to perform
#     similar fits to all cube spectra, building a 2D NDF image of strength
#     of the line.
#
#  Parameters:
#     -i filename
#       The script will use this as its input file, the specified file should
#       be a 3D NDF, by default the script will prompt for the input file.
#     -o filename
#       The filename for the output NDF of the line strengh map.
#     -r number
#       Rest wavelength of the line being fitted
#     -f
#       Force the script to accept the first attempt to fit a gaussian to
#       the line. This is a dangerous option, if the fit is poor, or
#       unobtainable the script may terminate abruptly if it is forced to
#       accept the fit.
#     -p
#       The script will plot the final image map to the current display 
#       as well as saving it to an NDF file. Additionally it will over-
#       plot the white light image as a contour map for comparison.
#     -v
#       The script will generate a variance array from the line fits and
#       attach it to the velocity map NDF.
#     -z 
#       The script will automatically prompt the user to select a region to
#       zoom before prompting for the region of interest.
#     +z 
#       The program will not prompt for a zoom before requesting the region
#       of interest.

#
#  Authors:
#     AALLAN: Alasdair Allan (Starlink, Keele University)
#     {enter_new_authors_here}
#
#  History:
#     04-SEP-2000 (AALLAN):
#       Original version.
#     06-SEP-2000 (AALLAN):
#       Heavy modifications.
#     13-SEP-2000 (AALLAN):
#       Rewritten on the train.
#     18-SEP-2000 (AALLAN):
#       Major rewrite to make full use of parameter system.
#     19-SEP-2000 (AALLAN):
#       Moved from using bc to using KAPPA calc for some calculations.
#     31-OCT-2000 (AALLAN):
#       Fixed some bugs.
#     05-NOV-2000 (AALLAN):
#       Added variance arrays.
#     09-NOV-2000 (AALLAN):
#       Changed input method of inputing the continuum measurement.
#     10-NOV-2000 (AALLAN):
#       Added bad fit check, modified plot colour for contour lines.
#     12-NOV-2000 (AALLAN):
#       Modified to work under Solaris 5.8, problems with bc and csh.
#     20-NOV-2000 (AALLAN):
#       Incorporated changes made to source at ADASS.
#     23-NOV-2000 (AALLAN):
#       Added lots of command line options.
#       Added interrupt handling.
#     31-DEC-2000 (AALLAN):
#       Allowed 1 character responses to yes/no prompts
#     03-JAN-2001 (AALLAN)
#       Added manual refitting of data points
#     08-JAN-2001 (AALLAN):
#       Variance calculation made more robust
#       Fixed bug in magic value propogation
#       Major bug in manual refitting routine fixed
#     18-OCT-2001 (AALLAN):
#       Modified temporary files to use ${tmpdir}/${user}
#     {enter_changes_here}
#
#  Copyright:
#     Copyright (C) 2000 Central Laboratory of the Research Councils

#-

# on interrupt
onintr cleanup

# get the user name

set user = `whoami`
set tmpdir = "/tmp"

# clean up from previous runs

rm -f ${tmpdir}/${user}/pmap* >& /dev/null
rm -f ${tmpdir}/${user}/?_?.sdf >& /dev/null

# do variable initialisation

mkdir ${tmpdir}/${user} >& /dev/null
set curfile = "${tmpdir}/${user}/pmap_cursor.tmp"
set fitfile = "${tmpdir}/${user}/pmap_fitgauss.tmp"
set colfile = "${tmpdir}/${user}/pmap_col.sdf"
set ripfile = "${tmpdir}/${user}/pmap_rip.sdf"
set mapfile = "${tmpdir}/${user}/pmap_map.dat"
set varfile = "${tmpdir}/${user}/pmap_var.dat"

touch ${curfile}

set plotspec = "FALSE"
set plotdev = "xwin"
set fitgood = "yes"
set args = ($argv[1-])
set dovar = "FALSE"
set gotinfile = "FALSE"
set gotoutfile = "FALSE"
set gotzoom = "ASK"
set gotrest = "FALSE"
set forcefit = "FALSE"

# specdre component used to store the gaussian fit

set component = 1

# the number of contours used to display the white light image

set numcont = 15

# do package setup

alias echo 'echo > /dev/null'
source ${DATACUBE_DIR}/datacube.csh
unalias echo

# handle any command line arguements

while ( $#args > 0 )
   switch ($args[1])
   case -i:    # input 3D IFU NDF
      shift args
      set gotinfile = "TRUE"
      set infile = $args[1]
      shift args
      breaksw
   case -o:    # output peak map
      shift args
      set gotoutfile = "TRUE"
      set outfile = $args[1]
      shift args
      breaksw
   case -p:    # plot output spectra
      set plotspec = "TRUE"
      set plotdev = "${plotdev}"
      shift args
      breaksw
   case -v:    # generate variances
      set dovar = "TRUE"
      shift args
     breaksw 
   case -z:    # zoom
      set gotzoom = "TRUE"
      shift args
      breaksw 
   case +z:    # not zoom
      set gotzoom = "FALSE"
      shift args
      breaksw 
   case -r:    # rest wavelength of line
      shift args
      set gotrest = "TRUE"
      set line_centre = $args[1]
      shift args
      breaksw
   case -f:    # force fit
      set forcefit = "TRUE"
      shift args
      breaksw                             
   endsw  
end

# get input filename

if ( ${gotinfile} == "FALSE" ) then
   echo -n "NDF input file: "
   set infile = $<
endif

echo " "

echo "      Input NDF:"
echo "        File: ${infile}.sdf"

# check that it exists

if ( ! -e ${infile}.sdf ) then
   echo "PEAKMAP_ERR: ${infile}.sdf does not exist."
   rm -f ${curfile} >& /dev/null
   exit  
endif

# find out the cube dimensions

ndftrace ${infile} >& /dev/null
set ndim = `parget ndim ndftrace`
set dims = `parget dims ndftrace`
set lbnd = `parget lbound ndftrace`
set ubnd = `parget ubound ndftrace`

if ( $ndim != 3 ) then
   echo "PEAKMAP_ERR: ${infile}.sdf is not a datacube."
   rm -f ${curfile} >& /dev/null
   exit  
endif

set bnd = "${lbnd[1]}:${ubnd[1]}, ${lbnd[2]}:${ubnd[2]}, ${lbnd[3]}:${ubnd[3]}"
@ pixnum = $dims[1] * $dims[2] * $dims[3]

echo "      Shape:"
echo "        No. of dimensions: ${ndim}"
echo "        Dimension size(s): ${dims[1]} x ${dims[2]} x ${dims[3]}"
echo "        Pixel bounds     : ${bnd}"
echo "        Total pixels     : $pixnum"

# collapse white light image

echo "      Collapsing:"
echo "        White light image: ${dims[1]} x ${dims[2]}"
collapse "in=${infile} out=${colfile:r} axis=3" >& /dev/null 

# display collapsed image

gdclear device=${plotdev}
paldef device=${plotdev}
lutgrey device=${plotdev}
display "${colfile:r} device=${plotdev} mode=SIGMA sigmas=[-3,2]" >&/dev/null 

# grab x,y position

echo " "
echo "  Left click on pixel to be extracted"
   
cursor showpixel=true style="Colour(marker)=2" plot=mark \
       maxpos=1 marker=2 device=${plotdev} frame="PIXEL" >> ${curfile}

# wait for cursor output then get x,y co-ordinates from 
# the temporary file created by KAPPA cursor.

while ( ! -e ${curfile} ) 
   sleep 1
end

# we don't clean up the collapsed white light image here as normal because
# it will be used later to clone both the AXIS and WCS co-ordinates for the
# new velocity map.

# grab position 

set pos=`parget lastpos cursor | awk '{split($0,a," ");print a[1], a[2]}'`

# get pixel co-ordinates

set xpix = `echo $pos[1] | awk '{split($0,a,"."); print a[1]}'`
set ypix = `echo $pos[2] | awk '{split($0,a,"."); print a[1]}'`
@ xpix = $xpix + 1
@ ypix = $ypix + 1

# clean up the cursor temporary file

rm -f ${curfile}
touch ${curfile}

# extract the spectra

echo " "
echo "      Extracing:"
echo "        (X,Y) pixel: ${xpix},${ypix}"

# extract spectra from cube

ndfcopy "in=${infile}($xpix,$ypix,) out=${ripfile} trim=true trimwcs=true"

# check to see if the NDF has an AXIS extensions
set axis = `parget axis ndftrace`

if ( ${axis} == "FALSE" ) then
   putaxis "${ripfile} spectral=1" >& /dev/null
   echo "        Axes: Adding AXIS extensions"
endif

# check to see if the NDF has Variance extension
set variance = `parget variance ndftrace`
if ( ${variance} == "FALSE" ) then
   echo "        Variances: Extension present."
else
   echo "        Variances: Extension absent."
endif

# label for repeated fitting of the GAUSSIAN

refit:

# plot the ripped spectra

linplot "${ripfile} device=${plotdev}" style="Colour(curves)=1" >& /dev/null

# zoom if required

if ( ${gotzoom} == "ASK") then
   echo " "
   echo -n "Zoom in (yes/no): "
   set zoomit = $<
else if ( ${gotzoom} == "TRUE") then
   set zoomit = "yes"
else
   set zoomit = "no"
endif

if ( ${zoomit} == "yes" || ${zoomit} == "y" ) then

# get the lower limit

   echo " "
   echo "  Left click on lower zoom boundary"
   
   cursor showpixel=true style="Colour(curves)=3" plot=vline \
          maxpos=1 device=${plotdev} >> ${curfile}
   while ( ! -e ${curfile} ) 
      sleep 1
   end
   set pos = `parget lastpos cursor`
   set low_z = $pos[1]

# clean up the cursor temporary file
   rm -f ${curfile}
   touch ${curfile}
   
# get the upper limit

   echo "  Left click on upper zoom boundary"
   
   cursor showpixel=true style="Colour(curves)=3" plot=vline \
          maxpos=1 device=${plotdev} >> ${curfile}
   while ( ! -e ${curfile} ) 
      sleep 1
   end
   set pos = `parget lastpos cursor`
   set upp_z = $pos[1]

   echo " "
   echo "      Zooming:"
   echo "        Lower Boundary: ${low_z}"
   echo "        Upper Boundary: ${upp_z}"

# clean up the cursor temporary file
   rm -f ${curfile}
   touch ${curfile}

# label for repeated fitting of the GAUSSIAN

rezoom:  
 
#  replot the spectra
linplot ${ripfile} xleft=${low_z} xright=${upp_z} \
        device=${plotdev} >& /dev/null

endif

# grab the information needed by the FITGAUSS routine

# get the lower mask boundary

echo " "
echo "  Left click on the lower limit of the fitting region"
   
cursor showpixel=true style="Colour(curves)=2" plot=vline \
       maxpos=1 device=${plotdev} >> ${curfile}

# wait for cursor output then get lambda,continuum measurement from 
# the temporary file created by KAPPA cursor.

while ( ! -e ${curfile} ) 
   sleep 1
end

# grab position

set pos = `parget lastpos cursor`
set low_mask = $pos[1]

# clean up the cursor temporary file

rm -f ${curfile}
touch ${curfile}
 
# get the upper mask boundary

echo "  Left click on the upper limit of the fitting region"
    
cursor showpixel=true style="Colour(curves)=2" plot=vline \
       maxpos=1 device=${plotdev} >> ${curfile}

# wait for cursor output then get lambda,continuum measurement from 
# the temporary file created by KAPPA cursor.

while ( ! -e ${curfile} ) 
   sleep 1
end

# grab position

set pos = `parget lastpos cursor`
set upp_mask = $pos[1]

echo " " 
echo "      Fit Mask:"
echo "        Lower Mask Boundary: ${low_mask}"
echo "        Upper Mask Boundary: ${upp_mask}"

# clean up the cursor temporary file

rm -f ${curfile}
touch ${curfile}

# get continuum values

echo " "
echo "  Left click on your first estimate of the continuum"

cursor showpixel=true style="Colour(curves)=4" plot=hline \
       maxpos=1 device=${plotdev} >> ${curfile}

# wait for cursor output then get lambda,continuum measurement from 
# the temporary file created by KAPPA cursor.

while ( ! -e ${curfile} ) 
   sleep 1
end

# grab position

set pos = `parget lastpos cursor`
set first_cont = $pos[2]

echo "  Left click on your second estimate of the continuum"

cursor showpixel=true style="Colour(curves)=4" plot=hline \
       maxpos=1 device=${plotdev} >> ${curfile}

# wait for cursor output then get lambda,continuum measurement from 
# the temporary file created by KAPPA cursor.

while ( ! -e ${curfile} ) 
   sleep 1
end

# grab position

set pos = `parget lastpos cursor`
set second_cont = $pos[2]

echo " "
echo "      Continuum:"
echo "        First Estimate: ${first_cont}"
echo "        Second Estimate: ${second_cont}"

# work out average continuum

set cont = `echo "${first_cont}+${second_cont}" | bc`
set scale = `echo "${cont}/2.0" | bc`
set remainder = `echo "${cont}%2.0" | bc`
set cont = `echo "${scale}+${remainder}" | bc`

echo "        Average Value: ${cont}"

# get peak position

echo " " 
echo "  Left click on the line peak"

cursor showpixel=true style="Colour(marker)=3" plot=mark \
       maxpos=1 marker=2 device=${plotdev} >> ${curfile}

# wait for cursor output then get lambda,continuum measurement from 
# the temporary file created by KAPPA cursor.

while ( ! -e ${curfile} ) 
   sleep 1
end

# grab position

set pos = `parget lastpos cursor`
set position = $pos[1]
set peak = $pos[2]

echo " "
echo "      Line Position:"
echo "        Peak Position: ${position}"
echo "        Peak Height: ${peak}"

# clean up the cursor temporary file

rm -f ${curfile}
touch ${curfile}

# get fwhm left side

echo " " 
echo "  Left click on the left hand edge of the FWHM"

cursor showpixel=true style="Colour(marker)=3" plot=mark \
       maxpos=1 marker=2 device=${plotdev} >> ${curfile}

# wait for cursor output then get lambda,continuum measurement from 
# the temporary file created by KAPPA cursor.

while ( ! -e ${curfile} ) 
   sleep 1
end

# grab position

set pos = `parget lastpos cursor`
set fwhm_low = $pos[1]

# clean up the cursor temporary file

rm -f ${curfile}
touch ${curfile}

# get fwhm right side

echo "  Left click on the right hand edge of the FWHM"
echo " "

cursor showpixel=true style="Colour(marker)=3" plot=mark \
       maxpos=1 marker=2 device=${plotdev} >> ${curfile}

# wait for cursor output then get lambda,continuum measurement from 
# the temporary file created by KAPPA cursor.

while ( ! -e ${curfile} ) 
   sleep 1
end

# grab position

set pos = `parget lastpos cursor`
set fwhm_upp = $pos[1]

echo "      FWHM:"
echo "        Lower Bound: ${fwhm_low}"
echo "        Upper Bound: ${fwhm_upp}"

# clean up the cursor temporary file

rm -f ${curfile}
touch ${curfile}

# work out fwhm

set fwhm_low = `echo ${fwhm_low} | sed 's/E/\\*10\\^/' | sed 's/+//'`
set fwhm_upp = `echo ${fwhm_upp} | sed 's/E/\\*10\\^/' | sed 's/+//'`
set fwhm = `echo "scale = 15; ${fwhm_upp}-${fwhm_low}" | bc`
echo "        FWHM: ${fwhm}"
echo " "

# get rest wavelength

if ( ${gotrest} == "FALSE" ) then
   echo -n "Rest wavelength: "
   set line_centre = $<
endif

echo " "
echo "      Rest Wavelength:"
echo "        Wavelength: ${line_centre}"

# fit the line

echo "      Fitting:"

fitgauss \
    "in=${ripfile} mask1=${low_mask} mask2=${upp_mask} cont=${cont} "\
    "peak=${peak} fwhm=${fwhm} reguess=no remask=no ncomp=1 "\
    "cf=0 pf=0 wf=0 comp=${component} fitgood=${fitgood} "\
    "centre=${position} logfil=${fitfile} device=${plotdev} dialog=f" >& /dev/null 

# check to see whether fitting was sucessful

if ( ! -e $fitfile ) then
   echo "        No fit available"
   echo ""
   echo -n "Refit (yes/no): "
   set fitgood = $<
   echo " "
   if ( ${fitgood} == "no" || ${fitgood} == "n" ) then
      rm -f ${fitfile}
      goto cleanup 
   else
      if( ${zoomit} == "yes" || ${zoomit} == "y" ) then
         goto rezoom
      else
         goto refit
      endif 
   endif
endif

# get fit from temporary file

set results = `cat ${fitfile} | head -n 23 | tail -1`
set array = \
   `echo $results | awk '{split($0,a," "); for(i=1; i<10; i++) print a[i]}'`

set centre_fit = $array[2]
set centre_err = $array[3]
set peak_height = $array[4]
set peak_err = $array[5]
set fwhm_fit = $array[6]
set fwhm_err = $array[7]
set integral = $array[8]
set integral_err = $array[9]

# show the user the fit

echo "        Centre Position: ${centre_fit} +- ${centre_err}"
echo "        Peak Height: ${peak_height} +- ${peak_err}"
echo "        FWHM: ${fwhm_fit} +- ${fwhm_err}"
echo "        Line integral: ${integral} +- ${integral_err}"

# fit okay?

echo " "

if ( ${forcefit} == "FALSE" ) then
   echo -n "Fit okay (yes/no): "
   set fitgood = $<
   echo " "
else
   set fitgood = yes
endif

if ( ${fitgood} == "no" || ${fitgood} == "n" ) then
   rm -f ${fitfile} 
   if( ${zoomit} == "yes" || ${zoomit} == "y" ) then
      goto rezoom
   else
      goto refit
   endif
else
   rm -f ${fitfile} 
endif

# loop through the entire datacube

touch ${mapfile}
touch ${varfile}

set x = 0
@ x = $x + $lbnd[1]
set y = 0
@ y = $y + $lbnd[2]

set line = ""
set vars = ""

# setup output filename

if ( ${gotoutfile} == "FALSE" ) then
   echo -n "NDF output file: "
   set outfile = $<
endif

date > ${tmpdir}/${user}/pmap_time.dat 

# fit the cube

echo "      Fitting:"
while( $y <= ${ubnd[2]} )
   while ( $x <= ${ubnd[1]} )
      
      set specfile = "${tmpdir}/${user}/${x}_${y}.sdf"
      ndfcopy "in=${infile}($x,$y,) out=${specfile}" \
              "trim=true trimwcs=true"
      fitgauss \
         "in=${specfile} mask1=${low_mask} mask2=${upp_mask} "\
         "cont=${cont} peak=${peak} fwhm=${fwhm} reguess=no remask=no "\
         "ncomp=1 cf=0 pf=0 wf=0 comp=${component} fitgood=${fitgood} "\
         "centre=${position} logfil=${fitfile} device=! "\
         "dialog=f" >& /dev/null

      if ( -e $fitfile ) then
         set results = `cat ${fitfile} | head -n 23 | tail -1`
         set array = \
      `echo $results | awk '{split($0,a," "); for(i=1; i<10; i++) print a[i]}'`

         set centre_fit = $array[2]
         set centre_err = $array[3]
         set peak_height = $array[4]
         set peak_err = $array[5]
         set fwhm_fit = $array[6]
         set fwhm_err = $array[7] 
         set integral = $array[8]
         set integral_err = $array[9] 
         set condition = `echo "if($peak_height < 0) 1" | bc`
         if ( $condition == 1 ) then
            echo "        Spectra ($x,$y)"
            set line = "${line} -9999.99"  
            if ( ${dovar} == "TRUE" ) then
               set vars = "${vars} -9999.99"  
            endif  
         else

            echo "        Spectra ($x,$y): $peak_height +- $peak_err" 

            set line = "${line} ${peak_height}"
            if ( ${dovar} == "TRUE" ) then
              if( ${centre_err} == "nan" || ${centre_err} == "INF" ) then	

                # set variance to magic value VAL__BADD
	        set vars = "${vars} -9999.99"
	      	    
	      else
	      
	        # set variance tp $peak_err
	        set vars = "${vars} ${peak_err}"
	      endif
	       
            endif              
                          
              
         endif
      else
         echo "        Spectra ($x,$y)" 
         set line = "${line} -9999.99" 
         if ( ${dovar} == "TRUE" ) then
            set vars = "${vars} -9999.99"
         endif     
      endif
      rm -f ${fitfile} >& /dev/null
      rm -f ${specfile}       
      @ x = ${x} + 1
   end
   echo "${line}" >> ${mapfile}
   set line = ""
   if ( ${dovar} == "TRUE" ) then
      echo "${vars}" >> ${varfile}
      set vars = ""
   endif     
   set x = 0
   @ x = $x + $lbnd[1]
   @ y = ${y} + 1
end

date >> ${tmpdir}/${user}/pmap_time.dat 

# convert to 2D NDF

echo " "
echo "      Output NDF:"
echo "        Converting: Creating NDF from data." 

ascii2ndf "in=${mapfile} out=${outfile}_tmp shape=[${dims[1]},${dims[2]}] "\
          "maxlen=2048 type='_real'" >& /dev/null

# set the magic values to VAL__BADR

setmagic "in=${outfile}_tmp out=${outfile} repval=-9999.99" >& /dev/null
if ( -e ${outfile}.sdf ) then
   rm -f "${outfile}_tmp.sdf" >& /dev/null
else
   echo "WARNING: Setting MAGIC values failed."
   mv -f ${outfile}_tmp.sdf ${outfile}.sdf
endif

# set the NDF origin

echo "        Origin: Attaching origin (${lbnd[1]},${lbnd[2]})." 
setorigin "ndf=${colfile:r} origin=[${lbnd[1]},${lbnd[2]}]" >& /dev/null
setorigin "ndf=${outfile} origin=[${lbnd[1]},${lbnd[2]}]" >& /dev/null

# attach the variance array

if ( ${dovar} == "TRUE" ) then
   echo "        Converting: Attaching VARIANCE array." 
   ascii2ndf in=${varfile} comp="Variance" out=${outfile} \
             shape="[${dims[1]},${dims[2]}]" \
             maxlen=2048 type='_real'
	     
# set magic values
   mv -f ${outfile}.sdf ${outfile}_tmp.sdf
   setmagic in=${outfile}_tmp out=${outfile} \
            comp="Variance" repval=-9999.99 >& /dev/null
   if ( -e ${outfile}.sdf ) then
      rm -f "${outfile}_tmp.sdf" >& /dev/null
   else
      echo "WARNING: Setting MAGIC variance values failed."
      mv -f ${outfile}_tmp.sdf ${outfile}.sdf
   endif    
	     
endif

# use the white light image to clone the axis and wcs co-ordinates, the
# wcs information will be copied incorrectly if the AXIS structures do
# not exist before the wcs extensiion is cloned. 

if ( ${axis} == "FALSE" ) then
   echo "        Axes: Creating AXIS extensions."
   putaxis "${colfile:r} spectral=3" >& /dev/null
endif
echo "        Axes: Attaching AXIS extensions." 
copyaxis "in=${outfile} like=${colfile:r}" >& /dev/null

echo "        WCS: Attaching WCS information." 
wcscopy "ndf=${outfile} like=${colfile:r}" >& /dev/null

echo "        Title: Setting title." 
settitle "ndf=${outfile} title='Line Strength Map'"

# check to see if we need to plot the output spectra

if ( ${plotspec} == "TRUE" ) then
   lutcol device=${plotdev}
   echo "      Plotting:"
   echo "        Display: Line strength map using percentile scaling." 
   display "${outfile} device=${plotdev} mode=per percentiles=[15,98]"\
           "axes=yes margin=!" >& /dev/null
   echo "        Contour: White light image with equally spaced contours." 
   contour "ndf=${colfile:r} device=${plotdev} clear=no mode=equi"\
           "axes=no ncont=${numcont} pens='colour=2' margin=!" >& /dev/null 
endif

# loop for manual fitting

set loop_var = 1
if ( ${forcefit} == "FALSE" ) then
   if( ${plotspec} == "FALSE" ) then
      lutcol device=${plotdev}
      echo "      Plotting:"
      echo "        Display: Line strength map using percentile scaling." 
      display "${outfile} device=${plotdev} mode=per percentiles=[15,98]"\
              "axes=yes margin=!" >& /dev/null
      echo "        Contour: White light image with equally spaced contours." 
      contour "ndf=${colfile:r} device=${plotdev} clear=no mode=equi"\
              "axes=no ncont=${numcont} pens='colour=2' margin=!" >& /dev/null 
   endif   
   while ( ${loop_var} == 1 )
      
      echo " "
      echo -n "Refit points (yes/no): "
      set refit = $<
      echo " "

      if ( ${refit} == "yes" || ${refit} == "y" ) then

# copy the current output file to a _tmp file
         mv -f ${outfile}.sdf ${outfile}_tmp.sdf
	 
# grab x,y position

         echo " "
         echo "  Left click on pixel to be extracted"
   
         cursor showpixel=true style="Colour(marker)=2" plot=mark \
             maxpos=1 marker=2 device=${plotdev} frame="PIXEL" >> ${curfile}

# wait for cursor output then get x,y co-ordinates from 
# the temporary file created by KAPPA cursor.

         while ( ! -e ${curfile} ) 
            sleep 1
         end
# grab position 

         set pos = \
	    `parget lastpos cursor | awk '{split($0,a," ");print a[1], a[2]}'`

# get pixel co-ordinates

         set xpix = `echo $pos[1] | awk '{split($0,a,"."); print a[1]}'`
         set ypix = `echo $pos[2] | awk '{split($0,a,"."); print a[1]}'`
         @ xpix = $xpix + 1
         @ ypix = $ypix + 1
	 
# clean up the cursor temporary file

         rm -f ${curfile}
         touch ${curfile}
	 
#  extract the spectra     	 

         echo " "
         echo "      Extracing:"
         echo "        (X,Y) pixel: ${xpix},${ypix}"

# extract spectra from cube

         ndfcopy in="${infile}($xpix,$ypix,)" out=${ripfile} \
	         trim=true trimwcs=true

# check to see if the NDF has an AXIS extensions
         set axis = `parget axis ndftrace`

         if ( ${axis} == "FALSE" ) then
            putaxis "${ripfile} spectral=1" >& /dev/null
            echo "        Axes: Adding AXIS extensions"
         endif

# check to see if the NDF has Variance extension
         if ( ${variance} == "FALSE" ) then
            echo "        Variances: Extension present."
         else
            echo "        Variances: Extension absent."
         endif

# label for repeated fitting of the GAUSSIAN

manual_refit: 
     
# plot the ripped spectra

         linplot "${ripfile} device=${plotdev}" >& /dev/null

# zoom if required

         if ( ${gotzoom} == "ASK") then
            echo " "
            echo -n "Zoom in (yes/no): "
            set zoomit = $<
         else if ( ${gotzoom} == "TRUE") then
            set zoomit = "yes"
         else
            set zoomit = "no"
         endif

         if ( ${zoomit} == "yes" || ${zoomit} == "y" ) then

# get the lower limit

            echo " "
            echo "  Left click on lower zoom boundary"
   
            cursor showpixel=true style="Colour(curves)=3" plot=vline \
                   maxpos=1 device=${plotdev} >> ${curfile}
            while ( ! -e ${curfile} ) 
               sleep 1
            end
            set pos = `parget lastpos cursor`
            set low_z = $pos[1]

# clean up the cursor temporary file
            rm -f ${curfile}
            touch ${curfile}
   
# get the upper limit

            echo "  Left click on upper zoom boundary"
   
            cursor showpixel=true style="Colour(curves)=3" plot=vline \
                   maxpos=1 device=${plotdev} >> ${curfile}
            while ( ! -e ${curfile} ) 
               sleep 1
            end
            set pos = `parget lastpos cursor`
            set upp_z = $pos[1]

            echo " "
            echo "      Zooming:"
            echo "        Lower Boundary: ${low_z}"
            echo "        Upper Boundary: ${upp_z}"

# clean up the cursor temporary file
            rm -f ${curfile}
            touch ${curfile}

# label for repeated fitting of the GAUSSIAN

manual_rezoom:   

#  replot the spectra
            linplot "${ripfile} xleft=${low_z} xright=${upp_z}"\
	            "device=${plotdev}" >& /dev/null

         endif
       
# grab the information needed by the FITGAUSS routine

# get the lower mask boundary

         echo " "
         echo "  Left click on the lower limit of the fitting region"
   
         cursor showpixel=true style="Colour(curves)=2" plot=vline \
                maxpos=1 device=${plotdev} >> ${curfile}

# wait for cursor output then get lambda,continuum measurement from 
# the temporary file created by KAPPA cursor.

         while ( ! -e ${curfile} ) 
            sleep 1
         end

# grab position

         set pos = `parget lastpos cursor`
         set low_mask = $pos[1]

# clean up the cursor temporary file

         rm -f ${curfile}
         touch ${curfile}
 
# get the upper mask boundary

         echo "  Left click on the upper limit of the fitting region"
    
         cursor showpixel=true style="Colour(curves)=2" plot=vline \
                maxpos=1 device=${plotdev} >> ${curfile}

# wait for cursor output then get lambda,continuum measurement from 
# the temporary file created by KAPPA cursor.

        while ( ! -e ${curfile} ) 
           sleep 1
         end

# grab position

        set pos = `parget lastpos cursor`
        set upp_mask = $pos[1]

        echo " " 
        echo "      Fit Mask:"
        echo "        Lower Mask Boundary: ${low_mask}"
        echo "        Upper Mask Boundary: ${upp_mask}"

# clean up the cursor temporary file

        rm -f ${curfile}
        touch ${curfile}

# get continuum values

        echo " "
        echo "  Left click on your first estimate of the continuum"

        cursor showpixel=true style="Colour(curves)=4" plot=hline \
               maxpos=1 device=${plotdev} >> ${curfile}

# wait for cursor output then get lambda,continuum measurement from 
# the temporary file created by KAPPA cursor.

        while ( ! -e ${curfile} ) 
           sleep 1
        end

# grab position

        set pos = `parget lastpos cursor`
        set first_cont = $pos[2]

        echo "  Left click on your second estimate of the continuum"

        cursor showpixel=true style="Colour(curves)=4" plot=hline \
               maxpos=1 device=${plotdev} >> ${curfile}

# wait for cursor output then get lambda,continuum measurement from 
# the temporary file created by KAPPA cursor.

        while ( ! -e ${curfile} ) 
          sleep 1
        end

# grab position

        set pos = `parget lastpos cursor`
        set second_cont = $pos[2]

        echo " "
        echo "      Continuum:"
        echo "        First Estimate: ${first_cont}"
        echo "        Second Estimate: ${second_cont}"

# work out average continuum

        set cont = `echo "${first_cont}+${second_cont}" | bc`
        set scale = `echo "${cont}/2.0" | bc`
        set remainder = `echo "${cont}%2.0" | bc`
        set cont = `echo "${scale}+${remainder}" | bc`

        echo "        Average Value: ${cont}"

# get peak position

        echo " " 
        echo "  Left click on the line peak"

        cursor showpixel=true style="Colour(marker)=3" plot=mark \
               maxpos=1 marker=2 device=${plotdev} >> ${curfile}

# wait for cursor 

        while ( ! -e ${curfile} ) 
           sleep 1
        end

# grab position

        set pos = `parget lastpos cursor`
        set position = $pos[1]
        set peak = $pos[2]

        echo " "
        echo "      Line Position:"
        echo "        Peak Position: ${position}"
        echo "        Peak Height: ${peak}"

# clean up the cursor temporary file

        rm -f ${curfile}
        touch ${curfile}

# get fwhm left side

        echo " " 
        echo "  Left click on the left hand edge of the FWHM"

        cursor showpixel=true style="Colour(marker)=3" plot=mark \
               maxpos=1 marker=2 device=${plotdev} >> ${curfile}

# wait for cursor output then get lambda,continuum measurement from 
# the temporary file created by KAPPA cursor.

        while ( ! -e ${curfile} ) 
           sleep 1
        end

# grab position

        set pos = `parget lastpos cursor`
        set fwhm_low = $pos[1]

# clean up the cursor temporary file

        rm -f ${curfile}
        touch ${curfile}

# get fwhm right side

        echo "  Left click on the right hand edge of the FWHM"
        echo " "

        cursor showpixel=true style="Colour(marker)=3" plot=mark \
               maxpos=1 marker=2 device=${plotdev} >> ${curfile}

# wait for cursor output then get lambda,continuum measurement from 
# the temporary file created by KAPPA cursor.

        while ( ! -e ${curfile} ) 
           sleep 1
        end

# grab position

        set pos = `parget lastpos cursor`
        set fwhm_upp = $pos[1]

        echo "      FWHM:"
        echo "        Lower Bound: ${fwhm_low}"
        echo "        Upper Bound: ${fwhm_upp}"

# clean up the cursor temporary file

        rm -f ${curfile}
        touch ${curfile}

# work out fwhm

        set fwhm_low = `echo ${fwhm_low} | sed 's/E/\\*10\\^/' | sed 's/+//'`
        set fwhm_upp = `echo ${fwhm_upp} | sed 's/E/\\*10\\^/' | sed 's/+//'`
        set fwhm = `echo "scale = 15; ${fwhm_upp}-${fwhm_low}" | bc`
        echo "        FWHM: ${fwhm}"
        echo " "

# get rest wavelength

        if ( ${gotrest} == "FALSE" ) then
           echo -n "Rest wavelength: "
           set line_centre = $<
        endif

        echo " "
        echo "      Rest Wavelength:"
        echo "        Wavelength: ${line_centre}"

# fit the line

        echo "      Fitting:"

        fitgauss \
            "in=${ripfile} mask1=${low_mask} mask2=${upp_mask} cont=${cont} "\
            "peak=${peak} fwhm=${fwhm} reguess=no remask=no ncomp=1 "\
            "cf=0 pf=0 wf=0 comp=${component} fitgood=${fitgood} "\
            "centre=${position} logfil=${fitfile} device=${plotdev}"\
	    "dialog=f" >& /dev/null 

# check to see whether fitting was sucessful

        if ( ! -e $fitfile ) then
           echo "        No fit available"
           echo ""
           echo -n "Refit (yes/no): "
           set fitgood = $<
           echo " "
           if ( ${fitgood} == "no" || ${fitgood} == "n" ) then
              rm -f ${fitfile}
              goto cleanup 
           else
              if( ${zoomit} == "yes" || ${zoomit} == "y" ) then
                 goto manual_rezoom
              else
                 goto manual_refit
              endif 
           endif
        endif     
      
 
# get fit from temporary file

        set results = `cat ${fitfile} | head -n 23 | tail -1`
        set array = \
     `echo $results | awk '{split($0,a," "); for(i=1; i<10; i++) print a[i]}'`

        set centre_fit = $array[2]
        set centre_err = $array[3]
        set peak_height = $array[4]
        set peak_err = $array[5]
        set fwhm_fit = $array[6]
        set fwhm_err = $array[7]
        set integral = $array[8]
        set integral_err = $array[9]

# show the user the fit

        echo "        Centre Position: ${centre_fit} +- ${centre_err}"
        echo "        Peak Height: ${peak_height} +- ${peak_err}"
        echo "        FWHM: ${fwhm_fit} +- ${fwhm_err}"
        echo "        Line integral: ${integral} +- ${integral_err}"

# fit okay?

        echo " "

        if ( ${forcefit} == "FALSE" ) then
           echo -n "Fit okay (yes/no/quit): "
           set fitgood = $<
           echo " "
        else
           set fitgood = yes
        endif

        if ( ${fitgood} == "no" || ${fitgood} == "n" ) then
           rm -f ${fitfile} 
           if( ${zoomit} == "yes" || ${zoomit} == "y" ) then
              goto manual_rezoom
           else
              goto manual_refit
           endif
        else if( ${fitgood} == "quit" || ${fitgood} == "q" ) then
           rm -f ${fitfile}
	   goto dropout 
        else    
           rm -f ${fitfile}
	endif 

# use peak_height and peak_err values
	           
	if ( ${dovar} == "TRUE" ) then
	   echo  "        (X,Y) Pixel: ${xpix}:${ypix}"
           echo -n "        Peak Height: $peak_height" 
        else
	   echo "        (X,Y) Pixel: ${xpix},${ypix}"
           echo "        Peak Height: $peak_height ms^-1" 
        endif


# change the pixel value    
	set pixel = "${xpix}:${xpix},${ypix}:${ypix}"
        chpix in=${outfile}_tmp out=${outfile} comp="Data"\
	      newval=${peak_height} section=\'${pixel}\' 
	if ( -e ${outfile}.sdf ) then
	   rm -f ${outfile}_tmp.sdf >& /dev/null
	else
	   echo "WARNING: Inserting new pixel value failed"
	   mv  -f ${outfile}_tmp.sdf ${outfile}.sdf
	endif
	             
# calculate error
            
        if ( ${dovar} == "TRUE" ) then

           echo " +- ${peak_err} ms^-1"	 
	   
	   if( ${peak_err} == "nan" || ${peak_err} == "INF" ) then	

                # set variance to magic value VAL__BADR
	      
	        echo " ms^-1"
	        set peak_err = "-9999.99"
	      	    
	   endif

	    	
# move the output file to a temporary place holder	  	    
 	   mv -f ${outfile}.sdf ${outfile}_tmp.sdf 
	   		     
# change the pixel value    
	   set pixel = "${xpix}:${xpix},${ypix}:${ypix}"
           chpix in=${outfile}_tmp out=${outfile} comp="Variance" \
	         newval=${peak_err} section=\'${pixel}\' 
           if ( -e ${outfile}.sdf ) then
	      rm -f ${outfile}_tmp.sdf >& /dev/null
           else
	     echo "WARNING: Inserting new variance value failed."
	     mv -f ${outfile}_tmp.sdf ${outfile}.sdf
	   endif
	   
# set the magic values to VAL__BADR

	   mv -f ${outfile}.sdf ${outfile}_tmp.sdf >& /dev/null
           setmagic in=${outfile}_tmp out=${outfile} \
	            comp="Variance" repval=-9999.99 >& /dev/null
           if ( -e ${outfile}.sdf ) then
	      rm -f ${outfile}_tmp.sdf
           else
	      echo "WARNING: Setting MAGIC values failed."
	      mv -f ${outfile}_tmp.sdf ${outfile}
	   endif
	   
	endif

# plot the new peak map	 
        lutcol device=${plotdev}
        echo "      Plotting:"
        echo "        Display: Line strength map using percentile scaling." 
        display "${outfile} device=${plotdev} mode=per percentiles=[15,98]"\
                "axes=yes margin=!" >& /dev/null

# cleanup temporary peakmap files, salvage ${outfile}_tmp in the case
# where there is no existing $outfile (i.e. CHPIX has not run)
         if ( -e ${outfile}_tmp.sdf ) then
            if ( -e ${outfile}.sdf ) then
               rm -f ${outfile}_tmp.sdf
            else
               mv -f ${outfile}_tmp.sdf ${outfile}.sdf
            endif
         endif     
	
# end of manual refitting loop      
      else
# drop out of while loop     
         set loop_var = 0
      endif
   end
endif

# dropout point for aborted fitting, try and salvage already existing
# velocity maps that may be lying around instead of throwing them away
dropout:     
if ( -e ${outfile}_tmp.sdf ) then
   if ( -e ${outfile}.sdf ) then
      rm -f ${outfile}_tmp.sdf
   else
      mv -f ${outfile}_tmp.sdf ${outfile}.sdf
   endif
endif

# clean up
cleanup:

rm -f ${curfile} >& /dev/null
rm -f ${colfile} >& /dev/null
rm -f ${ripfile} >& /dev/null
rm -f ${fitfile} >& /dev/null
rm -f ${mapfile} >& /dev/null
rm -f ${varfile} >& /dev/null
rm -f ${tmpdir}/${user}/pmap_time.dat >& /dev/null
rmdir ${tmpdir}/${user} >& /dev/null
