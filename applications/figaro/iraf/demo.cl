#+
#  Name:
#     demo
# 
#  Purpose:
#     Test a cross-section of IRAF Figaro applications.
# 
#  Language:
#     IRAF CL
# 
#  Type of Module:
#     CL procedure with some Unix commands.
# 
#  Description:
#     Start this procedure with:
#
#     cl> task $demo=figaro$demo.cl
#     cl> demo
# 
#  Parameters:
#     none.
# 
#  Authors:
#     AJC: A J Chipperfield (Starlink, RAL)
# 
#  History:
#     29-MAY-1997 (AJC):
#        Adapted from the ICL Figaro demo
#     10-JUL-1997 (AJC):
#        Change version to 5.2
#     28-AUG-1997 (AJC):
#        Remove Version no from labels
#        Find data files in figaro$
#-
#.
#  Start-up.
      figaro
      use_ndf
      unlearn figaro
      print 'Clearing out old files.'
      delete test_sdist.sdf,test_stand.sdf
# 
#  First, let's create a 1-D data array for testing.
      print "This is the Figaro 5.2 demo."
      print "Creating a 1D test file ..."
      creobj type=STRUCT dims=0   object=test1d
      creobj type=AXIS   dims=1   object=test1d.axis
      creobj type=_REAL  dims=100 object="test1d.axis(1).data_array"
      creobj type=_REAL  dims=100 object=test1d.data_array
      setobj value=0 object="test1d.axis(1).data_array(1)"
      setobj value=0 object="test1d.data_array(1)"
      print 'Setting the axis data (logarithmic) ...'
      lxset image=test1d output=test1d  wstart=100 wend=1000   log=yes
      print 'Copying the axis data into the main data ...'
      delobj object=test1d.data_array
      copobj "test1d.axis(1).data_array" test1d.data_array
      print 'Setting the axis data (linear) ...'
      lxset image=test1d output=test1d  wstart=1 wend=100   log=no
# 
#  See what we got there.
      print 'Examining the file ...'
#      hdstrace test1d full=yes
      istat test1d
# 
#  Plot it on the soft device.
      print 'Opening the devices for graphics and imaging ...'
      soft xw
      idev xw
      splot test1d hardcopy=no whole=yes autoscale=yes label="Figaro demo"
# 
#  Assemble a 2-D data set.
      print 'Growing the 1D test file into a 2D test file ...'
      growx spectrum=test1d image=test2d ystart=1 yend=50 new=yes ysize=50
      delobj object="test2d.axis(1).data_array"
      copobj "test1d.axis(1).data_array" "test2d.axis(1).data_array"
      print 'Setting the axis data (linear) ...'
      lxset image=test1d output=test1d  wstart=10000 wend=20000   log=no
      print 'Fiddling with the 1D test file in order to paste a different'
      print '   spectrum into the 2D data ...'
      delobj object=test1d.data_array
      copobj "test1d.axis(1).data_array" test1d.data_array
      lxset image=test1d output=test1d  wstart=1 wend=100 log=no
      print 'Growing the 1D test file into part of the 2D test file ...'
      growx spectrum=test1d image=test2d ystart=24 yend=26 new=no
# 
#  Have a look at this image.
      print 'Examining the file ...'
#      hdstrace test2d full=yes
      istat test2d 
      print 'Grey scale display on the line graphics device ...'
      igrey test2d low=0 high=2000 label="Figaro demo" \
         adjust=no hardcopy=no
      print 'Load colour lookup table into imaging device ...'
      colour standard_lut
      print 'Colour display on the imaging device (75% histogram optimised) ...'
      image test2d autoscale=yes optimize=0.75 xplaces=0 \
         xorigin=50 yorigin=50 xpixels=400 ypixels=200 aspect=no erase=yes
#
#  Write/read disk FITS, compare.
      print 'Writing the image to disk FITS and reading it back ...'
      wdfits image=test2d file=test_fits.fits
      rdfits image=test_fits file=test_fits.fits swap=yes float=yes
      print 'Examining the difference ...'
      isub   test_fits test2d test_fits2
!     mv test_fits2.sdf test_fits.sdf
      istat  test_fits  
#
#  Try S distortion correction.
      copy figaro$test_sdist.sdf test_sdist.sdf
      print 'S distortion correction ...'
      image test_sdist erase=yes optimize=0.25 \
         autoscale=yes xplaces=1 yplaces=1 aspect=no
      colour grey
      print ' '
      print ' Please indicate one position in each of the three bright'
      print ' S curves with the cursor and space bar. Then quit with Q.'
      print ' '
      icur
!     rm -f sdist.dat
      sdist test_sdist columns=3 trace=Gauss width=3 maxdeg=10 softd=no
      cdist test_sdist output=test_sdist2 maxdegy=10 ystart=1 yend=76
      image test_sdist2 erase=no auto=yes aspect=no opt=.25 xplaces=1 yplaces=1
# 
#  Now try a GKS calling application.
      print 'Finding and extracting fibres in the image ...'
      print ' '
      print 'Respond with <RETURN> or 0 as appropriate to the next four prompts'
      print ' '
      findsp image=test_sdist pfile=test_pfile black=0 white=35 numfib=3 \
         norder=10 npts=100 fwcent=3 cfw=1 yfirst=11 ysep=23
      polext image=test_sdist pfile=test_pfile.pol dfile="" extwidth=6 \
         output=test_sdist3
      iplots test_sdist3 whole=yes autoscale=no \
         ystart=1 yend=3 nspect=3 label="Extracted fibres" hardcopy=no
      print ' '
#
#  Spectrophotometric calibration
      copy figaro$test_stand.sdf test_stand.sdf
      print ' Pretend this is HD84937 uncalibrated. Flux calibrate it.'
      splot test_stand hardcopy=no whole=yes autoscale=yes \
         label="Figaro demo"
      gspike spectrum=test_stand table=hd84937 spiketrum=test_stand2
      splot test_stand2 hardcopy=no whole=yes autoscale=yes \
         label="Figaro demo"
      cspike spiketrum=test_stand2 spectrum=test_stand output=test_stand3
      print ' '
      print 'You have to press Q to quit,'
      print 'and then confirm by entering YES.'
      print ' '
      spied  spiketrum=test_stand3 output=test_stand3
      interp spiketrum=test_stand3 spectrum=test_stand4
      splot test_stand4 hardcopy=no whole=yes autoscale=yes \
         label="Figaro demo"
      spflux spectrum=test_stand calspect=test_stand4 output=test_stand5
      splot test_stand5 hardcopy=no whole=yes autoscale=yes \
         label="Figaro demo"
#
      print ' Pretend this is G158M100 uncalibrated. Flux calibrate it.'
      splot test_stand hardcopy=no whole=yes autoscale=yes \
         label="Figaro demo"
      gspike spectrum=test_stand table=g158m100 spiketrum=test_stand2
      splot test_stand2 hardcopy=no whole=yes autoscale=yes \
         label="Figaro demo"
      interp spiketrum=test_stand2 spectrum=test_stand3
      splot test_stand3 hardcopy=no whole=yes autoscale=yes \
         label="Figaro demo"
      splot test_stand hardcopy=no whole=yes autoscale=yes \
         label="Figaro demo"
      print ' '
      print 'Use cursor and key A to mark some continuum points,'
      print 'then quit by pressing X and entering NO.'
      print ' '
      cfit   output=test_stand4
      splot test_stand4 hardcopy=no whole=yes autoscale=yes \
         label="Figaro demo"
      caldiv standard=test_stand3 spectrum=test_stand4 output=test_stand5
      splot test_stand5 hardcopy=no whole=yes autoscale=yes \
         label="Figaro demo"
      spflux spectrum=test_stand calspect=test_stand5 output=test_stand6
      splot test_stand6 hardcopy=no whole=yes autoscale=yes \
         label="Figaro demo"
#
#  Complex data and FFT
      print ' Reduce the noise by filtering in Fourier-transformed space.'
      colour grey
      image test_sdist erase=yes optimize=0.25 \
         autoscale=yes xplaces=1 yplaces=1 aspect=no
      cosbell test_sdist 10 test_cmplx1
      imult   test_sdist test_cmplx1 test_cmplx2
      image test_cmplx2 erase=no optimize=0.25 \
         autoscale=yes xplaces=1 yplaces=1 aspect=no
      r2cmplx test_cmplx2 test_cmplx3
      i2cmplx test_cmplx2 test_cmplx3
      r2cmplx test_cmplx2 test_cmplx3
      fft     test_cmplx3 test_cmplx4
      cmplxconj test_cmplx4 test_cmplx4
      cmplx2m test_cmplx4 test_cmplx5
      image test_cmplx5 0 1 0 1 erase=no optimize=0.25 \
         autoscale=yes xplaces=1 yplaces=1 aspect=no
      cmplxfilt test_cmplx4 0 0.3 test_cmplx5
      unlearn cmplxfilt
      cmplxmult test_cmplx4 test_cmplx5 test_cmplx6
      cmplx2m test_cmplx6 test_cmplx7
      image test_cmplx7 0 1 0 1 erase=no optimize=0.25 \
         autoscale=yes xplaces=1 yplaces=1 aspect=no
      bfft test_cmplx6 test_cmplx7
      cmplx2r test_cmplx7 test_cmplx8
      image test_cmplx8 erase=no optimize=0.25 \
         autoscale=yes xplaces=1 yplaces=1 aspect=no
      idiv    test_cmplx8 test_cmplx1 test_cmplx9
      image test_cmplx9 erase=no optimize=0.25 \
         autoscale=yes xplaces=1 yplaces=1 aspect=no
# 
#  Delete the files created.
      print 'Deleting all test files ...'
!rm -f test1d.* test2d.* test_pfile.* test_fits.* test_sdist*.* sdist.dat \
test_stand*.* test_cmplx*.*
      print ' '
      print 'Now try something yourself.'
#
#  That's it
