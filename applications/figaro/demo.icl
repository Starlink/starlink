{+
{  Name:
{     demo
{
{  Purpose:
{     Test a cross-section of ICL Figaro applications.
{
{  Language:
{     ICL
{
{  Type of Module:
{     ICL procedure with some Unix commands.
{
{  Description:
{     Start this procedure with:
{
{     ICL> load $FIG_DIR/demo
{
{  Parameters:
{     none.
{
{  Authors:
{     hme: Horst Meyerdierks (UoE, Starlink)
{     acd: Clive Davenhall (UoE, Starlink)
{
{  History:
{     17 Jan 1992 (hme):
{        Original version.
{     26 Oct 1992 (hme):
{        Adapted for Figaro 3.1.
{     06 Apr 1993 (hme):
{        idev, colour, image included.
{        creobj rather than crobj.
{        copobj rather than let.
{     20 Jul 1993 (hme):
{        Disk-FITS and icur added.
{     01 Feb 1994 (hme):
{        Add a copobj between files.
{     27 Jul 1994 (hme):
{        Re-adapt from C shell demo script.
{     25 Jul 1995 (hme):
{        Make it a Unix ICL script (no VMS commands).
{     22 Feb 1996 (hme):
{        Review for the new implementation of data access (FDA).
{     15 Dec 1997 (acd):
{        Removed version number from the comments and labels, so that
{        the script does not have to be edited each time the version
{        number changes).
{-
{.
{
{  Start-up.
      figaro
{
{  First, let's create a 1-D data array for testing.
      print 'This is the Figaro demo.'
      print 'Creating a 1D test file ...'
      creobj type=STRUCT dims=0   object=test1d
      creobj type=AXIS   dims=1   object=test1d.axis
      creobj type=_REAL  dims=100 object=test1d.axis(1).data_array
      creobj type=_REAL  dims=100 object=test1d.data_array
      setobj value=0 object=test1d.axis(1).data_array(1)
      setobj value=0 object=test1d.data_array(1)
      print 'Setting the axis data (logarithmic) ...'
      lxset image=test1d output=test1d  wstart=100 wend=1000   log
      print 'Copying the axis data into the main data ...'
      delobj object=test1d.data_array
      copobj test1d.axis(1).data_array test1d.data_array
      print 'Setting the axis data (linear) ...'
      lxset image=test1d output=test1d  wstart=1 wend=100   log=false
{
{  See what we got there.
      print 'Examining the file ...'
      hdstrace test1d full
      istat test1d  min max min max
{
{  Plot it on the soft device.
      print 'Opening the devices for graphics and imaging ...'
      soft xw
      idev xw
      splot test1d hardcopy=f whole autoscale label="Figaro demo"
{
{  Assemble a 2-D data set.
      print 'Growing the 1D test file into a 2D test file ...'
      growx spectrum=test1d image=test2d ystart=1 yend=50 new ysize=50
      delobj object=test2d.axis(1).data_array
      copobj test1d.axis(1).data_array test2d.axis(1).data_array
      print 'Setting the axis data (linear) ...'
      lxset image=test1d output=test1d  wstart=10000 wend=20000   log=f
      print 'Fiddling with the 1D test file in order to paste a different'
      print '   spectrum into the 2D data ...'
      delobj object=test1d.data_array
      copobj test1d.axis(1).data_array test1d.data_array
      lxset image=test1d output=test1d  wstart=1 wend=100 log=f
      print 'Growing the 1D test file into part of the 2D test file ...'
      growx spectrum=test1d image=test2d ystart=24 yend=26 new=f
{
{  Have a look at this image.
      print 'Examining the file ...'
      hdstrace test2d full
      istat test2d  min max min max
      print 'Grey scale display on the line graphics device ...'
      igrey test2d  min max min max low=0 high=2000 label="Figaro demo" ~
         adjust=f hardcopy=f
      print 'Load colour lookup table into imaging device ...'
      cp $FIG_DIR/standard_lut.sdf .
      colour standard_lut
      print 'Colour display on the imaging device (75% histogram optimised) ...'
      image test2d min max min max autoscale=y optimize=0.75 xplaces=0 ~
         xorigin=50 yorigin=50 xpixels=400 ypixels=200 aspect=no erase=yes
{
{  Write/read disk FITS, compare.
      print 'Writing the image to disk FITS and reading it back ...'
      wdfits image=test2d file=test_fits.fits
      rdfits file=test_fits.fits image=test_fits swap=t float=t
      print 'Examining the difference ...'
      isub   test_fits test2d test_fits2
!     mv test_fits2.sdf test_fits.sdf
      istat  test_fits  min max min max
{
{  Try S distortion correction.
!     cp $FIG_DIR/test_sdist.sdf .
      cp $FIG_DIR/spiketrum.def .
      cp $FIG_DIR/hd84937.tab .
      cp $FIG_DIR/g158m100.tab .

      print 'S distortion correction ...'
      image test_sdist min max min max erase=t optimize=0.25 ~
         autoscale=t xplaces=1 yplaces=1 aspect=f accept
      colour grey
      print ' '
      print ' Please indicate one position in each of the three bright'
      print ' S curves with the cursor and space bar. Then quit with Q.'
      print ' '
      icur
      sdist test_sdist columns=3 trace=Gauss width=3 maxdeg=10 softd=f accept
      cdist test_sdist ystart=min yend=max output=test_sdist2 maxdegy=10 accept
      image test_sdist2 erase=f accept
{
{  Now try a GKS calling application.
      print 'Finding and extracting fibres in the image ...'
      print ' '
      print 'You have to give <RETURN> four times in response to prompts.'
      print ' '
      findsp image=test_sdist pfile=test_pfile black=0 white=35 numfib=3 ~
         norder=10 npts=100 fwcent=3 cfw=1 yfirst=11 ysep=23
      polext image=test_sdist pfile=test_pfile.pol extwidth=6 ~
         output=test_sdist3 accept
      iplots test_sdist3 ystart=min yend=max whole=t autoscale=f ~
         label="Extracted fibres" hardcopy=f accept
      print ' '
{
{  Spectrophotometric calibration
!     cp $FIG_DIR/test_stand.sdf .
      print ' Pretend this is HD84937 uncalibrated. Flux calibrate it.'
      splot test_stand hardcopy=f whole autoscale label="Figaro demo"
      gspike spectrum=test_stand table=hd84937 spiketrum=test_stand2
      splot test_stand2 hardcopy=f whole autoscale label="Figaro demo"
      cspike spiketrum=test_stand2 spectrum=test_stand output=test_stand3
      print ' '
      print 'You have to press Q to quit,'
      print 'and then confirm by entering YES.'
      print ' '
      spied  spiketrum=test_stand3 output=test_stand3
      interp spiketrum=test_stand3 spectrum=test_stand4
      splot test_stand4 hardcopy=f whole autoscale label="Figaro demo"
      spflux spectrum=test_stand calspect=test_stand4 output=test_stand5
      splot test_stand5 hardcopy=f whole autoscale label="Figaro demo"
{
      print ' Pretend this is G158M100 uncalibrated. Flux calibrate it.'
      splot test_stand hardcopy=f whole autoscale label="Figaro demo"
      gspike spectrum=test_stand table=g158m100 spiketrum=test_stand2
      splot test_stand2 hardcopy=f whole autoscale label="Figaro demo"
      interp spiketrum=test_stand2 spectrum=test_stand3
      splot test_stand3 hardcopy=f whole autoscale label="Figaro demo"
      splot test_stand hardcopy=f whole autoscale label="Figaro demo"
      print ' '
      print 'Use cursor and key A to mark some continuum points,'
      print 'then quit by pressing X and entering NO.'
      print ' '
      cfit   output=test_stand4
      splot test_stand4 hardcopy=f whole autoscale label="Figaro demo"
      caldiv standard=test_stand3 spectrum=test_stand4 output=test_stand5
      splot test_stand5 hardcopy=f whole autoscale label="Figaro demo"
      spflux spectrum=test_stand calspect=test_stand5 output=test_stand6
      splot test_stand6 hardcopy=f whole autoscale label="Figaro demo"
{
{  Complex data and FFT
      print ' Reduce the noise by filtering in Fourier-transformed space.'
      colour grey
      image test_sdist min max min max erase=t optimize=0.25 ~
         autoscale=t xplaces=1 yplaces=1 aspect=f accept
      cosbell test_sdist 10 test_cmplx1
      imult   test_sdist test_cmplx1 test_cmplx2
      image test_cmplx2 min max min max erase=f optimize=0.25 ~
         autoscale=t xplaces=1 yplaces=1 aspect=f accept
      r2cmplx test_cmplx2 test_cmplx3
      i2cmplx test_cmplx2 test_cmplx3
      r2cmplx test_cmplx2 test_cmplx3
      fft     test_cmplx3 test_cmplx4
      cmplxconj test_cmplx4 test_cmplx4
      cmplx2m test_cmplx4 test_cmplx5
      image test_cmplx5 min max min max erase=f optimize=0.25 ~
         autoscale=t xplaces=1 yplaces=1 aspect=f accept
      cmplxfilt test_cmplx4 0 0.3 test_cmplx5
      cmplxmult test_cmplx4 test_cmplx5 test_cmplx6
      cmplx2m test_cmplx6 test_cmplx7
      image test_cmplx7 min max min max erase=f optimize=0.25 ~
         autoscale=t xplaces=1 yplaces=1 aspect=f accept
      bfft test_cmplx6 test_cmplx7
      cmplx2r test_cmplx7 test_cmplx8
      image test_cmplx8 min max min max erase=f optimize=0.25 ~
         autoscale=t xplaces=1 yplaces=1 aspect=f accept
      idiv    test_cmplx8 test_cmplx1 test_cmplx9
      image test_cmplx9 min max min max erase=f optimize=0.25 ~
         autoscale=t xplaces=1 yplaces=1 aspect=f accept
{
{  Delete the files created.
      print 'Deleting all test files ...'
!     rm -f test1d.* test2d.* test_pfile.* test_fits.* test_sdist*.* sdist.dat test_stand*.* test_cmplx*.* hd84937.tab g158m100.tab spiketrum.def standard_lut.sdf
      print ' '
      print 'Now try something yourself.'
{
{  That's it


