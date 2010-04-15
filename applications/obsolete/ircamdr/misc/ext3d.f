	program ext3d

* Program to extract sub-array sum from 3d stack of images

	implicit none

        integer
     :	  chr_len

	integer
     :	  stn,
     :	  ste,
     :	  fps, fpi, fpjj,
     :	  i,                  ! looping variable
     :	  j,                  ! looping variable
     :	  jj,                 ! looping variable
     :	  k,                  ! looping variable
     :	  kk,                  ! looping variable
     :	  l,                  ! looping variable
     :	  l1,                 ! length of character string
     :	  xdim,               ! x-dimension of NOD2 array
     :	  ydim,               ! y-dimension of NOD2 array
     :	  nx,                 ! x-size of internal array for data
     :	  ny,                 ! y-size of internal array for data
     :	  m,                  ! number of pixels in poly fitting area
     :	  kplus1,             ! order of polynomial fit + 1
     :	  nrows,              ! number of rows of coefficient file
     :	  ifail,              ! fail status for NAG routine
     :	  kval,               ! order of polynomial fit
     :	  mmax,               ! maximum number of pixels in fit
     :	  kplus1max,          ! maximum number for kplus1
     :	  bestdeg,            ! best fit degree in poly fit
     :	  num,                ! number variable
     :	  status,             ! status for sdf subroutines
     :	  maxdeg,             ! maximum order of poly fit
     :	  xstart,             ! x-scan start pixel for fit
     :    xend,               ! x-scan end pixel for fit
     :	  ystart,             ! y-scan start pixel for fit
     :    yend,               ! y-scan end pixel for fit
     :	  dotpos,             ! position of dot in filename
     :	  npts                ! number of points in fit

	parameter ( nx = 256, ny = 256)
	parameter ( mmax=256, kplus1max=40, nrows=kplus1max)

	real
     :	  aarr( nx, ny),      ! work array for data
     :	  barr( nx, ny),      ! work array for data
     :	  restval,            ! value for rest of output array
     :	  threshup,           ! upper threshold level for fit
     :	  threshdn,           ! lower threshold level for fit
     :	  minrms,             ! minimum rms from poly-fit data
     :	  yfit2( mmax, 1),    ! work array
     :	  mean,               ! mean of array
     :	  median,             ! median of array
     :	  mode                ! mode of array

	real*8
     :	  sum,
     :	  x( mmax),           ! x values fitted
     :	  y( mmax),           ! y values fitted
     :	  w( mmax),           ! weights of fitted data
     :	  work1( 3*mmax),     ! NAG work array 1
     :	  work2( 2*kplus1max),! NAG work array 2
     :	  a( nrows, kplus1max), ! NAG coefficient array
     :	  s( kplus1max),      ! NAG work array
     :	  xk,                 ! x values scaled to -1 to +1
     :	  yfit,               ! y poly fit value
     :	  ak( kplus1max)      ! coefficients at best k

	character
     :    utd*6,
     :	  junk1*10,
     :	  filenam1*80,        ! filename of input NOD2 file
     :	  filenam2*80,        ! filename of output NOD2 file
     :	  dline*132,          ! data line read from user
     :	  tline*80,           ! temporary data input line
     :	  whatfit*80          ! what is fitted X,Y or Surface


*     Change these parameters for each night's observing !!!!!

        utd = '950422'
	xstart = 127
	ystart = 127
	xend = 129
	yend = 129

  50	type *, 'Start, End number in seq. : '
	read( 5, *) stn, ste

  	type *, 'FP Start Z, FP Z Increment : '
	read( 5, *) fps, fpi

	open( unit=142, file='fp.dat', status='new')

*      Put name input to upper case and get length and set real variable
	kk = 0
	do jj = stn, ste
	  kk = kk + 1
	  call chr_itoc( jj, junk1, l1)
*	  type *, junk1( 1:l1)

	  tline = 'ro'//utd(1:6)//'_'//junk1( 1:l1)

	  call chr_ucase( tline)
	  l1 = chr_len( tline)
	  filenam1 = tline( 1:l1)
	  type *, 'Filename = ', filenam1( 1:20)

*        Read sdf file of input data
	  call mapgl_readsdf2( filenam1, nx, ny, aarr, xdim, ydim)

	  sum = 0.0d0
	  do j = ystart, yend
	    do k = xstart, xend
	      sum = sum + aarr( j, k)
	    end do
	  end do
	  mean = sngl( sum/real( (yend-ystart+1)*(xend-xstart+1)))

	  fpjj = fps + (kk-1)*fpi
	  write( 142, *) fpjj, mean

	end do

	close( 142)

  	end
