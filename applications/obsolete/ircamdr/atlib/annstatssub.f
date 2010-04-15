	SUBROUTINE ANNSTATSSUB( NX, NY, INARR, NXO, NYO, OUTARR,
     :	                        X, Y, ECC, ANG, WIDTH, PLATSCAL,
     :	                        USEBAD, BADVAL, OUTFILE)

*      No implicit variables
	IMPLICIT NONE

*     History
*     13-JUL-1994  Changed STR$ to CHR_ and LIB$ to FIO_ (SKL@JACH)
*      9-AUG-2004  Use COS/SIN rather than COSD/SIND (TIMJ@JACH)
*                  Use FIO for open and close

*      Include ADAM parameter defintions
	INCLUDE 'SAE_PAR'
        INCLUDE 'CHR_ERR'
        INCLUDE 'FIO_PAR'

*      Local variables
	INTEGER
     :    NX,                  ! x size of input image
     :	  NY,                  ! y size of input image
     :    NXO,                 ! x size of output image
     :	  NYO,                 ! y size of output image
     :	  X,                   ! x centre of annuli
     :	  Y,                   ! y centre of annuli
     :	  NPTS,                ! number of good pixels in annulus
     :	  BNPTS                ! number of bad pixels in annulus
      INTEGER
     :	  I,                   ! looping variable
     :	  J,                   ! looping variable
     :	  K,                   ! looping variable
     :	  STATUS,              ! status input/return
     :	  NUMANN,              ! number of annuli in image
     :	  LUN,                 ! logical unit for output file
     :	  MAXDATA              ! maximum number of pixels in each annulus

*      Setup fixed parameters
	PARAMETER ( MAXDATA = 10000)
	REAL D2R	       ! Degrees to radians
	PARAMETER ( D2R = 0.017453292519943295769 )

	REAL
     :	  INARR( NX, NY),      ! input image with data
     :	  OUTARR( NXO, NYO),   ! output image for map of annuli
     :	  PLATSCAL,            ! plate scale in arcsec/pixel
     :    WIDTH,               ! width of annuli in arcseconds
     :	  R,                   ! vector length from centre to pixel
     :	  R1,                  ! inner edge of annuli radius
     :	  R2                   ! outer edge of annuli radius
      REAL
     :	  MAXX,                ! x maximum radius from centre to pixel
     :	  MAXY,                ! y maximum radius from centre to pixel
     :	  MAXXR,               ! rotated version of MAXX
     :	  MAXYR,               ! rotated version of MAXY
     :	  MAXR,                ! maximum radius vector from centre to pixel
     :	  MAXRA,               ! arcsecond version of MAXR
     :	  VALMAX,              ! maximum value in annulus
     :	  VALMIN,              ! minimum value in annulus
     :	  SUM,                 ! sum of pixels in annulus
     :	  MEAN,                ! mean of pixels in annulus
     :	  MEDIAN,              ! median of pixels in annulus
     :	  MODE,                ! mode of pixels in annulus
     :	  BADVAL,              ! bad pixel value
     :	  STD,                 ! standard deviation in annulus
     :	  ECC,                 ! eccentricity of annuli
     :	  ANG,                 ! position angle of annuli wrt E
     :    ANGR,                ! position angle ANG in radians
     :	  XR,                  ! rotated pixel vector in X
     :	  YR                   ! rotated pixel vector in Y

	REAL*8
     :	  DATARR( MAXDATA),    ! data array for NAG sorting
     :    SUMSQ                ! sum of squares for std calculation

	CHARACTER*( *)
     :	  OUTFILE              ! output ascii file name

	LOGICAL
     :	  USEBAD               ! whether to use bad pixel value

*     calculate maximum values from centre to edge pixel
	MAXX = MAX( REAL( ABS( NX-X)), REAL( X))
	MAXY = MAX( REAL( ABS( NY-Y)), REAL( Y))

*     convert to radians
	ANGR = ANG * D2R

*      rotate maxima
	MAXXR = MAXX*COS( ANGR ) - MAXY*SIN( ANGR )
	MAXYR = MAXX*SIN( ANGR ) + MAXY*COS( ANGR )

*      calculate maximum radius vector
	MAXR = SQRT( MAXXR**2 + MAXYR**2/( 1-ECC**2))

*      convert to arcseconds
	MAXRA = MAXR*PLATSCAL

*      calculate number of annuli in image
	IF( WIDTH .GT. 0.0) THEN
	  NUMANN = MAXRA/WIDTH+1
	ELSE
	  NUMANN = 0
	END IF

*      tell user number of annuli in image
	CALL MSG_OUT( 'BLANK', ' ', STATUS)
	CALL MSG_SETR( 'SZ', WIDTH)
	CALL MSG_SETI( 'NU', NUMANN)
	CALL MSG_OUT( 'MESS',
     :	  'Number of (^SZ arcsec) annuli in your image is ^NU',
     :	  STATUS)

*      Open output file
	CALL FIO_OPEN( OUTFILE, 'WRITE','LIST',0, LUN, STATUS)

*      write header line for output file
	CALL FIO_WRITE(LUN,
     :	  '  No       Ri     Ro       N    BN         ' //
     :	  'Mea         Med         Mod         Sum         ' //
     :	  'Max         Min         Std', STATUS)

*      loops to initialize the output array
	DO K = 1, NYO
	  DO J = 1, NXO
	    OUTARR( J, K) = 0.0
	  END DO
	END DO

*      loop to scan through all annuli in image
	DO I = 1, NUMANN

	  CALL MSG_OUT( 'BLANK', ' ', STATUS)

*        set number of good/bad pixels in annuli to zero
	  NPTS = 0
	  BNPTS = 0

*        calculate inner and outer radius of current annulus
	  R1 = ( I-1)*WIDTH/PLATSCAL
	  R2 = I*WIDTH/PLATSCAL

*        loops to scan through image
	  DO K = 1, NY
	    DO J = 1, NX

*            calculate x and y of current pixel wrt rotated annulus
*            centre
	      XR = ( J-X)*COS( ANGR ) - (K-Y)*SIN( ANGR )
	      YR = ( J-X)*SIN( ANGR ) + (K-Y)*COS( ANGR )

*            calculate radius vector from above x,y
	      R = SQRT( XR**2 + YR**2/( 1-ECC**2))

*            test if current pixel in current annulus
	      IF( R .GE. R1 .AND. R .LT. R2) THEN

*              test if current pixel is a good value
	        IF( USEBAD .AND.
     :	            INARR( J, K) .NE. BADVAL) THEN

*                increment number of good pixels in current annulus
	          NPTS = NPTS + 1

*                set array for sorting with current pixel
	          DATARR( NPTS) = DBLE( INARR( J, K))

*                test if current annulus is odd or even and set
*                output map accordingly
	          IF( IFIX( I/2.0+0.5)*2 .EQ. I) THEN
	            OUTARR( J, K) = 1
	          ELSE
	            OUTARR( J, K) = 0
	          END IF

*              here if current pixel is bad
	        ELSE IF( USEBAD .AND.
     :	                 INARR( J, K) .EQ. BADVAL) THEN

*                increment number of bad pixels in current array
	          BNPTS = BNPTS + 1

*                put value into bad pixels (-1)
	            OUTARR( J, K) = -1

*              here if not using bad pixel value
	        ELSE IF( .NOT. USEBAD ) THEN

*                increment number of good pixels in annulus
	          NPTS = NPTS + 1

*                set array for sorting with good pixel value
	          DATARR( NPTS) = DBLE( INARR( J, K))

	        END IF
	      END IF
	    END DO
	  END DO

*        tell user some important information on current annulus
	  CALL MSG_SETI( 'N', I)
	  CALL MSG_OUT( 'MESS',
     :	    ' Annulus number ^N',
     :	    STATUS)
	  CALL MSG_SETI( 'NU', NPTS)
	  CALL MSG_OUT( 'MESS',
     :	    ' Number of pixels in annulus       = ^NU',
     :	    STATUS)

	  IF( USEBAD .AND. BNPTS .NE. 0) THEN
	    CALL MSG_SETI( 'N', I)
	    CALL MSG_SETI( 'NU', BNPTS)
	    CALL MSG_OUT( 'MESS',
     :	      ' Number of BAD pixels in annulus   = ^NU',
     :	      STATUS)
	  END IF

*        Sort the pixel values in current annulus
	  IF( NPTS .GT. 0) THEN
	    CALL PDA_QSAD( NPTS, DATARR )

*          call subroutine to find median for the input DATARR
	    CALL MED3D_CALMEDSUB( NPTS, DATARR, VALMAX, VALMIN, SUM, MEAN,
     :	                          MEDIAN, MODE)

*          Scan through pixels in annulus and calculate standard deviation
	    SUMSQ = 0.0D0
	    DO J = 1, NPTS
	      SUMSQ = SUMSQ + DATARR( J)**2
	    END DO

*          Test for overflows of single precision variable
	    IF( ABS( SUM) .LT. 1.0E20) THEN

*            calculate std over pixels
	      STD = SUMSQ/NPTS - ( SUM/NPTS)**2
	      IF( STD .GE. 0.0D0) THEN
	        STD = SQRT( STD )
	      ELSE
	       STD = -999.0
	      END IF
	    ELSE
	      STD = -999.0
	    END IF

	  ELSE

*          set output values if no pixels found in current annulus
	    VALMAX = 0.0
	    VALMIN = 0.0
	    SUM = 0.0
	    MEAN = 0.0
	    MEDIAN = 0.0
	    MODE = 0.0
	    STD = 0.0

	  END IF

*       scale current radius vectors to arcseconds
	  R1 = R1*PLATSCAL
	  R2 = R2*PLATSCAL

*        tell user result of all this messing about...
	  CALL MSG_SETR( 'RI', R1)
	  CALL MSG_SETR( 'RO', R2)
	  CALL MSG_OUT( 'MESS',
     :	    ' Inner/outer radius (arcsec)       = ^RI/^RO' ,
     :	    STATUS)
	  CALL MSG_SETR( 'MEA', MEAN)
	  CALL MSG_SETR( 'MED', MEDIAN)
	  CALL MSG_SETR( 'MOD', MODE)
	  CALL MSG_OUT( 'MESS',
     :	    ' Mean/Median/Mode                  = ^MEA/^MED/^MOD',
     :	    STATUS)
	  CALL MSG_SETR( 'SUM', SUM)
	  CALL MSG_SETR( 'MAX', VALMAX)
	  CALL MSG_SETR( 'MIN', VALMIN)
	  CALL MSG_OUT( 'MESS',
     :	    ' Sum/Maximum/Minimum               = ^SUM/^MAX/^MIN',
     :	    STATUS)
	  CALL MSG_SETR( 'STD', STD)
	  CALL MSG_OUT( 'MESS',
     :	    ' Standard Deviation in annulus     = ^STD',
     :	    STATUS)

*        write result to output ascii file
	  WRITE( LUN, 10) I, R1, R2, NPTS, BNPTS, MEAN, MEDIAN, MODE,
     :	                  SUM, VALMAX, VALMIN, STD

  10	  FORMAT( I4,2X,2F7.2,2X,2I6,7(F12.3))

	END DO

*      close output ascii file and release lun
	CALL FIO_CLOSE( LUN, STATUS )

*      tell user the name of the output ascii file
	CALL CHR_UCASE( OUTFILE )
	CALL MSG_OUT( 'BLANK', ' ', STATUS)
	CALL MSG_SETC( 'OUT', OUTFILE)
	CALL MSG_OUT( 'MESS', 'Results written to ^OUT', STATUS)
	CALL MSG_OUT( 'BLANK', ' ', STATUS)

	END
