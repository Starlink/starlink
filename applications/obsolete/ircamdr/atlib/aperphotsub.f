	SUBROUTINE APERPHOTSUB( NX, NY, INARR, X, Y, ECC, ANG, RAD1, 
     :	                        RAD2, PLATSCAL, USEBAD, BADVAL, NPTS, 
     :	                        BNPTS, SUM, MEAN, MEDIAN, MODE, 
     :	                        VALMAX, VALMIN, STD)

	IMPLICIT NONE           

	INCLUDE 'SAE_PAR'       

	INTEGER
     :    NX,
     :	  NY,
     :	  X,
     :	  Y,
     :	  NPTS,
     :	  BNPTS,
     :	  J,
     :	  K,
     :	  STATUS,
     :	  IFAIL,
     :	  MAXDATA

	PARAMETER ( MAXDATA = 10000)

	REAL
     :	  INARR( NX, NY),
     :	  PLATSCAL,
     :	  R,
     :	  RAD1,
     :	  RAD2,
     :	  VALMAX, 
     :	  VALMIN, 
     :	  SUM, 
     :	  MEAN, 
     :	  MEDIAN, 
     :	  MODE,
     :	  BADVAL,
     :	  STD,
     :	  ECC,
     :	  ANG,
     :	  XR,
     :	  YR

	REAL*8
     :	  DATARR( MAXDATA),
     :    SUMSQ

	LOGICAL
     :	  USEBAD

*      Initialize number of good/bad pixels in annulus variables
	NPTS = 0
	BNPTS = 0

!	type *, 'xcen, ycen = ', x, y
!	type *, 'ecc, ang  = ', ecc, ang

*      Scale radii from arcseconds to pixels
!	type *, 'rad1, rad2 before = ', rad1, rad2
	RAD1 = RAD1/PLATSCAL
	RAD2 = RAD2/PLATSCAL
!	type *, 'rad1, rad2 after  = ', rad1, rad2

*      Scan through array looking to see if pixels in annuli
	DO K = 1, NY

	  DO J = 1, NX

*          Calculate rotated coordinates of current pixel
	    XR = ( J-X)*COSD( ANG) - (K-Y)*SIND( ANG)
	    YR = ( J-X)*SIND( ANG) + (K-Y)*COSD( ANG)

*          Calculate radius vector from centre to current pixel
	    R = SQRT( XR**2 + YR**2/( 1-ECC**2))

*           Test to see if radius is in annulus
	    IF( R .GE. RAD1 .AND. R .LT. RAD2) THEN

*            Test to see if using bad pixels and pixel is not bad
	      IF( USEBAD .EQ. .TRUE. .AND. 
     :	          INARR( J, K) .NE. BADVAL) THEN

*              Increment number of pixels found
	        NPTS = NPTS + 1

*              Stuff pixel into nag work array
	        DATARR( NPTS) = DBLE( INARR( J, K))

*            Test to see if using bad pixels and pixel is bad
	      ELSE IF( USEBAD .EQ. .TRUE. .AND. 
     :	               INARR( J, K) .EQ. BADVAL) THEN

*              Increment number of bad pixel found
	        BNPTS = BNPTS + 1

*            Test to see if not using bad pixels 
	      ELSE IF( USEBAD .NE. .TRUE.) THEN

*              Increment number of good pixels found
	        NPTS = NPTS + 1

*              Stuff pixel into nag work array
	        DATARR( NPTS) = DBLE( INARR( J, K))

	      END IF
	    END IF
	  END DO
	END DO

!	type *, 'npts = ', npts

*      Sort the pixel values in each stack image
	IF( NPTS .GT. 0) THEN

*        Set nag status variable and call nag routine for sorting
	  IFAIL = 0
	  CALL M01CAF( DATARR, 1, NPTS, 'A', IFAIL)
 
*        Call subroutine to find median for the input DATARR
	  CALL MED3D_CALMEDSUB( NPTS, DATARR, VALMAX, VALMIN, SUM, MEAN, 
     :	                        MEDIAN, MODE)

*        Scan through pixels in annulus and calculate standard deviation
	  SUMSQ = 0.0D0
	  DO J = 1, NPTS
	    SUMSQ = SUMSQ + DATARR( J)**2
	  END DO

*        Test for overflows of single precision variable
	  IF( ABS( SUM) .LT. 1.0E20) THEN

*          calculate std over pixels
	    STD = SUMSQ/NPTS - ( SUM/NPTS)**2
	    IF( STD .GE. 0.0D0) THEN
	      STD = SQRT( SNGL( STD))
	    ELSE
	     STD = -999.0
	    END IF
	  ELSE
	    STD = -999.0
	  END IF

*      Here is no pixels are in specified annulus
	ELSE

*        Set output variables to zero
	  VALMAX = -999
	  VALMIN = -999
	  SUM = -999
	  MEAN = -999
	  MEDIAN = -999
	  MODE = -999
	  STD = -999

	END IF

*      Re-scale radii from pixels to arcseconds
	RAD1 = RAD1*PLATSCAL
	RAD2 = RAD2*PLATSCAL

!	type *, 'median, npts = ', median, npts

	END
