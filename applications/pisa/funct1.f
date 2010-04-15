      SUBROUTINE FUNCT1( N, XC, RMS )
*+
*  Name:
*     FUNCT1

*  Purpose:
*     Returns the RMS of the PISAFIT fitting function to the data to the
*     calling NAG routine E04JAF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FUNCT1( N, XC, RMS )

*  Description:
*     The routine uses the parameters passed from the calling NAG
*     routine to estimate the goodness of fit to the data. The form
*     which the fitting function takes is the PISA mixed
*     gaussian-lorentzian-exponential. The actual data to which the
*     function is fitted is passed from the routine which calls the NAG
*     minimisation in common. Added functionality; the standard
*     deviations ( actually standard error - the best estimate of the
*     standard deviation ) of the data values are now passed and the
*     RMS is weighted by the deviations squared. According to
*
*          rms = sum of all ( rms / sigma**2 )
*                ----------------------------
*                 sum of all ( 1 / sigma**2 )
*     The use of this facility is controlled by a logical flag.

*  Arguments:
*     N = INTEGER (Given)
*        Number of values in XC ( = 3 )
*     XC( 3 ) = DOUBLE PRECISION (Given)
*        The real model variables as passed by the calling minimisation
*        routine. XC( 1 ) = GSIGM, XC( 2 ) = CROSS, XC( 3 ) = COMIX.
*     RMS = DOUBLE PRECISION (Returned)
*        The possibly weighted root mean squared deviation of the data
*        points to the present model fit.

*  Notes:
*     No status return is possible with this routine

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-OCT-1990 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

* Local constants
      INTEGER FITSIZ
      PARAMETER ( FITSIZ = 256 )
      DOUBLE PRECISION NOTZER
      PARAMETER ( NOTZER = 1.0D-10)
      DOUBLE PRECISION ZERO
      PARAMETER ( ZERO = 0.0D0 )
      DOUBLE PRECISION LOG2
      PARAMETER ( LOG2 = 0.6931471806D0 )

* Arguments Given:
      INTEGER N
      DOUBLE PRECISION XC( N )
      DOUBLE PRECISION RMS

* Local variables:
      INTEGER I
      DOUBLE PRECISION XC1SQ
      DOUBLE PRECISION CHANGE
      DOUBLE PRECISION COEF1
      DOUBLE PRECISION COEF2
      DOUBLE PRECISION RADTHR
      DOUBLE PRECISION FGAUSS
      DOUBLE PRECISION FLOR
      DOUBLE PRECISION FEXP
      DOUBLE PRECISION FIT
      DOUBLE PRECISION DDAT,CDAT
      DOUBLE PRECISION INVSD
      DOUBLE PRECISION INVSUM

* Global variables
      INTEGER NPOINT
      REAL XDAT, XCOR
      DOUBLE PRECISION SD
      LOGICAL NOTWEI
      COMMON /COMMUN/ SD( FITSIZ ), XDAT( FITSIZ ), XCOR( FITSIZ ),
     :                NPOINT, NOTWEI

*  Xdat contains the data to be fitted and xcor the radii associated
*  with the data points, npoint is the number of points (less than equal
*  to FITSIZ) which are to be used in the fit. SD contains the standard
*  errors, NOTWEI controls whether weighting occurs or not

*.

* derive the present model parameters, note hopefully bomb proof nature
      CHANGE = LOG ( MAX( NOTZER, XC( 2 ) ) )
      XC1SQ = ( MAX( NOTZER, XC( 1 ) ) )**2
      COEF1 = -1.0D0 / XC1SQ
      COEF2 = SQRT( MAX( ZERO, ( -4.0D0 * CHANGE / XC1SQ ) ) )
      RADTHR = XC( 1 ) * SQRT( MAX( ZERO, -CHANGE ))
      RMS = ZERO
      INVSUM = 0.0D0
*
* do for all points in fit
      DO 870 I = 1, NPOINT
         DDAT = DBLE( XDAT( I ))
         CDAT = DBLE( XCOR( I ))
         IF( DDAT .GE. CHANGE ) THEN
*
* above threshold for change from exponential to gaussian ( core part )
* so calculate lorentzian and gaussian part
            FGAUSS = ( 1.0D0 - XC( 3 ) ) * EXP( COEF1 * CDAT**2 )
            FLOR = XC( 3 ) / (1.0D0 - COEF1 * CDAT**2 / LOG2 )
            FIT = LOG10( MAX( NOTZER, FGAUSS + FLOR ))
         ELSE
*
* below threshold intensity; lorentzian and exponential part
     	   FEXP =( 1.0D0-XC( 3 ))*EXP( CHANGE + ( RADTHR - CDAT)*COEF2 )
    	   FLOR = XC( 3 )/( 1.0D0 - COEF1 * CDAT**2 / LOG2 )
    	   FIT = LOG10( MAX( NOTZER, FEXP + FLOR ))
         END IF
*
* estimate the goodness of fit, weight the fit if required
         IF ( NOTWEI ) THEN
               RMS = RMS + ( FIT - DDAT ) ** 2
         ELSE
            IF ( SD( I ) .GT. 0.0D0 ) THEN
               INVSD = 1.0/ ( SD( I )* SD ( I ) )
               RMS = RMS + ( FIT - DDAT)**2 * INVSD
               INVSUM = INVSUM + INVSD
            END IF
         END IF
  870 CONTINUE
*
* normalise the rms, if it has been weighted
      IF ( .NOT. NOTWEI ) THEN
         IF ( INVSUM .GT. 0.0D0 ) RMS = RMS / DBLE( INVSUM )
      END IF
      END
* $Id$
