      SUBROUTINE PSA1_FITFN( M, N, XC, FVEC, IFLAG )
*+
*  Name:
*     PSA_FITFN

*  Purpose:
*     Returns the difference of the PISAFIT model to the data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PSA1_FITFN( M, N, XC, FVEC, IFLAG )

*  Description:
*     The routine uses the parameters passed from the calling PDA
*     routine to derive the difference of the current model to the
*     stellar data. The form which the fitting function takes is the
*     PISA mixed gaussian-lorentzian-exponential. The actual data to
*     which the function is fitted is passed from the routine which
*     calls the PDA minimisation routine in common. The standard
*     deviations (actually standard error - the best estimate of the
*     standard deviation) of the data values can be passed and the
*     differences are weighted by these.

*  Arguments:
*     M = INTEGER (Given)
*        The number of functions to be evaluated == 1 for this routine.
*     N = INTEGER (Given)
*        Number of values in XC ( = 3 )
*     XC( 3 ) = DOUBLE PRECISION (Given)
*        The real model variables as passed by the calling minimisation
*        routine. XC( 1 ) = GSIGM, XC( 2 ) = CROSS, XC( 3 ) = COMIX.
*     FVEC( M )  = DOUBLE PRECISION (Returned)
*        The possibly weighted differences of the data points to the
*        present model fit.
*     IFLAG = INTEGER (Given and Returned)
*        Set this to a negative value to abort minimization.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-AUG-1996 (PDRAPER):
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
      DOUBLE PRECISION LOG2
      PARAMETER ( LOG2 = 0.6931471806D0 )

* Arguments Given:
      INTEGER M
      INTEGER N
      DOUBLE PRECISION XC( N )
      DOUBLE PRECISION FVEC( M )
      INTEGER IFLAG

* Global variables
      REAL XDAT, XCOR           ! Data values to fit
      INTEGER NPOINT            ! Number of points in XDAT and XCOR
      DOUBLE PRECISION SD       ! The weights if available
      LOGICAL NOTWEI            ! Whether to use weights or not
      REAL MINMIX, MAXMIX       ! Minimum and maximum values that
                                ! mixture fraction can take
      REAL MINSIG, MAXSIG       ! Maximum and minimum values that
                                ! gaussian sigma can take
      REAL MINTHR, MAXTHR       ! Maximum and minimum values that
                                ! fractional threshold can take
      COMMON /PSA1_FITCM/ SD( FITSIZ ), XDAT( FITSIZ ), XCOR( FITSIZ ),
     :                    NPOINT, MINMIX, MAXMIX, MINSIG, MAXSIG,
     :                    MINTHR, MAXTHR, NOTWEI

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

*.

*  Apply limits to model parameters (probably not quite best way to do
*  this, minimization algorithm with built in limits would be better).
      XC( 1 ) = MAX( MIN( XC( 1 ), DBLE( MAXSIG ) ), DBLE( MINSIG ) )
      XC( 2 ) = MAX( MIN( XC( 2 ), DBLE( MAXTHR ) ), DBLE( MINTHR ) )
      XC( 3 ) = MAX( MIN( XC( 3 ), DBLE( MAXMIX ) ), DBLE( MINMIX ) )

*  Derive the present model parameters, note hopefully bomb proof nature.
      CHANGE = LOG ( MAX( NOTZER, XC( 2 ) ) )
      XC1SQ = ( MAX( NOTZER, XC( 1 ) ) )**2
      COEF1 = -1.0D0 / XC1SQ
      COEF2 = SQRT( MAX( 0.0D0, ( -4.0D0 * CHANGE / XC1SQ ) ) )
      RADTHR = XC( 1 ) * SQRT( MAX( 0.0D0, -CHANGE ))

*  Do for all points in fit.
      DO 1 I = 1, M
         DDAT = DBLE( XDAT( I ))
         CDAT = DBLE( XCOR( I ))
         IF( DDAT .GE. CHANGE ) THEN

*  Above threshold for change from exponential to gaussian ( core part )
*  so calculate lorentzian and gaussian part.
            FGAUSS = ( 1.0D0 - XC( 3 ) ) * EXP( COEF1 * CDAT**2 )
            FLOR = XC( 3 ) / (1.0D0 - COEF1 * CDAT**2 / LOG2 )
            FIT = LOG10( MAX( NOTZER, FGAUSS + FLOR ))
         ELSE

*  Below threshold intensity; lorentzian and exponential part
            FEXP =( 1.0D0-XC( 3 ))*EXP( CHANGE + (RADTHR - CDAT)*COEF2 )
            FLOR = XC( 3 )/( 1.0D0 - COEF1 * CDAT**2 / LOG2 )
            FIT = LOG10( MAX( NOTZER, FEXP + FLOR ))
         END IF

*  Estimate the goodness of fit, weight the fit if required.
         FVEC( I ) = FIT - DDAT
         IF ( .NOT. NOTWEI ) THEN
            IF ( SD( I ) .GT. 0.0D0 ) THEN
               FVEC( I ) = FVEC( I ) / SD( I )
            ELSE
               FVEC( I ) = 0.0D0
            END IF
         END IF
 1    CONTINUE
      END
* $Id$
