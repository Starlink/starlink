      SUBROUTINE PHO1_GCALC( MAGS, XCEN, YCEN, ORIGIN, LPADU, LSTAR,
     :                       LAREA, LVSTAR, LSKY, LSKYARE, LSIGMA,
     :                       LVSKY, LSKYMAG, PHOTON, LBIASLE, MAJOR,
     :                       ECCEN, ANGLE, CODE, LETIME, BUFFER,
     :                       STATUS )

*+
*  Name :
*     PHO1_CALC

*  Purpose :
*     Calculates photometry measurements.

*  Language :
*     Starlink Fortran-77

*  Invocation :
*     CALL PHO1_GCALC( MAGS, XCEN, YCEN, ORIGIN, LPADU, LSTAR,
*                      LAREA, LVSTAR, LSKY, LSKYARE, LSIGMA,
*                      LVSKY, LSKYMAG, PHOTON, LBIASLE, MAJOR,
*                      ECCEN, ANGLE, CODE, LETIME, BUFFER, STATUS )

*  Description :
*     Calculates the final measurements for an aperture and writes the
*     results into a string buffer.

*  Arguments :
*     MAGS = LOGICAL (Given)
*        If TRUE then the results are converted into
*        magnitudes. Otherwise the results are returned as data counts.
*     XCEN = REAL (Given)
*        Centre of aperture in pixel coordinates
*     YCEN = REAL (Given)
*        Centre of aperture in pixel coordinates
*     ORIGIN( 2 ) = INTEGER (Given)
*        Origin of NDF axes
*     LPADU = REAL (Given)
*        Photons per data unit
*     LSTAR = REAL (Given)
*        Integrated signal in object aperture
*     LAREA = REAL (Given)
*        Area of object aperture
*     LVSTAR = REAL (Given)
*        Data variance in object aperture
*     LSKY = REAL (Given)
*        Sky value per pixel
*     LSKYARE = REAL (Given)
*        Area of sky aperture
*     LSIGMA = REAL (Given)
*        Deviation in sky aperture
*     LVSKY = REAL (Given)
*        Data variance in sky aperture per pixel
*     LSKYMAG = REAL (Given)
*        Magnitude of sky
*     PHOTON = INTEGER (Given)
*        Errors from photon noise, sky or data variance, or gaussian sky
*     LBIASLE = REAL (Given)
*        Zero point for photon noise calculation per pixel
*     MAJOR = REAL (Given)
*        Semi-major axis of elliptical aperture
*     ECCEN = REAL (Given)
*        Eccentricity of elliptical aperture
*     ANGLE = REAL (Given)
*        Orientation anti-clockwise from x-axis
*     CODE = CHARACTER * ( 2 ) (Given)
*        Error code flag
*     LETIME = REAL (Given)
*        Exposure time
*     BUFFER = CHARACTER * ( * ) (Returned)
*        The aperture measurements formatted into a string. The output
*        contains the following fields:
*           X Y MAG MAGERR SKY SIGNAL CODE MAJOR ECCEN ANGLE
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors :
*     PWD: Peter W. Draper (STARLINK - Durham University)
*     AA: Alasdair Allan (STARLINK - Exeter University)
*     EUS: Eduardo Unda-Sanzana
*     {enter_new_authors_here}
*
*  History :
*     18-APR-1996 (PWD):
*        Original version based on NE's OUTRES.
*     5-NOV-1996 (PWD):
*        Added MAGS parameter and converted to not output magnitudes
*        when this is FALSE.
*     30-DEC-2000 (AA):
*        Bug fix, signal was corrected for exposure time, but the error wasn't
*     17-JUN-2010 (Andy Gibb, UBC):
*        Write sky/signal values <1e-2 in E11.4 format
*     2011-12-06 (EUS):
*        Use 5 decimal places for MAG and MAGERR
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants :
      INCLUDE 'SAE_PAR'

*  Arguments Given :
      LOGICAL MAGS
      REAL XCEN
      REAL YCEN
      INTEGER ORIGIN( 2 )
      REAL LPADU
      REAL LSTAR
      REAL LAREA
      REAL LVSTAR
      REAL LSKY
      REAL LSKYARE
      REAL LSIGMA
      REAL LVSKY
      REAL LSKYMAG
      INTEGER PHOTON
      REAL LBIASLE
      REAL MAJOR
      REAL ECCEN
      REAL ANGLE
      CHARACTER * ( 2 ) CODE
      REAL LETIME
      CHARACTER * ( * ) BUFFER

*  Status :
      INTEGER STATUS

*  Local Variables :
      LOGICAL ERFLAG            ! Local error flag
      DOUBLE PRECISION SIGNAL   ! Total counts in aperture
      DOUBLE PRECISION ERR2     ! Variance in aperture count
      DOUBLE PRECISION ERRSIG   ! Error in aperture count
      DOUBLE PRECISION MAG      ! Magnitude of count in aperture
      DOUBLE PRECISION FACTOR   ! Used in error in magnitude calcs
      DOUBLE PRECISION ERRMAG   ! Error in magnitude
      DOUBLE PRECISION PSKY     ! Corrected sky value
      CHARACTER * ( 9 ) CXCEN, CYCEN, CMAG, CERRMG
      CHARACTER * ( 5 ) CA, CE, CT
      CHARACTER * ( 11 ) CSKY, CSIG
      DOUBLE PRECISION PADU, STAR, AREA, VSTAR, SKY, SKYARE, SIGMA,
     :                 VSKY, SKYMAG, BIASLE, ETIME ! Local DBLE variables
*.

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Transform all input REAL variables into DOUBLE PRECISION for
*   better accuracy.
      PADU = DBLE( LPADU )
      STAR = DBLE( LSTAR )
      AREA = DBLE( LAREA )
      VSTAR = DBLE( LVSTAR )
      SKY = DBLE( LSKY )
      SKYARE = DBLE( LSKYARE )
      SIGMA = DBLE( LSIGMA )
      VSKY = DBLE( LVSKY )
      SKYMAG = DBLE( LSKYMAG )
      BIASLE = DBLE( LBIASLE )
      ETIME  = DBLE( LETIME )

*   Initialise the error flag
      IF ( SIGMA .LT. 0.0D0 ) THEN
         ERFLAG = .TRUE.
      ELSE
         ERFLAG = .FALSE.
      ENDIF

*   Calculate the signal in the object aperture and the error on that
*   signal. The error is calculated assuming that the noise is photon
*   noise. if the noise is to be calculated from the sky variance then
*   it is assumed that the deviation in the sky aperture is equal to the
*   photon noise in the sky aperture.
      SIGNAL = PADU * ( STAR - AREA * SKY )

*   Check that the skyare is not zero or negative otherwise flag an error
      ERR2 = 0.0
      IF ( SKYARE .GT. 1.0D-6 ) THEN

*   Error from photon statistics
         IF ( PHOTON .EQ. 1 ) THEN
            ERR2 = PADU * ( STAR - AREA * BIASLE +
     :             ( AREA ** 2 / SKYARE ) * ( SKY - BIASLE ) )

*   Or error from sky variance
         ELSEIF ( PHOTON .EQ. 2 ) THEN
            ERR2 = ABS( SIGNAL ) + ( PADU * SIGMA ) ** 2 *
     :             AREA * ( 1.0D0 + AREA / SKYARE )

*   Or error from data variance
         ELSEIF ( PHOTON .EQ. 3 ) THEN
            ERR2 = PADU ** 2 * ( VSTAR + VSKY * AREA ** 2 / SKYARE )

*   Or error from sky variance as a gaussian estimate.
         ELSEIF ( PHOTON .EQ. 4 ) THEN
            ERR2 = AREA * ( ( PADU * SIGMA ) ** 2 )

         ENDIF
      ELSE
         ERR2 = 0.0D0
         ERFLAG = .TRUE.
      ENDIF

*   Check that the square root can be taken
      ERRSIG = 0.0D0
      IF ( ERR2 .GT. 0.0D0 ) THEN
         ERRSIG = SQRT( ERR2 )

*   Else set the error to be larger than the signal to force an error
      ELSE
         ERFLAG = .TRUE.
      ENDIF

*   The magnitude error = (2.5/ln(10)) * (errsig/signal) which comes
*   from differentiating the magnitude calculation. Check that the
*   signal is not zero, otherwise set an error condition. If MAGS is
*   FALSE then we do not want a magnitude conversion.
      FACTOR = 0.0

      IF ( MAGS ) THEN
         IF ( ABS( SIGNAL ) .GT. 1.0D-6 ) THEN
            FACTOR = ABS( ERRSIG / SIGNAL )
            ERRMAG = 1.08574D0 * FACTOR
         ELSE
            ERFLAG = .TRUE.
         ENDIF
      ELSE
         ERRMAG = ERRSIG
      END IF

*   Scale the signal according to the exposure time.
      IF ( ETIME .GT. 0.0D0 ) THEN
         SIGNAL = SIGNAL / ETIME

*   And also the error, if not doing magnitude calculations.
         IF ( .NOT. MAGS ) THEN
            ERRMAG = ERRMAG / ETIME
         ENDIF

      ENDIF

*   Calculate the stars magnitude. A magnitude of skymag means that the
*   star and sky are the same brightness. If the signal is negative then
*   flag an error.
      IF ( MAGS ) THEN
         IF ( SIGNAL .GT. 0.0D0 ) THEN
            MAG = SKYMAG - 2.5D0 * LOG10( ABS( SIGNAL ) )
         ELSE
            ERFLAG = .TRUE.
         ENDIF
      ELSE

*   If not calculating the magnitude of the star, derive the mean
*   signal. Change the error too.
         IF ( AREA .GT. 0.0D0 ) THEN
            MAG = SIGNAL / AREA
            ERRMAG = ERRMAG / AREA
         ELSE
            ERFLAG = .TRUE.
         END IF
      END IF

*   If there is no signal or an error has occured, give it a nominal value
      IF ( MAGS ) THEN
         IF ( ( FACTOR .GT. 5.0D0 ) .OR. ERFLAG ) THEN
            MAG = SKYMAG
            ERRMAG = 99.99999D0
            CODE = '?'
         ENDIF
      ELSE
         IF ( ERFLAG ) THEN
            MAG = 0.0D0
            ERRMAG = 0.0D0
            CODE = '?'
         END IF
      END IF

*   Code the output into separate character strings
*   Xcen
      WRITE( CXCEN, '( F9.2 )' ) XCEN + REAL( ORIGIN( 1 ) - 1 )

*   Ycen
      WRITE( CYCEN, '( F9.2 )' ) YCEN + REAL( ORIGIN( 2 ) - 1 )

*   Mag
      WRITE( CMAG, '( F9.5 )' ) MAG

*   Mag error
      WRITE( CERRMG, '( F9.5 )' ) ERRMAG

*   Sky - use E format if number is too large or too small
      PSKY = SKY * PADU
      IF ( ( PSKY .GT. 1.0E6 ) .OR. ( PSKY .LT. -1.0E6 )
     :     .OR. ((PSKY .LT. 1.0E-2) .AND. (PSKY .GT. -1.0E-2)) ) THEN
         WRITE( CSKY, '( E11.4 )' ) PSKY
      ELSE
         WRITE( CSKY, '( F11.3 )' ) PSKY
      ENDIF

*   Signal - use E format if number is too large or too small
      IF ( ( SIGNAL .GT. 1.0E6 ) .OR. ( SIGNAL .LT. -1.0E6 )
     :    .OR. ((SIGNAL .LT. 1.0E-2) .AND. (SIGNAL .GT. -1.0E-2)) ) THEN
         WRITE( CSIG, '( E11.4 )' ) SIGNAL
      ELSE
         WRITE( CSIG, '( F11.3 )' ) SIGNAL
      ENDIF

*   Shape
      WRITE( CA, '( F5.1 )' ) MAJOR
      WRITE( CE, '( F5.2 )' ) ECCEN
      WRITE( CT, '( F5.0 )' ) ANGLE

*   Concatenate these into the output string.
      BUFFER = CXCEN//' '//CYCEN//' '//CMAG//' '//CERRMG//' '//CSKY//' '
     :         //CSIG//' '//' '//CODE//' '//CA//' '//CE//' '//CT
  99  CONTINUE

      END

