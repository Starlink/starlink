************************************************************************

      SUBROUTINE OUTOPT ( FOUT, INDEX, XCEN, YCEN, ORIGIN, LPADU, LSTAR,
     :                    LAREA, LVSTAR, LSKY, LSKYARE, LSIGMA, LVSKY,
     :                    MAGS, LSKYMAG, PHOTON, LBIASLE, CODE, LETIME,
     :                    STATUS )

*+
*  Name :
*     OUTOPT
*
*  Purpose :
*     Output the results of the optimal-extraction measurement
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL OUTOPT ( FOUT, INDEX, XCEN, YCEN, ORIGIN, LPADU, LSTAR,
*                   LAREA, LVSTAR, LSKY, LSKYARE, LSIGMA, LVSKY,
*                   MAGS, LSKYMAG, PHOTON, LBIASLE, CODE, LETIME,
*                   STATUS )
*
*  Description :
*     Output the results of the optimal-extraction measurement.
*
*  Arguments :
*     FOUT = INTEGER (Given)
*        Identifier for results file used by FIO_
*     INDEX = INTEGER (Given)
*        Index number of object
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
*     MAGS = LOGICAL (Given)
*        If TRUE then results are converted into magnitudes
*     LSKYMAG = REAL (Given)
*        Magnitude of sky
*     PHOTON = INTEGER (Given)
*        Errors from photon noise, sky or data variance, gaussian sky
*     LBIASLE = REAL (Given)
*        Zero point for photon noise calculation per pixel
*     CODE = CHARACTER (Given)
*        Error code flag
*     LETIME = REAL (Given)
*        Exposure time
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     PWD: Peter W. Draper (Durham University)
*     AA: Alasdair Allan (Starlink, Keele University)
*     EUS: Eduardo Unda-Sanzana
*     MJC: Malcolm J. Currie (JAC)
*     {enter_new_authors_here}
*
*  History :
*     25-JAN-1999 (AA)
*         Original version, heavily based on OUTRES
*     21-FEB-2008 (PWD):
*        Stop using internal writes to copy constant strings.
*     2011-12-06 (EUS):
*        Use 5 decimal places for MAG and MAGERR.
*     2011 December 22 (MJC):
*        Use G-format for counts mean and its error, and for sky and
*        signal.  Increase the format width for magnitudes.
*     {enter_further_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants :
      INCLUDE 'SAE_PAR'


*  Arguments Given :
      INTEGER FOUT
      INTEGER INDEX
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
      LOGICAL MAGS
      REAL LSKYMAG
      INTEGER PHOTON
      REAL LBIASLE
      REAL A
      REAL E
      REAL THETA
      CHARACTER * ( 2 ) CODE
      REAL LETIME

*  Status :
      INTEGER STATUS

*  Local Variables :
      LOGICAL ERFLAG

      DOUBLE PRECISION ERRMAG, ERRSIG, ERR2, FACTOR, MAG, PSKY, SIGNAL
      DOUBLE PRECISION PADU, STAR, AREA, VSTAR, SKY, SKYARE, SIGMA,
     :                 VSKY, SKYMAG, BIASLE, ETIME

      CHARACTER CINDEX * 5, CXCEN * 9, CYCEN * 9, CMAG * 13, CERRMG * 13
      CHARACTER CSKY * 15, CSIG * 15, CA * 5, CE * 5, CT * 5
      CHARACTER TEXT * 80
      CHARACTER MAGFMT * 9
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
      IF ( SIGMA .LT. 0.0 ) THEN
         ERFLAG = .TRUE.
      ELSE
         ERFLAG = .FALSE.
      ENDIF

*   Unlike the aperture routines, for optimal extraction we pre-calculate
*   the signal and error, but we still need to put the result into electrons
      SIGNAL = PADU *STAR
      ERRSIG = PADU *VSTAR

*   The magnitude error = (2.5/ln(10)) * (errsig/signal) which comes from
*   differentiating the magnitude calculation. Check that the signal is
*   not zero, otherwise set an error condition. If MAGS is
*   FALSE then we do not want a magnitude conversion.
      FACTOR = 0.0D0
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
         MAG = SIGNAL
	 ERRMAG = ERRSIG
      END IF

*   If there is no signal or an error has occurred, give it a nominal value
      IF ( MAGS ) THEN
         IF ( ( FACTOR .GT. 1.0D0 ) .OR. ERFLAG ) THEN
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

*   Code the output into seperate character strings
*   Index - limit to 5 characters
      WRITE( TEXT, '( I80 )' ) INDEX
      CINDEX = TEXT( 76:80 )

*   Xcen
      WRITE( CXCEN, '( F9.2 )' ) XCEN + REAL( ORIGIN( 1 ) - 1 )

*   Ycen
      WRITE( CYCEN, '( F9.2 )' ) YCEN + REAL( ORIGIN( 2 ) - 1 )

*   Counts may cover a wide range of values so use G format.
      IF ( MAGS ) THEN
         MAGFMT = '( F11.5 )'
      ELSE
         MAGFMT = '( G13.5 )'
      END IF

*   Mag
      WRITE( CMAG, MAGFMT ) MAG

*   Mag error
      WRITE( CERRMG, MAGFMT ) ERRMAG

*   Sky - use G format to cope with all values to at least five
*   significant figures.
      PSKY = SKY * PADU
      WRITE( CSKY, '( G15.5 )' ) PSKY

*   Signal - use G format to cope with all values to at least five
*   significant figures.
      WRITE( CSIG, '( G15.5 )' ) SIGNAL

*   Concatenate these into the output strings
      TEXT = CINDEX//CXCEN//CYCEN//CMAG//CERRMG//CSKY//CSIG//' '//CODE
      CALL MSG_OUT( ' ', TEXT, STATUS )
      CALL FIO_WRITE( FOUT, TEXT, STATUS )

      END

