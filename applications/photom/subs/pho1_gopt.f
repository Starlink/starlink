      SUBROUTINE PHO1_GOPT( MAGS, XCEN, YCEN, ORIGIN, LPADU, LSTAR,
     :                      LVSTAR, LSKY, LSIGMA, LSKYMG, CLIP,
     :                      SEE, CODE, LETIME, BUFFER, STATUS )

*+
*  Name :
*     PHO1_GOPT
*
*  Purpose :
*     Calculates photometry measurements for optimal photometry
*
*  Language :
*     Starlink Fortran-77
*
*  Invocation:
*     CALL PHO1_GOPT( MAGS, XCEN, YCEN, ORIGIN, LPADU, LSTAR, LVSTAR,
*    :                LSKY, LSIGMA, LSKYMG, CLIP, SEE, CODE, LETIME,
*    :                BUFFER, STATUS )
*
*  Description :
*     Calculates the final measurements for an aperture and writes the
*     results into a string buffer.
*
*  Arguments :
*     {enter_new_arguments_here}
*
*  Authors :
*     AA: Alasdair Allan (STARLINK - Keele University)
*     MJC: Malcolm J. Currie (JAC)
*     {enter_new_authors_here}
*
*  History :
*     1-FEB-1999 (AA):
*        Original version based on PWD's PHO1_GCALC
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE definitions
      INCLUDE 'PRM_PAR'          ! VAL__ public constants

*  Arguments Given :
      LOGICAL MAGS
      REAL XCEN
      REAL YCEN
      INTEGER ORIGIN( 2 )
      REAL LPADU
      REAL LSTAR
      REAL LVSTAR
      REAL LSKY
      REAL LSIGMA
      REAL LSKYMG
      REAL CLIP
      REAL SEE
      CHARACTER * ( 2 ) CODE
      REAL LETIME
      CHARACTER * ( * ) BUFFER
      REAL CLIP, SEE

*  Status :
      INTEGER STATUS

*  Local Variables:
      LOGICAL ERFLAG             ! Local error flag
      DOUBLE PRECISION ERRMAG    ! Error in magnitude
      DOUBLE PRECISION ERRSIG    ! Error in aperture count
      DOUBLE PRECISION ETIME     ! Exposure time
      DOUBLE PRECISION FACTOR    ! Used in error in magnitude calcs
      DOUBLE PRECISION MAG       ! Magnitude of count in aperture
      DOUBLE PRECISION PADU      ! Photons per data unit
      DOUBLE PRECISION PSKY      ! Corrected sky value
      DOUBLE PRECISION SIGNAL    ! Total counts in aperture
      DOUBLE PRECISION SKY       ! Sky value per pixel
      DOUBLE PRECISION SKYMAG    ! Magnitude of sky
      CHARACTER*13 CERRMG        ! Error in mag/mean formatted in string
      CHARACTER*13 CMAG          ! Mag/mean value formatted in string
      CHARACTER*13 CSIG          ! Signal formatted in string
      CHARACTER*13 CSKY          ! Sky flux formatted in string
      CHARACTER*9 CXCEN          ! X centroid formatted in string
      CHARACTER*9 CYCEN          ! Y centroid formatted in string
      CHARACTER*9 MAGFMT         ! Format for magnitudes/mean count

*.

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Transform all input REAL variables into DOUBLE PRECISION for
*   better accuracy.
      PADU = DBLE( LPADU )
      STAR = DBLE( LSTAR )
      VSTAR = DBLE( LVSTAR )
      SKY = DBLE( LSKY )
      SKYMAG = DBLE( LSKYMG )
      ETIME = DBLE( LETIME )

*  Initialise the error flag.
      IF ( DBLE( LSIGMA ) .LT. 0.0D0 ) THEN
         ERFLAG = .TRUE.
      ELSE
         ERFLAG = .FALSE.
      ENDIF

*   Unlike the aperture routines, for optimal extraction we pre-calculate
*   the signal and error, but we still need to put the result into electrons
      SIGNAL = PADU*STAR
      ERRSIG = PADU*VSTAR

*   The magnitude error = (2.5/ln(10)) * (errsig/signal) which comes
*   from differentiating the magnitude calculation. Check that the
*   signal is not zero, otherwise set an error condition. If MAGS is
*   FALSE then we do not want a magnitude conversion.
      FACTOR = 0.0
      IF ( MAGS ) THEN
         IF ( ABS( SIGNAL ) .GT. VAL__SMLD ) THEN
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
      ENDIF

*   If there is no signal or an error has occured, give it a nominal value
      IF ( MAGS ) THEN
         IF ( ( FACTOR .GT. 1.0D0 ) .OR. ERFLAG ) THEN
            MAG = SKYMAG
            ERRMAG = 99.999D0
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

*  Counts may cover a wide range of values so use G format.
      IF ( MAGS ) THEN
         MAGFMT = '( F11.5 )'
      ELSE
         MAGFMT = '( G13.5 )'
      END IF

*  Mag
      WRITE( CMAG, MAGFMT ) MAG

*  Mag error
      WRITE( CERRMG, MAGFMT ) ERRMAG

*  Sky - use G format to cope with all values to at least five
*  significant figures.  Limit to 13 instead of 15 to squeeze in
*  an 80-character buffer, thus some values may be written in
*  exponential form.
      PSKY = SKY * PADU
      WRITE( CSKY, '( G13.5 )' ) PSKY

*  Signal - use G format to cope with all values to at least five
*  significant figures.
      WRITE( CSIG, '( G13.5 )' ) SIGNAL

*   Concatenate these into the output string.
      BUFFER = CXCEN//' '//CYCEN//' '//CMAG//' '//CERRMG//' '//CSKY//' '
     :         //CSIG//' '//' '//CODE
  99  CONTINUE

      END

