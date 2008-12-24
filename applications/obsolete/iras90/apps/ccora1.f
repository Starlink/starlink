      SUBROUTINE CCORA1( EL, T, O, CC, NU, OUT, BAD, STATUS )
*+
*  Name:
*     CCORA1

*  Purpose:
*     Generate colour corrected surface brightness values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCORA1( EL, T, O, CC, NU, OUT, BAD, STATUS )

*  Description:
*     The colour corrected surface brightness is the product of the
*     optical depth (assumed very small) and the Planck function, and
*     the supplied constant CC. 

*  Arguments:
*     EL = INTEGER (Given)
*        No. of elements in the images.
*     T( EL ) = REAL (Given)
*        Temperature values, in Kelvin.
*     O( EL ) = REAL (Given)
*        Optical depth values, in units of 1.0E-16.
*     CC = DOUBLE PRECISION (Given)
*        A constant dependant on the required wavelength, the
*        supplied TAU image wavelength, and beta.
*     NU = DOUBLE PRECISION (Given)
*        The central frequency of the required waveband, in units of
*        1.0E12 Hz.
*     OUT( EL ) = REAL (Returned)
*        The surface brightness values, in MJy/sr.
*     BAD = LOGICAL (Returned)
*        True if any bad output values generated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-MAY-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants.
      INCLUDE 'MSG_PAR'          ! MSG_ constants.

*  Arguments Given:
      INTEGER EL
      REAL T( EL )
      REAL O( EL )
      DOUBLE PRECISION CC
      DOUBLE PRECISION NU

*  Arguments Returned:
      REAL OUT( EL )
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL LARVAL                ! Largest optical depth value which is
      PARAMETER ( LARVAL = 0.001 )! considered "thin".

*  Local Variables:
      DOUBLE PRECISION
     :       B,                  ! Planck function value.
     :       DVAL                ! Dummy argument.

      INTEGER
     :       I                   ! Loop count.

      LOGICAL
     :       LARGE               ! True if large optical depths found.

      REAL
     :       OO,                 ! Optical depth value
     :       TT                  ! Temperature value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise a flag to indiocate that no particularly large optical
*  depths have been encountered.
      LARGE = .FALSE.

*  Initialise the bad pixel flag.
      BAD = .FALSE.

*  Loop round each output pixel.
      DO I = 1, EL
         TT = T( I )
         OO = O( I )

*  Check that both input values are good.
         IF( TT .GT. 0 .AND. TT .NE. VAL__BADR .AND.
     :       OO .GT. 0 .AND. OO .NE. VAL__BADR ) THEN

*  Calculate the planck function.
            CALL CCORP0( NU, DBLE( TT ), .FALSE., B, DVAL )

*  Store the colour corrected surface brightness.
            OUT( I ) = CC*DBLE( OO )*B

*  Set a flag if large values of optical depth are encountered.
            IF( OO .GT. LARVAL*1.0E16 ) LARGE = .TRUE.

*  If any of the input values are bad, set this output pixel bad.
         ELSE
            OUT( I ) = VAL__BADR
            BAD = .TRUE.
         END IF

      END DO

*  Give a warning if any large optical depths were encountered.
      IF( LARGE ) THEN
         CALL MSG_BLANKIF( MSG__QUIET, STATUS )
         CALL MSG_SETR( 'L', LARVAL )
         CALL MSG_OUTIF( MSG__QUIET, 'CCORA1_MSG1',
     :   'WARNING: Large optical depths (> ^LARVAL) encountered',
     :   STATUS )
         CALL MSG_BLANKIF( MSG__QUIET, STATUS )
      END IF

      END
