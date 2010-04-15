      SUBROUTINE SPD_UAAN( LABEL, LBCODE, STATUS )
*+
*  Name:
*     SPD_UAAN

*  Purpose:
*     Evaluate label string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_UAAN( LABEL, LBCODE, STATUS )

*  Description:
*     This routine compares a given label string with a set of
*     recognised strings. It returns a code number for the
*     identification, or zero if the string could not be identified.

*  Arguments:
*     LABEL = CHARACTER * ( * ) (Given)
*        The label string to identify.
*     LBCODE = INTEGER (Returned)
*        The identification code. 0 indicates no identification. Codes
*        between 101 and 199 identify data labels. Codes 201 to 299
*        identify labels for the spectroscopic axis.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     17 Jun 1992 (hme):
*        Original version.
*     25 Nov 1994 (hme):
*        Renamed from SPACA.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) LABEL

*  Arguments Returned:
      INTEGER LBCODE

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:
      INTEGER CMPLEN             ! Declared length of valid labels
      PARAMETER ( CMPLEN = 22 )

*  Local Variables:
      INTEGER I                  ! Loop index
      CHARACTER * ( CMPLEN ) LOCLAB  ! Local copy of label
      CHARACTER * ( CMPLEN ) DLAB( 101 : 108 ) ! Valid data labels
      CHARACTER * ( CMPLEN ) SLAB( 201 : 205 ) ! Valid spectrosc. labels

*  Internal References:
      LOGICAL CHR_SIMLR          ! Tell if strings are similar
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Data:
      DATA DLAB /
     :   'intensity             ', 'lambda intensity      ',
     :   'flux density          ', 'lambda flux density   ',
     :   'AB magnitude          ', 'brightness temperature',
     :   'counts                ', 'count rate            ' /
      DATA SLAB /
     :   'frequency             ', 'particle energy       ',
     :   'wavelength            ', 'radial velocity       ',
     :   'redshift              ' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default value for unidentified labels.
      LBCODE = 0

*  If given label has more significant characters than the longest valid
*  label, then return with code 0.
      IF ( CHR_LEN( LABEL ) .GT. CMPLEN ) GOTO 500
      LOCLAB = LABEL(:CMPLEN)

*  Compare with valid data labels.
      DO 1 I = 101, 108
         IF ( CHR_SIMLR(LOCLAB,DLAB(I)) ) THEN
            LBCODE = I
            GO TO 500
         END IF
 1    CONTINUE

*  Compare with valid spectroscopic labels.
      DO 2 I = 201, 205
         IF ( CHR_SIMLR(LOCLAB,SLAB(I)) ) THEN
            LBCODE = I
            GO TO 500
         END IF
 2    CONTINUE

*  Return.
 500  CONTINUE
      END
