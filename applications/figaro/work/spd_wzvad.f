      SUBROUTINE SPD_WZVAD( BADBAD, NEGBAD, ZERBAD, NELM,
     :   BAD, NEG, ZERO, ARRAY, STATUS )
*+
*  Name:
*     SPD_WZVA{DR}

*  Purpose:
*     Replace bad, negative, zero values in an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZVAD( BADBAD, NEGBAD, ZERBAD, NELM, BAD, NEG, ZERO,
*        ARRAY, STATUS )

*  Description:
*     This routine replaces bad, negative, and zero values in an array
*     by values specified.

*  Arguments:
*     BADBAD = LOGICAL (Given)
*        True if bad values are to be kept.
*     NEGBAD = LOGICAL (Given)
*        True if negative values are to be made bad.
*     ZERBAD = LOGICAL (Given)
*        True if zeroes are to be made bad.
*     NELM = INTEGER (Given)
*        Length of array.
*     BAD = DOUBLE PRECISION (Given)
*        Value to replace bad values.
*     NEG = DOUBLE PRECISION (Given and Returned)
*        Value to replace negative values.
*     ZERO = DOUBLE PRECISION (Given and Returned)
*        Value to replace zeroes.
*     ARRAY( NELM ) = DOUBLE PRECISION (Given and Returned)
*        The array to be cleaned from undesired values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     none.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     16 Jul 1991 (hme):
*        Original version.
*     14 May 1993 (hme):
*        Fix bug and use VAL__BADD instead of VAL__BADR.
*     25 Nov 1994 (hme):
*        Renamed from GVRDOD.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad values

*  Arguments Given:
      LOGICAL BADBAD
      LOGICAL NEGBAD
      LOGICAL ZERBAD
      INTEGER NELM
      DOUBLE PRECISION BAD

*  Arguments Given and Returned:
      DOUBLE PRECISION NEG
      DOUBLE PRECISION ZERO
      DOUBLE PRECISION ARRAY( NELM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Remove the negatives.
      IF ( NEGBAD ) NEG = VAL__BADD
      DO 2 I = 1, NELM
         IF ( ARRAY(I) .LT. 0. .AND. ARRAY(I) .NE. VAL__BADD )
     :      ARRAY(I) = NEG
 2    CONTINUE

*  Remove the noughties.
      IF ( ZERBAD ) ZERO = VAL__BADD
      DO 3 I = 1, NELM
         IF ( ARRAY(I) .EQ. 0. ) ARRAY(I) = ZERO
 3    CONTINUE

*  Remove the badies.
      IF ( .NOT. BADBAD ) THEN
         DO 1 I = 1, NELM
            IF ( ARRAY(I) .EQ. VAL__BADD ) ARRAY(I) = BAD
 1       CONTINUE
      END IF

      END
