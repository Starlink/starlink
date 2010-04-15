      SUBROUTINE SPD_UAAAR( BAD, NELM, ARRAY, MINVAL, MAXVAL, STATUS )
*+
*  Name:
*     SPD_UAAA{DR}

*  Purpose:
*     Get minimum and maximum from an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_UAAAR( BAD, NELM, ARRAY, MINVAL, MAXVAL, STATUS )

*  Description:
*     This routine returns the minimum and maximum found in an array. If
*     requested, the routine ignores bad values.

*  Arguments:
*     BAD = LOGICAL (Given)
*        If true, bad values are ignored.
*     NELM = INTEGER (Given)
*        Length of the array.
*     ARRAY( NELM ) = REAL (Given)
*        Array to be examined.
*     MINVAL = REAL (Returned)
*        Minimum value found. 0 if not found.
*     MAXVAL = REAL (Returned)
*        Maximum value found. 0 if not found.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set and an error report made if BAD
*        is given as true and ARRAY contains only bad values.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     18 Sep 1991 (hme):
*        Original version.
*     23 Jun 1993 (hme):
*        If array is all-bad, report error and return status.
*     05 May 1994 (hme):
*        Adapt from RANGER, make generic.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primdat constants

*  Arguments Given:
      LOGICAL BAD
      INTEGER NELM
      REAL ARRAY( NELM )

*  Arguments Returned:
      REAL MINVAL
      REAL MAXVAL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      LOGICAL FOUND              ! False while only bad found

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If checking for bad values needed.
      IF ( BAD ) THEN
         FOUND = .FALSE.
         DO 1 I = 1, NELM

*        Consider only non-bad values.
            IF ( ARRAY(I) .NE. VAL__BADR ) THEN

*           If first non-bad had been found before.
               IF ( FOUND ) THEN
                  MINVAL = MIN( MINVAL, ARRAY(I) )
                  MAXVAL = MAX( MAXVAL, ARRAY(I) )

*           Else (if first non-bad found).
               ELSE
                  MINVAL = ARRAY(I)
                  MAXVAL = ARRAY(I)
                  FOUND = .TRUE.
               END IF
            END IF
 1       CONTINUE

*     Return values in case of failure
         IF ( .NOT. FOUND ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPD_UAAA_E01', 'SPD_UAAA: Error finding ' //
     :         'extrema. Given array has only bad values.', STATUS )
            GO TO 500
         END IF

*  Else (if no bad values expected).
      ELSE
         MINVAL = ARRAY(1)
         MAXVAL = ARRAY(1)
         DO 2 I = 2, NELM
            MINVAL = MIN( MINVAL, ARRAY(I) )
            MAXVAL = MAX( MAXVAL, ARRAY(I) )
 2       CONTINUE
      END IF

 500  CONTINUE
      END
