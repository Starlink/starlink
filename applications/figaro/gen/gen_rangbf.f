      SUBROUTINE GEN_RANGBF( BAD, NELM, VECTOR,
     :   MINVAL, MAXVAL )
*+
*  Name:
*     GEN_RANGBF

*  Purpose:
*     Get minimum and maximum from an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GEN_RANGBF( BAD, NELM, VECTOR, MINVAL, MAXVAL )

*  Description:
*     This routine returns the minimum and maximum found in an array. If
*     requested, the routine ignores bad values.

*  Arguments:
*     BAD = LOGICAL (Given)
*        If true, bad values are ignored.
*     NELM = INTEGER (Given)
*        Length of the array.
*     VECTOR( NELM ) = REAL (Given)
*        Array to be examined.
*     MINVAL = REAL (Returned)
*        Minimum value found. 0 if not found.
*     MAXVAL = REAL (Returned)
*        Maximum value found. 0 if not found.

*  External Routines Used:
*     none.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     18-SEP-1991 (HME):
*        Original version (RANGER in Specdre).
*     10-MAR-1993 (HME):
*        Take status out, call it GEN_RANGBF.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'          ! Primdat constants

*  Arguments Given:
      LOGICAL BAD
      INTEGER NELM
      REAL VECTOR( NELM )

*  Arguments Returned:
      REAL MINVAL
      REAL MAXVAL

*  Local Variables:
      INTEGER I                  ! Loop variable
      LOGICAL FOUND              ! False while only bad found

*.

*  Return values in case of failure
      MINVAL = 0.
      MAXVAL = 0.

*  If checking for bad values needed.
      IF ( BAD ) THEN
         FOUND = .FALSE.
         DO 1 I = 1, NELM

*        Consider only non-bad values.
            IF ( VECTOR(I) .NE. VAL__BADR ) THEN

*           If first non-bad had been found before.
               IF ( FOUND ) THEN
                  MINVAL = MIN( MINVAL, VECTOR(I) )
                  MAXVAL = MAX( MAXVAL, VECTOR(I) )

*           Else (if first non-bad found).
               ELSE
                  MINVAL = VECTOR(I)
                  MAXVAL = VECTOR(I)
                  FOUND = .TRUE.
               END IF
            END IF
 1       CONTINUE

*  Else (if no bad values expected).
      ELSE
         MINVAL = VECTOR(1)
         MAXVAL = VECTOR(1)
         DO 2 I = 2, NELM
            MINVAL = MIN( MINVAL, VECTOR(I) )
            MAXVAL = MAX( MAXVAL, VECTOR(I) )
 2       CONTINUE
      END IF

      END
