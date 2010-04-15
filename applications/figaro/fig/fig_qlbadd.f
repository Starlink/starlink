      SUBROUTINE FIG_QLBADD( NELM, BADXST, LQUAL, INDAT, OUTDAT,
     :   STATUS )
*+
*  Name:
*     FIG_QLBADD

*  Purpose:
*     Copy a data array considering array of logical bad flags.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIG_QLBADD( NELM, BADXST, LQUAL, INDAT, OUTDAT, STATUS )

*  Description:
*     This routine copies data from INDAT to OUTDAT (_DOUBLE arrays).
*     While doing so and if BADXST is true, it considers the logical
*     array LQUAL to set OUTDAT elements bad where LQUAL is false.

*  Arguments:
*     NELM = INTEGER (Given)
*        The length of arrays INDAT, OUTDAT, LQUAL.
*     BADXST = LOGICAL (Given)
*        True if and only if LQUAL contains elements that are .FALSE.
*     LQUAL( NELM ) = LOGICAL (Given)
*        Each element is .TRUE. if the corresponding INDAT element is to
*        be copied to OUTDAT. If an element of LQUAL is .FALSE. the
*        corresponding OUTDAT element is to remain unchanged.
*     INDAT( NELM ) = DOUBLE PRECISION (Given)
*        The given data array.
*     OUTDAT( NELM ) = DOUBLE PRECISION (Returned)
*        The returned data array. This is assumed to have been
*        initialised with bad values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     2-JUL-1991 (HME):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NELM
      LOGICAL BADXST
      LOGICAL LQUAL( NELM )
      DOUBLE PRECISION INDAT( NELM )

*  Arguments Returned:
      DOUBLE PRECISION OUTDAT( NELM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Must check while copying.
      IF ( BADXST ) THEN
         DO 1 I = 1, NELM
            IF ( LQUAL(I) ) OUTDAT(I) = INDAT(I)
 1       CONTINUE

*  Need not check while copying.
      ELSE
         DO 2 I = 1, NELM
            OUTDAT(I) = INDAT(I)
 2       CONTINUE
      END IF

      END
