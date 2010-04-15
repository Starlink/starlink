      SUBROUTINE DSA2_NFILLF( NELM, ARRAY, LBND )
*+
*  Name:
*     DSA2_NFILLF

*  Purpose:
*     Fills an NDF axis-centre array with default values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA2_NFILLF( NELM, ARRAY, LBND )

*  Description:
*     This routine fills the given array with numbers that start at
*     LBND - 0.5 and increase by 1.0 with each index increment.

*  Arguments:
*     NELM = INTEGER (Given)
*        The size of the array.
*     ARRAY( NELM ) = REAL (Returned)
*        The array.
*     LBND = INTEGER (Given)
*        The lower bound of the NDF.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     01 Feb 1996 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER NELM
      INTEGER LBND

*  Arguments Returned:
      REAL ARRAY( NELM )

*  Local Variables:
      INTEGER I

*.

      DO 1 I = 1, NELM
         ARRAY(I) = FLOAT(I-LBND) + 0.5
 1    CONTINUE

      END
