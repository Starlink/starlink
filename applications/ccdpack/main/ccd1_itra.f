      SUBROUTINE CCD1_ITRA( PVALS, X, Y, NDEC, NRECS, NPAR, INSXY,
     :                      ARRAY, STATUS )
*+
*  Name:
*     CCD1_ITRA

*  Purpose:
*     Inserts parameter values and X, Y values into transformation
*     array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_ITRA( PVALS, X, Y, NDEC, NRECS, NPAR, INSXY, ARRAY,
*                     STATUS )

*  Description:
*     This routine inserts the values PVALS into the whole of the
*     corresponding record of ARRAY. It also optionally inserts the
*     values in the X,Y arrays at the end of the array.

*  Arguments:
*     PVALS( NPAR - 2 ) = DOUBLE PRECISION (Given)
*        The values to be repeatably inserted along the records of
*        the array ARRAY.
*     X( NDEC ) = DOUBLE PRECISION (Given)
*        Optional X values to be copied into ARRAY after the PVALS.
*     Y( NDEC ) = DOUBLE PRECISION (Given)
*        Optional Y values to be copied into ARRAY after the PVALS.
*     NDEC = INTEGER (Given)
*        The declared first dimension of X and Y.
*     NRECS = INTEGER (Given)
*        The first dimension of ARRAY and number of values to
*        transfer from XY.
*     NPAR = INTEGER (Given)
*        The first dimension plus 2 of PVALS (leaving room for XY
*        values) and the second dimension of ARRAY.
*     INSXY = LOGICAL (Given)
*        Whether or not the XY values are to be copied into ARRAY.
*     ARRAY( NRECS, NPAR ) = DOUBLE PRECISION (Returned)
*        Array to be filled with the PVALS values (one value from
*        PVAL inserted into every element along a row), and optional
*        followed by the XY array values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Specialised routine used in general image registration.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-JUL-1992 (PDRAPER):
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
      INTEGER NPAR
      INTEGER NDEC
      INTEGER NRECS
      DOUBLE PRECISION PVALS( NPAR - 2 )
      DOUBLE PRECISION X( NDEC )
      DOUBLE PRECISION Y( NDEC )
      LOGICAL INSXY

*  Arguments Returned:
      DOUBLE PRECISION ARRAY( NRECS, NPAR )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, J               ! Loop variables

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Insert PVALS into ARRAY by repeatably copying the value into the
*  corresponding rows.
      DO 1 J = 1, NPAR - 2
         DO 2 I = 1, NRECS
            ARRAY( I, J ) = PVALS( J )
 2       CONTINUE
 1    CONTINUE

*  Append X,Y values if asked.
      IF ( INSXY ) THEN 
         DO 3 I = 1, NRECS
               ARRAY( I, NPAR - 1 ) = X( I )
               ARRAY( I, NPAR ) = Y( I )
 3       CONTINUE
      END IF
      END
* $Id$
