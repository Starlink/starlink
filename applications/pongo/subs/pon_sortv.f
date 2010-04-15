      SUBROUTINE PON_SORTV( NDATA, IRANK, WORK, ARRAY, STATUS )
*+
*  Name:
*     PON_ISORT

*  Purpose:
*     Re-arrange the given array into increasing order.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PON_SORTV( NDATA, IRANK, WORK, ARRAY, STATUS )

*  Description:
*     This routine uses the result of the 'heap-sort' routine,
*     PON_ISORT, to re-arrange the given array into the order given by
*     the contents of IRANK; i.e. if ARRAY was the array from which
*     IRANK was derived, it will be returned with its values re-arranged
*     into ascending order.

*  Arguments:
*     NDATA = INTEGER (Given)
*        The number of given data to sort.
*     IRANK( NDATA ) = INTEGER (Returned)
*        Array of rank (index) positions for each element of the given
*        array of data.
*     WORK( NDATA ) = DOUBLE PRECISION (Given and returned)
*        A work array to be used in the re-arrangement.
*     ARRAY( NDATA ) = DOUBLE PRECISION (Given and Returned)
*        A real array to be sorted into ascending order.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-DEC-1992 (PCTR):
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
      INTEGER NDATA
      INTEGER IRANK( NDATA )

*  Arguments Given and Returned:
      DOUBLE PRECISION WORK( NDATA )
      DOUBLE PRECISION ARRAY( NDATA )

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER IDX                ! Loop index

*.

*  Check the inherited global status.
      IF ( STATUS.NE.SAI__OK ) RETURN

*  Check the given value of NDATA.
      IF ( NDATA.EQ.1 ) THEN

*     Do nothing; i.e. nothing to sort.
         CONTINUE
      ELSE

*     Initialize the work array.
         DO 50 IDX = 1, NDATA
            WORK( IDX ) = ARRAY( IDX )
 50      CONTINUE

*     Re-arrange the given array, using the work array as the source.
         DO 100 IDX = 1, NDATA
            ARRAY( IDX ) = WORK( IRANK( IDX ) )
 100     CONTINUE
      END IF

      END
* $Id$
