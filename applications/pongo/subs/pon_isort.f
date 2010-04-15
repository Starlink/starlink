      SUBROUTINE PON_ISORT( NDATA, ARRAY, IRANK, STATUS )
*+
*  Name:
*     PON_ISORT

*  Purpose:
*     Sort the given array into increasing order.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PON_ISORT( NDATA, ARRAY, IRANK, STATUS )

*  Description:
*     This routine uses a 'heap-sort' to generate a rank array to be
*     used in re-ordering the data in the array. The approach of not
*     sorting the given data array allows more than one array to be
*     re-ordered efficiently.

*  Arguments:
*     NDATA = INTEGER (Given)
*        The number of given data to sort.
*     ARRAY( NDATA ) = DOUBLE PRECISION (Given)
*        A real array to be sorted into ascending order. This array
*        remains unchanged.
*     IRANK( NDATA ) = INTEGER (Returned)
*        Array of rank (index) positions for each element of the given
*        array of data.
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
      DOUBLE PRECISION ARRAY( NDATA )

*  Arguments Returned:
      INTEGER IRANK( NDATA )

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER IDX                ! Loop index
      INTEGER IELEM              ! Array element index
      INTEGER IRNK               ! Rank element index
      INTEGER ISIFT              ! Sift index
      INTEGER IS1                ! Temporary sift index
      INTEGER IS2                ! Temporary sift index

      DOUBLE PRECISION ELEM      ! Array element

*.

*  Check the inherited global status.
      IF ( STATUS.NE.SAI__OK ) RETURN

*  Check the given value of NDATA.
      IF ( NDATA.EQ.1 ) THEN

*     Do nothing; i.e. nothing to sort.
         CONTINUE
      ELSE

*     Initialize the returned array.
         DO 50 IDX = 1, NDATA
            IRANK( IDX ) = IDX
 50      CONTINUE

*     Initialise the sorting indices.
         ISIFT = NDATA/2 + 1
         IELEM = NDATA

*     Loop to perform the 'heap sort'.
 100     CONTINUE
            IF ( ISIFT.GT.1 ) THEN
               ISIFT = ISIFT - 1
               IRNK = IRANK( ISIFT )
               ELEM = ARRAY( IRNK )
            ELSE
               IRNK = IRANK( IELEM )
               ELEM = ARRAY( IRNK )
               IRANK( IELEM ) = IRANK( 1 )
               IELEM = IELEM - 1

*           Test for only two elements in the given array.
               IF ( IELEM.EQ.1 ) THEN
                  IRANK( 1 ) = IRNK
                  GO TO 999
               END IF
            END IF

*        Now sift ELEM to its correct place.
            IS1 = ISIFT
            IS2 = ISIFT*2

*        DO WHILE loop.
 150        CONTINUE
            IF ( IS2.LE.IELEM ) THEN
               IF ( IS2.LT.IELEM ) THEN
                  IF ( ARRAY( IRANK( IS2 ) )
     :                 .LT.ARRAY( IRANK( IS2+1 ) ) ) IS2 = IS2 + 1
               END IF

            IF ( ELEM.LT.ARRAY( IRANK( IS2 ) ) ) THEN
               IRANK( IS1 ) = IRANK( IS2 )
               IS1 = IS2
               IS2 = IS2*2
            ELSE
               IS2 = IELEM + 1
            END IF
            GO TO 150
         END IF

*     Assign ELEM to its correct place.
         IRANK( IS1 ) = IRNK
         GO TO 100
      END IF

*  Exit point.
 999  CONTINUE

      END
* $Id$
