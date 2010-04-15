      SUBROUTINE SPD_WZPKD( LMAX, CLM, CRSL, NEEDC, STATUS )
*+
*  Name:
*     SPD_WZPK{DR}

*  Purpose:
*     Add up matrix rows.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZPKD( LMAX, CLM, CRSL, NEEDC, STATUS )

*  Description:
*     This routine adds up the rows of a given matrix and stores the
*     results in a vector. If the given matrix is found to be diagonal
*     or to contain only bad and zero values, this is signalled by a
*     logical switch.
*
*     The result vector component is never set bad. If the sum along
*     the row turns out to be zero, either the data value is perfect, or
*     the data value is bad. In the first case the covariance row sum
*     must be zero as well, in the second case the value set here does
*     not matter.

*  Arguments:
*     LMAX = INTEGER (Given)
*        The length of matrix columns and rows. Also the length of the
*        result vector.
*     CLM( LMAX, LMAX ) = DOUBLE PRECISION (Given)
*        The given matrix.
*     CRSL( LMAX ) = DOUBLE PRECISION (Returned)
*        The vector of sums of matrix rows.
*     NEEDC = LOGICAL (Returned)
*        False if the vector of row sums does not contain more
*        information than the matrix diagonal.
*        As long as the matrix contained any off-diagonal, non-zero,
*        non-bad elements, this logical will be true.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     14 Feb 1992 (hme):
*        Original version.
*     26 Jan 1995 (hme):
*        Renamed from SPAAKx.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants

*  Arguments Given:
      INTEGER LMAX
      DOUBLE PRECISION CLM( LMAX, LMAX )

*  Arguments Returned:
      DOUBLE PRECISION CRSL( LMAX )
      LOGICAL NEEDC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER L                  ! Index counting rows
      INTEGER M                  ! Index counting row elements
      DOUBLE PRECISION SUMM                ! Sum over the row

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise switch that indicates usefulness of the result.
      NEEDC = .FALSE.

*  Loop through rows (l).
      DO 2 L = 1, LMAX

*     Initialise sum.
         SUMM  = 0.

*     Loop through row elements (m).
         DO 1 M = 1, LMAX
            IF ( CLM(M,L) .NE. VAL__BADD ) THEN
               SUMM  = SUMM  + CLM(M,L)
               IF ( M .NE. L .AND. CLM(M,L) .NE. 0. ) NEEDC = .TRUE.
            END IF
 1       CONTINUE

*     Store the sum.
         CRSL(L) = SUMM
 2    CONTINUE

*  Return.
      END
