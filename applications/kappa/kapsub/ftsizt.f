      SUBROUTINE FTSIZT( INSIZ, OUTSIZ, STATUS )
*+
*  Name:
*     FTSIZT

*  Purpose:
*     Calculates a dimension of the Fourier transform by trimming.

*  Language:
*     Starlink Fortran 77

*  Invocation
*     CALL FTSIZT( INSIZ, OUTSIZ, STATUS )

*  Description:
*     Calculates the size of an array dimension which can be used by
*     the Fourier-transform routines. If the input value is acceptable
*     then it is returned as the output value. Otherwise the next
*     smaller acceptable size is returned.

*  Arguments:
*     INSIZ = INTEGER (Given)
*        The input axis size, i.e. the dimension of the input array
*        to be transformed elsewhere.
*     OUTSIZ = INTEGER (Returned)
*        The output axis size.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     - All dimensions are OK with the current FFTPACK routines, so
*       just return the supplied dimension.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 Mar 29 (MJC):
*        Original version based on David Berry's IMSIZE.
*     17-FEB-1995 (DSB):
*        Modified to use FFTPACK instead of NAG.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER
     :  INSIZ

*  Arguments Returned:
      INTEGER
     :  OUTSIZ

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Just return the supplied dimension.
      OUTSIZ = INSIZ

      END
