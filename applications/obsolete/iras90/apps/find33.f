      SUBROUTINE FIND33( NOELEM, POSN, ARRAYC, VALUEC, STATUS )
*+
*  Name:
*     FIND33

*  Purpose:
*     To put the character value VALUEC in the character array
*     element ARRAYC(POSN)

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND33( NOELEM, POSN, ARRAYC, VALUEC, STATUS )

*  Description:
*     To put the value character value VALUEC in the character array
*     element ARRAYC(POSN)

*  Arguments:
*     NOELEM = INTEGER (Given)
*        Number of elements in ARRAYC
*     POSN = INTEGER (Given)
*        Position in array in which value is to be put
*     ARRAYC( NOELEM ) = CHARACTER * ( * ) (Given and Returned)
*        Array in which value is to be placed
*     VALUEC = CHARACTER * ( * ) (Given)
*        Value to be put in ARRAYC( POSN )
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     None

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     2-APR-1992 (DCP):
*        Original version.
*     20-MAY-1993 (DCP):
*        Order in which variables are passed to this subroutine was
*        altered to make it compatible with UNIX (see other IRPUT
*        routines for original order)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NOELEM
      INTEGER POSN
      CHARACTER * ( * ) VALUEC

*  Arguments Given and Returned:
      CHARACTER * ( * ) ARRAYC( NOELEM )

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Put the value VALUEC in the element ARRAYC( POSN )
      ARRAYC( POSN ) = VALUEC

      END
