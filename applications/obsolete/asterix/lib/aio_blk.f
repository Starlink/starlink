      BLOCK DATA AIO_BLK
*+
*  Name:
*     AIO_BLK

*  Purpose:
*     AIO Block Data Initialisation

*  Language:
*     Starlink Fortran 77

*  Description:
*     Initialise the AIO_CMN Common Block to force activation of AIO subsystem.

*  Authors:
*     DJA: David J. Allan (ROSAT,University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      6-May-1994 (DJA):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              	! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_PAR'

*  Global Variables:
      INCLUDE 'ASTLIB(AIO_CMN)'		! AIO Initialisation Switches
*        AIO_DEF = LOGICAL (Returned)
*           AIO channel defined?
*        AIO_WRAP = LOGICAL (Returned)
*           AIO line wrapping on?

*  Global data:
      DATA AIO_DEF/.FALSE./
      DATA AIO_WRAP/.TRUE./
*.

      END
