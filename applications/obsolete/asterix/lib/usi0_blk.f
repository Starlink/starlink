      BLOCK DATA USI0_BLK
*+
*  Name:
*     USI0_BLK

*  Purpose:
*     USI Block Data Initialisation

*  Language:
*     Starlink Fortran 77

*  Description:
*     Initialise the USI_CMN Common Block to force initialisation

*  Authors:
*     DJA: David J. Allan (JET-X,University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     17-Aug-1994 (DJA):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              	! No implicit typing

*  Global Variables:
      INCLUDE 'USI_CMN'			! USI initialisation

*.

      DATA USI_NPS/0/
      DATA USI_ICTX/0/
      DATA USI_SYINIT/.FALSE./

      END
