      SUBROUTINE NDF_BEGIN
*+
*  Name:
*     NDF_BEGIN

*  Purpose:
*     Begin a new NDF context.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_BEGIN

*  Description:
*     The routine begins a new NDF context. A subsequent call to
*     NDF_END may then be used to annul all the NDF identifiers (and
*     placeholders) issued since the call to NDF_BEGIN was made.

*  Notes:
*     Matching pairs of calls to NDF_BEGIN and NDF_END may be nested.

*  Algorithm:
*     -  Increment the current identifier context value.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-SEP-1989 (RFWS):
*        Original version.
*     10-OCT-1989 (RFWS):
*        Removed all the arguments, which are no longer needed.
*     17-OCT-1989 (RFWS):
*        Improved the prologue description.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCTX = INTEGER (Read and Write)
*           Current identifier context level.

*.

*  Increment the current identifier context level.
      ACB_IDCTX = ACB_IDCTX + 1

      END
