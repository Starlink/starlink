      BLOCK DATA ARY1_INIT
*+
*  Name:
*     ARY1_INIT

*  Purpose:
*     Initialise the ARY_ system common blocks.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     BLOCK DATA

*  Description:
*     The routine initialises global variables stored in the ARY_ system
*     common blocks.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1989 (RFWS):
*        Original version.
*     19-SEP-1989 (RFWS):
*        Added support for the Placeholder Control Block.
*     22-NOV-1989 (RFWS):
*        Added support for the error tracing flag in the TCB.
*     13-FEB-1990 (RFWS):
*        Changed default state of the error tracing flag to .FALSE..
*     12-DEC-1990 (RFWS):
*        Changed to initialise the ACB_IDCNT and PCB_PLCNT values using
*        the constant ARY__FACNO.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_USED( ARY__MXDCB ) = LOGICAL (Write)
*           Whether a DCB slot has been used.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCNT = INTEGER (Write)
*           Count of array identifiers issued.
*        ACB_USED( ARY__MXACB ) = LOGICAL (Write)
*           Whether an ACB slot has been used.

      INCLUDE 'ARY_MCB'          ! ARY_ Mapping Control Block
*        MCB_USED( ARY__MXMCB ) = LOGICAL (Write)
*           Whether an MCB slot has been used.

      INCLUDE 'ARY_PCB'          ! ARY_ Placeholder Control Block
*        PCB_PLCNT = INTEGER (Write)
*           Count of placeholders issued.
*        PCB_USED( ARY__MXPCB ) = LOGICAL (Write)
*           Whether a PCB slot has been used.

      INCLUDE 'ARY_TCB'          ! ARY_ Error Tracing Control Block
*        TCB_ETFLG = LOGICAL (Write)
*           Error tracing flag.

*  Global Data:
      DATA ACB_IDCNT / ARY__FACNO /
      DATA ACB_USED / ARY__MXACB * .FALSE. /
      DATA DCB_USED / ARY__MXDCB * .FALSE. /
      DATA MCB_USED / ARY__MXMCB * .FALSE. /
      DATA PCB_PLCNT / ARY__FACNO /
      DATA PCB_USED / ARY__MXPCB * .FALSE. /
      DATA TCB_ETFLG / .FALSE. /

*.

      END
