      BLOCK DATA MSG1_BLK
*+
*  Name:
*     MSG1_BLK

*  Purpose:
*     Initial filtering level for conditional message output.

*  Language:
*     Starlink Fortran 77

*  Type of module:
*     BLOCK DATA

*  Description:
*     This routine initialises the MSG_CMN common blocks to perform 
*     the initialisation of the MSG_ conditional message output 
*     filtering.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-JUN-1991 (PCTR):
*        Original version.
*     21-JUL-1999 (AJC):
*        Added MSGWSZ and MSGSTM
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'MSG_PAR'                 ! MSG_ public constants
      INCLUDE 'MSG_SYS'                 ! MSG_ system constants

*  Global Variables:
      INCLUDE 'MSG_CMN'                 ! MSG_ informational filtering etc.

*  Global Data:
      DATA MSGINF / MSG__NORM /
      DATA MSGWSZ / MSG__SZOUT /
      DATA MSGSTM / .FALSE. /

*.
 
      END
