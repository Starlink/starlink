      BLOCK DATA ERR1_BLK
*+
*  Name:
*     ERR1_BLK

*  Purpose:
*     Initial error reporting output file status.

*  Language:
*     Starlink Fortran 77

*  Type of module:
*     BLOCK DATA

*  Description:
*     This routine initialises the ERR_CMN common blocks.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-JUN-1991 (PCTR):
*        Original version.
*     21-JUL-1999 (AJC):
*        Added Tuning parameters
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'ERR_SYS'                 ! ERR_ private constants

*  Global Variables:
      INCLUDE 'ERR_CMN'                 ! ERR_ global variables

*  Global Data:
      DATA ERROPN / ERR__NEWFL /
      DATA ERRBEL / ERR__NOBEL /
      DATA ERRWSZ / ERR__SZOUT /
      DATA ERRRVL / .FALSE. /
      DATA ERRSTM / .FALSE. /

*.
 
      END
