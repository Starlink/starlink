      BLOCK DATA FIO_BLK
*+
*  Name:
*     FIO_BLK

*  Purpose:
*     FIO Block Data Initialisation

*  Language:
*     Starlink Fortran 77

*  Description:
*     Initialise the FIOGO_CMN Common Block to force activation
*     of FIO.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

*  Authors:
*     AJC: A.Chipperfield (Starlink, RAL)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     13-Oct-1986 (AJC):
*        Original
*     29-OCT-1991 (PMA):
*        Changed prologue to new style.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*     18-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*     31-JUL-2002 (AJC):
*        Split parameter system part to fiopa_blk
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'FIOGO_CMN'        ! FIO Initialisation Switches
*        FIOINT = LOGICAL (Returned)
*           Is package started?

*  Global data:
      DATA FIOINT /.FALSE./

*.

      END
