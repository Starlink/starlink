      BLOCK DATA FIOPA_BLK
*+
*  Name:
*     FIOPA_BLK

*  Purpose:
*     FIO Block Data Initialisation

*  Language:
*     Starlink Fortran 77

*  Description:
*     Initialise the FIOGOPA_CMN Common Block to force activation
*     of FIO parameter system.

*  Copyright:
*     Copyright (C) 2002 CCLRC

*  Authors:
*     AJC: A.Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     31-JUL-2002 (AJC):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'FIOGOPA_CMN'        ! FIO Initialisation Switches
*        FIOSLP = LOGICAL (Returned)
*           Is package asleep?

*  Global data:
      DATA FIOSLP /.TRUE./

*.

      END
