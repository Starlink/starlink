      BLOCK DATA ATTRBB
*+
*  Name:
*     ATTRBB
*  Purpose:
*     Block data statement for common block ATTRB.
*  Language:
*     Fortran 77.
*  Type of Module:
*     BLOCK DATA
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     13/8/93 (ACD): Original version.
*     16/9/93 (ACD): First stable version.
*     28/5/98 (ACD): Removed counts for individual data types.
*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
*  Global Constants:
      INCLUDE 'CAT_PAR'
      INCLUDE 'CAT1_PAR'
*  Global Variables:
      INCLUDE 'CAT1_ATTRB_CMN'
*  Global Data:
      DATA
     :  NATT__CAT1/0/
*.
      END
