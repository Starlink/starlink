      BLOCK DATA CTRLB
*+
*  Name:
*     CTRLB
*  Purpose:
*     Block data statement for common block CTRL.
*  Language:
*     Fortran 77.
*  Type of Module:
*     BLOCK DATA
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     13/8/93 (ACD):  Original version.
*     16/9/93 (ACD):  First stable version.
*     9/3/95  (ACD):  Added ANGCV__CAT1.
*     17/8/99 (ACD):  removed PARSE__CAT1.
*     14/10/99 (ACD): Added QUIET__CAT1.
*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
*  Global Constants:
      INCLUDE 'CAT_PAR'
      INCLUDE 'CAT1_PAR'
*  Global Variables:
      INCLUDE 'CAT1_CTRL_CMN'
*  Global Data:
      DATA ANGCV__CAT1/.TRUE./,  QUIET__CAT1/.FALSE./
*.
      END
