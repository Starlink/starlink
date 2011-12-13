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
*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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
