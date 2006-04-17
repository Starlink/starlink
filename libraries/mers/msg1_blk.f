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

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

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
