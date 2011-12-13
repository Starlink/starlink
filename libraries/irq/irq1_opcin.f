      BLOCK DATA IRQ1_OPCIN
*+
*  Name:
*     IRQ1_OPCIN

*  Purpose:
*     Initialise the arrays declared in module IRQ_CMO which hold
*     information about logical operators.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     BLOCK DATA

*  Description:
*     Information describing the properties of each supported logical
*     operator are defined by this module. This information should be
*     accessed by including module IRQ_OPC in the application code.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-MAY-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'IRQ_PAR'          ! General IRQ constants.
      INCLUDE 'IRQ_PAO'          ! Scalars describing the logical
                                 ! operators.

*  Global Variables:
      INCLUDE 'IRQ_CMO'          ! Variables used to store vector
                                 ! information about logical operators.
*  Local Variables:
      INTEGER I                  ! Implicit loop count.

*  Global Data:
      DATA ( OPC_SYM( I ), OPC_L( I ),   OPC_OPL( I ),  OPC_OPR( I ),
     :       OPC_PRL( I ), OPC_PRR( I ), OPC_DSTK( I ), OPC_WRT( I ),
     :       OPC_OPS( I ),      I = 1, IRQ__MXINS )/

     :    '       ', 0, 0, 0, 10, 10, 1, 1, 0,! Load single quality bit.
     :    '=      ', 1, 1, 1,  0,  0,-1, 0, 1,! End expression.
     :    '.FALSE.', 7, 0, 0, 10, 10, 1, 1, 0,! Load .false.
     :    '.TRUE. ', 6, 0, 0, 10, 10, 1, 1, 0,! Load .true.
     :    ')      ', 1, 1, 0,  2, 10, 0, 0, 0,! Close brackets.
     :    '(      ', 1, 0, 1, 10, 1,  0, 0, 0,! Open brackets.
     :    '.EQV.  ', 5, 1, 1,  3, 3, -1, 1, 2,! Equivalence operator.
     :    '.XOR.  ', 5, 1, 1,  3, 3, -1, 1, 2,! Exclusive OR operator.
     :    '.OR.   ', 4, 1, 1,  4, 4, -1, 1, 2,! OR operator.
     :    '.AND.  ', 5, 1, 1,  5, 5, -1, 1, 2,! AND operator.
     :    '.NOT.  ', 5, 0, 1,  8, 7,  0, 1, 1,! NOT operator
     :    '       ', 0, 0, 0,  0, 0,  1, 1, 0,! Load qualities with EQV.
     :    '       ', 0, 0, 0,  0, 0,  1, 1, 0,! Load qualities with XOR
     :    '       ', 0, 0, 0,  0, 0,  1, 1, 0,! Load qualities with OR.
     :    '       ', 0, 0, 0,  0, 0,  1, 1, 0,! Load qualities with AND.
     :    '       ', 0, 0, 0,  0, 0,  1, 1, 0,! Load quality with NOT.
     :    '       ', 0, 0, 0,  0, 0,  0, 0, 0/! Null operation.
*.

      END
