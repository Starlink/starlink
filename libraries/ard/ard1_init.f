      BLOCK DATA ARD1_INIT
*+
*  Name:
*     ARD1_INIT

*  Purpose:
*     Initialise the ARD common blocks.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     BLOCK DATA

*  Description:
*     Information describing the available operators, keywords and
*     statements are stored in common arrays.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     22-FEB-1994 (DSB):
*        Original version
*     18-JUL-2001 (DSB):
*        Modified for ARD version 2.0.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'ARD_CONST'        ! ARD private constants.

*  Global Variables:
      INCLUDE 'ARD_COM'          ! ARD common blocks.

*  Local Constants:
      INTEGER AST__NULL
      PARAMETER ( AST__NULL = 0 )

*  Local Variables:
      INTEGER I                  ! Implicit loop count.

*  Global Data:
*  Operator symbols used within ARD expressions, and the corresponding
*  integer codes used to represent them internally.
      DATA ( CMN_OPSYM( I ), CMN_OPLEN( I ), CMN_OPCOD( I ),
     :       I = 1, ARD__NOPSY )/

     :    '.AND.', 5, ARD__AND,
     :    '.OR. ', 4, ARD__OR,
     :    '.XOR.', 5, ARD__XOR,
     :    '.EQV.', 5, ARD__EQV,
     :    '.NOT.', 5, ARD__NOT,
     :    '-    ', 1, ARD__NOT,
     :    ')    ', 1, ARD__CLS,
     :    '(    ', 1, ARD__OPN/


*  Properties of each instruction...
*  "AND"...
      DATA CMN_OPL( ARD__AND ), CMN_OPR( ARD__AND ),
     :     CMN_PRL( ARD__AND ), CMN_PRR( ARD__AND ),
     :     CMN_DSTK( ARD__AND ), CMN_WRT( ARD__AND ),
     :     CMN_OPS( ARD__AND ), CMN_INSTR( ARD__AND ),
     :     CMN_NARG( ARD__AND )

     :     /1,  1,  5,  5, -1,  1,  2, 'AND', 0 /

*  "OR"...
      DATA CMN_OPL( ARD__OR ), CMN_OPR( ARD__OR ),
     :     CMN_PRL( ARD__OR ), CMN_PRR( ARD__OR ),
     :     CMN_DSTK( ARD__OR ), CMN_WRT( ARD__OR ),
     :     CMN_OPS( ARD__OR ), CMN_INSTR( ARD__OR ),
     :     CMN_NARG( ARD__OR )

     :     /1,  1,  4,  4, -1,  1,  2, 'OR', 0 /

*  "XOR"...
      DATA CMN_OPL( ARD__XOR ), CMN_OPR( ARD__XOR ),
     :     CMN_PRL( ARD__XOR ), CMN_PRR( ARD__XOR ),
     :     CMN_DSTK( ARD__XOR ), CMN_WRT( ARD__XOR ),
     :     CMN_OPS( ARD__XOR ), CMN_INSTR( ARD__XOR ),
     :     CMN_NARG( ARD__XOR )

     :     /1,  1,  3,  3, -1,  1,  2, 'XOR', 0 /

*  "EQV"...
      DATA CMN_OPL( ARD__EQV ), CMN_OPR( ARD__EQV ),
     :     CMN_PRL( ARD__EQV ), CMN_PRR( ARD__EQV ),
     :     CMN_DSTK( ARD__EQV ), CMN_WRT( ARD__EQV ),
     :     CMN_OPS( ARD__EQV ), CMN_INSTR( ARD__EQV ),
     :     CMN_NARG( ARD__EQV )

     :     /1,  1,  3,  3, -1,  1,  2, 'EQV', 0 /

*  "NOT"...
      DATA CMN_OPL( ARD__NOT ), CMN_OPR( ARD__NOT ),
     :     CMN_PRL( ARD__NOT ), CMN_PRR( ARD__NOT ),
     :     CMN_DSTK( ARD__NOT ), CMN_WRT( ARD__NOT ),
     :     CMN_OPS( ARD__NOT ), CMN_INSTR( ARD__NOT ),
     :     CMN_NARG( ARD__NOT )

     :     /0,  1,  8,  7,  0,  1,  1, 'NOT', 0 /

*  "Close brackets"...
      DATA CMN_OPL( ARD__CLS ), CMN_OPR( ARD__CLS ),
     :     CMN_PRL( ARD__CLS ), CMN_PRR( ARD__CLS ),
     :     CMN_DSTK( ARD__CLS ), CMN_WRT( ARD__CLS ),
     :     CMN_OPS( ARD__CLS ), CMN_INSTR( ARD__CLS ),
     :     CMN_NARG( ARD__CLS )

     :     /1,  0,  2, 10,  0,  0,  0, ')', 0 /

*  "Open brackets"...
      DATA CMN_OPL( ARD__OPN ), CMN_OPR( ARD__OPN ),
     :     CMN_PRL( ARD__OPN ), CMN_PRR( ARD__OPN ),
     :     CMN_DSTK( ARD__OPN ), CMN_WRT( ARD__OPN ),
     :     CMN_OPS( ARD__OPN ), CMN_INSTR( ARD__OPN ),
     :     CMN_NARG( ARD__OPN )

     :     /0,  1, 10,  1,  0,  0,  0, '(', 0 /

*  "End expression"
      DATA CMN_OPL( ARD__END ), CMN_OPR( ARD__END ),
     :     CMN_PRL( ARD__END ), CMN_PRR( ARD__END ),
     :     CMN_DSTK( ARD__END ), CMN_WRT( ARD__END ),
     :     CMN_OPS( ARD__END ), CMN_INSTR( ARD__END ),
     :     CMN_NARG( ARD__END )

     :     /1,  1,  0,  0, 0,  0,  1, '"End Expression"', 0 /

*  "Load Supplied Mask"...
      DATA CMN_OPL( ARD__LSM ), CMN_OPR( ARD__LSM ),
     :     CMN_PRL( ARD__LSM ), CMN_PRR( ARD__LSM ),
     :     CMN_DSTK( ARD__LSM ), CMN_WRT( ARD__LSM ),
     :     CMN_OPS( ARD__LSM ), CMN_INSTR( ARD__LSM ),
     :     CMN_NARG( ARD__LSM )

     :     /0,  0, 10, 10,  1,  1,  0, '"Load Supplied Mask"', 0 /

*  "Load Keyword Region"...
      DATA CMN_OPL( ARD__LKR ), CMN_OPR( ARD__LKR ),
     :     CMN_PRL( ARD__LKR ), CMN_PRR( ARD__LKR ),
     :     CMN_DSTK( ARD__LKR ), CMN_WRT( ARD__LKR ),
     :     CMN_OPS( ARD__LKR ), CMN_INSTR( ARD__LKR ),
     :     CMN_NARG( ARD__LKR )

     :     /0,  0, 10, 10,  1,  1,  0, '"Load Keyword Region"', 1 /

*  "Null" (i.e. do nothing)...
      DATA CMN_OPL( ARD__NUL ), CMN_OPR( ARD__NUL ),
     :     CMN_PRL( ARD__NUL ), CMN_PRR( ARD__NUL ),
     :     CMN_DSTK( ARD__NUL ), CMN_WRT( ARD__NUL ),
     :     CMN_OPS( ARD__NUL ), CMN_INSTR( ARD__NUL ),
     :     CMN_NARG( ARD__NUL )

     :     /0,  0,  0,  0,  0,  0,  0, '"Null"', 0 /

*  Keyword symbols.
      DATA CMN_KWSYM( ARD__POI ) /'POINT'/
      DATA CMN_KWSYM( ARD__PIX ) /'PIXEL'/
      DATA CMN_KWSYM( ARD__LIN ) /'LINE'/
      DATA CMN_KWSYM( ARD__ROW ) /'ROW'/
      DATA CMN_KWSYM( ARD__COL ) /'COLUMN'/
      DATA CMN_KWSYM( ARD__BOX ) /'BOX'/
      DATA CMN_KWSYM( ARD__REC ) /'RECT'/
      DATA CMN_KWSYM( ARD__ROT ) /'ROTBOX'/
      DATA CMN_KWSYM( ARD__POL ) /'POLYGON'/
      DATA CMN_KWSYM( ARD__CIR ) /'CIRCLE'/
      DATA CMN_KWSYM( ARD__ELL ) /'ELLIPSE'/
      DATA CMN_KWSYM( ARD__FRA ) /'FRAME'/
      DATA CMN_KWSYM( ARD__WHO ) /'WHOLE'/
      DATA CMN_KWSYM( ARD__INP ) /'INPUT'/

*  Lengths of keyword symbols.
      DATA CMN_KWLEN( ARD__POI ) /5/
      DATA CMN_KWLEN( ARD__PIX ) /5/
      DATA CMN_KWLEN( ARD__LIN ) /4/
      DATA CMN_KWLEN( ARD__ROW ) /3/
      DATA CMN_KWLEN( ARD__COL ) /6/
      DATA CMN_KWLEN( ARD__BOX ) /3/
      DATA CMN_KWLEN( ARD__REC ) /4/
      DATA CMN_KWLEN( ARD__ROT ) /6/
      DATA CMN_KWLEN( ARD__POL ) /7/
      DATA CMN_KWLEN( ARD__CIR ) /6/
      DATA CMN_KWLEN( ARD__ELL ) /7/
      DATA CMN_KWLEN( ARD__FRA ) /5/
      DATA CMN_KWLEN( ARD__WHO ) /5/
      DATA CMN_KWLEN( ARD__INP ) /5/

*  The symbols used to represent each statement.
      DATA CMN_STSYM( ARD__DIM ) /'DIMENSION'/
      DATA CMN_STSYM( ARD__COE ) /'COEFFS'/
      DATA CMN_STSYM( ARD__OFF ) /'OFFSET'/
      DATA CMN_STSYM( ARD__SCA ) /'SCALE'/
      DATA CMN_STSYM( ARD__TWI ) /'TWIST'/
      DATA CMN_STSYM( ARD__STR ) /'STRETCH'/
      DATA CMN_STSYM( ARD__WCS ) /'WCS'/
      DATA CMN_STSYM( ARD__COF ) /'COFRAME'/

*  The actual length of each statement.
      DATA CMN_STLEN( ARD__DIM ) /9/
      DATA CMN_STLEN( ARD__COE ) /6/
      DATA CMN_STLEN( ARD__OFF ) /6/
      DATA CMN_STLEN( ARD__SCA ) /5/
      DATA CMN_STLEN( ARD__TWI ) /5/
      DATA CMN_STLEN( ARD__STR ) /7/
      DATA CMN_STLEN( ARD__WCS ) /3/
      DATA CMN_STLEN( ARD__COF ) /7/

*  The number of arguments taken by each statement. -1 is used if
*  the number of arguments is variable.
      DATA CMN_STARG( ARD__DIM ) /1/
      DATA CMN_STARG( ARD__COE ) /-1/
      DATA CMN_STARG( ARD__OFF ) /-1/
      DATA CMN_STARG( ARD__SCA ) /1/
      DATA CMN_STARG( ARD__TWI ) /1/
      DATA CMN_STARG( ARD__STR ) /-1/
      DATA CMN_STARG( ARD__WCS ) /-1/
      DATA CMN_STARG( ARD__COF ) /-1/

*  The number of arguments taken by each keyword. -1 is used if
*  the number of arguments is variable.
      DATA CMN_KWARG( ARD__POI ) /-1/
      DATA CMN_KWARG( ARD__PIX ) /-1/
      DATA CMN_KWARG( ARD__LIN ) /-1/
      DATA CMN_KWARG( ARD__ROW ) /-1/
      DATA CMN_KWARG( ARD__COL ) /-1/
      DATA CMN_KWARG( ARD__BOX ) /-1/
      DATA CMN_KWARG( ARD__REC ) /-1/
      DATA CMN_KWARG( ARD__ROT ) /5/
      DATA CMN_KWARG( ARD__POL ) /-1/
      DATA CMN_KWARG( ARD__CIR ) /-1/
      DATA CMN_KWARG( ARD__ELL ) /5/
      DATA CMN_KWARG( ARD__FRA ) /1/
      DATA CMN_KWARG( ARD__WHO ) /0/
      DATA CMN_KWARG( ARD__INP ) /0/

*  Use zero to indicate that no memory has yet been allocated to store
*  a positions list.
      DATA CMN_PSET /0/

*  No Application FrameSet is avalable as yet.
      DATA CMN_AWCS /AST__NULL/

*  Allow linear Mappings.
      DATA CMN_LINOK /.TRUE./


      END
