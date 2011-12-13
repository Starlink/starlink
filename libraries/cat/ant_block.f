      BLOCK DATA ANT_XBLOCK
*+
*  Name:
*     ANT_XBLOCK

*  Purpose:
*     Block data to set function/operator names/characteristics

*  Description:
*     To add a new function:
*      (1) Edit ANTX1.INC to increase the value of MAXFUN
*      (2) Add a statement below to define the values of FNAME, FPREC, FARGS
*          and note the array index which is the same as the label used below.
*      (3) Edit module ANT_XEVAL as follows:
*          (a) add the new label to the computed GOTO at the top,
*          (b) add a labelled section to execute the function at the bottom.
*      (4) Recompile all modified subroutines and those which access the
*          include file.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 1996, 2000 Central Laboratory of the
*     Research Councils. All Rights Reserved.

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
*     CGP: Clive Page
*     ACD: A C Davenhall (Edinburgh)

*  History:
*     1993-JUL-29 (CGP):
*        Original Version
*     1995-JAN-26 (ACD):
*        Added GREAT.
*     1996-AUG-15 (ACD):
*        Added SCALE.
*     2000-JUN-13 (ACD):
*        Added PANGLE.
*-

*  Global variables:
      INCLUDE 'antx1.inc'

*  Local Variables:
      INTEGER I

*.

* FNAME is the function or operator, must be upper-case up to 6 chars long.
* FPREC is the operator/function precedence, =30 for all functions.
* FARGS: is a string of (N+1) chars for a function with N arguments, coded
* as follows:
* First char (output):
*   C=character, L=logical, N=numeric, F=floating (real/dble),
*   B=byte, H=halfint, I=integer, R=real, D=double.
* Subsequent chars (one for each input argument):
*   C=char, L=logical, N=numeric, X=char/numeric, *=any type.
* Note that X invokes special code to check all args same type.
*opcodes 1 to 5 used only during compilation - run-time meaning differs.
      DATA FNAME(1)/ '*START'/, FPREC(1)/  2 /, FARGS(1)/ ' ' /
      DATA FNAME(2)/ '*END'  /, FPREC(2)/  2 /, FARGS(2)/ ' ' /
      DATA FNAME(3)/ '('     /, FPREC(3)/  4 /, FARGS(3)/ ' ' /
      DATA FNAME(4)/ ')'     /, FPREC(4)/  4 /, FARGS(4)/ ' ' /
      DATA FNAME(5)/ ','     /, FPREC(5)/  6 /, FARGS(5)/ ' ' /
*opcodes in range 6 to 29 must be left unused.
      DATA (FNAME(I),I=6,29)/ 24*' ' /
      DATA FNAME(30)/'EQV   '/, FPREC(30)/8 /, FARGS(30)/'LLL   '/
      DATA FNAME(31)/'NEQV  '/, FPREC(31)/8 /, FARGS(31)/'LLL   '/
      DATA FNAME(32)/'OR    '/, FPREC(32)/10/, FARGS(32)/'LLL   '/
      DATA FNAME(33)/'|     '/, FPREC(33)/10/, FARGS(33)/'LLL   '/
      DATA FNAME(34)/'AND   '/, FPREC(34)/12/, FARGS(34)/'LLL   '/
      DATA FNAME(35)/'&     '/, FPREC(35)/12/, FARGS(35)/'LLL   '/
      DATA FNAME(36)/'NOT   '/, FPREC(36)/14/, FARGS(36)/'LL    '/
      DATA FNAME(37)/'#     '/, FPREC(37)/14/, FARGS(37)/'LL    '/
      DATA FNAME(38)/'EQ    '/, FPREC(38)/16/, FARGS(38)/'LXX   '/
      DATA FNAME(39)/'==    '/, FPREC(39)/16/, FARGS(39)/'LXX   '/
      DATA FNAME(40)/'GE    '/, FPREC(40)/16/, FARGS(40)/'LXX   '/
      DATA FNAME(41)/'>=    '/, FPREC(41)/16/, FARGS(41)/'LXX   '/
      DATA FNAME(42)/'GT    '/, FPREC(42)/16/, FARGS(42)/'LXX   '/
      DATA FNAME(43)/'>     '/, FPREC(43)/16/, FARGS(43)/'LXX   '/
      DATA FNAME(44)/'LE    '/, FPREC(44)/16/, FARGS(44)/'LXX   '/
      DATA FNAME(45)/'<=    '/, FPREC(45)/16/, FARGS(45)/'LXX   '/
      DATA FNAME(46)/'LT    '/, FPREC(46)/16/, FARGS(46)/'LXX   '/
      DATA FNAME(47)/'<     '/, FPREC(47)/16/, FARGS(47)/'LXX   '/
      DATA FNAME(48)/'NE    '/, FPREC(48)/16/, FARGS(48)/'LXX   '/
      DATA FNAME(49)/'/=    '/, FPREC(49)/16/, FARGS(49)/'LXX   '/
      DATA FNAME(50)/'LIKE  '/, FPREC(50)/16/, FARGS(50)/'LXX   '/
      DATA FNAME(51)/'=     '/, FPREC(51)/16/, FARGS(51)/'LXX   '/
      DATA FNAME(52)/'FROM  '/, FPREC(52)/16/, FARGS(52)/'LXXX  '/
      DATA FNAME(53)/'TO    '/, FPREC(53)/18/, FARGS(53)/'LXXX  '/
      DATA FNAME(54)/'//    '/, FPREC(54)/20/, FARGS(54)/'CCC   '/
      DATA FNAME(55)/'+     '/, FPREC(55)/22/, FARGS(55)/'NNN   '/
      DATA FNAME(56)/'-     '/, FPREC(56)/22/, FARGS(56)/'NNN   '/
      DATA FNAME(57)/'UNARY-'/, FPREC(57)/24/, FARGS(57)/'NN    '/
      DATA FNAME(58)/'*     '/, FPREC(58)/26/, FARGS(58)/'NNN   '/
      DATA FNAME(59)/'/     '/, FPREC(59)/26/, FARGS(59)/'FNN   '/
      DATA FNAME(60)/'**    '/, FPREC(60)/28/, FARGS(60)/'FNN   '/
      DATA FNAME(61)/'BYTE  '/, FPREC(61)/30/, FARGS(61)/'BN    '/
      DATA FNAME(62)/'HALF  '/, FPREC(62)/30/, FARGS(62)/'HN    '/
      DATA FNAME(63)/'INT   '/, FPREC(63)/30/, FARGS(63)/'IN    '/
      DATA FNAME(64)/'NINT  '/, FPREC(64)/30/, FARGS(64)/'IN    '/
      DATA FNAME(65)/'REAL  '/, FPREC(65)/30/, FARGS(65)/'RN    '/
      DATA FNAME(66)/'DBLE  '/, FPREC(66)/30/, FARGS(66)/'DN    '/
      DATA FNAME(67)/'MIN   '/, FPREC(67)/30/, FARGS(67)/'NNN   '/
      DATA FNAME(68)/'MAX   '/, FPREC(68)/30/, FARGS(68)/'NNN   '/
      DATA FNAME(69)/'MOD   '/, FPREC(69)/30/, FARGS(69)/'NNN   '/
      DATA FNAME(70)/'ABS   '/, FPREC(70)/30/, FARGS(70)/'NN    '/
      DATA FNAME(71)/'SQRT  '/, FPREC(71)/30/, FARGS(71)/'FN    '/
      DATA FNAME(72)/'LOG   '/, FPREC(72)/30/, FARGS(72)/'FN    '/
      DATA FNAME(73)/'LOG10 '/, FPREC(73)/30/, FARGS(73)/'FN    '/
      DATA FNAME(74)/'EXP   '/, FPREC(74)/30/, FARGS(74)/'FN    '/
      DATA FNAME(75)/'SIN   '/, FPREC(75)/30/, FARGS(75)/'FN    '/
      DATA FNAME(76)/'COS   '/, FPREC(76)/30/, FARGS(76)/'FN    '/
      DATA FNAME(77)/'TAN   '/, FPREC(77)/30/, FARGS(77)/'FN    '/
      DATA FNAME(78)/'ASIN  '/, FPREC(78)/30/, FARGS(78)/'FN    '/
      DATA FNAME(79)/'ACOS  '/, FPREC(79)/30/, FARGS(79)/'FN    '/
      DATA FNAME(80)/'ATAN  '/, FPREC(80)/30/, FARGS(80)/'FN    '/
      DATA FNAME(81)/'ATAN2 '/, FPREC(81)/30/, FARGS(81)/'FNN   '/
      DATA FNAME(82)/'IAND  '/, FPREC(82)/30/, FARGS(82)/'INN   '/
      DATA FNAME(83)/'IOR   '/, FPREC(83)/30/, FARGS(83)/'INN   '/
      DATA FNAME(84)/'IXOR  '/, FPREC(84)/30/, FARGS(84)/'INN   '/
      DATA FNAME(85)/'DTOR  '/, FPREC(85)/30/, FARGS(85)/'FN    '/
      DATA FNAME(86)/'RTOD  '/, FPREC(86)/30/, FARGS(86)/'FN    '/
      DATA FNAME(87)/'UPCASE'/, FPREC(87)/30/, FARGS(87)/'CC    '/
      DATA FNAME(88)/'STRIP '/, FPREC(88)/30/, FARGS(88)/'CC    '/
      DATA FNAME(89)/'SUBSTR'/, FPREC(89)/30/, FARGS(89)/'CCNN  '/
      DATA FNAME(90)/'SCAN  '/, FPREC(90)/30/, FARGS(90)/'LCC   '/
      DATA FNAME(91)/'NULL  '/, FPREC(91)/30/, FARGS(91)/'L*    '/
      DATA FNAME(92)/'HMSRAD'/, FPREC(92)/30/, FARGS(92)/'DNNN  '/
      DATA FNAME(93)/'DMSRAD'/, FPREC(93)/30/, FARGS(93)/'DCNNN '/
      DATA FNAME(94)/'GREAT '/, FPREC(94)/30/, FARGS(94)/'DNNNN '/
      DATA FNAME(95)/'SCALE '/, FPREC(95)/30/, FARGS(95)/'DNNNNN'/
      DATA FNAME(96)/'PANGLE'/, FPREC(96)/30/, FARGS(96)/'DNNNN '/
      END
