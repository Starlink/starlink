      SUBROUTINE LEX_CMDSET
*+
*  Name:
*     name

*  Purpose:
*     LEX_CMDSET

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL LEX_CMDSET

*  Description:
*     Initialize the LEX parser for ADAM command line parsing

*  Algorithm:
*     Make entries in the parser state table to correspond to
*     the ADAM command line syntax

*  Implementation Deficiencies:
*     There should be a distinction between NUMBER states entered
*     from PAR and those entered from KEYPAR so that we know
*     whether to switch back to KAMBIG or AMBIG if the number
*     doesn't complete. For the moment, switch to AMBIG as it
*     always has done.

*  Copyright:
*     Copyright (C) 1987, 1989-1991, 1993 Science & Engineering
*     Research Council. Copyright (C) 2002 Central Laboratory of the
*     Research Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     Jeremy Bailey  (AAOEPP::JAB)  Jan 8 1987
*     {enter_new_authors_here}

*  History:
*     28-JAN-1987 (AAOEPP::JAB):
*        Don't stack state in KEYPAR
*     05-APR-1987 (AAOEPP::JAB):
*        Allow commas within brackets in AMBIG
*     02-JUL-1987: Allow non alphabetic characters in literals
*                     after keywords  (AAOEPP::JAB)
*     09-AUG-1989: Allow any legal literal as an array element
*               Also use symbolic names for actions
*               and status  (RLVAD::AJC)
*     25-OCT-1989 (RLVAD::AJC):
*        Treat tabs (C9) as spaces
*     13-MAR-1990: Make ch 33-127 in KEYPAR the same
*               corrects bug where non-alpha stacked  (RLVAD::AJC)
*     20-JUL-1990 (RLVAD::AJC):
*        Allow number to AMBIG state after E or D
*     17-JUL-1991 (RLVAD::AJC):
*        Remove unused decalaration of STRING and TOKEN
*     05-MAY-1993 (RLVAD::AJC):
*        Don't change exponent letter e,d,D to E
*     24-DEC-2002 (AJC):
*        Add ARRAMBIG state so ] is not special in normal AMBIG
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'LEX_PAR'

*  Parser state table

      BYTE TABLE(4,0:127,25)
      COMMON /LEX_COM/ TABLE

*  Local Constants:

*  Parser states

      INTEGER PAR,NAME,LIT,QUOTE,NUMBER,FRACTION,EXP,AMBIG,ARRAY
      INTEGER KAMBIG,KEND,SNUMBER,NFRACTION,STEXP,XEXP,KEYPAR
      INTEGER STDEXP,XDEXP,DEXP,BRACKET,ABRACKET,KBRACKET,ARRAMBIG
      PARAMETER(PAR=1)
      PARAMETER(NAME=2)
      PARAMETER(LIT=3)
      PARAMETER(QUOTE=4)
      PARAMETER(NUMBER=5)
      PARAMETER(FRACTION=6)
      PARAMETER(EXP=7)
      PARAMETER(AMBIG=8)
      PARAMETER(ARRAY=9)
      PARAMETER(KAMBIG=10)
      PARAMETER(KEND=11)
      PARAMETER(SNUMBER=12)
      PARAMETER(NFRACTION=13)
      PARAMETER(STEXP=14)
      PARAMETER(XEXP=15)
      PARAMETER(KEYPAR=16)
      PARAMETER(STDEXP=17)
      PARAMETER(XDEXP=18)
      PARAMETER(DEXP=19)
      PARAMETER(BRACKET=20)
      PARAMETER(ABRACKET=21)
      PARAMETER(KBRACKET=22)
      PARAMETER(ARRAMBIG=23)

*  Logical constants

      LOGICAL T,F
      PARAMETER(T=.TRUE.)
      PARAMETER(F=.FALSE.)

*  Local Variables:

      CHARACTER*1 C0,C9,C13,C32,C33,C34,C126,C127
      INTEGER S

*.

      C0 = CHAR(0)
      C9 = CHAR(9)
      C32 = CHAR(32)
      C33 = CHAR(33)
      C34 = CHAR(34)
      C126 = CHAR(126)
      C127 = CHAR(127)
      C13 = CHAR(13)
      S = SAI__OK

*  Set up state table

      CALL LEX_INIT(25,TABLE)

*  PAR state - expecting a parameter

      CALL LEX_SET(25,TABLE,PAR,C32,C127,KAMBIG,0,F,T,F,T,' ',S)
      CALL LEX_SET(25,TABLE,PAR,'@','@',NAME,0,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,PAR,'''','''',LIT,0,F,F,F,T,' ',S)
      CALL LEX_SET(25,TABLE,PAR,'"','"',LIT,0,F,F,F,T,' ',S)
      CALL LEX_SET(25,TABLE,PAR,'0','9',NUMBER,0,F,T,F,T,' ',S)
      CALL LEX_SET(25,TABLE,PAR,'+','+',SNUMBER,0,F,T,F,T,' ',S)
      CALL LEX_SET(25,TABLE,PAR,'-','-',SNUMBER,0,F,T,F,T,' ',S)
      CALL LEX_SET(25,TABLE,PAR,'.','.',NFRACTION,0,F,T,F,T,' ',S)
      CALL LEX_SET(25,TABLE,PAR,' ',' ',PAR,0,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,PAR,C9,C9,PAR,0,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,PAR,',',',',PAR,0,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,PAR,C0,C0,PAR,LEX__NULL,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,PAR,'[','[',ARRAY,LEX__STARR,F,F,F,T,' ',S)
      CALL LEX_SET(25,TABLE,PAR,C13,C13,PAR,LEX__ELINE,F,F,F,F,' ',S)

*  KEYPAR state - look for a parameter following a keyword=
*                 construct (so can't be a keyword)

      CALL LEX_SET(25,TABLE,KEYPAR,C33,C127,AMBIG,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,KEYPAR,'@','@',NAME,0,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,KEYPAR,'''','''',LIT,0,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,KEYPAR,'"','"',LIT,0,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,KEYPAR,'0','9',NUMBER,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,KEYPAR,'+','+',SNUMBER,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,KEYPAR,'-','-',SNUMBER,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,KEYPAR,'.','.',NFRACTION,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,KEYPAR,' ',' ',KEYPAR,0,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,KEYPAR,C9,C9,KEYPAR,0,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,KEYPAR,',',',',KEYPAR,0,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,KEYPAR,C0,C0,PAR,LEX__NULL,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,KEYPAR,'[','[',ARRAY,LEX__STARR,F,F,F,F,
     : ' ',S)
      CALL LEX_SET(25,TABLE,KEYPAR,C13,C13,PAR,LEX__ELINE,F,F,F,F,' ',S)

*  NAME state  -  processing a name (any sequence of characters preceeded
*                 by a @

      CALL LEX_SET(25,TABLE,NAME,C0,C127,NAME,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,NAME,' ',' ',PAR,LEX__NAME,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,NAME,C9,C9,PAR,LEX__NAME,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,NAME,',',',',PAR,LEX__NAME,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,NAME,'(','(',BRACKET,0,F,T,F,F,' ',S)

*  BRACKET state  -  In brackets in a name (, is not a terminator here)

      CALL LEX_SET(25,TABLE,BRACKET,C0,C127,BRACKET,0,F,T,F,F,' ',
     : S)
      CALL LEX_SET(25,TABLE,BRACKET,')',')',NAME,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,BRACKET,' ',' ',PAR,LEX__NAME,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,BRACKET,C9,C9,PAR,LEX__NAME,F,F,F,F,' ',S)

*  LIT state  -  processing a literal string, a ' character
*                causes transition to QUOTE state

      CALL LEX_SET(25,TABLE,LIT,C0,C127,LIT,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,LIT,'''','''',QUOTE,0,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,LIT,'"','"',QUOTE,0,F,F,F,F,' ',S)

*  QUOTE state - if another ' is seen output a single quote and
*                return to LIT state. On any other character
*                backspace and set STRING action

      CALL LEX_SET(25,TABLE,QUOTE,C0,C127,0,LEX__STRING,T,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,QUOTE,'"','"',LIT,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,QUOTE,'''','''',LIT,0,F,T,F,F,' ',S)

*  NUMBER state - processing a number before decimal point

      CALL LEX_SET(25,TABLE,NUMBER,C33,C126,AMBIG,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,NUMBER,'0','9',NUMBER,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,NUMBER,'.','.',FRACTION,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,NUMBER,'E','E',STEXP,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,NUMBER,'e','e',STEXP,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,NUMBER,'D','D',STDEXP,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,NUMBER,'d','d',STDEXP,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,NUMBER,' ',' ',0,LEX__INTEGER,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,NUMBER,C9,C9,0,LEX__INTEGER,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,NUMBER,',',',',0,LEX__INTEGER,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,NUMBER,']',']',0,LEX__INTEGER,T,F,F,F,' ',S)

*  SNUMBER state - processing a number following a sign

      CALL LEX_SET(25,TABLE,SNUMBER,C32,C126,AMBIG,0,T,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,SNUMBER,C9,C9,AMBIG,0,T,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,SNUMBER,'0','9',NUMBER,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,SNUMBER,'.','.',NFRACTION,0,F,T,F,F,' ',S)

*  NFRACTION state - processing a number after the decimal point
*                    when there were no digits preceeding the point

      CALL LEX_SET(25,TABLE,NFRACTION,C32,C126,AMBIG,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,NFRACTION,'0','9',FRACTION,0,F,T,F,F,' ',S)

*  FRACTION state - processing a number after the decimal point

      CALL LEX_SET(25,TABLE,FRACTION,C33,C126,AMBIG,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,FRACTION,'0','9',FRACTION,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,FRACTION,'E','E',STEXP,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,FRACTION,'e','e',STEXP,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,FRACTION,'D','D',STDEXP,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,FRACTION,'d','d',STDEXP,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,FRACTION,' ',' ',0,LEX__REAL,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,FRACTION,C9,C9,0,LEX__REAL,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,FRACTION,',',',',0,LEX__REAL,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,FRACTION,']',']',0,LEX__REAL,T,F,F,F,' ',S)

*  STEXP state - start of exponent following the E

      CALL LEX_SET(25,TABLE,STEXP,C32,C126,AMBIG,0,T,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,STEXP,C9,C9,AMBIG,0,T,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,STEXP,'0','9',EXP,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,STEXP,'+','+',XEXP,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,STEXP,'-','-',XEXP,0,F,T,F,F,' ',S)

*  STDEXP state - start of exponent of double precision number
*                 following the D

      CALL LEX_SET(25,TABLE,STDEXP,C32,C126,AMBIG,0,T,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,STDEXP,C9,C9,AMBIG,0,T,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,STDEXP,'0','9',DEXP,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,STDEXP,'+','+',XDEXP,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,STDEXP,'-','-',XDEXP,0,F,T,F,F,' ',S)

*  XEXP state - number exponent after a sign but no digits yet

      CALL LEX_SET(25,TABLE,XEXP,C32,C126,AMBIG,0,T,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,XEXP,C9,C9,AMBIG,0,T,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,XEXP,'0','9',EXP,0,F,T,F,F,' ',S)

*  XDEXP state - number exponent after a sign but no digits yet
*                (double precision)

      CALL LEX_SET(25,TABLE,XDEXP,C32,C126,AMBIG,0,T,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,XDEXP,C9,C9,AMBIG,0,T,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,XDEXP,'0','9',DEXP,0,F,T,F,F,' ',S)

*  EXP state - exponent of number

      CALL LEX_SET(25,TABLE,EXP,C33,C126,AMBIG,0,T,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,EXP,'0','9',EXP,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,EXP,' ',' ',0,LEX__REAL,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,EXP,C9,C9,0,LEX__REAL,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,EXP,',',',',0,LEX__REAL,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,EXP,']',']',0,LEX__REAL,T,F,F,F,' ',S)

*  DEXP state - exponent of double precision number

      CALL LEX_SET(25,TABLE,DEXP,C33,C126,AMBIG,0,T,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,DEXP,'0','9',DEXP,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,DEXP,' ',' ',0,LEX__DOUBLE,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,DEXP,C9,C9,0,LEX__DOUBLE,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,DEXP,',',',',0,LEX__DOUBLE,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,DEXP,']',']',0,LEX__DOUBLE,T,F,F,F,' ',S)

*  AMBIG state - could be a name or literal string - to be
*                decided later on basis of parameter type

      CALL LEX_SET(25,TABLE,AMBIG,C33,C126,AMBIG,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,AMBIG,' ',' ',0,LEX__AMBIG,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,AMBIG,C9,C9,0,LEX__AMBIG,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,AMBIG,',',',',0,LEX__AMBIG,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,AMBIG,'(','(',ABRACKET,0,F,T,F,F,' ',S)

*  BRACKET state  -  In brackets in a name (, is not a terminator here)

      CALL LEX_SET(25,TABLE,ABRACKET,C0,C127,ABRACKET,0,F,T,F,F,' ',
     : S)
      CALL LEX_SET(25,TABLE,ABRACKET,')',')',AMBIG,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,ABRACKET,' ',' ',0,LEX__AMBIG,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,ABRACKET,C9,C9,0,LEX__AMBIG,F,F,F,F,' ',S)

*  ARRAY state - first array component expected

      CALL LEX_SET(25,TABLE,ARRAY,C34,C127,ARRAMBIG,0,F,T,F,T,' ',S)
      CALL LEX_SET(25,TABLE,ARRAY,'"','"',LIT,0,F,F,F,T,' ',S)
      CALL LEX_SET(25,TABLE,ARRAY,'''','''',LIT,0,F,F,F,T,' ',S)
      CALL LEX_SET(25,TABLE,ARRAY,'0','9',NUMBER,0,F,T,F,T,' ',S)
      CALL LEX_SET(25,TABLE,ARRAY,'+','+',SNUMBER,0,F,T,F,T,' ',S)
      CALL LEX_SET(25,TABLE,ARRAY,'-','-',SNUMBER,0,F,T,F,T,' ',S)
      CALL LEX_SET(25,TABLE,ARRAY,'.','.',NFRACTION,0,F,T,F,T,' ',S)
      CALL LEX_SET(25,TABLE,ARRAY,' ',' ',ARRAY,0,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,ARRAY,C9,C9,ARRAY,0,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,ARRAY,',',',',ARRAY,0,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,ARRAY,C0,C0,PAR,LEX__NULL,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,ARRAY,'[','[',ARRAY,LEX__STARR,F,F,F,T,' ',
     : S)
      CALL LEX_SET(25,TABLE,ARRAY,']',']',0,LEX__ENDARR,F,F,F,F,' ',S)
*   use NAME to signal an illegal array component
      CALL LEX_SET(25,TABLE,ARRAY,'@','@',PAR,LEX__NAME,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,ARRAY,'!','!',PAR,LEX__NAME,F,F,F,F,' ',S)

*  KAMBIG state - could be keyword or name or string

      CALL LEX_SET(25,TABLE,KAMBIG,C33,C126,KAMBIG,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,KAMBIG,' ',' ',KEND,0,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,KAMBIG,C9,C9,KEND,0,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,KAMBIG,',',',',0,LEX__KAMBIG,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,KAMBIG,'=','=',KEYPAR,LEX__KEYWORD,F,F,F,F,
     : ' ',S)
      CALL LEX_SET(25,TABLE,KAMBIG,'(','(',KBRACKET,0,F,T,F,F,' ',S)

*  KBRACKET state  -  In brackets in a name (, is not a terminator here)

      CALL LEX_SET(25,TABLE,KBRACKET,C0,C127,KBRACKET,0,F,T,F,F,' ',
     : S)
      CALL LEX_SET(25,TABLE,KBRACKET,')',')',KAMBIG,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,KBRACKET,' ',' ',KEND,0,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,KBRACKET,C9,C9,KEND,0,F,F,F,F,' ',S)

*  KEND state - end of KAMBIG due to space or tab, look for an equals sign
*               in which case it is a keyword

      CALL LEX_SET(25,TABLE,KEND,C0,C127,0,LEX__KAMBIG,T,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,KEND,' ',' ',KEND,0,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,KEND,C9,C9,KEND,0,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,KEND,'=','=',KEYPAR,LEX__KEYWORD,F,F,F,F,
     : ' ',S)

*  ARRAMBIG state - within array, could be a name or literal string - to be
*                decided later on basis of parameter type

      CALL LEX_SET(25,TABLE,ARRAMBIG,C33,C126,ARRAMBIG,0,F,T,F,F,' ',S)
      CALL LEX_SET(25,TABLE,ARRAMBIG,' ',' ',0,LEX__AMBIG,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,ARRAMBIG,C9,C9,0,LEX__AMBIG,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,ARRAMBIG,',',',',0,LEX__AMBIG,F,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,ARRAMBIG,']',']',0,LEX__AMBIG,T,F,F,F,' ',S)
      CALL LEX_SET(25,TABLE,ARRAMBIG,'(','(',ABRACKET,0,F,T,F,F,' ',S)

      END
