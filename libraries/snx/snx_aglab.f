
      SUBROUTINE snx_AGLAB (LNAME, TEXT)

*+
*  Name:
*     AGLAB

*  Purpose:
*     In NCAR AUTOGRAPH, set up one of the predefined
*     informational labels.

*  Language:
*     Starlink Fortran 77

*  Description:
*     See section 2.30 et seq in the AUTOGRAPH manual.

*  Arguments:
*     LNAME = CHAR (Given)
*         Label name - R,L,B, or T.
*     TEXT = CHAR (Given)
*         Text of label

*  Notes:
*     Line numbers 100 for 'T' and 'L', and -100 for 'B' and 'R'
*     are used.
*
*     Trailing blanks in TEXT are ignored to achieve centring.
*
*     If an illegal LNAME is given, the top label is set
*     to '*** AGLAB LABEL ERROR ***'

*  Copyright:
*     Copyright (C) 1987 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     PTW: P. T. Wallace (Starlink)
*     {enter_new_authors_here}

*  History:
*     24-JUN-1987 (PTW):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Externals:
*     AGGETI, AGSETC, AGSETI

*-

      IMPLICIT NONE

      CHARACTER*(*) LNAME,TEXT

      CHARACTER LN*1
      INTEGER NL,I,LM
      LOGICAL OK



*  Find out where the trailing blanks start
      DO 100 I=LEN(TEXT),2,-1
         IF (TEXT(I:I).NE.' ') GO TO 200
 100     CONTINUE
      I=1
 200  CONTINUE

*  Validate the label name and select line number
      OK=.TRUE.
      LN=LNAME
      IF (LN.EQ.'L'.OR.LN.EQ.'T') THEN
         NL=100
      ELSE IF (LN.EQ.'R'.OR.LN.EQ.'B') THEN
         NL=-100
      ELSE
         OK=.FALSE.
         LN='T'
         NL=100
         I=40
      END IF

*  Set up the label
      CALL AGGETI('LINE/MAXIMUM.',LM)
      CALL AGSETI('LINE/MAXIMUM.',I)
      IF (OK) THEN
         CALL AGSETC('LABEL/NAME.',LN)
         CALL AGSETI('LINE/NUMBER.',NL)
         CALL AGSETC('LINE/TEXT.',TEXT(:I))
      ELSE
         CALL AGSETC('LABEL/NAME.',LN)
         CALL AGSETI('LINE/NUMBER.',NL)
         CALL AGSETC('LINE/TEXT.','*** AGLAB LABEL ERROR ***')
      END IF
      CALL AGSETI('LINE/MAXIMUM.',LM)

      END
