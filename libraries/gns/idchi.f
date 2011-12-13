      SUBROUTINE gns_1IDCHI (STRING, NPTR, LENSTR, NVEC, DIGIT)
*+
*  Name:
*     IDCHI

*  Purpose:
*     Identify next character in string for INTIN

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     Internal routine

*  Arguments:
*     STRING = CHAR (Given)
*         String
*     NPTR = INTEGER (Given)
*         Pointer to character to be identified
*     LENSTR = INTEGER (Given)
*         String length
*     NPTR = INTEGER (Returned)
*         Incremented unless end of field
*     NVEC = INTEGER (Returned)
*         Vector for identified character
*     DIGIT = DOUBLE (Returned)
*         Double precision digit if 0-9

*  Notes:
*     NVEC takes the following values:-
*
*     1     0-9
*     2     space
*     3     +
*     4     -
*     5     , or _
*     6     else
*     7     outside string

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
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     01-JAN-1987 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      IMPLICIT NONE

      CHARACTER*(*) STRING
      INTEGER NPTR,LENSTR,NVEC
      DOUBLE PRECISION DIGIT

      INTEGER NCHAR

*  Character/vector tables
      INTEGER NCREC
      PARAMETER (NCREC=15)
      CHARACTER KCTAB(NCREC)
      INTEGER KVTAB(NCREC)
      DATA KCTAB/'0','1','2','3','4','5','6','7','8','9',
     :           ' ','+','-',',','_'/
      DATA KVTAB/10*1,2,3,4,2*5/



*  Handle pointer outside field
      IF (NPTR.LT.1.OR.NPTR.GT.LENSTR) THEN
         NVEC=7
      ELSE

*     Not end of field

*     Identify character
         DO 10 NCHAR=1,NCREC
            IF (KCTAB(NCHAR).EQ.STRING(NPTR:NPTR)) GO TO 2200
   10    CONTINUE

*     Unrecognised
         NVEC=6
         GO TO 2300

*     Recognised
 2200    CONTINUE
         NVEC=KVTAB(NCHAR)

*     Allow for numerals
         DIGIT=DBLE(NCHAR-1)

*     Increment pointer
 2300    CONTINUE
         NPTR=NPTR+1
      END IF

*  Return

      END
