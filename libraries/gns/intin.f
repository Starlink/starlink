      SUBROUTINE gns_1INTIN (STRING, NSTRT, IRESLT, JFLAG)
*+
*  Name:
*     INTIN

*  Purpose:
*     Convert free-format input into integer

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     Internal routine

*  Arguments:
*     STRING = CHAR (Given)
*         String containing field to be decoded
*     NSTRT = INTEGER (Given)
*         Pointer to 1st character of field in string
*     NSTRT = INTEGER (Returned)
*         Advanced to next field
*     IRESLT = INTEGER (Returned)
*         Result
*     JFLAG = INTEGER (Returned)
*         -1 = -OK, 0 = +OK, 1 = null field, 2 = error

*  Notes:
*     1     The basic format is #^ where # means + or -, and ^
*     means a string of decimals.
*
*     2     Spaces:
*     Leading spaces are ignored.
*     Spaces between # and ^ are ignored.
*     Trailing spaces are ignored;  the first signifies
*     end of decoding and subsequent ones are skipped.
*
*     3     # is optional.  The default is +.
*     If # is present, ^ must be as well.
*
*     4     A null field is one that does not begin with
*     +,-, or 0-9, or consists entirely of spaces.
*     If the field is null, JFLAG is set to 1 and
*     IRESLT is left untouched.
*
*     5     NSTRT = 1 for the first character in the string.
*
*     6     On return from INTIN, NSTRT is set ready for the next
*     decode - following trailing blanks and (if used) the
*     separator. If a separator other than a comma or an underscore
*     is being used, NSTRT must be incremented before the next
*     call to INTIN.
*
*     7     Errors (JFLAG=2) occur when:
*     a)  The field has # but no ^.
*     b)  ^ is greater than 2**31-1.
*
*     8     When an error has been detected, NSTRT is left
*     pointing to the character which caused the error.
*
*     9     End of field may occur in either of two ways:
*     a)  As dictated by the string length.
*     b)  Detected during the decode.
*     (b overrides a.)

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

*  Externals:
*     gns_1IDCHI

*-
      IMPLICIT NONE

      CHARACTER*(*) STRING
      INTEGER NSTRT,IRESLT,JFLAG

      INTEGER JPTR,LENSTR,MSIGN,NVEC,J
      DOUBLE PRECISION DRES,DIGIT



*  Current character
      JPTR=NSTRT
*  End of field
      LENSTR=LEN(STRING)

*  Set defaults
      DRES=0D0
      MSIGN=1

*  Look for sign
 100  CONTINUE
      CALL gns_1IDCHI(STRING,JPTR,LENSTR,NVEC,DIGIT)
      GO TO ( 400, 100,  300,  200, 9110, 9100, 9110),NVEC
*             0-9   SP     +     -     ,   ELSE   END

*  Negative
 200  CONTINUE
      MSIGN=-1

*  Look for first decimal
 300  CONTINUE
      CALL gns_1IDCHI(STRING,JPTR,LENSTR,NVEC,DIGIT)
      GO TO ( 400, 300, 9200, 9200, 9200, 9200, 9210),NVEC
*             0-9   SP     +     -     ,   ELSE   END

*  Accept decimals
 400  CONTINUE
      DRES=DRES*1D1+DIGIT

*  Test for overflow
      IF(DRES.GT.2147483653D0) GO TO 9200

*  Look for subsequent decimals
      CALL gns_1IDCHI(STRING,JPTR,LENSTR,NVEC,DIGIT)
      GO TO ( 400, 1610, 1600, 1600, 1600, 1600, 1610),NVEC
*             0-9   SP     +     -     ,   ELSE   END

*  Get result & status
 1600 CONTINUE
      JPTR=JPTR-1
 1610 CONTINUE
      J=0
      IF(MSIGN.EQ.1) GO TO 1620
      J=-1
      DRES=-DRES
 1620 CONTINUE
      IRESLT=INT(DNINT(DRES))

*  Skip to end of field
 1630 CONTINUE
      CALL gns_1IDCHI(STRING,JPTR,LENSTR,NVEC,DIGIT)
      GO TO (1720, 1630, 1720, 1720, 9900, 1720, 9900),NVEC
*             0-9   SP     +     -     ,   ELSE   END

 1720 CONTINUE
      JPTR=JPTR-1
      GO TO 9900

*  Exits

*  Null field
 9100 CONTINUE
      JPTR=JPTR-1
 9110 CONTINUE
      J=1
      GO TO 9900

*  Errors
 9200 CONTINUE
      JPTR=JPTR-1
 9210 CONTINUE
      J=2

*  Return
 9900 CONTINUE
      NSTRT=JPTR
      JFLAG=J

      END
