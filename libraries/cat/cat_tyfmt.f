      SUBROUTINE CAT_TYFMT (DTYPE, CSIZE, STRING, POSN, STATUS)
*+
*  Name:
*     CAT_TYFMT
*  Purpose:
*     Construct a character representation of a CAT data type.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TYFMT (DTYPE, CSIZE; STRING, POSN; STATUS)
*  Description:
*     Construct a character string representation of a CAT data
*     type and append it to a character string.
*
*     CAT data types are represented using an integer code (here
*     argument DTYPE) with a further integer variable (here argument
*     CSIZE) giving the size of character strings.  A character
*     representation of the data type is constructed from these integer
*     numbers.  This character string representation is (deliberately)
*     identical to that used by HDS.
*
*     The constructed value is appended to the input string, starting
*     at element POSN+1 of the input string (using the input value for
*     POSN).  On output POSN is set to the new length of the string
*     (cf. the CHR routines).
*
*     If an illegal CAT data type is input, the assembled string
*     contains an error text, but an error status is deliberately not
*     raised.  This behaviour is adopted because CAT_TYFMT is just
*     formatting a character string and an invalid data type is not
*     really an error for it.
*  Arguments:
*     DTYPE  =  INTEGER (Given)
*         Code for a CAT data type.
*     CSIZE  =  INTEGER (Given)
*         Size of a CAT character string.
*     STRING  =  CHARACTER*(*) (Given and Returned)
*         The character string into which the data type is to be
*         appended.
*     POSN  =  INTEGER (Given and Returned)
*         The last non-blank element of STRING.  VALUE is inserted into
*         STRING starting at the element given by input value of POSN+1.
*         On output POSN is set to the new length of the string, again
*         excluding trailing blanks.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     for each data type
*       Construct the representation for that data type.
*     end for
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
*     15/7/93  (ACD): Prologue only.
*     25/1/94  (ACD): Modified error reporting.
*     8/2/94   (ACD): Changed parameters for data type codes.
*     23/11/94 (ACD): Changed all occurrences of 'StarBase' to 'CAT'.
*     22/1/97  (ACD): Fixed a spelling mistake in the prologue.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      INTEGER
     :  DTYPE,
     :  CSIZE
*  Arguments Given and Returned:
      CHARACTER
     :  STRING*(*)
      INTEGER
     :  POSN
*  Status:
      INTEGER STATUS             ! Global status
*.

      IF (STATUS .EQ. CAT__OK) THEN

C        print3000, dtype, csize
C3000    format(1x, 'cat_tyfmt: dtype, csize: ', i4, i4)

*
*       Check the given data type for each supported CAT data
*       type and construct an appropriate character representation.
*       Note that an illegal CAT data type does not constitute an
*       error condition here.

         IF (DTYPE .EQ. CAT__TYPEUB) THEN
            CALL CHR_PUTC ('_UBYTE', STRING, POSN)

         ELSE IF (DTYPE .EQ. CAT__TYPEB) THEN
            CALL CHR_PUTC ('_BYTE', STRING, POSN)

         ELSE IF (DTYPE .EQ. CAT__TYPEUW) THEN
            CALL CHR_PUTC ('_UWORD', STRING, POSN)

         ELSE IF (DTYPE .EQ. CAT__TYPEW) THEN
            CALL CHR_PUTC ('_WORD', STRING, POSN)

         ELSE IF (DTYPE .EQ. CAT__TYPEI) THEN
            CALL CHR_PUTC ('_INTEGER', STRING, POSN)

         ELSE IF (DTYPE .EQ. CAT__TYPER) THEN
            CALL CHR_PUTC ('_REAL', STRING, POSN)

         ELSE IF (DTYPE .EQ. CAT__TYPED) THEN
            CALL CHR_PUTC ('_DOUBLE', STRING, POSN)

         ELSE IF (DTYPE .EQ. CAT__TYPEL) THEN
            CALL CHR_PUTC ('_LOGICAL', STRING, POSN)

         ELSE IF (DTYPE .EQ. CAT__TYPEC) THEN
            CALL CHR_PUTC ('_CHAR*', STRING, POSN)
            CALL CHR_PUTI (CSIZE, STRING, POSN)

         ELSE
            CALL CHR_PUTC ('(Illegal type: ', STRING, POSN)
            CALL CHR_PUTI (DTYPE, STRING, POSN)
            CALL CHR_PUTC (')', STRING, POSN)

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_TYFMT_ERR', 'CAT_TYFMT: error '/
     :        /'constructing character reprsentation of data type.',
     :        STATUS)
         END IF

      END IF

      END
