      SUBROUTINE CAT1_AXSET (FRM, AXIS, CI, COLNAM, STATUS)
*+
*  Name:
*     CAT1_AXSET
*  Purpose:
*     Set the details of an axis in an AST frame-set.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL AT1_AXSET (FRM, AXIS, CI, COLNAM; STATUS)
*  Description:
*     Set the details of a specified axis in an AST frame-set.  The
*     details are obtained from the corresponding CAT column.
*  Arguments:
*     FRM  =  INTEGER (Given)
*        Pointer to the AST frame.
*     AXIS  =  INTEGER (Given)
*        AST axis for which details are to be set.
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     COLNAM  =  CHARACTER*(*) (Given)
*        Name of the catalogue column corresponding to the AST axis.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to get an identifier for the column.
*     If ok then
*       Get the units of the column.
*       Get the external format of the column.
*       Determine the number of displayed digits corresponding to the
*       format.
*       Convert the axis number to a character string.
*       Set the frame-set axis label.
*       Set the frame-set axis symbol.
*       Set the frame-set axis units.
*       Set the frame-set axis number of display digits.
*     end if
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     7/11/99 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard SAE constants.
      INCLUDE 'CAT_PAR'           ! External CAT constants.
c      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
c      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  FRM,
     :  AXIS,
     :  CI
      CHARACTER*(*)
     :  COLNAM
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  COLI,       ! Identifier to CAT column.
     :  NUMDIG,     ! Number of digits in the format.
     :  DOTPOS,     ! Position of '.' in EXFMT.
     :  NCHAR,      ! Number of characters.
     :  LCOLNM,     ! Length of COLNAM (excl. trail. blanks).
     :  LUNITS      !   "    "  UNITS  ( "  .   "  .   "   ).
      CHARACTER
     :  UNITS*(CAT__SZUNI),  ! Units           of CAT column.
     :  EXFMT*(CAT__SZEXF),  ! External format "   "    "   .
     :  CAXIS*1     ! AST axis number converted to a CHARACTER string.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Attempt to get an identifier for the column and proceed if ok.

         CALL CAT_TIDNT (CI, COLNAM, COLI, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            LCOLNM = CHR_LEN(COLNAM)

*
*          Get the units and external format of the column.

            CALL CAT_TIQAC (COLI, 'UNITS', UNITS, STATUS)

            IF (UNITS .NE. ' ') THEN
               LUNITS = CHR_LEN(UNITS)
            ELSE
               LUNITS = 1
            END IF

            CALL CAT_TIQAC (COLI, 'EXFMT', EXFMT, STATUS)

*
*          Determine the number of displayed digits corresponding to the
*          external format.

            DOTPOS = INDEX (EXFMT, '.')

            IF (DOTPOS .GT. 0) THEN
               EXFMT(1 : DOTPOS) = ' '
               CALL CHR_CTOI (EXFMT, NUMDIG, STATUS)
            ELSE
               NUMDIG = 0
            END IF

            NUMDIG = MAX(NUMDIG, 0)

*
*          Convert the axis number to a character string.

            CALL CHR_ITOC (AXIS, CAXIS, NCHAR)

*
*          Set the frame-set axis label.

            CALL AST_SETC (FRM, 'LABEL(' // CAXIS // ')',
     :        COLNAM(1 : LCOLNM), STATUS)

*
*          Set the frame-set axis symbol.

            CALL AST_SETC (FRM, 'SYMBOL(' // CAXIS // ')',
     :        COLNAM(1 : LCOLNM), STATUS)

*
*          Set the frame-set axis units.

            CALL AST_SETC (FRM, 'UNIT(' // CAXIS // ')',
     :        UNITS(1 : LUNITS), STATUS)

*
*          Set the frame-set axis number of display digits.

            CALL AST_SETI (FRM, 'DIGITS(' // CAXIS // ')',
     :        NUMDIG, STATUS)

         END IF

      END IF

      END
