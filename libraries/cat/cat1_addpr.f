      SUBROUTINE CAT1_ADDPR (CI, PNAME, PDTYPE, PCSIZE, PDIM, PSIZE,
     :  PDATE, PUNIT, PXFMT, PPDISP, PCOMM, PVALUE, QI, STATUS)
*+
*  Name:
*     CAT1_ADDPR
*  Purpose:
*     Add a parameter to the list and get an identifier for it.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ADDPR (CI; PNAME; PDTYPE, PCSIZE, PDIM, PSIZE, PDATE,
*       PUNIT, PXFMT, PPDISP, PCOMM, PVALUE; QI; STATUS)
*  Description:
*     Add a parameter to the list of parameters and obtain an
*     identifier for it.  The parameter is defined by the values for
*     all its attributes.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Identifier to the catalogue to which the parameter belongs.
*     PNAME  =  CHARACTER*(*) (Given and Returned)
*        Name attribute.
*     PDTYPE  =  INTEGER (Given)
*        Data type attribute.
*     PCSIZE =  INTEGER (Given)
*        Character column size attribute (set to zero if the column
*        is note of data type character).
*     PDIM  =  INTEGER (Given)
*        Dimensionality attribute.
*     PSIZE  =  INTEGER (Given)
*        Size attribute.
*     PDATE  =  DOUBLE PRECISION (Given)
*        Modification date attribute.
*     PUNIT  =  CHARACTER (Given)
*        Units attribute.
*     PXFMT  =  CHARACTER*(*) (Given)
*        External format attribute.
*     PPDISP  =  LOGICAL (Given)
*        Preferential display flag attribute.
*     PCOMM  =  CHARACTER*(*) (Given)
*        Comments attribute.
*     PVALUE  =  CHARACTER*(*) (Given)
*        Value attribute.
*     QI  =  INTEGER (Returned)
*        Identifier to the parameter.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Check whether the given name exists and if necessary attempt
*     to substitute a unique name.
*     If ok then
*       If the given and final names are not the same then
*         Report a message.
*         Adopt the final name.
*       end if
*       Attempt to create the parameter identifier.
*       If it creates ok then
*         Add the attributes for the parameter to the list of attributes.
*       end if
*     end if
*     Report any error.
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
*     12/7/93  (ACD): Original version.
*     16/12/99 (ACD): Added checks to ensure the column name is unique.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI,
     :  PDTYPE,
     :  PCSIZE,
     :  PDIM,
     :  PSIZE
      CHARACTER
     :  PUNIT*(*),
     :  PXFMT*(*),
     :  PCOMM*(*),
     :  PVALUE*(*)
      DOUBLE PRECISION
     :  PDATE
      LOGICAL
     :  PPDISP
*  Arguments Given and Returned:
      CHARACTER
     :  PNAME*(*)
*  Arguments Returned:
      INTEGER
     :  QI
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  WNAME*(CAT__SZCMP),     ! Working name.
     :  BUFFER*75               ! Output buffer.
      INTEGER
     :  LPNAME,  ! Length of PNAME  (excl. trail. blanks).
     :  LWNAME,  !   "    "  WNAME  ( "  .   "  .   "   ).
     :  BUFPOS   !   "    "  BUFFER ( "  .   "  .   "   ).
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check whether the given name exists, if necessary attempt to
*       generate a unique name and proceed if ok.

         WNAME = PNAME
         CALL CAT1_NMUNQ (CI, WNAME, STATUS)

         IF (STATUS .EQ. CAT__OK) THEN

*
*          If the generated name differs from the original one then
*          report a message and adopt it.

            IF (PNAME .NE. WNAME) THEN
               BUFFER = ' '
               BUFPOS = 0

               CALL CHR_PUTC ('Duplicate parameter name ', BUFFER,
     :           BUFPOS)

               IF (PNAME .NE. ' ') THEN
                  LPNAME = CHR_LEN(PNAME)
               ELSE
                  LPNAME = 1
               END IF

               CALL CHR_PUTC (PNAME(1 : LPNAME), BUFFER, BUFPOS)

               CALL CHR_PUTC (' changed to ', BUFFER, BUFPOS)

               IF (WNAME .NE. ' ') THEN
                  LWNAME = CHR_LEN(WNAME)
               ELSE
                  LWNAME = 1
               END IF

               CALL CHR_PUTC (WNAME(1 : LWNAME), BUFFER, BUFPOS)

               CALL CHR_PUTC ('.', BUFFER, BUFPOS)

               CALL CAT1_MSG (' ', BUFFER(1 : BUFPOS), STATUS)

               PNAME = WNAME
            END IF

*
*          Attempt to create the parameter identifier.

            CALL CAT1_CRTID (CAT__QITYP, CI, QI, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN

*
*             Attempt to add the attributes for this column to the list
*             of attributes.

               CALL CAT1_ADDAC (QI, 'NAME', .FALSE., PNAME, STATUS)
               CALL CAT1_ADDAI (QI, 'DTYPE', .FALSE., PDTYPE, STATUS)
               CALL CAT1_ADDAI (QI, 'CSIZE', .FALSE., PCSIZE, STATUS)
               CALL CAT1_ADDAI (QI, 'DIMS', .FALSE., PDIM, STATUS)
               CALL CAT1_ADDAI (QI, 'SIZE', .FALSE., PSIZE, STATUS)
               CALL CAT1_ADDAD (QI, 'DATE', .TRUE., PDATE, STATUS)
               CALL CAT1_ADDAC (QI, 'UNITS', .TRUE., PUNIT, STATUS)
               CALL CAT1_ADDAC (QI, 'EXFMT', .TRUE., PXFMT, STATUS)
               CALL CAT1_ADDAL (QI, 'PRFDSP', .TRUE., PPDISP, STATUS)
               CALL CAT1_ADDAC (QI, 'COMM', .TRUE., PCOMM, STATUS)
               CALL CAT1_ADDAC (QI, 'VALUE', .TRUE., PVALUE, STATUS)
            END IF
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            BUFFER = ' '
            BUFPOS = 0

            CALL CHR_PUTC ('Failed to create parameter ', BUFFER,
     :        BUFPOS)

            IF (PNAME .NE. ' ') THEN
               LPNAME = CHR_LEN(PNAME)
            ELSE
               LPNAME = 1
            END IF

            CALL CHR_PUTC (PNAME(1 : LPNAME), BUFFER, BUFPOS)

            IF (STATUS .EQ. CAT__DUPNM) THEN
               CALL CHR_PUTC (' (multiple use of name)', BUFFER, BUFPOS)
            END IF

            CALL CHR_PUTC ('.', BUFFER, BUFPOS)

            CALL CAT1_ERREP ('CAT1_ADDPR_ERR', BUFFER(1 : BUFPOS),
     :        STATUS)
         END IF

      END IF

      END
