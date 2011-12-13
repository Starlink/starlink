      SUBROUTINE CAT1_ADDCL (CI, FNAME, FGENUS, FEXP, FDTYPE, FCSIZE,
     :  FDIM, FSIZE, FNULL, FXCEPT, FSCALE, FZERO, FORDER, FDATE, FUNIT,
     :  FXFMT, FPDISP, FCOMM, FI, STATUS)
*+
*  Name:
*     CAT1_ADDCL
*  Purpose:
*     Add a column to the list of columns and get an identifier for it.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ADDCL (CI; FNAME; FGENUS, FEXP, FDTYPE, FCSIZE, FDIM,
*       FSIZE, FNULL, FXCEPT, FSCALE, FZERO, FORDER, FDATE, FUNIT,
*       FXFMT, FPDISP, FCOMM; FI; STATUS)
*  Description:
*     Add a column to the list of columns and obtain an identifier for
*     it.  The column is defined by the values for all its attributes.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Identifier to the catalogue to which the column belongs.
*     FNAME  =  CHARACTER*(*) (Given and Returned)
*        Name attribute.
*     FGENUS  =  INTEGER (Given)
*        Genus attribute.
*     FEXP  =  CHARACTER*(*) (Given)
*        Expression attribute.
*     FDTYPE  =  INTEGER (Given)
*        Data type attribute.
*     FCSIZE =  INTEGER (Given)
*        Character column size attribute (set to zero if the column
*        is note of data type character).
*     FDIM  =  INTEGER (Given)
*        Dimensionality attribute.
*     FSIZE  =  INTEGER (Given)
*        Size attribute.
*     FNULL  =  INTEGER
*        Null flag attribute.(Given)
*     FXCEPT  =  CHARACTER*(*) (Given)
*        Exception value attribute.
*     FSCALE  =  DOUBLE PRECISION (Given)
*        Scale factor attribute.
*     FZERO  =  DOUBLE PRECISION (Given)
*        Zero point attribute.
*     FORDER  =  INTEGER (Given)
*        Order attribute.
*     FDATE  =  DOUBLE PRECISION (Given)
*        Modification date attribute.
*     FUNIT  =  CHARACTER (Given)
*        Units attribute.
*     FXFMT  =  CHARACTER*(*) (Given)
*        External format attribute.
*     FPDISP  =  LOGICAL (Given)
*        Preferential display flag attribute.
*     FCOMM  =  CHARACTER*(*) (Given)
*        Comments attribute.
*     FI  =  INTEGER (Returned)
*        Identifier to the column.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Check whether the given name exists and if necessary attempt
*     to substitute a unique name.
*     If ok then
*       If the given and final names are not the same then
*         Report a message.
*         If the comments are blank then
*           Adopt the original name as comments.
*         end if
*         Adopt the final name.
*       end if
*       Attempt to create the column identifier.
*       If it creates ok then
*         Add the attributes for the column to the list of attributes.
*       end if
*     end if
*     Report any error.
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
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
*     23/6/93  (ACD): Original version.
*     11/7/93  (ACD): First working version.
*     16/12/99 (ACD): Added checks to ensure the column name is unique.
*     8/5/01   (ACD): If the name is modified and the comments are blank
*       then adopt the original name as comments.
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
     :  FGENUS,
     :  FDTYPE,
     :  FCSIZE,
     :  FDIM,
     :  FSIZE,
     :  FNULL,
     :  FORDER
      CHARACTER
     :  FEXP*(*),
     :  FXCEPT*(*),
     :  FUNIT*(*),
     :  FXFMT*(*),
     :  FCOMM*(*)
      DOUBLE PRECISION
     :  FSCALE,
     :  FZERO,
     :  FDATE
      LOGICAL
     :  FPDISP
*  Arguments Given and Returned:
      CHARACTER
     :  FNAME*(*)
*  Arguments Returned:
      INTEGER
     :  FI
*  Status:
      INTEGER STATUS            ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  WNAME*(CAT__SZCMP),     ! Working name.
     :  BUFFER*75               ! Output buffer.
      INTEGER
     :  LFNAME,  ! Length of FNAME  (excl. trail. blanks).
     :  LWNAME,  !   "    "  WNAME  ( "  .   "  .   "   ).
     :  LFCOMM,  !   "    "  FCOMM  ( "  .   "  .   "   ).
     :  BUFPOS   !   "    "  BUFFER ( "  .   "  .   "   ).
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check whether the given name exists, if necessary attempt to
*       generate a unique name and proceed if ok.

         WNAME = FNAME
         CALL CAT1_NMUNQ (CI, WNAME, STATUS)
C        print2000, fname, wname, status
C2000    format(1x, 'fname, wname, status:' / 3x, a, 1x, a, 1x, i15)

         IF (STATUS .EQ. CAT__OK) THEN

*
*          If the generated name differs from the original one then
*          report a message and adopt it.  If the comments are blank
*          the original name is adopted as comments.

            IF (FNAME .NE. WNAME) THEN
               BUFFER = ' '
               BUFPOS = 0

               CALL CHR_PUTC ('Duplicate column name ', BUFFER, BUFPOS)

               IF (FNAME .NE. ' ') THEN
                  LFNAME = CHR_LEN(FNAME)
               ELSE
                  LFNAME = 1
               END IF

               CALL CHR_PUTC (FNAME(1 : LFNAME), BUFFER, BUFPOS)

               CALL CHR_PUTC (' changed to ', BUFFER, BUFPOS)

               IF (WNAME .NE. ' ') THEN
                  LWNAME = CHR_LEN(WNAME)
               ELSE
                  LWNAME = 1
               END IF

               CALL CHR_PUTC (WNAME(1 : LWNAME), BUFFER, BUFPOS)

               CALL CHR_PUTC ('.', BUFFER, BUFPOS)

               CALL CAT1_MSG (' ', BUFFER(1 : BUFPOS), STATUS)

*
*             If the comments are blank then adopt the original name
*             as comments.

               IF (FCOMM .EQ. ' ') THEN
                  LFCOMM = 1

                  CALL CHR_PUTC (FNAME(1 : LFNAME), FCOMM, LFCOMM)
                  CALL CHR_PUTC (' (original name).', FCOMM, LFCOMM)
               END IF

*
*             Adopt the modified name.

               FNAME = WNAME
            END IF

*
*          Attempt to create the column identifier.

            CALL CAT1_CRTID (CAT__FITYP, CI, FI, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN

*
*             Attempt to add the attributes for this column to the list
*             of attributes.

               CALL CAT1_ADDAC (FI, 'NAME', .FALSE., FNAME, STATUS)
               CALL CAT1_ADDAI (FI, 'GENUS', .FALSE., FGENUS, STATUS)
               CALL CAT1_ADDAC (FI, 'EXPR', .FALSE., FEXP, STATUS)
               CALL CAT1_ADDAI (FI, 'DTYPE', .FALSE., FDTYPE, STATUS)
               CALL CAT1_ADDAI (FI, 'CSIZE', .FALSE., FCSIZE, STATUS)
               CALL CAT1_ADDAI (FI, 'DIMS', .FALSE., FDIM, STATUS)
               CALL CAT1_ADDAI (FI, 'SIZE', .FALSE., FSIZE, STATUS)
               CALL CAT1_ADDAI (FI, 'NULL', .FALSE., FNULL, STATUS)
               CALL CAT1_ADDAC (FI, 'EXCEPT', .FALSE., FXCEPT, STATUS)
               CALL CAT1_ADDAD (FI, 'SCALEF', .FALSE., FSCALE, STATUS)
               CALL CAT1_ADDAD (FI, 'ZEROP', .FALSE., FZERO, STATUS)
               CALL CAT1_ADDAI (FI, 'ORDER', .FALSE., FORDER, STATUS)
               CALL CAT1_ADDAD (FI, 'DATE', .TRUE., FDATE, STATUS)
               CALL CAT1_ADDAC (FI, 'UNITS', .TRUE., FUNIT, STATUS)
               CALL CAT1_ADDAC (FI, 'EXFMT', .TRUE., FXFMT, STATUS)
               CALL CAT1_ADDAL (FI, 'PRFDSP', .TRUE., FPDISP, STATUS)
               CALL CAT1_ADDAC (FI, 'COMM', .TRUE., FCOMM, STATUS)
            END IF
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            BUFFER = ' '
            BUFPOS = 0

            CALL CHR_PUTC ('Failed to create column ', BUFFER, BUFPOS)

            IF (FNAME .NE. ' ') THEN
               LFNAME = CHR_LEN(FNAME)
            ELSE
               LFNAME = 1
            END IF

            CALL CHR_PUTC (FNAME(1 : LFNAME), BUFFER, BUFPOS)

            IF (STATUS .EQ. CAT__DUPNM) THEN
               CALL CHR_PUTC (' (multiple use of name)', BUFFER, BUFPOS)
            END IF

            CALL CHR_PUTC ('.', BUFFER, BUFPOS)

            CALL CAT1_ERREP ('CAT1_ADDCL_ERR', BUFFER(1 : BUFPOS),
     :        STATUS)
         END IF

      END IF

      END
