      SUBROUTINE CAT6_COLDT (BUFFER, MAXCOL, GOTUNT, COLUNT, GOTTYP,
     :  COLTYP, COLCSZ, GOTFMT, COLFMT, STATUS)
*+
*  Name:
*     CAT6_COLDT
*  Purpose:
*     Check and decode column details from CURSA-specific comment lines.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_COLDT (BUFFER, MAXCOL; GOTUNT, COLUNT, GOTTYP,
*       COLTYP, COLCSZ, GOTFMT, COLFMT; STATUS)
*  Description:
*     Check and decode column details from CURSA-specific comment lines.
*
*     If a TST comment line is found to contain CURSA-specific
*     column details the details are decoded and the arrays holding
*     these details updated.
*  Arguments:
*     BUFFER  =  CHARACTER*(*) (Given)
*        Buffer holding the TST comment line to be examined.
*     MAXCOL  =  INTEGER (Given)
*        The maximum permitted number of columns.
*     GOTUNT  =  LOGICAL (Given and Returned)
*        Flag; has a list of column units been specified, coded as
*        follows:
*        .TRUE.  - list specified,
*        .FALSE. - no list specified.
*     COLUNT(MAXCOL)  =  CHARACTER*(*) (Given and Returned)
*        List of column units, if specified.
*     GOTTYP  =  LOGICAL (Given and Returned)
*        Flag; has a list of column data types been specified, coded as
*        follows:
*        .TRUE.  - list specified,
*        .FALSE. - no list specified.
*     COLTYP(MAXCOL)  =  INTEGER (Given and Returned)
*        List of column data type codes, if specified.
*     COLCSZ(MAXCOL)  =  INTEGER (Given and Returned)
*        List of column character sizes, if specified.
*     GOTFMT  =  LOGICAL (Given and Returned)
*        Flag; has a list of column external formats been specified, coded
*        as follows:
*        .TRUE.  - list specified,
*        .FALSE. - no list specified.
*     COLFMT(MAXCOL)  =  CHARACTER*(*) (Given and Returned)
*        List of column external formats, if specified.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Create a local upper-case copy of the input TST comment line.
*     If the line corresponds to one of the CURSA-specific directives then
*       If the line contains text beyond the preamble then
*         Split the line into fields
*         If the line is a list of units then
*           Set the 'got units' flag.
*           Copy the units.
*         else if the line is a list of data types then
*           Set the 'got data types' flag.
*           Decode the corresponding integer codes from the character
*           data types.
*           Copy the data types.
*         else if the line is a list of external formats then
*           Set the 'got external formats' flag.
*           Copy the external formats.
*         end if
*       end if
*     end if
*     Report any warning.
*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils
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
*     13/7/00 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
*  Global Variables:
      INCLUDE 'CAT6_TST_CMN'      ! TST back-end common block.
*  Arguments Given:
      CHARACTER
     :  BUFFER*(*)
      INTEGER
     :  MAXCOL
*  Arguments Given and Returned:
      LOGICAL
     :  GOTUNT,
     :  GOTTYP,
     :  GOTFMT
      CHARACTER
     :  COLUNT(MAXCOL)*(*),
     :  COLFMT(MAXCOL)*(*)
      INTEGER
     :  COLTYP(MAXCOL),
     :  COLCSZ(MAXCOL)
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      INTEGER CHR_LEN
      INTEGER CHR_INDEX
*  Local Variables:
      CHARACTER
     :  UPBUFF*(CAT6__SZDRC),  ! Upper-case copy of BUFFER.
     :  COLITM(CAT6__MXCOL)*(CAT__SZEXF),  ! List of items decoded.
     :  TAB*1,                 ! Tab-separator character.
     :  CURTYP*10,             ! Current (character) data type.
     :  WRNMSG*75              ! Warning message.
      LOGICAL
     :  NULFLG(CAT6__MXCOL),   ! Null value flags (dummy here).
     :  BADDET      ! Flag; bad details found?
      INTEGER
     :  NCOL,       ! Number of columns for which details found.
     :  LOOP,       ! Loop index.
     :  START,      ! Start position of list of details.
     :  COLPOS,     ! Position of colon terminating preamble.
     :  LSTAT,      ! Local status.
     :  FCSIZE,     ! Size of character column.
     :  BUFLEN,     ! Length of BUFFER (excl. trail. blanks).
     :  WRNLEN      !   "    "  WRMMSG ( "  .   "  .    "  ).
*.

      IF (STATUS .EQ. CAT__OK) THEN

         BADDET = .FALSE.

*
*       Create a local upper-case copy of the input TST comment line.

         UPBUFF = BUFFER
         CALL CHR_UCASE(UPBUFF)

*
*       Check whether the line corresponds to any of the CURSA-specific
*       directives.

         IF (UPBUFF(1 : 15) .EQ. '#COLUMN-UNITS:'  .OR.
     :       UPBUFF(1 : 15) .EQ. '#COLUMN-TYPES:'  .OR.
     :       UPBUFF(1 : 17) .EQ. '#COLUMN-FORMATS:') THEN

*
*          Proceed there is text beyond the preamble.

            IF (BUFFER .NE. ' ') THEN
               COLPOS = CHR_INDEX(BUFFER, ':')
               BUFLEN = CHR_LEN(BUFFER)
            ELSE
               COLPOS = 0
               BUFLEN = 0
            END IF

            IF (BUFLEN .GT. COLPOS) THEN

*
*             Split the line into fields.  Any leading spaces in the
*             resulting fields are removed.

               START = COLPOS + 1

               TAB = CHAR(CAT6__TABI)
               CALL CAT6_SPLIT (BUFFER(START : BUFLEN), TAB,
     :           CAT6__MXCOL, NCOL, COLITM, NULFLG, STATUS)
               NCOL = MIN(NCOL, MAXCOL)

               DO LOOP = 1, NCOL
                  CALL CHR_LDBLK (COLITM(LOOP) )
               END DO

*
*             If the line is a list of units then set the 'got units' flag
*             and copy the units.

               IF (UPBUFF(1 : 15) .EQ. '#COLUMN-UNITS:') THEN
                  GOTUNT = .TRUE.

                  DO LOOP = 1, NCOL
                     COLUNT(LOOP) = COLITM(LOOP)
                  END DO

*
*             If the line is a list of data types then set the 'got data
*             types' flag, decode the corresponding integer codes from
*             the character data types and copy the decoded types.

               ELSE IF (UPBUFF(1 : 15) .EQ. '#COLUMN-TYPES:') THEN
                  GOTTYP = .TRUE.

                  DO LOOP = 1, NCOL
                     COLCSZ(LOOP) = 0

                     CURTYP = COLITM(LOOP)
                     CALL CHR_UCASE(CURTYP)

                     IF (CURTYP .EQ. 'BYTE') THEN
                        COLTYP(LOOP) = CAT__TYPEB

                     ELSE IF (CURTYP .EQ. 'WORD') THEN
                        COLTYP(LOOP) = CAT__TYPEW

                     ELSE IF (CURTYP .EQ. 'INTEGER') THEN
                        COLTYP(LOOP) = CAT__TYPEI

                     ELSE IF (CURTYP .EQ. 'REAL') THEN
                        COLTYP(LOOP) = CAT__TYPER

                     ELSE IF (CURTYP .EQ. 'DOUBLE') THEN
                        COLTYP(LOOP) = CAT__TYPED

                     ELSE IF (CURTYP .EQ. 'LOGICAL') THEN
                        COLTYP(LOOP) = CAT__TYPEL

                     ELSE IF (CURTYP(1 : 5) .EQ. 'CHAR*') THEN
                        COLTYP(LOOP) = CAT__TYPEC

                        CURTYP(1 : 5) = '     '

                        LSTAT = CAT__OK
                        CALL CHR_CTOI (CURTYP, FCSIZE, LSTAT)
                        IF (LSTAT .EQ. CAT__OK) THEN
                           COLCSZ(LOOP) = FCSIZE
                        ELSE
                           BADDET = .TRUE.
                           GOTTYP = .FALSE.
                        END IF
                     ELSE
                        BADDET = .TRUE.
                        GOTTYP = .FALSE.

                     END IF
                  END DO


*
*             If the line is a list of external formats then set the
*             'got external formats' flag and copy the external formats.

               ELSE IF (UPBUFF(1 : 17) .EQ. '#COLUMN-FORMATS:') THEN
                  GOTFMT = .TRUE.

                  DO LOOP = 1, NCOL
                     COLFMT(LOOP) = COLITM(LOOP)
                  END DO

               END IF
            END IF
         END IF

*
*       Report any warning.

         IF (BADDET) THEN
            WRNLEN = 0
            WRNMSG = ' '

            CALL CHR_PUTC ('*Warning* ignored bad column details: ',
     :        WRNMSG, WRNLEN)

            COLPOS = CHR_INDEX(BUFFER, ':')
            IF (COLPOS .GT. 0) THEN
               CALL CHR_PUTC (BUFFER(1 : COLPOS), WRNMSG, WRNLEN)
            END IF

            CALL CHR_PUTC ('...', WRNMSG, WRNLEN)

            CALL CAT1_MSG (' ', WRNMSG(1 : WRNLEN), STATUS)
         END IF

      END IF

      END
