      SUBROUTINE CAT1_CXFMT (FNAME, FXFMT, FDTYPE, FCSIZE, FMTOK,
     :  STATUS)
*+
*  Name:
*     CAT1_CXFMT
*  Purpose:
*     Check that an external format is valid for a given data type.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_CXFMT (FNAME, FXFMT, FDTYPE, FCSIZE; FMTOK; STATUS)
*  Description:
*     Check that an external format is valid for a given data type.
*     If the format is not valid then a warning message is issued.
*  Arguments:
*     FNAME  =  CHARACTER*(*) (Given)
*        Name of the column.
*     FXFMT  =  CHARACTER*(*) (Returned)
*        External format for the column.
*     FDTYPE  =  INTEGER (Given)
*        Data type of the column.
*     FCSIZE  =  INTEGER (Given)
*        The size of a character data type, otherwise not used.
*     FMTOK  =  LOGICAL (Returned)
*        Flag indicating whether the data type is ok or not, coded as
*        follows:
*        .TRUE.  - external format is ok,
*        .FALSE. - external format is not ok.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise the 'format ok' flag to ok.
*     If the format is not completely blank then
*       Take an upper case copy.
*       For each data type
*         If the format does not contain required by a format for this
*         data type then
*           Set the 'format ok' flag to not ok.
*         end if
*       end for
*     else
*       Set the 'format ok' flag to not ok.
*     end if
*     If the format is not ok then
*       Assemble and report a message.
*     end if
*  Implementation Deficiencies:
*     The checks performed in this version of the routine are fairly
*     rudimentary.
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
*     20/7/00 (ACD): Original version.
*     22/6/01 (ACD): Added 'G' format specifiers for data types REAL
*        and DOUBLE PRECISION.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'      ! External CAT constants.
*  Arguments Given:
      CHARACTER
     :  FNAME*(*),
     :  FXFMT*(*)
      INTEGER
     :  FDTYPE,
     :  FCSIZE
*  Arguments Returned:
      LOGICAL
     :  FMTOK
*  Status:
      INTEGER STATUS         ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  UFXFMT*(CAT__SZEXF), ! Upper case copy of external format.
     :  WRNMSG*75  ! Buffer for warning message.
      INTEGER
     :  WRNLEN,    ! Length of WRNMSG (excl. trail. blanks).
     :  LFNAME,    !   "    "  FNAME  ( "  .   "  .   "   ).
     :  LFXFMT,    !   "    "  FXFMT  ( "  .   "  .   "   ).
     :  LETPOS,    ! Position of given letter in external format.
     :  LETPO2,    !    "     "    "     "    "     "       "   .
     :  LETPO3     !    "     "    "     "    "     "       "   .
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the 'format ok' flag to ok.

         FMTOK = .TRUE.

*
*       Check that the format is not completely blank.

         IF (FXFMT .NE. ' ') THEN

*
*          Take an upper case copy.

            UFXFMT = FXFMT
            CALL CHR_UCASE (UFXFMT)

*
*          For each data type check that the letter required by the
*          data type occurs in the external format and set the 'format
*          ok' flag to FALSE if it does not.
*
*          Note that as a check that the format is valid this approach
*          is fairly rudimentary ('necessary but not sufficient').

            IF (FDTYPE .EQ. CAT__TYPEUB) THEN
               LETPOS = INDEX(UFXFMT, 'I')
               IF (LETPOS .LE. 0) THEN
                  FMTOK = .FALSE.
               END IF

            ELSE IF (FDTYPE .EQ. CAT__TYPEB) THEN
               LETPOS = INDEX(UFXFMT, 'I')
               IF (LETPOS .LE. 0) THEN
                  FMTOK = .FALSE.
               END IF

            ELSE IF (FDTYPE .EQ. CAT__TYPEUW) THEN
               LETPOS = INDEX(UFXFMT, 'I')
               IF (LETPOS .LE. 0) THEN
                  FMTOK = .FALSE.
               END IF

            ELSE IF (FDTYPE .EQ. CAT__TYPEW) THEN
               LETPOS = INDEX(UFXFMT, 'I')
               IF (LETPOS .LE. 0) THEN
                  FMTOK = .FALSE.
               END IF

            ELSE IF (FDTYPE .EQ. CAT__TYPEI) THEN
               LETPOS = INDEX(UFXFMT, 'I')
               IF (LETPOS .LE. 0) THEN
                  FMTOK = .FALSE.
               END IF

            ELSE IF (FDTYPE .EQ. CAT__TYPER) THEN
               LETPOS = INDEX(UFXFMT, 'F')
               LETPO2 = INDEX(UFXFMT, 'E')
               LETPO3 = INDEX(UFXFMT, 'G')
               IF (LETPOS .LE. 0  .AND.  LETPO2 .LE. 0  .AND.
     :           LETPO3 .LE. 0) THEN
                  FMTOK = .FALSE.
               END IF

            ELSE IF (FDTYPE .EQ. CAT__TYPED) THEN
               LETPOS = INDEX(UFXFMT, 'F')
               LETPO2 = INDEX(UFXFMT, 'D')
               LETPO3 = INDEX(UFXFMT, 'G')
               IF (LETPOS .LE. 0  .AND.  LETPO2 .LE. 0  .AND.
     :           LETPO3 .LE. 0) THEN
                  FMTOK = .FALSE.
               END IF

            ELSE IF (FDTYPE .EQ. CAT__TYPEL) THEN
               LETPOS = INDEX(UFXFMT, 'L')
               IF (LETPOS .LE. 0) THEN
                  FMTOK = .FALSE.
               END IF

            ELSE IF (FDTYPE .EQ. CAT__TYPEC) THEN
               LETPOS = INDEX(UFXFMT, 'A')
               IF (LETPOS .LE. 0) THEN
                  FMTOK = .FALSE.
               END IF

            ELSE
               FMTOK = .FALSE.

            END IF

         ELSE

*
*          The format is completely blank; set the flag to 'not ok'.

            FMTOK = .FALSE.

         END IF

*
*       If the format is not ok then assemble and report a message.

         IF (.NOT. FMTOK) THEN
            WRNMSG = ' '
            WRNLEN = 0

            CALL CHR_PUTC ('Column ', WRNMSG, WRNLEN)

            IF (FNAME .NE. ' ') THEN
               LFNAME = CHR_LEN(FNAME)
               CALL CHR_PUTC (FNAME(1 : LFNAME), WRNMSG, WRNLEN)
            ELSE
               CALL CHR_PUTC ('<blank>', WRNMSG, WRNLEN)
            END IF

            CALL CHR_PUTC (': bad external format (', WRNMSG, WRNLEN)

            IF (FXFMT .NE. ' ') THEN
               LFXFMT = CHR_LEN(FXFMT)
               CALL CHR_PUTC (FXFMT(1 : LFXFMT), WRNMSG, WRNLEN)
            ELSE
               CALL CHR_PUTC ('<blank>', WRNMSG, WRNLEN)
            END IF

            CALL CHR_PUTC (') ignored.', WRNMSG, WRNLEN)

            CALL CAT1_MSG (' ', WRNMSG(1 : WRNLEN), STATUS)

         END IF

      END IF

      END
