      SUBROUTINE CAT6_CKNME (CNAME, STATUS)
*+
*  Name:
*     CAT6_CKNME
*  Purpose:
*     Check that a tab-separated table column name conforms to CAT rules.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_CKNME (CNAME; STATUS)
*  Description:
*     Check that a tab-separated table column name conforms to CAT rules.
*
*     Tab-separated table column names often contain characters which
*     are not permitted in CAT column names.  Such illegal characters
*     are replaced by an underscore and a valid CAT name generated.
*     tab-separated tables cannot contain vector columns, so only the
*     simple case of CAT scalar columns need be handled.
*  Arguments:
*     CNAME  =  CHARACTER*(*) (Given)
*        Column name.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the name is not completely blank then
*       Remove any leading blanks.
*       Take a copy (for use in warning messages).
*       If the first character is not alphabetic then
*         Inset an alphabetic character at the start of the name.
*         Set the name modified flag.
*       end if
*       For each character
*         If the character is not either alphabetic or numeric then
*           Replace the character with an underscore.
*           Set the name modified flag.
*         end if
*       end for
*       If the name was modified then
*         Report a message (not an error).
*       end if
*     else
*       Report an error.
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
*     21/5/99 (ACD): Original version (loosely derived from CAT1_DCNME).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'          ! External CAT constants.
      INCLUDE 'CAT_ERR'          ! CAT error codes.
*  Arguments Given:
      CHARACTER
     :  CNAME*(*)
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      INTEGER CHR_LEN
      LOGICAL CHR_ISALM
      LOGICAL CHR_ISALF
*  Local Variables:
      LOGICAL
     :  MODIFY     ! Flag; has the name been modified?
      INTEGER
     :  LOOP,      ! Loop index.
     :  LCNAME,    ! Length of CNAME   (excl. trail. blanks).
     :  LNAMEC,    !   "    "  CNAMEC  ( "  .   "  .   "   ).
     :  LONAME,    !   "    "  ONAME   ( "  .   "  .   "   ).
     :  MSGLEN     !   "    "  MESSGE  ( "  .   "  .   "   ).
      CHARACTER
     :  ONAME*(CAT__SZCMP),  ! Copy of column name.
     :  CNAMEC*(CAT__SZCMP), !  "   "    "     "  .
     :  MESSGE*75            ! Message text.
*.

      IF (STATUS .EQ. CAT__OK) THEN

         MODIFY = .FALSE.

*
*       Check whether the name is not completely blank and proceed
*       if not.

         IF (CNAME .NE. ' ') THEN

*
*          Remove any leading blanks.  Save the resulting string as a
*          copy of the original name to be used in any warning messages.

            CALL CHR_LDBLK (CNAME)
            ONAME = CNAME


*
*          If the first character is not alphabetic then insert an
*          'A' as the first character of the name.  This procedure
*          is necesary in order to form a valid CAT column name.

            IF (.NOT. CHR_ISALF(CNAME(1 : 1) ) ) THEN
               CNAMEC = ' '
               LNAMEC = 0

               CALL CHR_PUTC ('A', CNAMEC, LNAMEC)

               LCNAME = CHR_LEN(CNAME)
               CALL CHR_PUTC (CNAME(1 : LCNAME), CNAMEC, LNAMEC)

               CNAME = CNAMEC

               MODIFY = .TRUE.
            END IF

*
*          Check if the name contains illegal characters and if so
*          replace them with an underscore.  Only letters and numbers are
*          permitted.

            LCNAME = CHR_LEN (CNAME)

            DO LOOP = 1, LCNAME
               IF (.NOT. CHR_ISALM(CNAME(LOOP : LOOP) ) ) THEN
                  MODIFY = .TRUE.
                  CNAME(LOOP : LOOP) = '_'
               END IF
            END DO

*
*          If the name has been modifed then report a message.

            IF (MODIFY) THEN
               MESSGE = ' '
               MSGLEN = 0

               CALL CHR_PUTC ('Invalid column name ', MESSGE, MSGLEN)

               LONAME = CHR_LEN(ONAME)
               CALL CHR_PUTC (ONAME(1 : LONAME), MESSGE, MSGLEN)

               CALL CHR_PUTC (' changed to ', MESSGE, MSGLEN)

               LCNAME = CHR_LEN(CNAME)
               CALL CHR_PUTC (CNAME(1 : LCNAME), MESSGE, MSGLEN)

               CALL CHR_PUTC ('.', MESSGE, MSGLEN)

               CALL CAT1_MSG (' ', MESSGE(1 : MSGLEN), STATUS)
            END IF

         ELSE

*
*          The name is completely blank; set the status and report an
*          error.

            STATUS = CAT__INVCD

            CALL CAT1_ERREP ('CAT6_CKNME_ERR', 'Blank column name '/
     :        /'encountered.', STATUS)

         END IF

      END IF

      END
