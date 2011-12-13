      SUBROUTINE CAT1_DCITM (ITEM, PRSOK, PRSMSG, NAME, VALUE, STATUS)
*+
*  Name:
*     CAT1_DCITM
*  Purpose:
*     Decompose an item into its name and value.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_DCITM (ITEM; PRSOK, PRSMSG; NAME, VALUE; STATUS)
*  Description:
*     Decompose an item into its name and value.
*
*     An item has the syntax 'name=value'.  Spaces are not allowed
*     on either side of the equals sign.  The value may optionally
*     be enclosed in either single or double quotes, which are removed.
*  Arguments:
*     ITEM  =  CHARACTER*(*) (Given)
*        The item to be decomposed.
*     PRSOK  =  LOGICAL (Given and Returned)
*        Flag indicating whether the record parsed ok, coded as follows:
*        .TRUE.  -  the record is ok,
*        .FALSE. -  a parse error has been encountered.
*     PRSMSG  =  CHARACTER*(*) (Given and Returned)
*        The message associated with any parse error.
*     NAME  =  CHARACTER*(*) (Returned)
*        Name of the decomposed item.
*     VALUE  =  CHARACTER*(*) (Returned)
*        Value of the decomposed item.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the item is not completely blank then
*       Take a copy of the item.
*       Remove any leading spaces.
*       Attempt to locate an equal sign.
*       If an equal sign was found then
*         Copy the string before the equal to the name.
*         If the name string is blank then
*           Set a parse error.
*         end if
*         Copy the string after the blank to the value.
*         Remove any leading and trailing quotes from the value.
*         If the value string is not blank then
*           Remove any leading spaces.
*         else
*           Set a parse error.
*         end if
*       else
*         Set a parse error: omitted equals sign.
*       end if
*     else
*       Set a parse error: blank item.
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
*     4/7/96  (ACD): Original version.
*     28/8/96 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal  "      "    .
*  Arguments Given:
      CHARACTER
     :  ITEM*(*)
*  Arguments Given and Returned:
      LOGICAL
     :  PRSOK
      CHARACTER
     :  PRSMSG*(*)
*  Arguments Returned:
      CHARACTER
     :  NAME*(*),
     :  VALUE*(*)
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  WRKITM*(CAT1__SZDRC)  ! Copy of ITEM (which may be modified).
      INTEGER
     :  WRKLEN,  ! Length of WRKITM (excl. trail. blanks).
     :  VALLEN,  !   "    "  VALUE  ( "  .   "  .   "   ).
     :  EQLPOS,  ! Position of '=' in WRKITM.
     :  START,   ! Start position of name or value in ITEM.
     :  STOP     ! Stop     "     "   "   "    "   "   "  .
*.

      IF (STATUS .EQ. CAT__OK) THEN

C        print2000, item(1 : 30)
C2000    format(1x, 'CAT1_DCITM: ', a30)

*
*       Check that the item is not completely blank.

         IF (ITEM .NE. ' ') THEN

*
*          Take a copy of the item (to avoid modifying the calling
*          argument).

            WRKITM = ITEM

*
*          Remove any leading spaces.

            CALL CHR_LDBLK (WRKITM)

            WRKLEN = CHR_LEN(WRKITM)

*
*          Attempt to locate an equal sign and proceed if one was found.

            EQLPOS = INDEX(WRKITM, '=')

            IF (EQLPOS .GT. 0) THEN

*
*             Copy the string before the equal sign to the name and
*             set a parse error if the name is blank.

               START = 1
               STOP = MAX(1, EQLPOS-1)

               NAME = WRKITM(START : STOP)

               IF (NAME .EQ. ' ') THEN
                  PRSOK = .FALSE.
                  PRSMSG = 'missing name'
               END IF

*
*             Copy the string after the equal sign to the value,
*             removing any leading and trailing blanks.  Set a parse
*             error if the value is blank.

               START = MIN(EQLPOS+1, WRKLEN)
               STOP = WRKLEN

               VALUE = WRKITM(START : STOP)

               IF (VALUE(1 : 1) .EQ. ''''  .OR.  VALUE(1 : 1) .EQ. '"')
     :           THEN
                  VALUE(1 : 1) = ' '
               END IF

               CALL CHR_LDBLK (VALUE)

               IF (VALUE .NE. ' ') THEN
                  VALLEN = CHR_LEN(VALUE)
               ELSE
                  VALLEN = 1
               END IF

               IF (VALUE(VALLEN : VALLEN) .EQ. ''''  .OR.
     :             VALUE(VALLEN : VALLEN) .EQ. '"') THEN
                  VALUE(VALLEN : VALLEN) = ' '
               END IF

               IF (VALUE .EQ. ' ') THEN
                  PRSOK = .FALSE.
                  PRSMSG = 'missing value'
               END IF

            ELSE
               PRSOK = .FALSE.
               PRSMSG = 'missing equals sign'
            END IF

         ELSE
            PRSOK = .FALSE.
            PRSMSG = 'blank item'
         END IF

      END IF

      END
