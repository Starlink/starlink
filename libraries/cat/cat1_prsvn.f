      SUBROUTINE CAT1_PRSVN (VNAME, CNAME, VELEM, STATUS)
*+
*  Name:
*     CAT1_PRSVN
*  Purpose:
*     Parse a vector element name.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_PRSVN (VNAME; CNAME, VELEM; STATUS)
*  Description:
*     Parse a vector element name to yield the component name and
*     vector element.  A vector element name comprises the name of
*     component followed, without spaces, by the element number
*     enclosed in square brackets.  For example: MAG[5].  In this
*     example the routine would return with CNAME = 'MAG' and VELEM = 5.
*
*     Note that if the parse fails then the return status is set but
*     NO ERROR IS REPORTED.  It is assumed that the calling routine
*     will handle this condition.
*  Arguments:
*     VNAME  =  CHARACTER (Given)
*        The vector element name to be parsed.
*     CNAME  =  CHARACTER (Returned)
*        The component name for the vector element.
*     VELEM  =  INTEGER (Returned)
*        The element number for the vector element.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the name is not completely blank then
*       Check that the last character is ']'
*       If ok then
*         Attempt to locate the '['
*         If ok then
*           If the '[' is not at the start of the string then
*             Extract the component name.
*             If the '[' and ']' are not adjacent then
*               Attempt to decode their contents as an integer number.
*             else
*               Set the return status.
*             end if
*           else
*             Set the return status.
*           end if
*         else
*           Set the return status.
*         end if
*       else
*         Set the return status.
*       end if
*     else
*       Set the return status.
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     2/2/94 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      CHARACTER
     :  VNAME*(*)
*  Arguments Returned:
      CHARACTER
     :  CNAME*(*)
      INTEGER
     :  VELEM
*  Status:
      INTEGER STATUS               ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  LOPEN,   ! Element in VNAME corresponding to '['.
     :  LCLOSE,  !    "    "    "         "       "  ']'.
     :  LVNAME,  ! Length of VNAME (excl. trail. blanks).
     :  LOOP,    ! Loop index.
     :  LSTAT    ! Local ADAM status.
      LOGICAL
     :  FOUND    ! Flag: has '[' been found?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check that the name is not completely blank.

         IF (VNAME .NE. ' ') THEN

*
*          Check that the last character is ']'.

            LVNAME = CHR_LEN (VNAME)

            IF (VNAME(LVNAME : LVNAME) .EQ. ']') THEN
               LCLOSE = LVNAME

*
*             Attempt to locate the opening '['.

               FOUND = .FALSE.

               DO LOOP = 1, LVNAME
                  IF (.NOT. FOUND) THEN
                     IF (VNAME(LOOP : LOOP) .EQ. '[') THEN
                        FOUND = .TRUE.
                        LOPEN = LOOP
                     END IF
                  END IF
               END DO

               IF (FOUND) THEN

*
*                Check that the '[' is not at the start of the string.

                  IF (LOPEN .GT. 1) THEN

*
*                   Extract the component name.

                     CNAME = VNAME(1 : LOPEN - 1)

*
*                   Check that the '[' and ']' are not adjacent.

                     IF (LOPEN .LT. LCLOSE - 1) THEN

*
*                      Attempt to decode the string between the '[' and
*                      ']' as an integer.

                        LSTAT = CAT__OK
                        CALL CHR_CTOI (VNAME(LOPEN + 1 : LCLOSE - 1),
     :                    VELEM, LSTAT)

                        IF (LSTAT .NE. CAT__OK) THEN
                           STATUS = CAT__INVEC
                        END IF

                     ELSE
                        STATUS = CAT__INVEC

                     END IF

                  ELSE
                     STATUS = CAT__INVEC

                  END IF

               ELSE
                  STATUS = CAT__INVEC

               END IF

            ELSE
               STATUS = CAT__INVEC

            END IF

         ELSE
            STATUS = CAT__INVEC

         END IF

      END IF

      END
