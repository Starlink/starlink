      SUBROUTINE CAT1_NMUNQ (CI, NAME, STATUS)
*+
*  Name:
*     CAT1_NMUNQ
*  Purpose:
*     Check whether a column, parameter or expression name is unique.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_NMUNQ (CI; NAME; STATUS)
*  Description:
*     Check whether a column, parameter or expression name is unique
*     in a catalogue and if not then attempt to generate a unique
*     name.  The number of attempts to generate unique name permitted
*     is equal to the local constant MAXTRY.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     NAME  =  CHARACTER*(*) (Given and Returned)
*        Name to be checked.  If the given name is not unique then
*        an attempt is made to return a unique name.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise the number of attempts.
*     While more attempts are required
*       Increment the number of attempts.
*       Generate the current name.
*       Check whether this name exists.
*       If the name does not exist then
*         The name is unique; set the termination flag.
*       else
*         If this is the last attempt then
*           Set the termination flag.
*           Set the status; duplicate column name.
*         end if
*       end if
*     end do
*     If the status is ok then
*       If the name has changed then
*         Adopt the new name.
*       end if
*     end if
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     16/12/99 (ACD): Original version.
*     8/5/01   (ACD): Modified so that the generated name cannot attempt
*       to exceed the maximum name length.
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
     :  CI
*  Arguments Given and Returned:
      CHARACTER
     :  NAME*(*)
*  Status:
      INTEGER STATUS           ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      INTEGER MAXTRY           ! Maximum permitted number of attempts
      PARAMETER (MAXTRY = 10)  ! to generate a new name.
*
      INTEGER MXGNNM           ! Maximum length if the generated name is not
      PARAMETER (MXGNNM = CAT__SZCMP - 3)  ! to fit in the permitted size.
*  Local Variables:
      INTEGER
     :  CURTRY,  ! Current try at generating a unique name.
     :  LNAME,   ! Length of NAME  (excl. trail. blanks).
     :  LWNAME,  !   "    "  WNAME ( "  .   "  .   "   ).
     :  NUMSFX   ! Numerical suffix to the new name.
      CHARACTER
     :  WNAME*(CAT__SZCMP)     ! Current working name.
      LOGICAL
     :  MORE,    ! Flag; more attempts to generate a unique name?
     :  EXIST    ! Flag; does the current name exist?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the number of attempts.

         CURTRY = 0

*
*       Attempt to generate a unique name.

         MORE = .TRUE.

         DO WHILE (MORE)
            CURTRY = CURTRY + 1

*
*          Generate the current name.  The given name is used for the
*          first attempt.

            IF (CURTRY .EQ. 1) THEN
               WNAME = NAME

            ELSE
               WNAME = ' '
               LWNAME = 0

               IF (NAME .NE. ' ') THEN
                  LNAME = CHR_LEN(NAME)
                  LNAME = MIN(LNAME, MXGNNM)
               ELSE
                  LNAME = 1
               END IF

               CALL CHR_PUTC (NAME(1 : LNAME), WNAME, LWNAME)
               CALL CHR_PUTC ('_', WNAME, LWNAME)

               NUMSFX = CURTRY - 1
               CALL CHR_PUTI (NUMSFX, WNAME, LWNAME)

            END IF

*
*          Check whether this name exists.

            CALL CAT1_NMCHK (CI, WNAME, EXIST, STATUS)
C           print2000, curtry, wname, exist, status
C 2000       format(5x, 'curtry, wname, exist, status: ',
C    :        i3, 1x, a, l5, i20)

*
*          If the name does not exist then it is unique and looping
*          can terminate.  If it does exist then check whether the
*          maximum permitted number of attempts have been made and
*          if so then set the status and termination flag.

            IF (.NOT. EXIST) THEN
               MORE = .FALSE.

            ELSE
               IF (CURTRY .GE. MAXTRY) THEN
                  MORE = .FALSE.
                  STATUS = CAT__DUPNM
               END IF
            END IF
         END DO

*
*       If the status is ok and the name has changed then adopt the new
*       name.

         IF (STATUS .EQ. CAT__OK) THEN
            IF (NAME .NE. WNAME) THEN
               NAME = WNAME
            END IF
         END IF

      END IF

      END
