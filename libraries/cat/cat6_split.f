      SUBROUTINE CAT6_SPLIT (BUFFER, SPLITC, MFIELD, NFIELD, FIELDS,
     :  NULFLG, STATUS)
*+
*  Name:
*     CAT6_SPLIT
*  Purpose:
*     Split a string into a number of fields.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_SPLIT (BUFFER, SPLITC, MFIELD; NFIELD, FIELDS, NULFLG;
*       STATUS)
*  Description:
*     Split a string into a number of fields.  The character used to
*     split the string is passed as an argument.
*
*     Consecutive occurences of the split character are taken to
*     imply a field between them containing a null value.
*  Arguments:
*     BUFFER  =  CHARACTER*(*) (Given)
*        String to be split.
*     SPLITC  =  CHARACTER*1 (Given)
*        Character to be used to split the string.
*     MFIELD  =  INTEGER (Given)
*        Maximum permitted number of fields into which the string may be
*        split.
*     NFIELD  =  INTEGER (Returned)
*        Number of fields found in the string.
*     FIELDS(MFIELD)  =  CHARACTER*(*) (Returned)
*        Fields found in the string.
*     NULFLG(MFIELD)  =  LOGICAL (Returned)
*        Null value flags for the individual fields.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the string is not blank then
*       For every character
*         If the current character is the split character then
*           If there is space then
*             Increment the number of fields.
*             If the current field is not of zero length then
*               Copy the field.
*               Set the null value flag to false.
*             else
*               Set the field to blank.
*               Set the null value flag to true.
*             end if
*           end if
*         end if
*       end for
*       Handle the last field.
*     else
*       Set the number of fields to zero.
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
*     19/5/99 (ACD): Original version.
*     21/5/99 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'          ! External CAT constants.
*  Arguments Given:
      CHARACTER
     :  BUFFER*(*),
     :  SPLITC*1
      INTEGER
     :  MFIELD
*  Arguments Returned:
      INTEGER
     :  NFIELD
      CHARACTER
     :  FIELDS(MFIELD)*(*)
      LOGICAL
     :  NULFLG(MFIELD)
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  LENBUF,   ! Length of BUFFER (excl. trail. blanks).
     :  LOOP,     ! Loop index.
     :  START,    ! Start position of current field.
     :  STOP      ! Stop     "     "     "      "  .
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check that the string is not blank.

         IF (BUFFER .NE. ' ') THEN

*
*          Determine the length of the string and examine every character.

            NFIELD = 0
            START = 1

            LENBUF = CHR_LEN(BUFFER)

            DO LOOP = 1, LENBUF

*
*             Check whether the current character is the split character.

               IF (BUFFER(LOOP : LOOP) .EQ. SPLITC) THEN

*
*                Check if there is space for another field.

                  IF (NFIELD .LE. MFIELD) THEN
                     NFIELD = NFIELD + 1

*
*                   Copy the current field if it is not of zero length.
*                   A zero length field corresponds to two consecutive
*                   split characters and a null value.

                     STOP = LOOP - 1

                     IF (STOP .GE. START) THEN
                        FIELDS(NFIELD) = BUFFER(START : STOP)
                        NULFLG(NFIELD) = .FALSE.
                     ELSE
                        FIELDS(NFIELD) = ' '
                        NULFLG(NFIELD) = .TRUE.
                     END IF
                  END IF

                  START = LOOP + 1
               END IF
            END DO

*
*          Handle the last word.

            IF (NFIELD .LT. MFIELD) THEN
               NFIELD = NFIELD + 1

               IF (LENBUF .GE. START) THEN
                  FIELDS(NFIELD) = BUFFER(START : LENBUF)
                  NULFLG(NFIELD) = .FALSE.
               ELSE
                  FIELDS(NFIELD) = ' '
                  NULFLG(NFIELD) = .TRUE.
               END IF
            END IF

         ELSE

*
*          The string is blank; set the number of fields to zero.

            NFIELD = 0

         END IF

      END IF

      END
