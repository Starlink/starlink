      SUBROUTINE CAT1_DCNME (ANAME, FNAME, FDIM, FSIZE, PRSOK, PRSMSG,
     :  STATUS)
*+
*  Name:
*     CAT1_DCNME
*  Purpose:
*     Decode the name, dimensionality and size of a column.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_DCNME (ANAME; FNAME, FDIM, FSIZE, PRSOK, PRSMSG; STATUS)
*  Description:
*     Decode the name, dimensionality and size of a column, as read
*     from a description file (eg. for a small text list of direct
*     access binary catalogue).
*
*     CAT column names, as read from a description file, have one
*     of the following two forms.
*
*     Scalar columns:   name              example:  HDNUM
*
*     Vector columns:   name(size)        example:  MAG[15]
*
*     The second example is a fifteen element vector.  Arrays of
*     dimensionality higher than one are not supported by CAT.
*
*     It is easy for manually generated description files to contain
*     'illegal' column names, that is names which contain characters
*     other alphabetic characters, numeric characters and the underscore.
*     This routine attempts to detect such names and convert them into
*     permissible names by replacing the offending characters with an
*     underscore.  Two cases are checked for:
*
*     - invalid characters,
*
*     - brackets which are part of the name (such brackets enclose a
*       non-numeric item.
*  Arguments:
*     ANAME  =  CHARACTER*(*) (Given)
*        Column name.
*     FNAME  =  CHARACTER*(*) (Returned)
*        StarBase column.
*     FDIM  =  INTEGER (Returned)
*        Dimensionality of the StarBase column.
*     FSIZE  =  INTEGER (Returned)
*        Size of the StarBase column if it is an array.  If the column
*        is a scalar FSIZE is set to one.
*     PRSOK  =  LOGICAL (Returned)
*        Flag indicating whether the name parsed ok, coded as follows:
*        .TRUE.   -  parsed ok,
*        .FALSE.  -  failed to parse.
*     PRSMSG  =  CHARACTER*(*) (Returned)
*        Message associated with the failure parsing the name.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Note that the algorithm works on a copy of the given name in
*     order to avoid modifying the original.
*
*     If the name is not completely blank then
*       Take a copy of the name (henceforth this copy will be used).
*       Remove any leading blanks.
*       If the first character is not alphabetic then
*         Inset an alphabetic character at the start of the name.
*       end if
*       Determine the length of the name.
*       For each character
*         If the character is not one of: alphabetic, numeric,
*         underscore, '(', or ')' then
*           Set the name modified flag.
*           Replace the character with an underscore.
*         end if
*       end for
*       Remove any right or left brackets ('(' or ')').
*       Decode the string into its constituent words.
*       Extract the first word as the column name.
*       If there is more than one word then
*         Attempt to decode an integer from the second word.
*         If ok then
*           The column is a vector:
*           Set the dimensionality to indicate a vector.
*           Set the size to the number decoded.
*         else
*           The column is a scalar with a bad name:
*           Set the dimensionality to indicate a scalar.
*           Set the name to one.
*           Assemble a name comprising the first and second words,
*           seaprated by underscores.
*       else
*         Set the dimensionality to scalar.
*         Set the size to one.
*       end if
*       If the name was modified then
*         Report a message (not an error).
*       end if
*     else
*       Set the 'parse failed' flag and message.
*       Set the column name to blank.
*       Set the dimensionality to scalar.
*       Set the vector size to one.
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
*     17/7/96  (ACD): Original version (modified version of CAT2_DCNME).
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
     :  ANAME*(*)
*  Arguments Returned:
      CHARACTER
     :  FNAME*(*),
     :  PRSMSG*(*)
      INTEGER
     :  FDIM,
     :  FSIZE
      LOGICAL
     :  PRSOK
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      INTEGER CHR_LEN
      LOGICAL CHR_ISALM
      LOGICAL CHR_ISALF
*  Local Constants:
      INTEGER MXWORD   ! Maximum permitted number of words in name.
      PARAMETER (MXWORD = 2)
*  Local Variables:
      CHARACTER
     :  CNAME*(CAT__SZCMP),  ! Copy of column name.
     :  CNAMEC*(CAT__SZCMP), !  "   "    "     "  .
     :  DNAME*(CAT__SZCMP),  ! Copy of col. name for error messages.
     :  WORDS(MXWORD)*(CAT__SZCMP), ! Words decoded from col. name.
     :  MESSGE*75            ! Message text.
      INTEGER
     :  LCNAME,    ! Length of CNAME   (excl. trail. blanks).
     :  LNAMEC,    !   "    "  CNAMEC  ( "  .   "  .   "   ).
     :  LDNAME,    !   "    "  DNAME   ( "  .   "  .   "   ).
     :  LFNAME,    !   "    "  FNAME   ( "  .   "  .   "   ).
     :  MSGLEN     !   "    "  MESSGE  ( "  .   "  .   "   ).
      INTEGER
     :  LOOP,      ! Loop index.
     :  NWORD,     ! Number of words decoded from col. name.
     :  START(MXWORD), ! Start position of each word in col. name.
     :  STOP(MXWORD),  ! Stop     "     "   "    "   "   " .  "  .
     :  LSTAT,     ! Local status.
     :  SIZE       ! Number decoded from WORD(2)
      LOGICAL
     :  MODIFY     ! Flag; has the name been modified?
*.

      IF (STATUS .EQ. CAT__OK) THEN

         DNAME = ' '
         MODIFY = .FALSE.

         PRSOK = .TRUE.
         PRSMSG = ' '

*
*       Check whether the name is not completely blank and proceed
*       if not.

         IF (ANAME .NE. ' ') THEN

*
*          Take a copy of the name (henceforth this copy will be
*          used in order not to modify the input argument).

            CNAME = ANAME

*
*          Remove any leading blanks.  Save the resulting string as the
*          version of the ADC column name to be used in error messages.

            CALL CHR_LDBLK (CNAME)
            DNAME = CNAME

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
*          replace them with an underscore.  The permitted characters
*          are: letters, numbers, underscore, '[' and ']'.

            LCNAME = CHR_LEN (CNAME)

            DO LOOP = 1, LCNAME
               IF (.NOT. (CHR_ISALM(CNAME(LOOP : LOOP) )  .OR.
     :           CNAME(LOOP : LOOP) .EQ. ']'  .OR.
     :           CNAME(LOOP : LOOP) .EQ. '[' ) ) THEN
                  MODIFY = .TRUE.
                  CNAME(LOOP : LOOP) = '_'
               END IF
            END DO

*
*          Replace any right or left brackets ('[' or ']') with spaces.

            DO LOOP = 1, LCNAME
               IF (CNAME(LOOP : LOOP) .EQ. '['  .OR.
     :           CNAME(LOOP : LOOP) .EQ. ']') THEN
                  CNAME(LOOP : LOOP) = ' '
               END IF
            END DO

*
*          Decode the string into its constituent words (where a word
*          is a contiguous string of non-blank characters separated by
*          one or more blanks).

            CALL CHR_DCWRD (CNAME, MXWORD, NWORD, START, STOP, WORDS,
     :        LSTAT)

*
*          Extract the first word as the StarBase column name.

            FNAME = WORDS(1)

*
*          If there is one word the column is a scalar.  If there are
*          two words it is either a vector or a scalar with a bad name
*          (that is, a name with brackets in it).

            IF (NWORD .GT. 1) THEN

*
*             Attempt to decode a number from the second word.
*             If the attempt succeeds then the name is a vector and
*             the number is its size.  If it fails the column is a
*             scalar with a bad name.

               LSTAT = 0
               CALL CHR_CTOI (WORDS(2), SIZE, LSTAT)

               IF (LSTAT .EQ. 0) THEN
                  FDIM = CAT__VECTR
                  FSIZE = SIZE

               ELSE
                  MODIFY = .TRUE.

                  FDIM = CAT__SCALR
                  FSIZE = 1

                  DO LOOP = 1, LCNAME
                     IF (CNAME(LOOP : LOOP) .EQ. ' ') THEN
                        CNAME(LOOP : LOOP) = '_'
                     END IF
                  END DO

                  FNAME = CNAME
               END IF

            ELSE
               FDIM = CAT__SCALR
               FSIZE = 1

            END IF

*
*          If the name has been modifed then report a message.

            IF (MODIFY) THEN
               MESSGE = ' '
               MSGLEN = 0

               CALL CHR_PUTC ('Invalid column name ', MESSGE, MSGLEN)

               LDNAME = CHR_LEN(DNAME)
               CALL CHR_PUTC (DNAME(1 : LDNAME), MESSGE, MSGLEN)

               CALL CHR_PUTC (' changed to ', MESSGE, MSGLEN)

               LFNAME = CHR_LEN(FNAME)
               CALL CHR_PUTC (FNAME(1 : LFNAME), MESSGE, MSGLEN)

               CALL CHR_PUTC ('.', MESSGE, MSGLEN)

               CALL CAT1_MSG (' ', MESSGE(1 : MSGLEN), STATUS)
            END IF

         ELSE

*
*          The name is completely blank; set the 'parse failed' flag and
*          message.

            PRSOK = .FALSE.
            PRSMSG = 'blank name'

            FNAME = ' '
            FDIM = CAT__SCALR
            FSIZE = 1

         END IF

      END IF

      END
