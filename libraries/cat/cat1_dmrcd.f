      SUBROUTINE CAT1_DMRCD (BUFFER, MAXITM, PRSOK, PRSMSG, NITEM,
     :  ITMLST, STATUS)
*+
*  Name:
*     CAT1_DMRCD
*  Purpose:
*     Decompose a record into its constituent items.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_DMRCD (BUFFER, MAXITM; PRSOK, PRSMSG; NITEM, ITMLST;
*       STATUS)
*  Description:
*     Decompose a record into its constituent items.
*
*     Items are identified by being separated by spaces.  However,
*     spaces embedded in (quote-delimited) strings are ignored.
*  Arguments:
*     BUFFER  =  CHARACTER*(*) (Given)
*        Record to be decomposed.
*     MAXITM  =  INTEGER (Given)
*        Maximum permitted number of items into which the record may
*        be decomposed.
*     PRSOK  =  LOGICAL (Given and Returned)
*        Flag indicating whether the record parsed ok, coded as follows:
*        .TRUE.  -  the record is ok,
*        .FALSE. -  a parse error has been encountered.
*     PRSMSG  =  CHARACTER*(*) (Given and Returned)
*        The message associated with any parse error.
*     NITEM  =  INTEGER (Returned)
*        The number of items located in the record.
*     ITMLST(MAXITM)  =  CHARACTER*(*) (Returned)
*        List of items decomposed from the record.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the string is not blank then
*       Determine the length of the string.
*       Set the previous character to ' '.
*       For every character
*         If not inside a string then
*           If the previous character was a space and the current
*           character is not a space then
*             Set the item start position
*           end if
*           If the previous character was not a space and the current
*           character is a space then
*             If there is space for another item
*               Increment the number of items.
*               Copy the item.
*             else
*               Set the parse error flag.
*             end if
*           end if
*         end if
*         If the character is a quote then increment the number of
*         quotes.
*       end for
*       If the last character was not a space (that is, there is a
*       final item to be terminated) then
*         If there is space for another item
*           Increment the number of items.
*           Copy the item.
*         else
*           Set the parse error flag.
*         end if
*       end if
*     else
*       Set the number of items to zero.
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
*     8/7/96  (ACD): Original version.
*     9/7/96  (ACD): First stable version.
*     17/3/00 (ACD): Changed so that the checks for characters being
*        inside quotes are not made in the first three items on new
*        definitions and the first item of continuations.  This
*        check prevents (illegal) quote characters in column and
*        parameter names confusing the parser.
*     19/4/00 (ACD): Fixed bug in checks for characters being inside
*        quotes.
*     14/7/00 (ACD): Really fixed bug in checks for characters being
*        inside quotes.

*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      CHARACTER
     :  BUFFER*(*)
      INTEGER
     :  MAXITM
*  Arguments Given and Returned:
      LOGICAL
     :  PRSOK
      CHARACTER
     :  PRSMSG*(*)
*  Arguments Returned:
      INTEGER
     :  NITEM
      CHARACTER
     :  ITMLST(MAXITM)*(*)
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  BUFLEN,   ! Length of BUFFER (excl. trail. blanks).
     :  LOOP,     ! Loop index.
     :  NSQUOT,   ! Number of single quotes.
     :  NDQUOT,   !   "    "  double   "   .
     :  START     ! Start position of the current item.
      CHARACTER
     :  PRVCHR*1, ! Previous character.
     :  CURCHR*1  ! Current      "    .
      LOGICAL
     :  CHKQUT    ! Flag; check whether character is inside quotes?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check that the string is not completely blank.

         IF (BUFFER .NE. ' ') THEN
C           print4000, buffer
C4000       format(1x, 'buffer: ', a)

*
*          Determine the length of the string.

            BUFLEN = CHR_LEN(BUFFER)

*
*          Set the previous character to blank (' ') and initialise the
*          various counts.

            PRVCHR = ' '

            NSQUOT = 0
            NDQUOT = 0
            NITEM = 0

*
*          Examine every character in the string.

            DO LOOP = 1, BUFLEN
               CURCHR = BUFFER(LOOP : LOOP)

C              print4001, nitem, loop, curchr, nsquot, ndquot
C4001          format(1x, 'nitem, loop, curchr, nsquot, ndquot: ',
C    :           i4, i4, 1x, a1, i4, i4)

*
*             Check whether the character is outside a quote delemited
*             string.

               IF (MOD(NSQUOT, 2) .EQ. 0  .AND.  MOD(NDQUOT, 2) .EQ. 0)
     :           THEN

*
*                If the previous character was a space and the current
*                character is not a space then a new item is starting;
*                set the start position.

                  IF (PRVCHR .EQ. ' '  .AND.  CURCHR .NE. ' ') THEN
                     START = LOOP
                  END IF

*
*                If the previous character was not a space and the
*                current character is then an item is ending.  If there
*                is space for a new item then increment the number of
*                items and add it to the list of items.  Otherwise set
*                the parse status.

                  IF (PRVCHR .NE. ' '  .AND.  CURCHR .EQ. ' ') THEN
                     IF (NITEM .LT. MAXITM) THEN
                        NITEM = NITEM + 1
                        ITMLST(NITEM) = BUFFER(START : LOOP-1)
C                       print4002, nitem, itmlst(nitem)
C4002                   format(1x, 'nitem, itmlst: ', i4, 1x, a)

                     ELSE
                        PRSOK = .TRUE.
                        PRSMSG = 'too many items found.'

                     END IF
                  END IF
               END IF

*
*             If the character is a quote then increment the number of
*             quotes.  Note that separate counts are kept of single and
*             double quotes.
*
*             The first three items of new definitions and the first
*             item of comments are not checked for quotes.

C              print4000, nitem, itmlst(1)(1 : 1)
C4000          format(1x, 'nitem, itmlst: ', i4, 2x, '-', a1, '-')

               IF (NITEM .EQ. 1) THEN
                  IF (ITMLST(1)(1 : 1) .EQ. 'C'  .OR.
     :                ITMLST(1)(1 : 1) .EQ. 'c'  .OR.
     :                ITMLST(1)(1 : 1) .EQ. 'P'  .OR.
     :                ITMLST(1)(1 : 1) .EQ. 'p') THEN
                     CHKQUT = .FALSE.
                  ELSE
                     CHKQUT = .TRUE.
                  END IF
               ELSE
                  CHKQUT = .TRUE.
               END IF

               IF (CHKQUT) THEN
                  IF (CURCHR .EQ. '''') THEN
                     NSQUOT = NSQUOT + 1
                  ELSE IF (CURCHR .EQ. '"') THEN
                     NDQUOT = NDQUOT + 1
                  END IF
               END IF

               PRVCHR = CURCHR
            END DO

*
*          If the last character was not a space (that is, there is a
*          final item to be terminated) add it to the list if there is
*          enough space.

            IF (BUFFER(BUFLEN : BUFLEN) .NE. ' ') THEN
               IF (NITEM .LT. MAXITM) THEN
                  NITEM = NITEM + 1
                  ITMLST(NITEM) = BUFFER(START : LOOP-1)

               ELSE
                  PRSOK = .TRUE.
                  PRSMSG = 'too many items found.'

               END IF
            END IF

         ELSE

*
*          The input string was blank; set the number of items to zero.

            NITEM = 0

         END IF

C        print5000, nitem
C5000    format(// 1x, 'after the show: nitem = ', i5)

C        do loop = 1, nitem
C           print5001, loop, itmlst(loop)
C5001       format(3x, i3, 1x, a)
C        end do

      END IF

      END
