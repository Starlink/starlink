      SUBROUTINE CAT1_DCRCD (BUFFER, MXITEM, PRSOK, PRSMSG, NITEMS,
     :  ITMNAM, ITMVAL, CODE, COMM, STATUS)
*+
*  Name:
*     CAT1_DCRCD
*  Purpose:
*     Decode a record read from the description file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_DCRCD (BUFFER, MXITEM; PRSOK, PRSMSG, NITEMS,
*       ITMNAM, ITMVAL; CODE, COMM, STATUS)
*  Description:
*     Decode a record read from the description file.
*  Arguments:
*     BUFFER  =  CHARACTER*(*) (Given)
*        Buffer holding the record read from the description file.
*     MXITEM  =  INTEGER (Given)
*        The maximum permitted number of items which may be specified.
*     PRSOK  =  LOGICAL (Given and Returned)
*        Flag indicating whether the record parsed ok, coded as follows:
*        .TRUE.  -  the record is ok,
*        .FALSE. -  a parse error has been encountered.
*     PRSMSG  =  CHARACTER*(*) (Given and Returned)
*        The message associated with any parse error.
*     NITEMS  =  INTEGER (Given and Returned)
*        The number of items for colums, parameters and directives.
*     ITMNAM(MXITEM)  =  CHARACTER*(*) (Given and Returned)
*        The names of the items.
*     ITMVAL(MXITEM)  =  CHARACTER*(*) (Given and Returned)
*        The values of the items.
*     CODE  =  CHARACTER*(*) (Returned)
*        Code defining the type of the record.  The permitted values are:
*        C  -  column,
*        P  -  parameter,
*        D  -  directive,
*        :  -  continuation,
*        B  -  'BEGINTABLE',
*        !  -  comment (here includes blank lines and text lines).
*     COMM  =  CHARACTER*(*) (Returned)
*        Any in-line comments for columns and parameters.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise the in-line comments.
*     If the record is not completely blank then
*       Remove any leading spaces.
*       If the line is not a comment then
*         Locate, copy and remove any in-line comments.
*         Decompose the string into its constituent items.
*         Force the first item into upper case.
*         If the type is 'C' or 'P' then
*           If there are four or more items then
*             Set the code to 'C' or 'P' as appropriate.
*             Copy input items two to four to the mandatory items.
*             Decompose any remaining items.
*             Initialise the number of items.
*           else
*             Set the parse error flag and message.
*           end if
*         else if the type is 'D' then
*           Set the code to 'D'.
*           Decompose all the items.
*           Initialise the number of items.
*         else if the type is ':' (that is, a continuation) then
*           Set the code to ':'
*           Decompose all the items.
*         else of the type is 'B' (for 'BEGINTABLE') then
*           Set the code to 'B'.
*         else
*             Set the parse error flag and message; undecodable line.
*         end if
*       else
*         Set the return code to comment.
*       end if
*     else
*       Set the return code to comment.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     3/7/96  (ACD): Original version.
*     30/7/96 (ACD): First stable version.
*     6/12/96 (ACD): Added support for 'KAPPA format' small text lists.
*     6/11/97 (ACD): Added proper initialisation of the in-line
*        comments.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      CHARACTER
     :  BUFFER*(*)
      INTEGER
     :  MXITEM
*  Arguments Given and Returned:
      LOGICAL
     :  PRSOK
      CHARACTER
     :  PRSMSG*(*),
     :  ITMNAM(MXITEM)*(*),
     :  ITMVAL(MXITEM)*(*)
      INTEGER
     :  NITEMS
*  Arguments Returned:
      CHARACTER
     :  CODE*(*),
     :  COMM*(*)
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      INTEGER MAXITM             ! Maximum permitted number of items.
      PARAMETER (MAXITM = 20)
*  Local Variables:
      INTEGER
     :  LOOP,     ! Loop index.
     :  NSQUOT,   ! Number of single quotes.
     :  NDQUOT,   ! Number of double quotes.
     :  WRKLEN,   ! Length of WRKBUF (excl. trail. blanks).
     :  COMPOS,   ! Position of comment character.
     :  RITEM     ! Number of items decoded from record.
      CHARACTER
     :  WRKBUF*(CAT1__SZDRC),  ! Work buffer with modified copy of record.
     :  INCOMM*(CAT__SZCOM),   ! Input comments.
     :  RITEMS(MAXITM)*(CAT__SZANM + CAT__SZVAL + 1)  ! Decoded items.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the in-line comments.

         COMM = ' '
         INCOMM = ' '

*
*       Proceed if the record is not completely blank.

         IF (BUFFER .NE. ' ') THEN

*
*          Remove any leading spaces and proceed if the line is not
*          a comment.  Note that here a text line (beginning with 'T',
*          't', '#T', or '#t') is considered a comment.

            CALL CHR_LDBLK (BUFFER)

            IF (BUFFER(1 : 1) .NE. '!'   .AND.
     :          BUFFER(1 : 1) .NE. 'T'   .AND.
     :          BUFFER(1 : 1) .NE. 't'   .AND.
     :          BUFFER(1 : 2) .NE. '# '  .AND.
     :          BUFFER(1 : 2) .NE. '#T'  .AND.
     :          BUFFER(1 : 2) .NE. '#t')  THEN

*
*             Trap and remove the '#' used in KAPPA format STLs.

               WRKBUF = BUFFER

               IF (WRKBUF(1 : 1) .EQ. '#') THEN
                  WRKBUF(1 : 1) = ' '
                  CALL CHR_LDBLK (WRKBUF)
               END IF

               WRKLEN = CHR_LEN(WRKBUF)

*
*             Locate, copy and remove any in-line comments.  Comment
*             delimiters inside strings are ignored.  Note that a copy
*             of the input string is taken to avoid further modifying
*             the original.

               NSQUOT = 0
               NDQUOT = 0
               COMPOS = 0

               DO LOOP = 1, WRKLEN
                  IF (WRKBUF(LOOP : LOOP) .EQ. '!'  .OR.
     :                WRKBUF(LOOP : LOOP) .EQ. '#') THEN
                     IF (MOD(NSQUOT, 2) .EQ. 0  .AND.
     :                   MOD(NDQUOT, 2) .EQ. 0  .AND.
     :                   COMPOS .EQ. 0) THEN
                        COMPOS = LOOP
                     END IF
                  ELSE IF (WRKBUF(LOOP : LOOP) .EQ. '''') THEN
                     NSQUOT = NSQUOT + 1
                  ELSE IF (WRKBUF(LOOP : LOOP) .EQ. '"') THEN
                     NDQUOT = NDQUOT + 1
                  END IF
               END DO

               IF (COMPOS .GT. 0) THEN
                  INCOMM = ' '
                  INCOMM = WRKBUF(COMPOS+1 : WRKLEN)
                  CALL CHR_LDBLK (INCOMM)

                  WRKBUF(COMPOS : WRKLEN) = ' '
               END IF

*
*             Decompose the remaining record into its constituent items.

               CALL CAT1_DMRCD (WRKBUF, MAXITM, PRSOK, PRSMSG, RITEM,
     :           RITEMS, STATUS)

*
*             Force the first item into upper case.

               CALL CHR_UCASE (RITEMS(1) )

*
*             Check for the various permitted types of record and
*             proceed appropriately.  First check for a column or
*             parameter.

               IF (RITEMS(1)(1 : 1) .EQ. 'C'  .OR.
     :             RITEMS(1)(1 : 1) .EQ. 'P') THEN

*
*                Check that the line contains at least the three items
*                which are mandatory for columns and parameters.

                  IF (RITEM .GE. 4) THEN

*
*                   Set the return code to 'C' or 'P' as appropriate.

                     CODE = RITEMS(1)(1 : 1)

*
*                   Copy the mandatory items to the return arguments.

                     ITMNAM(1) = 'NAME'
                     ITMVAL(1) = RITEMS(2)

                     ITMNAM(2) = 'DTYPE'
                     ITMVAL(2) = RITEMS(3)

                     ITMNAM(3) = 'VALPOS'
                     ITMVAL(3) = RITEMS(4)

*
*                   Decompose any remaining items.

                     DO LOOP = 5, RITEM
                        CALL CAT1_DCITM (RITEMS(LOOP), PRSOK, PRSMSG,
     :                    ITMNAM(LOOP-1), ITMVAL(LOOP-1), STATUS)
                     END DO

*
*                   Set the number of items.

                     NITEMS = RITEM - 1

*
*                   Set the return comments.

                     COMM = INCOMM

                  ELSE
                     PRSOK = .FALSE.
                     PRSMSG = 'mandatory items missing from column or '/
     :                 /'parameter'

                  END IF

               ELSE IF (RITEMS(1)(1 : 1) .EQ. 'D') THEN

*
*                The record is a set of directives; set the return
*                code.

                  CODE = 'D'

*
*                Decompose all the items.

                  DO LOOP = 2, RITEM
                     CALL CAT1_DCITM (RITEMS(LOOP), PRSOK, PRSMSG,
     :                 ITMNAM(LOOP-1), ITMVAL(LOOP-1), STATUS)
                  END DO

                  NITEMS = RITEM - 1

               ELSE IF (RITEMS(1)(1 : 1) .EQ. ':') THEN

*
*                The record is a continuation; set the return code.

                  CODE = ':'

*
*                Decompose all the items.

                  DO LOOP = 2, RITEM
                     CALL CAT1_DCITM (RITEMS(LOOP), PRSOK, PRSMSG,
     :                 ITMNAM(NITEMS+LOOP-1), ITMVAL(NITEMS+LOOP-1),
     :                 STATUS)
                  END DO

                  NITEMS = NITEMS + RITEM - 1

               ELSE IF (RITEMS(1)(1 : 1) .EQ. 'B') THEN

*
*                The record is a 'BEGINTABLE'; set the return code.

                  CODE = 'B'

               ELSE
                  PRSOK = .FALSE.
                  PRSMSG = 'undecodable line'

               END IF
            ELSE
               CODE = '!'

            END IF

         ELSE
            CODE = '!'

         END IF

      END IF

      END
