      SUBROUTINE CAT1_DCTYP (BUFFER, PRSOK, PRSMSG, CODE, STATUS)
*+
*  Name:
*     CAT1_DCTYP
*  Purpose:
*     Decode the type of a record read from the description file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_DCTYP (BUFFER; PRSOK, PRSMSG; CODE; STATUS)
*  Description:
*     Decode the type of a record read from the description file.
*  Arguments:
*     BUFFER  =  CHARACTER*(*) (Given)
*        Buffer holding the record read from the description file.
*     PRSOK  =  LOGICAL (Given and Returned)
*        Flag indicating whether the record parsed ok, coded as follows:
*        .TRUE.  -  the record is ok,
*        .FALSE. -  a parse error has been encountered.
*     PRSMSG  =  CHARACTER*(*) (Given and Returned)
*        The message associated with any parse error.
*     CODE  =  CHARACTER*(*) (Returned)
*        Code defining the type of the record.  The permitted values are:
*        C  -  column,
*        P  -  parameter,
*        D  -  directive,
*        :  -  continuation,
*        B  -  'BEGINTABLE',
*        !  -  comment (here includes blank lines and text lines).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the record is not completely blank then
*       Take a working copy which may be modified.
*       Remove any leading spaces.
*       Force the first character into upper cases.
*       Check for the various permitted cases and set the parse
*       flag and status if none are found.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     9/7/96  (ACD): Original version.
*     28/8/96 (ACD): First stable version.
*     6/12/96 (ACD): Added support for 'KAPPA format' small text lists.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
*  Arguments Given:
      CHARACTER
     :  BUFFER*(*)
*  Arguments Given and Returned:
      LOGICAL
     :  PRSOK
      CHARACTER
     :  PRSMSG*(*)
*  Arguments Returned:
      CHARACTER
     :  CODE*(*)
*  Status:
      INTEGER STATUS          ! Global status
*  Local Constants:
      INTEGER MAXITM          ! Maximum permitted number of items.
      PARAMETER (MAXITM = 20)
*  Local Variables:
      CHARACTER
     :  WRKBUF*(CAT1__SZDRC), ! Work buffer with modified copy of record.
     :  FRSCHR*2              ! First two none blank characters.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Proceed if the record is not completely blank.

         IF (BUFFER .NE. ' ') THEN

*
*          Take a working copy which may be modified and remove any
*          leading spaces.

            WRKBUF = BUFFER
            CALL CHR_LDBLK (WRKBUF)

*
*          Force the first two characters into upper case.

            FRSCHR = WRKBUF(1 : 2)
            CALL CHR_UCASE (FRSCHR)

*
*          Check for the various permitted cases and set the parse
*          flag and status if none are found.  Note that text lines
*          and blank lines are considered comments here.

            IF (FRSCHR(1 : 1) .EQ. '!'   .OR.
     :          FRSCHR(1 : 1) .EQ. 'T'   .OR.
     :          FRSCHR(1 : 2) .EQ. '# '  .OR.
     :          FRSCHR(1 : 2) .EQ. '#T') THEN
               CODE = '!'

            ELSE IF (FRSCHR(1 : 1) .EQ. 'C'  .OR.
     :               FRSCHR(1 : 2) .EQ. '#C') THEN
               CODE = 'C'

            ELSE IF (FRSCHR(1 : 1) .EQ. 'P'  .OR.
     :               FRSCHR(1 : 2) .EQ. '#P') THEN
               CODE = 'P'

            ELSE IF (FRSCHR(1 : 1) .EQ. 'D'  .OR.
     :               FRSCHR(1 : 2) .EQ. '#D') THEN
               CODE = 'D'

            ELSE IF (FRSCHR(1 : 1) .EQ. ':'  .OR.
     :               FRSCHR(1 : 2) .EQ. '#:') THEN
               CODE = ':'

            ELSE IF (FRSCHR(1 : 1) .EQ. 'B'  .OR.
     :               FRSCHR(1 : 2) .EQ. '#B') THEN
               CODE = 'B'

            ELSE
               PRSOK = .FALSE.
               PRSMSG = 'undecodable line'

            END IF

         ELSE
            CODE = '!'

         END IF

      END IF

      END
