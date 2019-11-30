      SUBROUTINE SUBPAR_CONVALS ( FIRST, LAST, TYPE, STRING, STATUS )
*+
*  Name:
*     SUBPAR_CONVALS

*  Purpose:
*     Convert a set of stored values to characters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_CONVALS ( FIRST, LAST, TYPE, STRING, STATUS )

*  Description:
*     Convert a set of values from internal list storage of the
*     indicated data type into characters, and concatenate them into a
*     string, separated by quotes. This is the form required if they are
*     to be displayed to a user.

*  Arguments:
*     FIRST=INTEGER (given)
*        Position of first required value in the list
*     LAST=INTEGER (given)
*        Position of last required value in the list
*     TYPE=INTEGER (given)
*        Encoded data-type of the required list
*     STRING=CHARACTER*(*) (returned)
*        String containing list of values separated by commas.
*     STATUS=INTEGER

*  Algorithm:
*     The parameter system contains the arrays CHARLIST, REALLIST,
*     INTLIST, INT64LIST, DOUBLELIST and LOGLIST. These are used for storing
*     values associated with parameters, such as static and dynamic defaults
*     and constraints for RANGE and IN. CONVALS provides a mechanism for
*     extracting a set of values from one of these arrays and formatting
*     the values into a form suitable for display to a user.

*  Copyright:
*     Copyright (C) 1984, 1985, 1986, 1987, 1988, 1991, 1992, 1993 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     02-OCT-1984 (BDK):
*        Original
*     16-APR-1985 (BDK):
*        change syntax of logical defaults
*     25-MAR-1986 (BDK):
*        improve format of REALs
*     08-SEP-1986 (BDK):
*        give more precision for REALs
*     20-JUL-1987 (BDK):
*        new syntax, prefix HDSnames by @
*     18-AUG-1987 (BDK):
*        take @ out again - causes trouble with graphics
*        devices
*     09-NOV-1987 (BDK):
*        avoid addressing outside STRING
*     18-FEB-1987 (BDK):
*        put @ back again
*     13-DEC-1988 (AJC):
*        improve to character conversion - use CHR
*     22-JUL-1991 (AJC):
*        remove unused declaration  ISTAT
*     19-AUG-1992 (AJC):
*        bracket arrays to ensure CHAR param values interpreted OK
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     27-NOV-1996 (AJC):
*        Use SUBPAR_ENQUOTE to quote string
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PAR'

*  Arguments Given:
      INTEGER FIRST                  ! index to first required item in array
      INTEGER LAST                   ! index to last required item in array
      INTEGER TYPE                   ! data type of array

*  Arguments Returned:
      CHARACTER*(*) STRING           ! string to contain values

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*  Local Variables:
      INTEGER START                       ! pointer to end of used part
                                          ! of output string
      INTEGER J                           ! loop counter
      INTEGER SLEN                        ! length of STRING
      INTEGER LENGTH                      ! used length of CHARLIST item
      CHARACTER*(SUBPAR__STRLEN+2) TCHAR  ! temporary character string
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Take the values from the list array of the relevant data type,
*   convert them into character strings and concatenate them into the
*   output string, each value being inserted with a trailing comma. Then
*   remove the final (superfluous) comma.
      SLEN = LEN(STRING)

*   Insert the opening [ if required
      IF ( LAST .GT. FIRST ) THEN
         STRING(1:1) = '['
         START = 1
      ELSE
         START = 0
      ENDIF

      IF ( TYPE .EQ. SUBPAR__REAL ) THEN

         DO J = FIRST, LAST
*          Put the number followed by comma into the string
             CALL CHR_PUTR( REALLIST(J), STRING, START)
             CALL CHR_PUTC( ',', STRING, START)
         ENDDO

      ELSE IF ( TYPE .EQ. SUBPAR__INTEGER ) THEN

         DO J = FIRST, LAST
            CALL CHR_PUTI( INTLIST(J), STRING, START)
            CALL CHR_PUTC( ',', STRING, START)
         ENDDO

      ELSE IF ( TYPE .EQ. SUBPAR__INT64 ) THEN

         DO J = FIRST, LAST
            CALL CHR_PUTK( INT64LIST(J), STRING, START)
            CALL CHR_PUTC( ',', STRING, START)
         ENDDO

      ELSE IF ( TYPE .EQ. SUBPAR__DOUBLE ) THEN

         DO J = FIRST, LAST
            CALL CHR_PUTD( DOUBLELIST(J), STRING, START)
            CALL CHR_PUTC( ',', STRING, START)
         ENDDO

      ELSE IF ( TYPE .EQ. SUBPAR__CHAR ) THEN

         DO J = FIRST, LAST
            LENGTH = CHR_LEN(CHARLIST(J))
            CALL SUBPAR_ENQUOTE(
     :         CHARLIST(J)(1:LENGTH), TCHAR, LENGTH, STATUS )
            CALL CHR_PUTC( TCHAR(1:LENGTH)//',',
     :       STRING, START)
         ENDDO

      ELSE IF ( TYPE .EQ. SUBPAR__LOGICAL ) THEN

         DO J = FIRST, LAST
            IF ( LOGLIST(J) ) THEN
               CALL CHR_PUTC( 'YES,', STRING, START )
            ELSE
               CALL CHR_PUTC( 'NO,', STRING, START )
            ENDIF
         ENDDO

      ELSE IF ( TYPE .GE. 20 ) THEN

*      Data structure names
         DO J = FIRST, LAST
            LENGTH = CHR_LEN(CHARLIST(J))
            CALL CHR_PUTC ('@'//CHARLIST(J)(1:LENGTH)//',',
     :       STRING, START)
         ENDDO

      ENDIF

*   Now overwrite the last comma and fill with blanks
      IF ((START .GT. 1) .AND. ( START .LT. SLEN )) THEN
*       Pointer is within string
         IF ( LAST .GT. FIRST ) THEN
            STRING(START:) = ']'
         ELSE
            STRING(START:) = ' '
         ENDIF

      ELSEIF ( START .EQ. SLEN ) THEN
*       There were too many values to fit in STRING
*       The last may be truncated.
         STRING ( SLEN:SLEN ) = '#'

      ELSE
*       Should never happen
         STRING = '###'
      ENDIF

      END
