      SUBROUTINE KPG1_GILST( LONUM, UPNUM, MAXLIN, PARAM, FLAG, NUMBER,
     :                       NDISP, STATUS )
*+
*  Name:
*     KPG1_GILST

*  Purpose:
*     Selects integers within a range of values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GILST( LONUM, UPNUM, MAXLIN, PARAM, FLAG, NUMBER,
*                      NDISP, STATUS )

*  Description:
*     This routine selects integer numbers from LONUM to UPNUM. The
*     routine gets a character string from the user and parses it to
*     extract the specified numbers. The default selection is the all
*     integers within the range LONUM to UPNUM.

*  Arguments:
*     LONUM = INTEGER (Given)
*        The lower limit of the selection range.
*     UPNUM = INTEGER (Given)
*        The upper limit of the selection range.
*     MAXLIN = INTEGER (Given)
*        The max. number of numbers can be selected.
*     PARAM = CHARACTER*(*) (Given)
*        The name of the parameter used to get the number specification
*        from the user. It can be a list, separated by commas,
*        comprising any reasonable combination of the following formats:
*
*           ALL or * - All integers between 1 and NUM
*
*           xx,yy,zz - A list of integers.
*
*           xx:yy - All integers between xx and yy inclusively. When xx
*                   is omitted the range begins from 1, when yy is
*                   omitted the range ends with UPNUM.
*
*        Any number can be specified more than once but it has the same
*        effect as just entering it once.
*     FLAG( LONUM : UPNUM ) = INTEGER (Returned)
*        A temporary flag array used to flag the number which has been
*        selected.
*     NUMBER( MAXLIN ) = INTEGER (Returned)
*        The selected numbers.
*     NDISP = INTEGER (Returned)
*        The number of selected numbers.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (DSB):
*     {enter_new_authors_here}

*  History:
*     10-MAY-1991 (WG):
*        Original version.
*     1991 June 18 (MJC):
*        Renamed from GTLINE, renamed the LININD argument to NUMBER and
*        the error reports.
*     1991 July 8 (MJC):
*        Increased the size of the item buffer and allowed for * as wild
*        card.
*     12-AUG-1999 (DSB):
*        Use ":" instead of "-" as a range specifier. This is to avoid
*        confusion with negative pixel indices.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER LONUM
      INTEGER UPNUM
      INTEGER FLAG( LONUM : UPNUM )
      INTEGER MAXLIN
      CHARACTER*(*) PARAM

*  Arguments Returned:
      INTEGER NUMBER( MAXLIN )
      INTEGER NDISP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
       INTEGER CHR_LEN           ! The used length of a string

*  Local Variables:
       INTEGER BERNGI            ! Begin number of a range
       LOGICAL BELG              ! Flag showing begin number is omitted
       INTEGER EDRNGI            ! End number of a range
       LOGICAL EDLG              ! Flag showing end number is omitted
       LOGICAL GOT               ! Flag showing got valid string
       INTEGER I, J              ! Do-loop indices
       INTEGER ITMBGN            ! The begin position of an item
       INTEGER ITMEND            ! The end position of an item
       LOGICAL MORE              ! Flag showing more item left
       CHARACTER * ( 20 ) SCT    ! An item of the string SELIST
       INTEGER SCTLN             ! Used length of the string SCT
       INTEGER SCTNUM            ! Number specified by SCT
       CHARACTER * ( 128 ) SELIST ! The number specification string
       INTEGER SELN              ! Used length of string SELIST
       INTEGER TMP               ! A temporary buffer for swapping

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new error context.
      CALL ERR_MARK

*  Enter a do loop until a valid specification is got.
      GOT = .FALSE.
      DO WHILE ( .NOT.GOT )

*  Initially, no number has been selected so initialise the flags as 0.
         DO I = LONUM, UPNUM
            FLAG( I ) = 0
         END DO

*  Get a specification string from the user.
         CALL PAR_GET0C( PARAM, SELIST, STATUS )
         GOT = .TRUE.

*  Remove its leading blank and get its used length.
         CALL CHR_LDBLK( SELIST )
         SELN = CHR_LEN( SELIST )

*  Check error, if so, exit.
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  The items of the string are separated by comma, extract them one
*  be one until no element left.
         ITMBGN = 1
         MORE = .TRUE.
         DO WHILE ( MORE )

*  Find the relative end position of this item.
            ITMEND = INDEX( SELIST( ITMBGN : ), ',' )

*  Find the absolute end position of this item.
            IF ( ITMEND .NE. 0 ) THEN
               ITMEND = ITMBGN + ITMEND - 2

*  Since the last item does not end with comma, its end position should
*  be the end of the menu list. And in this case no more item left.
            ELSE
               ITMEND = SELN
               MORE = .FALSE.
            END IF

*  Extract the item from the string.
            SCT = SELIST( ITMBGN : ITMEND )

*  Convert to upper case, strip leading blank and get its used length.
            CALL CHR_UCASE( SCT )
            CALL CHR_LDBLK( SCT )
            SCTLN = CHR_LEN( SCT )

*  Get the begin position of next item.
            ITMBGN = ITMEND + 2

*  Parse this element according to its type.
*  =========================================

*  If 'ALL' or '*' is specified, select all numbers between LONUM
*  and UPNUM
            IF ( ( INDEX( 'ALL' , SCT( : SCTLN ) ) .EQ. 1 ) .OR.
     :           ( SCT( : SCTLN ) .EQ. '*' ) ) THEN
               DO J = LONUM, UPNUM
                  FLAG( J ) = 1
               END DO

*  If a range is defined, get its begin and end limits.
            ELSE IF ( INDEX( SCT, ':' ) .NE. 0 ) THEN
               CALL KPG1_RGLMT( SCT, ':', BERNGI, BELG, EDRNGI, EDLG,
     :                          STATUS )

*  Check status, if error, report the error and go to error processing.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL MSG_SETC( 'SCT', SCT )
                  CALL ERR_REP( 'KPG1_GILST_ERRLIN',
     :              'Something is wrong within / ^SCT /.', STATUS )
                  GOTO 99
               END IF

*  If the begin number has been omitted, set it as 1.
               IF ( BELG ) BERNGI = LONUM

*  If the end number has been omitted, set it as NUM.
               IF ( EDLG ) EDRNGI = UPNUM

*  If any of the limit number beyond the selectable range, report a
*  message and go to error processing.
               IF ( BERNGI .LT. LONUM .OR. BERNGI .GT. UPNUM .OR.
     :              EDRNGI .LT. LONUM .OR. EDRNGI .GT. UPNUM ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'LONUM', LONUM )
                  CALL MSG_SETI( 'UPNUM', UPNUM )
                  CALL ERR_REP( 'KPG1_GILST_ERRLIN', 'Only numbers '/
     :                         /'between ^LONUM and ^UPNUM are '/
     :                         /'selectable.', STATUS )
                  GOTO 99
               END IF

*  If range limits are given in wrong order, swap them round.
               IF ( BERNGI .GT. EDRNGI ) THEN
                  TMP = BERNGI
                  BERNGI = EDRNGI
                  EDRNGI = TMP
               END IF

*  Set the flag for number between the range as 1.
               DO J = BERNGI, EDRNGI
                  FLAG( J ) = 1
               END DO

*  If neither 'ALL', nor range is specified, this element must be a
*  number as long as its used length is not 0. Convert it to a
*  numeric value.
            ELSE IF ( SCTLN .NE. 0 ) THEN
               CALL CHR_CTOI( SCT( : SCTLN ), SCTNUM, STATUS )

*  If error happens when converting, something must wrong within this
*  element. Output an error message, and go to error processing.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL MSG_SETC( 'SCT', SCT )
                  CALL ERR_REP( 'KPG1_GILST_ERRLIN',
     :              'There is something wrong within / ^SCT /.',
     :              STATUS )
                  GOTO 99
               END IF

*  If the given number is outside selectable range, Output an error
*  message, and go to error processing.
               IF ( SCTNUM .LT. LONUM .OR. SCTNUM .GT. UPNUM ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'LONUM', LONUM )
                  CALL MSG_SETI( 'UPNUM', UPNUM )
                  CALL ERR_REP( 'KPG1_GILST_ERRLIN',
     :              'Only numbers between ^LONUM and ^UPNUM are '/
     :              /'selectable.', STATUS )
                  GOTO 99
               END IF

*  This number is eligible, set flag for it.
               FLAG( SCTNUM ) = 1
            END IF

*  Go back to parse the next item until no more item left.
         END DO

*  The selected numbers have been flagged; note these numbers in NUMBER.
         NDISP = 0
         DO I = LONUM, UPNUM
            IF ( FLAG( I ) .EQ. 1 ) THEN
               NDISP = NDISP + 1
               IF ( NDISP .GT. MAXLIN ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'KPG1_GILST_EXNUM',
     :              'Too many numbers selected.', STATUS )
                  GOTO 99
               END IF
               NUMBER( NDISP ) = I
            END IF
         END DO

 99      CONTINUE

*  If a bad string is inputed, cancel the parameter for in-input, flush
*  out the error message and reset the status.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PAR_CANCL( PARAM, STATUS )
            CALL ERR_FLUSH( STATUS )
            GOT = .FALSE.
         END IF

*  Go back to re-input a number specification string.
      END DO

*  Release the error context.
      CALL ERR_RLSE

 999  CONTINUE

      END
