      SUBROUTINE CNTHLT( NCONT, CNTLEV, CNTUSD, STATUS )
*+
*  Name:
*     CNTHLT

*  Purpose:
*     List the contour heights that have been used.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CNTHLT( NCONT, CNTLEV, CNTUSD, STATUS )

*  Description:
*     The routine takes a list of contour heights and flags that
*     indicate whether a given height has been used (or is required),
*     and reports the values to the user via the message system.

*  Arguments:
*     NCONT = INTEGER (Given)
*        The number of contour heights.
*     CNTLEV( NCONT ) = REAL (Given)
*        The contour heights.
*     CNTUSD( NCONT ) = LOGICAL (Given)
*        If true a contour height is to be reported.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The messages use ADAM parameters HEIGHT_LIST and HEIGHT_TITLE.

*  Algorithm:
*     -  Find the first and last heights used, watching out for special
*        cases like only one height supplied.
*     -  For each used height append the (converted) height to a buffer,
*        append commas between values unless it is the last height.
*     -  Continue to append heights until more than 60 characters have
*        been used, then report the line of heights, and start a new
*        line in the buffer.

*  Prior Requirements:
*     -  The contour levels and the flags should be sorted into order.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 Mar 30 (MJC):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER
     :  NCONT

      REAL
     :  CNTLEV( NCONT )

      LOGICAL
     :  CNTUSD( NCONT )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER
     :  LIMCH                    ! The number of characters before a
                                 ! line is output when there are still
                                 ! values to report.
      PARAMETER( LIMCH = 60 )

*  Local Variables:
      INTEGER
     :  FIRST,                   ! Index of first height used
     :  J,                       ! Loop counter
     :  LAST,                    ! Index of first height used
     :  NCH                      ! Number of characters used in the
                                 ! message buffer

      CHARACTER
     :  BUFFER*80                ! Message buffer

*.

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find the first and last used height from the flags, since the
*    heights are in ascending order.

*    First height was used.

      IF ( CNTUSD( 1 ) ) THEN
         FIRST = 1

*       Last is also defined if only one contour height was selected.

         IF ( NCONT .EQ. 1 ) THEN
            LAST = 1
         ELSE

            LAST = 2

*          Otherwise loop until the flag switches to off. Make sure
*          the second test is done first to prevent a bounds error.

            DO WHILE ( CNTUSD( LAST ) .AND. ( LAST .LE. NCONT ) )
               LAST = LAST + 1
            END DO

*          Go back to the last required height.

            LAST = LAST - 1
         END IF

*    First height chosen was not actually used.

      ELSE
         FIRST = 1

*       Loop until the flag switches to "used".  Make sure the second
*       test is done first to prevent a bounds error.

         DO WHILE ( .NOT. CNTUSD( FIRST ) .AND. ( FIRST .LE. NCONT ) )
            FIRST = FIRST + 1
         END DO

*       Only one height used.

         IF ( FIRST .EQ. NCONT ) THEN
            LAST = FIRST
         ELSE
            LAST = FIRST

*          Otherwise loop until the flag switches to off. Make sure
*          the second test is done first to prevent a bounds error.

            DO WHILE ( CNTUSD( LAST ) .AND. ( LAST .LE. NCONT ) )
               LAST = LAST + 1
            END DO

*          Go back to the last required height.

            LAST = LAST - 1
         END IF
      END IF

*    Report the heights to the user.

      CALL MSG_OUT( 'HEIGHT_TITLE', 'Contour heights used:', STATUS )

*    Start with a new line of heights.  Therefore initialise the
*    buffer pointer.

      NCH = 0

*    Loop for each height.

      DO J = FIRST, LAST

*       Form the output string by concatenating the heights into a
*       buffer.

         CALL CHR_PUTR( CNTLEV( J ), BUFFER, NCH )

*       Put in commas to divide the list, terminated by a full stop.

         IF ( J .EQ. LAST ) THEN
            CALL CHR_PUTC( '.', BUFFER, NCH )
         ELSE
            CALL CHR_PUTC( ',   ', BUFFER, NCH )
         END IF

*       Write out a line of heights if at least 60 characters have
*       already been used, or its the last height to report.

         IF ( NCH .GT. LIMCH .OR. J .EQ. LAST ) THEN
            CALL MSG_OUT( 'HEIGHT_LIST', BUFFER( :NCH ), STATUS )

*          Start a new line (will be ignored if there are no more
*          heights to report).

            NCH = 0
         END IF
      END DO

      END
