      SUBROUTINE COF_ASTWN( FC, INDF, STATUS )
*+
*  Name:
*     COF_ASTWN

*  Purpose:
*     Displays any AST warning messages stored in the supplied FitsChan.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_ASTWN( FC, INDF, STATUS )

*  Description:
*     The AST library can store warning messages in a FitsChan in the
*     form of header cards with the keyword "ASTWARN". This routine
*     searches for such cards and displays them nicely if any are found.

*  Arguments:
*     FC = INTEGER (Given)
*        The AST pointer to the FitsChan.
*     INDF = INTEGER (Given)
*        The NDF identifier for the NDF being created.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2000, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-JAN-2000 (DSB):
*        Original version.
*     10-SEP-2004 (TIMJ):
*        Remove unused variable NDFNAM
*        Initialise HEADER variable
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Arguments Given:
      INTEGER FC
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER HEADER*80        ! A FITS header
      CHARACTER MESS*1024        ! The warning message
      CHARACTER VALUE*80         ! A FITS keyword value
      INTEGER HLEN               ! Used length of string
      INTEGER IAT                ! Used length of MESS
      INTEGER START              ! Index of start of string
      LOGICAL REPORT             ! Have we a message to report?
      LOGICAL WARNED             ! Has the user been warned?
*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Indicate no warnings have yet been issued. */
      WARNED = .FALSE.

*  Indicate there is no message to report yet.
      REPORT = .FALSE.
      MESS = ' '
      IAT = 0

*  Initialise some variables
      HEADER = ' '

*  Rewind the FitsChan so that the following search starts with the first
*  card.
      CALL AST_CLEAR( FC, 'Card', STATUS )

*  Search the FItsChan for cards containing messages issued by AST.
      DO WHILE( AST_FINDFITS( FC, 'ASTWARN', HEADER, .TRUE., STATUS ) )

*  Extract the keyword value from the header card (minus delimiting quotes).
         VALUE = ' '
         START = INDEX( HEADER, '''' )
         IF( START .NE. 0 ) THEN
            HLEN = CHR_LEN( HEADER )
            IF( HEADER( HLEN : HLEN ) .EQ. '''' ) THEN
               IF( START + 1 .LE. HLEN - 1 ) THEN
                  VALUE = HEADER( START + 1 : HLEN - 1 )
               END IF
            END IF
         END IF

*  Remove leading spaces and find the used length of the value.
         CALL CHR_LDBLK( VALUE )
         HLEN = CHR_LEN( VALUE )

*  If the value is not blank, append it to the total message, followed by
*  a single space. Indicate we have a message to send.
         IF( HLEN .GT. 0 ) THEN
            CALL CHR_APPND( VALUE( : HLEN ), MESS, IAT )
            IAT = IAT + 1
            REPORT = .TRUE.

*  Otherwise, display the current total message. Prepend the name of the
*  NDF to the first non-blank warning.
         ELSE
            CALL MSG_SETC( 'T', MESS )

            IF( .NOT. WARNED .AND. IAT .GT. 0 )  THEN
               CALL NDF_MSG( 'NDF', INDF )
               CALL MSG_OUT( 'COF_ASTWN_MSG1', '''^NDF'' - ^T',
     :                       STATUS )
               WARNED = .TRUE.

            ELSE
               CALL MSG_OUT( 'COF_ASTWN_MSG2', '^T', STATUS )
            END IF

            MESS = ' '
            IAT = 0
            REPORT = .FALSE.

         END IF

      END DO

*  If we still have a message to report, report it now.
      IF( REPORT .AND. IAT .GT. 0 ) THEN
         CALL MSG_SETC( 'T', MESS( : IAT ) )

         IF( .NOT. WARNED )  THEN
            CALL NDF_MSG( 'NDF', INDF )
            CALL MSG_OUT( 'COF_ASTWN_MSG3', '''^NDF'' - ^T', STATUS )
            WARNED = .TRUE.

         ELSE
            CALL MSG_OUT( 'COF_ASTWN_MSG4', '^T', STATUS )
         END IF

      END IF

*  Display a blank line if any warnings were reported.
      IF( WARNED ) CALL MSG_BLANK( STATUS )

      END
