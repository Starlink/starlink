       SUBROUTINE SST_HLPAP( FIRST, LAST, STATUS )
*+
*  Name:
*     SST_HLPAP

*  Purpose:
*     Output help information on the parameters of an ADAM A-task.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_HLPAP( FIRST, LAST, STATUS )

*  Description:
*     The routine sends the body of a prologue section describing the
*     parameters of an ADAM A-task to the output file in help file
*     format.  The body of each individual parameter sub-section is
*     output in paragraph mode.  Each parameter is a level 3 help
*     topic.

*  Arguments:
*     FIRST = INTEGER (Given)
*        First line number in the SCB to be processed.
*     LAST = INTEGER (Given)
*        Last line number in the SCB to be processed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The parameter names are assumed to occur at the start of the
*     header line for each parameter subsection and to be separated from
*     the rest of the line by an '=' sign.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify it under
*     the terms of the GNU General Public License as published by the Free Software
*     Foundation; either version 2 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,but WITHOUT ANY
*     WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
*     PARTICULAR PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License along with
*     this program; if not, write to the Free Software Foundation, Inc., 59 Temple
*     Place,Suite 330, Boston, MA  02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-DEC-1989 (RFWS):
*        Original version.
*     14-AUG-1990 (RFWS):
*        Added a call to SST_HLPKY to format a help key from each
*        parameter name as it appears in the prologue.
*     15-AUG-1990 (RFWS):
*        Changed to output level 3 help keys, instead of level 2.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'SST_PAR'          ! SST_ constants

*  Global Variables:
      INCLUDE 'SST_SCB'          ! SST_ Source Code Buffer

*  Arguments Given:
      INTEGER FIRST
      INTEGER LAST

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      CHARACTER * ( SST__SZLIN ) KEY ! Help key string
      INTEGER F                  ! First line of subsection
      INTEGER FC                 ! Saved first character position
      INTEGER HEADER             ! Number of sub-section header line
      INTEGER L                  ! Last line of subsection
      INTEGER LC                 ! Last character of parameter name
      INTEGER NC                 ! No. characters in help key

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start searching for sub-sections at the line before the first line
*  to be processed and set the first character position of this line
*  (if it exists) to beyond the end of the line, so that the FIRST line
*  will always be found initially. Save the first character position
*  for restoration later.
      HEADER = FIRST - 1
      IF ( FIRST .GT. 1 ) THEN
         FC = SCB_FC( FIRST - 1 )
         SCB_FC( FIRST - 1 ) = SST__SZLIN + 1
      ENDIF

*  Loop to process each sub-section in the main section body.
1     CONTINUE                   ! Start of 'DO WHILE' loop

*  Find the next section.
      CALL SST_FSECT( HEADER, F, L, STATUS )
      IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :     ( HEADER .NE. 0 ) .AND.
     :     ( HEADER .LE. LAST ) ) THEN
         L = MIN( L, LAST )

*  Find the last character in the parameter name (actually, the last
*  character before the '=' sign).
         LC = INDEX( SCB_LINE( HEADER )( SCB_FC( HEADER ) :
     :                                   SCB_LC( HEADER ) ) // '=',
     :               '=' ) + SCB_FC( HEADER ) - 2

*  Extract the parameter name, convert it to a help key and output it
*  as a level 3 help topic key.
         NC = LC - SCB_FC( HEADER ) + 1
         KEY( : NC ) = SCB_LINE( HEADER )( SCB_FC( HEADER ) : LC )
         CALL SST_HLPKY( KEY( : NC ), STATUS )
         NC = MAX( 1, CHR_LEN( KEY( : NC ) ) )
         CALL SST_PUT( 0, '3 ' // KEY( : NC ), STATUS )

*  Output the header line itself.
         CALL SST_PUT( 0,
     :                 SCB_LINE( HEADER )( SCB_FC( HEADER ) :
     :                                     SCB_LC( HEADER ) ),
     :                 STATUS )

*  Output the body of the sub-section in paragraph mode.
         CALL SST_PUTP( 3, F, L, STATUS )

*  Return to get the next parameter sub-section.
         GO TO 1
      END IF

*  Restore the first character position which was saved previously.
      IF ( FIRST .GT. 1 ) SCB_FC( FIRST - 1 ) = FC

      END
* @(#)sst_hlpap.f   1.1   94/12/05 11:31:28   96/07/05 10:27:30
