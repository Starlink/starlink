      SUBROUTINE NDF_HECHO( NLINES, TEXT, STATUS )
*+
*  Name:
*     NDF_HECHO

*  Purpose:
*     Write out lines of history text.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_HECHO( NLINES, TEXT, STATUS )

*  Description:
*     The routine writes a series of lines of text to the standard
*     output channel, indented by three spaces. It is provided as a
*     default service routine which may be passed as an argument to
*     NDF_HOUT in order to display NDF history information.
*
*     The specification of this routine may be used as a template when
*     writing alternative service routines for use by NDF_HOUT.

*  Arguments:
*     NLINES = INTEGER (Given)
*        Number of lines of text to be written.
*     TEXT( NLINES ) = CHARACTER * ( * ) (Given)
*        Array of text lines.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     Before passing this (or a similar) routine as an argument to
*     NDF_HOUT the calling routine should declare it in a Fortran
*     EXTERNAL statement.

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
*     Copyright (C) 1993, 1994 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     7-MAY-1993 (RFWS):
*        Original version.
*     16-NOV-1994 (RFWS):
*        Fixed bug: leading blanks on history lines were being removed
*        during assignment to a message token.
*     10-FEB-2009 (TIMJ):
*        Do not need to use MSG_FMTC simply to prepend 3 spaces to the output.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NLINES
      CHARACTER * ( * ) TEXT( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for text lines

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Mark the message/error stack. This prevents use of MSG_ routines
*  here from interfering with previously-defined message tokens, etc.
      CALL ERR_MARK

*  Loop to write out each line of text.
      DO 1 I = 1, NLINES

*  Assign each line to a message token and write out a message
*  containing the translation of the token.  This prevents any
*  "special" characters in the line from being interpreted.
         CALL MSG_SETC( 'LINE', TEXT( I ) )
         CALL MSG_OUT( ' ', '   ^LINE', STATUS )

*  Quit if an error occurs.
         IF ( STATUS .NE. SAI__OK ) GO TO 2
 1    CONTINUE
 2    CONTINUE

*  Release the message/error stack.
      CALL ERR_RLSE

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_HECHO_ERR',
     :   'NDF_HECHO: Error writing out lines of history text.', STATUS )
         CALL NDF1_TRACE( 'NDF_HECHO', STATUS )
      END IF

      END
