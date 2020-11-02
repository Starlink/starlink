      SUBROUTINE ASTUNFORMAT( STATUS )
*+
*  Name:
*     ASTUNFORMAT

*  Purpose:
*     Read a formatted coordinate value for a Frame axis

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTUNFORMAT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application reads a formatted coordinate value (given as a
*     literal string) for a Frame axis and displays the equivalent numerical
*     value. It also displays the number of characters read from the string.
*
*     The principle use of this function is in decoding user-supplied
*     input which contains formatted coordinate values. Free-format input
*     is supported as far as possible. If input is ambiguous, it is
*     interpreted with reference to the Frame's attributes (in
*     particular, the Format string associated with the Frame's axis).
*     This function is, in essence, the inverse of AST_FORMAT.

*  Usage:
*     astunformat this axis value result

*  ADAM Parameters:
*     AXIS = _INTEGER (Read)
*         The number of the Frame axis for which unformatting is to be
*         performed (axis numbering starts at 1 for the first axis).
*     DVAL = _DOUBLE (Write)
*         An output parameter left holding the last unformatted value.
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     RESULT = LITERAL (Read)
*        The name of a text file in which to put the unformatted axis
*        values. No file is produced if a null (!) value is supplied. One
*        axis value is stored on each line of the file. [!]
*     THIS = LITERAL (Read)
*        An NDF, FITS file or text file holding the Frame. If an NDF is
*        supplied, the current Frame of the WCS FrameSet will be used. If a
*        FITS file is supplied, the Frame corresponding to the primary axis
*        descriptions will be used.
*     VALUE = GROUP (Read)
*        A comma-separated list of formatted axis values to be read.
*        A text file may be specified by preceeding the name of the file
*        with an up arrow character "^". If the supplied value ends with a
*        minus sign, the user is re-prompted for additional values.

*  Copyright:
*     Copyright (C) 2006 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-NOV-2006 (DSB):
*        Original version.
*     3-DEC-2013 (DSB):
*        Added parameter DVAL.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  External References:
      EXTERNAL AST_ISAFRAME
      INTEGER CHR_LEN

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER THIS, IGRP, NP
      CHARACTER FNAME*255, BUF*255
      INTEGER I, FD, NC, AXIS
      DOUBLE PRECISION VALUE
      LOGICAL LOG
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the Frame.
      CALL KPG1_GTOBJ( 'THIS', 'Frame', AST_ISAFRAME, THIS,
     :                 STATUS )

*  Get the axis index.
      CALL PAR_GDR0I( 'AXIS', 1, 1, AST_GETI( THIS, 'NAXES', STATUS ),
     :                .FALSE., AXIS, STATUS )

*  Get a group holding the input formatted axis values.
      IGRP = GRP__NOID
      CALL KPG1_GTGRP( 'VALUE', IGRP, NP, STATUS )

*  If required, open an output text file.
      LOG = .FALSE.
      CALL PAR_GET0C( 'RESULT', FNAME, STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE

*  We delete any pre-existing file first.
         CALL ATL_RM( FNAME, STATUS )

*  Open a new file and get an FIO identifier for it.
         CALL FIO_OPEN( FNAME, 'WRITE', 'LIST', 132, FD, STATUS )
         IF( STATUS .EQ. SAI__OK ) LOG = .TRUE.

      END IF

*  Loop round the group.
      DO I = 1, NP

*  Get this element.
         CALL GRP_GET( IGRP, I, 1, BUF, STATUS )

*  Unformat the value.
         NC = AST_UNFORMAT( THIS, AXIS, BUF( : CHR_LEN( BUF ) ), VALUE,
     :                      STATUS )

*  Display it to the screen.
         CALL MSG_SETC( 'STRING', BUF )
         CALL MSG_SETD( 'VALUE', VALUE )
         CALL MSG_SETI( 'NC', NC )
         CALL MSG_OUT( ' ', '  ^STRING -> ^VALUE (^NC characters read)',
     :                 STATUS )

*  Write it to the text file if necessary.
         CALL CHR_DTOC( VALUE, BUF, NC )
         IF( LOG ) CALL FIO_WRITE( FD, BUF( : NC ), STATUS )

      END DO

*  Write the last unformatted value to an output parameter.
      CALL PAR_PUT0D( 'DVAL', VALUE, STATUS )

*  Free resources.
      CALL GRP_DELET( IGRP, STATUS )

*  Close the file.
      IF( LOG ) CALL FIO_ANNUL( FD, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTUNFORMAT_ERR', 'Error unformatting an '//
     :                 'axis value.', STATUS )
      END IF

      END
