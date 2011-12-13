      SUBROUTINE TRA1_ARSTR( NAME, NDIM, DIMS, INDENT, LOGEXM, FD, LINE,
     :                       STATUS )
*+
*  Name:
*     TRA1_ARSTR

*  Purpose:
*     Outputs the name and dimensions of an array of structures.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRA1_ARSTR( NAME, NDIM, DIMS, INDENT, LOGEXM, FD, LINE,
*    :                 STATUS )

*  Description:
*     The given name and dimensions are formatted into a message,
*     indented by the specified number of spaces.  The string
*     {array of structures} is then appended and the message output.
*     The same text may optionally be written to an open ASCII file.

*  Arguments:
*     NAME = CHARACTER * ( DAT__SZNAM ) (Given)
*        Name of the array of structures.
*     NDIM = INTEGER (Given)
*        Dimensionality of the array of structures.
*     DIMS( DAT__MXDIM ) = INTEGER (Given)
*        Array of dimensions for the array of structures.
*     INDENT = INTEGER (Given)
*        Indentation level for the output line of information.
*     LOGEXM = LOGICAL (Given)
*        If true a log of the header records is written to an ASCII
*        file.
*     FD = INTEGER (Given)
*        The ASCII file descriptor, ignored if LOGEXM is false.
*     LINE = CHARACTER * ( * ) (Given and Returned)
*        The line of text to be output.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     If no error on entry then
*        Initialise line with indentation.
*        Append the name.
*        Append the dimensions.
*        Find the last blank character in line and move 5 characters
*          past it.
*        Append "{array of structures}"
*        Output the line as the only token in a message.
*        If output to file required, then write line to the log
*     Endif

*  Prior Requirements:
*     -  The ASCII file associated with FD must already be open.

*  Copyright:
*     Copyright (C) 1984, 1988-1989, 1991 Science & Engineering
*     Research Council. All Rights Reserved.

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
*     DB: Dave Baines (ROE)
*     BDK: Dennis Kelly (ROE)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14/03/1984 (DB):
*        Original version.
*     07/04/1984 (DB):
*        Documentation revised, PUTDIM used.
*     09.02.1988 (BDK):
*        Fix typo.
*     1989 May 10 (MJC):
*        Used {} to delimit commentary instead of <>.
*     1989 May 15 (MJC):
*        Tidy up and added LINE to arguments.
*     1989 Jun 15 (MJC):
*        Renamed from ARRSTR to avoid confusion with the original TRACE
*        version, and added output to log file.
*     1991 January 30 (MJC):
*        Converted to the SST prologue.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! Switch off the default typing

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Environment constants
      INCLUDE 'DAT_PAR'        ! Data-system constants

*  Arguments Given:
      CHARACTER * ( DAT__SZNAM )
     :  NAME                   ! Name of the array of structures

      INTEGER
     :  NDIM,                  ! Dimensionality of the array of
                               ! structures
     :  DIMS( DAT__MXDIM ),    ! Array of dimensions for the array
     :  INDENT,                ! indentation for output
     :  FD                     ! Log file's description

      LOGICAL                  ! True if:
     :  LOGEXM                 ! Output to go to the log file

*  Arguments Given and Returned:
      CHARACTER *( * )
     :  LINE                   ! Line string

*  Status:
      INTEGER STATUS           ! The global status

*  External References:
      INTEGER CHR_LEN          ! String length
      INTEGER CHR_SIZE         ! String size

*  Local Variables:
      INTEGER
     :  LNSIZE,                ! Line string length
     :  LENG,                  ! Current position in line string
     :  I                      ! Character index

*.

*    Check the global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

      LNSIZE = CHR_SIZE( LINE )
      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*    Initialise the line (with indentation).

      LINE = ' '
      LENG = INDENT

*    Append the name.

      I = CHR_LEN( NAME )
      CALL CHR_PUTC( NAME(1:I), LINE, LENG )

*    Append the dimensions.

      CALL TRA_PUTDM( NDIM, DIMS, LINE, LENG, STATUS )

*    Find the last blank character in the line and move 5 characters
*    past it.

      LENG = CHR_LEN( LINE ) + 5

*    Append "{array of structures}".

      IF ( LENG+20 .LE. LNSIZE )
     :  CALL CHR_PUTC( '{array of structures}', LINE, LENG )

*    Output the line as the only token in a message.

      CALL MSG_SETC( 'LINE', LINE(1:LENG) )
      CALL MSG_OUT( 'TRACE_ARRS', '^LINE', STATUS )

*    Record the line in the log file.

      IF ( LOGEXM ) CALL FIO_WRITE( FD, LINE(1:LENG), STATUS )

      END
