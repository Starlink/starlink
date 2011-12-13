      SUBROUTINE TRA_PRIMI( PRIMLO, NAME, TYPE, SIZE, NDIM, DIMS,
     :                      INDENT, CMNVAL, NEWLIN, NLINES, ONEPLN,
     :                      LOGEXM, FD, LINE, STATUS )
*+
*  Name:
*     TRA_PRIMI

*  Purpose:
*     Output an informational message for a primitive object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRA_PRIMI( PRIMLO, NAME, TYPE, SIZE, NDIM, DIMS, INDENT,
*    :                CMNVAL, NEWLIN, NLINES, ONEPLN, LOGEXM, FD, LINE,
*    :                STATUS )

*  Description:
*     Puts out information and values for the primitive object with
*     specified locator via the message system, or optionally written to
*     an ASCII file.
*
*     The information comprises name, dimensions, type and value(s). See
*     TRA_PUTx for details of the formatting options and layout.

*  Arguments:
*     PRIMLO = CHARACTER * ( DAT__SZLOC ) (Given)
*        Locator to the primitive object.
*     NAME = CHARACTER * ( * ) (Given)
*        Name of the primitive object.
*     TYPE = CHARACTER * ( * ) (Given)
*        Type of the primitive object.
*     SIZE = INTEGER (Given)
*        Number of elements for the primitive object if it is treated
*        as a vector.
*     NDIM = INTEGER (Given)
*        Dimensionality of the primitive object.
*     DIMS( DAT__MXDIM ) = INTEGER (Given)
*        Array of dimensions of the primitive object.
*     INDENT = INTEGER (Given)
*        Indentation level for the output information.
*     CMNVAL = INTEGER (Given)
*        Indentation level for the values.
*     NEWLIN = LOGICAL (Given)
*        If true the values begin on a new line.
*     NLINES = INTEGER (Given)
*        Number of lines to store values.
*     ONEPLN = LOGICAL (Given)
*        If true the elements of a character array each appear on a
*        separate line
*     LOGEXM = LOGICAL (Given)
*        If true a log of the header records is written to an ASCII
*        file.
*     FD = INTEGER (Given)
*        The ASCII file descriptor, ignored if LOGEXM is false.
*     LINE = CHARACTER * ( * ) (Returned)
*        The line of text to be output.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     If no error on entry then
*        Initialise message line, with indentation.
*        Append the primitive component name.
*        Append the primitive component dimensions.
*        Output the line as the only token in a message.
*        If file output then output line to the log
*        Initialise a new line
*        Write the values.
*        Output the line as the only token in a message.
*        If file output then output line to the log
*     Endif

*  Prior Requirements:
*     -  The ASCII file associated with file descriptor FD must be
*     open.

*  Copyright:
*     Copyright (C) 1984, 1989, 1991 Science & Engineering Research
*     Council. All Rights Reserved.

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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14/03/1984 (DB):
*        Original version.
*     07/04/1984 (DB):
*        Revised to use PUTDIM and PUTVAL.
*     1989 May 10 (MJC):
*        Put values to a new line.
*     1989 May 15 (MJC):
*        Tidy up and added NEWLIN, NLINES, LINE to arguments.
*     1989 Jun 15 (MJC):
*        Renamed from PRIMIT to avoid confusion with the original TRACE
*        version; added CMNVAL, ONEPLN, LOGEXM and FD arguments.
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
      CHARACTER*(DAT__SZLOC)
     :  PRIMLO                 ! Locator pointing at primitive object
                               ! to be examined
      CHARACTER*(DAT__SZNAM)
     :  NAME                   ! Object name
      CHARACTER*(DAT__SZTYP)
     :  TYPE                   ! Type of the primitive object

      INTEGER
     :  SIZE,                  ! Size of object if vectorized
     :  NDIM,                  ! Dimensionality of the object
     :  NLINES,                ! Number of lines to present values
     :  DIMS( DAT__MXDIM ),    ! Array of object dimensions
     :  INDENT,                ! Indentation for output
     :  CMNVAL,                ! Indentation for value
     :  FD                     ! Log file's description

      LOGICAL                  ! True if:
     :  LOGEXM,                ! Output to go to the log file
     :  NEWLIN,                ! Values start on a new line
     :  ONEPLN                 ! Elements of a character array each
                               ! appear on a new line

*  Arguments Returned:
      CHARACTER * ( * )
     :  LINE                   ! Line string

*  Status:
      INTEGER STATUS           ! The global status

*  External References:
      INTEGER CHR_LEN          ! String length ignoring trailing blanks
      INTEGER CHR_SIZE         ! String length including trailing blanks

*  Local Variables:
      INTEGER
     :  LNSIZE,                ! Line string length
     :  LENG,                  ! Line length
     :  I                      ! Character index

*.

*    Check the global status for an error.

      IF ( STATUS .NE. SAI__OK ) RETURN

      LNSIZE = CHR_SIZE( LINE )

*    Initialise the line (with indentation).

      LINE = ' '
      LENG = INDENT

*    Append the component name.

      I = CHR_LEN( NAME )
      CALL CHR_PUTC( NAME(1:I), LINE, LENG )

*    Append the dimensions.
      IF ( NDIM .GT. 0 ) THEN
         CALL TRA_PUTDM( NDIM, DIMS, LINE, LENG, STATUS )
      END IF

*    Move to the column where values will be written, note that there
*    are at least two spaces following the type.

      LENG = MAX( LENG + 2, CMNVAL + INDENT )

*    Is a new line required?

      IF ( NEWLIN ) THEN

*       Output the line as the only token in a message.

         CALL MSG_SETC( 'LINE', LINE(1:LENG) )
         CALL MSG_OUT( 'TRA_PRIMI_INFO1', '^LINE', STATUS )

*       Record the line in the log file.

         IF ( LOGEXM ) CALL FIO_WRITE( FD, LINE(1:LENG), STATUS )

*       Start a new line.

         LINE = ' '
         LENG = INDENT + 1
      END IF

*    Write the values to the new line.

      CALL TRA_PUTVL( PRIMLO, TYPE, NDIM, DIMS, SIZE, NLINES, INDENT,
     :                ONEPLN, LOGEXM, FD, LINE, LENG, STATUS )

*    Output the line as the only token in a message.

      CALL MSG_SETC( 'LINE', LINE(1:LENG) )
      CALL MSG_OUT( 'TRA_PRIMI_INFO2', '^LINE', STATUS )

*    Record the line in the log file.

      IF ( LOGEXM ) CALL FIO_WRITE( FD, LINE(1:LENG), STATUS )

      END
