      SUBROUTINE TRA1_TRCON( NAME, NDIM, DIMS, STEP, INDENT, LOGEXM, FD,
     :                      LINE, STATUS )
*+
*  Name:
*     TRA1_TRCON

*  Purpose:
*     Outputs a "contents of" message for an array of structures.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRA1_TRCON( NAME, NDIM, DIMS, INDENT, LOGEXM, FD, LINE,
*    :                 STATUS )

*  Description:
*     Outputs the "Contents of " message for an element of an array of
*     structures which is being traced.

*  Arguments:
*     NAME = CHARACTER * ( DAT__SZNAM ) (Given)
*        Name of the array of structures being traced through.
*     NDIM = INTEGER (Given)
*        Dimensionality of the array of structures being traced
*        through.
*     DIMS( DAT__MXDIM ) = INTEGER (Given)
*        Dimension indices for current element of array of structures.
*     STEP = INTEGER (Given)
*        Indentation step between levels of the hierarchy.
*     INDENT = INTEGER (Given)
*        Indentation level for the message output.
*     LOGEXM = LOGICAL (Given)
*        If true a log of the header records is written to an ASCII
*        file.
*     FD = INTEGER (Given)
*        The ASCII file descriptor, ignored if LOGEXM is false.
*     LINE = CHARACTER* ( * ) (Given and Returned)
*        The string to which the "Contents of " message is to be
*        written.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     If bad status on entry then return
*     Write out blank line for spacing
*     Set message line to all blanks
*     Initialise start position in the line to the indent level
*     Place 'Contents of ' in line at this position
*     Find out the length of the NAME ignoring trailing blanks
*     Append the name onto the message line
*     Append the dimensions
*     Get the line length ignoring trailing blanks
*     Set up the message line as a single message token
*     Push the message out
*     If file output then output line to the log
*     Empty message buffer
*     End

*  Prior Requirements:
*     -  The ASCII file associated with descriptor FD must already be
*     opened.

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
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15/03/1984 (DB):
*        Original version.
*     07/04/1984 (DB):
*        Revised version to use PUTDIM.
*     1989 May 16 (MJC):
*        Tidy up and added STEP to arguments.
*     1989 Jun 15 (MJC):
*        Renamed from TRACON to avoid confusion with the original TRACE
*        version; added LOGEXM, FD and LINE arguments.
*     1991 January 30 (MJC):
*        Converted to SST prologue.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! Switch off the default typing

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Environment constants
      INCLUDE 'DAT_PAR'        ! Data-system constants

*  Arguments Given:
      CHARACTER *( DAT__SZNAM )
     :  NAME                   ! Name of the array

      INTEGER
     :  NDIM,                  ! Dimensionality of array
     :  DIMS( DAT__MXDIM ),    ! Dimensions of array
     :  INDENT,                ! Indentation level
     :  STEP,                  ! Indentation step size
     :  FD                     ! Log file's description

      LOGICAL                  ! True if:
     :  LOGEXM                 ! Output to go to the log file

*  Arguments Given and Returned:
      CHARACTER* ( * )
     :  LINE                   ! Message line string

*  Status:
      INTEGER STATUS           ! The global status

*  External References:
      INTEGER CHR_LEN          ! Returns string length ignoring trailing
                               ! blanks

*  Local Variables:
      INTEGER
     :  LENG,                  ! Index into character string LINE
     :  LEN                    ! Length of character strings ignoring
                               ! trailing blanks

*.

*    Check the global status for an error.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Write out a blank line for spacing.

      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*    Set LINE to all blanks.

      LINE = ' '

*    Initialise the start position in the line to the indentation level
*    above, i.e. aligned to the array structure's name.

      LENG = INDENT - STEP

*    Place 'Contents of ' in line at this position

      CALL CHR_PUTC( 'Contents of ', LINE, LENG )

*    Find out the length of the NAME ignoring any trailing blanks.

      LEN = CHR_LEN( NAME )

*    Push the NAME out into the LINE string.

      CALL CHR_PUTC( NAME(1:LEN), LINE, LENG )

*    Append the dimensions of the component.

      CALL TRA_PUTDM( NDIM, DIMS, LINE, LENG, STATUS )

*    Get the LINE length ignoring any trailing blanks.

      LEN = CHR_LEN( LINE )

*    Set up the message line, LINE, as a single message token, MESSAGE.

      CALL MSG_SETC( 'MESSAGE', LINE( 1:LEN ) )

*    Report the message.

      CALL MSG_OUT( 'TRARR_CON', '^MESSAGE', STATUS )

*    Record in the log file if requested.

      IF ( LOGEXM ) CALL FIO_WRITE( FD, LINE(1:LENG), STATUS )

*    Reset the line.

      LINE = ' '

      END
