      SUBROUTINE SHL_PAGRST( STATUS )
*+
*  Name:
*     SHL_PAGRST

*  Purpose:
*     Prepare the SHL library for paged text output

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SHL_PAGRST( STATUS)

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2004 Oct 4 (TIMJ):
*        Original version. Public wrapper to PTHLPO

*  Notes:
*     Currently all values are hard-wired. In the future the interface
*     may change to support specific controls of the initial state.

*  Bugs:
*     {note_new_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions

*  Global Variables:
      INCLUDE 'SHL_HLPCMD'       ! Common block for page status
*        CMD = CHARACTER * ( 80 ) (Write)
*           The command line.
*        LHELP = INTEGER (Write)
*           Lines of output this screenful.
*        HELPN = LOGICAL (Write)
*           If true, output is enabled.
*        LTOP = INTEGER (Write)
*           Top line number for the scrolling region.
*        LBOT = INTEGER (Write)
*           Bottom line number for the scrolling region.
*        ANSI = LOGICAL (Write)
*           If true, an ANSI terminal is in use.
*        LUCMD = INTEGER (Write)
*           Logical-unit number of the command input.
*        LUTERM = INTEGER (Write)
*           Logical-unit number of the terminal output.


*.

*  Status:
      INTEGER STATUS             ! Inherited global status

*  Local Variables:
      INTEGER WIDTH              ! Screen width

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Nothing has been output, and there is no command.
      LHELP = 0
      HELPN = .TRUE.
      CMD = ' '

*  Fixed for test purposes.  Note these are hardware specific.
      ANSI = .FALSE.
      LUCMD = 5
      LUTERM = 6

*  Find the height and width of the screen.  Use the full screen area.
*  A zero or negative LBOT (which occurs when there is an error) will
*  suppress paging.
      CALL ONE_SCRSZ( WIDTH, LBOT, STATUS )
      LTOP = 1

      END
