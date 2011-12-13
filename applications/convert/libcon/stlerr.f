      SUBROUTINE STLERR( ROUTINE, STATUS )
*+
*  Name:
*     STLERR

*  Purpose:
*     Handles Starlink Interim-environment errors.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL STLERR( ROUTINE, STATUS )

*  Description:
*     This is called by any Starlink Interim-environment routine which
*     sets STATUS non-zero.
*
*     This version will return an appropiate message to the user
*     if the status value is greater than 2 (warning level).

*  Arguments:
*     ROUTINE = CHARACTER * ( * ) (Given)
*        The name of the calling routine.
*     STATUS = INTEGER (Given)
*        The INTERIM status.
*
*     [optional_subroutine_items]...

*  Copyright:
*     Copyright (C) 1980, 1984, 1988, 1992 Science & Engineering
*     Research Council. All Rights Reserved.

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
*     DP: David Pearce (STARLINK)
*     BMC: Bernie McNally (ROE)
*     AJC: Alan Chipperfield (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1980 July 24 (DP):
*        Version 2 (version one lost in the mists of time?---MJC)
*     1984 January 9 (BMC):
*        Include textual messages in error output.
*     1988 January 12 (AJC):
*        Initialise ISTAT.  (Replaces the standard version for CONVERT
*        as applications may already be loaded in ICL.---MJC)
*     1992 September 4 (MJC):
*        Used SST prologue and documented the variables.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) ROUTINE
      INTEGER STATUS

*  Local Variables:
      INTEGER FINISH             ! End character
      INTEGER ISTAT              ! Error-message number
      INTEGER IND                ! Local status
      CHARACTER * (72) MESSAGE(9)! Error messages
      INTEGER START              ! Start character

*  Local Data:
      DATA MESSAGE/
     :  ' STATUS = %%,  OBJECT NULL STATUS',
     :  ' STATUS = %%,  NOT PRESENT',
     :  ' STATUS = %%,  WRONG FILENAME OR FILE BELONGS TO SOMEONE ELSE',
     :  ' STATUS = %%,  PROGRAMMER ERROR IN PARAMETER NAME',
     :  ' STATUS = %%,  PROGRAMMER ERROR IN FORMAT CODE',
     :  ' STATUS = %%,  INVALID FRAME SIZE',
     :  ' STATUS = %%,  FRAME IS NOT AN IMAGE',
     :  ' STATUS = %%,  AUTOMATIC DATA CONVERSION FAILED',
     :  ' STATUS = %%,  UNDOCUMENTED ERROR'/

*.

*  Set up the text message number from the input status value.

      ISTAT = 0
      IF ( ( STATUS .GE. 3 ) .AND. ( STATUS .LE. 8 ) ) THEN
        ISTAT = STATUS
      ELSE IF ( STATUS .GT. 8 ) THEN
        ISTAT = 9
      END IF

*   Output the error message for severity levels greater than 2
*   (warning).

      IF ( ISTAT .GE. 3 ) THEN
         START = INDEX( MESSAGE( ISTAT ), '%%' )
         FINISH = START + 1
         CALL ITOC( ISTAT, MESSAGE(ISTAT)( START:FINISH ), IND )
         CALL WRUSER( '0---'//ROUTINE//MESSAGE( ISTAT ), IND )
         WRITE( MESSAGE( ISTAT )( START:FINISH ), '(''%%'')' )
      END IF

      END
