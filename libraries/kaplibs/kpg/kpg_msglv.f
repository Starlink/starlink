      SUBROUTINE KPG_MSGLV( LEVEL, STATUS )
*+
*  Name:
*     KPG_MSGLV

*  Purpose:
*     Sets the message reporting level from the MSG_FILTER environment
*     variable.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG_MSGLV( LEVEL, STATUS )

*  Description:
*     This routine reads the value of the MSG_FILTER environment
*     variable and uses it to set the conditional message-reporting 
*     level.  No error occurs should the environment variable not be
*     defined, or not be one of the allowed values.  In these 
*     circumstances the reporting level is set to NORMAL (MSG__NORM).
*
*     The allowed values for MSG_FILTER conditional message reporting
*     levels are as follows.
*       - QUIET   -- reports only the most important messages
*       - NORMAL  -- normal reporting
*       - VERBOSE -- verbose reporting of results and for user aids

*  Arguments:
*     LEVEL = INTEGER (Given)
*        The message reporting level.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     MJC:  Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2009 July 21 (MJC):
*        Original version.
*     {enter_changes_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! Message-system constants 

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Arguments Returned:
      INTEGER LEVEL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ISTAT              ! CHR status
      CHARACTER*7 MSGLEV         ! Message level (copes with DEBUGnn)

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the reporting level from the environment variable, and
*  defaulting to normal reporting.
      LEVEL = MSG__NORM
      CALL KPG_ENV0C( 'MSG_FILTER', MSGLEV, STATUS )
      CALL CHR_UCASE( MSGLEV )

*  Test for each level, including abbreviations.
      IF ( MSGLEV( 1:1 ) .EQ. 'N' ) THEN
         LEVEL = MSG__NORM

      ELSE IF ( MSGLEV( 1:1 ) .EQ. 'Q' ) THEN
         LEVEL = MSG__QUIET
      
      ELSE IF ( MSGLEV( 1:1 ) .EQ. 'V' ) THEN
         LEVEL = MSG__VERB

      END IF

*  Now set the reporting level.
      CALL MSG_IFSET( LEVEL, STATUS )
      
      END
