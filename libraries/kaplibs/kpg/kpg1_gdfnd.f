      SUBROUTINE KPG1_GDFND( NAME, IPIC, STATUS )
*+
*  Name:
*     KPG1_GDFND

*  Purpose:
*     Selects the highest picture of a given name with WCS within the
*     current AGI picture.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GDFND( NAME, IPIC, STATUS )

*  Description:
*     This routine searches forwards through the AGI database for a
*     picture of a given name that lies within the current picture and
*     has an associated AST Plot structure. The the current picture
*     itself is included in the search.  If such a picture is found it
*     becomes the new current picture.  Otherwise, a bad status will be
*     returned, and the current picture is unchanged.
*
*     This routine is like KPG1_AGFND except that KPG1_AGFDN does not
*     require the returned picture to have an associated Plot.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the picture to be searched for in the graphics
*        database.
*     IPIC = INTEGER (Returned)
*        The picture identifier of the most-recent picture named NAME.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007, 2011 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S Berry (JAC, UCLan)
*     MJC: Malcolm J. Currie (Starlink)
*     {enter_new_authors_here}

*  History:
*     13-AUG-2007 (DSB):
*        Original version.
*     11-SEP-2007 (DSB):
*        Do not annul the picture identifier until it has been finished
*        with.
*     2011 May 10 (MJC):
*        Set mandatory bad status before calling ERR_REP.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'         ! Data-system constants

*  Arguments Given:
      CHARACTER * ( * ) NAME

*  Arguments Returned:
      INTEGER IPIC              ! Returned picture identifier

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER MORLOC*(DAT__SZLOC)! HDS locator for MORE structure
      CHARACTER CNAME*( DAT__SZNAM ) ! Name of the current picture
      INTEGER IPICC             ! Data image picture identifier
      INTEGER IPICT             ! Work picture identifier
      LOGICAL HASMOR            ! Does picture have a MORE component?
      LOGICAL HASWCS            ! Does picture have a WCS Plot?
      LOGICAL MORE              ! Continue searching?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get an identifier for the original current picture.
      CALL AGI_ICURP( IPICC, STATUS )

*  Get its name and convert to upper case.
      CALL AGI_INAME( CNAME, STATUS )
      CALL CHR_UCASE( CNAME )

*  Find the last (most recent) picture in the database that has the requested
*  name, and lies within the current picture.
      CALL AGI_RCL( NAME, IPICT, STATUS )

*  Loop until we have found a suitable position, or the database has been
*  exhausted.
      MORE = .TRUE.
      DO WHILE( MORE )

*  If no picture with the requested name was found, annul the error and
*  see if the current picture itself has the required name. If not, leave
*  the loop.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )

            IF( CNAME .EQ. NAME ) THEN
               IPICT = IPICC
            ELSE
               IPIC = -1
               MORE = .FALSE.
            END IF

*  If a picture was found with the required name, it will have been made
*  the current picture. Re-instate the original current picture.
         ELSE
            CALL AGI_SELP( IPICC, STATUS )
         END IF

*  If a picture with the required name was found, see if it has a WCS Plot
*  in its MORE component.
         IF( MORE ) THEN

            CALL AGI_IMORE( IPICT, HASMOR, STATUS )
            IF( HASMOR ) THEN
               CALL AGI_MORE( IPICT, 'READ', MORLOC, STATUS )
               CALL DAT_THERE( MORLOC, 'AST_PLOT', HASWCS, STATUS )
               CALL DAT_ANNUL( MORLOC, STATUS )
            ELSE
               HASWCS = .FALSE.
            END IF

*  If so we have found the required picture, so leave the loop.
            IPIC = IPICT
            IF( HASWCS ) THEN
               MORE = .FALSE.

*  If not, find the preceeding picture with the required name.
            ELSE
               CALL AGI_RCP( NAME, IPIC, IPICT, STATUS )

*  We have now finished with the IPIC picture identifier, so we can annul
*  it. We do not do this if IPIC identifiers the current picture since we
*  still need access to the current picture.
               IF( IPIC .NE. IPICC ) CALL AGI_ANNUL( IPIC, STATUS )
               IPIC = -1

            END IF

         END IF

      END DO

*  If no picture of the chosen name with WCS was found report an error.
      IF( IPIC .EQ. -1 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'KPG1_GDFND_NOTPRE', 'No suitable ^NAME '//
     :                 'picture can be found within the the current '//
     :                 'database picture.', STATUS )

*  Otherwise, select the picture of the chosen name to be the current
*  database picture.
      ELSE
         CALL AGI_SELP( IPIC, STATUS )
      END IF

      END
