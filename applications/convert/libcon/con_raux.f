      SUBROUTINE CON_RAUX( INLOC, CUBID, STATUS )
*+
*  Name:
*     CON_RAUX

*  Purpose:
*     Copies the auxiliary information for an Asterix data cube to an
*     NDF.

*  Language:
*     Fortran 77.

*  Invocation:
*     CALL CON_RAUX( INLOC, CUBID; STATUS )

*  Description:
*     Copy the auxiliary information for an Asterix data cube to an NDF.
*     The auxiliary information is the contents of the MORE extension.
*     Any history information is also copied.

*  Arguments:
*     INLOC  =  CHARACTER*(*) (Given)
*        Locator to the input dataset.
*     CUBID  =  INTEGER (Given)
*        Identifier for the data cube.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Get a locator for the input MORE component.
*     Get a locator for the output structure.
*     Copy the MORE component.
*     If ok then
*       Attempt to get a locator for the input HISTORY component.
*       If ok then
*         Copy the HISTORY component.
*         Set the HISTORY update mode to NORMAL.
*       else
*         cube.
*         If (and only if) the failure is due to the absence of a
*         HISTORY component then
*           Annul the error.
*           Report a message.
*         end if
*       end if
*       Report any error copying the history information.
*     else
*       Report an error copying the MORE extension.
*     end if

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     ACD: A C Davenhall (Edinburgh)
*     MJC: Malcolm J. Currie (Starlink)
*     {enter_new_authors_here}

*  History:
*     3/9/97 (ACD):
*        Original version (from CON_CAUX).
*     2009 June 29 (MJC):
*        Used modern coding style.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard Starlink constants
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'DAT_ERR'          ! HDS error codes
      INCLUDE 'MSG_PAR'          ! Message system constants

*  Arguments Given:
      CHARACTER*(*) INLOC
      INTEGER CUBID

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC) CUBLOC ! Locator to output NDF cube
      CHARACTER*(DAT__SZLOC) HISLOC ! Locator to input HISTORY structure
      CHARACTER*(DAT__SZLOC) MMRLOC ! Locator to input map MORE struct

*.

*  Check the global inherited status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Get a locator for the MORE component of the input SPECX map
*  structure.
         CALL DAT_FIND( INLOC, 'MORE', MMRLOC, STATUS )

*  Get a locator for the output NDF cube structure.
         CALL NDF_LOC( CUBID, 'WRITE', CUBLOC, STATUS )

*  Copy the MORE component and proceed if ok.
         CALL DAT_COPY( MMRLOC, CUBLOC, 'MORE', STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN

*  Attempt to get a locator for the input HISTORY component.
*
*  If the input HISTORY component is present and OK then copy the
*  HISTORY component and set the HISTORY update mode to NORMAL.
*
*  If the input HISTORY component is absent or not OK then if (and only
*  if) a bad status has been raised because the HISTORY component was
*  absent then annul the status and report a message.
            CALL DAT_FIND( INLOC, 'HISTORY', HISLOC, STATUS )

            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL DAT_COPY( HISLOC, CUBLOC, 'HISTORY', STATUS )

               CALL NDF_HSMOD( 'NORMAL', CUBID, STATUS )

            ELSE
               IF ( STATUS .EQ. DAT__OBJNF ) THEN
                  CALL ERR_ANNUL( STATUS )

*  Note that the following message would only be issued in a
*  not-yet-implemented 'verbose' mode for the CONVERT applications.
                  CALL MSG_OUTIF( MSG__VERB, ' ', 'History '/
     :              /'recording is disabled for this dataset.',
     :              STATUS )
               END IF
            END IF

*  Report any error copying the history information.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'CON_RAUX_HIS', 'CON_RAUX: failure '/
     :                       /'copying history information.', STATUS )
            END IF

         ELSE
            CALL ERR_REP( 'CON_RAUX_MRE', 'CON_RAUX: failure '/
     :                    /'copying extension MORE.', STATUS )
         END IF

      END IF

      END
