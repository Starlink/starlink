      SUBROUTINE CON_CAUX( MAPID, CUBID, APPHST, STATUS )
*+
*  Name:
*     CON_CAUX

*  Purpose:
*     Copies the auxiliary information for a SPECX map grid to an NDF cube.

*  Language:
*     Fortran 77.

*  Invocation:
*     CALL CON_CAUX( MAPID, CUBID, APPHST; STATUS )

*  Description:
*     Copy the auxiliary information for a SPECX map grid to an NDF cube.
*
*     The auxiliary information is the contents of the MORE extension.
*     However, the POSN structure containing the original spectra is
*     not copied.

*  Arguments:
*     MAPID  =  INTEGER (Given)
*        Identifier for the input SPECX map grid.
*     CUBID  =  INTEGER (Given)
*        Identifier for the data cube.
*     APPHST  =  LOGICAL (Returned)
*        Flag indicating whether history information is being appended
*        to the output data cube.  It is coded as follows:
*        .TRUE.  -  history information is being appended,
*        .FALSE. -  history information is not being appended.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Get a locator for the input structure.
*     Get a locator for the input MORE component.
*     Get a locator for the output structure.
*     Copy the MORE component.
*     If ok then
*       Get a locator for the output POSN component.
*       Delete the output POSN component.
*       If ok then
*         Attempt to get a locator for the input HISTORY component.
*         If ok then
*           Set flag saying history is being appended to the output cube.
*           Copy the HISTORY component.
*           Set the HISTORY update mode to NORMAL.
*         else
*           Set flag saying history is not being appended to the output
*           cube.
*           If (and only if) the failure is due to the absence of a
*           HISTORY component then
*             Annul the error.
*             Report a message.
*           end if
*         end if
*         Report any error copying the history information.
*       else
*         Report an error deleting the POSN component.
*       end if
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
*     24/7/97 (ACD):
*        Original version.
*     28/8/97 (ACD):
*        First stable version.
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
      INTEGER MAPID
      INTEGER CUBID

*  Arguments Returned:
      LOGICAL APPHST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC) CMRLOC ! Locator to output more structure
      CHARACTER*(DAT__SZLOC) CUBLOC ! Locator to output NDF cube
      CHARACTER*(DAT__SZLOC) HISLOC ! Locator to input HISTORY structure
      CHARACTER*(DAT__SZLOC) MAPLOC ! Locator to SPECX map structure
      CHARACTER*(DAT__SZLOC) MMRLOC ! Locator to input map MORE struct

*.

*  Check the global inherited status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Get a locator for the input SPECX map structure.
         CALL NDF_LOC( MAPID, 'READ', MAPLOC, STATUS )

*  Get a locator for the MORE component of the input SPECX map
*  structure.
         CALL DAT_FIND( MAPLOC, 'MORE', MMRLOC, STATUS )

*  Get a locator for the output NDF cube structure.
         CALL NDF_LOC( CUBID, 'WRITE', CUBLOC, STATUS )

*  Copy the MORE component and proceed if ok.
         CALL DAT_COPY( MMRLOC, CUBLOC, 'MORE', STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN

*  Get a locator for the POSN component in the 'MORE' structure
*  of the output NDF.
            CALL DAT_FIND( CUBLOC, 'MORE', CMRLOC, STATUS )

*  Delete the output POSN component and proceed if OK.  Note
*  that this component holds the original spectra.
            CALL DAT_ERASE( CMRLOC, 'POSN', STATUS )

            IF ( STATUS .EQ. SAI__OK ) THEN

*  Attempt to get a locator for the input HISTORY component.
*
*  If the input HISTORY component is present and OK then set the flag
*  indicating that HISTORY is to be appended to the output cube, copy
*  the HISTORY component and set the HISTORY update mode to NORMAL.
*
*  If the input HISTORY component is absent or not OK then set the flag
*  indicating that the output cube will not contain any HISTORY.  If
*  (and only if) a bad status has been raised because the HISTORY
*  component was absent then annul the status and report a message.
               CALL DAT_FIND( MAPLOC, 'HISTORY', HISLOC, STATUS )

               IF ( STATUS .EQ. SAI__OK ) THEN
                  APPHST = .TRUE.

                  CALL DAT_COPY( HISLOC, CUBLOC, 'HISTORY', STATUS )

                  CALL NDF_HSMOD( 'NORMAL', CUBID, STATUS )

               ELSE
                  APPHST = .FALSE.

                  IF ( STATUS .EQ. DAT__OBJNF ) THEN
                     CALL ERR_ANNUL( STATUS )

*  Note that the following message would only be issued in a
*  not-yet-implemented 'verbose' mode for the CONVERT applications.
                     CALL MSG_OUTIF( MSG__VERB, ' ', 'History '/
     :                 /'recording is disabled for this dataset.',
     :                 STATUS )
                  END IF
               END IF

*  Report any error copying the history information.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_REP( 'CON_CAUX_HIS', 'CON_CAUX: failure '/
     :              /'copying history information.', STATUS )
               END IF

            ELSE
               CALL ERR_REP( 'CON_CAUX_PSN', 'CON_CAUX: failure '/
     :           /'deleting component POSN.', STATUS )
            END IF

         ELSE
            CALL ERR_REP( 'CON_CAUX_MRE', 'CON_CAUX: failure '/
     :        /'copying extension MORE.', STATUS )
         END IF

      END IF

      END
