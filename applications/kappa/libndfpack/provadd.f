      SUBROUTINE PROVADD( STATUS )
*+
*  Name:
*     PROVADD

*  Purpose:
*     Stores provenance information in an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PROVADD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application modifies the provenance information stored in an
*     NDF. It records a second specified NDF as a direct parent of the 
*     first NDF. If an NDF has more than one direct parent then this
*     application should be run multiple times, once for each parent.

*  Usage:
*     provadd ndf parent creator isroot more

*  ADAM Parameters:
*     CREATOR = LITERAL (Read)
*        A text identifier for the software that created the main NDF
*        (usually the name of the calling application).  The format of 
*        the identifier is arbitrary, but the form "PACKAGE:COMMAND" is
*        recommended. If a null (!) value is supplied, no creator
*        information is stored.  [!]
*     ISROOT = _LOGICAL (Read)
*        If TRUE, then the NDF given by parameter "PARENT" will be 
*        treated as a root NDF.  That is, any provenance information 
*        within PARENT describing its own parents is ignored.  If FALSE,
*        then any provenance information within PARENT is copied into
*        the main NDF.  PARENT is then only a root NDF only if it 
*        contains no provenance information.  [FALSE]
*     MORE = UNIV (Read)
*        An HDS object containing arbitrary additional information about
*        the parent NDF, and how it was used in the creation of the main
*        NDF.  If supplied, this information is stored with the 
*        provenance in the main NDF.  [!]
*     NDF = NDF (Read and Write)
*        The NDF which is to be modified.
*     PARENT = NDF (Read)
*        An NDF that is to be recorded as a direct parent of the NDF
*        given by parameter "NDF".

*  Examples:
*     provadd m51_ff ff
*        Records the fact that NDF "ff" was used in the creation of NDF
*        "m51_ff".

*  Notes:
*     Provenance information is stored in an NDF extension called
*     PROVENANCE, and is propagated automatically by all KAPPA
*     applications.

*  Related Applications:
*     KAPPA: PROVSHOW, HISCOM.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. berry (JAC,UCLan)
*     {enter_new_authors_here}

*  History:
*     24-JAN-2008 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS public constants
      INCLUDE 'PAR_ERR'          ! PAR_ error codes

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CREATR*255       ! Name of creator
      CHARACTER MORE*(DAT__SZLOC) ! Locator for MORE structure
      INTEGER INDF1              ! Identifier for NDF being modified
      INTEGER INDF2              ! Identifier for parent NDF
      LOGICAL ISROOT             ! Is the parent an orphan?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the NDF to be modified.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', INDF1, STATUS )

*  Obtain the parent NDF.
      CALL LPG_ASSOC( 'PARENT', 'READ', INDF2, STATUS )

*  Obtain a locator to the MORE object.
      IF( STATUS .EQ. SAI__OK ) THEN
         MORE = DAT__NOLOC
         CALL DAT_ASSOC( 'MORE', 'READ', MORE, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            MORE = DAT__NOLOC
         END IF
      END IF      

*  Obtain the creator string.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL PAR_GET0C( 'CREATOR', CREATR, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            CREATR = ' '
         END IF
      END IF

*  Obtain the root flag.
      CALL PAR_GET0L( 'ISROOT', ISROOT, STATUS )

*  Store the information in the provenance extension of INDF1.
      CALL NDG_PTPRV( INDF1, INDF2, MORE, ISROOT, CREATR, STATUS )

*  Free the MORE structure locator.
      IF( MORE .NE. DAT__NOLOC ) CALL DAT_CANCL( 'MORE', STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report contextual information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PROVADD_ERR', 'PROVADD: Error adding '//
     :                 'provenance information to an NDF.', STATUS )
      END IF

      END
