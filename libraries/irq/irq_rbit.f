      SUBROUTINE IRQ_RBIT( LOCS, QNAME, BIT, STATUS )
*+
*  Name:
*     IRQ_RBIT

*  Purpose:
*     Reserve a bit number for a given quality name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ_RBIT( LOCS, QNAME, BIT, STATUS )

*  Description:
*     If the supplied quality name already has a bit number associated
*     with it, the bit number is returned. Otherwise, the next available
*     plane in the quality array is asigned to the quality name, and its
*     bit number is returned.
*
*     Note, write or update access must be available for the NDF (as
*     set up by routine LPG_ASSOC for instance).
*
*  Arguments:
*     LOCS(5) = CHARACTER * ( * ) (Given)
*        An array of five HDS locators. These locators identify the NDF
*        and the associated quality name information.  They should have
*        been obtained using routine IRQ_FIND or routine IRQ_NEW.
*     QNAME = CHARACTER * ( * ) (Given)
*        The quality name. This quality name must be defined in the NDF
*        specified by LOCS. Name definitions can be added to the NDF
*        using routine IRQ_ADDQN.
*     BIT = INTEGER (Returned)
*        The bit number used by the quality name within the quality
*        array. Note, the least-significant bit is Bit 1, not Bit 0.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JAN-2008 (DSB):
*        Original version.
*     15-FEB-2008 (DSB):
*        Added RDONLY argument to IRQ1_SEARC and IRQ1_MOD.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants.
      INCLUDE 'IRQ_PAR'          ! IRQ constants.
      INCLUDE 'IRQ_ERR'          ! IRQ error values.
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER LOCS*(*)
      CHARACTER QNAME*(*)

*  Arguments Returned:
      INTEGER BIT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER COMMNT*(IRQ__SZCOM)! Descriptive comment stored with
                                 ! the quality name.
      INTEGER FIRST              ! Index of first non-blank character
      LOGICAL FIXBIT             ! Does quality have a fixed bit number?
      LOGICAL FIXED              ! True if all pixels either do or don't
                                 ! have the quality.
      INTEGER INDF               ! Identifier for the NDF containing the
                                 ! quality names information.
      INTEGER LAST               ! Index of last non-blank character
      CHARACTER LQNAME*(IRQ__SZQNM) ! Upper case copy of quality name.
      LOGICAL RDONLY             ! Read-only flag for quality name.
      INTEGER SLOT               ! Index into the QUALITY_NAMES
                                 ! structure at which the new name will
                                 ! be stored.
      LOGICAL VALUE              ! True if all pixels have the quality,
                                 ! false if no pixels used to have the
                                 ! quality, indeterminate if some did
                                 ! and some didn't.
      LOGICAL WRITE              ! True if write access is available to
                                 ! the NDF.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the NDF identifier from LOCS, and check it is still valid.
      CALL IRQ1_INDF( LOCS, INDF, STATUS )

*  Produce an uppercase copy of the supplied quality name, excluding
*  leading blanks.
      CALL CHR_FANDL( QNAME, FIRST, LAST )
      LQNAME = QNAME( FIRST : LAST )
      CALL CHR_UCASE( LQNAME )

*  Find the quality name information.
      CALL IRQ1_SEARC( LOCS, LQNAME, FIXED, VALUE, BIT, COMMNT, RDONLY,
     :                 FIXBIT, SLOT, STATUS )

*  If the quality name already has a bit number assigned to it, return
*  it. Otherwise we assign a bit number to the quality name now.
      IF( BIT .EQ. 0 ) THEN

*  Check that write access is available to the NDF.
         CALL NDF_ISACC( INDF, 'WRITE', WRITE, STATUS )
         IF( .NOT. WRITE .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = IRQ__NOWRT
            CALL ERR_REP( 'IRQ_RBIT_ERR1',
     :                    'IRQ_RBIT: Write access is not available '//
     :                    'to the NDF.', STATUS )
         END IF

*  If there is currently no bit plane reserved for this quality,
*  reserve one now.
         CALL IRQ1_RBIT( LOCS, BIT, STATUS )

*  Modify the FIXED, VALUE and BIT settings in the quality name
*  information.
         CALL IRQ1_MOD( LOCS, SLOT, .FALSE., VALUE, BIT, RDONLY,
     :                  FIXBIT, STATUS )
      END IF

*  If an error occur, give context information.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', INDF )
         CALL MSG_SETC( 'QN', QNAME )
         CALL ERR_REP( 'IRQ_RBIT_ERR2',
     :          'IRQ_RBIT: Unable to reserve a bit number for '//
     :          'quality name ^QN in NDF ^NDF', STATUS )
      END IF

      END
