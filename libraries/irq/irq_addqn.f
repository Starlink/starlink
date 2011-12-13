      SUBROUTINE IRQ_ADDQN( LOCS, QNAME, DEFLT, COMMNT, STATUS )
*+
*  Name:
*     IRQ_ADDQN

*  Purpose:
*     Define a new quality name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ_ADDQN( LOCS, QNAME, DEFLT, COMMNT, STATUS )

*  Description:
*     This routine adds the quality name specified by QNAME to the NDF
*     specified by LOCS. LOCS must previously have been assigned values
*     by one of the routines IRQ_FIND or IRQ_NEW. If the quality name
*     is already defined, an error is reported. Note, this routine does
*     not reserve a bit in the QUALITY component for the new quality
*     name, it mearly established a default value for the quality which
*     will be used for all pixels in the NDF if no subsequent call to
*     IRQ_SETQC is made. Note, the string ANY cannot be used as a
*     quality name. Also, quality names may not contain any full stops.
*
*     An error is reported if only READ access is available to the NDF.

*  Arguments:
*     LOCS(5) = CHARACTER * ( * ) (Given)
*        An array of five HDS locators. These locators identify the NDF
*        and the associated quality name information.  They should have
*        been obtained using routine IRQ_FIND or routine IRQ_NEW.
*     QNAME = CHARACTER * ( * ) (Given)
*        The new quality name to store. The maximum length of this
*        string is given by symbolic constant IRQ__SZQNM which currently
*        has the value 15. Leading spaces are ignored, and the stored
*        name is converted to upper case.
*     DEFLT = LOGICAL (Given)
*        If true, then by default all pixels are assumed to hold the
*        quality specified by QNAME. If false, then it is assumed that
*        no pixels hold the quality.
*     COMMNT = CHARACTER * ( * ) (Given)
*        A descriptive comment to store with the quality name. The
*        maximum length of this string is given by symbolic constant
*        IRQ__SZCOM, which currently has the value 50. Any characters
*        beyond this length are ignored.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     25-JUL-1991 (DSB):
*        Original version.
*     15-FEB-2008 (DSB):
*        Add RDONLY argument to IRQ1_SEARC.
*     4-MAR-2008 (DSB):
*        Add FIXBIT argument to IRQ1_SEARC.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants.
      INCLUDE 'IRQ_ERR'          ! IRQ error values.

*  Arguments Given:
      CHARACTER LOCS(5)*(*)
      CHARACTER QNAME*(*)
      LOGICAL DEFLT
      CHARACTER COMMNT*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BIT                ! QUALITY bit used to represent the
                                 ! quality.
      INTEGER FIRST              ! Position of first non-blank character
      LOGICAL FIXBIT             ! Does quality have a fixed bit number?
      LOGICAL FIXED              ! True if all or no pixels hold the
                                 ! specified quality.
      INTEGER INDF               ! Identifier for the NDF containing the
                                 ! quality names information.
      INTEGER LAST               ! Position of last non-blank character.
      CHARACTER LQNAME*(IRQ__SZQNM) ! Upper case copy of quality name.
      LOGICAL RDONLY             ! Read-only flag for quality name.
      INTEGER SLOT               ! Index into the QUAL structure at
                                 ! which the new name will be stored.
      LOGICAL VALUE              ! True if all pixels hold the quality.
                                 ! False otherwise.
      LOGICAL WRITE              ! True if write access is available to
                                 ! the NDF.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the NDF identifier from LOCS, and check it is still valid.
      CALL IRQ1_INDF( LOCS, INDF, STATUS )

*  Check that write access is available to the NDF.
      CALL NDF_ISACC( INDF, 'WRITE', WRITE, STATUS )
      IF( .NOT. WRITE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IRQ__NOWRT
         CALL ERR_REP( 'IRQ_ADDQN_ERR1',
     :   'IRQ_ADDQN: Write access is not available to the NDF.',
     :   STATUS )
      END IF
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Check that the supplied quality name is not too long.
      CALL CHR_FANDL( QNAME, FIRST, LAST )
      IF( LAST - FIRST + 1 .GT. IRQ__SZQNM ) THEN
         STATUS = IRQ__QLONG
         CALL MSG_SETI( 'LEN', IRQ__SZQNM )
         CALL ERR_REP( 'IRQ_ADDQN_ERR2',
     :  'IRQ_ADDQN: Quality name is longer than ^LEN characters',
     :                 STATUS )
      END IF

*  Produce an uppercase copy of the supplied quality name, exluding
*  leading blanks.
      LQNAME = QNAME( FIRST : LAST )
      CALL CHR_UCASE( LQNAME )

*  Mark the error stack so that any errors generated within IRQ1_SEARC
*  will not be delivered immediately. Error reports are automatically
*  deferred within ADAM applications, but not within standalone
*  applications.
      CALL ERR_MARK

*  See if the name is already defined. If, so report an error.
      CALL IRQ1_SEARC( LOCS, LQNAME, FIXED, VALUE, BIT, COMMNT, RDONLY,
     :                 FIXBIT, SLOT, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = IRQ__QNEXS
         CALL ERR_REP( 'IRQ_ADDQN_ERR3',
     :                 'IRQ_ADDQN: Quality name is already defined',
     :                 STATUS )
*  If the name is not already defined, annul the error reported by
*  IRQ1_SEARC.
      ELSE IF( STATUS .EQ. IRQ__NOQNM ) THEN
         CALL ERR_ANNUL( STATUS )

      END IF

*  Release the current error context.
      CALL ERR_RLSE

*  Reserve a slot in the QUALITY_NAMES structure.
      CALL IRQ1_RSLOT( LOCS, SLOT, STATUS )

*  Store the quality in the reserved slot.
      CALL IRQ1_ADD( LOCS, SLOT, LQNAME, DEFLT, COMMNT, STATUS )

*  If an error occur, give context information.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', INDF )
         CALL MSG_SETC( 'QN', QNAME )
         CALL ERR_REP( 'IRQ_ADDQN_ERR4',
     :       'IRQ_ADDQN: Unable to add quality name ^QN to NDF ^NDF',
     :        STATUS )
      END IF

      END
