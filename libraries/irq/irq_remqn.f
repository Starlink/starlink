      SUBROUTINE IRQ_REMQN( LOCS, QNAME, STATUS )
*+
*  Name:
*     IRQ_REMQN

*  Purpose:
*     Remove the definition of a specified quality name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ_REMQN( LOCS, QNAME, STATUS )

*  Description:
*     The specified quality name is removed from the NDF specified
*     by LOCS. Any associated bit in the QUALITY array is freed for
*     future use. If the name is not defined an error is reported.
*     A value of ANY for the quality names causes all defined quality
*     names to be removed.
*
*     Note, an error is reported if only read access is available to the
*     NDF, or if the quality name has been flagged as read-only using
*     routine IRQ_RWQN.

*  Arguments:
*     LOCS(5) = CHARACTER * ( * ) (Given)
*        An array of five HDS locators. These locators identify the NDF
*        and the associated quality name information.  They should have
*        been obtained using routine IRQ_FIND or routine IRQ_NEW.
*     QNAME = CHARACTER * ( * ) (Given)
*        The quality name to remove, or 'ANY' if all quality names are
*        to be removed.
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
*        Added RDONLY argument to IRQ1_SEARC.
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

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BIT                ! Bit in QUALITY component
                                 ! corresponding to the supplied quality
                                 ! name (if FIXED is false).
      CHARACTER COMMNT*(IRQ__SZCOM)! Descriptive comment stored with
                                 ! quality name.
      INTEGER FIRST              ! Position of first non-blank character
      LOGICAL FIXBIT             ! Does quality have a fixed bit number?
      LOGICAL FIXED              ! true if quality doesn't vary from
                                 ! pixel to pixel.
      INTEGER INDF               ! Identifier for the NDF containing the
                                 ! quality names information.
      INTEGER LAST               ! Position of last non-blank character.
      INTEGER LUSED              ! Last used slot number.
      CHARACTER LQNAME*(IRQ__SZQNM) ! Upper case copy of quality name.
      LOGICAL RDONLY             ! Read-only flag for quality name.
      INTEGER SLOT               ! Index into the QUAL structure at
                                 ! which the name was found.
      LOGICAL VALUE              ! True if quality is held by all
                                 ! pixels. False if no pixels hold the
                                 ! quality. Indeterminate if FIXED is
                                 ! false.
      LOGICAL WRITE              ! True if write access is available to
                                 ! the NDF.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the NDF identifier from LOCS, and check it is still valid.
      CALL IRQ1_INDF( LOCS, INDF, STATUS )

*  Check that write access is available to the NDF.
      CALL NDF_ISACC( INDF, 'WRITE', WRITE, STATUS )
      IF( .NOT. WRITE ) THEN
         STATUS = IRQ__NOWRT
         CALL ERR_REP( 'IRQ_REMQN_ERR1',
     :   'IRQ_REMQN: Write access is not available to the NDF.',
     :   STATUS )
      END IF
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Produce an uppercase copy of the supplied quality name, exluding
*  leading blanks.
      CALL CHR_FANDL( QNAME, FIRST, LAST )
      LQNAME = QNAME( FIRST : LAST )
      CALL CHR_UCASE( LQNAME )

*  If the name is "ANY" attempt to remove all slots.
      IF( LQNAME .EQ. 'ANY' ) THEN
         CALL DAT_GET0I( LOCS(3), LUSED, STATUS )
         DO SLOT = 1, LUSED
            CALL IRQ1_RESET( LOCS, SLOT, STATUS )
         END DO

*  If a single quality name is to be deleted, search for the supplied
*  name.
      ELSE
         CALL IRQ1_SEARC( LOCS, LQNAME( : LAST - FIRST + 1 ), FIXED,
     :                    VALUE, BIT, COMMNT, RDONLY, FIXBIT,
     :                    SLOT, STATUS )

*  Reset the slot containing the supplied quality name.
         CALL IRQ1_RESET( LOCS, SLOT, STATUS )

      END IF

*  If an error occur, give context information.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', INDF )
         CALL MSG_SETC( 'QN', QNAME )
         CALL ERR_REP( 'IRQ_REMQN_ERR2',
     :  'IRQ_REMQN: Unable to remove quality name ^QN from NDF ^NDF',
     :                  STATUS )
      END IF

      END
