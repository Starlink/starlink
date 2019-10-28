      SUBROUTINE IRQ_RESQ( LOCS, QNAME, STATUS )
*+
*  Name:
*     IRQ_RESQ

*  Purpose:
*     Remove a given quality from all pixels in the NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ_RESQ( LOCS, QNAME, STATUS )

*  Description:
*     The quality specified by QNAME is removed from all pixels in the
*     NDF specified by LOCS (LOCS should be obtained either by calling
*     IRQ_FIND or IRQ_NEW). An error is reported if the quality name is
*     undefined within the NDF.
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
*        The quality name to be removed from all pixels in the NDF. This
*        quality name must be defined in the NDF specified by LOC. Name
*        definitions can be added to the NDF using routine IRQ_ADDQN.
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
*        Added RDONLY argument to IRQ1_SEARC and IRQ1_MOD.
*     4-MAR-2008 (DSB):
*        Cater for fixed-bit qualities.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants.
      INCLUDE 'IRQ_ERR'          ! IRQ error values.
      INCLUDE 'CNF_PAR'          ! CNF values.

*  Arguments Given:
      CHARACTER LOCS*(*)
      CHARACTER QNAME*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BIT                ! QUALITY bit corresponding to the
                                 ! quality name (LSB = 1).
      CHARACTER COMMNT*(IRQ__SZCOM)! Descriptive comment stored with
                                 ! the quality name.
      INTEGER FIRST              ! Position of first non-blank character
      LOGICAL DEF                ! True if QUALITY component is in a
                                 ! defined state.
      LOGICAL FIXBIT             ! Deos quality have a fixed bit number?
      LOGICAL FIXED              ! True if all pixels either do or don't
                                 ! have the quality.
      INTEGER INDF               ! Identifier for the NDF containing the
                                 ! quality names information.
      INTEGER LAST               ! Position of last non-blank character.
      CHARACTER LQNAME*(IRQ__SZQNM) ! Upper case copy of quality name.
      CHARACTER MODE*10          ! Mapping mode for QUALITY component.
      INTEGER*8 NEL              ! No. of pixels in the NDF.
      INTEGER PNT                ! Pointer to the mapped QUALITY array.
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

*  Check write access is available to the NDF.
      CALL NDF_ISACC( INDF, 'WRITE', WRITE, STATUS )
      IF( .NOT. WRITE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IRQ__NOWRT
         CALL ERR_REP( 'IRQ_RESQL_ERR2',
     :           'IRQ_RESQL: Write access is not available to the NDF.',
     :            STATUS )
      END IF

*  Produce an uppercase copy of the supplied quality name, exluding
*  leading blanks.
      CALL CHR_FANDL( QNAME, FIRST, LAST )
      LQNAME = QNAME( FIRST : LAST )
      CALL CHR_UCASE( LQNAME )

*  Find the quality name information.
      CALL IRQ1_SEARC( LOCS, LQNAME, FIXED, VALUE, BIT, COMMNT, RDONLY,
     :                 FIXBIT, SLOT, STATUS )

*  If the quality has a fixed bit number, we need to reset the bit for
*  all pixels in the quality component.
      IF( FIXBIT ) THEN
         CALL NDF_STATE( INDF, 'QUALITY', DEF, STATUS )
         IF( DEF ) THEN
            MODE = 'UPDATE'
         ELSE
            MODE = 'WRITE/ZERO'
         END IF

         CALL NDF_MAP8( INDF, 'QUALITY', '_UBYTE', MODE, PNT, NEL,
     :                  STATUS )
         IF( DEF ) CALL IRQ1_QSET( BIT, .FALSE., NEL,
     :                             %VAL( CNF_PVAL( PNT ) ), STATUS )
         CALL NDF_UNMAP( INDF, 'QUALITY', STATUS )

      END IF

*  Modify the FIXED and VALUE settings in the quality name
*  information.
      CALL IRQ1_MOD( LOCS, SLOT, .TRUE., .FALSE., BIT, RDONLY,
     :               FIXBIT, STATUS )

*  If an error occur, give context information.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', INDF )
         CALL MSG_SETC( 'QN', QNAME )
         CALL ERR_REP( 'IRQ_RESQ_ERR3',
     :          'IRQ_RESQ: Unable to remove quality name '//
     :          '^QN from all pixels in NDF ^NDF', STATUS )
      END IF

      END
