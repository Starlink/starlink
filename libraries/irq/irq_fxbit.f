      SUBROUTINE IRQ_FXBIT( LOCS, QNAME, BIT, FIXBIT, STATUS )
*+
*  Name:
*     IRQ_FXBIT

*  Purpose:
*     Assign a fixed bit number to a quality name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ_FXBIT( LOCS, QNAME, BIT, SET, FIXBIT, STATUS )

*  Description:
*     This routine associates a fixed bit number with a specified quality
*     name. Normally, IRQ manages the allocation of bit numbers to named
*     qualities, but this routine allows the calling application to specify
*     which bit is to be used for a given quality.
*
*     By default, a QUALITY-array bit is associated with a quality name
*     only if some pixels hold the quality and some do not hold the
*     quality (i.e. there is a mix of values). Otherwise, a flag is stored
*     in the QUALITY_NAMES structure indicating this, and any quality bit
*     previously associated with the quality name is released for re-use.
*
*     This default behaviour is changed by calling this routine. The
*     specified bit number will continue to be associated with the quality
*     name even if all pixels do, or do not, hold the quality.
*
*     An error will be returned if the named quality is already associated
*     with a different bit number when this routine is called. An error
*     will also be reported if the specified bit number is already associated
*     with a different quality name.

*  Arguments:
*     LOCS(5) = CHARACTER * ( * ) (Given)
*        An array of five HDS locators. These locators identify the NDF
*        and the associated quality name information.  They should have
*        been obtained using routine IRQ_FIND or routine IRQ_NEW.
*     QNAME = CHARACTER * ( * ) (Given)
*        The quality name to use. Leading blanks are ignored and
*        the search is case-insensitive. The maximum allowed length for
*        quality names is given by symbolic constant IRQ__SZQNM which
*        currently has the value of 15.
*     BIT = INTEGER (Given)
*        The bit number to use. The least significant bit is Bit 1, not
*        Bit 0. If a value below 0 or above 8 is supplied, the
*        properties of the quality name are left unchanged, but the
*        FIXBIT value is still returned.
*     FIXBIT = LOGICAL (Returned)
*        Returned .TRUE. if the specified quality name had a fixed bit
*        number on entry to this routine, and .FALSE. otherwise.
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
*     4-MAR-2008 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants

*  Arguments Given:
      CHARACTER LOCS(5)*(*)
      CHARACTER QNAME*(*)
      INTEGER BIT

*  Arguments Given:
      LOGICAL FIXBIT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER FIRST              ! Position of first non-blank character
      INTEGER INDF               ! Identifier for the NDF containing the
                                 ! quality names information
      INTEGER LAST               ! Position of last non-blank character
      CHARACTER LQNAME*(IRQ__SZQNM) ! Upper case copy of quality name
      CHARACTER OLDNAM*(IRQ__SZQNM) ! Existing quality name
      LOGICAL RDONLY             ! Original read-only flag
      INTEGER SLOT               ! Index into the QUAL structure at
                                 ! which the name was found
      LOGICAL FIXED
      LOGICAL VALUE
      INTEGER OLDBIT
      CHARACTER COMMNT*100
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the NDF identifier from LOCS, and check it is still valid.
      CALL IRQ1_INDF( LOCS, INDF, STATUS )

*  Produce an uppercase copy of the supplied quality name, exluding
*  leading blanks.
      CALL CHR_FANDL( QNAME, FIRST, LAST )
      LQNAME = QNAME( FIRST : LAST )
      CALL CHR_UCASE( LQNAME )

*  Search for the requested quality name.
      CALL IRQ1_SEARC( LOCS, LQNAME( : LAST - FIRST + 1 ), FIXED, VALUE,
     :                 OLDBIT, COMMNT, RDONLY, FIXBIT, SLOT, STATUS )

*  Check the supplied bit number.
      IF( BIT .GE. 1 .AND. BIT .LE. 8 ) THEN

*  If the quality name is already associated with a different bit, report
*  an error.
         IF( OLDBIT .NE. 0 ) THEN

            IF( OLDBIT .NE. BIT ) THEN
               IF( STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'B', OLDBIT )
                  CALL ERR_REP( 'IRQ_FXBIT_ERR0', 'IRQ_FXBIT: The '//
     :                          'quality name is already associated '//
     :                          'with bit ^B.', STATUS )
               END IF
            END IF

         END IF

*  If the specified BIT number is already associated with a different
*  quality name, report an error.
         IF( OLDBIT .NE. BIT ) THEN
            CALL IRQ1_QBIT( LOCS, BIT, OLDNAM, STATUS )
            IF( OLDNAM .NE. ' ' .AND. OLDNAM .NE. QNAME .AND.
     :          STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'N', OLDNAM )
               CALL MSG_SETI( 'B', BIT )
               CALL ERR_REP( 'IRQ_FXBIT_ERR0', 'IRQ_FXBIT: The '//
     :                          'quality name ''^N'' is already '//
     :                          'associated with bit ^B.', STATUS )
            END IF
         END IF

*  Store the new information.
         CALL IRQ1_MOD( LOCS, SLOT, .FALSE., VALUE, BIT, VALUE, .TRUE.,
     :                  STATUS )
      END IF

*  If an error occur, give context information.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETI( 'B', BIT )
         CALL NDF_MSG( 'NDF', INDF )
         CALL MSG_SETC( 'QN', QNAME )
         CALL ERR_REP( 'IRQ_FXBIT_ERR1', 'IRQ_FXBIT: Unable to assign'//
     :                 ' fixed bit number ^B to quality name ^QN in '//
     :                 'NDF ^NDF', STATUS )
      END IF

      END
