      SUBROUTINE IRQ_RWQN( LOCS, QNAME, SET, NEWVAL, OLDVAL, STATUS )
*+
*  Name:
*     IRQ_RWQN

*  Purpose:
*     Get and/or set the read-only flag for a quality name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ_RWQN( LOCS, QNAME, SET, NEWVAL, OLDVAL, STATUS )

*  Description:
*     This routine returns the current value of the read-only flag
*     associated with a quality name, and optionally assigns a new
*     value to the flag.
*
*     If the read-only flag is set for a quality name, any attempt to
*     remove the quality name using IRQ_REMQN will result in an error
*     being reported.

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
*     SET = LOGICAL (Given)
*        If true, then the read-only flag for the quality name will be
*        set to the value supplied in NEWVAL. Otherwise, the current
*        value of the flag will be left unchanged.
*     NEWVAL = LOGICAL (Given)
*        The new value for the read-only flag. Only accessed if SET is
*        true.
*     OLDVAL = LOGICAL (Returned)
*        The value of the read-only flag on entry to this routine. If the
*        old value is of no interest, it is safe to supply the same
*        variable for OLDVAL as for NEWVAL since OLDVAL is updated after
*        NEWVAL is used.
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
*     15-FEB-2008 (DSB):
*        Original version.
*     4-MAR-2008 (DSB):
*        Added FIXBIT.
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

*  Arguments Given:
      CHARACTER LOCS(5)*(*)
      CHARACTER QNAME*(*)
      LOGICAL SET
      LOGICAL NEWVAL

*  Arguments returned:
      LOGICAL OLDVAL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER FIRST              ! Position of first non-blank character
      INTEGER INDF               ! Identifier for the NDF containing the
                                 ! quality names information.
      INTEGER LAST               ! Position of last non-blank character.
      CHARACTER LQNAME*(IRQ__SZQNM) ! Upper case copy of quality name.
      LOGICAL RDONLY             ! Original read-only flag
      INTEGER SLOT               ! Index into the QUAL structure at
                                 ! which the name was found.
      LOGICAL FIXBIT
      LOGICAL FIXED
      LOGICAL VALUE
      INTEGER BIT
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

*  Search for the requested quality name, and return the original
*  read-only flag value.
      CALL IRQ1_SEARC( LOCS, LQNAME( : LAST - FIRST + 1 ), FIXED, VALUE,
     :                 BIT, COMMNT, RDONLY, FIXBIT, SLOT, STATUS )

*  If required, set the new value.
      IF( SET ) THEN
         CALL IRQ1_MOD( LOCS, SLOT, FIXED, VALUE, BIT, NEWVAL,
     :                  FIXBIT, STATUS )
      END IF

*  Now that the new value has been set, it is safe to return the old value
*  (OLDVAL and NEWVAL may possibly refer to the same variable in the
*  caller).
      OLDVAL = RDONLY

*  If an error occur, give context information.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', INDF )
         CALL MSG_SETC( 'QN', QNAME )
         CALL ERR_REP( 'IRQ_RWQN_ERR1', 'IRQ_RWQN: Unable to set or '//
     :                 'get read-only flag for quality name ^QN in '//
     :                 'NDF ^NDF', STATUS )
      END IF

      END
