      SUBROUTINE IRQ_RESQL8( LOCS, LISTED, QNAME, NDIM, NCOORD, LIST,
     :                       SET, STATUS )
*+
*  Name:
*     IRQ_RESQL8

*  Purpose:
*     Remove a quality from a list of pixels, leaving unlisted pixels
*     unchanged.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ_RESQL8( LOCS, LISTED, QNAME, NDIM, NCOORD, LIST, SET,
*                      STATUS )

*  Description:
*     The quality specified by QNAME is removed from all pixels
*     included in (or, if LISTED is false, not included in) the
*     supplied list of pixel indices. The quality of other pixels
*     is left unaltered.  The quality name must be defined in the NDF
*     specified by LOCS (LOCS should be obtained either by calling
*     IRQ_FIND or IRQ_NEW). An error is reported if the quality name is
*     undefined.
*
*     Note, write or update access must be available for the NDF (as
*     set up by routine LPG_ASSOC for instance), and the QUALITY
*     component must not be mapped on entry to this routine.
*
*  Arguments:
*     LOCS(5) = CHARACTER * ( * ) (Given)
*        An array of five HDS locators. These locators identify the NDF
*        and the associated quality name information.  They should have
*        been obtained using routine IRQ_FIND or routine IRQ_NEW.
*     LISTED = LOGICAL (Given)
*        If true, then the quality is removed from all pixels included
*        in the list given by LIST.  If false, then the quality is
*        removed from all pixels not included in the list given by LIST.
*     QNAME = CHARACTER * ( * ) (Given)
*        The quality name to be removed from the selected pixels. This
*        quality name must be defined in the NDF specified by LOC. Name
*        definitions can be added to the NDF using routine IRQ_ADDQN.
*     NDIM = INTEGER (Given)
*        The number of values required to specify a pixel position
*        (i.e. the number of dimensions in the NDF).
*     NCOORD = INTEGER*8 (Given)
*        The number of pixels included in the input list.
*     LIST( NDIM, NCOORD ) = INTEGER*8 (Given)
*        The list of pixel indices. Any indices which lie outside the
*        bounds of the NDF are ignored.
*     SET = INTEGER*8 (Returned)
*        The number of pixels which hold the quality.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory
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
*     24-OCT-2019 (DSB):
*        Original version.
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
      LOGICAL LISTED
      CHARACTER QNAME*(*)
      INTEGER NDIM
      INTEGER*8 NCOORD
      INTEGER*8 LIST( NDIM, NCOORD )

*  Arguments Returned:
      INTEGER*8 SET

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BIT                ! QUALITY bit corresponding to the
                                 ! quality name (LSB = 1).
      INTEGER*8 CLEAR            ! No. of pixels without the quality.
      CHARACTER COMMNT*(IRQ__SZCOM)! Descriptive comment stored with
                                 ! the quality name.
      LOGICAL DEF                ! True if the QUALITY component is in a
                                 ! defined state.
      INTEGER FIRST              ! Position of first non-blank character
      LOGICAL FIXBIT             ! Does quality have a fixed bit number?
      LOGICAL FIXED              ! True if all pixels either do or don't
                                 ! have the quality.
      INTEGER INDF               ! Identifier for the NDF containing the
                                 ! quality names information.
      INTEGER LAST               ! Position of last non-blank character.
      INTEGER*8 LBND( NDF__MXDIM ) ! Lower bounds of the NDF.
      CHARACTER LQNAME*(IRQ__SZQNM) ! Upper case copy of quality name.
      INTEGER MDIM               ! NDF dimensionality.
      CHARACTER MODE*10          ! Mapping mode for the QUALITY array.
      INTEGER*8 NEL              ! No. of pixels in the NDF.
      INTEGER PNT                ! Pointer to the mapped QUALITY array.
      LOGICAL RDONLY             ! Read-only flag for quality name.
      INTEGER SLOT               ! Index into the QUALITY_NAMES
                                 ! structure at which the new name will
                                 ! be stored.
      INTEGER*8 UBND( NDF__MXDIM ) ! Upper bounds of the NDF.
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

*  Get the NDF bounds, and check that the supplied value for argument
*  NDIM matches the number of dimensions in the NDF.
      CALL NDF_BOUND8( INDF, NDF__MXDIM, LBND, UBND, MDIM, STATUS )
      IF( STATUS .EQ. SAI__OK .AND. MDIM .NE. NDIM ) THEN
         STATUS = IRQ__BADDM
         CALL MSG_SETI( 'ND', NDIM )
         CALL MSG_SETI( 'MD', MDIM )
         CALL ERR_REP( 'IRQ_RESQL8_ERR1',
     : 'IRQ_RESQL8: Supplied dimensionality (^ND) does not match the '//
     : 'NDF dimensionality (^MDIM)', STATUS )
      END IF

*  Produce an uppercase copy of the supplied quality name, exluding
*  leading blanks.
      CALL CHR_FANDL( QNAME, FIRST, LAST )
      LQNAME = QNAME( FIRST : LAST )
      CALL CHR_UCASE( LQNAME )

*  Find the quality name information.
      CALL IRQ1_SEARC( LOCS, LQNAME, FIXED, VALUE, BIT, COMMNT, RDONLY,
     :                 FIXBIT, SLOT, STATUS )

*  If no pixels currently hold the quality, removing the quality from
*  the selected pixels won't change anything, so return without further
*  action.
      IF( FIXED .AND. (.NOT.VALUE) .AND. STATUS .EQ. SAI__OK ) GO TO 999

*  Check that write access is available to the NDF.
      CALL NDF_ISACC( INDF, 'WRITE', WRITE, STATUS )
      IF( .NOT. WRITE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IRQ__NOWRT
         CALL ERR_REP( 'IRQ_RESQL8_ERR2', 'IRQ_RESQL8: Write access '//
     :                 'is not available to the NDF.', STATUS )
      END IF

*  Map the QUALITY component of the NDF.
      CALL NDF_STATE( INDF, 'QUALITY', DEF, STATUS )
      IF( DEF ) THEN
         MODE = 'UPDATE'
      ELSE
         MODE = 'WRITE/ZERO'
      END IF

      CALL NDF_MAP8( INDF, 'QUALITY', '_UBYTE', MODE, PNT, NEL,
     :               STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If there is currently no bit plane reserved for this quality,
*  reserve one now and initialise it to indicate that all pixels held
*  the quality on entry to this routine.
      IF( BIT .EQ. 0 ) THEN
         CALL IRQ1_RBIT( LOCS, BIT, STATUS )
         CALL IRQ1_QSET( BIT, .TRUE., NEL, %VAL( CNF_PVAL( PNT ) ),
     :                   STATUS )
      END IF

*  Clear the appropriate bit in the QUALITY array for the selected
*  pixels.
      CALL IRQ1_QLST8( BIT, LISTED, .FALSE., NDIM, NCOORD, LIST, LBND,
     :                 UBND, NEL, %VAL( CNF_PVAL( PNT ) ), STATUS )

*  Count the number of pixels for which the bit is set or clear.
      CALL IRQ1_QCNT( BIT, NEL, %VAL( CNF_PVAL( PNT ) ),
     :                SET, CLEAR, STATUS )

*  Unmap the QUALITY array.
      CALL NDF_UNMAP( INDF, 'QUALITY', STATUS )

*  Determine new settings for FIXED and VALUE.
      IF( SET .EQ. 0 ) THEN
         FIXED = .TRUE.
         VALUE = .FALSE.

      ELSE IF ( CLEAR .EQ. 0 ) THEN
         FIXED = .TRUE.
         VALUE = .TRUE.

      ELSE
         FIXED = .FALSE.

      ENDIF

*  Modify the FIXED, VALUE and BIT settings in the quality name
*  information.
      CALL IRQ1_MOD( LOCS, SLOT, FIXED, VALUE, BIT, RDONLY, FIXBIT,
     :               STATUS )

*  If an error occur, give context information.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', INDF )
         CALL MSG_SETC( 'QN', QNAME )
         CALL ERR_REP( 'IRQ_RESQL8_ERR3',
     :          'IRQ_RESQL8: Unable to assign quality name '//
     :          '^QN to pixels in NDF ^NDF', STATUS )
      END IF

      END
