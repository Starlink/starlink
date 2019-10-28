      SUBROUTINE IRQ_SETQM8( LOCS, BAD, QNAME, SIZE, MASK, SET, STATUS )
*+
*  Name:
*     IRQ_SETQM8

*  Purpose:
*     Assign a quality to pixels selected using a mask image, leaving
*     unselected pixels unchanged.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ_SETQM8( LOCS, BAD, QNAME, SIZE, MASK, SET, STATUS )

*  Description:
*     The quality specified by QNAME is assigned to all NDF pixels
*     which either do (or, if BAD is false,  do not) correspond to `bad'
*     pixels in the input mask array.  The quality of all other pixels
*     is left unchanged.  The quality name must be defined in the NDF
*     specified by LOCS (LOCS should be obtained either by calling
*     IRQ_FIND or IRQ_NEW). An error is reported if the quality name is
*     undefined.
*
*     Note, write or update access must be available for the NDF (as
*     set up by routine LPG_ASSOC for instance), and the QUALITY
*     component of the NDF must not be mapped on entry to this routine.
*
*  Arguments:
*     LOCS(5) = CHARACTER * ( * ) (Given)
*        An array of five HDS locators. These locators identify the NDF
*        and the associated quality name information.  They should have
*        been obtained using routine IRQ_FIND or routine IRQ_NEW.
*     BAD = LOGICAL (Given)
*        If true, then the quality is assigned to all NDF pixels
*        corresponding to `bad' pixels in the mask.  If false, then the
*        quality is assigned to all NDF pixels corresponding to pixels
*        which are not `bad' in the mask.
*     QNAME = CHARACTER * ( * ) (Given)
*        The quality name to assign to the selected pixels. This quality
*        name must be defined in the NDF specified by LOC. Name
*        definitions can be added to the NDF using routine IRQ_ADDQN.
*     SIZE = INTEGER*8 (Given)
*        The total number of pixels in the MASK array.
*     MASK( SIZE ) = REAL (Given)
*        A vector which defines the pixels to which the quality
*        specified by QNAME is to be assigned.  It is assumed that this
*        vector corresponds pixel-for-pixel with the vectorised NDF
*        supplied to IRQ_NEW or IRQ_FIND.
*     SET = INTEGER*8 (Returned)
*        The number of pixels in the NDF which hold the quality.
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
      INCLUDE 'IRQ_PAR'          ! IRQ constants.
      INCLUDE 'IRQ_ERR'          ! IRQ error values.
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER LOCS*(*)
      LOGICAL BAD
      CHARACTER QNAME*(*)
      INTEGER*8 SIZE
      REAL MASK( SIZE )

*  Arguments Returned:
      INTEGER*8 SET

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BIT                ! QUALITY bit corresponding to the
                                 ! quality name (LSB = 1).
      INTEGER*8 CLEAR            ! No. of pixels which do not hold the
                                 ! quality.
      CHARACTER COMMNT*(IRQ__SZCOM)! Descriptive comment stored with
                                 ! the quality name.
      LOGICAL DEF                ! True if QUALITY component is in a
                                 ! defined state.
      INTEGER FIRST              ! Position of first non-blank character
      LOGICAL FIXBIT             ! True if  fixed bit number is used
      LOGICAL FIXED              ! True if all pixels either do or don't
                                 ! have the quality.
      INTEGER INDF               ! Identifier for the NDF containing the
                                 ! quality names information.
      INTEGER LAST               ! Position of last non-blank character.
      CHARACTER LQNAME*(IRQ__SZQNM) ! Upper case copy of quality name.
      CHARACTER MODE*10          ! Mapping mode for QUALITY component.
      INTEGER*8 NBAD             ! No. of bad values found in the mask.
      INTEGER*8 NEL              ! No. of pixels in the NDF.
      INTEGER*8 NGOOD            ! No. of good values found in the mask.
      INTEGER PNT                ! Pointer to the mapped QUALITY array.
      LOGICAL RDONLY             ! Read-only flag for quality name.
      LOGICAL QMOD               ! Does QUALITY array need to be changed?
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

*  Count the number of good and bad pixels in the mask.
      CALL IRQ1_COUNT( SIZE, MASK, NGOOD, NBAD, STATUS )

*  Obtain the NDF identifier from LOCS, and check it is still valid.
      CALL IRQ1_INDF( LOCS, INDF, STATUS )

*  Produce an uppercase copy of the supplied quality name, exluding
*  leading blanks.
      CALL CHR_FANDL( QNAME, FIRST, LAST )
      LQNAME = QNAME( FIRST : LAST )
      CALL CHR_UCASE( LQNAME )

*  Find the quality name information.
      CALL IRQ1_SEARC( LOCS, LQNAME, FIXED, VALUE, BIT, COMMNT, RDONLY,
     :                 FIXBIT, SLOT, STATUS )

*  If all pixels already have the quality, assign a value to SET and
*  return without further action.
      IF( FIXED .AND. VALUE ) THEN
         CALL NDF_SIZE8( INDF, SET, STATUS )
         GO TO 999
      END IF

*  Check that write access is available to the NDF.
      CALL NDF_ISACC( INDF, 'WRITE', WRITE, STATUS )
      IF( .NOT. WRITE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IRQ__NOWRT
         CALL ERR_REP( 'IRQ_SETQM8_ERR1', 'IRQ_SETQM8: Write access '//
     :                 'is not available to the NDF.', STATUS )
      END IF

*  If all pixels in the mask are selected, change FIXED and VALUE to
*  indicate that all pixels hold the quality.
      IF(      BAD .AND. NGOOD .EQ. 0 .OR.
     :    .NOT.BAD .AND.  NBAD .EQ. 0 ) THEN
         FIXED = .TRUE.
         VALUE = .TRUE.
         CALL NDF_SIZE8( INDF, SET, STATUS )

*  If the quality name has a fixed bit number, we still need to modify
*  the QUALITY component.
         QMOD = FIXBIT

*  Otherwise, some but not all of the mask pixels are selected. So indicate
*  that we need to modify the QUALITY component.
      ELSE
         QMOD = .TRUE.
      END IF

*  If required, modify the QUALITY component of the NDF.
      IF( QMOD ) THEN

         CALL NDF_STATE( INDF, 'QUALITY', DEF, STATUS )
         IF( DEF ) THEN
            MODE = 'UPDATE'
         ELSE
            MODE = 'WRITE/ZERO'
         END IF

         CALL NDF_MAP8( INDF, 'QUALITY', '_UBYTE', MODE, PNT, NEL,
     :                  STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Check that the supplied mask has the same size as the NDF.
         IF( NEL .NE. SIZE ) THEN
            STATUS = IRQ__INCOM
            CALL ERR_REP( 'IRQ_SETQM8_ERR2',
     :      'IRQ_SETQM8: Supplied mask and NDF have different sizes.',
     :                    STATUS )
         END IF

*  If no bit plane in the QUALITY component was reserved for the
*  quality on input, reserve one know, and initialise it to indicate no
*  pixels currently hold the quality.
         IF( BIT .EQ. 0 ) THEN
            CALL IRQ1_RBIT( LOCS, BIT, STATUS )
            CALL IRQ1_QSET( BIT, .FALSE., SIZE, %VAL( CNF_PVAL( PNT ) ),
     :                      STATUS )
         END IF

*  Set the appropriate bit in the QUALITY array.
         CALL IRQ1_QMSK( BIT, BAD, .TRUE., SIZE, MASK,
     :                   %VAL( CNF_PVAL( PNT ) ),
     :                   STATUS )

*  Count the number of pixels which do and do not have the quality.
         CALL IRQ1_QCNT( BIT, SIZE, %VAL( CNF_PVAL( PNT ) ),
     :                   SET, CLEAR, STATUS )

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

      END IF

*  Update the quality information.
      CALL IRQ1_MOD( LOCS, SLOT, FIXED, VALUE, BIT, RDONLY, FIXBIT,
     :               STATUS )

*  If an error occur, give context information.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', INDF )
         CALL MSG_SETC( 'QN', QNAME )
         CALL ERR_REP( 'IRQ_SETQM8_ERR3',
     :          'IRQ_SETQM8: Unable to assign values for quality name'//
     :          ' ^QN to NDF ^NDF', STATUS )
      END IF

      END
