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
*        The supplied string may optionally contain two names separated by
*        a comma. In this case the first name will be assigned to the
*        pixels selected by argument BAD and the second name will be
*        assigned to all other pixels.
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
*     1-NOV-2019 (DSB):
*        Combine initialisation, masking and counting into one pass round
*        the data array, performed by IRQ1_QMSK. This speeds things up
*        for big arrays.
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
      INCLUDE 'PRM_PAR'          ! VAL__ constants

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
      INTEGER BIT1               ! QUALITY bit corresponding to the
                                 ! quality name (LSB = 1).
      INTEGER BIT2               ! QUALITY bit corresponding to the
                                 ! quality name (LSB = 1).
      INTEGER*8 CLEAR            ! No. of pixels which do not hold the
                                 ! quality.
      INTEGER COMMA              ! Position of comma
      CHARACTER COMMNT*(IRQ__SZCOM)! Descriptive comment stored with
                                 ! the quality name.
      LOGICAL DEF                ! True if QUALITY component is in a
                                 ! defined state.
      INTEGER FIRST              ! Position of first non-blank character
      LOGICAL FIXBIT1            ! True if  fixed bit number is used
      LOGICAL FIXBIT2            ! True if  fixed bit number is used
      LOGICAL FIXED1             ! True if all pixels either do or don't
                                 ! have the quality.
      LOGICAL FIXED2             ! True if all pixels either do or don't
                                 ! have the quality.
      INTEGER*8 IEL              ! Index of array element
      INTEGER INDF               ! Identifier for the NDF containing the
                                 ! quality names information.
      LOGICAL INIT               ! Does quality bit need initialising?
      INTEGER LAST               ! Position of last non-blank character.
      CHARACTER LQNAME*(IRQ__SZQNM*3) ! Upper case copy of quality name.
      CHARACTER MODE*10          ! Mapping mode for QUALITY component.
      INTEGER*8 NEL              ! No. of pixels in the NDF.
      INTEGER PNT                ! Pointer to the mapped QUALITY array.
      LOGICAL RDONLY1            ! Read-only flag for quality name.
      LOGICAL RDONLY2            ! Read-only flag for quality name.
      LOGICAL QMOD               ! Does QUALITY array need to be changed?
      CHARACTER QNAME1*(IRQ__SZQNM) ! First quality name
      CHARACTER QNAME2*(IRQ__SZQNM) ! Second quality name
      INTEGER SLOT1              ! Index into the QUALITY_NAMES
                                 ! structure at which the new name will
                                 ! be stored.
      INTEGER SLOT2              ! Index into the QUALITY_NAMES
                                 ! structure at which the new name will
                                 ! be stored.
      LOGICAL ALLBAD             ! Are all mask values bad?
      LOGICAL ALLGOOD            ! Are all mask values good?
      LOGICAL VALUE1             ! True if all pixels have the quality,
                                 ! false if no pixels used to have the
                                 ! quality, indeterminate if some did
                                 ! and some didn't.
      LOGICAL VALUE2             ! True if all pixels have the quality,
                                 ! false if no pixels used to have the
                                 ! quality, indeterminate if some did
                                 ! and some didn't.
      LOGICAL WRITE              ! True if write access is available to
                                 ! the NDF.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if all mask values are good or bad.
      ALLGOOD = .TRUE.
      ALLBAD = .TRUE.
      DO IEL = 1, SIZE
         IF( MASK( IEL ) .EQ. VAL__BADR ) THEN
            ALLGOOD = .FALSE.
            IF( .NOT. ALLBAD ) GO TO 10
         ELSE
            ALLBAD = .FALSE.
            IF( .NOT. ALLGOOD ) GO TO 10
         END IF
      END DO
 10   CONTINUE

*  Obtain the NDF identifier from LOCS, and check it is still valid.
      CALL IRQ1_INDF( LOCS, INDF, STATUS )

*  Produce an uppercase copy of the supplied quality name string, exluding
*  leading blanks.
      CALL CHR_FANDL( QNAME, FIRST, LAST )
      LQNAME = QNAME( FIRST : LAST )
      CALL CHR_UCASE( LQNAME )

*  If the QNAME string contains a comma, extract the two names.
      COMMA = INDEX( LQNAME, ',')
      IF( COMMA .EQ. 0 ) THEN
         QNAME1 = LQNAME
         QNAME2  = ' '
      ELSE IF( COMMA .EQ. 1 ) THEN
         STATUS = IRQ__BADNM
         CALL ERR_REP( ' ', 'IRQ_SETQM8: blank quality name supplied.',
     :                 STATUS )
      ELSE
         CALL CHR_FANDL( LQNAME( : COMMA - 1 ), FIRST, LAST )
         QNAME1 = LQNAME( FIRST : LAST )
         CALL CHR_FANDL( LQNAME( COMMA + 1 :  ), FIRST, LAST )
         QNAME2  = LQNAME( COMMA + FIRST: COMMA + LAST )
      END IF

*  Find the quality name information.
      CALL IRQ1_SEARC( LOCS, QNAME1, FIXED1, VALUE1, BIT1, COMMNT,
     :                 RDONLY1, FIXBIT1, SLOT1, STATUS )

*  Set a flag indicating if the quality bit needs initialising.
      INIT = ( BIT1 .EQ. 0 )

*  If all pixels already have the quality, assign a value to SET and
*  return without further action.
      IF( FIXED1 .AND. VALUE1 ) THEN
         CALL NDF_SIZE8( INDF, SET, STATUS )
         GO TO 999
      END IF

*  Find any secondary quality name information.
      IF( QNAME2 .NE. ' ' ) THEN
         CALL IRQ1_SEARC( LOCS, QNAME2, FIXED2, VALUE2, BIT2, COMMNT,
     :                    RDONLY2, FIXBIT2, SLOT2, STATUS )
      ELSE
         BIT2 = 0
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
      IF(      BAD .AND. ALLBAD .OR.
     :    .NOT.BAD .AND. ALLGOOD ) THEN
         FIXED1 = .TRUE.
         VALUE1 = .TRUE.
         CALL NDF_SIZE8( INDF, SET, STATUS )

*  If the quality name has a fixed bit number, we still need to modify
*  the QUALITY component.
         QMOD = FIXBIT1

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
*  quality on input, reserve one now.
         IF( QNAME2 .EQ. ' ' ) THEN
            IF( BIT1 .EQ. 0 ) CALL IRQ1_RBIT( LOCS, BIT1, STATUS )
         ELSE
            IF( BIT1 .EQ. 0 .AND. BIT2 .EQ. 0 ) THEN
               CALL IRQ1_RBIT2( LOCS, BIT1, BIT2, STATUS )
            ELSE IF( BIT1 .EQ. 0 ) THEN
               CALL IRQ1_RBIT( LOCS, BIT1, STATUS )
            ELSE IF( BIT2 .EQ. 0 ) THEN
               CALL IRQ1_RBIT( LOCS, BIT2, STATUS )
            END IF
         END IF

*  Set the appropriate bit in the QUALITY array. If the bit is new,
*  initialise unselected pixel to indicate they do not hold the quality.
*  This returns the number of pixels which do and do not have the quality
*  on exit.
         CALL IRQ1_QMSK( BIT1, BIT2, BAD, .TRUE., INIT, SIZE,
     :                   MASK, %VAL( CNF_PVAL( PNT ) ), SET, CLEAR,
     :                   STATUS )

*  Unmap the QUALITY array.
         CALL NDF_UNMAP( INDF, 'QUALITY', STATUS )

*  Determine new settings for FIXED and VALUE.
         IF( SET .EQ. 0 ) THEN
            FIXED1 = .TRUE.
            VALUE1 = .FALSE.
            FIXED2 = .TRUE.
            VALUE2 = .TRUE.

         ELSE IF ( CLEAR .EQ. 0 ) THEN
            FIXED1 = .TRUE.
            VALUE1 = .TRUE.
            FIXED2 = .TRUE.
            VALUE2 = .FALSE.

         ELSE
            FIXED1 = .FALSE.
            FIXED2 = .FALSE.

         ENDIF

      END IF

*  Update the quality information.
      CALL IRQ1_MOD( LOCS, SLOT1, FIXED1, VALUE1, BIT1, RDONLY1,
     :               FIXBIT1, STATUS )

      IF( QNAME2 .NE. ' ' ) THEN
         CALL IRQ1_MOD( LOCS, SLOT2, FIXED2, VALUE2, BIT2, RDONLY2,
     :                  FIXBIT2, STATUS )
      END IF

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
