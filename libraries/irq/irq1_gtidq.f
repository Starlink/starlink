      SUBROUTINE IRQ1_GTIDQ( LOCS, NMASKS, MASKS, NOPC, OPCODE, MXSTK,
     :                       IDQ, STATUS )
*+
*  Name:
*     IRQ1_GTIDQ

*  Purpose:
*     Create a new compiled quality expression structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_GTIDQ( LOCS, NMASKS, MASKS, NOPC, OPCODE, MXSTK, IDQ,
*                      STATUS )

*  Description:
*     The IRQ_ system stores information describing each compiled
*     quality expression in arrays in common.  The same element from
*     different arrays holds information for the same compiled quality
*     expression. Different elements within each array hold information
*     for different compiled quality expressions. The identifiers used
*     by IRQ to identify each compiled quality expression are just the
*     index into these common arrays at which the information
*     describing the compiled quality expression is stored. The arrays
*     in common have a lower bound of 1 and an upper bound given by
*     symbolic constant IRQ__MAXQ.
*
*     The common arrays hold the scalars needed to describe each
*     compiled quality expression.  The vectors needed to describe each
*     compiled quality expression are stored in temporary HDS
*     structures. These structures have an HDS type of QEXP. An array
*     of QEXP structures is created, of equal size to the common
*     arrays.  Each cell of this array is a single QEXP structure and
*     contains the following components:
*
*     MASKS( QCM_NMASK ) (_INTEGER) - These are bit masks which are
*     combined with each value from the QUALITY component of the NDF.
*     The uses of these masks is described in routine IRQ1_CMQM.
*
*     OPCODE( QCM_NOPC ) (_INTEGER) - Integer codes identifying the
*     operations which must be performed to evaluate the quality
*     expression. The codes are defined in module IRQ_OPC.
*
*     HDS locators to all these components are stored in common. The
*     arrays are kept permanently mapped until the compiled quality
*     expression is annulled.  The pointers are also stored in common.
*
*     This routine finds the lowest common array index not currently in
*     use, and returns it as the IDQ identifier value. If there is no
*     free space in the common arrays an error is reported. The
*     identifier thus found is used as an index into the HDS array of
*     QEXP structures. The components listed above are created within
*     the QEXP structure indexed by IDQ. The locator to the QEXP
*     structure is stored in common.
*
*     If any error occurs, an invalid compiled quality expression
*     identifier (IRQ__NOID) is returned.

*  Arguments:
*     LOCS( 5 ) = CHARACTER * ( * ) (Given)
*        A set of 5 HDS locators. LOCS( 1 ) locates a temporary
*        structure holding a cloned NDF identifier. LOCS(2) locates the
*        QUAL array. LOCS(3) locates the LAST_USED value, holding the
*        index of the last used slot in the QUAL array. LOCS(4) locates
*        the NFREE value, holding the number of free slots in the QUAL
*        array. LOCS(5) locates the FREE array, which contains a stack
*        of the NFREE slot indices corresponding to free slots. This
*        stack is accessed in a "First-In-Last-Out" method.
*     NMASKS = INTEGER (Given)
*        No. of masks needed to evaluate the quality expression.
*     MASKS( * ) = INTEGER (Given)
*        Masks defining tests to be performed on the QUALITY component.
*        These are accessed by the "LOAD QUALITY" instructions stored in
*        OPCODE, in the order in which such instructions appear in
*        OPCODE.
*     NOPC = INTEGER (Given)
*        The number of op. codes in OPCODE.
*     OPCODE( * ) = INTEGER (Given)
*        The codes which define the operations which must be performed
*        in order to evaluate the quality expression.
*     MXSTK = INTEGER (Given)
*        The maximum stack size needed to evaluate the quality
*        expression.
*     IDQ = INTEGER (Returned)
*        The IRQ identifier for the new compile quality expression. Set
*        to IRQ__NOID if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     12-JUL-1991 (DSB):
*        Original version.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

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

*  Global Variables:
      INCLUDE 'IRQ_COM'          ! IRQ common blocks.
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
*        QCM_INDF( IRQ__MAXQ ) = INTEGER (Write)
*           Cloned NDF identifiers for the NDFs to which each quality
*           expression refers.
*        QCM_LOC = CHARACTER (Read)
*           HDS locator to the array of QEXP structures.
*        QCM_LOCQ( IRQ__MAXQ ) = CHARACTER (Write)
*           HDS locators to the individual QEXP structures.
*        QCM_LOCMS( IRQ__MAXQ ) = CHARACTER (Write)
*           HDS locator to the MASKS component of each QEXP structure.
*        QCM_LOCOP( IRQ__MAXQ ) = CHARACTER (Write)
*           HDS locator to the OPCODE component of each QEXP structure.
*        QCM_MSPNT( IRQ__MAXQ ) = INTEGER (Write)
*           Pointer to mapped array holding bit masks.
*        QCM_MXSTK( IRQ__MAXQ ) = INTEGER (Write)
*           Maximum stack size needed to evaluate quality expression.
*        QCM_NMASK( IRQ__MAXQ ) = INTEGER (Write)
*           No. of quality masks needed to evaluate the quality
*           expression.
*        QCM_NOPC( IRQ__MAXQ ) = INTEGER (Write)
*           No. of operations needed to evaluate the quality expression.
*        QCM_OPPNT( IRQ__MAXQ ) = INTEGER (Write)
*           Pointer to mapped array holding op. codes.
*        QCM_VALID( IRQ__MAXQ ) = LOGICAL (Read and Write)
*           True if the corresponding compiled quality expression identifier is
*           use).

*  Arguments Given:
      CHARACTER LOCS(5)*(*)
      INTEGER NMASKS
      INTEGER MASKS( * )
      INTEGER NOPC
      INTEGER OPCODE( * )
      INTEGER MXSTK

*  Arguments Returned:
      INTEGER IDQ

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count.
      INTEGER IERR               ! Index which caused first numerical
                                 ! error.
      INTEGER INDF               ! Cloned NDF identifier.
      INTEGER NEL                ! No. of elements in mapped array.
      INTEGER NERR               ! Number of numerical errors which
                                 ! occurred.

*.

*  Set the output compiled quality expression identifier invalid before checking
      IDQ = IRQ__NOID

*  Check inherited global status.If bad, return with an invalid
*  identifier.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the lowest IRQ identifier not currently in use.
      DO I = 1, IRQ__MAXQ
         IF( .NOT.QCM_VALID( I ) .AND. IDQ .EQ. IRQ__NOID ) IDQ = I
      END DO

*  If no valid identifier was found, give an error report.
      IF( IDQ .EQ. IRQ__NOID ) THEN
         STATUS = IRQ__NOMOR
         CALL ERR_REP('IRQ1_GTIDQ_ERR1',
     : 'IRQ1_GTIDQ: Maximum number of quality expressions exceeded',
     :               STATUS )
      END IF

*  Get the cloned NDF identifier from LOCS(1) and check it is still
*  valid.
      CALL IRQ1_INDF( LOCS, INDF, STATUS )

*  Store values in common.
      QCM_INDF( IDQ ) = INDF
      QCM_NMASK( IDQ ) = NMASKS
      QCM_MXSTK( IDQ ) = MXSTK
      QCM_NOPC( IDQ ) = NOPC

*  Get a locator to the QEXP structure with index equal to IDQ, and
*  store in common.
      CALL DAT_CELL( QCM_LOC, 1, IDQ, QCM_LOCQ( IDQ ), STATUS )

*  Create an integer array component called MASKS, within the
*  QEXP structure.
      CALL DAT_NEW( QCM_LOCQ( IDQ ), 'MASKS', '_INTEGER', 1,
     :              MAX( 1, NMASKS ), STATUS )

*  Get a locator to it, and store it in common.
      CALL DAT_FIND( QCM_LOCQ( IDQ ), 'MASKS', QCM_LOCMS( IDQ ),
     :               STATUS )

*  Map the MASKS array, storing the pointer in common.
      CALL DAT_MAPV( QCM_LOCMS( IDQ ), '_INTEGER', 'WRITE',
     :               QCM_MSPNT( IDQ ), NEL, STATUS )

*  Store the supplied masks in the MASKS array.
      CALL VEC_ITOI( .FALSE., NEL, MASKS,
     :               %VAL( CNF_PVAL( QCM_MSPNT( IDQ ) ) ),
     :                IERR, NERR, STATUS )

*  Now do the same for the OPCODE array, which holds the op. codes
*  supplied in OPCODE.
      CALL DAT_NEW( QCM_LOCQ( IDQ ), 'OPCODE', '_INTEGER', 1, NOPC,
     :              STATUS )
      CALL DAT_FIND( QCM_LOCQ( IDQ ), 'OPCODE', QCM_LOCOP( IDQ ),
     :               STATUS )
      CALL DAT_MAPV( QCM_LOCOP( IDQ ), '_INTEGER', 'WRITE',
     :               QCM_OPPNT( IDQ ), NEL, STATUS )
      CALL VEC_ITOI( .FALSE., NEL, OPCODE,
     :               %VAL( CNF_PVAL( QCM_OPPNT( IDQ ) ) ),
     :                IERR, NERR, STATUS )

*  If all is OK, indicate that the identifier is in use.
      IF ( STATUS .EQ. SAI__OK ) QCM_VALID( IDQ ) = .TRUE.

*  If an error has occurred ensure the identifier is invalid, and
*  give a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         IDQ = IRQ__NOID
         CALL ERR_REP( 'IRQ1_GTIDQ_ERR2',
     :'IRQ1_GTIDQ: Unable to store a compiled quality expression',
     :                 STATUS )
      END IF

      END
