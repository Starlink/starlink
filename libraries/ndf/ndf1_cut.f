      SUBROUTINE NDF1_CUT( IACB1, NDIM, LBND, UBND, IACB2, STATUS )
*+
*  Name:
*     NDF1_CUT

*  Purpose:
*     Cut a section from an existing NDF with an entry in the ACB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CUT( IACB1, NDIM, LBND, UBND, IACB2, STATUS )

*  Description:
*     The routine produces a new NDF entry in the ACB representing a
*     section from an existing NDF (which may itself be a section). The
*     bounds and dimensionality of the new NDF are specified in the
*     call to this routine and the resulting NDF has access to a subset
*     (although possibly a null subset) of the data accessible to the
*     initial NDF from which it is derived.

*  Arguments:
*     IACB1 = INTEGER (Given)
*        Index to the ACB entry for an existing NDF.
*     NDIM = INTEGER (Given)
*        Number of dimensions for the new section.
*     LBND( NDIM ) = INTEGER (Given)
*        Lower dimension bounds for the section.
*     UBND( NDIM ) = INTEGER (Given)
*        Upper dimension bounds for the section.
*     IACB2 = INTEGER (Returned)
*        Index to the ACB entry for the new section.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If STATUS is set on entry, then a value of zero will be
*     returned for the IACB2 argument, although no further processing
*     will occur.
*     -  A value of zero will also be returned for the IACB2 argument if
*     the routine fails for any reason.

*  Algorithm:
*     -  Set an initial value of zero for the IACB2 argument before
*     checking the inherited status.
*     -  Obtain an index to a free slot in the ACB.
*     -  If no slot could be obtained, then reset the IACB2 argument.
*     -  Transfer those ACB entries which are to be propagated from the
*     old NDF to the new one, marking the new NDF as a section.
*     -  Form a new section from the old data array, storing the
*     resulting new ARY_ system identifier in the new ACB entry.
*     -  Set an initial null identifer for the new quality array entry
*     in the ACB. Then replace it with an ARY_ system identifier for a
*     new section from the old quality array, if available.
*     -  Set an initial null identifer for the new variance array entry
*     in the ACB. Then replace it with an ARY_ system identifier for a
*     new section from the old variance array, if available.
*     -  If an error occurred, then clean up by annulling any
*     identifiers which may have been acquired and release the new ACB
*     slot.
*     -  Otherwise, increment the reference count for the data object
*     in the DCB.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-OCT-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     23-NOV-1989 (RFWS):
*        Changed to use new ARY_SECT routine.
*     12-DEC-1989 (RFWS):
*        Installed support for the variance component.
*     30-JAN-1990 (RFWS):
*        Installed support for the quality component.
*     1-FEB-1990 (RFWS):
*        Installed propagation of the quality component control flags.
*     23-OCT-1990 (RFWS):
*        Removed initialisation of ACB quantities which are now
*        processed by NDF1_FFS.
*     2-NOV-1990 (RFWS):
*        Removed unnecessary resetting of IACB2 to zero.
*     16-JUL-2012 (DSB):
*        Report an error if the number of pixels in the section exceeds
*        the current value of the SECMAX tuning parameter.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_REFCT( NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of ACB entries which refer to each DCB entry.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_ACC( NDF__MXACC, NDF__MXACB ) = LOGICAL (Read and Write)
*           NDF access control flags.
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Write)
*           Whether the NDF is a section.
*        ACB_DID( NDF__MXACB ) = INTEGER (Read and Write)
*           ARY_ system identifier for the NDF's data array.
*        ACB_ISQBB( NDF__MXACB ) = LOGICAL (Read and Write)
*           Whether a quality component bad-bits override value has been
*           set.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read and Write)
*           Index to associated DCB entry.
*        ACB_QBB( NDF__MXACB ) = BYTE (Read and Write)
*           Quality component bad-bits override value.
*        ACB_QID( NDF__MXACB ) = INTEGER (Read and Write)
*           ARY_ system identifier for the quality array.
*        ACB_VID( NDF__MXACB ) = INTEGER (Read and Write)
*           ARY_ system identifier for the variance array.

      INCLUDE 'NDF_TCB'          ! NDF_ Tuning Control Block
*        TCB_SECMAX = INTEGER (Read)
*           The maximum number of pixels in a section.

*  Arguments Given:
      INTEGER IACB1
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )

*  Arguments Returned:
      INTEGER IACB2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACC               ! Loop counter for access control flags
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER IDIM               ! Pixel axis index
      INTEGER*8 SECMAX           ! Maximum number of pixels in section
      INTEGER*8 SECSIZ           ! Number of pixels in section
      LOGICAL VALID              ! Whether array identifier is valid

*.

*  Set an initial value for the IACB2 argument.
      IACB2 = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of pixels in the section.
      SECSIZ = 1
      DO IDIM = 1, NDIM
         SECSIZ = SECSIZ*( UBND( IDIM ) - LBND( IDIM ) + 1 )
      END DO

*  Get the maximum number of pixels in a section. TCB_SECMAX is in units
*  of maga-pixels to allow use of INTEGER*4 tuning parameters.
      SECMAX = TCB_SECMAX
      SECMAX = SECMAX*1E6

*  Report an error if the section size it too large. Also check for
*  negative section sizes in case of overflow.
      IF( SECSIZ .GT. SECMAX ) THEN
         STATUS = NDF__BGSEC
         CALL MSG_SETK( 'S', SECSIZ )
         CALL ERR_REP( ' ', 'Requested NDF section contains too '//
     :                 'many pixels (^S).', STATUS )
         CALL MSG_SETK( 'L', SECMAX )
         CALL ERR_REP( ' ', 'Current value of NDF tuning parameter '//
     :                 'SECMAX limits sections to ^L pixels.', STATUS )

      ELSE IF( SECSIZ .LT. 0 ) THEN
         STATUS = NDF__BGSEC
         CALL ERR_REP( ' ', 'Requested NDF section contains too '//
     :                 'many pixels.', STATUS )
         CALL MSG_SETK( 'L', SECMAX )
         CALL ERR_REP( ' ', 'Current value of NDF tuning parameter '//
     :                 'SECMAX limits sections to ^L pixels.', STATUS )
      END IF

*  Obtain an index to a free slot for the new NDF in the ACB.
      CALL NDF1_FFS( NDF__ACB, IACB2, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Mark the new entry as a section.
         ACB_CUT( IACB2 ) = .TRUE.

*  Transfer the access control flags.
         DO 1 IACC = 1, NDF__MXACC
            ACB_ACC( IACC, IACB2 ) = ACB_ACC( IACC, IACB1 )
1        CONTINUE

*  Transfer the index to the data object entry in the DCB.
         ACB_IDCB( IACB2 ) = ACB_IDCB( IACB1 )

*  Propagate the quality component control flags.
         ACB_QBB( IACB2 ) = ACB_QBB( IACB1 )
         ACB_ISQBB( IACB2 ) = ACB_ISQBB( IACB1 )

*  DATA component:
*  ==============
*  Form a new section from the data array of the old NDF, storing the
*  new ARY_ system identifier in the new ACB entry.
         CALL ARY_SECT( ACB_DID( IACB1 ), NDIM, LBND, UBND,
     :                  ACB_DID( IACB2 ), STATUS )

*  QUALITY component:
*  ==================
*  Set an initial null value for the new ACB entry's quality array
*  identifier.
         ACB_QID( IACB2 ) = ARY__NOID

*  See if the ARY_ system quality array identifier in the old ACB entry
*  is valid.
         CALL ARY_VALID( ACB_QID( IACB1 ), VALID, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If so, then form a new section from it, storing the new identifier in
*  the new ACB entry.
            IF ( VALID ) THEN
               CALL ARY_SECT( ACB_QID( IACB1 ), NDIM, LBND, UBND,
     :                        ACB_QID( IACB2 ), STATUS )
            END IF
         END IF

*  VARIANCE component:
*  ==================
*  Set an initial null value for the new ACB entry's variance array
*  identifier.
         ACB_VID( IACB2 ) = ARY__NOID

*  See if the ARY_ system variance array identifier in the old ACB entry
*  is valid.
         CALL ARY_VALID( ACB_VID( IACB1 ), VALID, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If so, then form a new section from it, storing the new identifier in
*  the new ACB entry.
            IF ( VALID ) THEN
               CALL ARY_SECT( ACB_VID( IACB1 ), NDIM, LBND, UBND,
     :                        ACB_VID( IACB2 ), STATUS )
            END IF
         END IF

*  If an error occurred, then annul any identifiers which may have been
*  acquired and release the new ACB slot.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ARY_ANNUL( ACB_DID( IACB2 ), STATUS )
            CALL ARY_ANNUL( ACB_QID( IACB2 ), STATUS )
            CALL ARY_ANNUL( ACB_VID( IACB2 ), STATUS )
            CALL NDF1_RLS( NDF__ACB, IACB2, STATUS )

*  Otherwise, increment the data object reference count.
         ELSE
            IDCB = ACB_IDCB( IACB2 )
            DCB_REFCT( IDCB ) = DCB_REFCT( IDCB ) + 1
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CUT', STATUS )

      END
