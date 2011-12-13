      SUBROUTINE NDF1_VAN( IACB, IAXIS, ALLOK, IAX1, IAX2, STATUS )
*+
*  Name:
*     NDF1_VAN

*  Purpose:
*     Validate an axis number.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_VAN( IACB, IAXIS, ALLOK, IAX1, IAX2, STATUS )

*  Description:
*     The routine checks an axis number for validity. If the number is
*     not valid, then an error is reported. Otherwise, the routine
*     returns the validated numbers of the first and last axes to be
*     processed.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the ACB entry for the NDF to which the axis number
*        refers.
*     IAXIS = INTEGER (Given)
*        Axis number to be validated.
*     ALLOK = LOGICAL (Given)
*        If this argument is set to .TRUE., then an axis number of zero
*        may be used to specify that all the axes are to be processed.
*        If it is set to .FALSE., then IAXIS may refer only to a single
*        axis.
*     IAX1 = INTEGER (Returned)
*        Number of first axis to be processed.
*     IAX2 = INTEGER (Returned)
*        Number of last axis to be processed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Obtain the bounds and number of dimensions of the NDF from the
*     ARY_ system identifier for its data array.
*     -  If all axes may be specified by giving an axis number of zero,
*     then set the numbers of the first and last axes to be processed.
*     -  If the axis number given is invalid, then report an error.
*     The error report has to be slightly different if the NDF is s
*     section.
*     -  Otherwise, set the first and last axes to refer to a specific
*     axis.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     29-MAY-1990 (RFWS):
*        Original version.
*     16-NOV-1990 (RFWS):
*        Improved the error message.
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
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether an NDF is a cut (i.e. section).
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.

*  Arguments Given:
      INTEGER IACB
      INTEGER IAXIS
      LOGICAL ALLOK

*  Arguments Returned:
      INTEGER IAX1
      INTEGER IAX2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the bounds and number of dimensions of the NDF from the ARY_
*  system identifier for its data array.
      CALL ARY_BOUND( ACB_DID( IACB ), NDF__MXDIM, LBND, UBND, NDIM,
     :                STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If all axes may be specified by giving an axis number of zero, then
*  set the numbers of the first and last axes to be processed.
         IF ( ALLOK .AND. ( IAXIS .EQ. 0 ) ) THEN
            IAX1 = 1
            IAX2 = NDIM

*  If the axis number given is invalid, then report an error.
         ELSE IF ( ( IAXIS .LT. 1 ) .OR. ( IAXIS .GT. NDIM ) ) THEN
            STATUS = NDF__AXNIN
            CALL MSG_SETI( 'IAXIS', IAXIS )
            CALL MSG_SETI( 'NDIM', NDIM )
            CALL NDF1_AMSG( 'NDF', IACB )

*  The error report has to be slightly different if the NDF is s
*  section.
            IF ( ACB_CUT( IACB ) ) THEN
               CALL ERR_REP( 'NDF1_VAN_BADS',
     :                       'Invalid axis number (^IAXIS) ' //
     :                       'specified; the identifier supplied ' //
     :                       'refers to the ^NDIM-dimensional NDF ' //
     :                       'section ^NDF (possible programming ' //
     :                       'error).', STATUS )
            ELSE
               CALL ERR_REP( 'NDF1_VAN_BAD',
     :                       'Invalid axis number (^IAXIS) ' //
     :                       'specified; the identifier supplied ' //
     :                       'refers to the ^NDIM-dimensional NDF ' //
     :                       '^NDF (possible programming error).',
     :                       STATUS )
            END IF

*  Otherwise, set the first and last axes to refer to a specific axis.
         ELSE
            IAX1 = IAXIS
            IAX2 = IAXIS
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_VAN', STATUS )

      END
