      SUBROUTINE ARY1_DLSHP( LOC, MXDIM, DIM, NDIM, STATUS )
*+
*  Name:
*     ARY1_DLSHP

*  Purpose:
*     Return the dimensions of a DELTA array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DLSHP( LOC, MXDIM, DIM, NDIM, STATUS )

*  Description:
*     The routine uses the components in the supplied DELTA compressed
*     data object to determine the dinmensions of the uncompressed array.

*  Arguments:
*     LOC= INTEGER (Given)
*        Locator for a DELTA compressed data object.
*     MXDIM = INTEGER (Given)
*        The Maximum allowed number of axes.
*     DIM( MXDIM ) = INTEGER (Returned)
*        The number of pixels along each axis of the uncompressed array.
*     NDIM = INTEGER (Returned)
*        The number of pixel axes in the uncompressed array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S Berry (JAC,Hawaii)
*     {enter_new_authors_here}

*  History:
*     30-OCT-2010 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Arguments Given:
      CHARACTER LOC*(*)
      INTEGER MXDIM

*  Arguments Returned:
      INTEGER DIM( MXDIM )
      INTEGER NDIM

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      CHARACTER LOC2*( DAT__SZLOC ) ! Locator to component
      INTEGER IDIM                  ! Axis index
      INTEGER ZAXIS                 ! Index of compression axis
      INTEGER ZDIM                  ! Length of compression axis
      LOGICAL THERE                 ! Whether a component exists
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the ZAXIS component exists, get its value. Otherwise report an error.
      CALL DAT_THERE( LOC, 'ZAXIS', THERE, STATUS )
      IF( THERE ) THEN
         CALL DAT_FIND( LOC, 'ZAXIS', LOC2, STATUS )
         CALL DAT_GET0I( LOC2, ZAXIS, STATUS )
         CALL DAT_ANNUL( LOC2, STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = ARY__DLTIN
         CALL DAT_MSG( 'A', LOC )
         call ERR_REP( ' ', 'The DELTA compressed array ''^A'' is '//
     :                 'invalid - the ZAXIS component is missing.',
     :                 STATUS )
         GOTO 999
      END IF

*  If the ZDIM component exists, get its value. Otherwise report an error.
      CALL DAT_THERE( LOC, 'ZDIM', THERE, STATUS )
      IF( THERE ) THEN
         CALL DAT_FIND( LOC, 'ZDIM', LOC2, STATUS )
         CALL DAT_GET0I( LOC2, ZDIM, STATUS )
         CALL DAT_ANNUL( LOC2, STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = ARY__DLTIN
         CALL DAT_MSG( 'A', LOC )
         call ERR_REP( ' ', 'The DELTA compressed array ''^A'' is '//
     :                 'invalid - the ZDIM component is missing.',
     :                 STATUS )
         GOTO 999
      END IF

*  If the FIRST_DATA component exists, get its shape. Otherwise report an
*  error. The FIRST_DATA array has one less dimension than the
*  uncompressed array.
      CALL DAT_THERE( LOC, 'FIRST_DATA', THERE, STATUS )
      IF( THERE ) THEN
         CALL DAT_FIND( LOC, 'FIRST_DATA', LOC2, STATUS )
         CALL DAT_SHAPE( LOC2, ARY__MXDIM - 1, DIM, NDIM, STATUS )
         CALL DAT_ANNUL( LOC2, STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = ARY__DLTIN
         CALL DAT_MSG( 'A', LOC )
         call ERR_REP( ' ', 'The DELTA compressed array ''^A'' is '//
     :                 'invalid - the FIRST_DATA component is '//
     :                 'missing.', STATUS )
         GOTO 999
      END IF

*  Get the number of axes in the uncompressed array.
      NDIM = NDIM + 1

*  Report an error if there are too many.
      IF( NDIM .GT. MXDIM .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = ARY__DLTIN
         CALL DAT_MSG( 'A', LOC )
         CALL MSG_SETI( 'N', NDIM )
         CALL MSG_SETI( 'X', MXDIM )
         call ERR_REP( ' ', 'The DELTA compressed array ''^A'' is '//
     :                 'invalid - the number of axes (^N) is more '//
     :                 'than the allowed maximum (^X).', STATUS )
         GOTO 999
      END IF

*  Check the compression axis index is within the range of axis indices
*  allowed by the value of NDIM.
      IF( ( ZAXIS .LT. 1 .OR. ZAXIS .GT. NDIM ) .AND.
     :    STATUS .EQ. SAI__OK ) THEN
         STATUS = ARY__DLTIN
         CALL DAT_MSG( 'A', LOC )
         CALL MSG_SETI( 'I', ZAXIS )
         call ERR_REP( ' ', 'The DELTA compressed array ''^A'' is '//
     :                 'invalid - the ZAXIS value (^I) is illegal.',
     :                 STATUS )
         GOTO 999
      END IF

*  If all is OK, shuffle the high dimensions up to make room for the
*  compression axis, and then store the dimension of the compression axis
*  in the correct element of the returned array.
      IF( STATUS .EQ. SAI__OK ) THEN
         DO IDIM = NDIM, ZAXIS + 1, -1
            DIM( IDIM ) = DIM( IDIM - 1 )
         END DO
         DIM( ZAXIS ) = ZDIM
      END IF

*  Call error tracing routine and exit.
 999  CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DLSHP', STATUS )

      END
