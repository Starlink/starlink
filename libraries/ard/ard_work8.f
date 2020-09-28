      SUBROUTINE ARD_WORK8( IGRP, NDIM, LBND, UBND, TRCOEF, CONCAT,
     :                      REGVAL, MASK, LBNDI, UBNDI, LBNDE, UBNDE,
     :                      STATUS )
*+
*  Name:
*     ARD_WORK8

*  Purpose:
*     Convert an ARD description into a pixel mask

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD_WORK8( IGRP, NDIM, LBND, UBND, TRCOEF, CONCAT, REGVAL,
*                     MASK, LBNDI, UBNDI, LBNDE, UBNDE, STATUS )

*  Description:
*     This routine is identical to ARD_WORK except it used INTEGER*8
*     values for the bounds arguments. Note, the supplied INTEGER*8
*     values are converted to INTEGER*4 before use, and an error will be
*     reported if this results in overflow.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group holding the ARD description.
*     NDIM = INTEGER (Given)
*        The number of pixl axes in the mask array.
*     LBND( NDIM ) = INTEGER*8 (Given)
*        The lower pixel index bounds of the mask array. The supplied
*        values must fall within the range fo a 4-byte INTEGER or an
*        error will be reported.
*     UBND( NDIM ) = INTEGER*8 (Given)
*        The upper pixel index bounds of the mask array. The supplied
*        values must fall within the range fo a 4-byte INTEGER or an
*        error will be reported.
*     TRCOEF( 0:NDIM, NDIM ) = REAL (Given)
*        The co-efficients of the mapping from application co-ordinates
*        (i.e. default user coordinates) to pixel co-ordinates. If the
*        first element is equal to VAL__BADR, then a unit mapping is used.
*        This argument is ignored if a call to ARD_WCS has already been
*        made to establish WCS Information.
*     CONCAT = LOGICAL (Given)
*        If .TRUE., then an INPUT keyword is inserted at the start of
*        the ARD description so long as the ARD description does not
*        already contain any INPUT keywords. If .FALSE., the ARD
*        description is left as supplied.
*     REGVAL = INTEGER (Given and Returned)
*        A positive integer to use to represent the first keyword in
*        the ARD description (excluding INPUT keywords). An error is
*        reported if the value 1 is supplied. If the supplied value is
*        negative or zero, then the value used is one greater than the
*        maximum pixel value supplied in MASK (except that 2 is used if
*        the maximum mask value is 1 or less). On return, REGVAL holds
*        one more than the largest value used to represent any of the
*        keywords in the ARD description.
*     MASK( * ) = INTEGER (Given and Returned)
*        The mask array. Any negative values in the supplied array are
*        treated as zero.
*     LBNDI( NDIM ) = INTEGER*8 (Returned)
*        The lower pixel bounds of a box which encompasses all internal
*        pixels. If there are no internal pixels in the returned mask,
*        each lower bound is returned greater than the corresponding
*        upper bound.
*     UBNDI( NDIM ) = INTEGER*8 (Returned)
*        The upper pixel bounds of a box which encompasses all internal
*        pixels.
*     LBNDE( NDIM ) = INTEGER*8 (Returned)
*        The lower pixel bounds of a box which encompasses all external
*        pixels. If there are no external pixels in the returned mask,
*        each lower bound is returned greater than the corresponding
*        upper bound.
*     UBNDE( NDIM ) = INTEGER*8 (Returned)
*        The upper pixel bounds of a box which encompasses all external
*        pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  An error is reported if the dimensionality of the ARD
*     description is different to that of the mask array (as specified
*     by argument NDIM).

*  Copyright:
*     Copyright (C) 2020 East Asian Observatory.
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
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     28-SEP-2020 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ARD_CONST'        ! ARD_ private constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      INTEGER IGRP
      INTEGER NDIM
      INTEGER*8 LBND( NDIM )
      INTEGER*8 UBND( NDIM )
      REAL TRCOEF( 0:NDIM, NDIM )
      LOGICAL CONCAT

*  Arguments Given and Returned:
      INTEGER REGVAL
      INTEGER MASK( * )

*  Arguments Returned:
      INTEGER*8 LBNDI( NDIM )
      INTEGER*8 UBNDI( NDIM )
      INTEGER*8 LBNDE( NDIM )
      INTEGER*8 UBNDE( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDIM
      INTEGER LBND4( ARD__MXDIM )
      INTEGER UBND4( ARD__MXDIM )
      INTEGER LBNDE4( ARD__MXDIM )
      INTEGER UBNDE4( ARD__MXDIM )
      INTEGER LBNDI4( ARD__MXDIM )
      INTEGER UBNDI4( ARD__MXDIM )
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert the supplied bounds to 4-byte integers, reporting an error if
*  an overflow occurs. */
      DO IDIM = 1, NDIM
         IF( LBND( IDIM ) .GE. VAL__MINI .AND.
     :       LBND( IDIM ) .LE. VAL__MAXI ) THEN
            LBND4( IDIM ) = LBND( IDIM )
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = ARD__TOOBG
            CALL MSG_SETI( 'I', IDIM )
            CALL MSG_SETK( 'B', LBND( IDIM ) )
            CALL ERR_REP( ' ', 'ARD_WORK8: Lower bound on axis ^I '//
     :                    'too big (^B)', STATUS )
         END IF

         IF( UBND( IDIM ) .GE. VAL__MINI .AND.
     :       UBND( IDIM ) .LE. VAL__MAXI ) THEN
            UBND4( IDIM ) = UBND( IDIM )
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = ARD__TOOBG
            CALL MSG_SETI( 'I', IDIM )
            CALL MSG_SETK( 'B', UBND( IDIM ) )
            CALL ERR_REP( ' ', 'ARD_WORK8: Upper bound on axis ^I '//
     :                    'too big (^B)', STATUS )
         END IF
      END DO

*  Call ARD_WORK.
      CALL ARD_WORK( IGRP, NDIM, LBND4, UBND4, TRCOEF, CONCAT, REGVAL,
     :               MASK, LBNDI4, UBNDI4, LBNDE4, UBNDE4, STATUS )

*  If no error has occurred, copy the returned 4-byte integer values to
*  the supplied INTEGER*8 arrays.
      IF( STATUS .EQ. SAI__OK ) THEN
         DO IDIM = 1, NDIM
            LBNDI( IDIM ) = LBNDI4( IDIM )
            UBNDI( IDIM ) = UBNDI4( IDIM )
            LBNDE( IDIM ) = LBNDE4( IDIM )
            UBNDE( IDIM ) = UBNDE4( IDIM )
         END DO
      END IF

      END
