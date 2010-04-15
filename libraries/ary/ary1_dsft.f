      SUBROUTINE ARY1_DSFT( NSHIFT, SHIFT, IDCB, STATUS )
*+
*  Name:
*     ARY1_DSFT

*  Purpose:
*     Apply pixel shifts to a data object entry in the DCB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DSFT( NSHIFT, SHIFT, IDCB, STATUS )

*  Description:
*     The routine applies a set of pixel shifts to a data object
*     identified by its index in the DCB. An integer shift is applied
*     to each dimension so that the array maintains the same data
*     content, although its bounds (and the indices of each pixel)
*     change by the amount of the shift applied to the corresponding
*     dimension. The DCB entry is updated to reflect the changes.

*  Arguments:
*     NSHIFT = INTEGER (Given)
*        Number of dimensions to which shifts are to be applied. If
*        more shifts are specified than there are dimensions in the
*        data object, then the excess shifts are disregarded. If fewer
*        shifts are specified, then the extra dimensions are not
*        shifted.
*     SHIFT( NSHIFT ) = INTEGER (Given)
*        The shifts to be applied to each dimension.
*     IDCB = INTEGER (Given)
*        Index to the entry in the DCB identifying the data object to
*        be shifted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Ensure that form and bounds information for the data object is
*     available in the DCB.
*     -  Handle each form of array in turn.
*     -  Handle primitive and simple arrays in the same way.
*     -  If the array is primitive, then it must first be converted to
*     simple form in order to apply pixel shifts.
*     -  Report context information if the conversion failed.
*     -  See if the array (which is now simple) has an ORIGIN
*     component. If not, then create one.
*     -  Apply the pixel shifts to the array bounds and the accumulated
*     pixel shifts held in the DCB.
*     -  Write new values to the ORIGIN component to reflect the array's
*     new origin position.
*     -  If the form information in the DCB was not recognised, then
*     report an error.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     2-AUG-1989 (RFWS):
*        Original version.
*     13-FEB-1990 (RFWS):
*        Installed support for primitive arrays.
*     5-MAY-2006 (DSB):
*        Installed support for scaled arrays.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_FRM( ARY__MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Read)
*           Form of data object.
*        DCB_KBND( ARY__MXDCB ) = LOGICAL (Write)
*           Whether bounds information is available in the DCB.
*        DCB_LBND( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Read and Write)
*           Lower bounds of data object.
*        DCB_LOC( ARY_MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to data object.
*        DCB_NDIM( ARY__MXDCB ) = INTEGER (Read)
*           Number of data object dimensions.
*        DCB_SFT( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Read and Write)
*           Accumulated pixel shifts for the data object.
*        DCB_UBND( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Read and Write)
*           Upper bounds of data object.

*  Arguments Given:
      INTEGER NSHIFT
      INTEGER SHIFT( NSHIFT )
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for dimensions
      LOGICAL THERE              ! Whether there is an ORIGIN component

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that form and bounds (and dimensionality) information is
*  available in the DCB.
      CALL ARY1_DFRM( IDCB, STATUS )
      CALL ARY1_DBND( IDCB, STATUS )

*  Handle each form of array in turn.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Primitive, scaled and simple arrays.
*  ====================================
*  All forms of array are handled here.
         IF ( ( DCB_FRM( IDCB ) .EQ. 'PRIMITIVE' ) .OR.
     :        ( DCB_FRM( IDCB ) .EQ. 'SCALED' ) .OR.
     :        ( DCB_FRM( IDCB ) .EQ. 'SIMPLE' ) ) THEN

*  If the array is primitive, then it must first be converted to simple
*  form in order to apply pixel shifts.
            IF ( DCB_FRM( IDCB ) .EQ. 'PRIMITIVE' ) THEN
               CALL ARY1_DP2S( IDCB, STATUS )

*  Report context information if the conversion failed.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_REP( 'ARY1_DSFT_CVT',
     :            'Unable to perform implicit conversion from ' //
     :            '''PRIMITIVE'' to ''SIMPLE'' array storage form.',
     :            STATUS )
               END IF
            END IF

*  See if the array (which is now simple) has an ORIGIN component.
            CALL DAT_THERE( DCB_LOC( IDCB ), 'ORIGIN', THERE, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If there is no ORIGIN component, then create one.
               IF ( .NOT. THERE ) THEN
                  CALL DAT_NEW1I( DCB_LOC( IDCB ), 'ORIGIN',
     :                            DCB_NDIM( IDCB ), STATUS )
               END IF

*  Apply the pixel shifts to both sets of array bounds and to the
*  accumulated pixel shifts held in the DCB.
               DO 1 I = 1, MIN( NSHIFT, DCB_NDIM( IDCB ) )
                  DCB_LBND( I, IDCB ) = DCB_LBND( I, IDCB ) + SHIFT( I )
                  DCB_UBND( I, IDCB ) = DCB_UBND( I, IDCB ) + SHIFT( I )
                  DCB_SFT( I, IDCB ) = DCB_SFT( I, IDCB ) + SHIFT( I )
1              CONTINUE

*  Write new values to the ORIGIN component reflecting the array's new
*  origin position.
               CALL CMP_PUT1I( DCB_LOC( IDCB ), 'ORIGIN',
     :                         DCB_NDIM( IDCB ), DCB_LBND( 1, IDCB ),
     :                         STATUS )

*  Note if the DCB bounds information is correct.
               DCB_KBND( IDCB ) = STATUS .EQ. SAI__OK
            END IF

*  If the form information in the DCB was not recognised, then report
*  an error.
         ELSE
            STATUS = ARY__FATIN
            CALL MSG_SETC( 'BADFORM', DCB_FRM( IDCB ) )
            CALL ERR_REP( 'ARY1_DSFT_FORM',
     :      'Unsupported array form ''^BADFORM'' found in Data ' //
     :      'Control Block (internal programming error).', STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DSFT', STATUS )

      END
