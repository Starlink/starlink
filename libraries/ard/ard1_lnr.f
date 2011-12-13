      SUBROUTINE ARD1_LNR( RINDEX, TYPE, NDIM, LBND, UBND, MSKSIZ, NPAR,
     :                     PAR, C, FRM, B, LBEXTB, UBEXTB, LBINTB,
     :                     UBINTB, STATUS )
*+
*  Name:
*     ARD1_LNR

*  Purpose:
*     Set an array to hold a keyword region using linear mapping.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_LNR( RINDEX, TYPE, NDIM, LBND, UBND, MSKSIZ, NPAR,
*                    PAR, C, FRM, B, LBEXTB, UBEXTB, LBINTB, UBINTB,
*                    STATUS )

*  Description:
*     The array B is initialised by setting all values within the
*     supplied interior bounding box to the exterior value 0.
*     All points outside this box already hold exterior values.
*     Interior values are then assigned to the points specified by the
*     supplied parameters.

*  Arguments:
*     RINDEX = INTEGER (Given)
*        The value to use to represent interior points.
*     TYPE = INTEGER (Given)
*        Integer code for region type.
*     NDIM = INTEGER (Given)
*        The number of dimensions in the B array.
*     LBND( NDIM ) = INTEGER (Given)
*        The lower pixel index bounds of the B array.
*     UBND( NDIM ) = INTEGER (Given)
*        The upper pixel index bounds of the B array.
*     MSKSIZ = INTEGER (Given)
*        The total number of elements in the B array.
*     NPAR = INTEGER (Given)
*        The size of the PAR array.
*     PAR( NPAR ) = DOUBLE PRECISION (Given)
*        The region parameters, as supplied in the ARD expression.
*     C( * ) = DOUBLE PRECISION (Given)
*        The coefficients of the pixel->user mapping. There should be
*        NDIM*(NDIM+1) elements in the array. The mapping is:
*
*        U1 = C0 + C1*P1 + C2*P2 + ...  + Cn*Pn
*        U2 = Cn+1 + Cn+2*P1 + Cn+3*P2 + ...  + C2n+1*Pn
*        ...
*        UN = ...
*     FRM = INTEGER (Given)
*        Pointer to the user coord Frame.
*     B( MSKSIZ ) = INTEGER (Given and Returned)
*        The returned array.
*     LBEXTB( NDIM ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        exterior points in B. A value of VAL__MAXI for element 1 is
*        used to indicate an infinite box, and a value of VAL__MINI for
*        element 1 is used to indicate a zero sized box.
*     UBEXTB( NDIM ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        exterior points in B.
*     LBINTB( NDIM ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        interior points in B. A value of VAL__MAXI for element 1 is
*        used to indicate an infinite box, and a value of VAL__MINI for
*        element 1 is used to indicate a zero sized box.
*     UBINTB( NDIM ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        interior points in B.
*     LOADED = LOGICAL (Given and Returned)
*        Have the contents of the supplied mask already been loaded onto
*        the stack? Always returned .TRUE. if the keyword is an INPUT
*        keyword.
*     RINDEX = INTEGER (Returned)
*        The region index used for the keyword.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     1-MAR-1994 (DSB):
*        Original version.
*     26-JUN-2001 (DSB):
*        Modified for ARD version 2.0.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'ARD_CONST'        ! ARD_ private constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants

*  Arguments Given:
      INTEGER RINDEX
      INTEGER TYPE
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      INTEGER MSKSIZ
      INTEGER NPAR
      DOUBLE PRECISION PAR( NPAR )
      DOUBLE PRECISION C( * )
      INTEGER FRM

*  Arguments Given and Returned:
      INTEGER B( MSKSIZ )
      INTEGER LBEXTB( NDIM )
      INTEGER UBEXTB( NDIM )
      INTEGER LBINTB( NDIM )
      INTEGER UBINTB( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL
     :        BADPAR            ! Are the parameters missing?

      DOUBLE PRECISION
     :        D( ARD__MXDIM*(ARD__MXDIM + 1 ) ) ! User->pixel coeffs
      integer i

*.

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Invert the supplied Mapping to get the user->pixel mapping.
      CALL ARD1_INVRS( NDIM, C, D, STATUS )

*  Initialise a flag to indicate that the correct number of parameters
*  are available.
      BADPAR = .FALSE.

*  Call a separate suboutine to handle each type of region.
*  POINT and PIXEL keywords...
      IF( TYPE .EQ. ARD__POI .OR. TYPE .EQ. ARD__PIX ) THEN
         IF( NPAR .GT. 0 .AND. MOD( NPAR, NDIM ) .EQ. 0  ) THEN
            CALL ARD1_POI( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPAR,
     :                     D, PAR, B, LBEXTB, UBEXTB, LBINTB, UBINTB,
     :                     STATUS )
         ELSE
            BADPAR = .TRUE.
         END IF

*  LINE keywords...
      ELSE IF( TYPE .EQ. ARD__LIN ) THEN
         IF( NPAR .EQ. 2*NDIM ) THEN
            CALL ARD1_LIN( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPAR,
     :                     D, PAR, B, LBEXTB, UBEXTB, LBINTB, UBINTB,
     :                     STATUS )
         ELSE
            BADPAR = .TRUE.
         END IF

*  ROW keywords...
      ELSE IF( TYPE .EQ. ARD__ROW ) THEN
         IF( NPAR .GT. 0  ) THEN
            CALL ARD1_ROW( RINDEX, NDIM, D, LBND, UBND, MSKSIZ,
     :                     NPAR, PAR, B, LBEXTB, UBEXTB, LBINTB,
     :                     UBINTB, STATUS )
         ELSE
            BADPAR = .TRUE.
         END IF

*  COLUMN keywords...
      ELSE IF( TYPE .EQ. ARD__COL ) THEN
         IF( NPAR .GT. 0  ) THEN
            CALL ARD1_COL( RINDEX, NDIM, D, LBND, UBND, MSKSIZ,
     :                     NPAR, PAR, B, LBEXTB, UBEXTB, LBINTB,
     :                     UBINTB, STATUS )
         ELSE
            BADPAR = .TRUE.
         END IF

*  BOX keywords...
      ELSE IF( TYPE .EQ. ARD__BOX ) THEN
         IF( NPAR .GT. 0  ) THEN
            CALL ARD1_BOX( RINDEX, NDIM, FRM, LBND, UBND, MSKSIZ, NPAR,
     :                     D, PAR, B, LBEXTB, UBEXTB,
     :                     LBINTB, UBINTB, STATUS )
         ELSE
            BADPAR = .TRUE.
         END IF

*  RECT keywords...
      ELSE IF( TYPE .EQ. ARD__REC ) THEN
         IF( NPAR .GT. 0  ) THEN
            CALL ARD1_REC( RINDEX, NDIM, FRM, LBND, UBND, MSKSIZ, NPAR,
     :                     D, PAR, B, LBEXTB, UBEXTB,
     :                     LBINTB, UBINTB, STATUS )
         ELSE
            BADPAR = .TRUE.
         END IF

*  POLYGON keywords...
      ELSE IF( TYPE .EQ. ARD__POL ) THEN
         IF( NPAR .GT. 0  ) THEN
            CALL ARD1_POL( RINDEX, LBND( 1 ), UBND( 1 ), LBND( 2 ),
     :                     UBND( 2 ), NPAR, D, PAR, B,
     :                     LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )
         ELSE
            BADPAR = .TRUE.
         END IF

*  ROTBOX keywords...
      ELSE IF( TYPE .EQ. ARD__ROT ) THEN
         IF( NPAR .GT. 0  ) THEN
            CALL ARD1_ROT( FRM, RINDEX, LBND( 1 ), UBND( 1 ), LBND( 2 ),
     :                     UBND( 2 ), NPAR, D, PAR, B,
     :                     LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )
         ELSE
            BADPAR = .TRUE.
         END IF

*  CIRCLE keywords...
      ELSE IF( TYPE .EQ. ARD__CIR ) THEN
         IF( NPAR .GT. 0  ) THEN
            CALL ARD1_CIR( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPAR,
     :                     D, PAR, FRM, B, LBEXTB, UBEXTB,
     :                     LBINTB, UBINTB, STATUS )
         ELSE
            BADPAR = .TRUE.
         END IF

*  ELLIPSE keywords...
      ELSE IF( TYPE .EQ. ARD__ELL ) THEN
         IF( NPAR .GT. 0  ) THEN
            CALL ARD1_ELL( FRM, RINDEX, LBND( 1 ), UBND( 1 ), LBND( 2 ),
     :                     UBND( 2 ), NPAR, D, PAR, B,
     :                     LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )
         ELSE
            BADPAR = .TRUE.
         END IF

*  FRAME keywords...
      ELSE IF( TYPE .EQ. ARD__FRA ) THEN
         IF( NPAR .GT. 0  ) THEN
            CALL ARD1_FRA( RINDEX, LBND( 1 ), UBND( 1 ), LBND( 2 ),
     :                     UBND( 2 ), D, PAR, B, LBEXTB,
     :                     UBEXTB, LBINTB, UBINTB, STATUS )
         ELSE
            BADPAR = .TRUE.
         END IF

*  Report an error and abort for any other keyword.
      ELSE
         STATUS = ARD__INTER
         CALL MSG_SETI( 'TYPE', TYPE )
         CALL ERR_REP( 'ARD1_LNR_ERR5', 'Illegal keyword identifier '//
     :                 ' (^TYPE) encountered in routine ARD1_LNR '//
     :                 '(programming error).', STATUS )
      END IF

*  Report an error if no parameters have been supplied.
      IF( BADPAR .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = ARD__INTER
         CALL MSG_SETI( 'NP', NPAR )
         CALL MSG_SETI( 'TY', TYPE )
         CALL ERR_REP( 'ARD1_LNR_ERR6', 'Zero parameters found for '//
     :                 'region type ^TY in ARD1_LNR (programming '//
     :                 'error).', STATUS )
      END IF

      END
