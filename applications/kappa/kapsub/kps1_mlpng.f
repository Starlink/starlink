      SUBROUTINE KPS1_MLPNG( ABSDIM, NDISP, LBND, LINDX, MAP, ABSAXS,
     :                       CFRM, IAXIS, IPNOM, LUTMAP, XL, XR,
     :                       STATUS )
*+
*  Name:
*     KPS1_MLPNG

*  Purpose:
*     Get the horizontal position of each sample to be displayed by
*     MLINPLOT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MLPNG( ABSDIM, NDISP, LBND, LINDX, MAP, ABSAXS, CFRM,
*                      IAXIS, IPNOM, LUTMAP, XL, XR, STATUS )

*  Description:
*     This routine finds the horizontal position of each sample to be
*     displayed by MLINPLOT. These positions are chosen so that a
*     vertical line on the screen corresponds to a constant value on the
*     chosen horizontal axis. The horizontal positions are returned in terms
*     of a "nominal GRID Frame". This is a 1D Frame which is later mapped
*     linearly onto the horizontal dimension of the screen. It corresponds
*     to the GRID axis used as the abscissa  for the middle data line (i.e.
*     the line with index (NDISP + 1)/2 ).

*  Arguments:
*     ABSDIM = INTEGER (Given)
*        The number of grid elements along the abscissa axis of the data
*        array to be displayed. The abscissa axis is specified by argument
*        ABSAXS.
*     NDISP = INTEGER (Given)
*        The number of lines of data to be displayed.
*     LBND = INTEGER (Given)
*        The lower pixel bound on the ordinate axis of the data array to be
*        displayed. The ordinate axis is implied by argument ABSAXS.
*     LINDX( NDISP ) = INTEGER (Given)
*        The pixel indices of the lines of data to be displayed. These
*        integer indices refer to the ordinate axis implied by argument
*        ABSAXS, and have a lower bound given by argument LBND.
*     MAP = INTEGER (Given)
*        A pointer to the Mapping from Base (GRID) to Current Frame in
*        the DNFs WCS FrameSet.
*     ABSAXS = INTEGER (Given)
*        The index of the abscissa (horizontal) grid axis. This should be 1
*        or 2. The other axis is the ordinate (vertical) axis.
*     CFRM = INTEGER (Given)
*        An AST pointer to the Current Frame of the WCS FrameSet.
*     IAXIS = INTEGER (Given)
*        The index within the Frame given by CFRM of the axis which is to
*        be used to annotate the horizontal axis.
*     IPNOM = INTEGER (Returned)
*        A pointer to a 2D array of type DOUBLE PRECISION with bounds
*        (ABSDIM, NDISP). Each row corresponds to the ABSDIM data
*        values in the corresponding displayed line (1 to NDISP). Each
*        element in a row is the nominal GRID value at the centre of the
*        data value. Returned equal to zero if an error occurrs.
*     LUTMAP = INTEGER (Returned)
*        The 1D Mapping from nominal GRID value, to the value used to
*        annotate the horizontal axis. This Mapping will have a defined
*        and usable inverse transformation.
*     XL = DOUBLE PRECISION (Returned)
*        The annotated axis value at the left end of the horizontal axis.
*     XR = DOUBLE PRECISION (Returned)
*        The annotated axis value at the right end of the horizontal axis.
*     STATUS = INTEGER (Given and Returned)
*        Global status value.

*  Environment Parameters:
*     XLEFT = LITERAL (Read)
*        The axis value to place at the left hand end of the horizontal
*        axis. The dynamic default is the value for the first element in the
*        data being displayed. The value supplied may be greater than or
*        less than the value supplied for XRIGHT. A formatted value for the
*        quantity specified by parameter USEAXIS should be supplied. []
*     XRIGHT = LITERAL (Read)
*        The axis value to place at the right hand end of the horizontal
*        axis. The dynamic default is the value for the last element in the
*        data being displayed. The value supplied may be greater than or
*        less than the value supplied for XLEFT. A formatted value for the
*        quantity specified by parameter USEAXIS should be supplied. []

*  Copyright:
*     Copyright (C) 1998, 2000, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     6-AUG-1998 (DSB):
*        Original version.
*     15-FEB-2000 (DSB):
*        Modified to take account of new KPG1_GTAXV argument list.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER ABSDIM
      INTEGER NDISP
      INTEGER LBND
      INTEGER LINDX( NDISP )
      INTEGER MAP
      INTEGER ABSAXS
      INTEGER CFRM
      INTEGER IAXIS

*  Arguments Returned:
      INTEGER IPNOM
      INTEGER LUTMAP
      DOUBLE PRECISION XL
      DOUBLE PRECISION XR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER ATTR*10          ! AST attribute name
      DOUBLE PRECISION A( 2 )    ! 1D annotated axis positions
      DOUBLE PRECISION C( 2, NDF__MXDIM )! Current Frame positions
      DOUBLE PRECISION G( 2, 2 ) ! 2D Grid positions
      DOUBLE PRECISION G1( 2 )   ! 1D Grid positions
      DOUBLE PRECISION GVAL      ! GRID value for current line
      DOUBLE PRECISION MAXL      ! Max. of annot. axis values at left ends
      DOUBLE PRECISION MAXR      ! Max. of annot. axis values at right  ends
      DOUBLE PRECISION MINL      ! Min. of annot. axis values at left ends
      DOUBLE PRECISION MINR      ! Min. of annot. axis values at right ends
      DOUBLE PRECISION POS( NDF__MXDIM ) ! Normalized Current Frame position
      DOUBLE PRECISION RANGE     ! Range of nominal GRID values
      INTEGER I                  ! Loop count
      INTEGER IAT                ! No. of characters in string
      INTEGER INDX               ! Pixel index of next display line
      INTEGER IPAXL              ! Pointer to annotated axis value
      INTEGER IPG                ! Pointer to nominal GRID values
      INTEGER IPGL               ! Pointer to actual GRID values
      INTEGER IPLUT              ! Pointer to annotated axis value
      INTEGER IPWORK             ! Pointer to work space
      INTEGER J                  ! Loop count
      INTEGER K                  ! Loop count
      INTEGER LBNDL              ! Lower nominal GRID bound of LUT
      INTEGER LUTLEN             ! Nominal GRID dimenson of LUT
      INTEGER MAP2               ! Mapping from nom. GRID to annot. axis
      INTEGER MAP3               ! Mapping from GRID to annot. axis
      INTEGER NAX                ! No. of axes in Current WCS Frame
      INTEGER NREPHI             ! No. of values above high threshold
      INTEGER NREPLO             ! No. of values below low threshold
      INTEGER NVAL               ! No. of axis values supplied
      INTEGER ORDAXS             ! Index of ordinate pixel axis
      INTEGER OUTPRM( 2 )        ! O/p axes permutation array
      INTEGER PMAP               ! Pointer to a PermMap
      INTEGER UBNDL              ! Upper nominal GRID bound of LUT
*.

*  Initialise.
      IPNOM = 0

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Indicate no dynamic memory has yet been allocated.
      IPG = 0
      IPLUT = 0
      IPGL = 0
      IPAXL = 0
      IPWORK = 0

*  Store the index of the ordinate axis.
      ORDAXS = 3 - ABSAXS

*  Store the number of axes in the Current WCS Frame.
      NAX = AST_GETI( CFRM, 'NAXES', STATUS )

*  Find the bounds of the horizontal axis.
*  =======================================
*  This application requires the annotated axis value to increase or
*  decrease monotonically along each of the displayed lines. Therefore,
*  extreme annotated axis values must be reached at the start or end of
*  the lines. To find the default annotated axis range, we map the end
*  points of each line to the required axis, and find the extreme values.
*  Initialise the extreme values, and then loop round each line.
      MAXL = VAL__MIND
      MINL = VAL__MAXD
      MAXR = VAL__MIND
      MINR = VAL__MAXD

      DO I = 1, NDISP

*  Store the GRID co-ordinates at the two ends of this line.
         G( 1, ABSAXS ) = 1.0D0
         G( 1, ORDAXS ) = DBLE( LINDX( I ) - LBND + 1 )

         G( 2, ABSAXS ) = DBLE( ABSDIM )
         G( 2, ORDAXS ) = G( 1, ORDAXS )

*  Get the corresponding values in the WCS Current Frame using the supplied
*  Mapping.
         CALL AST_TRANN( MAP, 2, 2, 2, G, .TRUE., NAX, 2, C, STATUS )

*  Normalize each position.
         DO K = 1, 2
            DO J = 1, NAX
               POS( J ) = C( K, J )
            END DO

            CALL AST_NORM( CFRM, POS, STATUS )

            DO J = 1, NAX
               C( K, J ) = POS( J )
            END DO
         END DO

*  Find the min and max values on the required axis at the start and end of
*  any line.
         MAXL = MAX( MAXL, C( 1, IAXIS ) )
         MINL = MIN( MINL, C( 1, IAXIS ) )

         MAXR = MAX( MAXR, C( 2, IAXIS ) )
         MINR = MIN( MINR, C( 2, IAXIS ) )

      END DO

*  Find the default values for XLEFT and XRIGHT.
      IF( MINL .LT. MINR ) THEN
         XL = MINL
         XR = MAXR
      ELSE
         XL = MAXL
         XR = MINR
      END IF

*  Get the annotated axis values for the left and right hand ends of the
*  horizontal axis, using the above values as dynamic defaults.
      CALL KPG1_GTAXV( 'XLEFT', 1, .TRUE., CFRM, IAXIS, XL, NVAL,
     :                 STATUS )
      CALL KPG1_GTAXV( 'XRIGHT', 1, .TRUE., CFRM, IAXIS, XR, NVAL,
     :                 STATUS )

*  Find a 1D Mapping from "nominal GRID" value to the annotated axis values.
*  The nominal GRID axis is mapped linearly onto the horizontal axis of the
*  screen.
*  =========================================================================
*  Find the ordinate GRID value at the centre of the middle line. This line
*  defines the 1D "nominal GRID" Frame.
      GVAL = DBLE( LINDX( ( NDISP + 1 ) / 2 ) - LBND + 1 )

*  Modify the supplied Mapping so that it has a single input (i.e. GRID) axis
*  corresponding to the abscissa. The other GRID axis value is fixed at the
*  value corresponding to the middle line chosen above. First created the
*  required PermMap.
      OUTPRM( ABSAXS ) = 1
      OUTPRM( ORDAXS ) = -1
      PMAP = AST_PERMMAP( 1, ABSAXS, 2, OUTPRM, GVAL, ' ', STATUS )

*  Now add the PermMap in front of the supplied Mapping, and annul the
*  PermMap.
      MAP2 = AST_CMPMAP( PMAP, MAP, .TRUE., ' ', STATUS )
      CALL AST_ANNUL( PMAP, STATUS )

*  The PermMaps present in the above Mapping may well mean that the inverse
*  transformation from horizontal axis value to nominal GRID value is
*  either not defined or gives AST_BAD (or otherwise inappropriate) values.
*  However, we need an inverse transformation, and so we re-implement the
*  above as a look-up table, giving horizontal axis values at a set of
*  evenly spaced nominal GRID values. This table can be found using only
*  the forward transformation of the above Mapping, and is implemented as
*  an AST LutMap. This LutMap will have an inverse Transformation so
*  long as the look-up table is monotonic and has no missing (i.e. bad)
*  values. One problem is that the LutMap is only defined over the range
*  of nominal GRID values covered by the table. So we need to ensure that
*  the table covers at least the entire range of nominal GRID values which
*  needs to be displayed. To do this we find an interval on the nominal
*  GRID axis which, when mapped into the annotated axis, encompasses the
*  required axis range given by XLEFT and XRIGHT.

*  Set the initial GRID limits to the bounds of the supplied array.
      G1( 1 ) = 1.0D0
      G1( 2 ) = DBLE( ABSDIM )

*  We make 10 checks, doubling the range covered by the GRID limits
*  each time.
      DO I = 1, 10

*  Transform the current GRID limits into the required annotated axis,
*  normalizing them in the process.
         CALL KPS1_MLPMP( NAX, CFRM, MAP2, 2, G1, .TRUE., IAXIS, A,
     :                    C, STATUS )

*  Check the annotated axis values are both good.
         IF( A( 1 ) .NE. AST__BAD .AND. A( 2 ) .NE. AST__BAD ) THEN

*  If both left and right X axis limits fall between the annotated axis
*  values found above, leave the loop with the current GRID limits.
            IF( A( 1 ) .GT. A( 2 ) ) THEN
               IF( XL .GE. A( 2 ) .AND. XL .LE. A( 1 ) .AND.
     :             XR .GE. A( 2 ) .AND. XR .LE. A( 1 ) ) GO TO 10

            ELSE
               IF( XL .GE. A( 1 ) .AND. XL .LE. A( 2 ) .AND.
     :             XR .GE. A( 1 ) .AND. XR .LE. A( 2 ) ) GO TO 10

            END IF

         END IF

*  Double the range of nomimal GRID value.
         RANGE = G1( 2 ) - G1( 1 )
         G1( 1 ) = G1( 1 ) - 0.5*RANGE
         G1( 2 ) = G1( 2 ) + 0.5*RANGE

      END DO

*  Arrive here if we were unable to find GRID limits which encompass the
*  required left and right axis values. Report an error and abort.
      IF( STATUS .EQ. SAI__OK ) THEN

         CALL MSG_SETI( 'AX', ABSAXS )

         ATTR = 'LABEL('
         IAT = 6
         CALL CHR_PUTI( IAXIS, ATTR, IAT )
         CALL CHR_APPND( ')', ATTR, IAT )

         CALL MSG_SETC( 'LAB', AST_GETC( CFRM, ATTR( : IAT ), STATUS ) )

         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_MLPNG_ERR1', 'Cannot determine the range'//
     :                 ' for the horizontal axis (^LAB) specified by '//
     :                 'parameter USEAXIS. A different value for '//
     :                 'USEAXIS may correct this.', STATUS )
         GO TO 999
      END IF

*  Arrive here if we were able to find GRID limits which encompass the
*  required left and right axis values.
 10   CONTINUE

*  We now find a LutMap which is equivalent to the MAP2 Mapping. That is, a
*  Look-up-table which gives the annotated axis value at the centre of each
*  element of the middle line. First allocate memory to hold the look up
*  table, and the corresponding nominal GRID values. The range of nominal
*  GRID value covered by the LUT is given by the bounds found above.
      LBNDL = NINT( G1( 1 ) )
      UBNDL = NINT( G1( 2 ) )
      LUTLEN = UBNDL - LBNDL + 1

      CALL PSX_CALLOC( LUTLEN, '_DOUBLE', IPLUT, STATUS )
      CALL PSX_CALLOC( LUTLEN, '_DOUBLE', IPG, STATUS )
      CALL PSX_CALLOC( NAX*LUTLEN, '_DOUBLE', IPWORK, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find nominal GRID positions evenly spaced along the middle line. The
*  positions are at the centre of each grid cell covered by the LUT.
      A( 1 ) = DBLE( LBNDL )
      A( 2 ) = DBLE( UBNDL )
      CALL KPG1_ASSMP( AST__NULL, 2, 1, 2, A, .FALSE., LUTLEN,
     :                 1.0D0, %VAL( CNF_PVAL( IPG ) ), STATUS )

*  Transform these GRID positions into values for the required annotated
*  axis.
      CALL KPS1_MLPMP( NAX, CFRM, MAP2, LUTLEN, %VAL( CNF_PVAL( IPG ) ),
     :                 .TRUE.,
     :                 IAXIS, %VAL( CNF_PVAL( IPLUT ) ),
     :                 %VAL( CNF_PVAL( IPWORK ) ), STATUS )

*  Create the LutMap.
      LUTMAP = AST_LUTMAP( LUTLEN, %VAL( CNF_PVAL( IPLUT ) ),
     :                     1.0D0, 1.0D0, ' ',
     :                     STATUS )

*  Report an error and abort if the inverse transformation is not defined
*  (i.e. if the annotated axis does not increase or decrease monotonically
*  along the middle line).
      IF( .NOT. AST_GETL( LUTMAP, 'TranInverse', STATUS ) .AND.
     :    STATUS .EQ. SAI__OK ) THEN

         ATTR = 'LABEL('
         IAT = 6
         CALL CHR_PUTI( IAXIS, ATTR, IAT )
         CALL CHR_APPND( ')', ATTR, IAT )

         CALL MSG_SETC( 'LAB', AST_GETC( CFRM, ATTR( : IAT ), STATUS ) )

         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_MLPNG_ERR1', 'The horizontal axis value '//
     :                 '(^LAB) does not increase or decrease '//
     :                 'monotonically along the data array, and '//
     :                 'so cannot be used.', STATUS )

         GO TO 999
      END IF

*  Allocate memory to hold the GRID value and annotated axis value at the
*  centre of each pixel in each line of displayed data.
      CALL PSX_CALLOC( ABSDIM, '_DOUBLE', IPGL, STATUS )
      CALL PSX_CALLOC( ABSDIM, '_DOUBLE', IPAXL, STATUS )

*  Allocate memory to hold the returned nominal GRID values.
      CALL PSX_CALLOC( ABSDIM*NDISP, '_DOUBLE', IPNOM, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find GRID positions evenly spaced along each line. The positions are
*  at the centre of each grid cell.
      A( 1 ) = 1.0D0
      A( 2 ) = DBLE( ABSDIM )
      CALL KPG1_ASSMP( AST__NULL, 2, 1, 2, A, .FALSE., ABSDIM,
     :                 1.0D0, %VAL( CNF_PVAL( IPGL ) ), STATUS )

*  Loop round each line.
      DO I = 1, NDISP
         INDX = LINDX( I ) - LBND + 1

*  Find the ordinate GRID value at the centre of this line.
         GVAL = DBLE( INDX )

*  Get the 1-D Mapping from the abscissa GRID value for this line to the
*  selected Current Frame axis. To do this we modify the supplied Mapping
*  so that it has a single input (i.e. GRID) axis corresponding to the
*  abscissa. The other GRID axis value is fixed at the value corresponding
*  to the current line. First created the required PermMap.
         OUTPRM( ABSAXS ) = 1
         OUTPRM( 3 - ABSAXS ) = -1
         PMAP = AST_PERMMAP( 1, ABSAXS, 2, OUTPRM, GVAL, ' ', STATUS )

*  Now add the PermMap in front of the supplied Mapping. This gives the
*  Mapping from 1D abscissa GRID value for this line, to nD Current Frame
*  position.
         MAP3 = AST_CMPMAP( PMAP, MAP, .TRUE., ' ', STATUS )

*  Use this Mapping to get the annotated axis value at the centre of each
*  pixel in this line.
         CALL KPS1_MLPMP( NAX, CFRM, MAP3, ABSDIM,
     :                    %VAL( CNF_PVAL( IPGL ) ), .TRUE.,
     :                    IAXIS, %VAL( CNF_PVAL( IPAXL ) ),
     :                    %VAL( CNF_PVAL( IPWORK ) ), STATUS )

*  Set bad any values which are outside the required range of the
*  annotated axis given by XLEFT and XRIGHT.
         CALL KPG1_THRSD( .TRUE., ABSDIM, %VAL( CNF_PVAL( IPAXL ) ),
     :                    MIN( XL, XR ),
     :                    MAX( XR, XL ), AST__BAD, AST__BAD,
     :                    %VAL( CNF_PVAL( IPAXL ) ),
     :                    NREPLO, NREPHI, STATUS )

*  Now use the inverse Transformation of the LutMap created earlier to
*  transform the annotated axis values into the nominal GRID value at
*  the centre of each pixel in this line.
         CALL AST_TRAN1( LUTMAP, ABSDIM, %VAL( CNF_PVAL( IPAXL ) ),
     :                   .FALSE.,
     :   %VAL( CNF_PVAL( IPNOM ) + ( I - 1 )*ABSDIM*VAL__NBD ),
     :                   STATUS )

*  Annul the Mappings related to this line.
         CALL AST_ANNUL( PMAP, STATUS )
         CALL AST_ANNUL( MAP3, STATUS )

      END DO

*  Tidy up.
 999  CONTINUE

*  Free memory
      IF( IPLUT .NE. 0 ) CALL PSX_FREE( IPLUT, STATUS )
      IF( IPG .NE. 0 ) CALL PSX_FREE( IPG, STATUS )
      IF( IPAXL .NE. 0 ) CALL PSX_FREE( IPAXL, STATUS )
      IF( IPGL .NE. 0 ) CALL PSX_FREE( IPGL, STATUS )
      IF( IPWORK .NE. 0 ) CALL PSX_FREE( IPWORK, STATUS )

*  Export the returned LutMap to the enclosing AST context.
      CALL AST_EXPORT( LUTMAP, STATUS )

*  If an error has occurred, free the returned memory.
      IF( STATUS .NE. SAI__OK .AND. IPNOM .NE. 0 ) THEN
         CALL PSX_FREE( IPNOM, STATUS )
         IPNOM = 0
      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
