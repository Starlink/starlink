      SUBROUTINE EXCLUDEBAD( STATUS )
*+
*  Name:
*     EXCLUDEBAD

*  Purpose:
*     Exclude bad rows or columns from a 2D NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL EXCLUDEBAD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application produces a copy of a 2D NDF, but excludes any
*     rows that contain too many bad Data values. Rows with higher pixel
*     indices are shuffled down to fill the gaps left by the omission of
*     bad rows. Thus if any bad rows are found, the output NDF will have
*     fewer rows than the input NDF, but the order of the remaining rows
*     will be unchanged. The number of good pixels required in a row for
*     the row to be retained is specified by parameter WLIM.
*
*     Bad columns may be omitted instead of bad rows (see parameter ROWS).

*  Usage:
*     excludebad in out [rows] [wlim]

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input 2D NDF.
*     OUT = NDF (Write)
*        The output NDF.
*     ROWS = _LOGICAL (Read)
*        If TRUE, bad rows are excluded from the output NDF. If FALSE bad
*        columns are excluded. [TRUE]
*     WLIM = _REAL (Read)
*        The minimum fraction of pixel which must be good in order for
*        a row to be retained. A value of 1.0 results in rows being
*        excluded if they contain one or more bad values. A value of
*        0.0 results in rows being excluded only if they contain no good
*        values. [0.0]

*  Notes:
*     -  The lower pixel bounds of the output will be the same as trhose
*     of the input, but the upper pixel bounds will be different if any
*     bad rows or columns are excluded.

*  Examples:
*     excludebad ifuframe goodonly false
*        Columns within NDF ifuframe that contain any good data are
*        copied to NDF goodonly.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of the
*     input NDF and propagates all extensions.

*  Copyright:
*     Copyright (C) 2014 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     24-APR-2014 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PRM_PAR'          ! VAL_ public constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NDIM               ! Dimensionality required
      PARAMETER ( NDIM = 2 )

*  Local Variables:
      CHARACTER ACOMP( 3 )*8     ! Names of axis array components
      CHARACTER TYPE*( NDF__SZTYP ) ! Array component numeric type
      INTEGER AXIS               ! Index of collapsed axis in full NDF
      INTEGER AXLEN              ! Length of collapsed axis in pixels
      INTEGER I                  ! Loop count
      INTEGER INDF1              ! Identifier for input NDF
      INTEGER INDF2              ! Identifier for output NDF
      INTEGER IPA( 3 )           ! Pointer to mapped Axis arrays
      INTEGER IPD                ! Pointer to mapped Data array
      INTEGER IPQ                ! Pointer to mapped Quality array
      INTEGER IPV                ! Pointer to mapped Variance array
      INTEGER IPW1               ! Pointer to first work array
      INTEGER IPW2               ! Pointer to second work array
      INTEGER IWCS               ! Identifier for WCS FrameSet
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of all pixel axes
      INTEGER MAP                ! 1-D mapping for collapsed GRID axis
      INTEGER MAP1               ! N-D mapping for lower GRID axes
      INTEGER MAP2               ! N-D mapping for all GRID axes
      INTEGER NDIMS              ! Total number of pixel axes
      INTEGER NEL                ! No. of elements in mapped array
      INTEGER NEX                ! No. of rows excluded
      INTEGER SDIM( NDIM )       ! Indicies of significant pixel axes
      INTEGER SLBND( NDIM )      ! Lower bounds of significant axes
      INTEGER SUBND( NDIM )      ! Upper bounds of significant axes
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of all pixel axes
      LOGICAL AXI( 3 )           ! Are Axis array components present?
      LOGICAL QUA                ! Is Quality component present?
      LOGICAL ROWS               ! Exclude bad rows?
      LOGICAL VAR                ! Is Variance component present?
      REAL WLIM                  ! Min fraction of good pixels

      DATA ACOMP /'Centre', 'Width', 'Variance' /
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin AST and NDF contexts.
      CALL AST_BEGIN( STATUS )
      CALL NDF_BEGIN

*  Obtain the input NDF, checking it has no more than 2 significant pixel
*  axes. Get the indices and bounds of the significant pixel axes.
      CALL KPG1_GTNDF( 'IN', NDIM, .FALSE., 'Read', INDF1, SDIM, SLBND,
     :                 SUBND, STATUS )

*  See if rows or columns are to be excluded.
      CALL PAR_GET0L( 'ROWS', ROWS, STATUS )

*  Get the minimum fraction of good pixels needed in a row for the row to
*  be retained in the output.
      CALL PAR_GDR0R( 'WLIM', 0.0, 0.0, 1.0, .FALSE., WLIM, STATUS )

*  Note the index of the axis within the full NDF that is to be
*  collapsed, and also its length.
      IF( ROWS ) THEN
         AXIS = SDIM( 2 )
         AXLEN = SUBND( 2 ) - SLBND( 2 ) + 1
      ELSE
         AXIS = SDIM( 1 )
         AXLEN = SUBND( 1 ) - SLBND( 1 ) + 1
      END IF

*  Create the output with the same shape and size as the input. Its shape
*  will be changed later. Copy everything.
      CALL LPG_PROP( INDF1, 'DATA,QUALITY,VARIANCE,WCS,AXIS,UNITS',
     :               'OUT', INDF2, STATUS )

*  Map the output NDF DATA array.
      CALL NDF_TYPE( INDF2, 'Data', TYPE, STATUS )
      CALL NDF_MAP( INDF2, 'Data', TYPE, 'WRITE', IPD, NEL, STATUS )

*  If present, map the variance component.
      CALL NDF_STATE( INDF2, 'Variance', VAR, STATUS )
      IF( VAR ) THEN
         CALL NDF_MAP( INDF2, 'Variance', TYPE, 'WRITE', IPV, NEL,
     :                 STATUS )
      ELSE
         IPV = IPD
      END IF

*  If present, map the quality component.
      CALL NDF_STATE( INDF2, 'Quality', QUA, STATUS )
      IF( QUA ) THEN
         CALL NDF_MAP( INDF2, 'Quality', '_UBYTE', 'WRITE', IPQ, NEL,
     :                 STATUS )
      ELSE
         IPQ = IPD
      END IF

*  If present, map the AXIS centre, width and variance arrays.
      DO I = 1, 3
         CALL NDF_ASTAT( INDF2, ACOMP( I ), AXIS, AXI( I ), STATUS )
         IF( AXI( I ) ) THEN
            CALL NDF_MAP( INDF2, 'Quality', '_DOUBLE', 'WRITE',
     :                    IPA( I ), NEL, STATUS )
         ELSE
            IPA( I ) = IPD
         END IF
      END DO

*  Allocate work arrays.
      CALL PSX_MALLOC( AXLEN*VAL__NBD, IPW1, STATUS )
      CALL PSX_MALLOC( AXLEN*VAL__NBD, IPW2, STATUS )

*  Select the appropriate routine to copy the good rows or columns.
      IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL KPS1_EXBI( WLIM, ROWS, VAR, QUA, AXI, SLBND, SUBND,
     :                   %VAL( CNF_PVAL( IPD ) ),
     :                   %VAL( CNF_PVAL( IPV ) ),
     :                   %VAL( CNF_PVAL( IPQ ) ),
     :                   %VAL( CNF_PVAL( IPA( 1 ) ) ),
     :                   %VAL( CNF_PVAL( IPA( 2 ) ) ),
     :                   %VAL( CNF_PVAL( IPA( 3 ) ) ),
     :                   %VAL( CNF_PVAL( IPW1 ) ),
     :                   %VAL( CNF_PVAL( IPW2 ) ),
     :                   MAP, NEX, STATUS )

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL KPS1_EXBR( WLIM, ROWS, VAR, QUA, AXI, SLBND, SUBND,
     :                   %VAL( CNF_PVAL( IPD ) ),
     :                   %VAL( CNF_PVAL( IPV ) ),
     :                   %VAL( CNF_PVAL( IPQ ) ),
     :                   %VAL( CNF_PVAL( IPA( 1 ) ) ),
     :                   %VAL( CNF_PVAL( IPA( 2 ) ) ),
     :                   %VAL( CNF_PVAL( IPA( 3 ) ) ),
     :                   %VAL( CNF_PVAL( IPW1 ) ),
     :                   %VAL( CNF_PVAL( IPW2 ) ),
     :                   MAP, NEX, STATUS )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL KPS1_EXBD( WLIM, ROWS, VAR, QUA, AXI, SLBND, SUBND,
     :                   %VAL( CNF_PVAL( IPD ) ),
     :                   %VAL( CNF_PVAL( IPV ) ),
     :                   %VAL( CNF_PVAL( IPQ ) ),
     :                   %VAL( CNF_PVAL( IPA( 1 ) ) ),
     :                   %VAL( CNF_PVAL( IPA( 2 ) ) ),
     :                   %VAL( CNF_PVAL( IPA( 3 ) ) ),
     :                   %VAL( CNF_PVAL( IPW1 ) ),
     :                   %VAL( CNF_PVAL( IPW2 ) ),
     :                   MAP, NEX, STATUS )

      ELSE IF ( TYPE .EQ. '_INT64' ) THEN
         CALL KPS1_EXBK( WLIM, ROWS, VAR, QUA, AXI, SLBND, SUBND,
     :                   %VAL( CNF_PVAL( IPD ) ),
     :                   %VAL( CNF_PVAL( IPV ) ),
     :                   %VAL( CNF_PVAL( IPQ ) ),
     :                   %VAL( CNF_PVAL( IPA( 1 ) ) ),
     :                   %VAL( CNF_PVAL( IPA( 2 ) ) ),
     :                   %VAL( CNF_PVAL( IPA( 3 ) ) ),
     :                   %VAL( CNF_PVAL( IPW1 ) ),
     :                   %VAL( CNF_PVAL( IPW2 ) ),
     :                   MAP, NEX, STATUS )

      ELSE IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL KPS1_EXBB( WLIM, ROWS, VAR, QUA, AXI, SLBND, SUBND,
     :                   %VAL( CNF_PVAL( IPD ) ),
     :                   %VAL( CNF_PVAL( IPV ) ),
     :                   %VAL( CNF_PVAL( IPQ ) ),
     :                   %VAL( CNF_PVAL( IPA( 1 ) ) ),
     :                   %VAL( CNF_PVAL( IPA( 2 ) ) ),
     :                   %VAL( CNF_PVAL( IPA( 3 ) ) ),
     :                   %VAL( CNF_PVAL( IPW1 ) ),
     :                   %VAL( CNF_PVAL( IPW2 ) ),
     :                   MAP, NEX, STATUS )

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL KPS1_EXBUB( WLIM, ROWS, VAR, QUA, AXI, SLBND, SUBND,
     :                   %VAL( CNF_PVAL( IPD ) ),
     :                   %VAL( CNF_PVAL( IPV ) ),
     :                   %VAL( CNF_PVAL( IPQ ) ),
     :                   %VAL( CNF_PVAL( IPA( 1 ) ) ),
     :                   %VAL( CNF_PVAL( IPA( 2 ) ) ),
     :                   %VAL( CNF_PVAL( IPA( 3 ) ) ),
     :                   %VAL( CNF_PVAL( IPW1 ) ),
     :                   %VAL( CNF_PVAL( IPW2 ) ),
     :                   MAP, NEX, STATUS )

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL KPS1_EXBW( WLIM, ROWS, VAR, QUA, AXI, SLBND, SUBND,
     :                   %VAL( CNF_PVAL( IPD ) ),
     :                   %VAL( CNF_PVAL( IPV ) ),
     :                   %VAL( CNF_PVAL( IPQ ) ),
     :                   %VAL( CNF_PVAL( IPA( 1 ) ) ),
     :                   %VAL( CNF_PVAL( IPA( 2 ) ) ),
     :                   %VAL( CNF_PVAL( IPA( 3 ) ) ),
     :                   %VAL( CNF_PVAL( IPW1 ) ),
     :                   %VAL( CNF_PVAL( IPW2 ) ),
     :                   MAP, NEX, STATUS )

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL KPS1_EXBUW( WLIM, ROWS, VAR, QUA, AXI, SLBND, SUBND,
     :                   %VAL( CNF_PVAL( IPD ) ),
     :                   %VAL( CNF_PVAL( IPV ) ),
     :                   %VAL( CNF_PVAL( IPQ ) ),
     :                   %VAL( CNF_PVAL( IPA( 1 ) ) ),
     :                   %VAL( CNF_PVAL( IPA( 2 ) ) ),
     :                   %VAL( CNF_PVAL( IPA( 3 ) ) ),
     :                   %VAL( CNF_PVAL( IPW1 ) ),
     :                   %VAL( CNF_PVAL( IPW2 ) ),
     :                   MAP, NEX, STATUS )

      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TY', TYPE )
         CALL ERR_REP( ' ', 'Unsupported data type ''^TY'' '//
     :                 '(programming error).', STATUS )
      END IF

*  Unmap everything so we can change the bounds of the output NDF.
      CALL NDF_UNMAP( INDF2, '*', STATUS )
      CALL NDF_AUNMP( INDF2, '*', AXIS, STATUS )

*  Report how many rows were excluded.
      IF( NEX .EQ. 1 ) THEN
         IF( ROWS ) THEN
            CALL MSG_OUT( ' ', '   One row was excluded.', STATUS )
         ELSE
            CALL MSG_OUT( ' ', '   One column was excluded.', STATUS )
         END IF
      ELSE
         IF( ROWS ) THEN
            CALL MSG_SETC( 'W', 'rows' )
         ELSE
            CALL MSG_SETC( 'W', 'columns' )
         END IF

         IF( NEX .EQ. 0 ) THEN
            CALL MSG_SETC( 'N', 'No' )
         ELSE
            CALL MSG_SETI( 'N', NEX )
         END IF

         CALL MSG_OUT( ' ', '   ^N ^W were excluded.', STATUS )
      END IF

*  Do nothing more if no bad rows or columns were found.
      IF( MAP .NE. AST__NULL ) THEN

*  Reshape the output NDF.
         CALL NDF_BOUND( INDF1, NDF__MXDIM, LBND, UBND, NDIMS, STATUS )
         LBND( SDIM( 1 ) ) = SLBND( 1 )
         UBND( SDIM( 1 ) ) = SUBND( 1 )
         LBND( SDIM( 2 ) ) = SLBND( 2 )
         UBND( SDIM( 2 ) ) = SUBND( 2 )
         CALL NDF_SBND( NDIMS, LBND, UBND, INDF2, STATUS )

*  Create a Mapping that translated N-dimensional GRID coords in the
*  input to N-dimensional GRID coords in the output. This is the 1D
*  mapping returned by KPS1_EXBx, in parallel with a pair of UnitMaps
*  that  map the unchanged pixel axes.
         IF( AXIS .GT. 1 ) THEN
            MAP1 = AST_CMPMAP( AST_UNITMAP( AXIS - 1, ' ', STATUS ),
     :                         MAP, .FALSE., ' ', STATUS )
         ELSE
            MAP1 = AST_CLONE( MAP, STATUS )
         END IF

         IF( AXIS .LT. NDIMS ) THEN
            MAP2 = AST_CMPMAP( MAP1, AST_UNITMAP( NDIMS - AXIS, ' ',
     :                                            STATUS ),
     :                         .FALSE., ' ', STATUS )
         ELSE
            MAP2 = AST_CLONE( MAP1, STATUS )
         END IF

*  Remap the base Frame of the output NDFs WCS component using this
*  mapping.
         CALL KPG1_GTWCS( INDF2, IWCS, STATUS )
         CALL AST_REMAPFRAME( IWCS, AST__BASE, MAP2, STATUS )
         CALL NDF_PTWCS( IWCS, INDF2, STATUS )
      END IF

*  If an error occurred, delete the output NDF.
      IF ( STATUS .NE. SAI__OK ) CALL NDF_DELET( INDF2, STATUS )

*  Free work arrays.
      CALL PSX_FREE( IPW1, STATUS )
      CALL PSX_FREE( IPW2, STATUS )

*  End the NDF and AST contexts.
      CALL NDF_END( STATUS )
      CALL AST_END( STATUS )

*  If an error occurred, report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'EXCLUDEBAD_ERR2', 'EXCLUDEBAD: Unable to '//
     :                 'exclude bad rows or columns from a 2D NDF.',
     :                 STATUS )
      END IF

      END
