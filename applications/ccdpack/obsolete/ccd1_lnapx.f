      SUBROUTINE CCD1_LNAPX( INDF, IWCS, JI, JO, TR, STATUS )
*+
*  Name:
*     CCD1_LNAPX

*  Purpose:
*     Work out coefficients of linear approximation to a WCS mapping.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_LNAPX( INDF, IWCS, JI, JO, TR, STATUS )

*  Description:
*     This routine generates the six coefficients of a linear
*     approximation to the mapping between two given frames of the 
*     WCS component of an NDF.
*
*     It does this by looking at how the points at three of the 
*     corners of the data grid are transformed between one frame and
*     the other.  No assessment is made of how linear or nonlinear 
*     the mapping actually is.
*
*     The coefficients returned can be used in the transformation 
*     between the first and second frames as follows:
*        X2 = TR( 1 ) + TR( 2 ) * X1 + TR( 3 ) * Y1
*        Y2 = TR( 4 ) + TR( 5 ) * X1 + TR( 6 ) * Y1
*
*     This routine may modify the Current frame of the frameset.

*  Algorithm:
*     First the positions of the corners of the data grid in both the
*     input frame and the output frame coordinate systems are obtained.
*
*     We then have three matrix equations which can be written:
*
*        Equation N:
*           ( XN ) = ( A ) + ( B  C ) ( xN )        
*           ( YN ) = ( D ) + ( E  F ) ( yN )
*
*     where N takes the values (1,2,3); A..F are the coefficients in
*     TR, and (xN, yN) and (XN, YN) are the input and output coordinates
*     respectively.
*
*     Then subtracting equation 3 from equations 1 and 2, and combining
*     the results, gives:
*
*        ( X1-X3  X2-X3 ) = ( B  C ) ( x1-x3  x2-x3 )
*        ( Y1-Y3  Y2-Y3 ) = ( E  F ) ( y1-y3  y2-y3 )
*
*     so that B, C, E and F can be solved by inverting the second matrix.
*     Substituting these values back into equation 3 then gives the
*     values of A and D.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier of the NDF containing the WCS component.
*     IWCS = INTEGER (Given)
*        AST pointer to the WCS component of the NDF.
*     JI = INTEGER (Given)
*        Index in WCS frameset of the frame at the input end of the mapping.
*     JO = INTEGER (Given)
*        Index in WCS frameset of the frame at the output end of the mapping.
*     TR( 6 ) = DOUBLE PRECISION (Returned)
*        The coeffiecients of the linear approximiation to the mapping.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-APR-1999 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! Standard AST constants
      
*  Arguments Given:
      INTEGER INDF
      INTEGER IWCS
      INTEGER JI
      INTEGER JO
      
*  Arguments Returned:
      DOUBLE PRECISION TR( 6 )
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DIM( 2 )           ! Dimensions of NDF
      INTEGER JGRID              ! Index of Grid frame in WCS component
      INTEGER MAPGI              ! AST pointer to grid->input mapping
      INTEGER MAPGO              ! AST pointer to grid->input mapping
      INTEGER NDIM               ! Number of NDF dimensions
      DOUBLE PRECISION DET       ! Determinant of MATI
      DOUBLE PRECISION MATI( 4 ) ! Matrix of input point differences
      DOUBLE PRECISION MATINV( 4 ) ! Inverse of MATI
      DOUBLE PRECISION MATO( 4 ) ! Matrix of input point differences
      DOUBLE PRECISION PXG( 3 )  ! X coordinates of corners in Grid frame
      DOUBLE PRECISION PYG( 3 )  ! Y coordinates of corners in Grid frame
      DOUBLE PRECISION PXI( 3 )  ! X coordinates of corners in input frame
      DOUBLE PRECISION PYI( 3 )  ! Y coordinates of corners in input frame
      DOUBLE PRECISION PXO( 3 )  ! X coordinates of corners in output frame
      DOUBLE PRECISION PYO( 3 )  ! Y coordinates of corners in output frame
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin AST context.
      CALL AST_BEGIN( STATUS )

*  Get bounds of NDF.
      CALL NDF_DIM( INDF, 2, DIM, NDIM, STATUS )

*  Fill an array with the coordinates of three corners of the GRID 
*  domain array.
      PXG( 1 ) = DBLE( DIM( 1 ) )
      PXG( 2 ) = 1D0
      PXG( 3 ) = 1D0
      PYG( 1 ) = 1D0
      PYG( 2 ) = DBLE( DIM( 2 ) )
      PYG( 3 ) = 1D0

*  Get the GRID domain frame from the WCS component.
      CALL CCD1_FRDM( IWCS, 'Grid', JGRID, STATUS )

*  Get AST mappings from the Grid domain frame to FR1 and FR2.
      MAPGI = AST_GETMAPPING( IWCS, JGRID, JI, STATUS )
      MAPGO = AST_GETMAPPING( IWCS, JGRID, JO, STATUS )

*  By applying the mappings from the Grid domain frame to frames FR1 and
*  FR2, find the positions of the corners of the data grid in the
*  coordinate systems of these two frames.
      CALL AST_TRAN2( MAPGI, 3, PXG, PYG, .TRUE., PXI, PYI, STATUS )
      CALL AST_TRAN2( MAPGO, 3, PXG, PYG, .TRUE., PXO, PYO, STATUS )

*  By examining the positions of the corners of the data grid in both
*  frames we can now work out coefficients of the linear approximation
*  which would map one to the other.
*  First generate 2*2 matrices of the relative displacements of input
*  and output points.
      MATI( 1 ) = PXI( 1 ) - PXI( 3 )
      MATI( 2 ) = PYI( 1 ) - PYI( 3 )
      MATI( 3 ) = PXI( 2 ) - PXI( 3 )
      MATI( 4 ) = PYI( 2 ) - PYI( 3 )
      MATO( 1 ) = PXO( 1 ) - PXO( 3 )
      MATO( 2 ) = PYO( 1 ) - PYO( 3 )
      MATO( 3 ) = PXO( 2 ) - PXO( 3 )
      MATO( 4 ) = PYO( 2 ) - PYO( 3 )

*  Invert the input matrix.
      DET = MATI( 1 ) * MATI( 4 ) - MATI( 2 ) * MATI( 3 )
      IF ( DET .EQ. 0D0 ) THEN 
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_LNAPX_ZERODET', 
     :                 'Singular transformation matrix', STATUS )
         GO TO 99
      END IF
      MATINV( 1 ) = MATI( 4 ) / DET
      MATINV( 2 ) = - MATI( 2 ) / DET
      MATINV( 3 ) = - MATI( 3 ) / DET
      MATINV( 4 ) = MATI( 1 ) / DET

*  Determine transformation matrix coefficients by premultiplying inverted
*  input matrix with output matrix.
      TR( 2 ) = MATO( 1 ) * MATINV( 1 ) + MATO( 3 ) * MATINV( 2 )
      TR( 3 ) = MATO( 1 ) * MATINV( 3 ) + MATO( 3 ) * MATINV( 4 )
      TR( 5 ) = MATO( 2 ) * MATINV( 1 ) + MATO( 4 ) * MATINV( 2 )
      TR( 6 ) = MATO( 2 ) * MATINV( 3 ) + MATO( 4 ) * MATINV( 4 )

*  Now work out translational coefficients by substituting the matrix
*  coefficients into the original equation.
      TR( 1 ) = PXO( 3 ) - TR( 2 ) * PXI( 3 ) - TR( 3 ) * PYI( 3 )
      TR( 4 ) = PYO( 3 ) - TR( 5 ) * PXI( 3 ) - TR( 6 ) * PYI( 3 )

*  Error label exit.
 99   CONTINUE

*  End AST context.
      CALL AST_END( STATUS )

      END
* $Id$
