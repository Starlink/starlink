      SUBROUTINE KPG1_PXDPW( IDIMS, INARR, EXPAND, ODIMS, OUTARR,
     :                         STATUS )
*+
*  Name:
*     KPG1_PXDPx
 
*  Purpose:
*     Expands an n-dimensional array by pixel duplication.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_PXDPx( IDIMS, INARR, EXPAND, ODIMS, OUTARR, STATUS )
 
*  Description:
*     This routine expands an input array by pixel duplication along
*     each dimension.  The duplication factors may be different for
*     each dimension.
 
*  Arguments:
*     IDIMS( NDF__MXDIM ) = INTEGER (Given)
*        The dimensions of the input array.  Unused dimensions
*        up to NDF__MXDIM should be set to one.
*     INARR( * ) = ? (Given)
*        The input data array that is to be enlarged by duplication.
*     EXPAND( NDF__MXDIM ) = INTEGER (Given)
*        The linear expansion factor applied along each dimension.
*        Factors for unused dimensions up to NDF__MXDIM should be set
*        to one.
*     ODIMS( NDF__MXDIM ) = INTEGER (Given)
*        The dimensions of the expanded array.  Unused dimensions up to
*        NDF__MXDIM should be set to one.
*     OUTARR( * ) = ? (Returned)
*        The expanded array.
*     STATUS  =  INTEGER (Given and Returned)
*        Global status value
 
*  Notes:
*     -  This routine works in n-D, where n is 1 to 7.  Even if the
*     array has actually less dimensions there is negligible loss of
*     efficiency to supply dummy (=1) higher dimensions.
*     -  There is a routine for each numeric data type: replace "x" in
*     the routine name by D, R, I, W, UW, B or UB as appropriate. The
*     base and paste arrays supplied to the routine must have the data
*     type specified.
 
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1992 January 7 (MJC):
*        Original version.
*     1995 April 28 (MJC):
*        Completed and used a more-modern style.
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT  NONE             ! No default typing allowed
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE definitions
      INCLUDE 'NDF_PAR'          ! NDF public constants
 
*  Arguments Given:
      INTEGER IDIMS( NDF__MXDIM )
      INTEGER*2 INARR( * )
      INTEGER EXPAND( NDF__MXDIM )
      INTEGER ODIMS( NDF__MXDIM )
 
*  Arguments Returned:
      INTEGER*2 OUTARR( * )
 
*  Status:
      INTEGER STATUS             ! Global inherited status
 
*  Local Variables:
      INTEGER I1                 ! Loop counter
      INTEGER I2                 ! Loop counter
      INTEGER I3                 ! Loop counter
      INTEGER I4                 ! Loop counter
      INTEGER I5                 ! Loop counter
      INTEGER I6                 ! Loop counter
      INTEGER I7                 ! Loop counter
      INTEGER IN( NDF__MXDIM )   ! Sum of pixel offsets for input array
                                 ! leading to input-array index [IN(1)]
      INTEGER IO                 ! Pixel index to output array
      INTEGER J                  ! Loop counter
      INTEGER STRID( NDF__MXDIM ) ! Dimension strides for input array
 
*.
 
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Initialise the stride of dimension number 1 for the input array. (The
*  stride for a dimension is the amount by which the vectorised array
*  index increases when the n-dimensional array index for that
*  dimension increases by 1.)
      STRID( 1 ) = 1
 
*  Calculate the stride for each remaining dimension.
      DO J = 2, NDF__MXDIM
         STRID( J ) = STRID( J - 1 ) * IDIMS( J - 1 )
      END DO
 
*  Initialise the pixel index of the output array.
      IO = 1
 
*  Copy the data from the input array to the output array the
*  appropriate number of times given the expansion factors.  Move
*  through the output array in order and calculate the vectorised pixel
*  index of the input array element that is to be copied to the output
*  element.  This is achieved using the strides and a summation.
      DO I7 = 1, ODIMS( 7 )
         IN( 7 ) = ( I7 - 1 ) / EXPAND( 7 ) * STRID( 7 ) + 1
 
         DO I6 = 1, ODIMS( 6 )
            IN( 6 ) = ( I6 - 1 ) / EXPAND( 6 ) * STRID( 6 ) + IN( 7 )
 
            DO I5 = 1, ODIMS( 5 )
               IN( 5 ) = ( I5 - 1 ) / EXPAND( 5 ) * STRID( 5 ) + IN( 6 )
 
               DO I4 = 1, ODIMS( 4 )
                  IN( 4 ) = ( I4 - 1 ) / EXPAND( 4 ) * STRID( 4 ) +
     :                      IN( 5 )
 
                  DO I3 = 1, ODIMS( 3 )
                     IN( 3 ) = ( I3 - 1 ) / EXPAND( 3 ) * STRID( 3 ) +
     :                         IN( 4 )
 
                     DO I2 = 1, ODIMS( 2 )
                        IN( 2 ) = ( I2 - 1 ) / EXPAND( 2 ) * STRID( 2 )
     :                            + IN( 3 )
 
                        DO I1 = 1, ODIMS( 1 )
                           IN( 1 ) = ( I1 - 1 ) / EXPAND( 1 ) + IN( 2 )
 
*  Copy the input value to the output array.
                           OUTARR( IO ) = INARR( IN( 1 ) )
 
*  Move to the next output pixel by shifting the pixel index of the
*  output pixel by one.
                           IO = IO + 1
 
                        END DO
                     END DO
                  END DO
               END DO
            END DO
         END DO
      END DO
 
      END
