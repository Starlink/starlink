      SUBROUTINE KPG1_LITNR( NDIM, ILBND, IUBND, OLBND, OUBND,
     :                         ITOO, OTOI, STATUS )
*+
*  Name:
*     KPG1_LITNx
 
*  Purpose:
*     Creates linear transformation expressions between two
*     n-dimensional co-ordinate systems.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_LITNx( NDIM, ILBND, IUBND, OLBND, OUBND, ITOO, OTOI,
*                      STATUS )
 
*  Description:
*     This routine evaluates linear transformations between each of the
*     dimensions of two co-ordinate systems of the same dimensionality.
*     It substitutes the scale factors and offsets into chartacter
*     expressions suitable for creating a TRANSFORM structure.
 
*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of dimensions in each co-ordinate system.
*     ILBND( NDIM ) = ? (Given)
*        The lower bounds of the input co-ordinate system corresponding
*        to points OLBND in the output co-ordinate system.
*     IUBND( NDIM ) = ? (Given)
*        The upper bounds of the input co-ordinate system corresponding
*        to points OUBND in the output co-ordinate system.
*     OLBND( NDIM ) = ? (Given)
*        The lower bounds of the output co-ordinate system corresponding
*        to points ILBND in the input co-ordinate system.
*     OUBND( NDIM ) = ? (Given)
*        The upper bounds of the output co-ordinate system corresponding
*        to points IUBND in the input co-ordinate system.
*     ITOO( NDIM ) = CHARACTER * ( * ) (Returned)
*        The transformation expressions to convert from the input to
*        the output co-ordinate system.  It should have a length at
*        least of 26 + 2 * VAL__SZR characters.
*     OTOI( NDIM ) = CHARACTER * ( * ) (Returned)
*        The transformation expressions to convert from the output to
*        the input co-ordinate system.  It should have a length at
*        least of 26 + 2 * VAL__SZR characters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for real and double-precision data types:
*     replace "x" in the routine name by R or D respectively.  The
*     bounds of the co-ordinate systems supplied to the routine must
*     have the data type specified.
 
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1993 March 10 (MJC):
*        Original version.
*     {enter_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
 
*  Arguments Given:
      INTEGER NDIM
      REAL ILBND( NDIM )
      REAL IUBND( NDIM )
      REAL OLBND( NDIM )
      REAL OUBND( NDIM )
 
*  Arguments Returned:
      CHARACTER * ( * ) ITOO( NDIM )
      CHARACTER * ( * ) OTOI( NDIM )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER I                  ! Loop counter
      INTEGER NSUBS              ! Number of token substitutions
      REAL OFFSET              ! Offset of a linear transformation
      REAL SCALE               ! Scale of a linear transformation
 
*.
 
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Loop around each dimension.
      DO I = 1, NDIM
 
*  Determine the scale and offset of the transformation between world
*  and data co-ordinates.
         SCALE = ( OUBND( I ) - OLBND( I ) ) /
     :           ( IUBND( I ) - ILBND( I ) )
         OFFSET =  OLBND( I ) - SCALE * ILBND( I )
 
*  Assign the transformations for the co-ordinates.
         ITOO( I ) = 'XL<I> = X<I> * scale + offset'
         OTOI( I ) = 'X<I> = ( XL<I> - offset ) / scale'
 
*  Substitute a token for the dim string so as to give different
*  transformation variables for each dimension.
         CALL TRN_STOKI( 'I', I, OTOI( I ), NSUBS, STATUS )
         CALL TRN_STOKI( 'I', I, ITOO( I ), NSUBS, STATUS )
 
*  Substitute the actual scales for the tokens.
         CALL TRN_STOKR( 'scale', SCALE, OTOI( I ), NSUBS, STATUS )
         CALL TRN_STOKR( 'scale', SCALE, ITOO( I ), NSUBS, STATUS )
 
*  Substitute the actual offsets for the tokens.
         CALL TRN_STOKR( 'offset', OFFSET, OTOI( I ), NSUBS, STATUS )
         CALL TRN_STOKR( 'offset', OFFSET, ITOO( I ), NSUBS, STATUS )
      END DO
 
      END
