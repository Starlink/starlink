      SUBROUTINE KPG1_CPNTR( PNNDF, PNTIT, NDIM, DIMS, ARRAY, NULL,
     :                         STATUS )
*+
*  Name:
*     KPG1_CPNTx
 
*  Purpose:
*     Creates a primitive NDF with a title via the parameter system.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_CPNTx( PNNDF, PNTIT, NDIM, DIMS, ARRAY, NULL, STATUS )
 
*  Description:
*     This routine packages a common series of NDF calls when creating
*     a primitive NDF, whose name is obtained from the parameter system,
*     and also obtaining its title from the parameter system.  The need
*     to handle an optional output file via the null character is
*     catered.  The data type of the NDF data array is the same as the
*     supplied array.  An NDF context is started and ended within this
*     routine.
 
*  Arguments:
*     PNNDF = CHARACTER * ( * ) (Given)
*        The ADAM parameter name to obtain the name of the output NDF.
*     PNTIT = CHARACTER * ( * ) (Given)
*        The ADAM parameter name to obtain the title of the output NDF.
*     NDIM = INTEGER (Given)
*        The number of dimensions in the NDF's data array.
*     DIMS( NDIM ) = INTEGER (Given)
*        The dimensions of the NDF's data array.
*     ARRAY( * ) = ? (Given)
*        The array to be placed in the primitive NDF's data array.
*     NULL = LOGICAL (Given)
*        If true a null value returned by either parameter GET is
*        annulled.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for each numeric data type: replace "x" in
*     the routine name by D, R, I, W, UW, B or UB as appropriate. The
*     data array supplied to the routine must have the data type
*     specified.
 
*  Implementation Deficiencies:
*     More sophisticated parameters to offer more flexibility.  For
*     example, to return the NDF identifier for other operations and
*     not call NDF_END.
*     {routine_deficiencies}...
 
*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1991 June 28 (MJC):
*        Original version.
*     {enter_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter-system error constants
 
*  Arguments Given:
      CHARACTER * ( * ) PNNDF
      CHARACTER * ( * ) PNTIT
      INTEGER NDIM
      INTEGER DIMS( NDIM )
      REAL ARRAY( * )
      LOGICAL NULL
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER
     :  EL,                      ! Number of elements in the data array
     :  IERR,                    ! Location of first conversion error
                                 ! (not used)
     :  NDFO,                    ! NDF identifier
     :  NERR,                    ! Number of conversion errors
                                 ! (not used)
     :  OPNTR( 1 )               ! Pointer to the data array
 
*.
 
*    Check the inherited global status.
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*    Start a new error context.
 
      CALL ERR_MARK
 
*    Start a new NDF context.
 
      CALL NDF_BEGIN
 
*    Create a new NDF.
 
      CALL NDF_CREP( PNNDF, '_REAL', NDIM, DIMS, NDFO, STATUS )
 
*    Map the data array.  Wrap to prevent line overflow when the token
*    is expanded.
 
      CALL KPG1_MAP( NDFO, 'Data', '_REAL', 'WRITE', OPNTR, EL,
     :              STATUS )
 
*    Get the title for the NDF.
 
      CALL NDF_CINP( PNTIT, NDFO, 'TITLE', STATUS )
 
*    Write the array to the file's data array.  There can be no
*    conversion errors so they are not checked.
 
      CALL VEC_RTOR( .FALSE., EL, ARRAY, %VAL( OPNTR( 1 ) ),
     :                   IERR, NERR, STATUS )
 
*    Handle the null case invisibly.
 
      IF ( NULL .AND. STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
 
*    Close down the NDF system.
 
      CALL NDF_END( STATUS )
 
*    Release the new error context.
 
      CALL ERR_RLSE
 
      END
