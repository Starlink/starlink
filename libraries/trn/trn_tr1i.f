      SUBROUTINE TRN_TR1I( BAD, NX, XIN, IDT, XOUT, STATUS )
 
 
 
 
 
 
 
*+
*  Name:
*     TRN_TR1I
 
*  Purpose:
*     transform 1-dimensional INTEGER data.
 
*  Language:
*     Starlink Fortran
 
*  Invocation:
*     CALL TRN_TR1I( BAD, NX, XIN, IDT, XOUT, STATUS )
 
*  Description:
*     The routine applies a compiled transformation to a set of
*     1-dimensional data points supplied as a single (X) array of
*     INTEGER coordinate values.
 
*  Arguments:
*     BAD = LOGICAL (given)
*        Whether the input coordinates may be "bad".
*     NX = INTEGER (given)
*        The number of data points to transform.
*     XIN( NX ) = INTEGER (given)
*        Input X coordinate values for the data points.
*     IDT = INTEGER (given)
*        ID for the compiled transformation to be applied.
*     XOUT( NX ) = INTEGER (returned)
*        Array to receive the transformed coordinate values.
*     STATUS = INTEGER (given & returned)
*        Inherited error status.
 
*  Algorithm:
*     - Call TRN_TRNI to perform the transformation.
 
*  Authors:
*     R.F. Warren-Smith (DUVAD::RFWS)
*     {enter_new_authors_here}
 
*  History:
*     18-AUG-1988:  Original version (DUVAD::RFWS)
*     {enter_further_changes_here}
 
*  Bugs:
*     None known.
*     {note_new_bugs_here}
 
*-
 
 
*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing
 
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
 
 
*  Arguments Given:
      LOGICAL BAD               ! Whether input coordinates may be "bad"
 
      INTEGER NX                ! Number of data points to transform
 
      INTEGER XIN( * )           ! Input X coordinate values
 
      INTEGER IDT               ! ID for the compiled transformation to
                                ! be applied
 
*  Arguments Returned:
      INTEGER XOUT( * )          ! Array for transformed coordinate
                                ! values
 
 
*  Status:
      INTEGER STATUS            ! Error status
 
 
*.
 
 
 
*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN
 
 
*   Transform the data points.
      CALL TRN_TRNI( BAD, NX, 1, NX, XIN, IDT, NX, 1, XOUT, STATUS )
 
 
*   Exit routine.
      END
 
 
