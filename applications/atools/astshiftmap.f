      SUBROUTINE ASTSHIFTMAP( STATUS )
*+
*  Name:
*     ASTSHIFTMAP

*  Purpose:
*     Create a ShiftMap.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTSHIFTMAP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new ShiftMap and optionally initialises
*     its attributes. A Winmap is a linear Mapping which transforms a 
*     rectangular window in one coordinate system into a similar window 
*     in another coordinate system by shifting each axis (the window 
*     edges being parallel to the coordinate axes). Thus, a ShiftMap is 
*     equivalent to a WinMap with unit scaling on each axis.

*  Usage:
*     astshiftmap ncoord shift options result

*  ADAM Parameters:
*     SHIFT() = _DOUBLE (Read)
*        The values to be added to each axis of the input coordinate system. 
*        There should be one value for each coordinate axis.
*     NCOORD = _INTEGER (Read)
*        The number of coordinate values for the ShiftMap (the same value
*        is used for both input and output axes).
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute 
*        assignments to be used for initialising the new ShiftMap. 
*     RESULT = LITERAL (Read)
*        A text file to receive the new ShiftMap.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-2003 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'NDF_PAR'          ! NDF constants 

*  Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION SHIFT( NDF__MXDIM )
      INTEGER NCOORD
      INTEGER RESULT       
*.

*  Check inherited status.      
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the required parameter values.
      CALL PAR_GET0I( 'NCOORD', NCOORD, STATUS )
      CALL PAR_EXACD( 'SHIFT', NCOORD, SHIFT, STATUS ) 

*  Create the required ShiftMap.
      RESULT = AST_SHIFTMAP( NCOORD, SHIFT, ' ', STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTSHIFTMAP_ERR', 'Error creating a new '//
     :                 'ShiftMap.', STATUS )
      END IF

      END
