      SUBROUTINE ASTCMPMAP( STATUS )
*+
*  Name:
*     ASTCMPMAP

*  Purpose:
*     Create a CmpMap.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTCMPMAP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new CmpMap and optionally initialises its 
*     attributes. A CmpMap is a compound Mapping which allows two component 
*     Mappings (of any class) to be connected together to form a more complex 
*     Mapping. This connection may either be "in series" (where the first 
*     Mapping is used to transform the coordinates of each point and the
*     second mapping is then applied to the result), or "in parallel" where
*     one Mapping transforms the earlier coordinates for each point and the 
*     second Mapping simultaneously transforms the later coordinates). 
*
*     Since a CmpMap is itself a Mapping, it can be used as a component in
*     forming further CmpMaps. Mappings of arbitrary complexity may be built 
*     from simple individual Mappings in this way. 

*  Usage:
*     astcmpmap map1 map2 series options result

*  ADAM Parameters:
*     MAP1 = LITERAL (Read) 
*        An NDF or text file holding the first component Mapping If an NDF 
*        is supplied, the Mapping from the Base Frame to the Current Frame 
*        of its WCS FrameSet will be used.
*     MAP2 = LITERAL (Read) 
*        An NDF or text file holding the second component Mapping If an NDF 
*        is supplied, the Mapping from the Base Frame to the Current Frame 
*        of its WCS FrameSet will be used.
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute 
*        assignments to be used for initialising the new CmpMap. 
*     RESULT = LITERAL (Read)
*        A text file to receive the new CmpMap.
*     SERIES = _LOGICAL (Read)
*        If a true value is given for this parameter, the two component
*        Mappings will be connected in series. A false value requests that 
*        they are connected in parallel. 

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JAN-2001 (DSB):
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

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL AST_ISAMAPPING

*  Local Variables:
      INTEGER RESULT
      INTEGER MAP1
      INTEGER MAP2
      LOGICAL SERIES
*.

*  Check inherited status.      
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the first Mapping.
      CALL ATL1_GTOBJ( 'MAP1', 'Mapping', AST_ISAMAPPING, MAP1, STATUS )

*  Get the second Frame.
      CALL ATL1_GTOBJ( 'MAP2', 'Mapping', AST_ISAMAPPING, MAP2, STATUS )

*  Should the Mappings be combined in series?
      CALL PAR_GET0L( 'SERIES', SERIES, STATUS )

*  Create the required CmpMap.
      RESULT = AST_CMPMAP( MAP1, MAP2, SERIES, ' ', STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTCMPMAP_ERR', 'Error creating a new CmpMap.',
     :                 STATUS )
      END IF

      END
