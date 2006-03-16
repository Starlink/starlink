      SUBROUTINE ASTSWITCHMAP( STATUS )
*+
*  Name:
*     ASTSWITCHMAP

*  Purpose:
*     Create a SwitchMap.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTSWITCHMAP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new SwitchMap and optionally initialises its 
*     attributes. 
*
*     A SwitchMap is a Mapping which represents a set of alternate
*     Mappings, each of which is used to transform positions within a 
*     particular region of the input or output coordinate system of the 
*     SwitchMap.
*     
*     A SwitchMap can encapsulate any number of Mappings, but they must
*     all have the same number of inputs (Nin attribute value) and the 
*     same number of outputs (Nout attribute value). The SwitchMap itself 
*     inherits these same values for its Nin and Nout attributes. Each of
*     these Mappings represents a "route" through the switch, and are
*     referred to as "route" Mappings below. Each route Mapping transforms 
*     positions between the input and output coordinate space of the entire 
*     SwitchMap, but only one Mapping will be used to transform any given
*     position. The selection of the appropriate route Mapping to use with 
*     any given input position is made by another Mapping, called the
*     "selector" Mapping. Each SwitchMap encapsulates two selector
*     Mappings in addition to its route Mappings; one for use with the
*     SwitchMap's forward transformation (called the "forward selector
*     Mapping"), and one for use with the SwitchMap's inverse transformation 
*     (called the "inverse selector Mapping"). The forward selector Mapping
*     must have the same number of inputs as the route Mappings, but
*     should have only one output. Likewise, the inverse selector Mapping
*     must have the same number of outputs as the route Mappings, but
*     should have only one input. 
*
*     When the SwitchMap is used to transform a position in the forward
*     direction (from input to output), each supplied input position is
*     first transformed by the forward transformation of the forward selector 
*     Mapping. This produces a single output value for each input position 
*     referred to as the selector value. The nearest integer to the selector 
*     value is found, and is used to index the array of route Mappings (the 
*     first supplied route Mapping has index 1, the second route Mapping has 
*     index 2, etc). If the nearest integer to the selector value is less 
*     than 1 or greater than the number of route Mappings, then the SwitchMap 
*     output position is set to a value of AST__BAD on every axis. Otherwise, 
*     the forward transformation of the selected route Mapping is used to 
*     transform the supplied input position to produce the SwitchMap output 
*     position. 
*
*     When the SwitchMap is used to transform a position in the inverse
*     direction (from "output" to "input"), each supplied "output" position 
*     is first transformed by the inverse transformation of the inverse 
*     selector Mapping. This produces a selector value for each "output" 
*     position. Again, the nearest integer to the selector value is found, 
*     and is used to index the array of route Mappings. If this selector 
*     index value is within the bounds of the array of route Mappings, then 
*     the inverse transformation of the selected route Mapping is used to 
*     transform the supplied "output" position to produce the SwitchMap 
*     "input" position. If the selector index value is outside the bounds 
*     of the array of route Mappings, then the SwitchMap "input" position is 
*     set to a value of AST__BAD on every axis. 
*
*     In practice, appropriate selector Mappings should be chosen to
*     associate a different route Mapping with each region of coordinate 
*     space. Note that the SelectorMap class of Mapping is particularly 
*     appropriate for this purpose.
*
*     If a compound Mapping contains a SwitchMap in series with its own
*     inverse, the combination of the two adjacent SwitchMaps will be 
*     replaced by a UnitMap when the compound Mapping is simplified using
*     astsimplify.

*  Usage:
*     astswitchmap fsmap ismap route1 route2 options result

*  ADAM Parameters:
*     FSMAP = LITERAL (Read) 
*        An NDF or text file holding the forward selector Mapping. If an NDF 
*        is supplied, the Mapping from the Base Frame to the Current Frame 
*        of its WCS FrameSet will be used. The supplied Mapping must have a
*        defined forward transformation, but need not have a defined
*        inverse transformation. It must have one output, and the number of
*        inputs must match the number of inputs of each of the supplied 
*        route Mappings. A null (!) value may be supplied, in which case the 
*        SwitchMap will have an undefined forward Mapping.
*     ISMAP = LITERAL (Read) 
*        An NDF or text file holding the inverse selector Mapping. If an NDF 
*        is supplied, the Mapping from the Base Frame to the Current Frame 
*        of its WCS FrameSet will be used. The supplied Mapping must have a
*        defined inverse transformation, but need not have a defined
*        forward transformation. It must have one input, and the number of
*        outputs must match the number of outputs of each of the supplied 
*        route Mappings. A null (!) value may be supplied, in which case the 
*        SwitchMap will have an undefined inverse Mapping.
*     ROUTEMAP1-ROUTEMAP25 = LITERAL (Given)
*        A set of 25 parameters associated with the NDFs or text files holding 
*        the route Mappings. If an NDF is supplied, the Mapping from the Base 
*        Frame to the Current Frame of its WCS FrameSet will be used. All the 
*        supplied route Mappings must have common values for the Nin and Nout 
*        attributes, and these values define the number of inputs and outputs 
*        of the SwitchMap. There can be no missing Mappings; if ROUTEMAP3 is
*        to be processed then ROUTEMAP1 and ROUTEMAP2 must also be supplied. 
*        A null value (!) should be supplied to indicate that there are no 
*        further Mappings. ROUTEMAP3 to ROUTEMAP25 default to null (!).  At 
*        least one Mapping must be supplied.
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute 
*        assignments to be used for initialising the new SwitchMap. 
*     RESULT = LITERAL (Read)
*        A text file to receive the new SwitchMap.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-MAR-2006 (DSB):
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
      INCLUDE 'PAR_ERR'          ! PAR error constants 

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL AST_ISAMAPPING

*  Local Constants:
      INTEGER MAXROUTE
      PARAMETER ( MAXROUTE = 25 )

*  Local Variables:
      CHARACTER PARAM*15
      INTEGER FSMAP
      INTEGER IAT
      INTEGER ISMAP
      INTEGER NROUTE
      INTEGER RESULT
      INTEGER ROUTEMAPS( MAXROUTE )
*.

*  Check inherited status.      
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the forward selector Mapping.
      CALL ATL1_GTOBJ( 'FSMAP', 'Mapping', AST_ISAMAPPING, FSMAP, 
     :                 STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         FSMAP = AST__NULL
      END IF

*  Get the inverse selector Mapping.
      CALL ATL1_GTOBJ( 'ISMAP', 'Mapping', AST_ISAMAPPING, ISMAP, 
     :                 STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         ISMAP = AST__NULL
      END IF

*  Get the first two route Mappings. These must be supplied.
      CALL ATL1_GTOBJ( 'ROUTEMAP1', 'Mapping', AST_ISAMAPPING, 
     :                 ROUTEMAPS( 1 ), STATUS )
      CALL ATL1_GTOBJ( 'ROUTEMAP2', 'Mapping', AST_ISAMAPPING, 
     :                 ROUTEMAPS( 2 ), STATUS )

*  Loop round getting route Mappings until a null value is supplied. 
*  These can be omitted.
      NROUTE = 3
      DO WHILE( NROUTE .LE. MAXROUTE .AND. STATUS .EQ. SAI__OK )
         PARAM = 'ROUTEMAP'
         IAT = 8
         CALL CHR_PUTI( NROUTE, PARAM, IAT )
         CALL ATL1_GTOBJ( PARAM, 'Mapping', AST_ISAMAPPING, 
     :                    ROUTEMAPS( NROUTE ), STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            NROUTE = NROUTE - 1
            GO TO 10
         ELSE
            NROUTE = NROUTE + 1
         END IF
      END DO
 10   CONTINUE

*  Create the required SwitchMap.
      RESULT = AST_SWITCHMAP( FSMAP, ISMAP, NROUTE, ROUTEMAPS, ' ', 
     :                        STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTSWITCHMAP_ERR', 'Error creating a new '//
     :                 'SwitchMap.', STATUS )
      END IF

      END
