      SUBROUTINE CONVERT( STATUS )
*+
*  Name:
*     CONVERT

*  Purpose:
*     Determine how to convert between two coordinate systems.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CONVERT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application compares two FrameSets (or Frames) and determines 
*     whether it is possible to convert between the coordinate systems which 
*     they represent. If conversion is possible, it returns a FrameSet
*     which describes the conversion and which may be used (as a
*     Mapping) to transform coordinate values in either direction.

*  Usage:
*     convert from to domainlist result

*  ADAM Parameters:
*     DOMAINLIST = LITERAL (Read)
*        A string containing a comma-separated list of Frame domains. 
*        This may be used to define a priority order for the different 
*        intermediate coordinate systems that might be used to perform 
*        the conversion. If a blank or null (!) value indicates that all
*        coordinate systems should be considered, regardless of their 
*        domains.
*     FROM = LITERAL (Read)
*        An NDF or text file holding a Frame or FrameSet. If an NDF is 
*        supplied, the WCS FrameSet will be used. It represents the 
*        "source" coordinate system.  This is the coordinate system in 
*        which you already have coordinates available. If a FrameSet is 
*        given, its current Frame is taken to describe the source 
*        coordinate system. 
*     TO = LITERAL (Read)
*        An NDF or text file holding a Frame or FrameSet. If an NDF is 
*        supplied, the WCS FrameSet will be used. It represents the
*        "destination" coordinate system. This is the coordinate system 
*        into which you wish to convert your coordinates. If a FrameSet 
*        is given, its current Frame is taken to describe the destination 
*        coordinate system.
*     RESULT = LITERAL (Read)
*        If the requested coordinate conversion is possible, a FrameSet is 
*        written to the specified text file. Otherwise, an error is
*        reported. If created, the FrameSet will contain two Frames. Frame
*        number 1 (its base Frame) will describe the source coordinate
*        system, corresponding to the FROM parameter. Frame number 2
*        (its current Frame) will describe the destination coordinate
*        system, corresponding to the TO parameter. The Mapping
*        which inter-relates these two Frames will perform the
*        required conversion between their respective coordinate
*        systems.

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
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  External References:
      EXTERNAL AST_ISAFRAME

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER DOMLST*255
      INTEGER FROM
      INTEGER RESULT
      INTEGER TO
*.

*  Check inherited status.      
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the source Frame.
      CALL ATL1_GTOBJ( 'FROM', 'Frame or FrameSet', AST_ISAFRAME, 
     :                 FROM, STATUS )

*  Get the destination Frame.
      CALL ATL1_GTOBJ( 'TO', 'Frame or FrameSet', AST_ISAFRAME, 
     :                 TO, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the domain list.
      CALL PAR_GET0C( 'DOMAINLIST', DOMLST, STATUS )

*  IF a null value was supplied, annul the error and use a blank domain
*  list.
      IF( STATUS .EQ. PAR__NULL ) THEN 
         CALL ERR_ANNUL( STATUS )
         DOMLST = ' '
      END IF

*  Get the required FrameSet.
      RESULT = AST_CONVERT( FROM, TO, DOMLST, STATUS )

*  Report an error if no conversion was possible.
      IF( RESULT .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN 
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CONVERT_ERR1', 'No Mapping could be found '//
     :                 'between the two supplied coordinate systems.', 
     :                 STATUS )
      
*  Otherwise, write the FrameSet out to a text file.
      ELSE
         CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )
      END IF

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CONVERT_ERR', 'Error finding a conversion '//
     :                 'between two FrameSets.', STATUS )
      END IF

      END
