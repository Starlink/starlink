      SUBROUTINE ASHOW( STATUS )
*+
*  Name:
*     ASHOW

*  Purpose:
*     Display attributes of a dataset in text form

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL ASHOW( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     {routine_description}

*  Usage:
*     ASHOW {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (Given)
*        Name of file object whose attributes are to be displayed
*     ITEM = CHAR (Given)
*        The attribute to be displayed
*     DEV = CHAR (Given)
*        The name of the output text device

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {task_references}...

*  Keywords:
*     ashow, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     13 Apr 1995 1.8-0 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_INSET
        LOGICAL			CHR_INSET

*  Local Constants:
      INTEGER			IC_WCS
        PARAMETER ( IC_WCS = 1 )
      INTEGER			IC_ALL
        PARAMETER ( IC_ALL = IC_WCS )

      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'ASHOW Version 1.8-0' )

*  Local Variables:
      CHARACTER*200		FILE, PATH		! File path info
      CHARACTER*20		ITEM			! Items to display

      INTEGER			IFID			! Input dataset id
      INTEGER			ITEMC			! Item code
      INTEGER			NLEV			! # path levels
      INTEGER			OCH			! Output channel
      INTEGER			OUTWID			! Output channel width
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Open the file
      CALL USI_TASSOCI( 'INP', '*', 'READ', IFID, STATUS )

*  Get item to be displayed
      CALL USI_GET0C( 'ITEM', ITEM, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Check against allowed alternatives
        IF ( ITEM .EQ. '*' ) THEN
          ITEMC = IC_ALL

*    Non-wildcard
        ELSE

*      World coordinates?
          IF ( CHR_INSET( ITEM, 'WCS' ) ) THEN
            ITEMC = ITEMC + IC_WCS

*      Otherwise error
          ELSE
            CALL MSG_SETC( 'ITEM', ITEM )
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'ASHOW item list contains unrecognised '/
     :       /'item /^ITEM/, see help for list of valid items', STATUS )

          END IF

        END IF

      END IF

*  Open the output channel
      CALL AIO_ASSOCO( 'DEV', 'LIST', OCH, OUTWID, STATUS )

*  All is well?
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Write details of file
        CALL ADI_FTRACE( IFID, NLEV, PATH, FILE, STATUS )

*    World coordinates?
        IF ( IAND( ITEMC, IC_WCS ) .NE. 0 ) THEN
          CALL ASHOW_WCS( IFID, OCH, STATUS )
        END IF

      END IF

*  Close output device
      CALL AIO_CANCL( 'DEV', STATUS )

*  Tidy up
      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



      SUBROUTINE ASHOW_WCS( IFID, OCH, STATUS )
*+
*  Name:
*     ASHOW_WCS

*  Purpose:
*     Display world coordinates data

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ASHOW_WCS( [p]... )

*  Description:
*     {routine_description}

*  Arguments:
*     {argument_name}[dimensions] = {data_type} ({argument_access_mode})
*        {argument_description}
*     STATUS = INTEGER ({status_access_mode})
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {task_references}...

*  Keywords:
*     ashow, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     13 Apr 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			IFID			! Input dataset id
      INTEGER			OCH			! Output channel

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*1		EFORM			! Form of epoch
      CHARACTER*3		PRJ			! Projection name
      CHARACTER*20		RAS, DECS		! RA,DEC in strings
      CHARACTER*3		SYS			! Coord system name

      DOUBLE PRECISION		EQNX, EPOCH		! Equinox & epoch
      DOUBLE PRECISION		PNT(2)			! Pointing direction

      INTEGER			NVAL			! Values read from obj
      INTEGER			PIXID			! Pixellation
      INTEGER			PRJID			! Projection
      INTEGER			SYSID			! Coord system

      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Load WCS info
      CALL WCI_GETIDS( IFID, PIXID, PRJID, SYSID, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
      END IF

*  Write heading
      CALL AIO_BLNK( OCH, STATUS )
      CALL AIO_IWRITE( OCH, 2, 'World Coordinates :', STATUS )
      CALL AIO_BLNK( OCH, STATUS )

*  First the coordinate system
      CALL AIO_IWRITE( OCH, 4, 'Coordinate system :', STATUS )
      CALL AIO_BLNK( OCH, STATUS )
      IF ( SYSID .NE. ADI__NULLID ) THEN
        CALL ADI_CGET0C( SYSID, 'NAME', SYS, STATUS )
        CALL AIO_IWRITE( OCH, 6, 'System name     : '//SYS, STATUS )
        CALL ADI_CGET0D( SYSID, 'EQUINOX', EQNX, STATUS )
        CALL MSG_SETD( 'EQNX', EQNX )
        CALL AIO_IWRITE( OCH, 6, 'Equinox         : ^EQNX', STATUS )
        CALL ADI_CGET0C( SYSID, 'EFORM', EFORM, STATUS )
        CALL ADI_CGET0D( SYSID, 'EPOCH', EPOCH, STATUS )
        CALL MSG_SETC( 'EF', EFORM )
        CALL MSG_SETD( 'EP', EPOCH )
        CALL AIO_IWRITE( OCH, 6, 'Epoch           : ^EF^EP', STATUS )
      ELSE
        CALL AIO_IWRITE( OCH, 6, '* not present *', STATUS )
      END IF
      CALL AIO_BLNK( OCH, STATUS )

*  The projection
      CALL AIO_IWRITE( OCH, 4, 'Coordinate reference & projection :',
     :                 STATUS )
      CALL AIO_BLNK( OCH, STATUS )
      IF ( SYSID .NE. ADI__NULLID ) THEN

*    Name of projection
        CALL ADI_CGET0C( PRJID, 'NAME', PRJ, STATUS )
        CALL AIO_IWRITE( OCH, 6, 'Projection name : '//PRJ, STATUS )

*    Axis origin
        CALL ADI_CGET1D( PRJID, 'SPOINT', 2, PNT, NVAL, STATUS )
        IF ( SYS(1:2) .EQ. 'FK' ) THEN
          CALL STR_DRADTOC( PNT(1)*MATH__DDTOR, 'HHh MMm SS.SSs', RAS,
     :                      STATUS )
          CALL STR_DRADTOC( PNT(2)*MATH__DDTOR, 'SDDd MMm SS.Ss', DECS,
     :                      STATUS )
          CALL MSG_SETC( 'A', RAS )
          CALL MSG_SETC( 'B', DECS )
        ELSE
          CALL MSG_SETD( 'A', PNT(1) )
          CALL MSG_SETD( 'B', PNT(2) )
        END IF
        CALL AIO_IWRITE( OCH, 6, 'Axis origin : ^A ^B', STATUS )

*    Centre of f.o.v
        CALL ADI_THERE( PRJID, 'NPOINT', THERE, STATUS )
        IF ( THERE ) THEN
          CALL ADI_CGET1D( PRJID, 'NPOINT', 2, PNT, NVAL, STATUS )
          IF ( SYS(1:2) .EQ. 'FK' ) THEN
            CALL STR_DRADTOC( PNT(1)*MATH__DDTOR, 'HHh MMm SS.SSs', RAS,
     :                      STATUS )
            CALL STR_DRADTOC( PNT(2)*MATH__DDTOR, 'SDDd MMm SS.Ss', DECS,
     :                      STATUS )
            CALL MSG_SETC( 'A', RAS )
            CALL MSG_SETC( 'B', DECS )
          ELSE
            CALL MSG_SETD( 'A', PNT(1) )
            CALL MSG_SETD( 'B', PNT(2) )
          END IF
          CALL AIO_IWRITE( OCH, 6, 'FOV position : ^A ^B', STATUS )
        END IF

      ELSE
        CALL AIO_IWRITE( OCH, 6, '* not present *', STATUS )
      END IF
      CALL AIO_BLNK( OCH, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ASHOW_WCS', STATUS )

      END
