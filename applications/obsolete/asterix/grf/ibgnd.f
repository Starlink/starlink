      SUBROUTINE IBGND( STATUS )
*+
*  Name:
*     IBGND

*  Purpose:
*     Background modelling of image data

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL IBGND( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     {routine_description}

*  Usage:
*     ibgnd {parameter_usage}

*  Environment Parameters:
*     CMD = CHARACTER (read)
*        Command name. One of RESET
*     X = REAL (read)
*        X coordinate of source position or bgnd region
*     Y = REAL (read)
*        Y coordinate of source position or bgnd region
*     R = REAL (read)
*        Radius of source to knock out
*     ISRC = INTEGER (read)
*        Source number
*     SAMPLING = CHARACTER (read)
*        The sampling mode
*     RBIN = REAL (read)
*        Radial bin size in annular sampling
*     OUT = CHAR (read)
*        Output file name for model

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
*     ibgnd, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 V2.0-0 (DJA):
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

*  Global Variables:
      INCLUDE 'IMG_CMN'

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_SIMLR
        LOGICAL			CHR_SIMLR

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'IBGND Version V2.0-0' )

*  Local Variables:
      CHARACTER*16		CMD			! Major mode
      CHARACTER*7		MODE			! Sampling mode
      CHARACTER*80		OFILE			! Output file name
      CHARACTER*10		SUBMODE			! Sampling mode

      REAL			XPOS, YPOS, R		! Source position

      INTEGER			ISRC			! Source number
      INTEGER			OFID			! Output file id
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL USI_INIT()

*  Check image processing active
      IF ( .NOT. I_OPEN ) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSE

*    Make sure transformations are correct
        CALL GTR_RESTORE( STATUS )
        CALL GCB_ATTACH( 'IMAGE', STATUS )
        CALL IMG_2DGCB( STATUS )

*    Get main mode
        CMD = ' '
        DO WHILE ( (CMD.EQ.' ') .AND. (STATUS.EQ.SAI__OK) )
          CALL USI_GET0C( 'MODE', CMD, STATUS )
          CALL CHR_UCASE( CMD )
          IF ( CMD .EQ. 'HELP' ) THEN
            CALL IBGND_HELP()
            CALL USI_CANCL( 'MODE', STATUS )
            CMD = ' '
          END IF
        END DO

*    Switch on command
        IF ( STATUS .EQ. SAI__OK ) THEN

*      Reset internal variables?
          IF ( CMD .EQ. 'RESET' ) THEN
            CALL IBGND_RESET( .TRUE., .TRUE., .TRUE., STATUS )

*      Start modelling
          ELSE IF ( CMD .EQ. 'START' ) THEN

*        Don't know what image is displayed
            I_BGM_DISIM = 0

*        If already modelling then update to take account of changes since
*        last restart (eg. in regions)
            IF ( I_BGM_ON ) THEN

*          Redefine samples
              CALL IBGND_SETQ( STATUS )

*          Recompute the samples and surface
              CALL IBGND_RECALC( .TRUE., .TRUE., STATUS )

            ELSE
              CALL IBGND_NEW( STATUS )
            END IF

*      Start a new model
          ELSE IF ( CMD .EQ. 'NEW' ) THEN
            CALL IBGND_NEW( STATUS )

*      Display internal state
          ELSE IF ( CMD .EQ. 'SHOW' ) THEN
            CALL IBGND_SHOW( STATUS )

*      Subsequent options depend on modeller being active
          ELSE IF ( .NOT. I_BGM_ON ) THEN
            CALL MSG_PRNT('AST_ERR: no background model defined')

*      Add a source to the database
          ELSE IF ( CMD .EQ. 'ADDSRC' ) THEN

*        Get circle from user
            CALL IMG_GETCIRC( 'X','Y', 'RAD', XPOS, YPOS, R, STATUS )

*        Add to list
            CALL IBGND_ADDSRC( XPOS, YPOS, R, STATUS )

*        Mark the new soruce
            CALL IBGND_MARK( I_BGM_NSRC, STATUS )

*      Mark sources
          ELSE IF ( CMD .EQ. 'MARKSRC' ) THEN
            DO ISRC = 1, I_BGM_NSRC
              CALL IBGND_MARK( ISRC, STATUS )
            END DO

*      Change the sampling mode
          ELSE IF ( CMD .EQ. 'SETSAMP' ) THEN

            CALL USI_GET0C( 'SAMPLING', MODE, STATUS )
            CALL CHR_UCASE( MODE )
            CALL IBGND_SETSAMP( MODE, 'MEAN', STATUS )

            CALL IBGND_RECALC( .TRUE., .TRUE., STATUS )

*      Change the sample fitting
          ELSE IF ( CMD .EQ. 'SETFIT' ) THEN

            CALL USI_GET0C( 'SAMPFIT', MODE, STATUS )
            CALL CHR_UCASE( MODE )
            I_BGM_FIT = MODE

            CALL IBGND_RECALC( .FALSE., .TRUE., STATUS )

*      Delete a source
          ELSE IF ( CMD .EQ. 'DELSRC' ) THEN

*        Delete selected source
            CALL USI_GET0I( 'ISRC', ISRC, STATUS )
            CALL IBGND_DELSRC( ISRC, STATUS )

*      Display model
          ELSE IF ( CMD .EQ. 'MDISP' ) THEN
            CALL IBGND_DISP_SURF( 1, STATUS )

*      Display data
          ELSE IF ( CMD .EQ. 'DDISP' ) THEN
            CALL IBGND_DISP_SURF( 2, STATUS )

*      Display model residuals
          ELSE IF ( CMD .EQ. 'RDISP' ) THEN
            CALL IBGND_DISP_SURF( -1, STATUS )

*      Save sources to file
          ELSE IF ( CMD .EQ. 'SAVESRC' ) THEN
            CALL USI_GET0C( 'SUBMODE', SUBMODE, STATUS )
            IF ( .NOT. CHR_SIMLR( SUBMODE, 'internal' ) ) THEN
              CALL USI_GET0C( 'OUT', OFILE, STATUS )
            END IF
            CALL IBGND_SAVESRC( SUBMODE, OFILE, STATUS )

*      Save model to file
          ELSE IF ( CMD .EQ. 'SAVE' ) THEN
            CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )
            CALL IBGND_SAVE( OFID, STATUS )

          END IF

        END IF

      END IF

*  Tidy up
      CALL USI_CLOSE()

      END



      SUBROUTINE IBGND_SHOW( STATUS )
*+
*  Name:
*     IBGND_SHOW

*  Purpose:
*     Display state of background modeller

*  Language:
*     Starlink Fortran

*  Type of Module:
*     Task subroutine

*  Invocation:
*     CALL IBGND_SHOW( STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     STATUS = INTEGER (given)
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
*     ibgnd, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'IMG_CMN'

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      CHARACTER*79		TXT			! Output text buffer

      REAL			M, EM
      REAL			X, Y, R			! Source attrs

      INTEGER			I			! Loop over sources
      INTEGER			ISTAT			! I/o status code
      INTEGER			NP			! # points in sample
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Modelling started?
      CALL MSG_BLNK()
      IF ( I_BGM_ON ) THEN

*    Sources
        IF ( I_BGM_NSRC .GT. 0 ) THEN
          CALL MSG_PRNT( '  Src      X          Y          R' )
          CALL MSG_BLNK()
          DO I = 1, I_BGM_NSRC
            CALL IBGND_GETSRC( I, X, Y, R, STATUS )
            WRITE( TXT, '(2X,I3,3(2X,1PG12.5))', IOSTAT=ISTAT )
     :                       I, X, Y, R
            CALL MSG_PRNT( TXT )

          END DO
        ELSE
          CALL MSG_PRNT( '  No source candidates defined' )
        END IF
        CALL MSG_BLNK()

*    Sampling
        CALL MSG_PRNT( '  Samp    Mean (N)' )
        DO I = 1, I_BGM_NSAMP
          CALL ARR_ELEM1R( I_BGM_SAMPTR(1), I_BGM_NSAMP, I, M, STATUS )
          CALL ARR_ELEM1R( I_BGM_SAMPTR(2), I_BGM_NSAMP, I, EM, STATUS )
          CALL ARR_ELEM1I( I_BGM_SAMPTR(3), I_BGM_NSAMP, I, NP, STATUS )
          CALL MSG_SETR( 'M', M )
          CALL MSG_SETR( 'EM', EM )
          CALL MSG_SETI( 'N', NP )
          CALL MSG_SETI( 'I', I )
          CALL MSG_PRNT( '   ^I    ^M +- ^EM (^N)' )
        END DO

*    Surface

      ELSE
        CALL MSG_PRNT( '  No source candidates defined' )
        CALL MSG_PRNT( '  No image sampling defined' )
        CALL MSG_PRNT( '  No surface definition defined' )
      END IF
      CALL MSG_BLNK()

      END


      SUBROUTINE IBGND_MARK( ISRC, STATUS )
*+
*  Name:
*     IBGND_MARK

*  Purpose:
*     Mark sources on display

*  Language:
*     Starlink Fortran

*  Type of Module:
*     Task subroutine

*  Invocation:
*     CALL IBGND_MARK( STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ISRC = INTEGER (given)
*        The source number
*     STATUS = INTEGER (given)
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
*     ibgnd, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'IMG_CMN'

*  Arguments Given:
      INTEGER			ISRC

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      CHARACTER*4		STR			! String of I

      REAL			X, Y, R			! Source attrs

      INTEGER			NDIGIT			! # chars used in STR
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      STR = ' '

*  Extract data
      CALL IBGND_GETSRC( ISRC, X, Y, R, STATUS )

*  Plot a circle
      CALL IMG_CIRCLE( X, Y, R, STATUS )

*  Write string with source number
      X = X + R*COSD(45.0)*SIGN(1.0,I_XSCALE)
      Y = Y + R*SIND(45.0)
      CALL PGUPDT(0)
      CALL CHR_ITOC( ISRC, STR(2:), NDIGIT )
      CALL PGTEXT( X, Y, STR(:NDIGIT+1) )
      CALL PGUPDT(2)
      CALL PGUPDT(1)

      END


      SUBROUTINE IBGND_GETSRC( ISRC, X, Y, R, STATUS )
*+
*  Name:
*     IBGND_GETSRC

*  Purpose:
*     Get source attributes

*  Language:
*     Starlink Fortran

*  Type of Module:
*     Task subroutine

*  Invocation:
*     CALL IBGND_GETSRC( ISRC, X, Y, R, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ISRC = INTEGER (given)
*        The source number
*     X = REAL (returned)
*        Source X position
*     Y = REAL (returned)
*        Source Y position
*     R = REAL (returned)
*        Source radius
*     STATUS = INTEGER (given)
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
*     ibgnd, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'IMG_CMN'

*  Arguments Given:
      INTEGER			ISRC

*  Arguments Returned:
      REAL			X, Y, R

*  Status:
      INTEGER			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract data
      CALL ARR_ELEM1R( I_BGM_SRCPTR(1), I_BGM_NSRC, ISRC, X, STATUS )
      CALL ARR_ELEM1R( I_BGM_SRCPTR(2), I_BGM_NSRC, ISRC, Y, STATUS )
      CALL ARR_ELEM1R( I_BGM_SRCPTR(3), I_BGM_NSRC, ISRC, R, STATUS )

      END



      SUBROUTINE IBGND_ADDSRC( X, Y, R, STATUS )
*+
*  Name:
*     IBGND_ADDSRC

*  Purpose:
*     Add a source to the background modeller database

*  Language:
*     Starlink Fortran

*  Type of Module:
*     Task subroutine

*  Invocation:
*     CALL IBGND_ADDSRC( X, Y, R, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     X = REAL (given)
*        X position of source
*     Y = REAL (given)
*        Y position of source
*     R = REAL (given)
*        Radius of source to exclude from background estimation
*     STATUS = INTEGER (given)
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
*     ibgnd, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'

*  Global Variables:
      INCLUDE 'IMG_CMN'

*  Arguments Given:
      REAL			X, Y, R

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      INTEGER			ITEMID
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Maximum number of sources already?
      IF ( I_BGM_NSRC .EQ. I__MXBGSRC ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Maximum number of sources exceeded',
     :                STATUS )
      ELSE

*    Increment source counter
        I_BGM_NSRC = I_BGM_NSRC + 1

*    Add source attributes
        CALL ARR_SELEM1R( I_BGM_SRCPTR(1), I__MXBGSRC, I_BGM_NSRC,
     :                    X, STATUS )
        CALL ARR_SELEM1R( I_BGM_SRCPTR(2), I__MXBGSRC, I_BGM_NSRC,
     :                    Y, STATUS )
        CALL ARR_SELEM1R( I_BGM_SRCPTR(3), I__MXBGSRC, I_BGM_NSRC,
     :                    R, STATUS )

*    Redo the quality
        CALL IBGND_SETQ( STATUS )

*    Recompute the samples and surface
        CALL IBGND_RECALC( .TRUE., .TRUE., STATUS )

*    Update noticeboard
        IF ( I_GUI ) THEN

          CALL NBS_FIND_ITEM( I_NBID, 'BG_NSRC', ITEMID, STATUS )
          CALL NBS_PUT_VALUE( ITEMID, 0, VAL__NBI, I_BGM_NSRC, STATUS )

          CALL NBS_FIND_ITEM( I_NBID, 'BG_SRCX', ITEMID, STATUS )
          CALL NBS_PUT_VALUE( ITEMID, 0, VAL__NBR, X, STATUS )

          CALL NBS_FIND_ITEM( I_NBID, 'BG_SRCY', ITEMID, STATUS )
          CALL NBS_PUT_VALUE( ITEMID, 0, VAL__NBR, Y, STATUS )

          CALL NBS_FIND_ITEM( I_NBID, 'BG_SRCR', ITEMID, STATUS )
          CALL NBS_PUT_VALUE( ITEMID, 0, VAL__NBR, R, STATUS )

        END IF

      END IF

      END



      SUBROUTINE IBGND_DELSRC( ISRC, STATUS )
*+
*  Name:
*     IBGND_DELSRC

*  Purpose:
*     Delete a source from the background modeller database

*  Language:
*     Starlink Fortran

*  Type of Module:
*     Task subroutine

*  Invocation:
*     CALL IBGND_DELSRC( ISRC, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ISRC = INTEGER (given)
*        Number of source to delete
*     STATUS = INTEGER (given)
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
*     ibgnd, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'

*  Global Variables:
      INCLUDE 'IMG_CMN'

*  Arguments Given:
      INTEGER			ISRC

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over source attrs
      INTEGER			ITEMID			! Source NBS item id
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Any sources above this one?
      IF ( I_BGM_NSRC .GT. ISRC ) THEN

*    Move their data down
        DO I = 1, I__NSRCATT
          CALL ARR_COP1R( I_BGM_NSRC - ISRC,
     :                    %VAL(I_BGM_SRCPTR(I)+ISRC*VAL__NBR),
     :                    %VAL(I_BGM_SRCPTR(I)+(ISRC-1)*VAL__NBR),
     :                    STATUS )
        END DO

      END IF

*  Update number of sources
      I_BGM_NSRC = I_BGM_NSRC - 1

*  Update noticeboard
      IF ( I_GUI ) THEN
        CALL NBS_FIND_ITEM( I_NBID, 'BG_NSRC', ITEMID, STATUS )
        CALL NBS_PUT_VALUE( ITEMID, 0, VAL__NBI, I_BGM_NSRC, STATUS )
      END IF

*  Initialise the the background model quality array. This is ok for points
*  inside the current region, and bad outside and for bad input pixels
      CALL IBGND_SETQ( STATUS )

*  Recompute the samples and surface
      CALL IBGND_RECALC( .TRUE., .TRUE., STATUS )

      END



      SUBROUTINE IBGND_DISP_SURF( PCODE, STATUS )
*+
*  Name:
*     IBGND_DISP_SURF

*  Purpose:
*     Display the background surface (or residuals) as a pixel plot

*  Language:
*     Starlink Fortran

*  Type of Module:
*     Task subroutine

*  Invocation:
*     CALL IBGND_DISP_SURF( PCODE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PCODE = INTEGER (given)
*        Which image? Model = 1, residuals = -1
*     STATUS = INTEGER (given)
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
*     ibgnd, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'QUAL_PAR'

*  Global Variables:
      INCLUDE 'IMG_CMN'

*  Arguments Given:
      INTEGER			PCODE

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      INTEGER			DPTR			! Data to plot
      REAL			PMIN, PMAX		! Pixel bounds

      LOGICAL			FRESH			! Device freshly opened
      LOGICAL			OK, YES			!
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Clear display unless freshly opened device
      CALL GDV_FRESH(FRESH,STATUS)
      IF (.NOT.FRESH) THEN
        IF (I_SPLIT_DISP) THEN
          CALL IMG_CLEAR(1,STATUS)
        ELSE
          CALL GDV_CLEAR(STATUS)
        END IF
      END IF

*  Reset transformation database
      CALL GTR_ZERO(STATUS)

*  Ensure we have the correct GCB
      CALL GCB_ATTACH( 'IMAGE', STATUS )
      CALL IMG_2DGCB( STATUS )

*  display image
      CALL IMG_WINDOW( STATUS )

      CALL GCB_GETL('PIX_FLAG',OK,YES,STATUS )
      IF (OK.AND.YES) THEN

*    Set colours
        CALL GFX_SETCOLS(STATUS)

*    Convert to residuals?
        IF ( PCODE .EQ. -1 ) THEN
          CALL IBGND_SURF_RESID( I_NX*I_NY, %VAL(I_BGM_SAMIDX),
     :                           %VAL(I_DPTR), %VAL(I_BGM_DPTR),
     :                           .TRUE., STATUS )
          DPTR = I_BGM_DPTR
        ELSE IF ( PCODE .EQ. 1 ) THEN
          DPTR = I_BGM_DPTR
        ELSE IF ( PCODE .EQ. 2 ) THEN
          DPTR = I_DPTR
        END IF

*    Plot bounds
        CALL ARR_RANG1R( I_NX*I_NY, %VAL(DPTR), PMIN, PMAX, STATUS )
        IF ( PMIN .EQ. PMAX ) THEN
          PMIN = 0.95*PMIN
          PMAX = 1.05*PMAX
        END IF
        I_PMIN = PMIN
        I_PMAX = PMAX

*    Plot the pixels
        IF ( (PCODE.NE.1) .AND. I_QOK .AND. I_BAD ) THEN
          CALL GFX_PIXEL(I_WKPTR,I_NX,I_NY,I_IX1,I_IX2,I_IY1,I_IY2,
     :                .TRUE.,%VAL(I_XPTR),%VAL(I_YPTR),0,0,
     :                     %VAL(DPTR), PMIN,PMAX,%VAL(I_QPTR),
     :                    I_MASK,STATUS)
        ELSE
          CALL GFX_PIXEL(I_WKPTR,I_NX,I_NY,I_IX1,I_IX2,I_IY1,I_IY2,
     :                .TRUE.,%VAL(I_XPTR),%VAL(I_YPTR),0,0,
     :                     %VAL(DPTR), PMIN,PMAX,STATUS)
        END IF

*    Convert back from residuals?
        IF ( PCODE .EQ. -1 ) THEN
          CALL IBGND_SURF_RESID( I_NX*I_NY, %VAL(I_BGM_SAMIDX),
     :                           %VAL(I_DPTR), %VAL(I_BGM_DPTR),
     :                           .FALSE., STATUS )
        END IF

      ENDIF

*  Plot axes
      CALL IMG_AXES( STATUS )

*  Flag current plotting status
      I_DISP = .TRUE.
      I_DISP_1D = .FALSE.
      I_BGM_DISIM = PCODE

      END



      SUBROUTINE IBGND_SAVE( FID, STATUS )
*+
*  Name:
*     IBGND_SAVE

*  Purpose:
*     Save current model to file

*  Language:
*     Starlink Fortran

*  Type of Module:
*     Task subroutine

*  Invocation:
*     CALL IBGND_SAVE( FID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     FID = INTEGER (given and returned)
*        Output file id of of saved model
*     STATUS = INTEGER (given)
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
*     ibgnd, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'QUAL_PAR'

*  Global Variables:
      INCLUDE 'IMG_CMN'

*  Arguments Given and Returned:
      INTEGER			FID

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			BIT_ANDUB
        BYTE			BIT_ANDUB
      EXTERNAL			BIT_NOTUB
        BYTE			BIT_NOTUB

*  Local Variables:
      INTEGER			DIMS(2)			! Output dimensions
      INTEGER			DPTR			! Output data array
      INTEGER			IAX			! Loop over axes
      INTEGER			QPTR			! Output quality array

      BYTE			MASK			! Output quality mask
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Link output image
      DIMS(1) = I_NX
      DIMS(2) = I_NY
      CALL BDI_LINK( 'XYimage', 2, DIMS, 'REAL', FID, STATUS )

*  Top level text
      CALL BDI_PUT0C( FID, 'Title', 'Background model image', STATUS )
      CALL BDI_PUT0C( FID, 'Label', I_LABEL, STATUS )
      CALL BDI_PUT0C( FID, 'Units', I_UNITS, STATUS )

*  Write out data
      CALL BDI_MAPR( FID, 'Data', 'WRITE', DPTR, STATUS )
      CALL ARR_COP1R( I_NX*I_NY, %VAL(I_BGM_DPTR), %VAL(DPTR), STATUS )
      CALL BDI_UNMAP( FID, 'Data', DPTR, STATUS )

*  Write quality
      MASK = BIT_ANDUB( QUAL__MASK, BIT_NOTUB(QUAL__PATCHED) )
      CALL BDI_PUT0UB( FID, 'QualityMask', MASK, STATUS )
      CALL BDI_MAPUB( FID, 'Quality', 'WRITE', QPTR, STATUS )
      CALL IBGND_IDX2Q( I_NX*I_NY, %VAL(I_BGM_SAMIDX), %VAL(QPTR),
     :                  STATUS )
      CALL BDI_UNMAP( FID, 'Quality', QPTR, STATUS )

*  Copy axes
      DO IAX = 1, 2
        CALL BDI_AXCOPY( I_FID, IAX, ' ', FID, IAX, STATUS )
      END DO

*  Copy ancilliary stuff from input
      CALL UDI_COPANC( I_FID, ' ', FID, STATUS )

      END



      SUBROUTINE IBGND_SAVESRC( MODE, FILE, STATUS )
*+
*  Name:
*     IBGND_SAVESRC

*  Purpose:
*     Save current source list

*  Language:
*     Starlink Fortran

*  Type of Module:
*     Task subroutine

*  Invocation:
*     CALL IBGND_SAVESRC( MODE, FILE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     MODE = CHARACTER*(*) (given)
*        Output mode
*     FILE = CHARACTER*(*) (given)
*        Output filename
*     STATUS = INTEGER (given and returned)
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
*     ibgnd, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MATH_PAR'

*  Global Variables:
      INCLUDE 'IMG_CMN'

*  Arguments Given:
      CHARACTER*(*)		MODE, FILE

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_SIMLR
        LOGICAL			CHR_SIMLR

*  Local Constants:
      INTEGER			M_INT			! Internal mode flag
        PARAMETER		( M_INT = 1 )

      INTEGER			M_TXT			! Ascii mode flag
        PARAMETER		( M_TXT = 2 )

*  Local Variables:
      CHARACTER*18		CTIME			! Date and time string
      CHARACTER*11		RAS, DECS		! Formatted coords

      DOUBLE PRECISION		RA, DEC			! Source position

      REAL			X, Y, R			! Source attributes

      INTEGER			I			! Loop over sources
      INTEGER			IMODE			! Output mode
      INTEGER			OCH			! Output channel
      INTEGER			OWID			! Channel width
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Internal mode?
      IF ( CHR_SIMLR( MODE, 'internal' ) ) THEN
        IMODE = M_INT
      ELSE
        IMODE = M_TXT
      END IF

*  Sources to output?
      IF ( I_BGM_NSRC .GT. 0 ) THEN

*    Start the listing. Internal mode?
        IF ( IMODE .EQ. M_INT ) THEN

*      Get group id for storing positions
          IF ( I_NPOS .EQ. 0 ) THEN
            CALL GRP_NEW( 'Source list', I_POS_ID, STATUS )

*      Should give choice of appending here
          ELSE
            CALL GRP_SETSZ( I_POS_ID, 0, STATUS )
            I_NPOS = 0

          END IF

*    Ascii text file mode
        ELSE IF ( IMODE .EQ. M_TXT ) THEN

*      Open the file
          CALL AIO_OPEN( FILE, 'LIST', OCH, OWID, STATUS )

*      Write a header
          CALL TCI_CTIME( CTIME, STATUS )
          CALL AIO_WRITE( OCH, '# Source list written by IBGND at '/
     :                    /CTIME, STATUS )

        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Read each source in turn
        DO I = 1, I_BGM_NSRC

*      Get source data
          CALL IBGND_GETSRC( I, X, Y, R, STATUS )

*      Convert X,Y to RA,DEC
          CALL IMG_WORLDTOCEL( X, Y, RA, DEC, STATUS )

*      Switch on mode. Internal mode
          IF ( IMODE .EQ. M_INT ) THEN

*        Add to internal list
            CALL IMG_PUTPOS( RA, DEC, STATUS )

*      Ascii mode
          ELSE IF ( IMODE .EQ. M_TXT ) THEN

*        Format text
            CALL STR_DRADTOC( RA*MATH__DDTOR, 'HH:MM:SS.SS', RAS,
     :                        STATUS )
            CALL STR_DRADTOC( DEC*MATH__DDTOR, 'SDD:MM:SS.S', DECS,
     :                        STATUS )

*        Write it out, with radius as third column
            CALL MSG_SETR( 'R', R )
            CALL AIO_WRITE( OCH, RAS//' '//DECS//' ^R', STATUS )

          END IF

        END DO

*    End the listing
        IF ( IMODE .EQ. M_INT ) THEN
        ELSE IF ( IMODE .EQ. M_TXT ) THEN

*      Close the file
          CALL AIO_CLOSE( OCH, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

        END IF

      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'No sources to output', STATUS )

      END IF

*  Abort point
  99  CONTINUE

      END



      SUBROUTINE IBGND_IDX2Q( N, IDX, QUAL, STATUS )
*+
*  Name:
*     IBGND_IDX2Q

*  Purpose:
*     Convert sampling index value to quality

*  Language:
*     Starlink Fortran

*  Type of Module:
*     Task subroutine

*  Invocation:
*     CALL IBGND_IDX2Q( N, IDX, QUAL, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     N = INTEGER (given)
*        Number of pixels in image
*     IDX[N] = INTEGER (given)
*        Sampling index per pixel, -1 for outside region, 0 for bad quality,
*        <sample> otherwise
*     QUAL[N] = BYTE (returned)
*        Quality value per pixel
*     STATUS = INTEGER (given and returned)
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
*     ibgnd, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'QUAL_PAR'

*  Arguments Given:
      INTEGER			N, IDX(*)

*  Arguments Returned:
      BYTE			QUAL(*)

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over arrays
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over input index
      DO I = 1, N
        IF ( IDX(I) .LT. 0 ) THEN
          QUAL(I) = QUAL__MISSING
        ELSE IF ( IDX(I) .EQ. 0 ) THEN
          QUAL(I) = QUAL__PATCHED
        ELSE
          QUAL(I) = QUAL__GOOD
        END IF
      END DO

      END



      SUBROUTINE IBGND_SURF_RESID( N, IDX, DATA, MODEL, FORW, STATUS )
*+
*  Name:
*     IBGND_SURF_RESID

*  Purpose:
*     Calculate or uncalculate residuals

*  Language:
*     Starlink Fortran

*  Type of Module:
*     Task subroutine

*  Invocation:
*     CALL IBGND_SURF_RESID( N, IDX, DATA, MODEL, FORW, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     RESID = LOGICAL (given)
*        Display residuals?
*     STATUS = INTEGER (given)
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
*     ibgnd, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 (DJA):
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
      INTEGER			N, IDX(*)
      REAL			DATA(*)
      LOGICAL			FORW

*  Arguments Given and Returned:
      REAL			MODEL(*)

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over inputs
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Forward?
      IF ( FORW ) THEN
        DO I = 1, N
          IF ( IDX(I) .GE. 0 ) THEN
            MODEL(I) = DATA(I) - MODEL(I)
          END IF
        END DO
      ELSE
        DO I = 1, N
          IF ( IDX(I) .GE. 0 ) THEN
            MODEL(I) = DATA(I) - MODEL(I)
          END IF
        END DO
      END IF

      END



      SUBROUTINE IBGND_NEW( STATUS )
*+
*  Name:
*     IBGND_NEW

*  Purpose:
*     Start new background model

*  Language:
*     Starlink Fortran

*  Type of Module:
*     Task subroutine

*  Invocation:
*     CALL IBGND_NEW( STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     STATUS = INTEGER (given)
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
*     ibgnd, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'QUAL_PAR'

*  Global Variables:
      INCLUDE 'IMG_CMN'

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      LOGICAL			ALLOC			! Allocate bg memory?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Reset internals
      CALL IBGND_RESET( .TRUE., .TRUE., .TRUE., STATUS )

*  Allocate space for the background data surface, quality (and errors?). If
*  image size has changed we need to increase existing allocations
      ALLOC = .TRUE.
      IF ( I_BGM_ON .AND. ((I_NX*I_NY) .GT. I_BGM_NELM) ) THEN
        CALL DYN_UNMAP( I_BGM_DPTR, STATUS )
        CALL DYN_UNMAP( I_BGM_SAMIDX, STATUS )
      ELSE IF ( I_BGM_ON ) THEN
        ALLOC = .FALSE.
      END IF
      IF ( ALLOC ) THEN
        CALL DYN_MAPR( 1, I_NX*I_NY, I_BGM_DPTR, STATUS )
        CALL DYN_MAPI( 1, I_NX*I_NY, I_BGM_SAMIDX, STATUS )
        I_BGM_NELM = I_NX*I_NY
      END IF

*  Set the sample mode to the whole image, simple mean and compute the samples
      CALL IBGND_SETSAMP( 'WHOLE', 'MEAN', STATUS )

*  Calculate everything
      CALL IBGND_RECALC( .TRUE., .TRUE., STATUS )

*  Switch modelling on
      I_BGM_ON = (STATUS.EQ.SAI__OK)

      END



      SUBROUTINE IBGND_SETQ( STATUS )
*+
*  Name:
*     IBGND_SETQ

*  Purpose:
*     Set quality good/bad for background modeller

*  Language:
*     Starlink Fortran

*  Type of Module:
*     Task subroutine

*  Invocation:
*     CALL IBGND_SETQ( STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     STATUS = INTEGER (given and returned)
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
*     ibgnd, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'IMG_CMN'

*  Status:
      INTEGER			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call internal routine to set sampling array according to data quality
*  and sources
      CALL IBGND_SETQ_INT( I_NX, I_NY, %VAL(I_QPTR),
     :                     %VAL(I_BGM_SAMIDX), STATUS )

*  Single sample per image?
      IF ( I_BGM_AREA(1:3) .EQ. 'WHO' ) THEN

*    The quality routine sets up the sampling array for the single sample case

*  Annular sampling?
      ELSE IF ( I_BGM_AREA(1:3) .EQ. 'ANN' ) THEN

*    Compute sample index
        CALL IBGND_SETSAMP_RIDX( I_NX, I_NY, I_BGM_X0, I_BGM_Y0,
     :                  I_BGM_RBIN, %VAL(I_BGM_SAMIDX), STATUS )

      ELSE IF ( I_BGM_AREA(1:3) .EQ. 'BOX' ) THEN
      END IF

      END



      SUBROUTINE IBGND_SETQ_INT( NX, NY, QUAL, IDX, STATUS )
*+
*  Name:
*     IBGND_SETQ_INT

*  Purpose:
*     Set background quality for points inside/outside the current region

*  Language:
*     Starlink Fortran

*  Type of Module:
*     Task subroutine

*  Invocation:
*     CALL IBGND_SETQ_INT( NX, NY, QUAL, IDX, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     NX = INTEGER (given)
*        Number of pixels in X axis
*     NY = INTEGER (given)
*        Number of pixels in Y axis
*     QUAL[NX,NY] = BYTE (given)
*        Mapped input data quality
*     IDX[NX,NY] = INTEGER (returned)
*        -1 for outside region, 0 for bad quality, 1 otherwise
*     STATUS = INTEGER (given and returned)
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
*     ibgnd, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'QUAL_PAR'

*  Global Variables:
      INCLUDE 'IMG_CMN'

*  Arguments Given:
      INTEGER			NX, NY
      BYTE			QUAL(NX,NY)

*  Arguments Returned:
      INTEGER			IDX(NX,NY)

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			BIT_ANDUB
        BYTE			BIT_ANDUB

      EXTERNAL			IMG_INREG
        LOGICAL			IMG_INREG

      EXTERNAL			IMG_INCIRC
        LOGICAL			IMG_INCIRC

*  Local Variables:
      REAL			X, Y, R			! Source position

      INTEGER			I, J			! Loop over image
      INTEGER			I1, I2, J1, J2		! Circle bounding box
      INTEGER			S			! Loop over sources

      LOGICAL			GOOD			! Good input pixel?
      LOGICAL			REGEX			! Region exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Region definition exists?
      REGEX = (I_REG_TYPE .NE. 'WHOLE')

*  If no region defined, and no data quality present
      IF ( (.NOT. REGEX) .AND. .NOT. (I_QOK.AND.I_BAD) ) THEN
        CALL ARR_INIT1I( 1, NX*NY, IDX, STATUS )

*  Otherwise must test each pixel
      ELSE

*    Loop over whole image
        DO J = 1, NY
          DO I = 1, NX

*        Default
            IDX(I,J) = 1
            IF ( REGEX ) THEN
              IF ( .NOT. IMG_INREG(I,J) ) IDX(I,J) = -1
            END IF

*        Still ok?
            IF ( IDX(I,J) .EQ. 1 ) THEN
              IF ( I_QOK .AND. I_BAD ) THEN
                GOOD = (BIT_ANDUB(QUAL(I,J),I_MASK).EQ.QUAL__GOOD)
                IF ( .NOT. GOOD ) IDX(I,J) = 0
              END IF
            END IF

          END DO
        END DO

      END IF

*  Force pixels inside source circles to bad
      DO S = 1, I_BGM_NSRC

*    Get position
        CALL IBGND_GETSRC( S, X, Y, R, STATUS )

*    Convert position and radius to bounding rectangle
        CALL IMG_CIRCTOBOX( X, Y, R, I1, I2, J1, J2, STATUS )

*    Loop over bounding rectangle setting source areas to bad quality
        DO J = J1, J2
          DO I = I1, I2

*        If model pixel is potentially good
            IF ( IDX(I,J) .GT. 0 ) THEN

*          If pixel is in circle then ignore this point for sampling purposes
              IF ( IMG_INCIRC(I,J,X,Y,R) ) THEN
                IDX(I,J) = 0
              END IF

            END IF

          END DO
        END DO

      END DO

      END


      SUBROUTINE IBGND_SETSAMP( AREA, MEAN, STATUS )
*+
*  Name:
*     IBGND_SETSAMP

*  Purpose:
*     Set up sampling space and compute samples

*  Language:
*     Starlink Fortran

*  Type of Module:
*     Task subroutine

*  Invocation:
*     CALL IBGND_SETSAMP( AREA, MEAN, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     AREA = CHARACTER*(*) (given)
*        Method of setting up samples
*     MEAN = CHARACTER*(*) (given)
*        Method of estimating means in areas
*     STATUS = INTEGER (given and returned)
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
*     ibgnd, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'

*  Global Variables:
      INCLUDE 'IMG_CMN'

*  Arguments Given:
      CHARACTER*(*)		AREA, MEAN

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      REAL			MAXR			! Extreme radii
      REAL			XW, YW			! World coords
      REAL			R			! Off-axis angle

      INTEGER			I, J			! Sample data
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Reset existing sampling
      CALL IBGND_RESET( .FALSE., .TRUE., .FALSE., STATUS )

*  Store attributes
      I_BGM_AREA = AREA
      I_BGM_MEAN = MEAN

*  Decide how many samples
      IF ( AREA(1:3) .EQ. 'WHO' ) THEN

*    Trivial case of one sample per image
        I_BGM_NSAMP = 1

      ELSE IF ( AREA(1:3) .EQ. 'ANN' ) THEN

*    Get centre of field
        CALL USI_GET0R( 'X', I_BGM_X0, STATUS )
        CALL USI_GET0R( 'Y', I_BGM_Y0, STATUS )

*    Find extreme radius
        MAXR = VAL__MINR
        DO J = 1, I_NY, I_NY-1
          DO I = 1, I_NX, I_NX-1
            CALL IMG_PIXTOWORLD( REAL(I)-0.5, REAL(J)-0.5, XW, YW,
     :                           STATUS )
            R = SQRT( (XW-I_BGM_X0)**2 + (YW-I_BGM_Y0)**2 )
            MAXR = MAX( MAXR, R )
          END DO
        END DO

*    Number of samples is radial range divided by annulus width
        CALL USI_GET0R( 'RBIN', I_BGM_RBIN, STATUS )
        I_BGM_NSAMP =  INT(MAXR / I_BGM_RBIN) + 1

      ELSE IF ( AREA(1:3) .EQ. 'BOX' ) THEN

        I_BGM_NSAMP = 1

      END IF

*  Allocate space for information needed per sample
      DO I = 1, I__NSAMATT
        CALL DYN_MAPR( 1, I_BGM_NSAMP, I_BGM_SAMPTR(I), STATUS )
      END DO
      CALL ARR_INIT1R( 0.0, I_BGM_NSAMP, %VAL(I_BGM_SAMPTR(1)), STATUS )
      CALL ARR_INIT1R( 0.0, I_BGM_NSAMP, %VAL(I_BGM_SAMPTR(2)), STATUS )
      CALL ARR_INIT1I( 0, I_BGM_NSAMP, %VAL(I_BGM_SAMPTR(3)), STATUS )

*  Compute indices
      CALL IBGND_SETQ( STATUS )

      END


      SUBROUTINE IBGND_SETSAMP_RIDX( NX, NY, X0, Y0, RBIN, IDX,
     :                               STATUS )
*+
*  Name:
*     IBGND_SETSAMP_RIDX

*  Purpose:
*     Set up annular sampling indices

*  Language:
*     Starlink Fortran

*  Type of Module:
*     Task subroutine

*  Invocation:
*     CALL IBGND_SETSAMP_RIDX( NX, NY, X0, Y0, RBIN, IDX, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     NX = INTEGER (given)
*        Number of pixels in X axis
*     NY = INTEGER (given)
*        Number of pixels in Y axis
*     IDX[NX,NY] = INTEGER (given and returned)
*        -1 for outside region, 0 for bad quality, <sample> otherwise
*     STATUS = INTEGER (given and returned)
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
*     ibgnd, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'

*  Global Variables:
      INCLUDE 'IMG_CMN'

*  Arguments Given:
      INTEGER			NX, NY
      REAL			X0, Y0, RBIN

*  Arguments Returned:
      INTEGER			IDX(NX,NY)

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      REAL			XW, YW			! World coords
      REAL			Y2			! Y offset squared
      REAL			R			! Off-axis angle

      INTEGER			I, J			! Sample data
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over image
      DO J = 1, NY
        YW = I_YBASE + (J-1)*I_YSCALE
        Y2 = (YW-Y0)**2
        DO I = 1, NX
          IF ( IDX(I,J) .GT. 0 ) THEN
            XW = I_XBASE + (I-1)*I_XSCALE
            R = SQRT( (XW-X0)**2 + Y2 ) / RBIN
            IDX(I,J) = INT(R) + 1
          END IF
        END DO
      END DO

      END



      SUBROUTINE IBGND_RECALC( SAMP, SURF, STATUS )
*+
*  Name:
*     IBGND_RECALC

*  Purpose:
*     Recalculate samples and surface

*  Language:
*     Starlink Fortran

*  Type of Module:
*     Task subroutine

*  Invocation:
*     CALL IBGND_SETSAMP( SAMP, SURF, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     SAMP = LOGICAL (given)
*        Recompute samples?
*     SURF = LOGICAL (given)
*        Recompute surface?
*     STATUS = INTEGER (given and returned)
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
*     ibgnd, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'QUAL_PAR'

*  Global Variables:
      INCLUDE 'IMG_CMN'

*  Arguments Given:
      LOGICAL			SAMP, SURF

*  Status:
      INTEGER			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Compute the samples
      IF ( SAMP ) THEN

        CALL IBGND_SAMP_COMP( I_NX, I_NY, %VAL(I_DPTR),
     :                      %VAL(I_BGM_SAMIDX), %VAL(I_BGM_DPTR),
     :                      I_BGM_NSAMP, %VAL(I_BGM_SAMPTR(1)),
     :                      %VAL(I_BGM_SAMPTR(2)),
     :                      %VAL(I_BGM_SAMPTR(3)),
     :                      STATUS )
      END IF

*  Compute the background surface
      IF ( SURF ) THEN
        CALL IBGND_SURF_COMP( I_NX, I_NY, %VAL(I_DPTR),
     :                      %VAL(I_BGM_SAMIDX), I_BGM_NSAMP,
     :                      %VAL(I_BGM_SAMPTR(1)),
     :                      %VAL(I_BGM_SAMPTR(2)),
     :                      %VAL(I_BGM_SAMPTR(3)),
     :                      %VAL(I_BGM_DPTR),
     :                      STATUS )

*    Bgnd derived image is displayed?
        IF ( (I_BGM_DISIM .EQ. 1) .OR. (I_BGM_DISIM.EQ.-1) ) THEN

*      Update it
          CALL IBGND_DISP_SURF( I_BGM_DISIM, STATUS )

        END IF

      END IF

      END


      SUBROUTINE IBGND_SAMP_COMP( NX, NY, DATA, IDX, WRK,
     :                      NSAMP, SAMM, SAMEM, SAMNP, STATUS )
*+
*  Name:
*     IBGND_SAMP_COMP

*  Purpose:
*     Compute values of samples

*  Language:
*     Starlink Fortran

*  Type of Module:
*     Task subroutine

*  Invocation:
*     CALL IBGND_SAMP_COMP( NX, NY, DATA, IDX, WRK, NSAMP, SAMM, SAMEM,
*                           SAMNP, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     NX = INTEGER (given)
*        Number of pixels in X axis
*     NY = INTEGER (given)
*        Number of pixels in Y axis
*     DATA[NX,NY] = REAL (given)
*        The image data
*     IDX[NX,NY] = BYTE (given)
*        Values > 0 for data to be included in sample
*     WRK[NX*NY] = REAL (given)
*        Workspace
*     NSAMP = INTEGER (given)
*        Number of samples
*     SAMM[NSAMP] = REAL (returned)
*        Sample means
*     SAMEM[NSAMP] = REAL (returned)
*        Sample mean errors
*     SAMNP[NSAMP] = INTEGER (returned)
*        Sample pixel counts
*     STATUS = INTEGER (given and returned)
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
*     ibgnd, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'QUAL_PAR'

*  Global Variables:
      INCLUDE 'IMG_CMN'

*  Arguments Given:
      INTEGER			NX, NY, NSAMP
      REAL			DATA(NX,NY),WRK(*)
      INTEGER			IDX(NX,NY)

*  Arguments Given and Returned:
      REAL			SAMM(*), SAMEM(*)
      INTEGER			SAMNP(*)

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      REAL			MEAN			!
      REAL			D, DMEAN, WTSUM, WTSUM2	!

      INTEGER			I, J			! Loop over sample data
      INTEGER			S			! Loop over samples

      BYTE			SQ			! Sample quality
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Switch on mean method
      IF ( I_BGM_MEAN .EQ. 'MEAN' ) THEN

*    Set up accumulators, 3 per sample. First is for the mean, the second
*    for the error on the mean. Third is pixel count per sample
        DO S = 1, I_BGM_NSAMP
          WRK(S) = 0.0
          SAMEM(S) = 0.0
          SAMNP(S) = 0
        END DO

*    Loop over all points
        DO J = 1, NY
          DO I = 1, NX

*        Which sample box does this point belong to
            S = IDX(I,J)

*        Accumulate sum
            WRK(S) = WRK(S) + DATA(I,J)

*        Accumulate standard deviation. Assumed mean from existing sample mean
            SAMEM(S) = SAMEM(S) + (DATA(I,J)-SAMM(S))**2

*        Count pixel
            SAMNP(S) = SAMNP(S) + 1

          END DO
        END DO

*    Compute means from accumulators
        DO S = 1, I_BGM_NSAMP

*      Any points in sample?
          IF ( SAMNP(S) .GT. 0 ) THEN

            MEAN = WRK(S)
            WTSUM = REAL(SAMNP(S))
            WTSUM2 = WTSUM

*        Find values from sums
            MEAN = MEAN / WTSUM
            D = WTSUM - WTSUM2/WTSUM

*        Correct standard deviation for assumed mean
            IF ( SAMNP(S) .GT. 1 ) THEN
              DMEAN = SAMM(S) - MEAN
              SAMEM(S) = SAMEM(S) - WTSUM*DMEAN**2
              SAMEM(S) = SQRT(SAMEM(S)/D) / SQRT(REAL(SAMNP(S)))
            ELSE
              SAMEM(S) = -1.0
            END IF

*        Store sample mean
            SAMM(S) = MEAN

          ELSE
            SAMM(S) = 0.0

          END IF

        END DO

      END IF

*  Export details of means to environment
      S = 1
      IF ( I_BGM_AREA(1:3) .EQ. 'WHO' ) THEN

*    Simply report mean
        CALL MSG_SETR( 'MEAN', SAMM(1) )
        CALL MSG_SETR( 'EMEAN', SAMEM(1) )
        CALL MSG_SETI( 'N', SAMNP(1) )
        CALL MSG_PRNT( '  Mean value in image is ^MEAN +- '/
     :                               /'^EMEAN (^N points)' )

*  Annulus mode?
      ELSE IF ( I_BGM_AREA(1:3) .EQ. 'ANN' ) THEN

*    Construct radial plot
        I_N_1D = I_BGM_NSAMP
        CALL IMG_GET1D( I_BGM_NSAMP, STATUS )

*    Set labels
        I_XLABEL_1D = 'Radius'
        I_XUNITS_1D = I_XYUNITS
        I_LABEL_1D = 'Surface brightness'
        I_TITLE_1D = 'Radial distribution'

*    Set data units to be per pixel
        I_UNITS_1D = I_UNITS(1:CHR_LEN(I_UNITS)) // ' / pixel'

*    Set axis values
        I_XBASE_1D = I_BGM_RBIN / 2.0
        I_XSCALE_1D = I_BGM_RBIN
        I_XWID_1D = I_BGM_RBIN

*    Copy sample data to 1-D data, errors to variance
        CALL ARR_COP1R( I_BGM_NSAMP, SAMM, %VAL(I_DPTR_1D), STATUS )
        CALL ARR_COP1R( I_BGM_NSAMP, SAMEM, %VAL(I_VPTR_1D), STATUS )

*    Convert error to variance
        CALL ARR_SQR1R( %VAL(I_VPTR_1D), I_BGM_NSAMP, STATUS )

*    1-D quality is good if more than 1 point per sample, bad otherwise
        DO S = 1, I_BGM_NSAMP
          IF ( SAMNP(S) .GT. 1 ) THEN
            SQ = QUAL__GOOD
          ELSE
            SQ = QUAL__BAD
          END IF
          CALL ARR_SELEM1B( I_QPTR_1D, I_BGM_NSAMP, S, SQ, STATUS )

        END DO
        IF ( .NOT. I_QOK ) I_MASK = QUAL__MASK

*    Set default axis ranges
        I_X1_1D = 0.0
        I_X2_1D = ABS(I_XSCALE)*REAL(I_N_1D)
        CALL ARR_RANG1RQ( I_N_1D, %VAL(I_DPTR_1D), %VAL(I_QPTR_1D),
     :                    QUAL__MASK, I_Y1_1D, I_Y2_1D, STATUS )
        I_Y2_1D = I_Y2_1D + 0.1*(I_Y2_1D-I_Y1_1D)

*    Set default plotting style
        CALL IMG_1DGCB( STATUS )
        CALL GCB_SETDEF( STATUS )
        CALL GCB_SETL( 'ERR_FLAG', .TRUE., STATUS )
        CALL GCB_SETL( 'STEP_FLAG', .FALSE., STATUS )
        CALL GCB_SETL( 'POLY_FLAG', .FALSE., STATUS )
        CALL GCB_SETL( 'POINT_FLAG', .FALSE., STATUS )

*    Go to new zone and plot
        CALL GDV_CLEAR( STATUS )
        CALL IMG_PLOT( STATUS )

*    Flag current plotting status
        I_DISP = .FALSE.
        I_DISP_1D = .TRUE.
        I_CLEAR = .FALSE.
        I_BGM_DISIM = 0

*    Cache the GCB
        CALL GCB_CACHE( I_CACHE_1D, STATUS )

      ELSE IF ( I_BGM_AREA .EQ. 'BOX' ) THEN

      END IF

      END



      SUBROUTINE IBGND_SURF_COMP( NX, NY, DATA, IDX, NSAMP, SAMM, SAMEM,
     :                            SAMNP, BGMOD, STATUS )
*+
*  Name:
*     IBGND_SURF_COMP

*  Purpose:
*     Compute background surface given samples

*  Language:
*     Starlink Fortran

*  Type of Module:
*     Task subroutine

*  Invocation:
*     CALL IBGND_SURF_COMP( )

*  Description:
*     {routine_description}

*  Arguments:
*     NX = INTEGER (given)
*        Number of pixels in X axis
*     NY = INTEGER (given)
*        Number of pixels in Y axis
*     DATA[NX,NY] = REAL (given)
*        The image data
*     IDX[NX,NY] = INTEGER (given)
*        Sample index
*     NSAMP = INTEGER (given)
*        Number of samples
*     SAMM[NSAMP] = REAL (given)
*        Sample means
*     SAMEM[NSAMP] = REAL (given)
*        Sample mean errors
*     SAMNP[NSAMP] = INTEGER (given)
*        Sample pixel counts
*     BGMOD[NX,NY] = REAL (returned)
*        The background model
*     STATUS = INTEGER (given and returned)
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
*     ibgnd, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'

*  Global Variables:
      INCLUDE 'IMG_CMN'

*  Arguments Given:
      INTEGER			NX, NY, NSAMP
      REAL			SAMM(*), SAMEM(*), DATA(NX,NY)
      INTEGER			SAMNP(*),IDX(NX,NY)

*  Arguments Returned:
      REAL			BGMOD(NX,NY)

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      INTEGER			F__CONS
        PARAMETER		( F__CONS = 1 )

      INTEGER			F__NONE
        PARAMETER		( F__NONE = 2 )

      INTEGER			F__SPLINE
        PARAMETER		( F__SPLINE = 3 )

      INTEGER			F__POLY
        PARAMETER		( F__POLY = 4 )

      INTEGER			MAXPT
        PARAMETER		( MAXPT = 512 )

*  Local Variables:
      DOUBLE PRECISION		PFX(MAXPT)
      DOUBLE PRECISION	       	PFY(MAXPT)
      DOUBLE PRECISION		YP, YFIT

      REAL			MEAN			! Mean sample value
      REAL			MINFR, MAXFR		! Extreme residuals
      REAL			FR, RMSFR		! RMS frac residual
      REAL			WRKC(MAXPT*2+1)	! Poly workspace

      REAL			XW1, XW2, YW1, YW2	! Worst positions
      REAL			XW, YW, R, Y2		! Pixel radius bits

      INTEGER			FMODE			! Fitting mode
      INTEGER			I, J			! Loop over image
      INTEGER			IR			! Radial bin number
      INTEGER			ITEMID			! GUI noticeboard item
      INTEGER			MAXFR_X, MAXFR_Y	! Max position
      INTEGER			MINFR_X, MINFR_Y	! Min position
      INTEGER			NFR			! # residuals
      INTEGER			NGS			! # good samples
      INTEGER			PFAIL			! PDA status code
      INTEGER			S			! Loop over samples
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert character fit to code
      IF ( I_BGM_FIT .EQ. 'NONE' ) THEN
        FMODE = F__NONE
      ELSE IF ( I_BGM_FIT .EQ. 'CONS' ) THEN
        FMODE = F__CONS
      ELSE IF ( I_BGM_FIT .EQ. 'POLY' ) THEN
        FMODE = F__POLY
      ELSE IF ( I_BGM_FIT .EQ. 'SPLINE' ) THEN
        FMODE = F__SPLINE
      END IF

*  Switch on sample mode
      IF ( I_BGM_AREA(1:3) .EQ. 'WHO' ) THEN

*    Loop over image
        DO J = 1, NY
          DO I = 1, NX

*      Outside area?
            IF ( IDX(I,J) .LT. 0 ) THEN
              BGMOD(I,J) = 0.0
            ELSE IF ( IDX(I,J) .EQ. 0 ) THEN
              IF ( FMODE .EQ. F__NONE ) THEN
                BGMOD(I,J) = 0.0
              ELSE
                BGMOD(I,J) = SAMM(1)
              END IF
            ELSE
              BGMOD(I,J) = SAMM(1)
            END IF

          END DO
        END DO

*    Export mean
        IF ( I_GUI ) THEN
          CALL NBS_FIND_ITEM( I_NBID, 'BG_MEAN', ITEMID, STATUS )
          CALL NBS_PUT_VALUE( ITEMID, 0, VAL__NBR, SAMM(1), STATUS )
        END IF

*  Annular sampling?
      ELSE IF ( I_BGM_AREA(1:3) .EQ. 'ANN' ) THEN

*    Simple mean?
        IF ( FMODE .EQ. F__CONS ) THEN

          MEAN = 0.0
          NGS = 0
          DO S = 1, I_BGM_NSAMP
            IF ( SAMNP(S) .GT. 1 ) THEN
              MEAN = MEAN + SAMM(S)
              NGS = NGS + 1
            END IF
          END DO
          IF ( NGS .GT. 0 ) THEN
            MEAN = MEAN / REAL(NGS)
          ELSE
            MEAN = 0.0
          END IF

*    Polynomial fit
        ELSE IF ( FMODE .EQ. F__POLY ) THEN

*      Fit coefficients
          NGS = 0
          DO I = 1, I_BGM_NSAMP
            IF ( SAMNP(I) .GT. 1 ) THEN
              NGS = NGS + 1
              PFX(NGS) = (REAL(I)-0.5) * I_BGM_RBIN
              PFY(NGS) = SAMM(I)
            END IF
          END DO

*      Trap single good sample case
          IF ( NGS .LT. 2 ) THEN
            FMODE = F__CONS
            MEAN = PFY(1)

*      Otherwise compute coefficients
          ELSE

*        Compute coefficients
            PFAIL = 0
            CALL PDA_DPLINT( NGS, PFX, PFY, WRKC, PFAIL )

          END IF

*    Spline fit
        ELSE IF ( FMODE .EQ. F__SPLINE ) THEN

        END IF

*    Loop over image
        DO J = 1, NY
          YW = I_YBASE + (J-1)*I_YSCALE
          Y2 = (YW - I_BGM_Y0)**2
          DO I = 1, NX

*      Outside area?
            IF ( IDX(I,J) .LT. 0 ) THEN
              BGMOD(I,J) = 0.0
            ELSE

*          Simple average
              IF ( FMODE .EQ. F__CONS ) THEN
                BGMOD(I,J) = MEAN

*          No fitting
              ELSE IF ( FMODE .EQ. F__NONE ) THEN
                IF ( IDX(I,J) .GT. 0 ) THEN
                  BGMOD(I,J) = SAMM(IDX(I,J))
                ELSE
                  BGMOD(I,J) = 0.0
                END IF

*          Evaluate polynomial at R
              ELSE IF ( FMODE .EQ. F__POLY ) THEN

*            Find exact distance to this pixel
                XW = I_XBASE + (I-1)*I_XSCALE
                XW = XW - I_BGM_X0
                R = SQRT( XW*XW + Y2 ) / I_BGM_RBIN

*            Evaluate polynomial
                CALL PDA_DPOLVL( 0, DBLE(R), YFIT, YP, NGS, PFX,
     :                           WRKC, PFAIL )

*            Store value
                BGMOD(I,J) = YFIT

*          Evaluate spline at R
              ELSE IF ( FMODE .EQ. F__SPLINE ) THEN

*            Find exact distance to this pixel
                XW = I_XBASE + (I-1)*I_XSCALE
                XW = XW - I_BGM_X0
                R = SQRT( XW*XW + Y2 ) / I_BGM_RBIN
                IR = INT(R) + 1

              END IF

            END IF

          END DO
        END DO

      END IF

*  Accumulate max and RMS fractional residual
      MINFR = VAL__MAXR
      MAXFR = VAL__MINR
      RMSFR = 0.0
      NFR = 0
      DO J = 1, NY
        DO I = 1, NX

*      Inside area?
          IF ( IDX(I,J) .GT. 0 ) THEN

            FR = DATA(I,J) - BGMOD(I,J)
            IF ( BGMOD(I,J) .NE. 0.0 ) THEN
              NFR = NFR + 1
              FR = FR / ABS(BGMOD(I,J))
              RMSFR = RMSFR + FR**2
              IF ( FR .GT. MAXFR ) THEN
                MAXFR = FR
                MAXFR_X = I
                MAXFR_Y = J
              END IF
              IF ( FR .LT. MINFR ) THEN
                MINFR = FR
                MINFR_X = I
                MINFR_Y = J
              END IF
            END IF
          END IF

        END DO
      END DO

*  Compute RMS fractional residual
      IF ( NFR .GT. 1 ) THEN
        RMSFR = SQRT( RMSFR  / REAL(NFR))
        IF ( I_GUI ) THEN
          CALL NBS_FIND_ITEM( I_NBID, 'BG_RMS', ITEMID, STATUS )
          CALL NBS_PUT_VALUE( ITEMID, 0, VAL__NBR, RMSFR, STATUS )
        ELSE
          CALL MSG_SETR( 'FR' , RMSFR )
          CALL MSG_PRNT( '  RMS fractional residual ^FR' )
        END IF

      END IF

*  Convert worst positions to world coordinates
      CALL IMG_PIXTOWORLD( REAL(MAXFR_X)-0.5, REAL(MAXFR_Y)-0.5,
     :                     XW1, YW1, STATUS )
      CALL IMG_PIXTOWORLD( REAL(MINFR_X)-0.5, REAL(MINFR_Y)-0.5,
     :                     XW2, YW2, STATUS )

*  Convert worst residuals to percentages
      MAXFR = MAXFR*100.0
      MINFR = MINFR*100.0

*  Write to environment
      IF ( I_GUI ) THEN

*    Most +ve deviation
        CALL NBS_FIND_ITEM( I_NBID, 'BG_MAXR', ITEMID, STATUS )
        CALL NBS_PUT_VALUE( ITEMID, 0, VAL__NBR, MAXFR, STATUS )
        CALL NBS_FIND_ITEM( I_NBID, 'BG_MAXX', ITEMID, STATUS )
        CALL NBS_PUT_VALUE( ITEMID, 0, VAL__NBR, XW1, STATUS )
        CALL NBS_FIND_ITEM( I_NBID, 'BG_MAXY', ITEMID, STATUS )
        CALL NBS_PUT_VALUE( ITEMID, 0, VAL__NBR, YW1, STATUS )

*    Most -ve deviation
        CALL NBS_FIND_ITEM( I_NBID, 'BG_MINR', ITEMID, STATUS )
        CALL NBS_PUT_VALUE( ITEMID, 0, VAL__NBR, MAXFR, STATUS )
        CALL NBS_FIND_ITEM( I_NBID, 'BG_MINX', ITEMID, STATUS )
        CALL NBS_PUT_VALUE( ITEMID, 0, VAL__NBR, XW2, STATUS )
        CALL NBS_FIND_ITEM( I_NBID, 'BG_MINY', ITEMID, STATUS )
        CALL NBS_PUT_VALUE( ITEMID, 0, VAL__NBR, YW2, STATUS )

      ELSE
        CALL MSG_SETR( 'R', MAXFR )
        CALL MSG_SETR( 'X', XW1 )
        CALL MSG_SETR( 'Y', YW1 )
        CALL MSG_PRNT( '  Worst +ve deviation from model is ^R% at '/
     :               /'(^X,^Y)' )
        CALL MSG_SETR( 'R', MINFR )
        CALL MSG_SETR( 'X', XW2 )
        CALL MSG_SETR( 'Y', YW2 )
        CALL MSG_PRNT( '  Worst -ve deviation from model is ^R% at '/
     :               /' (^X,^Y)' )
      END IF

      END


      SUBROUTINE IBGND_RESET( SRC, SAMP, FIT, STATUS )
*+
*  Name:
*     IBGND_RESET

*  Purpose:
*     Reset background modeller internal state

*  Language:
*     Starlink Fortran

*  Type of Module:
*     Task subroutine

*  Invocation:
*     CALL IBGND_RESET( SRC, SAMP, FIT, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     SRC = LOGICAL (given)
*        Reset source database info?
*     SAMP = LOGICAL (given)
*        Reset sampling info?
*     FIT = LOGICAL (given)
*        Reset surface fitting info?
*     STATUS = INTEGER (given and returned)
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
*     ibgnd, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'IMG_CMN'

*  Arguments Given:
      LOGICAL			SRC, SAMP, FIT

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over src arrays
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Destroy source database?
      IF ( SRC ) THEN
        I_BGM_NSRC = 0
        DO I = 1, I__NSRCATT
          CALL DYN_MAPR( 1, I__MXBGSRC, I_BGM_SRCPTR(I), STATUS )
        END DO
      END IF

*  Destroy sampling data if defined
      IF ( SAMP ) THEN
        I_BGM_AREA = 'NONE'
        I_BGM_NSAMP = 0
        DO I = 1, I__NSAMATT
          IF ( I_BGM_ON .AND. (I_BGM_SAMPTR(I) .NE. 0) ) THEN
            CALL DYN_UNMAP( I_BGM_SAMPTR(I), STATUS )
          END IF
          I_BGM_SAMPTR(I) = 0
        END DO
      END IF

*  Fitting
      I_BGM_FIT = 'CONS'

      END


*+
      SUBROUTINE IBGND_HELP()
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
*    Global variables :
*    Import :
*    Export :
*    Status :
*    Function declarations :
*    Local constants :
      INTEGER MLINE
      PARAMETER (MLINE=18)
*    Local variables :
      CHARACTER*79 MTEXT(MLINE)
     :/' Source commands:',' ',
     : '  ADDSRC  - Add a source to the source database',
     : '  DELSRC  - Delete a source from the source database',
     : '  MARKSRC - Mark source positions on current display',
     : '  SAVESRC - Save source list',
     : ' ', ' Sampling commands:',' ',
     : '  SETSAMP - Set sampling mode (WHOLE, ANNULUS or BOX)',
     : '  SETFIT  - Method of sample interpolation',
     : '  SAVE    - Save current model to a file',
     : ' ', ' Plotting commands:',' ',
     : '  DDISP   - Display source data',
     : '  MDISP   - Display background model image',
     : '  RDISP   - Display data - background model residuals'/
      INTEGER ILINE
*-

      CALL MSG_BLNK()
      CALL MSG_PRNT('Available modes are:')
      CALL MSG_BLNK()
      DO ILINE=1,MLINE
        CALL MSG_PRNT(MTEXT(ILINE))
      ENDDO

      CALL MSG_BLNK()

      END
