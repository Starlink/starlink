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
*     14 Aug 1995 1.8-1 (DJA):
*        Improved timing report and added links option.
*      6 Sep 1995 2.0-0 (DJA):
*        Display selection info
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
      INTEGER			IC_MIS
        PARAMETER ( IC_MIS = 2 )
      INTEGER			IC_TIM
        PARAMETER ( IC_TIM = 4 )
      INTEGER			IC_LNK
        PARAMETER ( IC_LNK = 8 )
      INTEGER			IC_SEL
        PARAMETER ( IC_SEL = 16 )
      INTEGER			IC_ALL
        PARAMETER ( IC_ALL = IC_WCS+IC_MIS+IC_TIM+IC_LNK+IC_SEL )

      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'ASHOW Version 2.1-0b' )

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
      CALL USI_ASSOC( 'INP', '*', 'READ', IFID, STATUS )

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

          ELSE IF ( CHR_INSET( ITEM, 'MISS' ) ) THEN
            ITEMC = ITEMC + IC_MIS

          ELSE IF ( CHR_INSET( ITEM, 'TIME' ) ) THEN
            ITEMC = ITEMC + IC_TIM

          ELSE IF ( CHR_INSET( ITEM, 'LINKS' ) ) THEN
            ITEMC = ITEMC + IC_LNK

          ELSE IF ( CHR_INSET( ITEM, 'SEL' ) ) THEN
            ITEMC = ITEMC + IC_SEL

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

*    Mission strings?
        IF ( AND( ITEMC, IC_MIS ) .NE. 0 ) THEN
          CALL ASHOW_MIS( IFID, OCH, STATUS )
        END IF

*    Timing
        IF ( AND( ITEMC, IC_TIM ) .NE. 0 ) THEN
          CALL ASHOW_TIM( IFID, OCH, STATUS )
        END IF

*    World coordinates?
        IF ( AND( ITEMC, IC_WCS ) .NE. 0 ) THEN
          CALL ASHOW_WCS( IFID, OCH, STATUS )
        END IF

*    File links
        IF ( AND( ITEMC, IC_LNK ) .NE. 0 ) THEN
          CALL ASHOW_LNK( IFID, OCH, STATUS )
        END IF

*    Dataset selection
        IF ( AND( ITEMC, IC_SEL ) .NE. 0 ) THEN
          CALL ASHOW_SEL( IFID, OCH, STATUS )
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
*     CALL ASHOW_WCS( IFID, OCH, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     IFID = INTEGER (given)
*        Input dataset identifier
*     OCH = INTEGER (given)
*        Output channel identifier
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

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

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
      INCLUDE 'MATH_PAR'

*  Arguments Given:
      INTEGER			IFID			! Input dataset id
      INTEGER			OCH			! Output channel

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*1		EFORM			! Form of epoch
      CHARACTER*20		RAS, DECS		! RA,DEC in strings
      CHARACTER*3		SYS			! Coord system name

      DOUBLE PRECISION		EPOCH			! Equinox & epoch
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
      CALL ASHOW_OB( SYSID, 'NAME', 'C', 'System name', ' ',
     :                'np', OCH, STATUS )
      CALL ASHOW_OB( SYSID, 'EQUINOX', 'D', 'Equinox', ' ',
     :                'np', OCH, STATUS )
      IF ( SYSID .NE. ADI__NULLID ) THEN
        CALL ADI_CGET0C( SYSID, 'EFORM', EFORM, STATUS )
        CALL ADI_CGET0D( SYSID, 'EPOCH', EPOCH, STATUS )
        CALL MSG_SETC( 'EF', EFORM )
        CALL MSG_SETD( 'EP', EPOCH )
        CALL AIO_IWRITE( OCH, 6, 'Epoch                : ^EF^EP',
     :                   STATUS )
      ELSE
        CALL AIO_IWRITE( OCH, 6, '* not present *', STATUS )
      END IF
      CALL AIO_BLNK( OCH, STATUS )

*  The projection
      CALL AIO_IWRITE( OCH, 4, 'Coordinate reference & projection :',
     :                 STATUS )
      CALL AIO_BLNK( OCH, STATUS )
      IF ( SYSID .NE. ADI__NULLID ) THEN

*    Name of system
        CALL ADI_CGET0C( SYSID, 'NAME', SYS, STATUS )

*    Name of projection
        CALL ASHOW_OB( PRJID, 'NAME', 'C', 'Projection name', ' ',
     :                'np', OCH, STATUS )

*    Axis origin
        CALL ADI_CGET1D( PRJID, 'SPOINT', 2, PNT, NVAL, STATUS )
        IF ( SYS(1:2) .EQ. 'FK' ) THEN
          CALL STR_DRADTOC( PNT(1)*MATH__DDTOR, 'HHh MMm SS.SSs', RAS,
     :                      STATUS )
          CALL STR_DRADTOC( PNT(2)*MATH__DDTOR, 'SDDd MMm SS.Ss',DECS,
     :                      STATUS )
          CALL MSG_SETC( 'A', RAS )
          CALL MSG_SETC( 'B', DECS )
        ELSE
          CALL MSG_SETD( 'A', PNT(1) )
          CALL MSG_SETD( 'B', PNT(2) )
        END IF
        CALL AIO_IWRITE( OCH, 6, 'Axis origin          : ^A ^B',
     :                   STATUS )

*    Centre of f.o.v
        CALL ADI_THERE( PRJID, 'NPOINT', THERE, STATUS )
        IF ( THERE ) THEN
          CALL ADI_CGET1D( PRJID, 'NPOINT', 2, PNT, NVAL, STATUS )
          IF ( SYS(1:2) .EQ. 'FK' ) THEN
            CALL STR_DRADTOC( PNT(1)*MATH__DDTOR, 'HHh MMm SS.SSs', RAS,
     :                      STATUS )
            CALL STR_DRADTOC( PNT(2)*MATH__DDTOR, 'SDDd MMm SS.Ss',
     :                      DECS, STATUS )
            CALL MSG_SETC( 'A', RAS )
            CALL MSG_SETC( 'B', DECS )
          ELSE
            CALL MSG_SETD( 'A', PNT(1) )
            CALL MSG_SETD( 'B', PNT(2) )
          END IF
          CALL AIO_IWRITE( OCH, 6, 'FOV position         : ^A ^B',
     :                     STATUS )
        END IF

      ELSE
        CALL AIO_IWRITE( OCH, 6, '* not present *', STATUS )
      END IF
      CALL AIO_BLNK( OCH, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ASHOW_WCS', STATUS )

      END


      SUBROUTINE ASHOW_MIS( IFID, OCH, STATUS )
*+
*  Name:
*     ASHOW_MIS

*  Purpose:
*     Display detector configuration info

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ASHOW_MIS( IFID, OCH, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     IFID = INTEGER (given)
*        Input dataset identifier
*     OCH = INTEGER (given)
*        Output channel identifier
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

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

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
      INTEGER			DETID			! DCI info
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Load mission strings
      CALL DCI_GETID( IFID, DETID, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
      END IF

*  Write heading
      CALL AIO_BLNK( OCH, STATUS )
      CALL AIO_IWRITE( OCH, 2, 'Mission Description Strings :', STATUS )
      CALL AIO_BLNK( OCH, STATUS )

*  The observation details
      CALL AIO_IWRITE( OCH, 4, 'Observation Details :', STATUS )
      CALL AIO_BLNK( OCH, STATUS )
      CALL ASHOW_OB( DETID, 'Observer', 'C', 'Observer', ' ',
     :                'ig', OCH, STATUS )
      CALL ASHOW_OB( DETID, 'Target', 'C', 'Target', ' ',
     :                'ig', OCH, STATUS )
      CALL AIO_BLNK( OCH, STATUS )

*  The hardware
      CALL AIO_IWRITE( OCH, 4, 'Instrument Configuration :', STATUS )
      CALL AIO_BLNK( OCH, STATUS )
      CALL ASHOW_OB( DETID, 'Mission', 'C', 'Mission', ' ',
     :                'ig', OCH, STATUS )
      CALL ASHOW_OB( DETID, 'Instrument', 'C', 'Instrument', ' ',
     :                'ig', OCH, STATUS )
      CALL ASHOW_OB( DETID, 'Detector', 'C', 'Detector', ' ',
     :                'ig', OCH, STATUS )
      CALL ASHOW_OB( DETID, 'Filter', 'C', 'Filter', ' ',
     :                'ig', OCH, STATUS )
      CALL ASHOW_OB( DETID, 'DataMode', 'C', 'Data mode', ' ',
     :                'ig', OCH, STATUS )

      CALL AIO_BLNK( OCH, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ASHOW_MIS', STATUS )

      END



      SUBROUTINE ASHOW_TIM( IFID, OCH, STATUS )
*+
*  Name:
*     ASHOW_TIM

*  Purpose:
*     Display timing info

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ASHOW_TIM( IFID, OCH, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     IFID = INTEGER (given)
*        Input dataset identifier
*     OCH = INTEGER (given)
*        Output channel identifier
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

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

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
      CHARACTER*8		DSTR, TSTR		! Date and time

      DOUBLE PRECISION		MJD			! MJD at start

      INTEGER			TIMID			! TCI info
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Load timing info
      CALL TCI_GETID( IFID, TIMID, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
      END IF

*  Write heading
      CALL AIO_BLNK( OCH, STATUS )
      CALL AIO_IWRITE( OCH, 2, 'Timing Information :', STATUS )
      CALL AIO_BLNK( OCH, STATUS )

*  The observation details
      CALL AIO_IWRITE( OCH, 4, 'Observation Dates & Exposure Times :',
     :                  STATUS )
      CALL AIO_BLNK( OCH, STATUS )

      CALL ADI_CGET0D( TIMID, 'MJDObs', MJD, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
        CALL TCI_MJD2DT( MJD, DSTR, TSTR, STATUS )
        CALL ASHOW_VAL( DSTR//' '//TSTR, 'Date/time at start', ' ',
     :                  OCH, STATUS )
      ELSE
        CALL ERR_ANNUL( STATUS )
      END IF
      CALL ASHOW_OB( TIMID, 'MJDObs', 'D', 'MJD at start', ' ',
     :                'np', OCH, STATUS )
      CALL ASHOW_OB( TIMID, 'TAIObs', 'C', 'TAI at start', 'days',
     :                'np', OCH, STATUS )
      CALL ASHOW_OB( TIMID, 'ObsLength', 'R', 'Obs. length', 'seconds',
     :                'np', OCH, STATUS )
      CALL ASHOW_OB( TIMID, 'Exposure', 'R', 'Exposure', 'seconds',
     :                'np', OCH, STATUS )
      CALL ASHOW_OB( TIMID, 'EffExposure', 'R', 'Effective exposure',
     :                'seconds', 'ig', OCH, STATUS )
      CALL AIO_BLNK( OCH, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ASHOW_TIM', STATUS )

      END



      SUBROUTINE ASHOW_LNK( IFID, OCH, STATUS )
*+
*  Name:
*     ASHOW_LNK

*  Purpose:
*     Display file links

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ASHOW_TIM( IFID, OCH, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     IFID = INTEGER (given)
*        Input dataset identifier
*     OCH = INTEGER (given)
*        Output channel identifier
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

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

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

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Constants:
      INTEGER			NLINK
        PARAMETER		( NLINK = 2 )

*  Local Variables:
      CHARACTER*4		LNAMS(NLINK)		! Link names
      CHARACTER*40		LDESC(NLINK)		! Link descriptions
      CHARACTER*132		LINK			! Link value

      INTEGER			I			! Loop over links
      INTEGER			L			! Length of an LNAMS

      LOGICAL			OK			! Link is present

*  Local Data:
      DATA			LNAMS/'BGND', 'VIGN'/
      DATA			LDESC/'Background dataset',
     :                                'Vignetting factors'/
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write heading
      CALL AIO_BLNK( OCH, STATUS )
      CALL AIO_IWRITE( OCH, 2, 'File link information :', STATUS )
      CALL AIO_BLNK( OCH, STATUS )

*  Write each link
      DO I = 1, NLINK

*    Link is present
        L = CHR_LEN( LNAMS(I) )
        CALL FRI_CHK( IFID, LNAMS(I)(:L), OK, STATUS )
        IF ( OK ) THEN
          CALL FRI_GETC( IFID, LNAMS(I)(:L),  LINK, STATUS )
          CALL ASHOW_VAL( LINK, LDESC(I), ' ', OCH, STATUS )
        ELSE
          CALL ASHOW_VAL( '* not set *', LDESC(I), ' ', OCH, STATUS )
        END IF

      END DO


*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ASHOW_LNK', STATUS )

      END


      SUBROUTINE ASHOW_SEL( IFID, OCH, STATUS )
*+
*  Name:
*     ASHOW_SEL

*  Purpose:
*     Display data selection

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ASHOW_SEL( IFID, OCH, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     IFID = INTEGER (given)
*        Input dataset identifier
*     OCH = INTEGER (given)
*        Output channel identifier
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

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

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

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*132		ARDIN			! ARD text
      CHARACTER*40		CREATOR			! Selection author
      CHARACTER*20		SNAME			! Selection name
      CHARACTER*20		VARIANT			! Selector variant

      REAL			START, STOP		! Range pair

      INTEGER			BPTR, EPTR		! Mapped range pairs
      INTEGER			GRPID			! ARD identifier
      INTEGER			I			! Loop over selections
      INTEGER			ICMP			! Loop over selectors
      INTEGER			ITXT			! Loop over ARD text
      INTEGER			L			! Length of ARDIN used
      INTEGER			NCMP			! # selectors
      INTEGER			NREC			! # selection records
      INTEGER			SELID			! Selection identifier
      INTEGER			SIID			! Selector identifier
      INTEGER			SID			! Selectors structure
      INTEGER			SIZE			! Amount of sel data
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write heading
      CALL AIO_BLNK( OCH, STATUS )
      CALL AIO_IWRITE( OCH, 2, 'Dataset Selection Information :',
     :                 STATUS )
      CALL AIO_BLNK( OCH, STATUS )

*  Get number of selection records
      CALL SLN_NREC( IFID, NREC, STATUS )

*  Display selections
      IF ( NREC .EQ. 0 ) THEN

        CALL AIO_IWRITE( OCH, 4, '** No selection data in input **',
     :                 STATUS )
        CALL AIO_BLNK( OCH, STATUS )

      ELSE

*    Write each selection
        DO I = 1, NREC

*      Announce this one
          CALL MSG_SETI( 'N', I )
          CALL AIO_IWRITE( OCH, 4, 'Selection Record ^N', STATUS )
          CALL AIO_BLNK( OCH, STATUS )

*      Get this record
          CALL SLN_GETREC( IFID, '*', I, SELID, STATUS )

*      Get program id
          CALL ADI_CGET0C( SELID, 'Version', CREATOR, STATUS )
          CALL ASHOW_VAL( CREATOR, 'Creator', ' ', OCH, STATUS )

*      Locate Selectors
          CALL ADI_FIND( SELID, 'Selectors', SID, STATUS )

*      Get number of components
          CALL ADI_NCMP( SID, NCMP, STATUS )
          DO ICMP = 1, NCMP

*        Index the selector
            CALL ADI_INDCMP( SID, ICMP, SIID, STATUS )

*        Get name and variant
            CALL ADI_NAME( SIID, SNAME, STATUS )
            CALL ADI_CGET0C( SIID, 'Variant', VARIANT, STATUS )

*        Switch on variant
            CALL ASHOW_VAL( 'Variant = '//VARIANT, SNAME, ' ',
     :                      OCH, STATUS )
            IF ( VARIANT .EQ. 'AREA_DESCRIPTION' ) THEN
              CALL ADI_CGET0I( SIID, 'GRPID', GRPID, STATUS )

              CALL GRP_GRPSZ(GRPID,SIZE,STATUS)

              DO ITXT = 1, SIZE
                CALL GRP_GET( GRPID, ITXT, 1, ARDIN, STATUS )
                L = CHR_LEN(ARDIN)
                IF ( ITXT .EQ. 1 ) THEN
                  CALL ASHOW_VAL( ARDIN(:L), 'Description', ' ',
     :                            OCH, STATUS )
                ELSE
                  CALL AIO_IWRITE( OCH, 29, ARDIN(:L), STATUS )
                END IF
              END DO

            ELSE IF ( VARIANT .EQ. 'RANGE_PAIRS' ) THEN

*          How many pairs?
              CALL ADI_CSIZE( SIID, 'START', SIZE, STATUS )

*          Map the data
              CALL ADI_CMAPR( SIID, 'START', 'READ', BPTR, STATUS )
              CALL ADI_CMAPR( SIID, 'STOP', 'READ', EPTR, STATUS )

*          Loop over pairs
              DO ITXT = 1, SIZE

*            Get these values
                CALL ARR_ELEM1R( BPTR, SIZE, ITXT, START, STATUS )
                CALL ARR_ELEM1R( EPTR, SIZE, ITXT, STOP, STATUS )

*            Print 'em out
                CALL MSG_SETR( 'START', START )
                CALL MSG_SETR( 'STOP', STOP )
                CALL MSG_MAKE( '^START -> ^STOP', ARDIN, L )
                IF ( ITXT .EQ. 1 ) THEN
                  CALL ASHOW_VAL( ARDIN(:L), 'Description', ' ',
     :                            OCH, STATUS )
                ELSE
                  CALL AIO_IWRITE( OCH, 29, ARDIN(:L), STATUS )
                END IF

              END DO

*          Release range pairs data
              CALL ADI_CUNMAP( SIID, 'START', BPTR, STATUS )
              CALL ADI_CUNMAP( SIID, 'STOP', EPTR, STATUS )

            END IF

*        Release selector
            CALL ADI_ERASE( SIID, STATUS )

          END DO

*      Destroy it
          CALL ADI_ERASE( SID, STATUS )
          CALL ADI_ERASE( SELID, STATUS )

        END DO

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ASHOW_SEL', STATUS )

      END



      SUBROUTINE ASHOW_OB( OBJ, MEMBER, TYPE, DESCRIP, UNITS, IMODE,
     :                      OCH, STATUS )
*+
*  Name:
*     ASHOW_OB

*  Purpose:
*     Format an ADI item

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ASHOW_OBI( OBJ, MEMBER, TYPE, DESCRIP, UNITS, IMODE, OCH, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     OBJ = INTEGER (given)
*        ADI object to extract data from
*     MEMBER = CHARACTER*(*) (given)
*        Name of data member to extract
*     TYPE = CHARACTER*(*) (given)
*        Type code for item
*     DESCRIP = CHARACTER*(*)
*        Description of the object
*     UNITS = CHARACTER*(*)
*        Units of the object
*     IMODE = CHARACTER*(*)
*        How to handle missing data members
*     OCH = INTEGER (given)
*        Output channel identifier
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

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

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

*  Arguments Given:
      INTEGER			OBJ			! ADI object
      CHARACTER*(*)		MEMBER, TYPE
      CHARACTER*(*)		DESCRIP, UNITS
      CHARACTER*(*)		IMODE
      INTEGER			OCH			! Output channel

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Constants:
      INTEGER			IND			! Indentation
        PARAMETER		( IND = 6 )

*  Local Variables:
      CHARACTER*20		LDESCRIP
      CHARACTER*80		CVALUE

      DOUBLE PRECISION		DVALUE

      REAL			RVALUE

      INTEGER			IVALUE

      LOGICAL			OK			! Data is ok?
      LOGICAL			THERE			! Member exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Local copy of description
      LDESCRIP = DESCRIP

*  Does member exist?
      CALL ADI_THERE( OBJ, MEMBER, THERE, STATUS )
      OK = .FALSE.
      IF ( THERE ) THEN

*    Extract data
        IF ( TYPE .EQ. 'C' ) THEN
          CALL ADI_CGET0C( OBJ, MEMBER, CVALUE, STATUS )
        ELSE IF ( TYPE .EQ. 'D' ) THEN
          CALL ADI_CGET0D( OBJ, MEMBER, DVALUE, STATUS )
        ELSE IF ( TYPE .EQ. 'R' ) THEN
          CALL ADI_CGET0R( OBJ, MEMBER, RVALUE, STATUS )
        ELSE IF ( TYPE .EQ. 'I' ) THEN
          CALL ADI_CGET0I( OBJ, MEMBER, IVALUE, STATUS )
        END IF

*    Trap errors
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
        ELSE
          OK = .TRUE.
        END IF

      END IF

      IF ( OK ) THEN

*    Set tokens
        IF ( TYPE .EQ. 'C' ) THEN
          CALL MSG_SETC( 'VAL', CVALUE )
        ELSE IF ( TYPE .EQ. 'R' ) THEN
          CALL MSG_SETR( 'VAL', RVALUE )
        ELSE IF ( TYPE .EQ. 'D' ) THEN
          CALL MSG_SETD( 'VAL', DVALUE )
        ELSE IF ( TYPE .EQ. 'I' ) THEN
          CALL MSG_SETI( 'VAL', IVALUE )
        END IF
        CALL MSG_SETC( 'UNITS', UNITS )
        CALL AIO_IWRITE( OCH, IND, LDESCRIP//' : ^VAL ^UNITS', STATUS )

      ELSE IF ( IMODE .EQ. 'np' ) THEN
        IF ( THERE ) THEN
          CALL AIO_IWRITE( OCH, IND, LDESCRIP//' : * unreadable *',
     :           STATUS )
        ELSE
          CALL AIO_IWRITE( OCH, IND, LDESCRIP//' : * not present *',
     :           STATUS )
        END IF

      END IF

      END


      SUBROUTINE ASHOW_VAL( CVALUE, DESCRIP, UNITS, OCH, STATUS )
*+
*  Name:
*     ASHOW_VAL

*  Purpose:
*     Format an ADI item

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ASHOW_OBI( CVALUE, DESCRIP, UNITS, OCH, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     CVALUE = CHARACTER*(*) (given)
*        Value to format
*     DESCRIP = CHARACTER*(*)
*        Description of the object
*     UNITS = CHARACTER*(*)
*        Units of the object
*     OCH = INTEGER (given)
*        Output channel identifier
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

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

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

*  Arguments Given:
      CHARACTER*(*)		CVALUE, DESCRIP, UNITS
      INTEGER			OCH			! Output channel

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Constants:
      INTEGER			IND			! Indentation
        PARAMETER		( IND = 6 )

*  Local Variables:
      CHARACTER*20		LDESCRIP
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Local copy of description
      LDESCRIP = DESCRIP

*    Set tokens
      CALL MSG_SETC( 'VAL', CVALUE )
      CALL MSG_SETC( 'UNITS', UNITS )
      CALL AIO_IWRITE( OCH, IND, LDESCRIP//' : ^VAL ^UNITS', STATUS )

      END
