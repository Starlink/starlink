      SUBROUTINE XRTHOT( STATUS )
*+
*  Name:
*     XRTHOT

*  Purpose:
*     Writes hotspot header into hotspot HDS file

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL XRTHOT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Reads the HOTSPOT file FITS header and writes some of the values into
*     the Header file.

*  Usage:
*     xrthot {parameter_usage}

*  Environment Parameters:
*     INPUT = CHAR (read)
*        Name of logfile from XRTDISK
*     ROOTNAME = CHAR (read)
*        Name of hotspot HDS file

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
*     xrthot, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     RDS: Richard Saxton (Starlink, University of Leicester)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      9 Mar 1994 V1.502 (JKA):
*        Now uses origin fits file for data
*     24 Apr 1994 V1.7-0 (JKA):
*        For new asterix release
*     18 Dec 1995 V2.0-0 (DJA):
*        ADI port. Split in two for cleaner XRTCONV interaction
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

*  Local Variables:
      CHARACTER*80		FNAME			! Hotspot file
      CHARACTER*80		RTNAME			! Root for output name
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get user inputs
      CALL USI_GET0C( 'INPUT', FNAME, STATUS )
      CALL USI_GET0C( 'ROOTNAME', RTNAME, STATUS )

*  Invoke internal routine
      CALL XRTHOT_INT( FNAME, RTNAME, STATUS )

*  Tidy up
      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


      SUBROUTINE XRTHOT_INT( FNAME, RTNAME, STATUS )
*+
*  Name:
*     XRTHOT_INT

*  Purpose:
*     {routine_purpose}

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL XRTHOT_INT( FNAME, RTNAME, STATUS )

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
*     xrthot, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     18 Dec 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'

*  Structure Definitions :
      INCLUDE 'INC_XRTHEAD'

*  Arguments Given:
      CHARACTER*(*)			FNAME, RTNAME

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'XRTHOT Version V2.0-0' )

*  Local Variables:
      RECORD /XRT_HEAD/ 	HEAD			! ROSAT header object

      CHARACTER*80		HNAME			! Header file name

      DOUBLE PRECISION 		SPOTS(MAXSPOT*3)	! Spot info

      INTEGER                   BLKSIZE			! FITS file block size
      INTEGER                   HID			! Header file identifier
      INTEGER                   ISTATUS			! FITSIO status
      INTEGER 			LP			! Loop over spots
      INTEGER                   LUNIT			! FITS file log unit
      INTEGER			NSPOT			! # of spots
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Open fits file
      ISTATUS = 0
      CALL FIO_GUNIT( LUNIT, ISTATUS )
      CALL FTOPEN( LUNIT, FNAME, 0, BLKSIZE, ISTATUS )
      IF ( ISTATUS .NE. 0 ) THEN
        CALL ADI2_FITERP( ISTATUS, STATUS )
        CALL MSG_PRNT('Error opening hotspot file')
        GOTO 99
      END IF

*  Get the hotspot array
      CALL GHISTND( LUNIT, 'BAD_PIX_X_Y_R', SPOTS, MAXSPOT*3, NSPOT,
     :              ISTATUS )

*  Close the fits file
      CALL FTCLOS( LUNIT, ISTATUS )
      CALL FIO_PUNIT( LUNIT, STATUS )

*  A fitsio error has occured
      IF ( ISTATUS .NE. 0 ) THEN
        CALL ADI2_FITERP( ISTATUS, STATUS )
        CALL MSG_PRNT('Error detected reading FITS file')
        GOTO 99
      ENDI

*  Read the header infotmation
      CALL RAT_GETXRTHEAD( RTNAME, HEAD, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Write spot values into the appropriate arrays
      HEAD.NSPOT = NSPOT / 3
      DO LP = 1, HEAD.NSPOT
        HEAD.XSPOT(LP) = SPOTS(1+(LP-1)*3)
        HEAD.YSPOT(LP) = SPOTS(2+(LP-1)*3)
        HEAD.SPOTRAD(LP) = SPOTS(LP*3)
      END DO

*  Inform user of hotspots found
      CALL MSG_SETI( 'NSPOT', HEAD.NSPOT )
      CALL MSG_PRNT( '^NSPOT hotspots/deadspots in field of view' )

*  Open the header file for update
      HNAME = RTNAME(1:CHR_LEN(RTNAME))//'_hdr%hds'
      CALL ADI_FOPEN( HNAME, '*', 'UPDATE', HID, STATUS )
      IF (STATUS .NE. SAI__OK) THEN
        CALL MSG_PRNT('Cannot open header file for update')
        GOTO 99
      END IF

*  Write the header structure into the header file
      CALL RAT_PUTHEAD( HID, 'HEAD', HEAD, STATUS )

*  Update the history information
      CALL HSI_ADD( HID, VERSION, STATUS )

*  Close header file
      CALL ADI_FCLOSE( HID, STATUS )

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'XRTHOT_INT', STATUS )

      END
