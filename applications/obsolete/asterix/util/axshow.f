      SUBROUTINE AXSHOW( STATUS )
*+
*  Name:
*     AXSHOW

*  Purpose:
*     Display axes of a dataset

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL AXSHOW( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     {routine_description}

*  Usage:
*     axshow {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (read)
*        Input object
*     DEV = CHAR (read)
*        Output device

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
*     axshow, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      8 May 1991 V1.4-0 (DJA):
*        Original version.
*      4 May 1994 V1.7-0 (DJA):
*        Use AIO_ for output
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*      7 Dec 1995 V2.0-0 (DJA):
*        ADI port
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'AXSHOW Version 2.1-0' )

*  Local Variables:
      CHARACTER*80              LABEL, UNITS      	! Axis attributes
      CHARACTER*79		OBUF			! Output buffer
      CHARACTER*30              RSTR              	! Axis range description
      CHARACTER*9               WSTR              	! Axis width description

      REAL                      LO, HI            	! Axis range

      INTEGER                   DEVWID            	! Device width
      INTEGER			DIMS(ADI__MXDIM)	! Dataset dimensions
      INTEGER                   FSTAT             	! i/o status code
      INTEGER                   I                 	! Loop over dimensions
      INTEGER			IFID			! Input dataset id
      INTEGER                   OCH               	! Output channel
      INTEGER                   NAX               	! Number of axes
      INTEGER                   PTR               	! Ptr to mapped component
      INTEGER                   TLEN              	! Text length

      LOGICAL                   OK, WOK           	! Input objects ok?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get input object
      CALL USI_ASSOC( 'INP', 'BinDS', 'READ', IFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Any axes present?
      CALL BDI_CHK( IFID, 'Axes', OK, STATUS )
      IF ( .NOT. OK ) THEN
        CALL MSG_PRNT( 'No axes present in input' )
        STATUS = SAI__OK
      ELSE

*    Set up output channel
        CALL AIO_ASSOCO( 'DEV', 'LIST', OCH, DEVWID, STATUS )

*    Heading
        WRITE( OBUF, '(1X,A,T70,A)' ) 'Axis Label                '/
     :            /'   Size  Range', 'Widths'
        CALL AIO_WRITE( OCH, OBUF, STATUS )
        CALL AIO_BLNK( OCH, STATUS )

*    Get dimensions
        CALL BDI_GETSHP( IFID, ADI__MXDIM, DIMS, NAX, STATUS )

*    Loop over axes and print out data
        DO I = 1, NAX

*      Get label and units
          CALL BDI_AXGET0C( IFID, I, 'Label', LABEL, STATUS )
          CALL BDI_AXGET0C( IFID, I, 'Units', UNITS, STATUS )

*      Construct range string
          CALL BDI_AXMAPR( IFID, I, 'Data', 'READ', PTR, STATUS )
          CALL ARR_ELEM1R( PTR, DIMS(I), 1, LO, STATUS )
          CALL ARR_ELEM1R( PTR, DIMS(I), DIMS(I), HI, STATUS )
          CALL BDI_AXUNMAP( IFID, I, 'Data', PTR, STATUS )
          CALL MSG_SETR( 'LO', LO )
          CALL MSG_SETR( 'HI', HI )
          CALL MSG_SETC( 'UNITS', UNITS )
          CALL MSG_MAKE( '^LO to ^HI ^UNITS', RSTR, TLEN )

*      Widths present?
          CALL BDI_AXCHK( IFID, I, 'Width', WOK, STATUS )
          IF ( WOK ) THEN
            CALL MSG_SETC( 'WID', 'Ok' )
          ELSE
            CALL MSG_SETC( 'WID', 'Absent' )
          END IF
          CALL MSG_MAKE( '^WID', WSTR, TLEN )

*      Write to output
          WRITE( OBUF, '(1X,I3,2X,A22,I6,2X,A,T70,A)',
     :        IOSTAT=FSTAT ) I, LABEL, DIMS(I), RSTR, WSTR
          CALL AIO_WRITE( OCH, OBUF, STATUS )

        END DO

*    Free device
        CALL AIO_CANCL( 'DEV', STATUS )

      END IF

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
