      SUBROUTINE EVCSUBSET( STATUS )
*+
*  Name:
*     EVCSUBSET

*  Purpose:
*     Select an annular subset of an event dataset

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL EVCSUBSET( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The user may select a circular or annular subset of the event dataset
*     according to X and Y coordinate list values.
*
*     The new values of the X and Y field extrema are derived from the
*     minimum and maximum values of the events which pass the annular
*     criterion.

*  Usage:
*     evcsubset {parameter_usage}

*  Environment Parameters:
*     INP = LITERAL (read)
*        Name of input event dataset
*     OUT = LITERAL (read)
*        Name of the output event dataset
*     ANNULUS = LOGICAL (read)
*        Annular subset?
*     XCENT = REAL (read)
*        X coordinate of centre of region
*     YCENT = REAL (read)
*        Y coordinate of centre of region
*     OUTER = REAL (read)
*        Outer radius of region
*     INNER = REAL (read)
*        Inner radius of region

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
*     evcsubset, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     ADM: Alan McFadzean (ROSAT, University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     16 Jan 1989 V1.1-1 (ADM):
*        Original version.
*     11 Jan 1990 V1.1-2 (DJA):
*        DATA_MIN and DATA_MAX references removed
*     28 Mar 1990 V1.1-3 (DJA):
*        X and Y coords made into separate parameters
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*      2 Jan 1996 V2.0-0 (DJA):
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
        PARAMETER		( VERSION = 'EVCSUBSET Version V2.0-0' )

*  Local Variables:
      CHARACTER*80		TEXT(2)			! History text
      CHARACTER*20		XLNAME, YLNAME		! X,Y list names

      REAL			IRAD, ORAD		! Selection radii
      REAL			XCEN, YCEN		! Centre of selection
      REAL			XMIN, XMAX		! X list extrema
      REAL			YMIN, YMAX		! Y list extrema

      INTEGER			COPY			! Event selection array
      INTEGER			EVENTS			! # input events
      INTEGER			IFID			! Input event dataset
      INTEGER			IFILES			! Input file info
      INTEGER			LSEL(2)			! X,Y list numbers
      INTEGER			NLISTS			! # lists in input
      INTEGER 			NSEL			! # selected lists
      INTEGER			OFID			! Output event dataset
      INTEGER			OUTLEN			! # output events
      INTEGER			TLEN			! Length of string
      INTEGER			X_LID, Y_LID		! X,Y list indices
      INTEGER			XPTR, YPTR		! Mapped list values

      LOGICAL			ANNULUS			! Annular selection?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Associate input
      CALL USI_ASSOC( 'INP', 'EventDS', 'READ', IFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get number of events and lists
      CALL EDI_GETNS( IFID, EVENTS, NLISTS, STATUS )

*  Associate output dataset
      CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )

*  Display lists
      CALL MSG_PRNT( ' ' )
      CALL MSG_PRNT( 'The availible LISTs are:' )
      CALL EDI_DISP( IFID, STATUS )

*  Find out which events are to be copied. We use a BYTE here rather than
*  a LOGICAL to save dynamic memory.
      CALL DYN_MAPB( 1, EVENTS, COPY, STATUS )

*  Display list names
      CALL MSG_PRNT (' ')
      CALL MSG_PRNT ('Enter the LISTs to have ranges applied, by '//
     :                                     'entering the index numbers')
      CALL MSG_PRNT ('E.g. 1 2 4')
      CALL EDI_SELCT( 'LISTS', NLISTS, 2, 2, LSEL, NSEL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Locate the lists
      CALL EDI_IDX( IFID, LSEL(1), X_LID, STATUS )
      CALL EDI_IDX( IFID, LSEL(2), Y_LID, STATUS )
      CALL ADI_CGET0C( X_LID, 'Name', XLNAME, STATUS )
      CALL ADI_CGET0C( Y_LID, 'Name', YLNAME, STATUS )
      CALL ADI_ERASE( X_LID, STATUS )
      CALL ADI_ERASE( Y_LID, STATUS )

*  Map the two lists
      CALL EDI_MAPR( IFID, XLNAME, 'READ', 0, 0, XPTR, STATUS )
      CALL EDI_MAPR( IFID, YLNAME, 'READ', 0, 0, YPTR, STATUS )

*  Get field ranges
      CALL EDI_RANGE( IFID, XLNAME, XPTR, XMIN, XMAX, STATUS )
      CALL EDI_RANGE( IFID, YLNAME, YPTR, YMIN, YMAX, STATUS )

*  Announce X axis range
      CALL MSG_SETR( 'XMIN', XMIN )
      CALL MSG_SETR( 'XMAX', XMAX )
      CALL MSG_SETC( 'XNAME', XLNAME )
      CALL MSG_PRNT( 'The ^XNAME data range is ^XMIN to ^XMAX' )

*  Announce Y axis range
      CALL MSG_SETR( 'YMIN', YMIN )
      CALL MSG_SETR( 'YMAX', YMAX )
      CALL MSG_SETC( 'YNAME', YLNAME )
      CALL MSG_PRNT( 'The ^YNAME data range is ^YMIN to ^YMAX' )

*  Annular or circular subset?
      CALL USI_GET0L( 'ANNULUS', ANNULUS, STATUS )

*  Central point
      CALL USI_GET0R( 'XCENT', XCEN, STATUS )
      CALL USI_GET0R( 'YCENT', YCEN, STATUS )

*  Get radius/radii
      CALL USI_GET0R('OUTER',ORAD,STATUS)
      IF ( ANNULUS ) THEN
        CALL USI_GET0R( 'INNER', IRAD, STATUS )
      ELSE
        IRAD = 0.0
      END IF

*  Find elements to copy to output
      CALL EVCSUBSET_SETSEL( EVENTS, %VAL(XPTR), %VAL(YPTR), XCEN, YCEN,
     :                       ORAD, IRAD, %VAL(COPY), OUTLEN, STATUS )

*  Tell user how many items will remain
      CALL MSG_SETI( 'N1', EVENTS )
      CALL MSG_SETI( 'N2', OUTLEN )
      CALL MSG_PRNT( '^N2 items remain out of ^N1 in original dataset' )
      IF ( OUTLEN .EQ. 0 ) THEN
        CALL MSG_PRNT ('FATAL ERROR: All data excluded')
        STATUS = SAI__ERROR
      ELSE IF ( OUTLEN .EQ. EVENTS ) THEN
        CALL MSG_PRNT ('FATAL ERROR: All data included')
        STATUS = SAI__ERROR
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Create output file
      CALL EDI_LINK( 'EventDS', OUTLEN, ' ', OFID, STATUS )
      CALL UDI_COPANC( IFID, ' ', OFID, STATUS )

*  Select events
      CALL EDI_SUBSET( IFID, %VAL(COPY), OFID, STATUS )

*  Update list ranges

*  Release selection array
      CALL DYN_UNMAP( COPY, STATUS )

*  Update history
      CALL HSI_ADD( OFID, VERSION, STATUS )
      CALL USI_NAMES( 'I', IFILES, STATUS )
      CALL HSI_PTXTI( OFID, IFILES, STATUS )
      CALL MSG_SETR( 'XC', XCEN )
      CALL MSG_SETR( 'YC', YCEN )
      CALL MSG_SETR( 'OR', ORAD )
      IF ( ANNULUS ) THEN
        CALL MSG_SETR( 'IR', IRAD )
        CALL MSG_MAKE( 'Annulus, centre (^XC,^YC)', TEXT(1), TLEN )
        CALL MSG_MAKE( 'Inner radius = ^IR, outer radius = ^OR',
     :                 TEXT(2), TLEN )
      ELSE
        CALL MSG_MAKE( 'Circle, centre (^XC,^YC)', TEXT(1), TLEN )
        CALL MSG_MAKE( 'Radius = ^OR', TEXT(2), TLEN )
      END IF
      CALL HSI_PTXT( OFID, 2, TEXT, STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



      SUBROUTINE EVCSUBSET_SETSEL( INLEN, XP, YP, XCEN, YCEN, ORAD,
     :                             IRAD, COPY, OUTLEN, STATUS )
*+
*  Name:
*     EVCSUBSET_SELSEL

*  Purpose:
*     Set up selection logicals according to X,Y list values

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EVCSUBSET_SELSEL( [p]... )

*  Description:
*     {routine_description}

*  Arguments:
*     INLEN = INTEGER (given)
*        Number of events in input
*     XP[] = REAL (given)
*        X position of events
*     YP[] = REAL (given)
*        Y position of events
*     XCEN = REAL (given)
*        X centre of selection region
*     YCEN = REAL (given)
*        Y centre of selection region
*     ORAD = REAL (given)
*        Outer radius of selection region
*     IRAD = REAL (given)
*        Inner radius of selection region
*     OUTLEN = INTEGER (returned)
*        Number of selected events
*     COPY[] = BYTE (returned)
*        Selection array, 1 for selected events, 0 otherwise
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

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {task_references}...

*  Keywords:
*     evcsubset, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     11 Jan 1996 (DJA):
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
      INTEGER			INLEN
      REAL			XP(*), YP(*), XCEN, YCEN, ORAD, IRAD

*  Arguments Returned:
      INTEGER			OUTLEN
      BYTE			COPY(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      REAL	  		IR2, OR2	  	! Radii squared
      REAL                   	TEST             	! Test value

      INTEGER                	I                	! Loop counter
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      OR2 = ORAD**2
      IR2 = IRAD**2
      OUTLEN = 0

*  Circle?
      IF ( IRAD .EQ. 0.0 ) THEN

        DO I=1,INLEN
          TEST=((XCEN-XP(I))**2)+((YCEN-YP(I))**2)
          IF(TEST.LE.OR2) THEN
            COPY(I) = 1
            OUTLEN = OUTLEN + 1
          ELSE
            COPY(I) = 0
          END IF
        END DO

*  Annnulus
      ELSE

        DO I=1,INLEN
          TEST=((XCEN-XP(I))**2)+((YCEN-YP(I))**2)
          IF(TEST.LE.OR2.AND.TEST.GT.IR2) THEN
            COPY(I) = 1
            OUTLEN = OUTLEN + 1
          ELSE
            COPY(I) = 0
          END IF
        END DO

      END IF

      END
