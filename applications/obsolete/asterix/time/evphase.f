      SUBROUTINE EVPHASE( STATUS )
*+
*  Name:
*     EVPHASE

*  Purpose:
*     Adds a PHASE list to input event dataset

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL EVPHASE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Given the RAW_TIMETAG info in an event dataset, constructs a phase
*     list using the ephemeris
*
*                     Ephemeris = c1 + c2*N + c3 * N**2
*
*     c1 in JD, c2 in days, and c3 days/day . c1 is the date of phase zero,
*     c2 the period and c3 the first derivative of the period wrt time. The
*     user supplies the coefficients c<i>, which are used to convert time
*     axis into phase.

*  Usage:
*     evphase input_evds output_evds torigin period period' [tlist]

*  Environment Parameters:
*
*     INP = CHAR (read)
*        Input file name
*     OUT = CHAR (read)
*        Output file
*     COEFF1 = DOUBLE (read)
*        First ephemeris coefficient
*     COEFF2 = DOUBLE (read)
*        Second ephemeris coefficient
*     COEFF3 = DOUBLE (read)
*        Third ephemeris coefficient
*     TLIST = CHARACTER (read)
*        Name of list containing times

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
*     Time axis is assumed to be in units of SECONDS.

*  References:
*     {task_references}...

*  Keywords:
*     evphase, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      7 Jun 1991 V1.5-0 (DJA):
*        Adapted from PHASE
*     24 Jul 1991 V1.5-1 (DJA):
*        Updated TAI definition
*      7 Oct 1992 V1.7-0 (DJA):
*        Minor changes for UNIX port
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     20 Apr 1995 V1.8-1 (DJA):
*        No longer uses BDA routines
*     16 Aug 1995 V2.0-0 (DJA):
*        ADI port, redid prologue. Made list name user controllable
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

*  Local Constants:
      DOUBLE PRECISION		DAY2S			! Days to seconds
        PARAMETER		( DAY2S = 86400D0 )

      DOUBLE PRECISION          MJDORG			! Origin of MJD in JD
        PARAMETER               ( MJDORG = 2400000.5D0 )

      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'EVPHASE Version 2.1-0' )

*  Local Variables:
      CHARACTER*80           	EPHEMERIS(3)            ! History entry
      CHARACTER*20		TLIST			! Time list name

      DOUBLE PRECISION       	BASEMJD                 ! Start time of obs'n
      DOUBLE PRECISION       	BASETAI                 ! Start time of observation (days after 1st Jan 1972)
      DOUBLE PRECISION       	EPHTAI               	! Ephemeris base time
                                                        ! in TAI
      DOUBLE PRECISION       	COEFF(3)                ! Ephemeris coefficients

      INTEGER                	C                       ! Loop over COEFF
      INTEGER			IFID			! Input dataset id
      INTEGER                	NEVENT                  ! # events
      INTEGER                	NLIST                   ! # lists
      INTEGER			OFID			! Output dataset id
      INTEGER			PLID			! Phase list identifier
      INTEGER                	PPTR                    ! Phase list data
      INTEGER			TIMID			! Timing info
      INTEGER			TLID			! Time list identifier
      INTEGER                	TPTR                    ! Time list data

      LOGICAL			ATOMIC			! Got atomic time?
      LOGICAL                	OK                      ! Input data ok?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Associate input dataset
      CALL USI_ASSOC( 'INP', 'EventDS', 'READ', IFID, STATUS )

*  Open a cloned copy of the input
      CALL USI_CLONE( 'INP', 'OUT', 'EventDS', OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get number of events
      CALL EDI_GETNS( IFID, NEVENT, NLIST, STATUS )

*  Get observation start time
      CALL TCI_GETID( IFID, TIMID, STATUS )
      CALL ADI_THERE( TIMID, 'TAIObs', ATOMIC, STATUS )
      IF ( ATOMIC ) THEN
        CALL ADI_CGET0D( TIMID, 'TAIObs', BASETAI, STATUS )
        CALL MSG_PRNT( 'Using base atomic time (TAI) as time origin' )
      ELSE
        CALL ADI_THERE( TIMID, 'MJDObs', OK, STATUS )
        IF ( OK ) THEN
          CALL ADI_CGET0D( TIMID, 'MJDObs', BASEMJD, STATUS )
          CALL MSG_PRNT( 'Using observation start as time origin' )
        ELSE
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'FATAL ERROR: No absolute time origin '/
     :                                     /'found in file', STATUS )
        END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get time list name
      CALL USI_GET0C( 'TLIST', TLIST, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Locate the list the user hsa nominated
      CALL EDI_IDXNAM( IFID, TLIST, TLID, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_SETC( 'TL', TLIST )
        CALL ERR_REP( ' ', 'Unable to find time list ^TL', STATUS )
        GOTO 99
      END IF
      CALL ADI_ERASE( TLID, STATUS )
      CALL EDI_MAPD( IFID, TLIST, 'READ', 0, 0, TPTR, STATUS )

*  Create output phase list
      CALL EDI_CREL0R( OFID, 'PHASE', .FALSE., 0.0, 1.0, 0.0, ' ',
     :                 PLID, STATUS )
      CALL EDI_CREAT( OFID, PLID, STATUS )
      CALL ADI_ERASE( PLID, STATUS )

*  Map the output phase list
      CALL EDI_MAPR( OFID, 'PHASE', 'WRITE', 0, 0, PPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get parameters of ephemeris (units JD)
      CALL MSG_PRNT('Enter Ephemeris Coeffs : a(1) + a(2)*N + a(3)*N*N')
      CALL USI_GET0D( 'COEFF1', COEFF(1), STATUS )
 20   CALL USI_GET0D( 'COEFF2', COEFF(2), STATUS )
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( COEFF(2) .LT. 0.0 ) ) THEN
        CALL MSG_PRNT( 'Negative period supplied !' )
        CALL USI_CANCL( 'COEFF2', STATUS )
        GOTO 20
      END IF
      CALL USI_GET0D( 'COEFF3', COEFF(3), STATUS )

*  Write ephemeris values to EPHEMERIS
      DO C = 1, 3
        WRITE(EPHEMERIS(C), '(A,I1,A,G15.5)')
     :                            'Coefficient ',C,' = ',COEFF(C)
      END DO

*  Convert to MJD
      COEFF(1) = COEFF(1) - MJDORG

*  Convert to difference from dataset reference time in seconds. Switch on
*  presence of atomic time
      IF ( ATOMIC ) THEN

*    Convert ephemeris origin to atomic time
        CALL TCI_MJD2TAI( COEFF(1), EPHTAI )

*    Find difference from dataset reference time in seconds,
        COEFF(1) = (EPHTAI - BASETAI) * DAY2S

      ELSE
        COEFF(1) = (COEFF(1) - BASEMJD) * DAY2S

      END IF

*  And other coefficents in seconds
      COEFF(2) = COEFF(2) * DAY2S
      COEFF(3) = COEFF(3) * DAY2S

*  Execute time-phase conversion
      CALL EVPHASE_INT( NEVENT, %VAL(TPTR), COEFF, %VAL(PPTR), STATUS )

*  History
      CALL HSI_ADD( OFID, VERSION, STATUS )
      CALL HSI_PTXT( OFID, 3, EPHEMERIS, STATUS )

*  Tidy up
 99   CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END



*+  EVPHASE_INT - Calculate phase bins given time axis bin centres
      SUBROUTINE EVPHASE_INT( NPTS, TIME, COEFF, OUTPHASE, STATUS )
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      6 Sep 90 : Original (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER                NPTS                          ! No of data poits
      DOUBLE PRECISION       TIME(NPTS)                    ! Time bin values
      DOUBLE PRECISION       COEFF(3)                      ! Ephemeris coefficients
*
*    Export :
*
      REAL                   OUTPHASE(NPTS)                ! Output phase axis
*
*    Status :
*
      INTEGER                STATUS
*
*    Local variables :
*
      DOUBLE PRECISION       ACCPHA                        ! Phase

      INTEGER                CYCLES                        ! Number of phase cycles
      INTEGER                I                             ! Loop counter
      INTEGER                IFAIL                         ! Phase sub status
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Execute time-phase conversion
      DO I = 1, NPTS
        CALL PHASE_ACCPHA( COEFF(1), COEFF(2), COEFF(3), TIME(I),
     :                                            ACCPHA, IFAIL )
        IF ( IFAIL .EQ. 0 ) THEN

          IF ( ACCPHA .LT. 0.0 .AND. ACCPHA .NE. INT(ACCPHA)) THEN
            CYCLES = - NINT(ABS( ACCPHA ) + 0.5)
          ELSE
            CYCLES = INT( ACCPHA )
          END IF
          OUTPHASE(I) = ACCPHA - REAL( CYCLES )

        ELSE
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Period has gone negative! - aborting',
     :                                                      STATUS )
          GOTO 99
        END IF

      END DO

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'EVPHASE_INT', STATUS )
      END IF

      END
