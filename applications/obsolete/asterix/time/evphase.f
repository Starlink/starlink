*+  EVPHASE - Adds a PHASE lists to input event dataset
      SUBROUTINE EVPHASE( STATUS )
*
*    Description :
*
*     Given the RAW_TIMETAG info in an event dataset, constructs a phase
*     list using the ephemeris
*
*                     Ephemeris = c1 + c2*N + c3 * N**2
*
*     c1 in JD, c2 in days, and c3 days/day . c1 is the date of phase zero,
*     c2 the period and c3 the first derivative of the period wrt time. The
*     user supplies the coefficients c<i>, which are used to convert time
*     axis into phase.
*
*    Environment parameters :
*
*     INP                = UNIV(R)
*        Input file
*     OUT                = UNIV(W)
*        Output file
*     COEFF<1,2,3>       = DOUBLE(R)
*        Ephemeris coefficients
*
*    Method :
*    Deficiencies :
*     Time axis is assumed to be in units of SECONDS
*    Bugs :
*    Authors :
*
*     David J. Allan   (BHVAD::DJA)
*
*    History :
*
*      7 Jun 91 : V1.5-0 Adapted from PHASE (DJA)
*     24 Jul 91 : V1.5-1 Updated TAI definition (DJA)
*      7 Oct 92 : V1.7-0 Minor changes for UNIX port (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     20 Apr 95 : V1.8-1 No longer uses BDA routines (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) 	ILOC                    ! Input dataset
      CHARACTER*(DAT__SZLOC) 	OLOC                    ! Output dataset
      CHARACTER*(DAT__SZLOC) 	PLOC                    ! Phase object
      CHARACTER*80           	EPHEMERIS(3)            ! History entry

      DOUBLE PRECISION       	BASETAI                 ! Start time of observation (days after 1st Jan 1972)
      DOUBLE PRECISION       	EPHEM_TAI               ! Ephemeris base time
                                                        ! in TAI
      DOUBLE PRECISION       	COEFF(3)                ! Ephemeris coefficients

      INTEGER                	C                       ! Loop over COEFF
      INTEGER			IFID			! Input dataset id
      INTEGER                	NELM                    ! No of data poits
      INTEGER			OFID			! Output dataset id
      INTEGER                	PPTR                    ! Phase list data
      INTEGER			TIMID			! Timing info
      INTEGER                	TPTR                    ! Time list data

      LOGICAL                	OK                      ! Input data ok?
      LOGICAL                	PHASE_THERE             ! Phase exists already?
*
*    Version :
*
      CHARACTER*25 		VERSION
        PARAMETER         	( VERSION = 'EVPHASE Version 1.8-1' )
*-

*    Version
      CALL MSG_PRNT( VERSION )

*    Initialize ASTERIX subroutines
      CALL AST_INIT()

*    Associate input and output datasets
      CALL USI_TASSOC2('INP', 'OUT', 'READ', IFID, OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Extract locators
      CALL ADI1_GETLOC( IFID, ILOC, STATUS )
      CALL ADI1_GETLOC( OFID, OLOC, STATUS )

*    Get observation start time
      CALL TCI_GETID( IFID, TIMID, STATUS )
      CALL ADI_THERE( TIMID, 'TAIObs', OK, STATUS )
      IF ( OK ) THEN
        CALL ADI_CGET0D( TIMID, 'TAIObs', BASETAI, STATUS )
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'FATAL ERROR: No base atomic time'/
     :                /' in file header', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Copy input to output
      CALL ADI_FCOPY( IFID, OFID, STATUS )

*    Find TIME list and its length
      CALL LIST_OK( ILOC, 'RAW_TIMETAG', OK, STATUS )
      IF ( .NOT. OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', '! Unable to find time list', STATUS )
        GOTO 99
      END IF
      CALL LIST_BDAMAP( ILOC, 'RAW_TIMETAG', '_REAL', 'READ',
     :                                   TPTR, NELM, STATUS )

*    Delete existing phase list if present
      CALL DAT_THERE( OLOC, 'PHASE', PHASE_THERE, STATUS )
      IF ( PHASE_THERE ) THEN
        CALL DAT_ERASE( OLOC, 'PHASE', STATUS )
      END IF

*    Create output phase list
      CALL LIST_CREMAPR( OLOC, 'PHASE', NELM, .FALSE., 0.0, .FALSE.,
     :                           ' ', 0.0, 1.0, PPTR, PLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get parameters of ephemeris (units JD)
      CALL MSG_PRNT('Enter Ephemeris Coeffs : a(1) + a(2)*N + a(3)*N*N')
      CALL USI_GET0D('COEFF1', COEFF(1), STATUS)
 20   CALL USI_GET0D('COEFF2', COEFF(2), STATUS)
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( COEFF(2) .LT. 0.0 ) ) THEN
        CALL MSG_PRNT( 'Negative period supplied !' )
        CALL USI_CANCL( 'COEFF2', STATUS )
        GOTO 20
      END IF
      CALL USI_GET0D( 'COEFF3', COEFF(3), STATUS )

*    Write ephemeris values to EPHEMERIS
      DO C = 1, 3
        WRITE(EPHEMERIS(C), '(A,I1,A,G15.5)')
     :                            'Coefficient ',C,' = ',COEFF(C)
      END DO

*    Convert to MJD
      COEFF(1) = COEFF(1) - 2400000.5

*    Then to atomic time
      CALL TIM_MJD2TAI( COEFF(1), EPHEM_TAI )

*    Find difference from dataset reference time in seconds, and other
*    coefficents in seconds
      COEFF(1) = (EPHEM_TAI - BASETAI) * 86400.0D0
      COEFF(2) = COEFF(2) * 86400.0D0
      COEFF(3) = COEFF(3) * 86400.0D0

*    Execute time-phase conversion
      CALL EVPHASE_INT( NELM, %VAL(TPTR), COEFF, %VAL(PPTR), STATUS )

*    History
      CALL HSI_ADD( OFID, VERSION, STATUS )
      CALL HSI_PTXT( OFID, 3, EPHEMERIS, STATUS )

*    Tidy up
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
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER                NPTS                          ! No of data poits
      REAL                   TIME(NPTS)                    ! Time bin values
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
        CALL ERR_REP( 'EXERR', '...from EVPHASE_INT', STATUS )
      END IF

      END
