*+  PHASE - converts time into phase series
      SUBROUTINE PHASE(STATUS)
*
*    Description :
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
*     Anthony Lavender (BHVAD::AFHL)
*     Phillip Andrews  (BHVAD::PLA)
*     David J. Allan   (BHVAD::DJA)
*
*    History :
*
*      6 Feb 86 : Original(BHVAD::AFHL)
*     11 Jun 87 : Redundancy in code removed. (pla)
*     30 Aug 88 : Major rewrite for ASTERIX88 (pla)
*     12 Jan 90 : Wasn't locating axes properly (BHVAD::DJA)
*     13 Jun 90 : V1.2-0 Various bugs fixed. Now always maps axis data, as
*                        algorithm did it anyway! Traps -ve periods. (DJA)
*     24 Jul 91 : V1.5-0 Uses updated TAI definition (DJA)
*     29 Jul 92 : V1.6-0 Explicitly create o/p axes structure (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     20 Apr 95 : V1.8-1 New data interfaces (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*80         	EPHEMERIS(3)            ! History entry

      DOUBLE PRECISION       	BASE_TAI                ! Start time of observation (days after 1st Jan 1972)
      DOUBLE PRECISION       	COEFF(3)                ! Ephemeris coefficients
      DOUBLE PRECISION       	EPHEM_TAI               ! Ephemeris reference
                                                        ! in TAI

      INTEGER			IFID			! Input dataset id
      INTEGER                	INAXPTR                 ! Input time axis
      INTEGER                	OUTAXPTR                ! Output phase axis
      INTEGER                	NPTS                    ! No of data poits
      INTEGER                	NDIMS                   ! Number of dimensions
      INTEGER                	LDIM(ADI__MXDIM)        ! Input dimensions
      INTEGER			OFID			! Output dataset id
      INTEGER                	SKIP                    ! Used in copying
      INTEGER                	AXN1                    ! axes from input
      INTEGER                	AXN2                    ! to output
      INTEGER			TIMID			! Timing info

      INTEGER                	TAXIS, XAXIS, YAXIS     ! Axis numbers

      LOGICAL                	OK                      ! Input data ok?
      LOGICAL                	INPRIM                  ! Input primitive?
      LOGICAL                	REG                     ! Input time axis regular?
*
*    Version :
*
      CHARACTER*22		VERSION
        PARAMETER         	( VERSION = 'PHASE Version 1.8-1' )
*-

*    Version Announcement
      CALL MSG_PRNT( VERSION )

*    Initialize ASTERIX
      CALL AST_INIT()

*    Associate input and output datasets
      CALL USI_TASSOC2( 'INP', 'OUT', 'READ', IFID, OFID, STATUS )
      CALL BDI_PRIM( IFID, INPRIM, STATUS )
      IF ( INPRIM ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP(' ', 'ERROR: Input object must be a dataset',
     :               STATUS )
        GOTO 99
      END IF

*    Get dimensions of axis structure (from data array).
      CALL BDI_CHKDATA(IFID, OK, NDIMS, LDIM, STATUS)
      IF (.NOT. OK) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'FATAL ERROR: No data!', STATUS )
        GOTO 99
      END IF

*    Check time axis exists
      CALL AXIS_TFINDXYT( IFID, NDIMS, XAXIS, YAXIS, TAXIS, STATUS )
      IF ( TAXIS .LE. 0 ) THEN
        CALL MSG_PRNT('FATAL ERROR: No time axis!')
        STATUS = SAI__ERROR

      ELSE
        CALL BDI_CHKAXVAL(IFID, TAXIS, OK, REG, NPTS,  STATUS )

        IF ( OK ) THEN
          CALL BDI_MAPAXVAL(IFID, 'READ', TAXIS, INAXPTR, STATUS )

        ELSE
          CALL MSG_PRNT('FATAL ERROR: Invalid time axis')
          STATUS = SAI__ERROR

        END IF
      END IF

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get observation start time
      CALL TCI_GETID( IFID, TIMID, STATUS )
      CALL ADI_THERE( TIMID, 'TAIObs', OK, STATUS )
      IF ( OK ) THEN
        CALL ADI_CGET0D( TIMID, 'TAIObs', BASE_TAI, STATUS )
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Unable to locate base atomic time in '/
     :                /'input dataset', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get parameters of ephemeris (units JD)
      CALL MSG_PRNT('Enter Ephemeris Coeffs : a(1) + a(2)*N + a(3)*N*N')
      CALL USI_GET0D('COEFF1', COEFF(1), STATUS)
 20   CALL USI_GET0D('COEFF2', COEFF(2), STATUS)
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( COEFF(2) .LT. 0.0 ) ) THEN
        CALL MSG_PRNT( 'Negative period supplied !' )
        CALL USI_CANCL( 'COEFF2', STATUS )
        GOTO  20
      END IF
      CALL USI_GET0D('COEFF3', COEFF(3), STATUS)

*    Write ephemeris values to EPHEMERIS
      WRITE (EPHEMERIS(1), '(A,G15.5)') 'Coefficient 1 = ',COEFF(1)
      WRITE (EPHEMERIS(2), '(A,G15.5)') 'Coefficient 2 = ',COEFF(2)
      WRITE (EPHEMERIS(3), '(A,G15.5)') 'Coefficient 3 = ',COEFF(3)

*    Convert to MJD
      COEFF(1) = COEFF(1) - 2400000.5

*    Then to atomic time
      CALL TIM_MJD2TAI( COEFF(1), EPHEM_TAI )

*    Find difference from dataset reference time in seconds, and other
*    coefficents in seconds
      COEFF(1) = (EPHEM_TAI - BASE_TAI ) * 86400.0D0
      COEFF(2) = COEFF(2) * 86400.0D0
      COEFF(3) = COEFF(3) * 86400.0D0

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Write output file
      CALL ADI_FCOPY( IFID, OFID, STATUS )

*    Delete output AXIS
      CALL BDI_CREAXES( OFID, NDIMS, STATUS )

*    Copy axes except time axis
      SKIP = 0
      IF (NDIMS .GT. 1) THEN
        DO AXN1 = 1, NDIMS
          IF (AXN1 .NE. TAXIS) THEN
            AXN2 = AXN1 - SKIP
            CALL BDI_COPAXIS( IFID, OFID, AXN1, AXN2, STATUS )
          ELSE
            SKIP = 1
          END IF
        END DO
      END IF

*    Create & map output phase axis
      CALL BDI_CREAXVAL( OFID, NDIMS, .FALSE., NPTS, STATUS)
      CALL BDI_MAPAXVAL( OFID, 'WRITE', NDIMS, OUTAXPTR, STATUS)

*    Write label & units
      CALL BDI_PUTAXTEXT( OFID, NDIMS, 'Phase', 'Unitless', STATUS )

*    Execute time-phase conversion
      CALL PHASE_DOIT( NPTS, %VAL(INAXPTR), COEFF, %VAL(OUTAXPTR),
     :                                                    STATUS )

*    History
      CALL HSI_ADD( OFID, VERSION, STATUS )
      CALL HSI_PTXT( OFID, 3, EPHEMERIS, STATUS )

*    Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+  PHASE_DOIT - Calculate phase bins given time axis bin centres
      SUBROUTINE PHASE_DOIT( NPTS, TIME, COEFF, OUTPHASE, STATUS )
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

           IF ( I .EQ. 1 ) THEN
              IF ( ACCPHA .LT. 0.0 .AND. ACCPHA .NE. INT(ACCPHA)) THEN
                 CYCLES = - NINT(ABS( ACCPHA ) + 0.5)
              ELSE
                 CYCLES = INT( ACCPHA )
              END IF
           END IF
           OUTPHASE(I) = ACCPHA - REAL( CYCLES )

        ELSE
           CALL MSG_PRNT( 'Period has gone negative! - aborting' )
           STATUS = SAI__ERROR
           GOTO 99
        END IF

      END DO

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'EXERR', '...from PHASE_DOIT', STATUS )
      END IF

      END



*+  PHASE_ACCPHA - Returns phase of a time since epoch
      SUBROUTINE PHASE_ACCPHA( EPOCH, PERIOD, PDIFF, ATIME, ACCPHA,
     :                                                      ISTAT )
*    Description :
*
*    History :
*
*     14 Jun 90 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      DOUBLE PRECISION        EPOCH, PERIOD, PDIFF, ATIME
*
*    Export :
*
      DOUBLE PRECISION        ACCPHA
      INTEGER                 ISTAT
*
*    Local variables :
*
      DOUBLE PRECISION        DELTAT,T1
*-

      ISTAT  = 0

      DELTAT = ATIME - EPOCH
      T1 = PDIFF * DELTAT / PERIOD

*    Trap zero PDIFF
      IF ( ABS(T1) .LT. 1.0E-10 ) THEN
         ACCPHA = DELTAT / PERIOD
      ELSE
         IF ( (PERIOD-DELTAT*PDIFF) .GT. PERIOD ) THEN
            ISTAT = 1
         ELSE
            ACCPHA = LOG(1.0D0+(EPOCH+DELTAT)*PDIFF/PERIOD) - LOG(1.0D0+
     :                                               EPOCH*PDIFF/PERIOD)
            ACCPHA = ACCPHA / PDIFF
         END IF
      END IF

      END
