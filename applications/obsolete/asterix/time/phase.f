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
*     13 Jun 90 : V1.2-0  Various bugs fixed. Now always maps axis data, as
*                         algorithm did it anyway! Traps -ve periods. (DJA)
*     24 Jul 91 : V1.5-0  Uses updated TAI definition (DJA)
*     29 Jul 92 : V1.6-0  Explicitly create o/p axes structure (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) ILOC                        ! Input dataset
      CHARACTER*(DAT__SZLOC) OLOC                        ! Output dataset
      CHARACTER*(DAT__SZLOC) HLOC                        ! Input HEADER
      CHARACTER*(80)         EPHEMERIS(3)                ! History entry

      DOUBLE PRECISION       BASE_TAI                    ! Start time of observation (days after 1st Jan 1972)
      DOUBLE PRECISION       COEFF(3)                    ! Ephemeris coefficients
      DOUBLE PRECISION       EPHEM_TAI                   ! Ephemeris reference
                                                         ! in TAI

      INTEGER                INAXPTR                     ! Input time axis
      INTEGER                OUTAXPTR                    ! Output phase axis
      INTEGER                NPTS                        ! No of data poits
      INTEGER                NDIMS                       ! Number of dimensions
      INTEGER                LDIM(DAT__MXDIM)            ! Input dimensions
      INTEGER                SKIP                        ! Used in copying
      INTEGER                AXN1                        ! axes from input
      INTEGER                AXN2                        ! to output

      INTEGER                TAXIS, XAXIS, YAXIS         ! Axis numbers

      LOGICAL                OK                          ! Input data ok?
      LOGICAL                INPRIM                      ! Input primitive?
      LOGICAL                REG                         ! Input time axis regular?
*
*    Version :
*
      CHARACTER*22           VERSION
        PARAMETER         ( VERSION = 'PHASE Version 1.6-0' )
*-

*    Version Announcement
      CALL MSG_PRNT( VERSION )

*    Initialize ASTERIX subroutines
      CALL AST_INIT()

*    Associate input and output datasets
      CALL USI_ASSOC2('INP', 'OUT', 'READ', ILOC, OLOC, INPRIM, STATUS)
      IF ( INPRIM ) THEN
        CALL MSG_PRNT('FATAL ERROR: Input object must be a dataset')
        STATUS = SAI__ERROR
        GOTO 99
      END IF

*    Get dimensions of axis structure (from data array).
      CALL BDA_CHKDATA(ILOC, OK, NDIMS, LDIM, STATUS)
      IF (.NOT. OK) THEN
        CALL MSG_PRNT('FATAL ERROR: No data!')
        STATUS = SAI__ERROR
        GOTO 99
      END IF

*    Check time axis exists
      CALL AXIS_FINDXYT( ILOC, NDIMS, XAXIS, YAXIS, TAXIS, STATUS )
      IF ( TAXIS .LE. 0 ) THEN
        CALL MSG_PRNT('FATAL ERROR: No time axis!')
        STATUS = SAI__ERROR

      ELSE
        CALL BDA_CHKAXVAL(ILOC, TAXIS, OK, REG, NPTS,  STATUS )

        IF ( OK ) THEN
          CALL BDA_MAPAXVAL(ILOC, 'READ', TAXIS, INAXPTR, STATUS )

        ELSE
          CALL MSG_PRNT('FATAL ERROR: Invalid time axis')
          STATUS = SAI__ERROR

        END IF
      END IF

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get observation start time
      CALL BDA_LOCHEAD( ILOC, HLOC, STATUS )
      CALL HDX_OK( HLOC, 'BASE_TAI', OK, STATUS )

      IF ( OK ) THEN
        CALL CMP_GET0D( HLOC, 'BASE_TAI', BASE_TAI, STATUS )
      ELSE
        CALL MSG_PRNT( 'FATAL ERROR: No BASE_TAI in file header' )
        STATUS = SAI__ERROR
      END IF

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get parameters of ephemeris (units JD)
      CALL MSG_PRNT('Enter Ephemeris Coeffs : a(1) + a(2)*N + a(3)*N*N')
      CALL PAR_GET0D('COEFF1', COEFF(1), STATUS)
 20   CALL PAR_GET0D('COEFF2', COEFF(2), STATUS)
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( COEFF(2) .LT. 0.0 ) ) THEN
        CALL MSG_PRNT( 'Negative period supplied !' )
        CALL PAR_CANCL( 'COEFF2', STATUS )
        GOTO  20
      END IF
      CALL PAR_GET0D('COEFF3', COEFF(3), STATUS)

*    Write ephemeris values to EPHEMERIS
      WRITE (EPHEMERIS(1), '(15X,A,G15.5)') 'Coefficient 1 = ',COEFF(1)
      WRITE (EPHEMERIS(2), '(15X,A,G15.5)') 'Coefficient 2 = ',COEFF(2)
      WRITE (EPHEMERIS(3), '(15X,A,G15.5)') 'Coefficient 3 = ',COEFF(3)

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
      CALL HDX_COPY( ILOC, OLOC, STATUS )

*    Delete output AXIS
      CALL BDA_CREAXES( OLOC, NDIMS, STATUS )

*    Copy axes except time axis
      SKIP = 0
      IF (NDIMS .GT. 1) THEN
        DO AXN1 = 1, NDIMS
          IF (AXN1 .NE. TAXIS) THEN
            AXN2 = AXN1 - SKIP
            CALL BDA_COPAXIS (ILOC, OLOC, AXN1, AXN2, STATUS)
          ELSE
            SKIP = 1
          END IF
        END DO
      END IF

*    Create & map output phase axis
      CALL BDA_CREAXVAL( OLOC, NDIMS, .FALSE., NPTS, STATUS)
      CALL BDA_MAPAXVAL( OLOC, 'WRITE', NDIMS, OUTAXPTR, STATUS)

*    Write label & units
      CALL BDA_PUTAXLABEL( OLOC, NDIMS, 'Phase',    STATUS )
      CALL BDA_PUTAXUNITS( OLOC, NDIMS, 'Unitless', STATUS )

*    Execute time-phase conversion
      CALL PHASE_DOIT( NPTS, %VAL(INAXPTR), COEFF, %VAL(OUTAXPTR),
     :                                                    STATUS )

*    History
      CALL HIST_ADD( OLOC, VERSION, STATUS )
      CALL HIST_PTXT( OLOC, 3, EPHEMERIS, STATUS )

*    Tidy up
 99   CALL AST_CLOSE
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
      INCLUDE 'PAR_ERR'
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
