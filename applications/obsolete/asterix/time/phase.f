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
*     13 Dec 1995 : V2.0-0 ADI port
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
      INTEGER                	NDIMS                   ! Number of dimensions
      INTEGER                	LDIM(ADI__MXDIM)        ! Input dimensions
      INTEGER			OFID			! Output dataset id
      INTEGER                	TAXIS			! Time axis number
      INTEGER			TIMID			! Timing info

      LOGICAL                	OK                      ! Input data ok?
*
*    Version :
*
      CHARACTER*22		VERSION
        PARAMETER         	( VERSION = 'PHASE Version 2.0-0' )
*-

*    Version Announcement
      CALL MSG_PRNT( VERSION )

*  Initialize ASTERIX
      CALL AST_INIT()

*  Associate input and output datasets
      CALL USI_ASSOC( 'INP', 'BinDS', 'READ', IFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      CALL USI_CLONE( 'INP', 'OUT', 'BinDS', OFID, STATUS )

*  Get dimensions of axis structure
      CALL BDI_GETSHP( IFID, ADI__MXDIM, LDIM, NDIMS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check time axis exists
      CALL BDI0_FNDAXC( IFID, 'T', TAXIS, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_FLUSH( STATUS )
        CALL MSG_PRNT( 'Assuming time axis is axis number one' )
        TAXIS = 1
      END IF

*  Check time axis
      CALL BDI_AXCHK( IFID, TAXIS, 'Data', OK, STATUS )
      IF ( OK ) THEN
        CALL BDI_AXMAPD( IFID, TAXIS, 'Data', 'READ', INAXPTR, STATUS )

      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'FATAL ERROR: Invalid time axis', STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get observation start time
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

*  Get parameters of ephemeris (units JD)
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

*  Convert to MJD
      COEFF(1) = COEFF(1) - 2400000.5

*  Then to atomic time
      CALL TCI_MJD2TAI( COEFF(1), EPHEM_TAI )

*  Find difference from dataset reference time in seconds, and other
*  coefficents in seconds
      COEFF(1) = (EPHEM_TAI - BASE_TAI ) * 86400.0D0
      COEFF(2) = COEFF(2) * 86400.0D0
      COEFF(3) = COEFF(3) * 86400.0D0
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Create & map output phase axis
      CALL BDI_AXMAPR( OFID, TAXIS, 'Data', 'WRITE', OUTAXPTR, STATUS )
      CALL BDI_AXPUT0C( OFID, TAXIS, 'Label', 'Phase', STATUS )
      CALL BDI_AXPUT0C( OFID, TAXIS, 'Units', 'unitless', STATUS )

*  Execute time-phase conversion
      CALL PHASE_DOIT( DIMS(TAXIS), %VAL(INAXPTR), COEFF,
     :                 %VAL(OUTAXPTR), STATUS )

*  History
      CALL HSI_ADD( OFID, VERSION, STATUS )
      CALL HSI_PTXT( OFID, 3, EPHEMERIS, STATUS )

*  Tidy up
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
        CALL AST_REXIT( 'PHASE_DOIT', STATUS )
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
