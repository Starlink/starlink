*+  SINFIT - Computes power spectrum for 1D data with general spacing
      SUBROUTINE SINFIT( STATUS )
*    Description :
*     Calls subroutine PERIODS to calculate the power spectrum of a
*     1D data object by least squares fitting of sine and cos waves.
*     Calcluates amplitude & phase as a function of frequency, where:
*        fit(frequency) = amplitude * SIN( frequency*angle + phase )
*     The phase is writen into a DATA_PHASE component, and the power
*     calculated as below, is written into the DATA_ARRAY component.
*     Normalization used is  power = (wave amplitude/2)**2 = F.T. of
*     autocovariance.
*     The data mean is removed before calculation of the periodogram.
*     Expected noise power due to data errors is also calculated.
*    Method :
*     See M.N.R.A.S. 196,p.583
*    Deficiencies :
*     Slow compared to FFT methods, hence use the latter for regularly
*     spaced data, unless variable data weights are important.
*    Bugs :
*    Authors :
*     Trevor Ponman   (BHVAD::TJP)
*     Phillip Andrews (BHVAD::PLA)
*     David J. Allan  (BHVAD::DJA)
*
*    History :
*
*      5 Dec 83 : Original.
*     13 Dec 84 : Version 4.
*     17 Dec 85 : Case of zero data error handled (v 0.4-1).
*     10 Jan 86 : Now uses TIM_PERIODS - zero frequency properly handled.
*     22 Jan 86 : Sensible message if data array is too long.
*      2 Mar 87 : Converted to ROSAT spec, renamed SINFIT (old name PGRAM).
*     25 Aug 87 : Bug fix to PHASE: now gives phase for SIN curve, in range
*                 0, 2PI. (pla)
*     25 Aug 88 : Major rewrite, now maps input file & creates dynamical
*                 storage for work arrays. Also now uses SINFIT_PGRAM, a
*                 rewritten version of TIM_PERIODS. (PLA)
*     13 Jun 90 : V1.2-0 Several bug fixes done (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     20 Apr 95 : V1.8-1 Updated data interface. Phase object written to
*                        separate dataset (DJA)
*     12 Dec 1995 V2.0-0 ADI port (DJA)
*    Type Definitions :
*
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'PAR_ERR'
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN
*
*    Local variables :
*
      CHARACTER*80           DUNITS        ! Data units
      CHARACTER*80           TUNITS        ! Time units
      CHARACTER*80           TEXT(5)       ! For history record

      REAL                   	BASEFREQ      		! Base freq.
      REAL                   	FCHNG         		! The frequency at which SINFIT_PGRAM changes algorithm.
      REAL                   	FREQSTEP      		! Freq.step
      REAL                   	NPOWER        		! Noise power
      REAL			SPARR(2)		! Spaced array data

      INTEGER                AXPTR         ! Pointer to input axis(1)
      INTEGER                DATPTR        ! Pointer to input data
      INTEGER                QPTR          ! Pointer to input QUALITY
      INTEGER                VARPTR        ! Pointer to input VARIANCE
      INTEGER                COSPTR        ! Pointer to cosine amp array
      INTEGER                SINPTR        ! Pointer to sin amp. array
      INTEGER                WORKPTR       ! Pointer to work array
      INTEGER                PWRPTR        ! Pointer to output data
      INTEGER                VAROUT        ! Pointer to output VARIANCE
      INTEGER                PHAPTR        ! Pointer to output PHASE array
      INTEGER                I             ! Dummy for DO loops.
      INTEGER                IERR          ! Error flag from PERIODS
      INTEGER			IFID			! Input dataset id
      INTEGER                LDIM(ADI__MXDIM)! Length of each dimension.
      INTEGER                NBAD          ! # of bad quality points
      INTEGER                NDATA         ! No.of data points
      INTEGER                NDIMS         ! Number of dimension of input data
      INTEGER                NFREQ         ! No.of frequencies
      INTEGER                NLINES        ! No. of history text lines
      INTEGER			OFID			! Output dataset id
      INTEGER			PFID			! Phase spectrum id

      LOGICAL                AXOK          ! AXIS(1) OK
      LOGICAL                OK
      LOGICAL                PHASE         ! Create a PHASE_SPECRUM too?
      LOGICAL                PRIM          ! Used to test for primitive
      LOGICAL                QUALOK        ! QUALITY OK
      LOGICAL                VAROK         ! VARIANCE OK
*
*    Version id :
*
      CHARACTER*21		VERSION
        PARAMETER           	( VERSION = 'SINFIT Version 2.1-0' )
*-

*  Check status.
      IF (STATUS .NE. SAI__OK) RETURN

*  Version ID.
      CALL MSG_PRNT(VERSION)

*  Initialize ASTERIX
      CALL AST_INIT()

*  Obtain data object name.
      CALL USI_ASSOC( 'INP', 'BinDS|Array', 'READ', IFID, STATUS )

*  Create output dataset
      CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )

*  Map input data.
      CALL ADI_DERVD( IFID, 'Array', PRIM, STATUS )
      CALL BDI_CHK( IFID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( IFID, ADI__MXDIM, LDIM, NDIMS, STATUS )
      IF (OK) THEN
        CALL BDI_MAPR( IFID, 'Data', 'READ', DATPTR, STATUS )
        CALL ARR_SUMDIM( NDIMS, LDIM, NDATA, STATUS )
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'FATAL ERROR: Unable to find suitable data'/
     :                                          /' component.', STATUS )
      END IF
      IF (STATUS .NE. SAI__OK) GOTO 99
      IF ( NDIMS .NE. 1 ) THEN
        CALL MSG_PRNT('WARNING: Data is not one-dimensional')
      END IF

*  Axis data present?
      CALL BDI_AXCHK( IFID, 1, 'Data', AXOK, STATUS )
      IF ( AXOK ) THEN
        CALL BDI_AXMAPR( IFID, 1, 'Data', 'READ', AXPTR, STATUS)

      ELSE
        CALL MSG_PRNT('WARNING: Axis data invalid'//
     :                         ' - proceeding assuming regular spacing')

        CALL DYN_MAPR (1, NDATA, AXPTR, STATUS)
        CALL ARR_REG1R( 1.0, 1.0, NDATA, %VAL(AXPTR), STATUS )

      END IF

*  Map VARIANCE if present
      CALL BDI_CHK( IFID, 'Variance', VAROK, STATUS )
      IF ( VAROK ) THEN
        CALL BDI_MAPR( IFID, 'Variance', 'READ', VARPTR, STATUS )
      END IF

*  User information
      CALL MSG_SETI ('NDAT', NDATA)
      CALL MSG_PRNT('^NDAT data points entered')

      IF ( AXOK ) THEN
        CALL BDI_AXGET0C( IFID, 1, 'Units', TUNITS, STATUS )

        IF ( TUNITS .GT. ' ' ) THEN
          CALL MSG_SETC ('TUNITS', TUNITS)
          CALL MSG_PRNT('The time units are ^TUNITS')

        ELSE
          CALL MSG_PRNT('No time units specified in input file')

        END IF
      END IF

*  Map QUALITY as a logical.
      CALL BDI_CHK( IFID, 'Quality', QUALOK, STATUS )
      IF ( QUALOK ) THEN
        CALL BDI_MAPL( IFID, 'LogicalQuality', 'READ', QPTR, STATUS )
        CALL ARR_CNT1L( NDATA, %VAL(QPTR), .FALSE., NBAD, STATUS )
        IF ( NBAD .GT. 0 ) THEN
          CALL MSG_SETI( 'NBAD', NBAD )
          CALL MSG_PRNT( 'There are ^NBAD bad quality points'/
     :                                         /' in the data' )
        ELSE
          CALL BDI_UNMAP( IFID, 'LogicalQuality', QPTR, STATUS )
          QUALOK = .FALSE.
        END IF

      END IF

*  User input.
      CALL USI_GET0R( 'BASE', BASEFREQ, STATUS )
      CALL USI_GET0R( 'INC', FREQSTEP, STATUS )
      CALL USI_GET0I( 'NUM', NFREQ, STATUS )

*  Phase output dataset
      CALL USI_CREAT( 'PHASE', ADI__NULLID, PFID, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
        CALL ERR_ANNUL( STATUS )
        PHASE = .FALSE.
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
        PHASE = .TRUE.
      ELSE
        GOTO 99
      END IF

*  Create dynamic arrays
      CALL DYN_MAPR( 1, NFREQ, COSPTR, STATUS )
      CALL DYN_MAPR( 1, NFREQ, SINPTR, STATUS )
      CALL DYN_MAPR( 1, NFREQ, WORKPTR, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Create interface to output
      CALL BDI_LINK( 'PowerSpectrum', 1, NFREQ, 'REAL', OFID, STATUS )

*  Create components in output dataset
      CALL BDI_MAPR( OFID, 'Data', 'WRITE', PWRPTR, STATUS )
      SPARR(1) = BASEFREQ
      SPARR(2) = FREQSTEP
      CALL BDI_AXPUT1R( OFID, 1, 'SpacedData', 2, SPARR, STATUS )
      CALL BDI_AXPUT0C( OFID, 1, 'Label', 'Frequency', STATUS )

      CALL BDI_PUT0C( OFID, 'Label', 'Power', STATUS )

*  Copy data units
      CALL BDI_GET0C( IFID, 'Units', DUNITS, STATUS )
      IF ( DUNITS .GT. ' ' ) THEN
        DUNITS = '('//DUNITS(1:CHR_LEN(DUNITS))//')**2'
        CALL BDI_PUT0C( OFID, 'Units', DUNITS, STATUS )
      END IF

      IF ( TUNITS .GT. ' ' ) THEN
        TUNITS = '('//TUNITS(1:CHR_LEN(TUNITS))//')**-1'
        CALL BDI_PUT0C( OFID, 1, 'Units', TUNITS, STATUS )
      END IF

*  Copy title
      CALL BDI_COPY( IFID, 'Title', OFID, ' ', STATUS )

*  Create output variance
      CALL BDI_MAPR( OFID, 'Variance', 'WRITE', VAROUT, STATUS )

*  Copy ancillary stuff
      CALL UDI_COPANC( IFID, 'grf', OFID, STATUS )

*  Create phase spectrum?
      IF ( PHASE ) THEN

        CALL BDI_LINK( 'PhaseSpectrum', 1, NFREQ, 'REAL', PFID, STATUS )
        CALL BDI_MAPR( PFID, 'Data', 'WRITE', PHAPTR, STATUS )
        CALL BDI_PUT0C( PFID, 'Label', 'Phase', STATUS )
        CALL BDI_PUT0C( PFID, 'Units', 'radians', STATUS )

        SPARR(1) = BASEFREQ
        SPARR(2) = FREQSTEP
        CALL BDI_AXPUT1R( PFID, 1, 'SpacedData', 2, SPARR, STATUS )
        CALL BDI_AXPUT0C( PFID, 1, 'Label', 'Frequency', STATUS )
        IF ( TUNITS .GT. ' ' ) THEN
          CALL BDI_AXPUT0C( OFID, 1, 'Units', TUNITS, STATUS )
        END IF

*    Copy ancillary stuff
        CALL UDI_COPANC( IFID, 'grf', PFID, STATUS )
        CALL HSI_COPY( IFID, PFID, STATUS )
        CALL HSI_ADD( PFID, VERSION, STATUS )

      END IF

*    Compute periodogram & phase.
      FCHNG = - 1.0
      CALL SINFIT_PGRAM (NDATA, %VAL(AXPTR), %VAL(DATPTR), %VAL(VARPTR),
     :      %VAL(QPTR), VAROK, QUALOK, BASEFREQ, FREQSTEP, NFREQ, FCHNG,
     :          %VAL(WORKPTR), %VAL(COSPTR), %VAL(SINPTR), %VAL(PWRPTR),
     :                               %VAL(VAROUT), NPOWER, IERR, STATUS)

*    Check status.
      IF (STATUS .NE. SAI__OK) GOTO 99

      IF (IERR .GT. 0) THEN
        CALL MSG_SETI ('NERR', IERR)

        IF (IERR .EQ. 1) THEN
          CALL MSG_PRNT('WARNING: ^NERR inversion error encountered'
     :                           //' - corresponding POWER set to zero')

        ELSE
          CALL MSG_PRNT('WARNING: ^NERR inversion errors encountered'
     :                          //' - corresponding POWERs set to zero')

        END IF
      END IF

      IF (PHASE) THEN
*      Calculate the phase
         CALL SINFIT_PHASE (NFREQ, %VAL(SINPTR), %VAL(COSPTR),
     :                                                     %VAL(PHAPTR))

      END IF

*  Copy history
      CALL HSI_COPY( IFID, OFID, STATUS )

*  Add new history component
      CALL HSI_ADD( OFID, VERSION, STATUS )

      TEXT(1) = 'Input {INP}'
      I       = 1
      NLINES  = 5

      IF (PHASE) THEN
        TEXT(2) =
     :  'Phase spectrum written into MORE.ASTERIX.PHASE_SPECTRUM'
        I = I + I

      END IF

      CALL USI_TEXT( I, TEXT, NLINES, STATUS )
      CALL HSI_PTXT( OFID, NLINES, TEXT, STATUS )

*  Exit
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END




*+  SINFIT_PHASE - Calculates phase from Cos & Sin amplitudes.
      SUBROUTINE SINFIT_PHASE( NFREQ, COSAMP, SINAMP, PHASE )
*    Description :
*     Calculates phase from cos & sin amplitudes
*    History :
*     24/8/88: original (pla@uk.ac.bham.sr.star)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      INTEGER                NFREQ         ! No.of frequencies

      REAL                   COSAMP(NFREQ) ! Cosine amplitudes
      REAL                   SINAMP(NFREQ) ! Sine amplitudes
*    Export :
      REAL                   PHASE(NFREQ)  ! Phase in radians
*    Local constants :
      REAL                   PI
         PARAMETER         ( PI = 3.1415926 )
*    Local variables :
      INTEGER                I             ! Counter for loop
*-
      DO I = 1, NFREQ
         IF (SINAMP(I) .NE. 0.0) THEN
*          This is the phase of the peak of the sinusoid for a sine wave.
            PHASE(I) = ATAN( ABS(COSAMP(I) / SINAMP(I)) )

            IF ( (COSAMP(I) .EQ. ABS(COSAMP(I))) .AND.
     :                            (SINAMP(I) .EQ. ABS(SINAMP(I))) ) THEN
               PHASE(I) = PHASE(I)

            ELSE IF ( ( COSAMP(I) .NE. ABS(COSAMP(I)) ) .AND.
     :                          ( SINAMP(I) .EQ. ABS(SINAMP(I)) ) ) THEN
               PHASE(I) = PHASE(I) + (PI / 2.0)

            ELSE IF ( ( COSAMP(I) .NE. ABS(COSAMP(I)) ) .AND.
     :                         ( SINAMP(I) .NE. ABS(SINAMP(I)) ) ) THEN
               PHASE(I) = PHASE(I) + PI

            ELSE IF ( ( COSAMP(I) .EQ. ABS(COSAMP(I)) ) .AND.
     :                          ( SINAMP(I) .NE. ABS(SINAMP(I)) ) ) THEN
               PHASE(I) = PHASE(I) + (3.0 * PI / 2.0)

            END IF
         ELSE
            PHASE(I) = PI / 2.0

         END IF
      END DO
      END




*+ SINFIT_PGRAM - computes periodogram
      SUBROUTINE SINFIT_PGRAM( NPTS, TIME, DATA, VARIN, QUAL, VAROK,
     :           QUALOK, BASEFREQ, FREQSTEP, NFREQ, FCHNG, WORK, COSAMP,
     :                      SINAMP, POWER, VAROUT, NOISE, IERR, STATUS )

*    Description :
*	Computes periodogram of data Y(X) for any spacing of X by least
*	squares fitting of cos & sine waves.
*	See M.N.R.A.S. 196,p.583 for details of the method.
*	Below frequency V1, cos & sine waves are fitted simultaneously by
*	a small matrix inversion, or set to zero if it fails.
*	Normalized so that periodogram = F.T.of autocovariance. Hence wave
*	amplitude = 2*SQRT(power).
*	The data mean is removed to minimize errors in the periodogram
*       arising from non-cancellation of cross terms.
*	Now fixed to give zero power at v=0, but will still give a large,
*	invalid result at the Nyquist frequency for regularly spaced data.
*	This arises from a genuine ambiguity in fitted sine amplitude.
*
*	Warning - if the fit is weighted then none of the variances may
*	be zero.
*    History :
*     24/8/88:  original based on earlier TIM_PERIODS (pla@uk.ac.bham.sr.star)

*    Type Definitions :
      IMPLICIT NONE
*    Include
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER                STATUS
*    Import : (in same order as arguments)
      INTEGER                NPTS                      ! Number of data points

      REAL                   TIME(*)                   ! Axis data
      REAL                   DATA(*)                   ! Data values
      REAL                   VARIN(*)                  ! Input VARIANCE

      LOGICAL                QUAL(*)                   ! Quality
      LOGICAL                VAROK                     ! Use VARIN ?
      LOGICAL                QUALOK                    ! Use QUAL ?

      REAL                   BASEFREQ                  ! Base frequency
      REAL                   FREQSTEP                  ! Frequency step

      INTEGER                NFREQ                     ! No of frequencies

      REAL                   FCHNG                     ! lowest frequency at which c * s
                                                       ! cross terms may be assumed to vanish.
      REAL                   WORK(*)                   ! Work array

*    Export :
      REAL                   COSAMP(*)                 ! Cosine amplitudes
      REAL                   SINAMP(*)                 ! Sine amplitudes
      REAL                   POWER(*)                  ! Power spectrum
      REAL                   VAROUT(*)                 ! Output VARIANCE
      REAL                   NOISE                     ! Noise power

      INTEGER                IERR                      ! 0=OK, 1=singularity
                                                       ! encountered in matrix
                                                       ! inversion

*    Local constants :
      REAL                   PI2                       ! 2.0 * PI
         PARAMETER         ( PI2 = 6.2831852 )

*    Local variables :
      INTEGER                I, J                      ! Loop counters
      INTEGER                NERR                      ! IERR <> 0 count
      INTEGER                START                     ! Start value for
                                                       ! frequency loops

      REAL                   A(3)                      ! Symmetric 2*2 matrix
      REAL                   C1, C2, S1, S2,TEMP,TEMP2 ! Variables used in the
                                                       ! calculation
      REAL                   MEAN                      ! Mean or weighted mean
                                                       ! of DATA
      REAL                   ERROR                     ! Error on mean - not
                                                       ! used
      REAL                   VAR                       ! Current value of VARIN

      LOGICAL                CONTINUE                  ! Used to skip bad
                                                       ! quality data.
      LOGICAL                DEMEAN
*-

*    Check status
      IF (STATUS .NE. SAI__OK) RETURN

*    Initialize values
      CONTINUE = .TRUE.
      IERR     = 0
      NERR     = 0
      VAR      = 1.0

*    Only positive frequencies are allowed
      IF (BASEFREQ .LT. 0.0) THEN
         BASEFREQ = 0.0
         START = 2

      ELSE
         START = 1

      END IF

*    Set up default frequency at which algorithm changes
      IF (FCHNG .LT. 0.0) THEN
         FCHNG = 5.0 / ( TIME( NPTS ) - TIME( 1 ) )

      END IF

*    Initialize arrays to zero
      DO I = 1, NFREQ
         COSAMP(I) = 0.0
         SINAMP(I) = 0.0
         VAROUT(I) = 0.0
         POWER(I)  = 0.0
         WORK(I)   = 0.0

      END DO

      CALL USI_GET0L ('DEMEAN', DEMEAN, STATUS)

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 99

      IF (DEMEAN) THEN
*      Calcualte data mean
        IF (VAROK) THEN
           CALL MATH_WTMEANR (NPTS, DATA, VARIN, QUAL, QUALOK, MEAN,
     :                                                    ERROR, STATUS)
           CALL MSG_SETR ('MEAN', MEAN)
           CALL MSG_PRNT('Weighted mean of ^MEAN subtracted')

        ELSE
           CALL MATH_MEANR (NPTS, DATA, QUAL, QUALOK, MEAN, ERROR,
     :                                                           STATUS)
           CALL MSG_SETR ('MEAN', MEAN)
           CALL MSG_PRNT('Mean of ^MEAN subtracted')

        END IF

*      Check status
        IF (STATUS .NE. SAI__OK) GOTO 99

*      Loop over data
        DO I = 1, NPTS
          IF ( QUALOK ) THEN
            CONTINUE = QUAL(I)

          END IF

          IF ( VAROK ) THEN
            VAR = VARIN(I)
          END IF

          IF (CONTINUE .AND. (VAR .GT. 0.0)) THEN
            C1 = COS (PI2 * TIME(I) * BASEFREQ)
            S1 = SIN (PI2 * TIME(I) * BASEFREQ)
            C2 = COS (PI2 * TIME(I) * FREQSTEP)
            S2 = SIN (PI2 * TIME(I) * FREQSTEP)

*          Loop over frequencies
            DO J = START, NFREQ

              COSAMP(J) = COSAMP(J) + ((DATA(I) - MEAN) * C1 / VAR) ! All
              SINAMP(J) = SINAMP(J) + ((DATA(I) - MEAN) * S1 / VAR) ! used
              POWER(J)  = POWER(J)  + ( C1 * C1 / VAR )             ! as
              VAROUT(J) = VAROUT(J) + ( S1 * S1 / VAR )             ! work
              WORK(J)   = WORK(J)   + ( C1 * S1 / VAR )             ! space.

*            Calculate next cos & sin values.
              TEMP = (C1 * C2) - (S1 * S2)
              S1   = (C1 * S2) + (S1 * C2)
              C1   = TEMP

            END DO
          END IF
        END DO
      ELSE
*      Loop over data
        DO I = 1, NPTS
          IF ( QUALOK ) THEN
            CONTINUE = QUAL(I)

          END IF

          IF ( VAROK ) THEN
            VAR = VARIN(I)
          END IF

          IF (CONTINUE .AND. (VAR .GT. 0.0)) THEN
            C1 = COS (PI2 * TIME(I) * BASEFREQ)
            S1 = SIN (PI2 * TIME(I) * BASEFREQ)
            C2 = COS (PI2 * TIME(I) * FREQSTEP)
            S2 = SIN (PI2 * TIME(I) * FREQSTEP)

*          Loop over frequencies
            DO J = START, NFREQ

              COSAMP(J) = COSAMP(J) + (DATA(I) * C1 / VAR) ! All
              SINAMP(J) = SINAMP(J) + (DATA(I) * S1 / VAR) ! used
              POWER(J)  = POWER(J)  + ( C1 * C1 / VAR )    ! as
              VAROUT(J) = VAROUT(J) + ( S1 * S1 / VAR )    ! work
              WORK(J)   = WORK(J)   + ( C1 * S1 / VAR )    ! space.

*            Calculate next cos & sin values.
              TEMP = (C1 * C2) - (S1 * S2)
              S1   = (C1 * S2) + (S1 * C2)
              C1   = TEMP

            END DO
          END IF
        END DO
      END IF

*    Combine data summations for each frequency
      DO J = START, NFREQ
         IF (BASEFREQ + (REAL(J-1) * FREQSTEP) .LT. FCHNG) THEN
*          Low frequency case - fit cos & sine simultaneously
            A(1) = POWER(J)
            A(2) = WORK(J)
            A(3) = VAROUT(J)

*          Invert the symmetric 2*2 matrix A
            CALL MATH_S22MATINVR (A, IERR)

            IF (IERR .EQ. 0) THEN
*             Inversion OK
               TEMP      = (A(1) * COSAMP(J)) + (A(2) * SINAMP(J))
               SINAMP(J) = (A(2) * COSAMP(J)) + (A(3) * SINAMP(J))
               COSAMP(J) = TEMP

            ELSE
*              Inversion failed
                IERR      = 0
                NERR      = NERR + 1
                COSAMP(J) = 0.0
                SINAMP(J) = 0.0

            END IF
         ELSE
*          Fit them independently
            COSAMP(J) = COSAMP(J) / POWER(J)
            SINAMP(J) = SINAMP(J) / VAROUT(J)

         END IF
      END DO

*    Noise power
      IF ( VAROK ) THEN
         NOISE    = 0.0
         CONTINUE = .TRUE.

         DO I = 1, NPTS
            IF ( QUALOK ) THEN
               CONTINUE = QUAL(I)
            END IF

            IF (CONTINUE .AND. VARIN(I) .GT. 0.0) THEN
               NOISE = NOISE + (1.0 / VARIN(I))

            END IF
         END DO
         NOISE = 1.0 / NOISE

      ELSE
         NOISE = 0.0

      END IF

*    Combine to give power and error
      DO J = START, NFREQ
         IF ( (POWER(J) .GT. 0.0) .AND. (VAROUT(J) .GT. 0.0) ) THEN
            TEMP2 = POWER(J) + NOISE
            TEMP = (( COSAMP(J)*COSAMP(J) - (1.0 / (2.0 * TEMP2)) )
     :                                                       / TEMP2) +
     :             (( SINAMP(J)*SINAMP(J) - (1.0 / (2.0 * VAROUT(J))) )
     :                                                     / VAROUT(J))

         END IF

         IF ( (TEMP .LT. 0.0) .OR. (POWER(J) .LE. 0.0) .OR.
     :                                       (VAROUT(J) .LE. 0.0) ) THEN
            TEMP = 0.0

         END IF

         VAROUT(J) = TEMP / 4.0
         POWER(J)  = ( COSAMP(J)*COSAMP(J) + SINAMP(J)*SINAMP(J) ) / 4.0

      END DO

*    Zero frequency case
      IF (BASEFREQ .EQ. 0.0) THEN
         POWER(1)  = 0.0
         VAROUT(1) = 0.0

      END IF

 99   IF (STATUS .NE. SAI__OK) THEN
        CALL AST_REXIT( 'SINFIT_PGRAM', STATUS )
      END IF
      IERR = NERR
      END
