*+  SINFIT - Computes power spectrum for 1D data with general spacing
      SUBROUTINE SINFIT(STATUS)
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
*      5 Dec 83: Original.
*     13 Dec 84: Version 4.
*     17 Dec 85: Case of zero data error handled (v 0.4-1).
*     10 Jan 86: Now uses TIM_PERIODS - zero frequency properly handled.
*     22 Jan 86: Sensible message if data array is too long.
*      2 Mar 87: Converted to ROSAT spec, renamed SINFIT
*                                                      (old name PGRAM).
*     25 Aug 87: Bug fix to PHASE: now gives phase for SIN curve, in range
*                0, 2PI. (pla)
*     25 Aug 88: Major rewrite, now maps input file & creates dynamical
*                storage for work arrays. Also now uses SINFIT_PGRAM, a
*                rewritten version of TIM_PERIODS. (PLA)
*
*     13 Jun 90: V1.2-0  Several bug fixes done (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) ASTLOC        ! Locator to ASTERIX box.
      CHARACTER*(DAT__SZLOC) INLOC         ! Locator to input file.
      CHARACTER*(DAT__SZLOC) OUTLOC        ! Power spectrum object locator
      CHARACTER*(DAT__SZLOC) PHASELOC      ! Locator to PHASE_SPECTRUM
      CHARACTER*80           DUNITS        ! Data units
      CHARACTER*80           TUNITS        ! Time units
      CHARACTER*80           TEXT(5)       ! For history record
      CHARACTER*80           TITLE         ! of data file.

      REAL                   BASEFREQ      ! Base freq.
      REAL                   FCHNG         ! The frequency at which SINFIT_PGRAM changes algorithm.
      REAL                   FREQSTEP      ! Freq.step
      REAL                   NPOWER        ! Noise power

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
      INTEGER                LDIM(DAT__MXDIM)! Length of each dimension.
      INTEGER                NBAD          ! # of bad quality points
      INTEGER                NDATA         ! No.of data points
      INTEGER                NDIMS         ! Number of dimension of input data
      INTEGER                NFREQ         ! No.of frequencies
      INTEGER                NLINES        ! No. of history text lines

      LOGICAL                AST_THERE     ! ASTERIX structure present?
      LOGICAL                AXOK          ! AXIS(1) OK
      LOGICAL                BAD           ! Bad quality points?
      LOGICAL                OK
      LOGICAL                PHASE         ! Create a PHASE_SPECRUM too?
      LOGICAL                PRIM          ! Used to test for primitive
      LOGICAL                QUALOK        ! QUALITY OK
      LOGICAL                THERE         ! Does MORE box exist?
      LOGICAL                VAROK         ! VARIANCE OK
*
*    Version id :
*
      CHARACTER*21            VERSION
         PARAMETER           (VERSION = 'SINFIT Version 1.2-0')
*-

*    Check status.
      IF (STATUS .NE. SAI__OK) RETURN

*    Version ID.
      CALL MSG_PRNT(VERSION)

*    Initialize BDA_, DYN_ HIST_, USI_, etc routines
      CALL AST_INIT

*    Obtain data object name.
      CALL USI_ASSOCI ('INPUT', 'READ', INLOC, PRIM, STATUS)

*    Create output dataset
      CALL USI_ASSOCO ('OUTPUT', 'POWER_SPECTRUM', OUTLOC, STATUS)

*    Map input data.
      CALL BDA_CHKDATA (INLOC, OK, NDIMS, LDIM, STATUS)

      IF (OK) THEN
         CALL BDA_MAPDATA(INLOC, 'READ', DATPTR, STATUS)
         CALL ARR_SUMDIM( NDIMS, LDIM, NDATA, STATUS )
      ELSE
         CALL MSG_PRNT( 'FATAL ERROR: Unable to find suitable data'/
     :                                              /' component.' )
         STATUS = SAI__ERROR
      END IF

*    Check status.
      IF (STATUS .NE. SAI__OK) GOTO 999

      IF (NDIMS .NE. 1) THEN
         CALL MSG_PRNT('WARNING: Data is not one-dimensional')
      END IF

      IF (.NOT. PRIM .AND. NDIMS .EQ. 1) THEN
*      Map axis data
        CALL BDA_CHKAXIS (INLOC, 1, AXOK, STATUS)

      ELSE
        AXOK = .FALSE.

      END IF

      IF ( AXOK ) THEN
         CALL BDA_MAPAXVAL (INLOC, 'READ', 1, AXPTR, STATUS)

      ELSE
         CALL MSG_PRNT('WARNING: Axis data invalid'//
     :                         ' - proceeding assuming regular spacing')

         CALL DYN_MAPR (1, NDATA, AXPTR, STATUS)
         CALL ARR_REG1R (1, 1, NDATA, %VAL(AXPTR), STATUS )

      END IF

*    Map VARIANCE if present
      CALL BDA_CHKVAR (INLOC, VAROK, NDIMS, LDIM, STATUS)
      IF (VAROK) THEN
         CALL BDA_MAPVAR (INLOC, 'READ', VARPTR, STATUS)
      END IF

*    User information
      CALL MSG_SETI ('NDAT', NDATA)
      CALL MSG_PRNT('^NDAT data points entered')

      IF ( AXOK ) THEN
         CALL BDA_GETAXUNITS (INLOC, 1, TUNITS, STATUS)

         IF (CHR_LEN(TUNITS) .GT. 0) THEN
            CALL MSG_SETC ('TUNITS', TUNITS)
            CALL MSG_PRNT('The time units are ^TUNITS')

         ELSE
            CALL MSG_PRNT('No time units specified in input file')

         END IF
      END IF

*    Map QUALITY as a logical.
      BAD = .FALSE.
      CALL BDA_CHKQUAL (INLOC, QUALOK, NDIMS, LDIM, STATUS)

      IF ( QUALOK ) THEN
         CALL BDA_MAPLQUAL (INLOC, 'READ', BAD, QPTR, STATUS)

         IF ( BAD ) THEN
            CALL ARR_NBAD( NDATA, %VAL(QPTR), NBAD, STATUS )
            CALL MSG_SETI( 'NBAD', NBAD )
            CALL MSG_PRNT( 'There are ^NBAD bad quality points'/
     :                                         /' in the data' )
         ELSE
            CALL BDA_UNMAPLQUAL (INLOC, STATUS)
            QUALOK = .FALSE.
         END IF

      END IF

*    User input.
      CALL PAR_GET0R ('BASE_FREQ',      BASEFREQ, STATUS)
      CALL PAR_GET0R ('FREQ_INCREMENT', FREQSTEP, STATUS)
      CALL PAR_GET0I ('NO_OF_FREQS',    NFREQ,    STATUS)
      CALL PAR_GET0L ('PHASE',          PHASE,    STATUS)

*    Create dynamic arrays
      CALL DYN_MAPR (1, NFREQ, COSPTR,  STATUS)
      CALL DYN_MAPR (1, NFREQ, SINPTR,  STATUS)
      CALL DYN_MAPR (1, NFREQ, WORKPTR, STATUS)

*    Check status.
      IF (STATUS .NE. SAI__OK) GOTO 999

*    Create components in output dataset
      CALL BDA_CREDATA (OUTLOC, 1, NFREQ,        STATUS)
      CALL BDA_MAPDATA (OUTLOC, 'WRITE', PWRPTR, STATUS)

      CALL BDA_PUTAXVAL   (OUTLOC, 1, BASEFREQ, FREQSTEP, NFREQ, STATUS)
      CALL BDA_PUTAXLABEL (OUTLOC, 1, 'Frequency', STATUS)

      CALL BDA_PUTLABEL (OUTLOC, 'Power', STATUS)

      IF (.NOT. PRIM) THEN
        CALL BDA_GETUNITS (INLOC, DUNITS, STATUS)

        IF (CHR_LEN(DUNITS) .GT. 0) THEN
           DUNITS = '('//DUNITS(1:CHR_LEN(DUNITS))//')**2'
           CALL BDA_PUTUNITS (OUTLOC, DUNITS, STATUS)

        END IF

        IF (CHR_LEN(TUNITS) .GT. 0) THEN
          TUNITS = '('//TUNITS(1:CHR_LEN(TUNITS))//')**-1'
          CALL BDA_PUTAXUNITS (OUTLOC, 1, TUNITS, STATUS)

        END IF
        CALL BDA_GETTITLE (INLOC,  TITLE, STATUS)
        CALL BDA_PUTTITLE (OUTLOC, TITLE, STATUS)

      END IF

      CALL BDA_CREVAR (OUTLOC, 1, NFREQ,        STATUS)
      CALL BDA_MAPVAR (OUTLOC, 'WRITE', VAROUT, STATUS)

      IF ( PRIM ) THEN
        THERE = .FALSE.
      ELSE
        CALL DAT_THERE (OUTLOC, 'MORE', THERE, STATUS)
      END IF

      IF (THERE) THEN
         CALL BDA_COPMORE( INLOC, OUTLOC, STATUS )
      END IF

      IF ( PHASE ) THEN
         CALL BDA_CHKAST( OUTLOC, AST_THERE, STATUS )
         IF ( .NOT. AST_THERE ) THEN
            CALL BDA_CREAST( OUTLOC, STATUS )
         END IF
         CALL BDA_LOCAST( OUTLOC, ASTLOC, STATUS )

         CALL DAT_NEW  (ASTLOC, 'PHASE_SPECTRUM', 'NDF', 0, 0, STATUS)
         CALL DAT_FIND (ASTLOC, 'PHASE_SPECTRUM', PHASELOC,    STATUS)

         CALL BDA_CREDATA  (PHASELOC, 1, NFREQ,        STATUS)
         CALL BDA_MAPDATA  (PHASELOC, 'WRITE', PHAPTR, STATUS)
         CALL BDA_PUTLABEL (PHASELOC, 'Phase',         STATUS)
         CALL BDA_PUTUNITS (PHASELOC, 'radians',       STATUS)

         CALL BDA_PUTAXVAL (PHASELOC, 1, BASEFREQ, FREQSTEP, NFREQ,
     :                                                      STATUS)
         CALL DAT_ANNUL( PHASELOC, STATUS )

         IF (CHR_LEN(TUNITS) .GT. 0) THEN
            CALL BDA_PUTAXUNITS (OUTLOC, 1, TUNITS,      STATUS)
            CALL BDA_PUTAXLABEL (OUTLOC, 1, 'Frequency', STATUS)
         END IF

      END IF

*    Compute periodogram & phase.
      FCHNG = - 1.0
      CALL SINFIT_PGRAM (NDATA, %VAL(AXPTR), %VAL(DATPTR), %VAL(VARPTR),
     :      %VAL(QPTR), VAROK, QUALOK, BASEFREQ, FREQSTEP, NFREQ, FCHNG,
     :          %VAL(WORKPTR), %VAL(COSPTR), %VAL(SINPTR), %VAL(PWRPTR),
     :                               %VAL(VAROUT), NPOWER, IERR, STATUS)

*    Check status.
      IF (STATUS .NE. SAI__OK) GOTO 999

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

*    Copy history
      CALL HIST_COPY (INLOC, OUTLOC, STATUS)

*    Add new history component
      CALL HIST_ADD  (OUTLOC, VERSION, STATUS)

      TEXT(1) = 'Input {INPUT}'
      I       = 1
      NLINES  = 5

      IF (PHASE) THEN
        TEXT(2) =
     :  'Phase spectrum written into MORE.ASTERIX.PHASE_SPECTRUM'
        I = I + I

      END IF

      CALL USI_TEXT  (I, TEXT, NLINES, STATUS)
      CALL HIST_PTXT (OUTLOC, NLINES, TEXT, STATUS)

*   Exit
999   IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP ('EXERR', 'from SINFIT', STATUS)
      END IF

      CALL AST_CLOSE

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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
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

      CALL PAR_GET0L ('DEMEAN', DEMEAN, STATUS)

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 999

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
        IF (STATUS .NE. SAI__OK) GOTO 999

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

999   IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP ('E', '...from SINFIT_PGRAM', STATUS)

      END IF
      IERR = NERR
      END
