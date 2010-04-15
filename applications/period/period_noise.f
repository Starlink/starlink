

      SUBROUTINE PERIOD_NOISE(YPTR, MXCOL, MXSLOT, NPTSARRAY,
     :                        YERRORARRAY, INFILEARRAY, DETRENDARRAY)

C===========================================================================
C Adds Gaussian noise and errors to data and resamples data by adding
C Gaussian irregularities. Also forms a random dataset with the same
C mean and standard deviation as the original.
C
C Written by Vikram Singh Dhillon @IAC 31-January-1992.
C
C Removed unused variable IMEAN - GJP June 1995
C
C Converted to Double Precision (KPD), August 2001
C Power-raising modified to use INTEGER power (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C===========================================================================

      IMPLICIT NONE

      INCLUDE 'CNF_PAR'

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

      INTEGER MXCOL, MXSLOT
      INTEGER YPTR(MXSLOT), NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_NOISE declarations.
C-----------------------------------------------------------------------------

      DOUBLE PRECISION AVE, ADEV, SDEV, VAR
      DOUBLE PRECISION RANDOM, PERIOD_GASDEV
      DOUBLE PRECISION NSIG, ESIG, RSIG
      DOUBLE PRECISION DMEAN, EMEAN
      DOUBLE PRECISION DSDEV, ESDEV, ISDEV
      INTEGER IDUM, ISET, SEED
      INTEGER N, NDATA, SLOT, FIRSTSLOT, LASTSLOT
      INTEGER IFAIL, FIRSTOUT, SLOTOUT, COUNTER
      INTEGER DATAPTR
      INTEGER YSLOT1, YSLOT2
      LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
      CHARACTER*1 NOISE, ERRORS, REPLY, REGULAR, OPTION
      CHARACTER*72 INFILEARRAY(MXSLOT)


C-----------------------------------------------------------------------------
C Select input and output slots to process.
C-----------------------------------------------------------------------------

      WRITE (*, *) ' '
      CALL PERIOD_SELECT(FIRSTSLOT, LASTSLOT, FIRSTOUT, MXSLOT, IFAIL)
      IF ( IFAIL.EQ.1 ) GO TO 700

C-----------------------------------------------------------------------------
C Prompt for NOISE options.
C-----------------------------------------------------------------------------

      WRITE (*, *) ' '
 100  CONTINUE
      WRITE (*, '(X,A,$)') '[N]ew dataset or [O]ld dataset ? [O] : '
      READ (*, '(A)', ERR=100) OPTION
      CALL PERIOD_CASE(OPTION, .TRUE.)
      IF ( OPTION.EQ.'N' ) THEN
 150     CONTINUE
         WRITE (*, '(X,A,$)')
     :                       'Enter seed for random number generator : '
         READ (*, *, ERR=150) SEED
         IDUM = -IABS(INT(SEED))
         ISET = 0
         RANDOM = PERIOD_GASDEV(ISET, IDUM)
      ELSE IF ( OPTION.EQ.' ' .OR. OPTION.EQ.'O' ) THEN
 200     CONTINUE
         WRITE (*, '(X,A,$)')
     :                       'Do you want to add noise to the Y data ? '
     :                       // '[N] : '
         READ (*, '(A)', ERR=200) NOISE
         CALL PERIOD_CASE(NOISE, .TRUE.)
 250     CONTINUE
         WRITE (*, '(X,A,$)')
     :                      'Do you want to add errors to the Y data ? '
     :                      // '[N] : '
         READ (*, '(A)', ERR=250) ERRORS
         CALL PERIOD_CASE(ERRORS, .TRUE.)
 300     CONTINUE
         WRITE (*, '(X,A,$)') 'Do you want to add ' //
     :                  'irregularities to the X data sampling ? [N] : '
         READ (*, '(A)', ERR=300) REGULAR
         CALL PERIOD_CASE(REGULAR, .TRUE.)
         IF ( NOISE.NE.'Y' .AND. ERRORS.NE.'Y' .AND. REGULAR.NE.'Y' )
     :        GO TO 700
 350     CONTINUE
         WRITE (*, '(X,A,$)')
     :                       'Enter seed for random number generator : '
         READ (*, *, ERR=350) SEED
         IDUM = -IABS(INT(SEED))
         ISET = 0
         RANDOM = PERIOD_GASDEV(ISET, IDUM)
         IF ( NOISE.EQ.'Y' ) THEN
 360        CONTINUE
            WRITE (*, '(X,A,$)') 'Enter number of sigma for data : '
            READ (*, *, ERR=360) NSIG
         END IF
         IF ( ERRORS.EQ.'Y' ) THEN
 380        CONTINUE
            WRITE (*, '(X,A,$)')
     :                'Enter number of sigma for errors (0 for SQRT) : '
            READ (*, *, ERR=380) ESIG
         END IF
         IF ( REGULAR.EQ.'Y' ) THEN
 400        CONTINUE
            WRITE (*, '(X,A,$)') 'Enter number of sigma for sampling : '
            READ (*, *, ERR=400) RSIG
         END IF
      ELSE
         GO TO 100
      END IF

C-----------------------------------------------------------------------------
C Calculate statistics of the data.
C-----------------------------------------------------------------------------

      WRITE (*, *) ' '
      COUNTER = 0

      DO 600 SLOT = FIRSTSLOT, LASTSLOT
         NDATA = NPTSARRAY(SLOT)

         IF ( NDATA.EQ.0 ) THEN
            CALL PERIOD_WRITEBELL()
            WRITE (*, *) '** ERROR: Slot empty =', SLOT
            GO TO 700
         END IF

         SLOTOUT = FIRSTOUT + COUNTER
         COUNTER = COUNTER + 1
         YSLOT1 = YPTR(SLOT)

         CALL PERIOD_ALLOC('_DOUBLE', NDATA, DATAPTR)

         CALL PERIOD_SETDATA(%VAL(CNF_PVAL(YSLOT1)),
     :                       NDATA, MXCOL, 2,
     :                       %VAL(CNF_PVAL(DATAPTR)))

         CALL PERIOD_MOMENT(%VAL(CNF_PVAL(DATAPTR)),
     :                      NDATA, AVE, ADEV, SDEV, VAR)

         DMEAN = AVE
         DSDEV = SDEV

         N = NDATA - 1

         CALL PERIOD_SETDATASQRT(%VAL(CNF_PVAL(YSLOT1)),
     :                           NDATA, MXCOL, N,
     :                           %VAL(CNF_PVAL(DATAPTR)))

         CALL PERIOD_MOMENT(%VAL(CNF_PVAL(DATAPTR)),
     :                      N, AVE, ADEV, SDEV, VAR)

         ISDEV = SDEV

         IF ( YERRORARRAY(SLOT) ) THEN

            CALL PERIOD_SETDATA(%VAL(CNF_PVAL(YSLOT1)),
     :                          NDATA, MXCOL, 3,
     :                          %VAL(CNF_PVAL(DATAPTR)))

            CALL PERIOD_MOMENT(%VAL(CNF_PVAL(DATAPTR)), NDATA, AVE,
     :                         ADEV, SDEV, VAR)

            EMEAN = AVE
            ESDEV = SDEV
         END IF

         CALL PERIOD_DEALL(DATAPTR)

C-----------------------------------------------------------------------------
C Apply noise to the existing data, sampling and errors.
C-----------------------------------------------------------------------------

         IF ( OPTION.EQ.' ' .OR. OPTION.EQ.'O' ) THEN
            IF ( ERRORS.EQ.'Y' ) THEN
               IF ( YERRORARRAY(SLOT) ) THEN
                  CALL PERIOD_WRITEBELL()
                  WRITE (*, *) '** WARNING: Data already has errors.'
 525              CONTINUE
                  WRITE (*, '(X,A,$)')
     :                            '** WARNING: Are you sure you want to'
     :                            // ' continue ? [N] : '
                  READ (*, '(A)', ERR=525) REPLY
                  WRITE (*, *) ' '
                  CALL PERIOD_CASE(REPLY, .TRUE.)
                  IF ( REPLY.NE.'Y' ) GO TO 700
               END IF
            END IF

            IF ( SLOT.NE.SLOTOUT ) THEN
               IF (NPTSARRAY(SLOTOUT).NE.0)
     :            CALL PERIOD_DEALL(YPTR(SLOTOUT))
               CALL PERIOD_ALLOC('_DOUBLE', NDATA*MXCOL, YPTR(SLOTOUT))
            END IF

            YSLOT2 = YPTR(SLOTOUT)

            IFAIL = 0

            CALL PERIOD_NOISEOLD(%VAL(CNF_PVAL(YSLOT1)), NDATA,
     :                           MXCOL, ISET, IDUM,
     :                           NOISE, DSDEV, NSIG, RANDOM, ERRORS,
     :                           ESDEV, ESIG, REGULAR, ISDEV, RSIG,
     :                           YERRORARRAY(SLOT),
     :                           %VAL(CNF_PVAL(YSLOT2)), IFAIL)

            IF ( IFAIL.EQ.1 ) GO TO 700

            NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
            YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
            IF ( ERRORS.EQ.'Y' ) YERRORARRAY(SLOTOUT) = .TRUE.
            DETRENDARRAY(SLOTOUT) = .FALSE.
            INFILEARRAY(SLOTOUT) = 'Noisy ' // INFILEARRAY(SLOT)
            WRITE (*, *) '** OK: Filled slot = ', SLOTOUT

C-----------------------------------------------------------------------------
C Generate a new dataset with the same mean and standard deviation as
C the old.
C-----------------------------------------------------------------------------

         ELSE IF ( OPTION.EQ.'N' ) THEN

            IF ( SLOT.NE.SLOTOUT ) THEN
               IF (NPTSARRAY(SLOTOUT).NE.0)
     :            CALL PERIOD_DEALL(YPTR(SLOTOUT))
               CALL PERIOD_ALLOC('_DOUBLE', NDATA*MXCOL, YPTR(SLOTOUT))
            END IF

            YSLOT2 = YPTR(SLOTOUT)

            IFAIL = 0

            CALL PERIOD_NOISENEW(%VAL(CNF_PVAL(YSLOT1)), NDATA, MXCOL,
     :                           ISET, IDUM,
     :                           DSDEV, DMEAN, RANDOM, ESDEV, EMEAN,
     :                           YERRORARRAY(SLOT),
     :                           %VAL(CNF_PVAL(YSLOT2)))

            NPTSARRAY(SLOTOUT) = NPTSARRAY(SLOT)
            YERRORARRAY(SLOTOUT) = YERRORARRAY(SLOT)
            DETRENDARRAY(SLOTOUT) = .FALSE.
            INFILEARRAY(SLOTOUT) = 'Noisy ' // INFILEARRAY(SLOT)
            WRITE (*, *) '** OK: Filled slot = ', SLOTOUT
         END IF
 600  CONTINUE

 700  CONTINUE
      RETURN

      END
