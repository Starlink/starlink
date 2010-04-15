      SUBROUTINE PERIOD_PERIOD(YPTR, MXCOL, MXSLOT, NPTSARRAY,
     :                         INFILEARRAY, YERRORARRAY, DETRENDARRAY,
     :                         SIG,LOG,LOGUNIT)

C==============================================================================
C The work-horse routine. Finds periodicities in data using a number of
C different techniques.
C
C Written by Vikram Singh Dhillon @Sussex 22-February-1992.
C
C Removed unused parameter LOGFILE - GJP June 1995
C
C Initialised some variables - GJP March 1997
C
C Converted to Double Precision (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C==============================================================================

      IMPLICIT NONE

      INCLUDE "mnmxvl.h"

      INCLUDE 'CNF_PAR'

C-----------------------------------------------------------------------------
C PERIOD_PERIOD declarations.
C-----------------------------------------------------------------------------

      INTEGER MXCOL, MXSLOT
      INTEGER YPTR(MXSLOT), NPTSARRAY(MXSLOT)
      INTEGER FIRSTSLOT, LASTSLOT, SLOT, FIRSTOUT, SLOTOUT
      INTEGER I, J, COUNTER, NOPTION, LOGUNIT
      INTEGER LOOP, ISTEP, MAXPTS, NDATA, IFAIL
      INTEGER NP, NWK, NOUT, NCL, NBIN
      INTEGER SAMPLE, NPERMS, SEED, EL, MAXPERMS, ELMAX, ELMIN
      PARAMETER (MAXPERMS=1000)
      DOUBLE PRECISION STAT(MAXPERMS+1), SIG(2, MXSLOT)
      INTEGER WORK1PTR, WORK2PTR
      INTEGER YRANPTR, ERANPTR
      INTEGER DATAPTR, WK1PTR, WK2PTR
      INTEGER XDATAPTR, YDATAPTR, YERRPTR
      INTEGER NFREQ, YSLOT1, YSLOT2
      DOUBLE PRECISION ZEROPT, FREQ, GAMMA, KVEL, PHASE, VAR(6), F
      DOUBLE PRECISION MINFREQ, MAXFREQ, FINTERVAL
      DOUBLE PRECISION FMIN, FMAX, FINT
      DOUBLE PRECISION AVE, ADEV, SDEV, VARI
      DOUBLE PRECISION XMIN, XDIF, OFAC, HIFAC
      DOUBLE PRECISION GAIN, STRING, PDM, WBIN
      DOUBLE PRECISION PEAK, TROUGH, PERIOD, PERIOD_RAN1, RANDOM, PERROR
      DOUBLE PRECISION SIGNOISE, SIGPERIOD, ERRNOISE, ERRPERIOD
      DOUBLE PRECISION PERIOD_GET2D
      DOUBLE PRECISION RVAL1, RVAL2, RVAL3
      DOUBLE PRECISION TOLFACT
      LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
      LOGICAL PERIOD_PARSE, LSELECT, LOG
      LOGICAL LPEAK, LOGWRITE, LSIG, LFAP1
      CHARACTER*12 COMMAND, REPLY*1
      CHARACTER*72 INFILEARRAY(MXSLOT)
      DATA ISTEP/50/
      DATA LSIG, NPERMS/.FALSE., 0/
      DATA MINFREQ,MAXFREQ,FINTERVAL/0.0D0,0.0D0,0.0D0/

      SAVE MINFREQ,MAXFREQ,FINTERVAL


C-----------------------------------------------------------------------------
C Initialise slot selection logical to guard against accidental erasure.
C-----------------------------------------------------------------------------

      LSELECT = .FALSE.

C-----------------------------------------------------------------------------
C Initialise frequency limits.
C-----------------------------------------------------------------------------

      MAXFREQ=0.0D0
      MINFREQ=0.0D0
      FINTERVAL=0.0D0
      FMIN=0.0D0
      FMAX=0.0D0
      FINT=0.0D0

C-----------------------------------------------------------------------------
C Menu.
C-----------------------------------------------------------------------------

 100  CONTINUE
      WRITE (*, *) ' '
      WRITE (*, *) 'Options.'
      WRITE (*, *) '--------'
      WRITE (*, *) ' '
      WRITE (*, *) '  SELECT   --  Select data slots.'
      WRITE (*, *) '  FREQ     --  Set/show frequency search limits.'
      WRITE (*, *)
     :            '  CHISQ    --  Chi-squared of sine fit vs frequency.'
      WRITE (*, *) '  CLEAN    --  CLEANed power spectrum.'
      WRITE (*, *) '  FT       --  Discrete Fourier power spectrum.'
      WRITE (*, *) '  PDM      --  Phase dispersion minimization.'
      WRITE (*, *) '  SCARGLE  --  Lomb-Scargle normalised periodogram.'
      WRITE (*, *) '  STRING   --  String-length vs frequency.'
      WRITE (*, *) '  PEAKS    --  Calculate period from periodogram.'
      WRITE (*, *) '  SIG      --  Enable/disable significance calc.'
      WRITE (*, *) '  PLT      --  Plot the contents of a slot.'
      WRITE (*, *) '  HELP     --  On-line help.'
      WRITE (*, *) '  QUIT     --  Quit PERIOD_PERIOD.'
      WRITE (*, *) '  '
      WRITE (*, '(X,A,$)') 'PERIOD_PERIOD> '
      READ (*, '(A)', ERR=100) COMMAND

      CALL PERIOD_CASE(COMMAND, .TRUE.)

      IF ( PERIOD_PARSE(COMMAND,'SELECT') ) THEN

C**********-------------------------------------------------------------------
C SELECT  * Select input and output data slots.
C**********-------------------------------------------------------------------

         WRITE (*, *) ' '

         CALL PERIOD_SELECT(FIRSTSLOT, LASTSLOT, FIRSTOUT, MXSLOT,
     :                      IFAIL)

         IF ( IFAIL.EQ.1 ) THEN
            LSELECT = .FALSE.
         ELSE
            WRITE (*, *) ' '
            WRITE (*, *) '** OK: Input and output slots selected.'
            LSELECT = .TRUE.
         END IF
         GO TO 100

      ELSE IF ( .NOT.(PERIOD_PARSE(COMMAND,'FREQ')) ) THEN
         IF ( PERIOD_PARSE(COMMAND,'CHISQ') ) THEN

C**********-------------------------------------------------------------------
C CHISQ   * Least-squares sine-curve fitting.
C**********-------------------------------------------------------------------

            IF ( .NOT.LSELECT ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: No slots selected.'
               GO TO 100
            ELSE
               LSELECT = .FALSE.
            END IF

C-----------------------------------------------------------------------------
C chisq - Loop through slots to be processed.
C-----------------------------------------------------------------------------

            DO 290 SLOT = FIRSTSLOT, LASTSLOT
               NDATA = NPTSARRAY(SLOT)

               IF ( NDATA.EQ.0 ) THEN
                  CALL PERIOD_WRITEBELL()
                  WRITE (*, *) '** ERROR: Slot empty =', SLOT
                  GO TO 100
               END IF

               SLOTOUT = FIRSTOUT + (SLOT-FIRSTSLOT)
               YSLOT1 = YPTR(SLOT)

C-----------------------------------------------------------------------------
C chisq - Loop through permutations.
C-----------------------------------------------------------------------------

               DO 270 SAMPLE = 1, NPERMS + 1
                  IF ( SAMPLE.EQ.1 ) THEN

C-----------------------------------------------------------------------------
C chisq - Set frequency limits.
C-----------------------------------------------------------------------------

                     CALL PERIOD_ALLOC('_DOUBLE', NDATA, DATAPTR)

                     CALL PERIOD_SETDATA(%VAL(CNF_PVAL(YSLOT1)), NDATA,
     :                                   MXCOL,
     :                                   1, %VAL(CNF_PVAL(DATAPTR)))
                     WRITE (*, *) ' '

                     IF ( SLOT.EQ.SLOTOUT ) THEN
                        MAXPTS = NDATA
                     ELSE
                        MAXPTS = 0
                     ENDIF

                     CALL PERIOD_AUTOLIM(%VAL(CNF_PVAL(DATAPTR)),
     :                                   NDATA,
     :                                   MAXPTS,
     :                                   MINFREQ, MAXFREQ, FINTERVAL,
     :                                   FMIN, FMAX, FINT, IFAIL)

                     IF ( IFAIL.EQ.1 ) GO TO 294
                     IF ( FMIN.EQ.0.0D0 ) THEN
                        CALL PERIOD_WRITEBELL()
                        WRITE (*, *)
     :                       '** ERROR: No sine fit for zero frequency.'
                        GO TO 294
                     END IF

C-----------------------------------------------------------------------------
C chisq - Set zero point to be the mean observation time.
C-----------------------------------------------------------------------------

                     CALL PERIOD_MOMENT(%VAL(CNF_PVAL(DATAPTR)), NDATA,
     :                                  AVE,
     :                                  ADEV, SDEV, VARI)
                     ZEROPT = AVE
                     WRITE (*, *) ' '
                     WRITE (*, *) ' '

                     IF ( SLOT.NE.SLOTOUT ) THEN
                        IF ( NPTSARRAY(SLOTOUT).NE.0 )
     :                     CALL PERIOD_DEALL(YPTR(SLOTOUT))
                        CALL PERIOD_ALLOC('_DOUBLE', MAXPTS*MXCOL,
     :                                    YPTR(SLOTOUT))
                     END IF

                     YSLOT2 = YPTR(SLOTOUT)

                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, XDATAPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, YDATAPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, YRANPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, YERRPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, ERANPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', MAXPTS, WK2PTR)

                  END IF

C-----------------------------------------------------------------------------
C chisq - Loop through trial frequencies.
C-----------------------------------------------------------------------------

                  COUNTER = 0
                  LOOP = 1
                  FREQ = FMIN
                  DO 250 WHILE ( FREQ .LE. FMAX )

                     CALL PERIOD_SETXYEDATA(%VAL(CNF_PVAL(YSLOT1)),
     :                                      NDATA, MXCOL,
     :                                      SAMPLE, YERRORARRAY(SLOT),
     :                                      %VAL(CNF_PVAL(XDATAPTR)),
     :                                      %VAL(CNF_PVAL(YDATAPTR)),
     :                                      %VAL(CNF_PVAL(YRANPTR)),
     :                                      %VAL(CNF_PVAL(YERRPTR)),
     :                                      %VAL(CNF_PVAL(ERANPTR)))

                     PERIOD = 1.0D0/FREQ

                     CALL PERIOD_FOLD(%VAL(CNF_PVAL(XDATAPTR)),
     :                                %VAL(CNF_PVAL(YDATAPTR)),
     :                                %VAL(CNF_PVAL(YERRPTR)), NDATA,
     :                                ZEROPT,
     :                                PERIOD, IFAIL)

                     IF ( IFAIL.EQ.1 ) GO TO 292

                     CALL PERIOD_SINFIT(%VAL(CNF_PVAL(XDATAPTR)),
     :                                  %VAL(CNF_PVAL(YDATAPTR)),
     :                                  %VAL(CNF_PVAL(YERRPTR)), NDATA,
     :                                  1.0D0,
     :                                  GAMMA, KVEL, PHASE, VAR, NP, F,
     :                                  IFAIL)

                     IF ( IFAIL.EQ.1 ) THEN
                        CALL PERIOD_WRITEBELL()
                        WRITE (*, *) '** ERROR: Sine fit unsuccessful.'
                        GO TO 292
                     END IF

                     COUNTER = COUNTER + 1

                     IF ( SAMPLE.EQ.1 ) THEN
                        IF ( COUNTER.EQ.(LOOP*ISTEP) ) THEN
                           WRITE (*, 99001) FREQ, F/(DFLOAT(NP)-3.0D0)
99001                      FORMAT ('+Trial Frequency =', D12.6,
     :                             ',  Reduced Chi-squared =', D18.6)
                           LOOP = LOOP + 1
                        END IF
                     END IF


C-----------------------------------------------------------------------------
C chisq - If the first permutation, load the output slot.
C-----------------------------------------------------------------------------

                     IF ( SAMPLE.EQ.1 ) THEN

                        CALL PERIOD_LOADOUTONE(%VAL(CNF_PVAL(YSLOT2)),
     :                                         MAXPTS,
     :                                         MXCOL, COUNTER, FREQ,
     :                                         F/(DFLOAT(NP)-3.0D0))
                     END IF

C                    WK2(COUNTER) = F/(DFLOAT(NP)-3.0D0)

                     RVAL1 = F/(DFLOAT(NP)-3.0D0)
                     CALL PERIOD_PUT1D(RVAL1, COUNTER,
     :                                 %VAL(CNF_PVAL(WK2PTR)),
     :                                 MAXPTS)
                     FREQ = FREQ + FINT
 250              CONTINUE

C-----------------------------------------------------------------------------
C chisq - Periodogram complete. If first permutation, load output arrays.
C-----------------------------------------------------------------------------

                  IF ( SAMPLE.EQ.1 ) THEN
                     YERRORARRAY(SLOTOUT) = .FALSE.
                     DETRENDARRAY(SLOTOUT) = .FALSE.
                     INFILEARRAY(SLOTOUT)
     :                   = 'Reduced Chi**2 vs Trial Freq ' // 'For ' //
     :                  INFILEARRAY(SLOT)
                     NPTSARRAY(SLOTOUT) = COUNTER
                     SIG(1, SLOTOUT) = -1.0D0
                     SIG(2, SLOTOUT) = -1.0D0
                     WRITE (*, *) ' '
                     WRITE (*, *) '** OK: Filled slot = ', SLOTOUT
                     IF ( LSIG ) THEN
                        WRITE (*, *) ' '
                        WRITE (*, *) ' '
                     END IF
                  END IF

C-----------------------------------------------------------------------------
C chisq - Determine significance information if requested.
C-----------------------------------------------------------------------------

                  IF ( LSIG ) THEN
                     STAT(SAMPLE) = DPMX30

                     CALL PERIOD_SIGINFOLT(%VAL(CNF_PVAL(YSLOT2)),
     :                                     MAXPTS, MXCOL,
     :                                     COUNTER, STAT, MAXPERMS+1,
     :                                     SAMPLE,
     :                                     %VAL(CNF_PVAL(WK2PTR)),
     :                                     MAXPTS)

                     IF ( SAMPLE.GT.1 ) THEN
                        WRITE (*, 99002) SAMPLE - 1
99002                   FORMAT ('+** OK: Processed permutation = ', I4)
                     END IF

C-----------------------------------------------------------------------------
C chisq - Randomize time-series for next permutation.
C-----------------------------------------------------------------------------

                     RANDOM = PERIOD_RAN1(SEED)
                     DO 260 J = 1, 1 + IDINT(RANDOM*2.0D0)

                        CALL PERIOD_YANDESHUFFLE(SEED,
     :                              %VAL(CNF_PVAL(YRANPTR)),
     :                              %VAL(CNF_PVAL(ERANPTR)),
     :                              NDATA)
 260                 CONTINUE

                  END IF

 270           CONTINUE

                  CALL PERIOD_DEALL(WK2PTR)
                  CALL PERIOD_DEALL(ERANPTR)
                  CALL PERIOD_DEALL(YERRPTR)
                  CALL PERIOD_DEALL(YRANPTR)
                  CALL PERIOD_DEALL(YDATAPTR)
                  CALL PERIOD_DEALL(XDATAPTR)
                  CALL PERIOD_DEALL(DATAPTR)

C-----------------------------------------------------------------------------
C chisq - Permutations complete - calculate significance; load output arrays.
C-----------------------------------------------------------------------------

               IF ( LSIG ) THEN
                  SIGNOISE = 0.0D0
                  DO 280 I = 2, NPERMS + 1
                     IF ( STAT(I).LE.STAT(1) )
     :                  SIGNOISE = SIGNOISE + 1.0D0
 280              CONTINUE
                  SIG(1, SLOTOUT) = DFLOAT(NPERMS)
                  SIG(2, SLOTOUT) = SIGNOISE/DFLOAT(NPERMS)
               END IF

C-----------------------------------------------------------------------------
C chisq - Process the next data slot.
C-----------------------------------------------------------------------------

 290        CONTINUE
            GO TO 100

 292        CONTINUE

            CALL PERIOD_DEALL(WK2PTR)
            CALL PERIOD_DEALL(ERANPTR)
            CALL PERIOD_DEALL(YERRPTR)
            CALL PERIOD_DEALL(YRANPTR)
            CALL PERIOD_DEALL(YDATAPTR)
            CALL PERIOD_DEALL(XDATAPTR)

 294        CONTINUE
            CALL PERIOD_DEALL(DATAPTR)
            GO TO 100

         ELSE IF ( PERIOD_PARSE(COMMAND,'FT') ) THEN

C**********-------------------------------------------------------------------
C FT      * Discrete Fourier power spectrum.
C**********-------------------------------------------------------------------

            IF ( .NOT.LSELECT ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: No slots selected.'
               GO TO 100
            ELSE
               LSELECT = .FALSE.
            END IF

C-----------------------------------------------------------------------------
C ft - Loop through slots to be processed.
C-----------------------------------------------------------------------------

            DO 390 SLOT = FIRSTSLOT, LASTSLOT
               NDATA = NPTSARRAY(SLOT)

               IF ( NDATA.EQ.0 ) THEN
                  CALL PERIOD_WRITEBELL()
                  WRITE (*, *) '** ERROR: Slot empty =', SLOT
                  GO TO 100
               END IF

               SLOTOUT = FIRSTOUT + (SLOT-FIRSTSLOT)
               YSLOT1 = YPTR(SLOT)

C-----------------------------------------------------------------------------
C ft - Loop through permutations.
C-----------------------------------------------------------------------------

               DO 370 SAMPLE = 1, NPERMS + 1
                  IF ( SAMPLE.EQ.1 ) THEN

C-----------------------------------------------------------------------------
C ft - Set frequency limits.
C-----------------------------------------------------------------------------

                     CALL PERIOD_ALLOC('_DOUBLE', NDATA, DATAPTR)

                     CALL PERIOD_SETDATA(%VAL(CNF_PVAL(YSLOT1)),
     :                                   NDATA, MXCOL,
     :                                   1, %VAL(CNF_PVAL(DATAPTR)))
                     WRITE (*, *) ' '

                     IF ( SLOT.EQ.SLOTOUT ) THEN
                        MAXPTS = NDATA
                     ELSE
                        MAXPTS = 0
                     ENDIF

                     CALL PERIOD_AUTOLIM(%VAL(CNF_PVAL(DATAPTR)), NDATA,
     :                                   MAXPTS,
     :                                   MINFREQ, MAXFREQ, FINTERVAL,
     :                                   FMIN, FMAX, FINT, IFAIL)

                     IF ( IFAIL.EQ.1 ) GO TO 394
                     WRITE (*, *) ' '
                     WRITE (*, *) ' '

                     IF ( SLOT.NE.SLOTOUT ) THEN
                        IF ( NPTSARRAY(SLOTOUT).NE.0 )
     :                     CALL PERIOD_DEALL(YPTR(SLOTOUT))
                        CALL PERIOD_ALLOC('_DOUBLE', MAXPTS*MXCOL,
     :                                    YPTR(SLOTOUT))
                     END IF

                     YSLOT2 = YPTR(SLOTOUT)


                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, XDATAPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, YDATAPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, YRANPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', MAXPTS, WK1PTR)
                  CALL PERIOD_ALLOC('_DOUBLE', MAXPTS, WK2PTR)

                  END IF

C-----------------------------------------------------------------------------
C ft - Loop through trial frequencies.
C-----------------------------------------------------------------------------

                  CALL PERIOD_SETXYDATA(%VAL(CNF_PVAL(YSLOT1)), NDATA,
     :                                  MXCOL,
     :                                  SAMPLE,
     :                                  %VAL(CNF_PVAL(XDATAPTR)),
     :                                  %VAL(CNF_PVAL(YDATAPTR)),
     :                                  %VAL(CNF_PVAL(YRANPTR)))

                  CALL PERIOD_FT(%VAL(CNF_PVAL(XDATAPTR)),
     :                           %VAL(CNF_PVAL(YDATAPTR)),
     :                           NDATA, MAXPTS, FMIN, FMAX, FINT,
     :                           %VAL(CNF_PVAL(WK1PTR)),
     :                           %VAL(CNF_PVAL(WK2PTR)), NOUT,
     :                           SAMPLE)

C-----------------------------------------------------------------------------
C ft - Periodogram complete. If first permutation, load output arrays.
C-----------------------------------------------------------------------------

                  IF ( SAMPLE.EQ.1 ) THEN

                     CALL PERIOD_LOADOUTALL(%VAL(CNF_PVAL(YSLOT2)),
     :                                      NOUT, MXCOL,
     :                                      %VAL(CNF_PVAL(WK1PTR)),
     :                                      %VAL(CNF_PVAL(WK2PTR)),
     :                                      MAXPTS)

                     YERRORARRAY(SLOTOUT) = .FALSE.
                     DETRENDARRAY(SLOTOUT) = .FALSE.
                     INFILEARRAY(SLOTOUT) = 'Normalised Power ' //
     :                  'vs Freq For ' // INFILEARRAY(SLOT)
                     NPTSARRAY(SLOTOUT) = NOUT
                     SIG(1, SLOTOUT) = -1.0D0
                     SIG(2, SLOTOUT) = -1.0D0
                     WRITE (*, *) ' '
                     WRITE (*, *) '** OK: Filled slot = ', SLOTOUT
                     IF ( LSIG ) THEN
                        WRITE (*, *) ' '
                        WRITE (*, *) ' '
                     END IF
                  END IF

C-----------------------------------------------------------------------------
C ft - Determine significance information if requested.
C-----------------------------------------------------------------------------

                  IF ( LSIG ) THEN
                     STAT(SAMPLE) = DNMX30

                     CALL PERIOD_SIGINFOGT(%VAL(CNF_PVAL(YSLOT2)),
     :                                     MAXPTS, MXCOL,
     :                                     NOUT, STAT, MAXPERMS+1,
     :                                     SAMPLE,
     :                                     %VAL(CNF_PVAL(WK2PTR)),
     :                                     MAXPTS)

                     IF ( SAMPLE.GT.1 ) THEN
                        WRITE (*, 99003) SAMPLE - 1
99003                   FORMAT ('+** OK: Processed permutation = ', I4)
                     END IF

C-----------------------------------------------------------------------------
C ft - Randomize time-series for next permutation.
C-----------------------------------------------------------------------------

                     RANDOM = PERIOD_RAN1(SEED)
                     DO 360 J = 1, 1 + IDINT(RANDOM*2.0D0)

                        CALL PERIOD_YSHUFFLE(SEED,
     :                                       %VAL(CNF_PVAL(YRANPTR)),
     :                                       NDATA)
 360                 CONTINUE

                  END IF

 370           CONTINUE

                  CALL PERIOD_DEALL(WK2PTR)
                  CALL PERIOD_DEALL(WK1PTR)
                  CALL PERIOD_DEALL(YRANPTR)
                  CALL PERIOD_DEALL(YDATAPTR)
                  CALL PERIOD_DEALL(XDATAPTR)
                  CALL PERIOD_DEALL(DATAPTR)

C-----------------------------------------------------------------------------
C ft - Permutations complete - calculate significance; load output arrays.
C-----------------------------------------------------------------------------

               IF ( LSIG ) THEN
                  SIGNOISE = 0.0D0
                  DO 380 I = 2, NPERMS + 1
                     IF ( STAT(I).GE.STAT(1) ) SIGNOISE = SIGNOISE + 1.0D0
 380              CONTINUE
                  SIG(1, SLOTOUT) = DFLOAT(NPERMS)
                  SIG(2, SLOTOUT) = SIGNOISE/DFLOAT(NPERMS)
               END IF

C-----------------------------------------------------------------------------
C ft - Process the next data slot.
C-----------------------------------------------------------------------------

 390        CONTINUE
            GO TO 100

 394        CONTINUE
            CALL PERIOD_DEALL(DATAPTR)
            GO TO 100

         ELSE IF ( PERIOD_PARSE(COMMAND,'SCARGLE') ) THEN

C**********-------------------------------------------------------------------
C SCARGLE * Fast evaluation of the Lomb-Scargle statistic for each frequency.
C**********-------------------------------------------------------------------

            IF ( .NOT.LSELECT ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: No slots selected.'
               GO TO 100
            ELSE
               LSELECT = .FALSE.
            END IF

C-----------------------------------------------------------------------------
C scargle - Loop through slots to be processed.
C-----------------------------------------------------------------------------

            DO 490 SLOT = FIRSTSLOT, LASTSLOT
               NDATA = NPTSARRAY(SLOT)

               IF ( NDATA.EQ.0 ) THEN
                  CALL PERIOD_WRITEBELL()
                  WRITE (*, *) '** ERROR: Slot empty =', SLOT
                  GO TO 100
               END IF

               SLOTOUT = FIRSTOUT + (SLOT-FIRSTSLOT)
               YSLOT1 = YPTR(SLOT)

C-----------------------------------------------------------------------------
C scargle - Loop through permutations.
C-----------------------------------------------------------------------------

               DO 470 SAMPLE = 1, NPERMS + 1
                  IF ( SAMPLE.EQ.1 ) THEN

C-----------------------------------------------------------------------------
C scargle - Set frequency limits.
C-----------------------------------------------------------------------------

                     CALL PERIOD_ALLOC('_DOUBLE', NDATA, DATAPTR)

                     CALL PERIOD_SETDATA(%VAL(CNF_PVAL(YSLOT1)),
     :                                   NDATA, MXCOL,
     :                                   1, %VAL(CNF_PVAL(DATAPTR)))
                     WRITE (*, *) ' '

                     IF ( SLOT.EQ.SLOTOUT ) THEN
                        MAXPTS = NDATA
                     ELSE
                        MAXPTS = 0
                     ENDIF

                     CALL PERIOD_AUTOLIM(%VAL(CNF_PVAL(DATAPTR)), NDATA,
     :                                   MAXPTS,
     :                                   MINFREQ, MAXFREQ, FINTERVAL,
     :                                   FMIN, FMAX, FINT, IFAIL)

                     IF ( IFAIL.EQ.1 ) GO TO 494
                     WRITE (*, *) ' '
                     WRITE (*, *) ' '

                     IF ( SLOT.NE.SLOTOUT ) THEN
                        IF ( NPTSARRAY(SLOTOUT).NE.0 )
     :                     CALL PERIOD_DEALL(YPTR(SLOTOUT))
                        CALL PERIOD_ALLOC('_DOUBLE', MAXPTS*MXCOL,
     :                                    YPTR(SLOTOUT))
                     END IF

                     YSLOT2 = YPTR(SLOTOUT)

                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, XDATAPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, YDATAPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, YRANPTR)

                  END IF

C-----------------------------------------------------------------------------
C scargle - Loop through trial frequencies.
C-----------------------------------------------------------------------------

                  CALL PERIOD_SETXYDATA(%VAL(CNF_PVAL(YSLOT1)),
     :                                  NDATA, MXCOL,
     :                                  SAMPLE,
     :                                  %VAL(CNF_PVAL(XDATAPTR)),
     :                                  %VAL(CNF_PVAL(YDATAPTR)),
     :                                  %VAL(CNF_PVAL(YRANPTR)))

C------------------------------------------------------------------------------
C Compute the mean and variance of the data.
C------------------------------------------------------------------------------

                  CALL PERIOD_MOMENT(%VAL(CNF_PVAL(YDATAPTR)), NDATA,
     :                               AVE,
     :                               ADEV, SDEV, VARI)

                  IF ( VARI.EQ.0.0D0 ) THEN
                     CALL PERIOD_WRITEBELL()
                     WRITE (*, *)
     :                     '** ERROR: Zero variance in PERIOD_SCARGLE.'
                     GO TO 492
                  END IF

                  CALL PERIOD_PREPSCARGLE(%VAL(CNF_PVAL(XDATAPTR)),
     :                                    %VAL(CNF_PVAL(YDATAPTR)),
     :                                    NDATA, FMAX,
     :                                    FINT, NWK, XMIN, XDIF, OFAC,
     :                                    HIFAC, NFREQ)

                  CALL PERIOD_ALLOC('_DOUBLE', NWK, WORK1PTR)
                  CALL PERIOD_ALLOC('_DOUBLE', NWK, WORK2PTR)

                  CALL PERIOD_SCARGLE(%VAL(CNF_PVAL(XDATAPTR)),
     :                                %VAL(CNF_PVAL(YDATAPTR)),
     :                                NDATA, %VAL(CNF_PVAL(WORK1PTR)),
     :                                %VAL(CNF_PVAL(WORK2PTR)), NWK,
     :                                NOUT,
     :                                IFAIL, FMIN, FMAX, FINT, SAMPLE,
     :                                AVE, VARI, XMIN, XDIF,
     :                                OFAC, HIFAC, NFREQ)

                  IF ( IFAIL.EQ.1 ) THEN
                     CALL PERIOD_WRITEBELL()
                     WRITE (*, *)
     :                    '** ERROR: Lomb-Scargle statistic calculation'
     :                    // ' unsuccessful.'
                     GO TO 492
                  END IF

C-----------------------------------------------------------------------------
C scargle - Periodogram complete. If first permutation, load output arrays.
C-----------------------------------------------------------------------------

                  IF ( SAMPLE.EQ.1 ) THEN

                     CALL PERIOD_LOADOUTALL(%VAL(CNF_PVAL(YSLOT2)),
     :                           NOUT, MXCOL,
     :                           %VAL(CNF_PVAL(WORK1PTR)),
     :                           %VAL(CNF_PVAL(WORK2PTR)), NWK)

                     YERRORARRAY(SLOTOUT) = .FALSE.
                     DETRENDARRAY(SLOTOUT) = .FALSE.
                     INFILEARRAY(SLOTOUT)
     :                   = 'Lomb-Scargle Statistic vs Freq ' //
     :                  'For ' // INFILEARRAY(SLOT)
                     NPTSARRAY(SLOTOUT) = NOUT
                     SIG(1, SLOTOUT) = -1.0D0
                     SIG(2, SLOTOUT) = -1.0D0
                     WRITE (*, *) ' '
                     WRITE (*, *) '** OK: Filled slot = ', SLOTOUT
                     IF ( LSIG ) THEN
                        WRITE (*, *) ' '
                        WRITE (*, *) ' '
                     END IF
                  END IF

C-----------------------------------------------------------------------------
C scargle - Determine significance information if requested.
C-----------------------------------------------------------------------------

                  IF ( LSIG ) THEN
                     STAT(SAMPLE) = DNMX30

                     CALL PERIOD_SIGINFOGT(%VAL(CNF_PVAL(YSLOT2)),
     :                           MAXPTS, MXCOL,
     :                           NOUT, STAT, MAXPERMS+1,
     :                           SAMPLE,
     :                           %VAL(CNF_PVAL(WORK2PTR)), NWK)

                     IF ( SAMPLE.GT.1 ) THEN
                        WRITE (*, 99004) SAMPLE - 1
99004                   FORMAT ('+** OK: Processed permutation = ', I4)
                     END IF

C-----------------------------------------------------------------------------
C scargle - Randomize time-series for next permutation.
C-----------------------------------------------------------------------------

                     RANDOM = PERIOD_RAN1(SEED)
                     DO 460 J = 1, 1 + IDINT(RANDOM*2.0D0)

                        CALL PERIOD_YSHUFFLE(SEED,
     :                                       %VAL(CNF_PVAL(YRANPTR)),
     :                                       NDATA)
 460                 CONTINUE

                  END IF

                  CALL PERIOD_DEALL(WORK2PTR)
                  CALL PERIOD_DEALL(WORK1PTR)

 470           CONTINUE

                  CALL PERIOD_DEALL(YRANPTR)
                  CALL PERIOD_DEALL(YDATAPTR)
                  CALL PERIOD_DEALL(XDATAPTR)
                  CALL PERIOD_DEALL(DATAPTR)

C-----------------------------------------------------------------------------
C scargle - Permutations complete - calculate significance; load output arrays
C-----------------------------------------------------------------------------

               IF ( LSIG ) THEN
                  SIGNOISE = 0.0D0
                  DO 480 I = 2, NPERMS + 1
                     IF ( STAT(I).GE.STAT(1) ) SIGNOISE = SIGNOISE + 1.0D0
 480              CONTINUE
                  SIG(1, SLOTOUT) = DFLOAT(NPERMS)
                  SIG(2, SLOTOUT) = SIGNOISE/DFLOAT(NPERMS)
               END IF

C-----------------------------------------------------------------------------
C scargle - Process the next data slot.
C-----------------------------------------------------------------------------

 490        CONTINUE
            GO TO 100

 492        CONTINUE
            CALL PERIOD_DEALL(WORK2PTR)
            CALL PERIOD_DEALL(WORK1PTR)
            CALL PERIOD_DEALL(YRANPTR)
            CALL PERIOD_DEALL(YDATAPTR)
            CALL PERIOD_DEALL(XDATAPTR)

 494        CONTINUE
            CALL PERIOD_DEALL(DATAPTR)
            GO TO 100

         ELSE IF ( PERIOD_PARSE(COMMAND,'CLEAN') ) THEN

C**********-------------------------------------------------------------------
C CLEAN   * CLEANed power spectrum.
C**********-------------------------------------------------------------------

            IF ( .NOT.LSELECT ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: No slots selected.'
               GO TO 100
            ELSE
               LSELECT = .FALSE.
            END IF

            WRITE (*, *) ' '

 510        CONTINUE
            WRITE (*, '(X,A,$)')
     :         'Enter number of CLEAN iterations : '
            READ (*, *, ERR=510) NCL
 520        CONTINUE
            WRITE (*, '(X,A,$)') 'Enter loop gain : '
            READ (*, *, ERR=520) GAIN
            IF ( (GAIN.LE.0.0D0) .OR. (GAIN.GE.2.0D0) ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: Gain must lie between 0 and 2.'
               GO TO 100
            END IF

C-----------------------------------------------------------------------------
C clean - Loop through slots to be processed.
C-----------------------------------------------------------------------------

            DO 590 SLOT = FIRSTSLOT, LASTSLOT
               NDATA = NPTSARRAY(SLOT)

               IF ( NDATA.EQ.0 ) THEN
                  CALL PERIOD_WRITEBELL()
                  WRITE (*, *) '** ERROR: Slot empty =', SLOT
                  GO TO 100
               END IF

               SLOTOUT = FIRSTOUT + (SLOT-FIRSTSLOT)
               YSLOT1 = YPTR(SLOT)

C-----------------------------------------------------------------------------
C clean - Loop through permutations.
C-----------------------------------------------------------------------------

               DO 570 SAMPLE = 1, NPERMS + 1
                  IF ( SAMPLE.EQ.1 ) THEN

C-----------------------------------------------------------------------------
C clean - Set frequency limits.
C-----------------------------------------------------------------------------

                     CALL PERIOD_ALLOC('_DOUBLE', NDATA, DATAPTR)

                     CALL PERIOD_SETDATA(%VAL(CNF_PVAL(YSLOT1)),
     :                                   NDATA, MXCOL,
     :                                   1, %VAL(CNF_PVAL(DATAPTR)))
                     WRITE (*, *) ' '
                     FMIN = 0.0D0

                     IF ( SLOT.EQ.SLOTOUT ) THEN
                        MAXPTS = NDATA
                     ELSE
                        MAXPTS = 0
                     ENDIF

                     CALL PERIOD_AUTOLIM(%VAL(CNF_PVAL(DATAPTR)), NDATA,
     :                                   MAXPTS,
     :                                   MINFREQ, MAXFREQ, FINTERVAL,
     :                                   FMIN, FMAX, FINT, IFAIL)

                     IF ( IFAIL.EQ.1 ) GO TO 594
                     WRITE (*, *) ' '

                     IF ( SLOT.NE.SLOTOUT ) THEN
                        IF ( NPTSARRAY(SLOTOUT).NE.0 )
     :                     CALL PERIOD_DEALL(YPTR(SLOTOUT))
                        CALL PERIOD_ALLOC('_DOUBLE', MAXPTS*MXCOL,
     :                                    YPTR(SLOTOUT))
                     END IF

                     YSLOT2 = YPTR(SLOTOUT)

                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, XDATAPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, YDATAPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, YRANPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', MAXPTS, WK1PTR)
                  CALL PERIOD_ALLOC('_DOUBLE', MAXPTS, WK2PTR)

                  END IF

C-----------------------------------------------------------------------------
C clean - Loop through trial frequencies.
C-----------------------------------------------------------------------------

                  CALL PERIOD_SETXYDATA(%VAL(CNF_PVAL(YSLOT1)), NDATA,
     :                                  MXCOL,
     :                                  SAMPLE,
     :                                  %VAL(CNF_PVAL(XDATAPTR)),
     :                                  %VAL(CNF_PVAL(YDATAPTR)),
     :                                  %VAL(CNF_PVAL(YRANPTR)))

                  CALL PERIOD_CLEAN(%VAL(CNF_PVAL(XDATAPTR)),
     :                        %VAL(CNF_PVAL(YDATAPTR)),
     :                        NDATA, MAXPTS, FMIN, FMAX, FINT,
     :                        NCL, GAIN, %VAL(CNF_PVAL(WK1PTR)),
     :                        %VAL(CNF_PVAL(WK2PTR)), NOUT, SAMPLE)

                  IF ( NOUT.EQ.0 ) THEN
                     CALL PERIOD_WRITEBELL()
                     WRITE (*, *)
     :                  '** ERROR: CLEAN calculation unsuccessful.'
                     NPTSARRAY(SLOTOUT) = NOUT
                     GO TO 592
                  END IF

C-----------------------------------------------------------------------------
C clean - Periodogram complete. If first permutation, load output arrays.
C-----------------------------------------------------------------------------

                  IF ( SAMPLE.EQ.1 ) THEN

                     CALL PERIOD_LOADOUTALL(%VAL(CNF_PVAL(YSLOT2)),
     :                                      NOUT,
     :                                      MXCOL,
     :                                      %VAL(CNF_PVAL(WK1PTR)),
     :                                      %VAL(CNF_PVAL(WK2PTR)),
     :                                      MAXPTS)

                     YERRORARRAY(SLOTOUT) = .FALSE.
                     DETRENDARRAY(SLOTOUT) = .FALSE.
                     INFILEARRAY(SLOTOUT) = 'CLEANed Power ' //
     :                                      'vs Freq For ' //
     :                                      INFILEARRAY(SLOT)
                     NPTSARRAY(SLOTOUT) = NOUT
                     SIG(1, SLOTOUT) = -1.0D0
                     SIG(2, SLOTOUT) = -1.0D0
                     WRITE (*, *) ' '
                     WRITE (*, *) '** OK: Filled slot = ', SLOTOUT
                     IF ( LSIG ) THEN
                        WRITE (*, *) ' '
                        WRITE (*, *) ' '
                     END IF
                  END IF

C-----------------------------------------------------------------------------
C clean - Determine significance information if requested.
C-----------------------------------------------------------------------------

                  IF ( LSIG ) THEN
                     STAT(SAMPLE) = DNMX30

                     CALL PERIOD_SIGINFOGT(%VAL(CNF_PVAL(YSLOT2)),
     :                                     MAXPTS, MXCOL,
     :                                     NOUT, STAT, MAXPERMS+1,
     :                                     SAMPLE,
     :                                     %VAL(CNF_PVAL(WK2PTR)),
     :                                     MAXPTS)

                     IF ( SAMPLE.GT.1 ) THEN
                        WRITE (*, 99007) SAMPLE - 1
99007                   FORMAT ('+** OK: Processed permutation = ', I4)
                     END IF

C-----------------------------------------------------------------------------
C clean - Randomize time-series for next permutation.
C-----------------------------------------------------------------------------

                     RANDOM = PERIOD_RAN1(SEED)
                     DO 560 J = 1, 1 + IDINT(RANDOM*2.0D0)

                        CALL PERIOD_YSHUFFLE(SEED,
     :                                       %VAL(CNF_PVAL(YRANPTR)),
     :                                       NDATA)
 560                 CONTINUE

                  END IF

 570           CONTINUE

                  CALL PERIOD_DEALL(WK2PTR)
                  CALL PERIOD_DEALL(WK1PTR)
                  CALL PERIOD_DEALL(YRANPTR)
                  CALL PERIOD_DEALL(YDATAPTR)
                  CALL PERIOD_DEALL(XDATAPTR)
                  CALL PERIOD_DEALL(DATAPTR)

C-----------------------------------------------------------------------------
C clean - Permutations complete - calculate significance; load output arrays.
C-----------------------------------------------------------------------------

               IF ( LSIG ) THEN
                  SIGNOISE = 0.0D0
                  DO 580 I = 2, NPERMS + 1
                     IF ( STAT(I).GE.STAT(1) )
     :                  SIGNOISE = SIGNOISE + 1.0D0
 580              CONTINUE
                  SIG(1, SLOTOUT) = DFLOAT(NPERMS)
                  SIG(2, SLOTOUT) = SIGNOISE/DFLOAT(NPERMS)
               END IF

C-----------------------------------------------------------------------------
C clean - Process the next data slot.
C-----------------------------------------------------------------------------

 590        CONTINUE
            GO TO 100

 592        CONTINUE
            CALL PERIOD_DEALL(WK2PTR)
            CALL PERIOD_DEALL(WK1PTR)
            CALL PERIOD_DEALL(YRANPTR)
            CALL PERIOD_DEALL(YDATAPTR)
            CALL PERIOD_DEALL(XDATAPTR)

 594        CONTINUE
            CALL PERIOD_DEALL(DATAPTR)
            GO TO 100

         ELSE IF ( PERIOD_PARSE(COMMAND,'STRING') ) THEN

C**********-------------------------------------------------------------------
C STRING  * String-length method.
C**********-------------------------------------------------------------------

            IF ( .NOT.LSELECT ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: No slots selected.'
               GO TO 100
            ELSE
               LSELECT = .FALSE.
            END IF

C-----------------------------------------------------------------------------
C string - Loop through slots to be processed.
C-----------------------------------------------------------------------------

            DO 690 SLOT = FIRSTSLOT, LASTSLOT
               NDATA = NPTSARRAY(SLOT)

               IF ( NPTSARRAY(SLOT).EQ.0 ) THEN
                  CALL PERIOD_WRITEBELL()
                  WRITE (*, *) '** ERROR: Slot empty =', SLOT
                  GO TO 100
               END IF

               SLOTOUT = FIRSTOUT + (SLOT-FIRSTSLOT)
               YSLOT1 = YPTR(SLOT)

C-----------------------------------------------------------------------------
C string - Loop through permutations.
C-----------------------------------------------------------------------------

               DO 670 SAMPLE = 1, NPERMS + 1
                  IF ( SAMPLE.EQ.1 ) THEN

C-----------------------------------------------------------------------------
C string - Set frequency limits.
C-----------------------------------------------------------------------------

                     CALL PERIOD_ALLOC('_DOUBLE', NDATA, DATAPTR)

                     CALL PERIOD_SETDATA(%VAL(CNF_PVAL(YSLOT1)),
     ;                                   NDATA, MXCOL,
     :                                   1, %VAL(CNF_PVAL(DATAPTR)))
                     WRITE (*, *) ' '

                     IF ( SLOT.EQ.SLOTOUT ) THEN
                        MAXPTS = NDATA
                     ELSE
                        MAXPTS = 0
                     ENDIF

                     CALL PERIOD_AUTOLIM(%VAL(CNF_PVAL(DATAPTR)),
     :                                   NDATA, MAXPTS,
     :                                   MINFREQ, MAXFREQ, FINTERVAL,
     :                                   FMIN, FMAX, FINT, IFAIL)

                     IF ( IFAIL.EQ.1 ) GO TO 694
                     IF ( FMIN.EQ.0.0D0 ) THEN
                        CALL PERIOD_WRITEBELL()
                        WRITE (*, *)
     :                  '** ERROR: No string-length for zero frequency.'
                        GO TO 694
                     END IF

C-----------------------------------------------------------------------------
C string - Set zero point to be the mean observation time.
C-----------------------------------------------------------------------------

                     CALL PERIOD_MOMENT(%VAL(CNF_PVAL(DATAPTR)),
     :                                  NDATA, AVE,
     :                                  ADEV, SDEV, VARI)
                     ZEROPT = AVE
                     WRITE (*, *) ' '
                     WRITE (*, *) ' '

                     IF ( SLOT.NE.SLOTOUT ) THEN
                        IF ( NPTSARRAY(SLOTOUT).NE.0 )
     :                     CALL PERIOD_DEALL(YPTR(SLOTOUT))
                        CALL PERIOD_ALLOC('_DOUBLE', MAXPTS*MXCOL,
     :                                    YPTR(SLOTOUT))
                     END IF

                     YSLOT2 = YPTR(SLOTOUT)

                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, XDATAPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, YDATAPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, YRANPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, YERRPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', MAXPTS, WK2PTR)

                  END IF

C-----------------------------------------------------------------------------
C string - Fold on trial frequencies and calculate string-lengths.
C-----------------------------------------------------------------------------

                  COUNTER = 0
                  LOOP = 1
                  FREQ = FMIN
                  DO 650 WHILE ( FREQ .LE. FMAX )

                     CALL PERIOD_SETXYDATA(%VAL(CNF_PVAL(YSLOT1)),
     :                                     NDATA, MXCOL,
     :                                     SAMPLE,
     :                                     %VAL(CNF_PVAL(XDATAPTR)),
     :                                     %VAL(CNF_PVAL(YDATAPTR)),
     :                                     %VAL(CNF_PVAL(YRANPTR)))
                     PERIOD = 1.0D0/FREQ

                     CALL PERIOD_FOLD(%VAL(CNF_PVAL(XDATAPTR)),
     :                                %VAL(CNF_PVAL(YDATAPTR)),
     :                                %VAL(CNF_PVAL(YERRPTR)),
     :                                NDATA, ZEROPT,
     :                                PERIOD, IFAIL)

                     IF ( IFAIL.EQ.1 ) GO TO 692

                     CALL PERIOD_STRING(%VAL(CNF_PVAL(XDATAPTR)),
     :                                  %VAL(CNF_PVAL(YDATAPTR)),
     :                                  NDATA, STRING,
     :                                  IFAIL)

                     IF ( IFAIL.EQ.1 ) THEN
                        CALL PERIOD_WRITEBELL()
                        WRITE (*, *) '** ERROR: String-length' //
     :                               ' calculation unsuccessful.'
                        GO TO 692
                     END IF

                     COUNTER = COUNTER + 1
                     IF ( SAMPLE.EQ.1 ) THEN
                        IF ( COUNTER.EQ.(LOOP*ISTEP) ) THEN
                           WRITE (*, 99005) FREQ, STRING
99005                      FORMAT ('+Trial Frequency =', D12.6,
     :                             ',  String Length =', D18.6)
                           LOOP = LOOP + 1
                        END IF
                     END IF

C-----------------------------------------------------------------------------
C string - If the first permutation, load the output slot.
C-----------------------------------------------------------------------------

                     IF ( SAMPLE.EQ.1 ) THEN

                        CALL PERIOD_LOADOUTONE(%VAL(CNF_PVAL(YSLOT2)),
     :                                         MAXPTS,
     :                                         MXCOL, COUNTER, FREQ,
     :                                         STRING)
                     END IF

C                    WK2(COUNTER) = STRING

                     CALL PERIOD_PUT1D(STRING, COUNTER,
     :                                 %VAL(CNF_PVAL(WK2PTR)),
     :                                 MAXPTS)
                     FREQ = FREQ + FINT
 650              CONTINUE

C-----------------------------------------------------------------------------
C string - Periodogram complete. If first permutation, load output arrays.
C-----------------------------------------------------------------------------

                  IF ( SAMPLE.EQ.1 ) THEN
                     YERRORARRAY(SLOTOUT) = .FALSE.
                     DETRENDARRAY(SLOTOUT) = .FALSE.
                     INFILEARRAY(SLOTOUT)
     :                   = 'String Length vs Trial Freq ' // 'For ' //
     :                  INFILEARRAY(SLOT)
                     NPTSARRAY(SLOTOUT) = COUNTER
                     SIG(1, SLOTOUT) = -1.0D0
                     SIG(2, SLOTOUT) = -1.0D0
                     WRITE (*, *) ' '
                     WRITE (*, *) '** OK: Filled slot = ', SLOTOUT
                     IF ( LSIG ) THEN
                        WRITE (*, *) ' '
                        WRITE (*, *) ' '
                     END IF
                  END IF

C-----------------------------------------------------------------------------
C string - Determine significance information if requested.
C-----------------------------------------------------------------------------

                  IF ( LSIG ) THEN
                     STAT(SAMPLE) = DPMX30

                     CALL PERIOD_SIGINFOLT(%VAL(CNF_PVAL(YSLOT2)),
     :                                     MAXPTS, MXCOL,
     :                                     COUNTER, STAT, MAXPERMS+1,
     :                                     SAMPLE,
     :                                     %VAL(CNF_PVAL(WK2PTR)),
     :                                     MAXPTS)

                     IF ( SAMPLE.GT.1 ) THEN
                        WRITE (*, 99006) SAMPLE - 1
99006                   FORMAT ('+** OK: Processed permutation = ', I4)
                     END IF

C-----------------------------------------------------------------------------
C string - Randomize time-series for next permutation.
C-----------------------------------------------------------------------------

                     RANDOM = PERIOD_RAN1(SEED)
                     DO 660 J = 1, 1 + IDINT(RANDOM*2.0D0)

                        CALL PERIOD_YSHUFFLE(SEED,
     ;                                       %VAL(CNF_PVAL(YRANPTR)),
     :                                       NDATA)
 660                 CONTINUE

                  END IF

 670           CONTINUE

                  CALL PERIOD_DEALL(WK2PTR)
                  CALL PERIOD_DEALL(YERRPTR)
                  CALL PERIOD_DEALL(YRANPTR)
                  CALL PERIOD_DEALL(YDATAPTR)
                  CALL PERIOD_DEALL(XDATAPTR)
                  CALL PERIOD_DEALL(DATAPTR)

C-----------------------------------------------------------------------------
C string - Permutations complete - calculate significance; load output arrays.
C-----------------------------------------------------------------------------

               IF ( LSIG ) THEN
                  SIGNOISE = 0.0D0
                  DO 680 I = 2, NPERMS + 1
                     IF ( STAT(I).LE.STAT(1) ) SIGNOISE = SIGNOISE + 1.0D0
 680              CONTINUE
                  SIG(1, SLOTOUT) = DFLOAT(NPERMS)
                  SIG(2, SLOTOUT) = SIGNOISE/DFLOAT(NPERMS)
               END IF

C-----------------------------------------------------------------------------
C string - Process the next data slot.
C-----------------------------------------------------------------------------

 690        CONTINUE
            GO TO 100

 692        CONTINUE
            CALL PERIOD_DEALL(WK2PTR)
            CALL PERIOD_DEALL(YERRPTR)
            CALL PERIOD_DEALL(YRANPTR)
            CALL PERIOD_DEALL(YDATAPTR)
            CALL PERIOD_DEALL(XDATAPTR)

 694        CONTINUE
            CALL PERIOD_DEALL(DATAPTR)
            GO TO 100

         ELSE IF ( PERIOD_PARSE(COMMAND,'PDM') ) THEN

C**********-------------------------------------------------------------------
C PDM     * Phase dispersion minimization (PDM) method.
C**********-------------------------------------------------------------------

            IF ( .NOT.LSELECT ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: No slots selected.'
               GO TO 100
            ELSE
               LSELECT = .FALSE.
            END IF
            WRITE (*, *) ' '

 710        CONTINUE
            WRITE (*, '(X,A,$)') 'Enter number of bins : '
            READ (*, *, ERR=710) NBIN
            IF ( (NBIN.LT.2) ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: Number of bins must be' //
     :                      ' greater than 2'
               GO TO 100
            END IF

 720        CONTINUE
            WRITE (*, '(X,A,$)') 'Enter width of each bin : '
            READ (*, *, ERR=720) WBIN
            IF ( (WBIN.LE.0.0D0) .OR. (WBIN.GE.1.0D0) ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: Width of bins must be' //
     :                      ' greater than 0 and less than 1'
               GO TO 100
            END IF

C-----------------------------------------------------------------------------
C pdm - Loop through slots to be processed.
C-----------------------------------------------------------------------------

            DO 790 SLOT = FIRSTSLOT, LASTSLOT
               NDATA = NPTSARRAY(SLOT)

               IF ( NPTSARRAY(SLOT).EQ.0 ) THEN
                  CALL PERIOD_WRITEBELL()
                  WRITE (*, *) '** ERROR: Slot empty =', SLOT
                  GO TO 100
               END IF

               SLOTOUT = FIRSTOUT + (SLOT-FIRSTSLOT)
               YSLOT1 = YPTR(SLOT)

C-----------------------------------------------------------------------------
C pdm - Loop through permutations.
C-----------------------------------------------------------------------------

               DO 770 SAMPLE = 1, NPERMS + 1
                  IF ( SAMPLE.EQ.1 ) THEN

C-----------------------------------------------------------------------------
C pdm - Set frequency limits.
C-----------------------------------------------------------------------------

                     CALL PERIOD_ALLOC('_DOUBLE', NDATA, DATAPTR)

                     CALL PERIOD_SETDATA(%VAL(CNF_PVAL(YSLOT1)),
     :                                   NDATA, MXCOL,
     :                                   1, %VAL(CNF_PVAL(DATAPTR)))

                     WRITE (*, *) ' '

                     IF ( SLOT.EQ.SLOTOUT ) THEN
                        MAXPTS = NDATA
                     ELSE
                        MAXPTS = 0
                     ENDIF

                     CALL PERIOD_AUTOLIM(%VAL(CNF_PVAL(DATAPTR)),
     :                                   NDATA, MAXPTS,
     :                                   MINFREQ, MAXFREQ, FINTERVAL,
     :                                   FMIN, FMAX, FINT, IFAIL)

                     IF ( IFAIL.EQ.1 ) GO TO 794
                     IF ( FMIN.EQ.0.0D0 ) THEN
                        CALL PERIOD_WRITEBELL()
                        WRITE (*, *)
     :                  '** ERROR: No PDM statistic for zero frequency.'
                        GO TO 794
                     END IF

C-----------------------------------------------------------------------------
C pdm - Set zero point to be the mean observation time.
C-----------------------------------------------------------------------------

                     CALL PERIOD_MOMENT(%VAL(CNF_PVAL(DATAPTR)),
     :                                  NDATA, AVE,
     :                                  ADEV, SDEV, VARI)
                     ZEROPT = AVE

C-----------------------------------------------------------------------------
C pdm - Calculate variance of whole dataset.
C-----------------------------------------------------------------------------

                     CALL PERIOD_SETDATA(%VAL(CNF_PVAL(YSLOT1)),
     ;                                   NDATA, MXCOL,
     :                                   2, %VAL(CNF_PVAL(DATAPTR)))

                     CALL PERIOD_MOMENT(%VAL(CNF_PVAL(DATAPTR)),
     ;                                  NDATA, AVE,
     :                                  ADEV, SDEV, VARI)
                     WRITE (*, *) ' '
                     WRITE (*, *) ' '

                     IF ( SLOT.NE.SLOTOUT ) THEN
                        IF ( NPTSARRAY(SLOTOUT).NE.0 )
     :                     CALL PERIOD_DEALL(YPTR(SLOTOUT))
                        CALL PERIOD_ALLOC('_DOUBLE', MAXPTS*MXCOL,
     :                                    YPTR(SLOTOUT))
                     END IF

                     YSLOT2 = YPTR(SLOTOUT)

                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, XDATAPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, YDATAPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, YRANPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', NDATA, YERRPTR)
                  CALL PERIOD_ALLOC('_DOUBLE', MAXPTS, WK2PTR)

                  END IF

C-----------------------------------------------------------------------------
C pdm - Fold on trial frequencies and calculate PDM statistic.
C-----------------------------------------------------------------------------

                  COUNTER = 0
                  LOOP = 0

                  TOLFACT = FINT*1.0D-4

                  FREQ = FMIN
                  DO 750 WHILE ( FREQ .LE. FMAX + TOLFACT )

                     CALL PERIOD_SETXYDATA(%VAL(CNF_PVAL(YSLOT1)),
     ;                                     NDATA, MXCOL,
     :                                     SAMPLE,
     :                                     %VAL(CNF_PVAL(XDATAPTR)),
     :                                     %VAL(CNF_PVAL(YDATAPTR)),
     :                                     %VAL(CNF_PVAL(YRANPTR)))
                     PERIOD = 1.0D0/FREQ

                     CALL PERIOD_FOLD(%VAL(CNF_PVAL(XDATAPTR)),
     :                                %VAL(CNF_PVAL(YDATAPTR)),
     :                                %VAL(CNF_PVAL(YERRPTR)),
     ;                                NDATA, ZEROPT,
     :                                PERIOD, IFAIL)

                     IF ( IFAIL.EQ.1 ) GO TO 792

                     CALL PERIOD_PDM(%VAL(CNF_PVAL(XDATAPTR)),
     :                               %VAL(CNF_PVAL(YDATAPTR)),
     :                               NDATA, NBIN, WBIN, VARI,
     :                               PDM, MAXPTS, IFAIL)

                     IF ( IFAIL.EQ.1 ) THEN
                        CALL PERIOD_WRITEBELL()
                        WRITE (*, *) '** ERROR: PDM statistic ' //
     :                         ' calculation unsuccessful.'
                        GO TO 792
                     END IF

                     COUNTER = COUNTER + 1
                     IF ( SAMPLE.EQ.1 ) THEN
                        IF ( COUNTER-1.EQ.(LOOP*ISTEP) ) THEN
                           WRITE (*, 99008) FREQ, PDM
99008                      FORMAT ('+Trial Frequency =', D12.6,
     :                       ',  PDM Statistic =', D18.6)
                           LOOP = LOOP + 1
                        END IF
                     END IF

C-----------------------------------------------------------------------------
C pdm - If the first permutation, load the output slot.
C-----------------------------------------------------------------------------

                     IF ( SAMPLE.EQ.1 ) THEN

                        CALL PERIOD_LOADOUTONE(%VAL(CNF_PVAL(YSLOT2)),
     :                                         MAXPTS,
     :                                         MXCOL, COUNTER, FREQ,
     :                                         PDM)
                     END IF

C                    WK2(COUNTER) = PDM

                     CALL PERIOD_PUT1D(PDM, COUNTER,
     :                                 %VAL(CNF_PVAL(WK2PTR)),
     :                                 MAXPTS)
                     FREQ = FREQ + FINT
 750              CONTINUE

C-----------------------------------------------------------------------------
C pdm - Periodogram complete. If first permutation, load output arrays.
C-----------------------------------------------------------------------------

                  IF ( SAMPLE.EQ.1 ) THEN
                     YERRORARRAY(SLOTOUT) = .FALSE.
                     DETRENDARRAY(SLOTOUT) = .FALSE.
                     INFILEARRAY(SLOTOUT) =
     :                            'PDM Statistic vs Trial Freq ' //
     :                                'For ' // INFILEARRAY(SLOT)
                     NPTSARRAY(SLOTOUT) = COUNTER
                     WRITE (*, *) ' '
                     WRITE (*, *) '** OK: Filled slot = ', SLOTOUT
                     IF ( LSIG ) THEN
                        WRITE (*, *) ' '
                        WRITE (*, *) ' '
                     END IF
                  END IF

C-----------------------------------------------------------------------------
C pdm - Determine significance information if requested.
C-----------------------------------------------------------------------------

                  IF ( LSIG ) THEN
                     STAT(SAMPLE) = DPMX30

                     CALL PERIOD_SIGINFOLT(%VAL(CNF_PVAL(YSLOT2)),
     ;                                     MAXPTS, MXCOL,
     :                                     COUNTER, STAT, MAXPERMS+1,
     :                                     SAMPLE,
     :                                     %VAL(CNF_PVAL(WK2PTR)),
     :                                     MAXPTS)

                     IF ( SAMPLE.GT.1 ) THEN
                        WRITE (*, 99009) SAMPLE - 1
99009                   FORMAT ('+** OK: Processed permutation = ', I4)
                     END IF

C-----------------------------------------------------------------------------
C pdm - Randomize time-series for next permutation.
C-----------------------------------------------------------------------------

                     RANDOM = PERIOD_RAN1(SEED)
                     DO 760 J = 1, 1 + IDINT(RANDOM*2.0D0)

                        CALL PERIOD_YSHUFFLE(SEED,
     :                                       %VAL(CNF_PVAL(YRANPTR)),
     :                                       NDATA)

 760                 CONTINUE
                  END IF

 770           CONTINUE

                  CALL PERIOD_DEALL(WK2PTR)
                  CALL PERIOD_DEALL(YERRPTR)
                  CALL PERIOD_DEALL(YRANPTR)
                  CALL PERIOD_DEALL(YDATAPTR)
                  CALL PERIOD_DEALL(XDATAPTR)
                  CALL PERIOD_DEALL(DATAPTR)

C-----------------------------------------------------------------------------
C pdm - Permutations complete - calculate significance; load output arrays.
C-----------------------------------------------------------------------------

               IF ( LSIG ) THEN
                  SIGNOISE = 0.0D0
                  DO 780 I = 2, NPERMS + 1
                     IF ( STAT(I).LE.STAT(1) )
     :                  SIGNOISE = SIGNOISE + 1.0D0
 780              CONTINUE
                  SIG(1, SLOTOUT) = DFLOAT(NPERMS)
                  SIG(2, SLOTOUT) = SIGNOISE/DFLOAT(NPERMS)
               END IF

C-----------------------------------------------------------------------------
C pdm - Process the next data slot.
C-----------------------------------------------------------------------------

 790        CONTINUE
            GO TO 100

 792        CONTINUE
            CALL PERIOD_DEALL(WK2PTR)
            CALL PERIOD_DEALL(YERRPTR)
            CALL PERIOD_DEALL(YRANPTR)
            CALL PERIOD_DEALL(YDATAPTR)
            CALL PERIOD_DEALL(XDATAPTR)

 794        CONTINUE
            CALL PERIOD_DEALL(DATAPTR)
            GO TO 100

         ELSE IF ( PERIOD_PARSE(COMMAND,'PLT') ) THEN

C**********-------------------------------------------------------------------
C PLT     * Call PLT
C**********-------------------------------------------------------------------

            CALL PERIOD_PLT(YPTR, NPTSARRAY, MXCOL,
     :                      MXSLOT, INFILEARRAY, YERRORARRAY)

            GO TO 100

         ELSE IF ( PERIOD_PARSE(COMMAND(1:4),'HELP') ) THEN

C**********-------------------------------------------------------------------
C HELP    * On-line help.
C**********-------------------------------------------------------------------

            CALL PERIOD_HELP(COMMAND)
            GO TO 100

         ELSE IF ( PERIOD_PARSE(COMMAND,'PEAKS') ) THEN

C**********-------------------------------------------------------------------
C PEAKS   * Calculate periods from peaks (and troughs) in periodogram.
C**********-------------------------------------------------------------------

            WRITE (*, *) ' '

 810        CONTINUE
            WRITE (*, '(X,A,$)') 'Enter first and last slots ' //
     :                   'containing periodograms (0,0 to quit) : '
            READ (*, *, ERR=810) FIRSTSLOT, LASTSLOT
            IF ( FIRSTSLOT.LE.0 .OR. LASTSLOT.LE.0 ) GO TO 100
            IF ( FIRSTSLOT.GT.MXSLOT .OR. LASTSLOT.GT.MXSLOT ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: Maximum slot number =', MXSLOT
               GO TO 100
            END IF
 820        CONTINUE
            WRITE (*, '(X,A,$)') 'Enter frequency range to' //
     :                     ' analyse (0,0 for whole range) : '
            READ (*, *, ERR=820) FMIN, FMAX
            IF ( LOG ) THEN
 830           CONTINUE
               WRITE (*, '(X,A,$)') 'Write results to the log' //
     :                     ' file ? [Y] : '
               READ (*, '(A)', ERR=830) REPLY
               CALL PERIOD_CASE(REPLY, .TRUE.)
               IF ( REPLY.EQ.'N' ) THEN
                  LOGWRITE = .FALSE.
               ELSE
                  LOGWRITE = .TRUE.
               END IF
            ELSE
               LOGWRITE = .FALSE.
            END IF

C-----------------------------------------------------------------------------
C peaks - Loop through periodograms.
C-----------------------------------------------------------------------------

            DO 890 SLOT = FIRSTSLOT, LASTSLOT
               NDATA = NPTSARRAY(SLOT)
               YSLOT1 = YPTR(SLOT)

            IF ( NDATA.EQ.0 ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: Slot empty =', SLOT
               GO TO 100
            END IF

C           IF ( FMIN.LE.0.D0 ) FMIN = Y(1, 1, SLOT)
C           IF ( FMAX.LE.0.D0 ) FMAX = Y(NDATA, 1, SLOT)

            RVAL1 = PERIOD_GET2D(1, 1, %VAL(CNF_PVAL(YSLOT1)),
     :                           NDATA, MXCOL)
            RVAL2 = PERIOD_GET2D(NDATA, 1, %VAL(CNF_PVAL(YSLOT1)),
     :                           NDATA, MXCOL)
            IF ( FMIN.LE.0.D0 ) FMIN = RVAL1
            IF ( FMAX.LE.0.D0 ) FMAX = RVAL2

            LFAP1 = .FALSE.
            IF ( FMIN.EQ.RVAL1 ) THEN
               IF ( FMAX.EQ.RVAL2 ) LFAP1 = .TRUE.
            END IF
            IF ( FMAX.GT.RVAL2 ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: Invalid frequency range.'
               GO TO 100
            ELSE IF ( FMIN.LT.RVAL1 ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: Invalid frequency range.'
               GO TO 100
            ELSE IF ( FMIN.GE.FMAX ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: Invalid frequency range.'
               GO TO 100
            END IF

            IF ( INFILEARRAY(SLOT)(1:14).EQ.'Reduced Chi**2' ) THEN
               LPEAK = .FALSE.
            ELSE IF
     :          ( INFILEARRAY(SLOT)(1:14).EQ.'Lomb-Scargle S' ) THEN
                  LPEAK = .TRUE.
               ELSE IF
     :          ( INFILEARRAY(SLOT)(1:14).EQ.'Normalised Pow' ) THEN
                  LPEAK = .TRUE.
               ELSE IF
     :          ( INFILEARRAY(SLOT)(1:14).EQ.'CLEANed Power ' ) THEN
                  LPEAK = .TRUE.
               ELSE IF
     :          ( INFILEARRAY(SLOT)(1:14).EQ.'String Length ' ) THEN
                  LPEAK = .FALSE.
               ELSE IF
     :          ( INFILEARRAY(SLOT)(1:14).EQ.'PDM Statistic ' ) THEN
                  LPEAK = .FALSE.
               ELSE
                  CALL PERIOD_WRITEBELL()
                  WRITE (*, *) '** ERROR: Input data is not' //
     :                           ' a periodogram.'
                  GO TO 100
               END IF

C-----------------------------------------------------------------------------
C peaks - Search for highest (or lowest) peak in periodogram between given
C         frequency range.
C-----------------------------------------------------------------------------

               EL = 0
               PEAK = DNMX30
               TROUGH = DPMX30
               DO 840 I = 1, NDATA
                  RVAL1 = PERIOD_GET2D(I, 1, %VAL(CNF_PVAL(YSLOT1)),
     :                                 NDATA, MXCOL)
                  IF ( RVAL1.LT.FMIN ) GO TO 840
                  IF ( RVAL1.GT.FMAX ) GO TO 845
                  RVAL2 = PERIOD_GET2D(I, 2, %VAL(CNF_PVAL(YSLOT1)),
     :                                 NDATA, MXCOL)
                  IF ( LPEAK ) THEN
                     IF ( RVAL2.GT.PEAK ) THEN
                        PEAK = RVAL2
                        EL = I
                     END IF
                  ELSE IF ( RVAL2.LT.TROUGH ) THEN
                     TROUGH = RVAL2
                     EL = I
                  END IF
 840           CONTINUE
 845           CONTINUE

               ELMAX = MAX0(EL-1, 1)
               ELMIN = MIN0(EL+1, NDATA)
               RVAL1 = PERIOD_GET2D(EL, 1, %VAL(CNF_PVAL(YSLOT1)),
     :                              NDATA, MXCOL)
               IF ( RVAL1.EQ.0.0D0 ) THEN
                  CALL PERIOD_WRITEBELL()
                  WRITE (*, *) '** ERROR: Peak is at infinite' //
     :                   ' period or frequency range too small.'
                  GO TO 100
               ELSE
                  PERIOD = 1.0D0/RVAL1
               END IF
               RVAL2 = PERIOD_GET2D(ELMIN, 1, %VAL(CNF_PVAL(YSLOT1)),
     :                              NDATA, MXCOL)
               RVAL3 = PERIOD_GET2D(ELMAX, 1, %VAL(CNF_PVAL(YSLOT1)),
     :                              NDATA, MXCOL)
               PERROR = 0.5D0*DABS((1.0D0/RVAL2)
     :                              -(1.0D0/RVAL3))

C-----------------------------------------------------------------------------
C peaks - Determine significances.
C-----------------------------------------------------------------------------

               IF ( LSIG ) THEN
                  IF ( SIG(1,SLOT).GE.0.0D0 ) THEN
                     SIGNOISE = SIG(2, SLOT)
                     IF ( SIGNOISE.EQ.0.0D0 ) THEN
                        ERRNOISE = -1.0D0
                     ELSE
                        ERRNOISE = DSQRT((SIGNOISE*(1.0D0-SIGNOISE))
     :                                             /SIG(1,SLOT))
                     END IF
                     RVAL3 = PERIOD_GET2D(EL, 3, %VAL(CNF_PVAL(YSLOT1)),
     :                                    NDATA, MXCOL)
                     SIGPERIOD = RVAL3/SIG(1, SLOT)
                     IF ( SIGPERIOD.EQ.0.0D0 ) THEN
                        ERRPERIOD = -1.0D0
                     ELSE
                        ERRPERIOD = DSQRT((SIGPERIOD*
     :                               (1.0D0-SIGPERIOD)) /SIG(1,SLOT))
                     END IF
                  ELSE
                     SIGNOISE = -1.0D0
                     ERRNOISE = -1.0D0
                     SIGPERIOD = -1.0D0
                     ERRPERIOD = -1.0D0
                  END IF
               END IF

C-----------------------------------------------------------------------------
C peaks - Output information to screen.
C-----------------------------------------------------------------------------

               WRITE (*, *) ' '
               WRITE (*, *) '** OK: Results for slot number = ',
     :                                                          SLOT
               WRITE (*, *) '** OK: ', INFILEARRAY(SLOT)
               WRITE (*, *) '** OK: Minimum frequency searched  = ',
     :                                                          FMIN
               WRITE (*, *) '** OK: Maximum frequency searched  = ',
     :                                                          FMAX
               IF
     :         ( INFILEARRAY(SLOT)(1:14).EQ.'String Length ' ) THEN
               WRITE (*, *) '** OK: Minimum string-length       = ',
     :                                                        TROUGH
               ELSE IF
     :         ( INFILEARRAY(SLOT)(1:14).EQ.'PDM Statistic ' ) THEN
               WRITE (*, *) '** OK: Minimum PDM statistic       = ',
     :                                                        TROUGH
               ELSE IF
     :         ( INFILEARRAY(SLOT)(1:14).EQ.'Reduced Chi**2' ) THEN
               WRITE (*, *) '** OK: Minimum reduced chi-squared = ',
     :                                                        TROUGH
               END IF
               IF ( LPEAK )
     :         WRITE (*, *) '** OK: Maximum power               = ',
     :                                                          PEAK
               WRITE (*, *) '** OK: Period                      = ',
     :                                                        PERIOD
               WRITE (*, *) '** OK: Minimum error in period     = ',
     :                                                        PERROR
               IF ( LSIG ) THEN
                  IF ( LFAP1 ) THEN
                     WRITE (*, *)
     :                      '** OK: False alarm probability 1   = ',
     :                                                      SIGNOISE
                     WRITE (*, *)
     :                      '** OK: Error in FAP1               = ',
     :                                                      ERRNOISE
                  END IF
                  WRITE (*, *)
     :                      '** OK: False alarm probability 2   = ',
     :                                                     SIGPERIOD
                  WRITE (*, *)
     :                      '** OK: Error in FAP2               = ',
     :                                                     ERRPERIOD
               END IF

C-----------------------------------------------------------------------------
C peaks - Output information to log file if requested.
C-----------------------------------------------------------------------------

               IF ( LOGWRITE ) THEN
                  WRITE (LOGUNIT, *) ' '
                  WRITE (LOGUNIT, *)
     :                          '** OK: Results for slot number = ',
     :                                                          SLOT
                  WRITE (LOGUNIT, *) '** OK: ', INFILEARRAY(SLOT)
                  WRITE (LOGUNIT, *)
     :                      '** OK: Minimum frequency searched  = ',
     :                                                          FMIN
                  WRITE (LOGUNIT, *)
     :                      '** OK: Maximum frequency searched  = ',
     :                                                          FMAX
                  IF ( INFILEARRAY(SLOT)(1:14).EQ.'String Length ' )
     :                                                        THEN
                     WRITE (LOGUNIT, *)
     :                      '** OK: Minimum string-length       = ',
     :                                                        TROUGH
                  ELSE IF
     :            ( INFILEARRAY(SLOT)(1:14).EQ.'PDM Statistic ' )
     :                                                        THEN
                     WRITE (LOGUNIT, *)
     :                      '** OK: Minimum PDM statistic       = ',
     :                                                        TROUGH
                  ELSE IF
     :            ( INFILEARRAY(SLOT)(1:14).EQ.'Reduced Chi**2' )
     :                                                        THEN
                     WRITE (LOGUNIT, *)
     :                      '** OK: Minimum reduced chi-squared = ',
     :                                                        TROUGH
                  END IF
                  IF ( LPEAK ) WRITE (LOGUNIT, *)
     :                      '** OK: Maximum power               = ',
     :                                                          PEAK
                  WRITE (LOGUNIT, *)
     :                      '** OK: Period                      = ',
     :                                                        PERIOD
                  WRITE (LOGUNIT, *)
     :                      '** OK: Minimum error in period     = ',
     :                                                        PERROR
                  IF ( LSIG ) THEN
                     IF ( LFAP1 ) THEN
                        WRITE (LOGUNIT, *)
     :                      '** OK: False alarm probability 1   = ',
     :                                                      SIGNOISE
                        WRITE (LOGUNIT, *)
     :                      '** OK: Error in FAP1               = ',
     :                                                      ERRNOISE
                     END IF
                     WRITE (LOGUNIT, *)
     :                      '** OK: False alarm probability 2   = ',
     :                                                     SIGPERIOD
                     WRITE (LOGUNIT, *)
     :                      '** OK: Error in FAP2               = ',
     :                                                     ERRPERIOD
                  END IF
                  WRITE (LOGUNIT, '(A)') '.'
               END IF
 890        CONTINUE
            GO TO 100

         ELSE
            IF ( PERIOD_PARSE(COMMAND,'SIG') ) THEN

C**********-------------------------------------------------------------------
C SIG     * Calculate significance of period.
C**********-------------------------------------------------------------------

               WRITE (*, *) ' '
               IF ( .NOT.LSIG ) THEN
 910              CONTINUE
                  WRITE (*, '(X,A,$)')
     :                       'Enter number of permutations in sample : '
                  READ (*, *, ERR=910) NPERMS
                  IF ( NPERMS.LT.100 ) THEN
                     CALL PERIOD_WRITEBELL()
                     WRITE (*, *)
     :                 '** ERROR: Minimum number of permutations = 100.'
                     NPERMS = 0
                     GO TO 100
                  ELSE IF ( NPERMS.GT.1000 ) THEN
                     CALL PERIOD_WRITEBELL()
                     WRITE (*, *)
     :                        '** ERROR: Maximum number of permutations'
     :                        // ' = ', MAXPERMS
                     NPERMS = 0
                     GO TO 100
                  END IF
 920              CONTINUE
                  WRITE (*, '(X,A,$)')
     :                       'Enter seed for random number generator : '
                  READ (*, *, ERR=920) SEED
C                 SEED = -ABS(INT(SEED))    !INT of an integer is daft (KPD)
                  SEED = -IABS(SEED)
                  RANDOM = PERIOD_RAN1(SEED)
                  LSIG = .TRUE.
                  WRITE (*, *) ' '
                  WRITE (*, *)
     :                        '** OK: Significance calculation enabled.'
               ELSE
                  LSIG = .FALSE.
                  NPERMS = 0
                  WRITE (*, *)
     :                       '** OK: Significance calculation disabled.'
               END IF
            ELSE
               IF ( PERIOD_PARSE(COMMAND,'QUIT') ) RETURN
               IF ( PERIOD_PARSE(COMMAND,'EXIT') ) RETURN
            END IF
            GO TO 100
         END IF
      END IF

C**********-------------------------------------------------------------------
C FREQ    * Set or show frequency search limits.
C**********-------------------------------------------------------------------

 1010 CONTINUE
      WRITE (*, *) ' '
      WRITE (*, *) 'FREQUENCY SEARCH LIMITS: '
      WRITE (*, *) ' '

      IF ( MINFREQ.LE.0. ) THEN
         WRITE (*, *) '1. Minimum frequency  = 0.0'
      ELSE
         WRITE (*, *) '1. Minimum frequency  = ', MINFREQ
      END IF

      IF ( MAXFREQ.LE.0. ) THEN
         WRITE (*, *)
     :              '2. Maximum frequency  = NYQUIST CRITICAL FREQUENCY'
      ELSE
         WRITE (*, *) '2. Maximum frequency  = ', MAXFREQ
      END IF

      IF ( FINTERVAL.LE.0.0D0 ) THEN
         WRITE (*, *)
     :         '3. Frequency interval = 1 / (4 * OVERALL TIME INTERVAL)'
      ELSE
         WRITE (*, *) '3. Frequency interval = ', FINTERVAL
      END IF
      WRITE (*, *) ' '

 1020 CONTINUE
      WRITE (*, '(X,A,$)')
     :                  'Enter number you wish to change (0 to quit) : '
      READ (*, *, ERR=1020) NOPTION
      IF ( NOPTION.EQ.1 ) THEN
 1030    CONTINUE
         WRITE (*, '(X,A,$)')
     :                     'Enter minimum frequency  (0 for default) : '
         READ (*, *, ERR=1030) MINFREQ
      ELSE IF ( NOPTION.EQ.2 ) THEN
 1040    CONTINUE
         WRITE (*, '(X,A,$)')
     :                     'Enter maximum frequency  (0 for default) : '
         READ (*, *, ERR=1040) MAXFREQ
      ELSE IF ( NOPTION.EQ.3 ) THEN
 1050    CONTINUE
         WRITE (*, '(X,A,$)')
     :                     'Enter frequency interval (0 for default) : '
         READ (*, *, ERR=1050) FINTERVAL
      ELSE IF ( NOPTION.EQ.0 ) THEN
         GO TO 100
      END IF
      GO TO 1010

      END
