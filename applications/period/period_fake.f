
      SUBROUTINE PERIOD_FAKE(YPTR, MXCOL, MXSLOT, NPTSARRAY,
     :                       INFILEARRAY, YERRORARRAY, DETRENDARRAY)

C=============================================================================
C Generates fake data with which to test the period finding programs. There
C are two options: (1) A strictly periodic dataset, and (2) a chaotic dataset.
C
C Written by Vikram Singh Dhillon @LPO 25-January-1992.
C
C GJP March 1997
C
C Removed redundant NVEC variable.
C
C Converted to Double Precision (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C=============================================================================

      IMPLICIT NONE

      INCLUDE 'CNF_PAR'

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

      INTEGER MXCOL, MXSLOT
      INTEGER YPTR(MXSLOT), NPTSARRAY(MXSLOT)

C-----------------------------------------------------------------------------
C PERIOD_FAKE declarations.
C-----------------------------------------------------------------------------

      INTEGER MAXSIN
      PARAMETER (MAXSIN=20)
      DOUBLE PRECISION PERIOD(MAXSIN), AMPLITUDE(MAXSIN)
      DOUBLE PRECISION ZEROPT(MAXSIN), GAMMA(MAXSIN)
      DOUBLE PRECISION STARTPT, ENDPT, INTERVAL
      DOUBLE PRECISION COEFF, INITVAL
      INTEGER NUMSIN, NUMPTS
      INTEGER J, SLOT, FIRSTSLOT, LASTSLOT
      INTEGER YSLOT1
      LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
      CHARACTER*72 INFILEARRAY(MXSLOT), OPTION*1

C-----------------------------------------------------------------------------
C Select slots to process.
C-----------------------------------------------------------------------------

      WRITE (*, *) ' '
 100  CONTINUE
      WRITE (*, '(X,A,$)') 'Enter first and last slots for output ' //
     :                     '(0,0 to quit) : '
      READ (*, *, ERR=100) FIRSTSLOT, LASTSLOT
      IF ( FIRSTSLOT.EQ.0 .OR. LASTSLOT.EQ.0 ) GO TO 800
      IF ( FIRSTSLOT.GT.MXSLOT .OR. LASTSLOT.GT.MXSLOT ) THEN
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** ERROR: Maximum slot number = ', MXSLOT
         GO TO 800
      END IF

C-----------------------------------------------------------------------------
C Select periodic or chaotic data.
C-----------------------------------------------------------------------------

      WRITE (*, *) ' '
 200  CONTINUE
      WRITE (*, '(X,A,$)') '[P]eriodic or [C]haotic data ? [P] : '
      READ (*, '(A)', ERR=200) OPTION
      CALL PERIOD_CASE(OPTION, .TRUE.)

C-----------------------------------------------------------------------------
C Calculate periodic fake data.
C-----------------------------------------------------------------------------

      IF ( OPTION.EQ.'P' .OR. OPTION.EQ.' ' ) THEN
 250     CONTINUE
         WRITE (*, '(X,A,$)') 'Enter number of sine curves : '
         READ (*, *, ERR=250) NUMSIN
         IF ( NUMSIN.LE.0 ) GO TO 250
         IF ( NUMSIN.GT.MAXSIN ) THEN
            CALL PERIOD_WRITEBELL()
            WRITE (*, *) '** ERROR: Maximum number of sine curves = ',
     :                   MAXSIN
            GO TO 800
         END IF
 300     CONTINUE
         WRITE (*, '(X,A,$)') 'Enter number of data points : '
         READ (*, *, ERR=300) NUMPTS
         IF ( NUMPTS.LE.0 ) GO TO 300
 350     CONTINUE
         WRITE (*, '(X,A,$)') 'Enter range of data points : '
         READ (*, *, ERR=350) STARTPT, ENDPT

         DO 400 J = 1, NUMSIN
            WRITE (*, *) ' '
            WRITE (*, *) 'SINE CURVE NUMBER = ', J
            WRITE (*, *) ' '
 360        CONTINUE
            WRITE (*, '(X,A,$)')
     :                        'Enter period, semi-amplitude, zero point'
     :                        // ' and gamma : '
            READ (*, *, ERR=360) PERIOD(J), AMPLITUDE(J), ZEROPT(J),
     :                           GAMMA(J)
            IF ( PERIOD(J).LE.0.0D0 ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: Invalid value for the period.'
               WRITE (*, *) ' '
               GO TO 360
            END IF
 400     CONTINUE

         WRITE (*, *) ' '

         DO 450 SLOT = FIRSTSLOT, LASTSLOT

            IF ( NPTSARRAY(SLOT).NE.0 )
     :         CALL PERIOD_DEALL(YPTR(SLOT))
            CALL PERIOD_ALLOC('_DOUBLE', NUMPTS*3, YPTR(SLOT))

            YSLOT1 = YPTR(SLOT)

            CALL PERIOD_INIT2D(0.0D0, %VAL(CNF_PVAL(YSLOT1)),
     :                         NUMPTS, 3)

            INTERVAL = (ENDPT-STARTPT)/(DFLOAT(NUMPTS)-1.0D0)

            CALL PERIOD_FAKEPERIOD(%VAL(CNF_PVAL(YSLOT1)),
     :                             NUMPTS, 3, PERIOD,
     :                             AMPLITUDE, ZEROPT, GAMMA, MAXSIN,
     :                             NUMSIN, STARTPT, INTERVAL)

            INFILEARRAY(SLOT) = 'Data File = Fake Periodic Data'
            YERRORARRAY(SLOT) = .FALSE.
            DETRENDARRAY(SLOT) = .FALSE.
            WRITE (*, *) '** OK: Filled slot = ', SLOT
            NPTSARRAY(SLOT) = NUMPTS
 450     CONTINUE

C-----------------------------------------------------------------------------
C Calculate chaotic fake data.
C-----------------------------------------------------------------------------

      ELSE IF ( OPTION.EQ.'C' ) THEN
         WRITE (*, *) ' '
         WRITE (*, *)
     :            '** OK: Logistic Equation Xn+1 = LAMBDA * Xn * (1-Xn)'
         WRITE (*, *) ' '
 500     CONTINUE
         WRITE (*, '(X,A,$)') 'Enter LAMBDA : '
         READ (*, *, ERR=500) COEFF
         IF ( COEFF.LT.0.0D0 .OR. COEFF.GT.4.0D0 ) THEN
            CALL PERIOD_WRITEBELL()
            WRITE (*, *) '** ERROR: LAMBDA must lie between 0 and 4.'
            GO TO 800
         END IF
 550     CONTINUE
         WRITE (*, '(X,A,$)') 'Enter initial value : '
         READ (*, *, ERR=550) INITVAL
 600     CONTINUE
         WRITE (*, '(X,A,$)') 'Enter number of data points : '
         READ (*, *, ERR=600) NUMPTS
 650     CONTINUE
         WRITE (*, '(X,A,$)') 'Enter range of data points : '
         READ (*, *, ERR=650) STARTPT, ENDPT
         INTERVAL = (ENDPT-STARTPT)/(DFLOAT(NUMPTS)-1.0D0)
         WRITE (*, *) ' '

         DO 700 SLOT = FIRSTSLOT, LASTSLOT

            IF ( NPTSARRAY(SLOT).NE.0 )
     :         CALL PERIOD_DEALL(YPTR(SLOT))
            CALL PERIOD_ALLOC('_DOUBLE', NUMPTS*3, YPTR(SLOT))

            YSLOT1 = YPTR(SLOT)

            CALL PERIOD_INIT2D(0.0D0, %VAL(CNF_PVAL(YSLOT1)),
     :                         NUMPTS, 3)

            CALL PERIOD_FAKECHAOS(%VAL(CNF_PVAL(YSLOT1)),
     :                            NUMPTS, 3, COEFF,
     :                            INITVAL, STARTPT, ENDPT)

            INFILEARRAY(SLOT) = 'Data File = Fake Chaotic Data'
            YERRORARRAY(SLOT) = .FALSE.
            DETRENDARRAY(SLOT) = .FALSE.
            WRITE (*, *) '** OK: Filled slot = ', SLOT
            NPTSARRAY(SLOT) = NUMPTS
 700     CONTINUE

      ELSE

         GO TO 200

      END IF

 800  CONTINUE
      RETURN
      END
