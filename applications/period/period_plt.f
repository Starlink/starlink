
      SUBROUTINE PERIOD_PLT(YPTR, NPTSARRAY, MXCOL,
     :                      MXSLOT, INFILEARRAY, YERRORARRAY)

C=============================================================================
C Routine to call PLT from within PERIOD.
C
C Written by Vikram Singh Dhillon @Sussex 3-June-1991.
C
C GJP March 1997
C
C Modified to replace Xanadu PLT routine with the PGPLOT equivalent.
C Removed PLT common block.
C
C Converted to Double Precision (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C=============================================================================

      IMPLICIT NONE

      INCLUDE "mnmxvl.h"

      INCLUDE 'CNF_PAR'

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

      INTEGER NPTS, MXCOL

      INTEGER XRPTR, YRPTR, ER1PTR, ER2PTR

C-----------------------------------------------------------------------------
C PERIOD_PLT declarations.
C-----------------------------------------------------------------------------

      INTEGER MXSLOT, FIRSTSLOT, LASTSLOT, SLOT
      INTEGER YPTR(MXSLOT), NPTSARRAY(MXSLOT), YSLOT1
      LOGICAL YERRORARRAY(MXSLOT)
      CHARACTER*72 INFILEARRAY(MXSLOT)
      DOUBLE PRECISION DMIX, DMIY, DMXX, DMXY, DRNG
      REAL MIX,MIY,MXX,MXY

C-----------------------------------------------------------------------------
C Select slots to process.
C-----------------------------------------------------------------------------

      WRITE (*, *) ' '
 100  CONTINUE
      WRITE (*, '(X,A,$)') 'Enter first and last slots for input ' //
     :                     '(0,0 to quit) : '
      READ (*, *, ERR=100) FIRSTSLOT, LASTSLOT
      IF ( FIRSTSLOT.NE.0 .AND. LASTSLOT.NE.0 ) THEN
         IF ( FIRSTSLOT.GT.MXSLOT .OR. LASTSLOT.GT.MXSLOT ) THEN
            CALL PERIOD_WRITEBELL()
            WRITE (*, *) '** ERROR: Maximum slot number = ', MXSLOT
            GO TO 200
         END IF

         DO 150 SLOT = FIRSTSLOT, LASTSLOT
            NPTS = NPTSARRAY(SLOT)

            IF ( NPTS.EQ.0 ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: Slot empty =', SLOT
               GO TO 200
            END IF

            YSLOT1 = YPTR(SLOT)

            CALL PERIOD_ALLOC('_REAL', NPTS, XRPTR)
            CALL PERIOD_ALLOC('_REAL', NPTS, YRPTR)
            CALL PERIOD_ALLOC('_REAL', NPTS, ER1PTR)
            CALL PERIOD_ALLOC('_REAL', NPTS, ER2PTR)

*         Set up arrays.
            DMXX=DNMX38
            DMIX=DPMX38
            DMXY=DNMX38
            DMIY=DPMX38

            CALL PERIOD_PLTXYERR(%VAL(CNF_PVAL(XRPTR)),
     :                           %VAL(CNF_PVAL(YRPTR)),
     :                           %VAL(CNF_PVAL(ER1PTR)),
     :                           %VAL(CNF_PVAL(ER2PTR)),
     :                           %VAL(CNF_PVAL(YSLOT1)),
     :                           NPTS, MXCOL,
     :                           YERRORARRAY(SLOT), DMIX, DMIY,
     :                           DMXX, DMXY)

*         Check limits and set up for prtetty display.

*         Handle zero width.
            IF(DMXX.EQ.DMIX) THEN
               IF (DMXX.NE.0.0D0) THEN
                  DMXX=DMXX*1.1D0
                  DMIX=DMIX*0.9D0
               ELSE
                  DMXX=1.0D0
                  DMIX=-1.0D0
               END IF
            END IF
            IF(DMXY.EQ.DMIY) THEN
               IF(DMXY.NE.0.0D0) THEN
                  DMXY=DMXY*1.1D0
                  DMIY=DMIY*0.9D0
               ELSE
                  DMXY=1.0D0
                  DMIY=-1.0D0
               END IF
            END IF

*         Handle near zero axis.
            IF(DABS(DMIX).LT.DPMN20) THEN
               DRNG=DMXX-DMXX
               DMIX=DMIX-DRNG*0.025D0
            END IF
            IF(DABS(DMIY).LT.DPMN20) THEN
               DRNG=DMXY-DMXY
               DMIY=DMIY-DRNG*0.025D0
            END IF

            MXX = SNGL(DMXX)
            MIX = SNGL(DMIX)
            MXY = SNGL(DMXY)
            MIY = SNGL(DMIY)

C-----------------------------------------------------------------------------
C Call PGPLOT.
C-----------------------------------------------------------------------------

            WRITE (*, *) ' '
            WRITE (*, *) '** OK: PGplotting slot number =', SLOT
            WRITE (*, *) ' '

*         Get the device name and open it.
            CALL PGBEGIN(0,'?',1,1)

*         Set sensible range values.
            CALL PGRNGE(MIX,MXX,MIX,MXX)
            CALL PGRNGE(MIY,MXY,MIY,MXY)

*         Set image scale limits.
            CALL PGENV(MIX,MXX,MIY,MXY,0,0)

*         Label plot.
            CALL PGLABEL('Time',' ',INFILEARRAY(SLOT))

*         Plot Y error bars.
            CALL PGSCI(2)
            IF (YERRORARRAY(SLOT)) THEN
               CALL PGERRY(NPTS,%VAL(CNF_PVAL(XRPTR)),
     :                     %VAL(CNF_PVAL(ER2PTR)),
     :                     %VAL(CNF_PVAL(ER1PTR)),.001)
               CALL PGSCI(1)
            END IF

*         Plot data.
            CALL PGPOINT(NPTS,%VAL(CNF_PVAL(XRPTR)),
     :                   %VAL(CNF_PVAL(YRPTR)),2)

*         Show user name.
            CALL PGIDEN

*         Turn off pgplot.
            CALL PGEND

         CALL PERIOD_DEALL(ER2PTR)
         CALL PERIOD_DEALL(ER1PTR)
         CALL PERIOD_DEALL(YRPTR)
         CALL PERIOD_DEALL(XRPTR)

 150     CONTINUE

      END IF

 200  CONTINUE
      RETURN
      END
