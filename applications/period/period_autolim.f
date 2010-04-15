
      SUBROUTINE PERIOD_AUTOLIM(DATA, NDATA, MAXPTS, MINFREQ, MAXFREQ,
     :                          FINTERVAL, FMIN, FMAX, FINT, IFAIL)

C=============================================================================
C Routine to set the frequency search limits. If any one of the frequency
C limits are less than or equal to zero, PERIOD_AUTOLIM returns default values.
C These are calculated as follows: MINFREQ = 0 (ie. infinite period);
C MAXFREQ = NYQUIST critical frequency (ie. 1 / (2 * SMALLEST TIME INTERVAL));
C FINTERVAL = 1 / (POINTS PER BEAM * OVERALL TIME INTERVAL). PERIOD_AUTOLIM
C also checks whether the limits are valid, and if not, returns IFAIL = 1.
C
C Written by Vikram Singh Dhillon @Sussex 16-June-1992.
C
C Divide by zero checks added June 1995 - GJP Starlink
C
C Converted to Double Precision (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C=============================================================================

      IMPLICIT NONE

      INCLUDE "mnmxvl.h"

      INTEGER NDATA, MAXPTS, IFAIL, I
      INTEGER INTMAX
      DOUBLE PRECISION DATA(NDATA)
      DOUBLE PRECISION MINFREQ, MAXFREQ, FINTERVAL
      DOUBLE PRECISION FMIN, FMAX, FINT
      DOUBLE PRECISION OTI, STI, PPB
      DATA PPB/4.0D0/

C-----------------------------------------------------------------------------
C Calculate the Smallest Time Interval STI.
C-----------------------------------------------------------------------------

      IFAIL = 0

      STI = DPMX30
      DO 100 I = 2, NDATA
         IF ( (DATA(I)-DATA(I-1)).LT.STI ) STI = DATA(I)-DATA(I-1)
 100  CONTINUE

C-----------------------------------------------------------------------------
C Calculate the Overall Time Interval OTI.
C-----------------------------------------------------------------------------

      OTI = DATA(NDATA)-DATA(1)

C-----------------------------------------------------------------------------
C If any frequency limit is less than or equal to zero, set to default value.
C-----------------------------------------------------------------------------

      IF ( MINFREQ.LE.0.0D0 ) THEN
         FMIN = 0.0D0
      ELSE
         FMIN = MINFREQ
      END IF

      IF ( MAXFREQ.LE.0.0D0 ) THEN
*  Check STI to avoid divide by zero error. GJP.
         IF ( DABS(STI).GT.DPMN30 ) THEN
            FMAX = 0.5D0/STI
         ELSE
            CALL PERIOD_WRITEBELL()
            WRITE (*, *) '** ERROR: Smallest time interval in data'
            WRITE (*, *) '** ERROR: is zero.'
            IFAIL = 1
            GO TO 200
         END IF
      ELSE
         FMAX = MAXFREQ
      END IF

      IF ( FINTERVAL.LE.0.0D0 ) THEN
*  Check OTI to avoid divide by zero error. GJP.
         IF ( DABS(OTI).GT.DPMN30 ) THEN
            FINT = 1.0D0/(PPB*OTI)
         ELSE
            CALL PERIOD_WRITEBELL()
            WRITE (*, *) '** ERROR: Overall time interval in data'
            WRITE (*, *) '** ERROR: is zero.'
            IFAIL = 1
            GO TO 200
         END IF
      ELSE
         FINT = FINTERVAL
      END IF

C-----------------------------------------------------------------------------
C Output final frequency limits.
C-----------------------------------------------------------------------------

      WRITE (*, *) '** OK: Minimum frequency  = ', FMIN
      WRITE (*, *) '** OK: Maximum frequency  = ', FMAX
      WRITE (*, *) '** OK: Frequency interval = ', FINT

C-----------------------------------------------------------------------------
C Check frequency limits.
C-----------------------------------------------------------------------------

      IF ( FMAX.LE.FMIN ) THEN
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** ERROR: Maximum frequency not greater than'
         WRITE (*, *) '** ERROR: minimum frequency in PERIOD_AUTOLIM.'
         IFAIL = 1
      ELSE
         INTMAX = IDINT((FMAX-FMIN)/FINT) + 2
         IF ( MAXPTS.GT.0 ) THEN
            IF ( INTMAX.GE.MAXPTS ) THEN
               CALL PERIOD_WRITEBELL()
               WRITE (*, *) '** ERROR: Frequency interval too small'
               WRITE (*, *) '** ERROR: in PERIOD_AUTOLIM.'
               IFAIL = 1
            END IF
         ELSE
            MAXPTS = INTMAX
         END IF
      END IF

 200  CONTINUE

      RETURN
      END
