C-----------------------------------------------------------------------

      SUBROUTINE INITHD

C   Routine to initialize header variables for a spectrum, so that it is
C   possible to create a spectrum from scratch using EDIT-SPECTRUM.

      IMPLICIT  NONE

      INCLUDE  'STACKCOMM'

      INTEGER   J
      INTEGER   STATUS

      LSCAN  = 1
      IMODE  = 1
      NQUAD  = 1
      IUTFLG = 0
      ICALZD = 0
      LSRFLG = 0
      IQCEN  = 1

      RA     = 0.D0
      DEC    = 0.D0

      INTT   = 0
      AZ     = 0.0
      EL     = 0.0

      DO J = 1, 8
        ITREC(J)  = 0
        ITSKY(J)  = 0
        ITTEL(J)  = 0
        TSYS(J)   = 0.0
        NPTS(J)   = 0
        JFINC(J)  = 0
        JFCEN(J)  = 0
        LOFREQ(J) = 0.D0
        IFFREQ(J) = 0.D0
      END DO

      VSL    = 0.
      VES    = 0.
      VTE    = 0.
      VLSR   = 0.
      DRA    = 0.
      DDEC   = 0.

      ITITLE = 'Edit/created'

      IDATE  = ' '
      CALL UGETDATE (IDATE(1:9), STATUS)
      CALL UGETTIME (ITIME,      STATUS)

      RETURN
      END


