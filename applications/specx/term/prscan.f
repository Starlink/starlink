*  History:
*     19 Nov 1993 (hme):
*        Disuse the OUTERR_HANDLER error handler.
*        Remove TABs.
*     06 Jan 1994 (rp):
*        Put IOSTAT=IERR in all relevant WRITEs to compensate for loss
*        of error handler.
*     20 July 2000 (ajc):
*        Missing commas in FORMAT
*     07 Mar 2002 (rpt):
*        Change test USB/LSB to look at sign iffreq
C-----------------------------------------------------------------------

      SUBROUTINE PRSCAN (ICHAN, ILEN)

C Routine to print details of scan on channel ICHAN

C    ICHAN : Channel no. for output
C    ILEN  : 0 for one line header
C          : 1 for full details
C          : 3 one-line header, don't close any output file

      IMPLICIT  NONE

*     formal parameters:

      INTEGER   ICHAN
      INTEGER   ILEN

*     common blocks:

      INCLUDE 'FLAGCOMM'
      INCLUDE 'STACKCOMM'

      INTEGER  NLINE
      COMMON /PR_SCAN/ NLINE

*     local variables:

      CHARACTER IUT*4
      CHARACTER RASTRING*12
      CHARACTER DECSTRING*12
      CHARACTER VFRAME*4
      CHARACTER VDEF*3
      CHARACTER SB*3
      INTEGER   IERR
      INTEGER   NQ
      REAL      XNTT
      REAL      XCEN
      REAL      XQCEN
      REAL*8    DFCEN
      REAL*8    DFINC
      REAL*8    DFREST

*     functions

      INTEGER   NTOT

*  Ok, go...

      WRITE  (ICHAN,70)
   70 FORMAT (/1X, 72('-'),/)

      IUT = ' '
      IF (IUTFLG.EQ.1)   IUT = '(UT)'

      WRITE  (ICHAN,10) LSCAN, ITITLE, IDATE, ITIME, IUT
   10 FORMAT (1X,'Scan :',I4,'  Title : ',A26/
     &       1X,' Recorded on ',A9,' at ',A8,A4)
      WRITE  (ICHAN,70)
      NLINE = 8

C  Short header info only

      IF (ILEN.EQ.1 .OR. ILEN.EQ.2)   THEN

        CALL DEG_TO_STRING  (RA/15., RASTRING)
        CALL DEG_TO_STRING  (DEC,    DECSTRING)

        WRITE  (ICHAN, 15, IOSTAT=IERR) RASTRING, DECSTRING,
     &                                  DRA,      DDEC
   15   FORMAT (1X,'Map centre: R.A.',A12,'  Dec. ',A12,/,
     &          '     Offset (R.A.,Dec.):  (',F7.1,2X,F7.1,') arcsec.')
        NLINE = NLINE + 2
      END IF

C  Remaining details if ILEN=1

      IF (ILEN.EQ.1) THEN
        XNTT = FLOAT(INTT)/1000.
        SB = 'USB'
C        IF (JFCEN(1)/1E+06 .LT. LOFREQ(1)) SB = 'LSB'
        IF (IFFREQ(1) .LT. 0) SB = 'LSB'
        WRITE  (ICHAN, 20, IOSTAT=IERR) XNTT, AZ, EL, VLSR, SB
   20   FORMAT (1X,'Integration Period : ',F8.2,'sec'/
     &          1X,'Azimuth ', F8.2, '  Elevation ',F8.2,' Degrees'/
     &          1X,'Vrad : ',F7.1,'Km/s  ',A3,/)
        NLINE = NLINE + 4

        CALL VELDECODE (LSRFLG,VFRAME,VDEF)
        WRITE (ICHAN, 21, IOSTAT=IERR) VDEF, VFRAME
   21   FORMAT (' Data observed using ',A3,' velocity law;',
     &          ' Freq''s corrected to ',A4,' ref. frame.')
        NLINE = NLINE + 1

        WRITE (ICHAN, 22, IOSTAT=IERR) IQCEN
   22   FORMAT (' Master sub-band = ',I2)
        NLINE = NLINE + 1

        WRITE  (ICHAN, 30)
   30   FORMAT (' Quad.  #pts.  Cent.Ch  Rest Freq(GHz) Obs.Freq(GHz)',
     &          ' Inc.freq(MHz) Tsys(K)')
        NLINE = NLINE + 1

        DO NQ = 1, NQUAD
          IF (MASK(NQ).NE.0)   THEN
            XQCEN  = FLOAT(NPTS(NQ)+1)/2.
            XCEN   = XQCEN+NTOT(NQ-1)

            IF (FCEN(NQ).NE.0.0) THEN
              DFREST = FCEN(NQ)
            ELSE
              DFREST = DFLOAT (JFREST(NQ))/1.E6
            END IF

            DFCEN  = DFLOAT (JFCEN(NQ))/1.E6
            DFINC  = DFLOAT (JFINC(NQ))/1.E6

            WRITE (ICHAN, 40, IOSTAT=IERR) NQ, NPTS(NQ), XCEN,
     &                                     DFREST, DFCEN, DFINC,
     &                                     TSYS(NQ)
   40       FORMAT (4X,I1,3X,I5,3X,F6.1,3X,F10.4,4X,F10.4,4X,F9.4,
     &              4X,G12.5)
            NLINE = NLINE + 1
          END IF
        END DO

        WRITE (ICHAN, 70)
        NLINE = NLINE + 1

      END IF

      RETURN
      END

C-----------------------------------------------------------------------
