*  History:
*     22 Nov 1993 (hme):
*        Disuse OUTERR_HANDLER error handler.
C-----------------------------------------------------------------------

      SUBROUTINE LISTP(BUF)

      REAL*4      BUF(*)
      LOGICAL     DOQUAD

      INCLUDE 'STACKCOMM'
      INCLUDE 'FLAGCOMM'

      CALL PRSCAN(ILOUT,3)

      DO NQ = 1, NQUAD
        IF (DOQUAD(NQ))   THEN
          CALL COPYBF (NQ, BUF)
          WRITE (ILOUT,'(//'' Quadrant no : ''I1)') NQ
          NST = NTOT(NQ-1)
          WRITE (ILOUT, 730, IOSTAT=IERR)
     &                   (J+NST,(BUF(K),K=J,MIN0(NPTS(NQ),J+4)),
     &                    J=1,5*((NPTS(NQ)+4)/5),5)
  730     FORMAT ((1X,I4,1X,5(G12.5,2X)))
        END IF
      END DO

      RETURN
      END


