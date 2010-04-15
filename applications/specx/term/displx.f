*  History:
*     20 Sep 2000 (ajc):
*        Missing commas in FORMAT
C-------------------------------------------------------------------------

      SUBROUTINE DISPLX (XSCALE, IFAIL)

C   Routine to list relevant data about XSCALE for each unmasked quadrant

      IMPLICIT         NONE

*     Formal parameters:

      REAL              XSCALE(*)
      INTEGER           IFAIL

*     Include files:

      INCLUDE          'STACKCOMM'
      INCLUDE          'FLAGCOMM'

*     Local variables:

      INTEGER           I
      INTEGER           IERR
      INTEGER           NQ
      REAL              STARTX, ENDX
      DOUBLE PRECISION  DFCEN,  DFINC

*     Functions

      INTEGER   NTOT
      LOGICAL   DOQUAD

*  Ok, go...

      IFAIL  = 0

      CALL SETXNEW (XSCALE, IERR)

      IF (FCORRECT) THEN
        WRITE (ILOUT, *)
        WRITE (ILOUT, *) 'Frequency axis polynomial correction applied'
     &                   //' for display purposes'
        WRITE (ILOUT, *) '-- polynomial coefficients [in MHz**(n-1)]:'
        WRITE (ILOUT, '(6(2X,G11.4))', IOSTAT=IERR) (FRQCOEFF(I),I=1,6)
      END IF

      WRITE (ILOUT,*)
      WRITE (ILOUT,1000, IOSTAT=IERR)
     &                    XAXIS_UNITS, XAXIS_UNITS, XAXIS_UNITS

      DO NQ = 1, NQUAD
        STARTX = XSCALE(NTOT(NQ-1)+1)
        ENDX   = XSCALE(NTOT(NQ))
        DFCEN  = DFLOAT(JFCEN(NQ))/1.D6
        DFINC  = DFLOAT(JFINC(NQ))/1.D6

        IF (DOQUAD(NQ)) THEN
          WRITE (ILOUT,1001, IOSTAT=IERR) NQ, DFCEN, DFINC,
     &                                    XFAC(NQ), STARTX, ENDX
        END IF

      END DO

 1000 FORMAT ('  Quad.   Centre freq.   Increment',
     &        '                  Start        End'      /
     &        '             (GHz)         (MHz)  ',
     &        ' (',A6,')     (',A6,')       (',A6,')'    /1X,67('-')/)

 1001 FORMAT (  T5,I1,    T11,F11.6, T27,F9.4,
     &          T37,F9.4, T48,F10.4, T61,F10.4)

      RETURN
      END


