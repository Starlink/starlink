*  History:
*     17 Dec 1993 (hme):
*        In order to adapt to the new STACKCOMM, use offest 101 (not
*        92) for number of points in quadrant. There is also no need for
*        a locally declared and equivalenced ITSYS, when SCAN_HEADER is
*        declared in STACKCOMM.
C-----------------------------------------------------------------------

      INTEGER FUNCTION NTOT2 (NQ)

C   Utility routine to obtain number of data points in Y-register to
C   end of quadrant NQ

      INCLUDE 'STAKPAR'
      INCLUDE 'STACKCOMM'

*      INTEGER  ITSYS(1)
*      EQUIVALENCE (TSYS, ITSYS)

      NTOT2 = 0
      IF(NQ.NE.0) THEN
        DO N = 1,NQ
*          NTOT2 = NTOT2 + ITSYS(LSTK+91+N)
          NTOT2 = NTOT2 + SCAN_HEADER(LSTK+100+N)
        END DO
      END IF

      RETURN
      END


