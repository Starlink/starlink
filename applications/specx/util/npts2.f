*  History:
*     17 Dec 1993 (hme):
*        In order to adapt to the new STACKCOMM, use offest 101 (not
*        92) for number of points in quadrant. There is also no need for
*        a locally declared and equivalenced ITSYS, when SCAN_HEADER is
*        declared in STACKCOMM.
C-----------------------------------------------------------------------

      INTEGER FUNCTION NPTS2 (NQ)

C   Function to obtain number of points in NQ'th quadrant of spectrum
C   in stack position Y.

      INCLUDE  'STAKPAR'
      INCLUDE  'SPECX_PARS'
      INCLUDE  'STACKCOMM'

*      INTEGER*4   ITSYS(1)
*      EQUIVALENCE (ITSYS,TSYS)

*      NPTS2 = ITSYS(LSTK+91+NQ)
      NPTS2 = SCAN_HEADER(LSTK+100+NQ)

      RETURN
      END


