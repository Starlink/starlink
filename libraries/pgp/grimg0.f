      SUBROUTINE GRIMG0(A,IDIM,JDIM,I1,I2,J1,J2,FOREGR,BACKGR,TR,
     :                  MINCI, MAXCI, MODE)
*+
*
*     - - - - - - - -
*       G R I M G 0     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Plots a cell array
*
*   Given
*      A        r()   Data array
*      IDIM     i     First dimension of A
*      JDIM     i     Second dimension of A
*      I1       i     Array subset
*      I2       i       "     "
*      J1       i       "     "
*      J2       i       "     "
*      FOREGR   r     Data value to map to foreground colour
*      BACKGR   r     Data value to map to background colour
*      MINCI    i     Lowest colour index to use
*      MAXCI    i     Highest colour index to use
*      TR       r()   transformation from array indices to world coordinates
*      MODE     i     Transfer function
*
*   Read from COMMON
*      GRCIDE   i     Current device
*
*   D.L.Terrett  Starlink  Feb 1995
*+
      IMPLICIT NONE

      INTEGER IDIM, JDIM, I1, I2, J1, J2, MODE
      INTEGER MINCI, MAXCI
      REAL A(IDIM,JDIM), BACKGR, FOREGR
      REAL TR(6)

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRGRAY - No PGPLOT device open',
     :   GRNODO)
      ELSE

*        Plot the cell array.
            CALL GRGRA2(A,IDIM,JDIM,I1,I2,J1,J2,FOREGR,BACKGR,TR,
     :                     MINCI, MAXCI, MODE)
      ENDIF

 9999 CONTINUE
      END
