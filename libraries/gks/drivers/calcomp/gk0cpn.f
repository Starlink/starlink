
*----------------------------------------------------------------
      SUBROUTINE GK0CPN(N)
*----------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     CC81 Output number to buffer
*
*  MAINTENANCE LOG
*  ---------------
*     00/99/83  MGC  Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP N   - number
*
      INTEGER N
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkio.par'
*
*  LOCALS
*  ------
*
      INTEGER ICPN(4),ITENS(3)
      INTEGER ILPN,IVAL,II
      DATA ITENS/1000,100,10/
*
*  ALGORITHM
*  ---------
*     PUT NUMBER : <N0><N1><N2><N3>
*
* --------------------------------------------------------------

      ILPN = 0
      IVAL = MOD(N,10000)
      DO 10 II=1,3
        IF(N.GE.ITENS(II)) THEN
          ILPN = ILPN + 1
          ICPN(ILPN) = 48 + IVAL/ITENS(II)
          IVAL = MOD(IVAL,ITENS(II))
        ENDIF
 10   CONTINUE
      ILPN = ILPN + 1
      ICPN(ILPN) = 48 + IVAL
      CALL GK0CPA(KIOPB,ILPN,ICPN)
      RETURN
      END
