

      SUBROUTINE GK1TSL(LNTYPE,LSIMUL)
* --------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     T4107 Set TEK LineType from GKSLineType
*
*  MAINTENANCE LOG
*  ---------------
*     00/99/83  MGC  Original version stabilized
*     19/03/84  MGC  Always select line type
*
*  ARGUMENTS
*  ---------
*     INP LNTYPE   - GKSLineType [1..5]
*     OUT LSIMUL   - True if simulation is required
*
      INTEGER LNTYPE
      LOGICAL LSIMUL
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkio.par'
*
*  LOCALS
*  ------
*
      INTEGER    ILSL,KZREM
      PARAMETER (ILSL=5)
      INTEGER ITYPES(ILSL),ICSL(4)
      DATA ITYPES/0,7,1,2,6/
      DATA ICSL/27,77,86,0/
*
*  ALGORITHM
*  ---------
*
*  COMMENTS
*  --------
*     GKS   4107   Description
*     1     0      solid
*     2     7      dash,dash
*     3     1      dot,dot
*     4     2      long dash, dot
*     5     6      long dash, short dash
*
* --------------------------------------------------------------------

      IF(LNTYPE.LT.1 .OR. LNTYPE.GT.ILSL) THEN
      LSIMUL=.TRUE.
      ICSL(4)=48
      ELSE
      LSIMUL=.FALSE.
      ICSL(4) = 48 + ITYPES(LNTYPE)
      ENDIF
      CALL GKIOBO(KIOPB,4,ICSL,KZREM)
      RETURN
      END
