C# IL>=a, OL>=1
      SUBROUTINE GEVTM (X0,Y0,DX,DY,PHI,SX,SY,ISW,TMOUT)
*
* (C) COPYRIGHT ICL & SERC  1984
*
*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  EVALUATE TRANSFORMATION MATRIX
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     The transformation given by the arguments is evaluated and the
*     result is put in the output matrix. The wc/ndc switch specifies
*     whether shift vector and fixpoint are given in WC or NDC units.
*
*  MAINTENANCE LOG
*  ---------------
*     30/11/83  AS    Original version stabilized
*     30/01/84  JRG   Turned matrix round; corrected argument list for
*                     _KTWN
*     04/03/84  JGW   Corrected handling of shift VECTOR (not a point!)
*     04/03/84  JGW   (3,1)(3,2):fix point must be restored with scale
*     10/04/84  JGW   (3,1)(3,2): !! got above wrong !! scale removed
*     21/01/87  ARG   IS conversion. Language binding has transposed
*                     matrix. Error check added.
*     21/09/87  PJWR  Changed from using GKTWN to using GKTWNP for
*                     transforming WC fixed point and GKTWNV for shift
*                     vector to NDC. (Fixes bug S281).
*
*  ARGUMENTS
*  ---------
*     INP  X0, Y0  fixed point
*     INP  DX, DY  shift vector
*     INP  PHI     rotation angle (in radians)
*     INP  SX, SY  scale factors
*     INP  ISW     coordinate switch
*     OUT  TMOUT   segment transformation matrix
*
      REAL X0, Y0, DX, DY, PHI, SX, SY
      INTEGER ISW
      REAL TMOUT (2,3)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     XM0     NDC equivalent of fixed point (X)
*     YM0     NDC equivalent of fixed point (Y)
*     DMX     NDC equivalent of shift vector (X)
*     DMY     NDC equivalent of shift vector (Y)
*     SI      Sine of rotation angle
*     CO      Cosine of rotation angle
*     X0A     Array copy of fixed point (X) for translation to NDC
*     Y0A     Array copy of fixed point (Y) for translation to NDC
*
*
      REAL XM0(1), YM0(1), DMX, DMY, SI, CO, X0A(1), Y0A(1)
*
*  ERRORS
*  ------
*     2000  Enumerated type out of range
*
*-----------------------------------------------------------------------

      CALL GKPRLG (EEVTM,GGKOP,GSGOP)
      IF (KERROR.EQ.0) THEN
        IF (ISW.EQ.GWC) THEN
*         Copy fixed point into temporary variables.
          X0A(1) = X0
          Y0A(1) = Y0
*         Map fixed point and shift vector to NDC.
          CALL GKTWNP(KCNTN,1,X0A,Y0A,XM0,YM0)
          CALL GKTWNV(KCNTN,DX,DY,DMX,DMY)
        ELSE IF (ISW.EQ.GNDC) THEN
*         Copy arguments into local variables.
          XM0(1) = X0
          YM0(1) = Y0
          DMX = DX
          DMY = DY
        ELSE
*         Invalid value given for the coordinate switch.
          KERROR = 2000
          CALL GKERR(KERROR)
        ENDIF
*       Now calculate the sine and cosine of the rotation angle and
*       fill in the transformation matrix unless the switch was invalid.
        IF (KERROR.EQ.0) THEN
          SI = SIN (PHI)
          CO = COS (PHI)
          TMOUT (1,1) = SX * CO
          TMOUT (2,1) = SX * SI
          TMOUT (1,2) = -SY * SI
          TMOUT (2,2) = SY * CO
          TMOUT (1,3) = -XM0(1)*CO*SX + YM0(1)*SI*SY + XM0(1) + DMX
          TMOUT (2,3) = -XM0(1)*SI*SX - YM0(1)*CO*SY + YM0(1) + DMY
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF

      END
