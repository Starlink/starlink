C# IL>=a, OL>=1
      SUBROUTINE GACTM  (TMIN,X0,Y0,DX,DY,PHI,SX,SY,ISW,TMOUT)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  ACCUMULATE TRANSFORMATION MATRIX
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     The transformation given by the arguments is combined with the
*     input matrix and the result is put in the output matrix. The
*     WC/NDC switch specifies whether shift vector and fix-point are
*     given in WC or NDC units.
*
*  MAINTENANCE LOG
*  ---------------
*     01/12/83  AS    Original version stabilized
*     30/01/84  JRG   Turned matrix round; corrected argument list for
*                     _KTWN
*     29/02/84  JGW   Corrected handling of shift VECTOR (not point)
*     10/04/84  JGW   Removed scaling on shift component
*     21/01/87  ARG   IS conversion. Language binding has transposed
*                     matrices. Error check added.
*     21/09/87  PJWR  Changed use of GKTWN to transform fixed point and
*                     shift vector to use of GKTWNP and GKTWNV. (Fixes
*                     bug S281).
*
*  ARGUMENTS
*  ---------
*     INP  TMIN    old segment transformation matrix
*     INP  X0, Y0  fixed point
*     INP  DX, DY  shift vector
*     INP  PHI     rotation angle (in radians)
*     INP  SX, SY  scale factors
*     INP  ISW     coordinate switch
*     OUT  TMOUT   new segment transformation matrix
*
      INTEGER ISW
      REAL TMIN(2,3), X0, Y0, DX, DY, PHI, SX, SY, TMOUT(2,3)
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
*     XM0     NDC version of fixed point (X)
*     YM0     NDC version of fixed point (Y)
*     DMX     NDC version of shift vector (X)
*     DMY     NDC version of shift vector (Y)
*     SI      Sine of rotation angle
*     CO      Cosine of rotation angle
*     TLOCAL  Array to hold transformation evaluated from arguments
*     X0A     Copy of WC fixed point (X) for transformation to NDC
*     Y0A     Copy of WC fixed point (Y) for transformation to NDC
*     TMINT   Temporary copy of input transformation for GKMTML
*     TMOUTT  Transformation from GKMTML,  transposed into TMOUT later
*     I       Loop variable
*     J       Loop variable
*
      REAL XM0(1), YM0(1), DMX, DMY, SI, CO, TLOCAL(3,2), X0A(1),
     :     Y0A(1), TMINT(3,2), TMOUTT(3,2)
      INTEGER I,J
*
*  ERRORS
*  ------
*     2000   Enumerated type out of range
*
*  COMMENTS
*  --------
*     GKMTML multiplies (3,2) matrices,  so the matrix holding the old
*     transformation and the resulting matrix must be transposed.
*
*-----------------------------------------------------------------------

      CALL GKPRLG (EACTM,GGKOP,GSGOP)
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
*       If there were no errors,  construct a transformation matrix from
*       the arguments and concatenate this with the old matrix to form
*       the new one.
        IF (KERROR.EQ.0) THEN
          SI = SIN(PHI)
          CO = COS(PHI)
          TLOCAL(1,1) = SX*CO
          TLOCAL(1,2) = SX*SI
          TLOCAL(2,1) = -SY*SI
          TLOCAL(2,2) = SY*CO
          TLOCAL(3,1) = -XM0(1)*CO*SX + YM0(1)*SI*SY + XM0(1) + DMX
          TLOCAL(3,2) = -XM0(1)*SI*SX - YM0(1)*CO*SY + YM0(1) + DMY
*         Transpose input matrix
          DO 100 I = 1, 3
            DO 100 J = 1, 2
  100         TMINT(I,J) = TMIN(J,I)
*         Concatenate matrices
          CALL GKMTML(TMINT, TLOCAL, TMOUTT)
*         Transpose output matrix
          DO 200 I = 1, 3
            DO 200 J = 1, 2
  200         TMOUT(J,I) = TMOUTT(I,J)
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF

      END
