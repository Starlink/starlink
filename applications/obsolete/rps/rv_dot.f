*+RV_DOT         Returns dot product of two vectors
      FUNCTION RV_DOT(VA,VB)

*  Calling Arguments
      REAL VA(3), VB(3)		! In	Vectors
      REAL RV_DOT		! Out	Dot product
*-

      REAL ABSV, ABSL1, ABSL2
      PARAMETER (ABSL1=1.0, ABSL2=1.01)

      RV_DOT = VA(1)*VB(1) + VA(2)*VB(2) + VA(3)*VB(3)
      ABSV = ABS(RV_DOT)

      IF(ABSV.GT.ABSL1.AND.ABSV.LT.ABSL2) RV_DOT = SIGN(1.0,RV_DOT)

      END
