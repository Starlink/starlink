*+RV_MOD         Returns size of vector
      FUNCTION RV_MOD(V)
 
*  Calling Arguments
      REAL V(3)			! In	Vector
      REAL RV_MOD			! Out	Magnitude of vector, V
*-
 
      RV_MOD = SQRT(V(1)*V(1) + V(2)*V(2) + V(3)*V(3) )
 
      END
