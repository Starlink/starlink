      FUNCTION NEXTK(KARD,K)
      DIMENSION KARD(80)
    5 CALL ELFKJ(KARD(K),IT,IN,IK)
      IF(IK.EQ.37) THEN
      K=K+1
      IF(K.GT.80) THEN
      NEXTK=0
      RETURN
      ENDIF
      GO TO 5
      ENDIF
      NEXTK=K
      RETURN
      END
