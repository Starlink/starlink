C-----------------------------------------------------------------------

      SUBROUTINE SETCHN(NQ,IFAIL)

C  Routine to set one or more (sequential) channels in one or more
C  quadrants to given value.

C  History:
C     6-JUN-2000
C       Declare external function INQUAD LOGICAL

      DIMENSION IC(2)

      EXTERNAL INQUAD
      LOGICAL INQUAD

      LOGICAL   OUTRNGE,DOQUAD

      INCLUDE        'STACKCOMM'
      INCLUDE        'FLAGCOMM'
      EQUIVALENCE    (IC(1),IC1)

      IFAIL=0

      CALL GEN_GETI4A('Range of points?',IC,2,
     &                '2(1X,I4,:,'','')',IC,JDEF)
      IF(IC2.EQ.0)   IC2=IC1
      CALL GEN_GETR4('Set to? (value)',VAL,'F7.1',VAL,JDEF)

      CALL QLIM(NQ,NQ1,NQ2)
      OUTRNGE=.TRUE.

      DO NQ=NQ1,NQ2
       IF(DOQUAD(NQ)) THEN
        JC1=MAX0(IC1,NTOT(NQ-1)+1)
        JC2=MIN0(IC2,NTOT(NQ))
        IF(INQUAD(NQ,JC1).AND.INQUAD(NQ,JC2)) THEN
         OUTRNGE=.FALSE.
         DO J=JC1,JC2
          DATA(J)=VAL
         END DO
        END IF
       END IF
      END DO

      IF(OUTRNGE) IFAIL=19
      RETURN

      END


