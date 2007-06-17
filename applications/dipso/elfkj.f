      SUBROUTINE ELFKJ(K,ITYP,INUM,IK)

C   SUBROUTINE TO IDENTIFY A CHARACTER AND DETERMINE WHETHER IT
C   IS A COMMAND, A VARIABLE, A NUMBER OR AN OPERATOR (ITYP=1 TO 4).

      COMMON/DEBUG/NY

      INCLUDE 'KARS_COM'  ! Declares common block KARS holding KAR

      DO I=1,40
         IF(K.EQ.KAR(I)) THEN
            IK=I
            ITYP=1
            IF(I.GT.10) ITYP=2
            IF(I.GT.20) ITYP=3
            IF(I.GT.30) ITYP=4
            INUM=I-(ITYP-1)*10
            IF(NY.GT.0) WRITE(6,10) K,ITYP,INUM
   10       FORMAT(1X,A1,3X,2I3)
            RETURN
         ENDIF
      ENDDO

      INUM=0
      IF(NY.GT.0) WRITE(6,10) K,ITYP,INUM
      RETURN

      END
