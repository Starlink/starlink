       SUBROUTINE VELSORT
     : (A,N,NTERMS,EXISTS,SRTARY,MAXCL,MAXARR)
       INTEGER N,NTERMS,MAXCL,MAXARR,SHIFT
       REAL A(1)
       LOGICAL EXISTS(*)
       REAL SRTARY(1)
       LOGICAL OK
!
       NTERMS=3*N
       J=MAXARR
       DO 5 I=1,N
          IF (.NOT.EXISTS(I)) THEN
30000     IF (.NOT.EXISTS(J)) THEN
            J=J-1
            GOTO 30000
          END IF
          SHIFT=(I-J)*3
          DO 6 K=3*J-2,3*J
            A(K+SHIFT)=A(K)
            A(K)=0.0
    6     CONTINUE
          J=J-1
          END IF
    5  CONTINUE
       DO I=1,N
          EXISTS(I)=.TRUE.
       END DO
       DO I=N+1,MAXCL
          EXISTS(I)=.FALSE.
       END DO
!
!   Sort clouds into order of increasing radial velocity
!
       IF (N.GT.1) THEN
          K=1
          DO 7 I=1,N
            DO 8 J=0,2
               SRTARY(I+J*N)=A(K)
               K=K+1
    8       CONTINUE
    7     CONTINUE
          IFAIL=0
          CALL ARYSRT
     :    (SRTARY,N,3,2,'ISINP',OK)
          K=1
          DO 9 I=1,N
            DO 11 J=0,2
               A(K)=SRTARY(I+J*N)
               K=K+1
   11       CONTINUE
    9     CONTINUE
       END IF

       END
