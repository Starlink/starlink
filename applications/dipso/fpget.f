      SUBROUTINE FPGET(NS,SUBCHK)
      REAL X(5000),Y(5000)
      INTEGER IBRKS(50)
      COMMON/DAT4/XV,FXV,ARP,WVF,CFLX,IXV,NXV,NPROF
      COMMON/DAT4A/ITXT
      COMMON/DEBUG/NY
      DIMENSION XV(5,1000),FXV(5,1000),ARP(5),IXV(5),NXV(5),ITXT(5)
      DIMENSION WVF(5),CFLX(5)
      CHARACTER*40 ITXT,TITLE
      LOGICAL OK
       LOGICAL SUBCHK
       SUBCHK = .TRUE.
      NPTS=5000
      NBRKS=50
      NMAX=1000
      NPROF=NPROF+1
      IF=0
      CALL GETSTK(NS,NPTS,X,Y,NBRKS,IBRKS,TITLE,WORV,OK)
      IF(.NOT.OK) THEN
        NPROF=NPROF-1
        RETURN
      ENDIF
      IF(WORV.EQ.1.) THEN
        WRITE (*,
     :  '(''   ELFPIN:   spectrum data are not in velocity space'')')
        IF=1
      ELSE
        IF(NBRKS.EQ.1) THEN
          IF(NPTS.LE.NMAX) THEN
            NXV(NPROF)=NPTS
            DO I=1,NPTS
              XV(NPROF,I)=X(I)
              FXV(NPROF,I)=Y(I)
            ENDDO
            CF=ELFPNT(NPROF,0.,IFAIL)
            IF(IFAIL.EQ.0) THEN
              CFLX(NPROF)=CF
              ARP(NPROF)=ELFPAREA(NPROF)
              DO I=1,NPTS
                FXV(NPROF,I)=FXV(NPROF,I)/CF
              ENDDO
              ITXT(NPROF)=TITLE
              WVF(NPROF)=WORV
            ELSE
              WRITE (*,
     :      '(''   ELFPIN:   unable to find zero on velocity axis'')')
              IF=1
            ENDIF
          ELSE
            WRITE (*,
     :      '(''   ELFPIN:   stack entry is longer than local'',
     :        '' dimensions (of'', I4, '')'')') NMAX
            IF=1
          ENDIF
        ELSE
          WRITE (*,'(''   ELFPIN:  stack entry has breaks'')')
          IF=1
        ENDIF
      ENDIF
      IF(IF.EQ.0) THEN
        NP=NPROF+5
        WRITE (*,'(''   ELFPIN:  stack entry stored as profile'',I3)')
     :  NP
      ELSE
        NPROF=NPROF-1
        WRITE (*,'(''          Stack entry not stored'')')
        SUBCHK = .FALSE.
      ENDIF
      IF(NY.GT.0) PRINT *,' FPGET: NPROF',NPROF
      RETURN
      END
