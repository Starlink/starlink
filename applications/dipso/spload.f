       SUBROUTINE SPLOAD(CMD,PARAMS,TITLE,IHHEAD,WORKSZ,WORK,NPOINT,
     :                   WAVE,FLUX,NBREAK,BREAK,IH1,MAXOUT,SUBCHK,
     :                   STATUS)

       IMPLICIT NONE
       INCLUDE 'SAE_PAR'
       INTEGER STATUS

       CHARACTER*(*) CMD, PARAMS, TITLE, IHHEAD

       INTEGER WORKSZ, NPOINT, NBREAK
       INTEGER BREAK(NBREAK)
       INTEGER IH1, MAXOUT

       REAL WORK(WORKSZ), WAVE(NPOINT), FLUX(NPOINT)

       LOGICAL SUBCHK

       INTEGER IAN, IBRK, IH, I
       REAL DWAV, DDWAV, WTST

       IF( STATUS .NE. SAI__OK ) RETURN

       DO 100 I = 1, 80
          IF (PARAMS(I:I).NE.' ') GOTO 300
  100  CONTINUE

  200  CONTINUE

       CALL RDSTR( CMD, 'File name', ' ', PARAMS, STATUS )
       IF( STATUS .NE. SAI__OK ) THEN
          SUBCHK = .FALSE.
          GO TO 500
       END IF

  300  CONTINUE
       CALL SSTRIP(PARAMS)
       IF (PARAMS(1:2).EQ.'!!') THEN
          WRITE (*,'(''   '',A,'':  user induced abort'')') CMD
          SUBCHK = .FALSE.
          GOTO 500
       ENDIF
       IF (CMD.EQ.'OSP0WR') THEN
          OPEN (UNIT=61,ACCESS='SEQUENTIAL',STATUS='NEW',
     :    FORM='UNFORMATTED',FILE=PARAMS(1:80),IOSTAT=IAN)
       ELSE
          OPEN (UNIT=61,STATUS='NEW',FILE=PARAMS(1:80),IOSTAT=IAN)
       ENDIF
       IF (IAN.NE.0) THEN
          WRITE (*,
     :    '(''   '',A,'':  error opening file, try again'')') CMD
          GOTO 200
       ENDIF
       IH1 = 0
       IBRK = 1
       DWAV = 0.0001
       MAXOUT = WORKSZ*0.5 - 1
       DO 400 IH = 1, NPOINT
          IH1 = IH1 + 1
          IF (IH1.GT.MAXOUT) THEN
  320        CONTINUE
             IH1 = IH1 - 1
             WRITE (*,'(''   '',A,'':  too many datum points'')') CMD
             WRITE (*,'(''   Only the first'',I6,'' output'')') MAXOUT
             GOTO 500
          ELSE
             WORK(IH1) = WAVE(IH)
             WORK(IH1+MAXOUT) = FLUX(IH)
             IF (IH.EQ.BREAK(IBRK) .AND. IH.NE.NPOINT) THEN
                WTST = WAVE(IH+1) - WAVE(IH)
                WTST = WTST/3.
                IF (WTST.GE.DWAV) THEN
                   DDWAV = DWAV
                ELSE
                   DDWAV = WTST
                ENDIF
                IH1 = IH1 + 1
                IF (IH1.GT.MAXOUT) GOTO 320
                WORK(IH1) = WORK(IH1-1) + DDWAV
                WORK(IH1+MAXOUT) = 0.0
                IH1 = IH1 + 1
                IF (IH1.GT.MAXOUT) GOTO 320
                WORK(IH1) = WAVE(IH+1) - DDWAV
                WORK(IH1+MAXOUT) = 0.0
                IBRK = IBRK + 1
             ENDIF
          ENDIF
  400  CONTINUE

  500  CONTINUE

       END
