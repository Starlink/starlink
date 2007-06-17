*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
*
*   SUBROUTINE HELP1
*
*   PRINTS OUT SOME 'HELP' INFORMATION
*
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE HELP1
*
       CHARACTER*1 IHFILE
       CHARACTER*80 IHHLP


       CHARACTER*80   prefix
       CHARACTER*80   prefix2
       CHARACTER*80   prefix3
       CHARACTER*80   prefix4
       INTEGER        plen,p2len,p3len,p4len

       COMMON /prefix1/ prefix,prefix2,prefix3,prefix4
       COMMON /prefix2/ plen,p2len,p3len,p4len
*
*
       OPEN (UNIT=77,
     :       FILE=prefix(1:plen)//'dipso.hlp',STATUS='OLD',
     : IOSTAT=IHX)
       IF (IHX.NE.0) THEN
          WRITE (*,'(''   HELP:  unable to open file'')')
          CLOSE (77)
       ELSE
          IHXTRA = 0
          IHDO = 18
   50     CONTINUE
          IF (IHXTRA.NE.0) THEN
             WRITE (*,'('' '',A79)') IHHLP(1:79)
             IHDO = 17
          ENDIF
          DO 100 IH = 1, IHDO
             READ (77,'(A80)',IOSTAT=IHX) IHHLP(1:80)
             IF (IHX.NE.0) GOTO 150
             WRITE (*,'('' '',A79)') IHHLP(1:79)
  100     CONTINUE
          IHXTRA = 1
          WRITE (*,'('' >>>> Hit RETURN to continue, Q to stop:  '',$)')
          READ (5,'(A1)',IOSTAT=IX) IHFILE(1:1)
          IF (IX.NE.0) GOTO 50
          IF (IHFILE(1:1).EQ.'Q' .OR. IHFILE(1:1).EQ.'q') GOTO 200
          GOTO 50
  150     CONTINUE
          IF (IHX.NE.-1) THEN
             WRITE (*,'(''   HELP:  error reading file'')')
          ENDIF
       ENDIF
  200  CONTINUE
       CLOSE (77)
*
       END
