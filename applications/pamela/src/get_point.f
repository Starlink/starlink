      SUBROUTINE GET_POINT(REPLY, X, XLO, XHI, Y, YLO, YHI)
*
* Get a point X,Y between limits XLO,XHI,YLO,YHI
*
* CHARACTER*2 REPLY --- First character = 'C' cursor is used,
*                       = 'T' terminal is used. Second character
*                       = 'X' only X value prompted, ='Y' only
*                       Y value prompted, ='B' for both.
*                       In either 'X' or 'Y' modes.
*
* REAL X,Y the returned point
* REAL XLO, XHI  -- Range of allowed X
* REAL YLO, YHI  -- Range of allowed Y
*
*
      CHARACTER*2 REPLY, CH
      CHARACTER*1 PROMPT, RANGE
      CHARACTER*80 STRING
      REAL X, Y, XLO, XHI, YLO, YHI
*
      PROMPT = REPLY(1:1)
      RANGE  = REPLY(2:2)
      IF(PROMPT.EQ.'C') THEN
        WRITE(*,*) 'Type S to show coordinates,'//
     &  ' C to continue'
        CH = 'S'
        DO WHILE(CH.NE.'C' .AND. CH.NE.'c')
          CALL PGCURSE(X, Y, CH)
          IF(CH.EQ.'S' .OR. CH.EQ.'s') THEN
            WRITE(*,'(A,F8.2,A,F8.2)') 'Coordinates are X: ',
     &                                  X,', Y: ',Y
          END IF
        END DO
        X = MAX(XLO, MIN(XHI, X))
        Y = MAX(YLO, MIN(YHI, Y))
      ELSE IF(PROMPT.EQ.'T') THEN
        IFAIL = 1
        DO WHILE(IFAIL.NE.0)
          IF(RANGE.EQ.'X') THEN
            WRITE(*,'(A,F8.2,A,F8.2,A,$)')
     &      'Enter X value (between ',XLO,' and ',XHI,'):'
          ELSE IF(RANGE.EQ.'Y') THEN
            WRITE(*,'(A,F8.2,A,F8.2,A,$)')
     &      'Enter Y value (between ',YLO,' and ',YHI,'):'
          ELSE IF(RANGE.EQ.'B') THEN
            WRITE(*,'(A,F8.2,A,F8.2,A,F8.2,A,F8.2,A,$)')
     &      'Enter point,X,Y (X: ',XLO,',',XHI,'), (Y: ',
     &      YLO,',',YHI,'): '
          END IF
          READ(*,'(A)') STRING
          IF(RANGE.EQ.'X') THEN
            READ(STRING, *, IOSTAT=IFAIL) X
            IF(X.LT.XLO .OR. X.GT.XHI) IFAIL = 1
          ELSE IF(RANGE.EQ.'Y') THEN
            READ(STRING, *, IOSTAT=IFAIL) Y
            IF(Y.LT.YLO .OR. Y.GT.YHI) IFAIL = 1
          ELSE IF(RANGE.EQ.'B') THEN
            READ(STRING, *, IOSTAT=IFAIL) X, Y
            IF(X.LT.XLO .OR. X.GT.XHI .OR. Y.LT.YLO
     &      .OR. Y.GT.YHI) IFAIL = 1
          END IF
        END DO
      END IF
      RETURN
      END
