      SUBROUTINE GET_LIM(REPLY, X1, X2, XLO, XHI, Y1, Y2, 
     &YLO, YHI, IFAIL)
*
* Gets X,Y-limits of a region X1 to X2
* and Y1 to Y2.
* The values of X1, X2 must lie betweem XLO to XHI
* similarly for Y1, Y2
*
* CHARACTER*2 REPLY --- First character = 'C' cursor is used, 
*                       = 'T' terminal is used. Second character
*                       ='X' only X range promted, ='Y' only
*                       Y range prompted, ='B' for both.
*                       In either 'X' or 'Y' modes, the numbers
*                       are ordered. In 'B' mode they are ordered
*                       in X.
*
* REAL X1, X2    -- X limits of points
* REAL XLO, XHI  -- Range of allowed X 
* REAL Y1, Y2    -- Y limits of points
* REAL YLO, YHI  -- Range of allowed Y
*
*
      IMPLICIT NONE
      INTEGER IFAIL
      CHARACTER*2 REPLY, CH
      CHARACTER*1 PROMPT, RANGE
      CHARACTER*80 STRING
      REAL X1, X2, Y1, Y2, X, Y
      REAL XLO, XHI, YLO, YHI
      LOGICAL SAMECI
*
      IFAIL = 1
      PROMPT = REPLY(1:1)
      RANGE  = REPLY(2:2)
      IF(SAMECI(PROMPT,'C')) THEN
        WRITE(*,*) 'S to show coordinates, C to continue, Q to quit'
        IF(SAMECI(RANGE,'B')) THEN
          WRITE(*,*) 'First point'
        ELSE
          WRITE(*,*) 'First ',RANGE,' limit'
        END IF
*        
        CH = 'S'
        DO WHILE(.NOT.SAMECI(CH,'C') .AND. .NOT.SAMECI(CH,'Q'))
          CALL PGCURSE(X, Y, CH)
          IF(SAMECI(CH,'S')) THEN
            WRITE(*,'(A,F8.2,A,F8.2)') 'Coordinates are X: ',
     &                                  X,', Y: ',Y
          END IF
        END DO
        IF(SAMECI(CH,'Q')) GOTO 999
        X1 = MAX(XLO, MIN(XHI, X))
        Y1 = MAX(YLO, MIN(YHI, Y))
        IF(SAMECI(RANGE,'B')) THEN
          WRITE(*,*) 'Second point'
        ELSE
          WRITE(*,*) 'Second ',RANGE,' limit'
        END IF
        CH = 'S'
        DO WHILE(.NOT.SAMECI(CH,'C') .AND. .NOT.SAMECI(CH,'Q'))
          CALL PGCURSE(X, Y, CH)
          IF(SAMECI(CH,'S')) THEN
            WRITE(*,'(A,F8.2,A,F8.2)') 'Coordinates are X: ',
     &                                  X,', Y: ',Y
          END IF
        END DO
        IF(SAMECI(CH,'Q')) GOTO 999
        X2 = MAX(XLO, MIN(XHI, X))
        Y2 = MAX(YLO, MIN(YHI, Y))
      ELSE IF(SAMECI(PROMPT,'T')) THEN
        IFAIL = 1
        DO WHILE(IFAIL.NE.0)
          IF(SAMECI(RANGE,'X')) THEN
            WRITE(*,'(A,F8.2,A,F8.2,A,$)') 
     &      'Enter X limits (between ',XLO,' and ',XHI,') or Q to quit:'
          ELSE IF(SAMECI(RANGE,'Y')) THEN
            WRITE(*,'(A,F8.2,A,F8.2,A,$)') 
     &      'Enter Y limits (between ',YLO,' and ',YHI,') or Q to quit:'
          ELSE IF(SAMECI(RANGE,'B')) THEN
            WRITE(*,'(A,F8.2,A,F8.2,A,F8.2,A,F8.2,A,$)')
     &      'Enter two points,X,Y (X: ',XLO,',',XHI,'), (Y: ',
     &      YLO,',',YHI,') or Q to quit: '
          END IF
          READ(*,'(A)') STRING
          IF(SAMECI(STRING,'Q')) GOTO 999
          IF(SAMECI(RANGE,'X')) THEN
            READ(STRING, *, IOSTAT=IFAIL) X1, X2
            IF(X1.LT.XLO .OR. X1.GT.XHI .OR. X2.LT.XLO
     &      .OR.X2.GT.XHI) IFAIL = 1
          ELSE IF(SAMECI(RANGE,'Y')) THEN
            READ(STRING, *, IOSTAT=IFAIL) Y1, Y2
            IF(Y1.LT.YLO .OR. Y1.GT.YHI .OR. Y2.LT.YLO
     &      .OR.Y2.GT.XHI) IFAIL = 1
          ELSE IF(SAMECI(RANGE,'B')) THEN
            READ(STRING, *, IOSTAT=IFAIL) X1, Y1, X2, Y2
            IF(X1.LT.XLO .OR. X1.GT.XHI .OR. X2.LT.XLO
     &      .OR.X2.GT.XHI .OR. Y1.LT.YLO .OR. Y1.GT.YHI 
     &      .OR. Y2.LT.YLO .OR.Y2.GT.YHI) IFAIL = 1
          END IF 
        END DO
      END IF
      IF(.NOT.SAMECI(RANGE,'Y')) THEN
        IF(X2.LT.X1) THEN
          X = X1
          X1 = X2
          X2 = X
          Y = Y1
          Y1 = Y2
          Y2 = Y1
        END IF
      ELSE
        IF(Y2.LT.Y1) THEN
          Y = Y1
          Y1 = Y2
          Y2 = Y1
        END IF
      END IF
      IFAIL = 0
999   RETURN
      END




