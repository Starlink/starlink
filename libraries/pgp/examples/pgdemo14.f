      PROGRAM PGDE14
C-----------------------------------------------------------------------
C Demonstration program for PGPLOT: text input with PGRSTR.
C
C This program illustrates how an interactive program can be written
C using PGPLOT. The program displays a number of active fields. Select
C one of these fields using the cursor (e.g., click the mouse) to 
C activate it; then use the keyboard keys to edit the string displayed
C in the field. Two of the fields have immediate action: 'DRAW' draws
C a simple picture using the parameters specified in the input fields;
C 'EXIT' terminates the program.
C
C A version of the subroutine used here, PGRSTR, may be included in a
C future release of the PGPLOT library.
C-----------------------------------------------------------------------
      INTEGER NBOX
      PARAMETER (NBOX=5)
      REAL BOX(4,NBOX), X, Y, XX, YY, A, D, XV(100), YV(100)
      INTEGER IVAL(NBOX)
      INTEGER PGOPEN, LSTR, I, JUNK, PGCURS, J, NV, BC, FC, CTOI
      INTEGER II, JJ
      CHARACTER CH
      CHARACTER*30 LABEL(NBOX), VALUE(NBOX), RESULT(NBOX)
C
      DATA BOX /0.44, 0.8, 0.79, 0.83,
     :          0.44, 0.8, 0.69, 0.73,
     :          0.44, 0.8, 0.59, 0.63,
     :          0.44, 0.7, 0.29, 0.33,
     :          0.44, 0.7, 0.19, 0.23/

      DATA LABEL /'Number of vertices:',
     :            'Background Color:',
     :            'Foreground Color:',
     :            ' ',
     :            ' '/
      DATA VALUE /'13',
     :            '0',
     :            '1',
     :            'DRAW', 
     :            'EXIT'/
C-----------------------------------------------------------------------
      WRITE (*,*) 'This program requires an interactive device.'
      WRITE (*,*) 'It presents a menu with editable fields which can be'
      WRITE (*,*) 'used to set parameters controlling a graph displayed'
      WRITE (*,*) 'beside the menu. To edit a field, first select it'
      WRITE (*,*) 'with the cursor (e.g., click mouse button) then use'
      WRITE (*,*) 'keyboard keys and DEL or ^U. TAB or CR terminates'
      WRITE (*,*) 'editing Click on DRAW to display the graph or EXIT'
      WRITE (*,*) 'to terminate the program.'
      WRITE (*,*)
C
C Open device for graphics.
C
      IF (PGOPEN('?') .LE. 0) STOP
      CALL PGPAP(10.0,0.5)
      IVAL(1) = 13
      IVAL(2) = 0
      IVAL(3) = 1
C
C Clear the screen. Draw a frame at the physical extremities of the
C plot, using full-screen viewport and standard window.
C
      CALL PGPAGE
      CALL PGSVP(0.0,1.0,0.0,1.0)
      CALL PGSWIN(0.0,2.0,0.0,1.0)
      CALL PGSCR(0, 0.4, 0.4, 0.4)
C
C Display fields
C
 5    WRITE(VALUE(1), '(I6)') IVAL(1)
      WRITE(VALUE(2), '(I6)') IVAL(2)
      WRITE(VALUE(3), '(I6)') IVAL(3)
      CALL PGSAVE
      CALL PGBBUF
      CALL PGERAS
      CALL PGSCI(1)
      CALL PGSLW(1)
      CALL PGSFS(1)
      CALL PGSCH(1.2)
      DO 10 I=1,NBOX
         RESULT(I) = VALUE(I)
         X = BOX(1,I) - 0.04
         Y = BOX(3,I) + 0.01
         CALL PGSCI(1)
         CALL PGPTXT(X, Y, 0.0, 1.0, LABEL(I))
         CALL PGRECT(BOX(1,I), BOX(2,I), BOX(3,I), BOX(4,I))
         X = BOX(1,I) + 0.01
         CALL PGSCI(2)
         CALL PGPTXT(X, Y, 0.0, 0.0, VALUE(I))
 10   CONTINUE
C
C Draw picture
C
      NV = MIN(100,IVAL(1))
      BC = IVAL(2)
      FC = IVAL(3)
      CALL PGSCI(BC)
      CALL PGSFS(1)
      CALL PGRECT(1.05,1.95,0.05,0.95)
      CALL PGSCI(FC)
      CALL PGSFS(2)
      CALL PGRECT(1.05,1.95,0.05,0.95)
      IF (NV.GT.3) THEN
         D = 360.0/NV
         A = -D
         DO 120 II=1,NV
            A = A+D
            XV(II) = 1.5 + 0.4*COS(A/57.29577951)
            YV(II) = 0.5 + 0.4*SIN(A/57.29577951)
 120     CONTINUE
C     
         DO 140 II=1,NV-1
            DO 130 JJ=II+1,NV
               CALL PGMOVE(XV(II),YV(II))
               CALL PGDRAW(XV(JJ),YV(JJ))
 130        CONTINUE
 140     CONTINUE
      END IF
      CALL PGEBUF
      CALL PGUNSA
C
C Cursor loop: user selects a box
C
      CALL PGSLW(2)
      CALL PGSFS(2)
      XX = 0.5
      YY = 0.5
      DO 60 J=1,1000
         JUNK = PGCURS(XX, YY, CH)
         IF (ICHAR(CH).EQ.0) GOTO 50
C
C Find which box and highlight it
C
         DO 30 I=1,NBOX
            IF (BOX(1,I).LE.XX .AND. BOX(2,I).GE.XX .AND.
     :          BOX(3,I).LE.YY .AND. BOX(4,I).GE.YY) THEN
               CALL PGSCI(2)
               CALL PGSLW(2)
               CALL PGSCH(1.2)
               CALL PGRECT(BOX(1,I), BOX(2,I), BOX(3,I), BOX(4,I))
               CALL PGSLW(1)
               IF (I.EQ.5) THEN
C                 -- EXIT box
                  GOTO 50
               ELSE IF (I.EQ.4) THEN
C                 -- DRAW box
                  GOTO 5
               ELSE
C
C Read value
C   
                  IF (RESULT(I).EQ.' ') THEN
                     LSTR = 0
                  ELSE
                     DO 11 II=LEN(RESULT(I)),1,-1
                        LSTR = II
                        IF (RESULT(I)(II:II).NE.' ') GOTO 12
 11                  CONTINUE
                     LSTR = 0
 12                  CONTINUE
                  END IF
                  X = BOX(1,I) + 0.01
                  Y = BOX(3,I) + 0.01
                  CALL PGRSTR(X, Y, 0.0, 0.0, RESULT(I), LSTR, 1)
                  II = 1
                  IVAL(I) = CTOI(RESULT(I)(1:LSTR), II)
               END IF
               CALL PGSLW(2)
               CALL PGSCI(1)
               CALL PGRECT(BOX(1,I), BOX(2,I), BOX(3,I), BOX(4,I))
               CALL PGSLW(1)
            END IF 
 30      CONTINUE
 60   CONTINUE
C
C Close the device and exit.
C
 50   CONTINUE
      CALL PGCLOS
      END


      SUBROUTINE PGRSTR(X, Y, ANGLE, FJUST, TEXT, LSTR, BCI)
      REAL X, Y, ANGLE, FJUST
      CHARACTER*(*) TEXT
      INTEGER LSTR, BCI
C-----------------------------------------------------------------------
      CHARACTER CH
      INTEGER JUNK, PGBAND, CI
      REAL XCUR, YCUR, XBOX(4), YBOX(4)
C
      CALL PGQCI(CI)
C
 10   CONTINUE
C     -- Draw current string
          IF (LSTR.GT.0) THEN
             CALL PGPTXT(X, Y, ANGLE, FJUST, TEXT(1:LSTR))
             CALL PGQTXT(X, Y, ANGLE, FJUST, TEXT(1:LSTR), XBOX, YBOX)
             XCUR = XBOX(4)
             YCUR = YBOX(4)
          ELSE
             XCUR = X
             YCUR = Y
          END IF
C         -- Read a character
          JUNK = PGBAND(0, 1, XCUR, YCUR, XCUR, YCUR, CH)
C         -- Erase old string
          CALL PGSCI(BCI)
          IF (LSTR.GT.0) 
     :         CALL PGPTXT(X, Y, ANGLE, FJUST, TEXT(1:LSTR))
          CALL PGSCI(CI)
C         -- Avoid problem with PGPLOT escape character
          IF (CH.EQ.CHAR(92)) CH = '*'
C         -- Backspace (ctrl H) or delete removes last character
          IF (ICHAR(CH).EQ.8 .OR. ICHAR(CH).EQ.127) THEN
             IF (LSTR.GT.0) TEXT(LSTR:LSTR) = ' '
             IF (LSTR.GT.0) LSTR = LSTR-1
C         -- Ctrl U removes entire string
          ELSE IF (ICHAR(CH).EQ.21) THEN
             TEXT(1:LSTR) = ' '
             LSTR = 0
C         -- Any other non-printing character terminates input
          ELSE IF (ICHAR(CH).LT.32) THEN
             IF (LSTR.GT.0)
     :            CALL PGPTXT(X, Y, ANGLE, FJUST, TEXT(1:LSTR))
             GOTO 20
C         -- Otherwise, add character to string if there is room
          ELSE IF (LSTR.LT.LEN(TEXT)) THEN
             LSTR = LSTR+1
             TEXT(LSTR:LSTR) = CH
          END IF
      GOTO 10
C
 20   RETURN
      END

      INTEGER FUNCTION CTOI (S, I)
      CHARACTER*(*) S
      INTEGER I
C
C Attempt to read an integer from a character string, and return
C the result. No attempt is made to avoid integer overflow. A valid 
C integer is any sequence of decimal digits.
C
C Returns:
C  CTOI            : the value of the integer; if the first character
C                    read is not a decimal digit, the value returned
C                    is zero.
C Arguments:
C  S      (input)  : character string to be parsed.
C  I      (in/out) : on input, I is the index of the first character
C                    in S to be examined; on output, either it points
C                    to the next character after a valid integer, or
C                    it is equal to LEN(S)+1.
C-----------------------------------------------------------------------
      INTEGER K
      CHARACTER*1 DIGITS(0:9)
      DATA  DIGITS/'0','1','2','3','4','5','6','7','8','9'/
C
      CTOI = 0
   10 IF (I.GT.LEN(S)) RETURN
      IF (S(I:I).EQ.' ') THEN
         I = I+1
         GOTO 10
      END IF
      DO 20 K=0,9
          IF (S(I:I).EQ.DIGITS(K)) GOTO 30
   20 CONTINUE
      RETURN
   30 CTOI = CTOI*10 + K
      I = I+1
      GOTO 10
      END
