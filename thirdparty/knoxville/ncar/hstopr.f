      SUBROUTINE HSTOPR (IOPT,ARRAY,ISIZE)
C
C SET THE HSTOGRAM OPTIONS
C
C INPUT
C     IOPT-CHARACTER STRING OF OPTION VALUE
C     ARRAY-   REAL ARRAY OF DIMENSION ISIZE
C     ISIZE-   SIZE OF ARRAY
C
C  SET COMMON DATA EQUAL TO INPUT DATA
C
C
      COMMON /HSTGC1/ HORZNT, PERCNT, MIDVAL, SHADE, MEDIAN, PERIM,
     -       HFRAME, LISTOP, WINDOW, COLORS, HSTFOR, TITLE, LABEL,
     -       FREQNC, HWIND(4), COLSHA, COLREC, COLAXI, COLMED, COLTEX,
     -       COLTIT, COLPER
      LOGICAL HORZNT, PERCNT, MIDVAL, SHADE, MEDIAN, PERIM, HFRAME,
     -        LISTOP, WINDOW, COLORS, HSTFOR, TITLE, LABEL, FREQNC
      COMMON /HSTGC2/ STRFOR, STRTIT, STRLAB, STRFRE
      CHARACTER*55  STRFOR, STRTIT, STRLAB, STRFRE
      DIMENSION ARRAY(ISIZE)
      CHARACTER*7  IOPT
      CHARACTER*2  TAG, OPT
C
C
      SAVE
      NERR = 0
C
C  DETERMINE THE OPTION DESIRED
C
      TAG = IOPT(1:2)
      IF (IOPT(3:3) .EQ. '=') THEN
          OPT = IOPT(4:5)
      ELSE
          OPT = IOPT(5:6)
      ENDIF
C
C  COLORS FLAG
C
      IF (TAG .EQ. 'CO') THEN
C
C  SWITCH = ON
C
        IF (OPT .EQ. 'ON') THEN
              COLORS = .TRUE.
              COLSHA = ARRAY(1)
              COLREC = ARRAY(2)
              COLAXI = ARRAY(3)
              COLMED = ARRAY(4)
              COLTEX = ARRAY(5)
              COLTIT = ARRAY(6)
              COLPER = ARRAY(7)
          RETURN
C
C  SWITCH = OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
              COLORS = .FALSE.
              COLSHA = 1.
              COLREC = 1.
              COLAXI = 1.
              COLMED = 1.
              COLTEX = 1.
              COLTIT = 1.
              COLPER = 1.
            RETURN
          ELSE
                GOTO 120
        ENDIF
C
C  WINDOWING FLAG
C
      ELSEIF (TAG .EQ. 'WI') THEN
C
C  SWITCH ON
C
          IF (OPT .EQ. 'ON') THEN
              WINDOW = .TRUE.
              HWIND(1) = ARRAY(1)
              HWIND(2) = ARRAY(2)
              HWIND(3) = ARRAY(3)
              HWIND(4) = ARRAY(4)
              RETURN
C
C  SWITCH OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
                WINDOW = .FALSE.
              RETURN
          ELSE
                GOTO 120
          ENDIF
      ELSE
          GOTO 120
      ENDIF
C
C  ERROR UNDEFINED OPTION DETECTED
C
 120  NERR = NERR + 1
      CALL SETER (' HSTOPR -- UNDEFINED OPTION',NERR,IREC)
      RETURN
      END
