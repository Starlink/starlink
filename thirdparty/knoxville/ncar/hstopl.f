      SUBROUTINE HSTOPL(IOPT)
C
C  THESE THREE ROUTINES ARE OPTION ROUTINES TO BE USED WITH
C  THE HISTOGRAM ROUTINE -- HSTGRM
C
C  SET THE HISTOGRAM OPTIONS
C
C  INPUT
C     IOPT-CHARACTER STRING OF OPTION VALUE
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
      CHARACTER*7  IOPT
      CHARACTER*2  TAG, OPT
C
C
      SAVE
      NERR = 0
C
C  DETERMINE OPTION AND ITS VALUE
C
      TAG = IOPT(1:2)
      IF (IOPT(3:3) .EQ. '=') THEN
          OPT = IOPT(4:5)
      ELSE
          OPT = IOPT(5:6)
      ENDIF
C
C  HORIZONTAL FLAG
C
      IF (TAG .EQ. 'HO') THEN
C
C  SWITCH = ON
C
          IF (OPT .EQ. 'ON') THEN
            HORZNT = .TRUE.
            RETURN
C
C  SWITCH = OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
            HORZNT = .FALSE.
            RETURN
          ELSE
                GOTO 120
          ENDIF
C
C  PERCENT FLAG
C
      ELSEIF (TAG .EQ. 'PE') THEN
C
C  SWITCH = ON
C
          IF (OPT .EQ. 'ON') THEN
            PERCNT = .TRUE.
            RETURN
C
C  SWITCH = OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
            PERCNT = .FALSE.
            RETURN
          ELSE
                GOTO 120
          ENDIF
C
C  MIDVALUE FLAG
C
      ELSEIF (TAG .EQ. 'MI') THEN
C
C  SWITCH = ON
C
          IF (OPT .EQ. 'ON') THEN
            MIDVAL = .TRUE.
           RETURN
C
C  SWITCH = OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
           MIDVAL = .FALSE.
           RETURN
          ELSE
                GOTO 120
          ENDIF
C
C  'DEF' FLAG,  SET ALL OPTIONS TO DEFAULT VALUES
C
      ELSEIF (TAG .EQ. 'DE') THEN
C
C  SET DEFAULT FORMAT
C
        HORZNT = .FALSE.
        PERCNT = .TRUE.
        MIDVAL = .FALSE.
        SHADE = .TRUE.
        MEDIAN = .FALSE.
        PERIM = .FALSE.
        HFRAME = .TRUE.
        LISTOP = .FALSE.
        WINDOW = .FALSE.
        HWIND(1) = 0.
        HWIND(2) = 1.
        HWIND(3) = 0.
        HWIND(4) = 1.
        COLORS = .FALSE.
        COLSHA = 1.
        COLREC = 1.
        COLAXI = 1.
        COLTEX = 1.
        COLMED = 1.
        COLTIT = 1.
        COLPER = 1.
        HSTFOR = .FALSE.
        STRFOR = '(G10.3)'
        TITLE = .FALSE.
        LABEL = .FALSE.
        FREQNC = .FALSE.
      RETURN
C
C  SHADE FLAG
C
      ELSEIF (TAG .EQ. 'SH') THEN
C
C  SWITCH ON
C
          IF (OPT .EQ. 'ON') THEN
            SHADE = .TRUE.
            RETURN
C
C  SWITCH OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
            SHADE = .FALSE.
            RETURN
          ELSE
                GOTO 120
        ENDIF
C
C  MEDIAN FLAG
C
      ELSEIF (TAG .EQ. 'ME') THEN
C
C  SWITCH ON
C
          IF (OPT .EQ. 'ON') THEN
            MEDIAN = .TRUE.
            RETURN
C
C  SWITCH OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
            MEDIAN = .FALSE.
            RETURN
          ELSE
                GOTO 120
        ENDIF
C
C  PERIMETER FLAG
C
      ELSEIF (TAG .EQ. 'PR') THEN
C
C  SWITCH ON
C
          IF (OPT .EQ. 'ON') THEN
            PERIM = .TRUE.
            RETURN
C
C  SWITCH OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
            PERIM = .FALSE.
            RETURN
          ELSE
                GOTO 120
          ENDIF
C
C  FRAME FLAG
C
      ELSEIF (TAG .EQ. 'FR') THEN
C
C  SWITCH ON
C
          IF (OPT .EQ. 'ON') THEN
            HFRAME = .TRUE.
            RETURN
C
C  SWITCH OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
            HFRAME = .FALSE.
            RETURN
          ELSE
                GOTO 120
        ENDIF
C
C  LIST OPTION GET VALUE OF SWITCH
C
      ELSEIF (TAG .EQ. 'LI') THEN
C
C  ON SET LIST OPTIONS FLAG
C
          IF (OPT .EQ. 'ON') THEN
            LISTOP = .TRUE.
              RETURN
C
C  TURN OFF LIST OPTIONS FLAG
C
          ELSEIF (OPT .EQ. 'OF') THEN
            LISTOP = .FALSE.
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
      CALL SETER (' HSTOPL -- UNDEFINED OPTION',NERR,IREC)
      RETURN
C
      END
