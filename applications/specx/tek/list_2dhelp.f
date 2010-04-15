*     14 Dec 1993 (hme):
*        Attempt to disuse IPUT_SCREEN, ISET_CURSOR.
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
C-----------------------------------------------------------------------------

      SUBROUTINE LIST_2DHELP (VALOPT)

C   Routine to list valid options on character page on VT100 or equivalent

      PARAMETER     (NMAX=30)
      CHARACTER     VALOPT*(*),ICH2*2
      CHARACTER     VOPTS(NMAX)*16, OPTIONS(NMAX)*1
      INTEGER       IOPT(NMAX)

      DATA IOPT / 68, 94, 62, 76, 82, 66, 84, 72, 13, 69, 67,
     &               78, 65, 81, 43, 71, 77, 86, 80, 63, 88, 73, 83,
     &               49, 50, 51, 52, 53, 87, 48 /
      DATA VOPTS   / 'Draw last box'    , 'Define height',
     &               'Define width'     , 'Mark left bndy',
     &               'Mark right bndy'  , 'Mark bottom bndy',
     &               'Mark top bndy'    , 'Show options',
     &               'Use default box'  , 'Erase and quit',
     &               'Clear alpha pge'  , 'New plot limits',
     &               'Accept box'       , 'Quit',
     &               'Draw to here'     , 'Get spectrum',
     &               'Mark z-value'     , 'Mark samples',
     &               'Hardcopy'         , 'Query coords',
     &               'Map max and min'  , 'Recontour',
     &               'Integrated int''y', 'Linear greyscale',
     &               'Colour contours'  , 'Power greyscale',
     &               'Blue->yellow'     , 'MRAO col spiral',
     &               'Change greyscale' , 'Toggle log scales'/

*  Initialise character array
      DO I = 1, NMAX
         OPTIONS(I) = CHAR( IOPT(I) )
      END DO

      J=0
      DO I = 1,NMAX
        CALL CONFIRM (OPTIONS(I), ICH2)
        IF (INDEX(VALOPT,ICH2).NE.0)   THEN
          J = J+1
*          CALL IPUT_SCREEN (ICH2//' - '//VOPTS(I),J+2,60,2)
          PRINT *,ICH2//' - '//VOPTS(I)
        END IF
      END DO

*      CALL ISET_CURSOR (1,1)

      RETURN
      END

C-----------------------------------------------------------------------
