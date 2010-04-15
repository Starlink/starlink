*  History:
*     25 Nov 1993 (hme):
*        Attempt to disuse IPUT_SCREEN.
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
C-----------------------------------------------------------------------------

      SUBROUTINE LIST_VTHELP(VALOPT)

C   Routine to list valid options on character page on VT100 or equivalent

      PARAMETER     (NMAX=15)
      CHARACTER     VALOPT*(*),ICH2*2
      CHARACTER     VOPTS(NMAX)*16, OPTIONS(NMAX)*1
      INTEGER       IOPT(NMAX)

      DATA IOPT    / 68, 76, 82, 66, 84, 72, 13, 69, 67, 78, 65,
     &               81, 43, 63, 83  /
      DATA VOPTS   / 'Draw last box'    , 'Mark left bndy',
     &               'Mark right bndy'  , 'Mark bottom bndy',
     &               'Mark top bndy'    , 'Show options',
     &               'Use default box'  , 'Erase and quit',
     &               'Clear alpha pge'  , 'New plot limits',
     &               'Accept box'       , 'Quit',
     &               'Draw to here'     , 'Query cursor',
     &               'Set new limits' /

*  Initialise character array
      DO I = 1, NMAX
         OPTIONS(I) = CHAR( IOPT(I) )
      END DO

      J=0
      DO I=1,NMAX
       CALL CONFIRM(OPTIONS(I),ICH2)
       IF (INDEX(VALOPT,ICH2).NE.0)   THEN
        J=J+1
*       CALL IPUT_SCREEN (ICH2//' - '//VOPTS(I),J+3,60,2)
*       CALL LIB$PUT_SCREEN(ICH2//' - '//VOPTS(I),J+3,60,2)
        PRINT *,ICH2//' - '//VOPTS(I)
       END IF
      END DO
*     CALL ISET_CURSOR(1,1)
*     CALL LIB$SET_CURSOR(1,1)


      RETURN
      END

C-----------------------------------------------------------------------
