C# IL>=a, OL>=0
      SUBROUTINE GQTXXS (IWK,XO,YO,LSTR,STR,IER,XP,YP,TXEXPX,TXEXPY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE TEXT EXTENT  (SUBSET)
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Inquire text extent and concatenation point
*
*  MAINTENANCE LOG
*  ---------------
*     30/11/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP   IWK    workstation id.
*     INP   XO     x-text position
*     INP   YO     y-text position
*     INP   LSTR   length of string
*     INP   STR    text string
*     OUT   IER    error code
*     OUT   XP     x-concatenation point
*     OUT   YP     y-concatenation point
*     OUT   TXEXPX x-text extent
*     OUT   TXEXPY y-text extent
*
      INTEGER IWK, LSTR, IER
      CHARACTER*(*) STR
      REAL XP,YP,XO,YO,TXEXPX(4),TXEXPY(4)
*
*---------------------------------------------------------------------


      CALL GQTXX (IWK,XO,YO,STR(1:LSTR),IER,XP,YP,TXEXPX,TXEXPY)

      END
