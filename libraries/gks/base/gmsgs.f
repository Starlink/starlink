C# IL>=a, OL>=0
      SUBROUTINE GMSGS (IWKID,LSTR,STRING)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  MESSAGE  (SUBSET)
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To send message to workstation
*
*  MAINTENANCE LOG
*  ---------------
*     30/11/83  AS   Original version stabilised
*
*  ARGUMENTS
*  ---------
*     INP   IWKID  Workstation identifier
*     INP   LSTR   Length of string
*     INP   STRING String to be displayed
*
      INTEGER IWKID, LSTR
      CHARACTER*(*) STRING
*
*---------------------------------------------------------------------

      CALL GMSG(IWKID,STRING(1:LSTR))

      END
