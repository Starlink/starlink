C# IL>=a, OL>=1
      SUBROUTINE GQDWKA (IWTYPE,IER,IPLBUN,IPMBUN,ITXBUN,IFABUN,
     :                                         IPAREP,ICOREP,IWKTR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE DYNAMIC MODIFICATION OF WORKSTATION
*                      ATTRIBUTES
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns dynamic modification of workstation attributes.
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE - workstation type
*     OUT IER    - error indicator
*     OUT IPLBUN - polyline bundle representation changeable
*     OUT IPMBUN - polymarker bundle representation changeable
*     OUT ITXBUN - text bundle representation changeable
*     OUT IFABUN - fill area bundle representation changeable
*     OUT IPAREP - pattern representation changeable
*     OUT ICOREP - colour representation changeable
*     OUT IWKTR  - workstation transformation changeable
*
      INTEGER IWTYPE, IER, IPLBUN, IPMBUN, ITXBUN, IFABUN, IPAREP
      INTEGER ICOREP, IWKTR
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkwke.par'
*
*---------------------------------------------------------------------


      CALL GKQINT(IWTYPE,KQDWKA,IER,IPLBUN,IPMBUN,ITXBUN,IFABUN,
     :               IPAREP,ICOREP,IWKTR)

      END
