C# IL>=a, OL>=1
      SUBROUTINE GQEPMI (IWKID,NTH,IER,IO,IPMIND)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE LIST ELEMENT OF POLYMARKER INDICES
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns Nth element of list of polymarker indices.
*
*  MAINTENANCE LOG
*  ---------------
*     29/09/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IWKID   workstation identifier
*     INP NTH     list element requested
*     OUT IER     error indicator
*     OUT IO      number of polyline bundle table entries
*     OUT IPMIND  Nth element of list of defined polymarker indices
*
      INTEGER IWKID, NTH, IER, IO, IPMIND
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkwke.par'
*
*---------------------------------------------------------------------


      CALL GKQXXI(IWKID,KQEPMI,NTH,IER,IO,IPMIND)

      END
