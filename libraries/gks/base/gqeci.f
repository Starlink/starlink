C# IL>=a, OL>=0
      SUBROUTINE GQECI (IWKID,NTH,IER,IO,ICTIND)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE LIST ELEMENT OF COLOUR INDICES
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns Nth element of list of colour indices.
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
*     OUT IO      number of colour table entries
*     OUT ICTIND  Nth element of list of defined colour indices
*
      INTEGER IWKID, NTH, IER, IO, ICTIND
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkwke.par'
*
*---------------------------------------------------------------------


      CALL GKQXXI(IWKID,KQECI,NTH,IER,IO,ICTIND)

      END
