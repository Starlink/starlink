C# IL>=a, OL>=0
      SUBROUTINE GTXS (X,Y,LSTR,STRING)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  TEXT  (SUBSET)
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     User level text output primitive routine
*
*  MAINTENANCE LOG
*  ---------------
*     30/11/82  AS   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP   X,Y     Text coordinates
*     INP   LSTR    Length of string
*     INP   STRING  String to be displayed
*
      INTEGER LSTR
      REAL X,Y
      CHARACTER*(*) STRING
*
*---------------------------------------------------------------------


      CALL GTX (X,Y,STRING(1:LSTR))

      END
