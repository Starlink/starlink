C# IL>=a, OL>=1
      SUBROUTINE GQLWK (IWTYPE,IER,MPLBTE,MPMBTE,MTXBTE,MFABTE,
     :                                                 MPAI,MCOLI)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE MAXIMUM LENGTH OF WORKSTATION STATE TABLES
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns maximum length of workstation state tables.
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*     30/03/83  AS    Fix call to GKQINT
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE - workstation type
*     OUT IER    - error indicator
*     OUT MPLBTE - maximum number of polyline bundle table entries
*     OUT MPMBTE - maximum number of polymarker bundle table entries
*     OUT MTXBTE - maximum number of text bundle table entries
*     OUT MFABTE - maximum number of fill area bundle table entries
*     OUT MPAI   - maximum number of pattern indices
*     OUT MCOLI  - maximum number of colour indices
*
      INTEGER IWTYPE, IER, MPLBTE, MPMBTE, MTXBTE, MFABTE, MPAI, MCOLI
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkwke.par'
*
*  LOCALS
*  ------
      INTEGER I
*
*---------------------------------------------------------------------


      CALL GKQINT(IWTYPE,KQLWK,IER,MPLBTE,MPMBTE,MTXBTE,MFABTE,
     :                                               MPAI,MCOLI,I)

      END
