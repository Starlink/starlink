C# IL>=b, OL>=0
      SUBROUTINE GQLI (IWTYPE,IER,NLCD,NSKD,NVLD,NCHD,NPCD,NSTD)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE NUMBER OF AVAILABLE LOGICAL INPUT DEVICES
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns number of available logical input devices
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*     28/09/83  AS    Change subroutine name
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE - workstation type
*     OUT IER    - error indicator
*     OUT NLCD   - number of locator devices
*     OUT NSKD   - number of stroke devices
*     OUT NVLD   - number of valuator devices
*     OUT NCHD   - number of choice devices
*     OUT NPCD   - number of pick devices
*     OUT NSTD   - number of string devices
*
      INTEGER IWTYPE, IER, NLCD, NSKD, NVLD, NCHD, NPCD, NSTD
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkwke.par'
*
*  LOCALS
*  ------
*
      INTEGER I
*
*---------------------------------------------------------------------


      CALL GKQINT(IWTYPE,KQLI,IER,NLCD,NSKD,NVLD,NCHD,NPCD,NSTD,I)

      END
