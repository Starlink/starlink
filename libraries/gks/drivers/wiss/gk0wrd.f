      SUBROUTINE GK0WRD(IENT,NR,NI,NC,NRP,NIP,NCP,MORE)
*
* (C) COPYRIGHT ICL & SERC  1985
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM

*  Type of routine:    W/S
*  Author:             MGC
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     WISS - Read item from current segment
*
*  ARGUMENTS
*  ---------
*     OUT IENT  - Entrypoint code
*     OUT NI    - Size of integer data
*     OUT NIP   - Stack pointer to Integer data
*     OUT NR    - Size of real data
*     OUT NRP   - Stack pointer to Real data
*     OUT NC    - Size of character data
*     OUT NCP   - Stack pointer to Character data
*     OUT MORE  - MORE in this segment item
*
      INTEGER IENT, NI, NIP, NR, NC, NRP, NCP, MORE
*
*  COMMON BLOCK USAGE
*  ------------------
*
*  EXTERNALS
*  ---------
*
*  LOCALS
*  ------
*
*  ERRORS
*  ------
*
*  COMMENTS
*  --------
*     Incapable wiss needs CSS
*
*---------------------------------------------------------------------

      CALL GKCSRD(IENT,NR,NI,NC,NRP,NIP,NCP,MORE)
      RETURN
      END
