C# IL>=a, OL>=0
      SUBROUTINE GKQPXA(ISIZ,IPIXA,RISUB)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             NGB
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     returns the Colour Indices of pixels in the specified extent
*
*  MAINTENANCE LOG
*  ---------------
*     01/02/83  NGB Original version stabilized
*     13/06/83  AS  Remove superfluous declarations
*     11/01/84  MGC RISUB pixel array first dimension parameter
*     22/07/88  KEVP  Replaced IFIX with machine independent INT
*
*  ARGUMENTS
*  ---------
*     INP ISIZ  Size of pixel array
*     OUT IPIXA Pixel Array
*     INP RISUB Device's Raster Input Routine
*
      INTEGER ISIZ, IPIXA(ISIZ)
      EXTERNAL RISUB
*
*  COMMON BLOCK USAGE
*  ------------------
*   Additional Arguments passed in the Comms Area:
*     QWR1 : Origin WC X-Value
*     QWR2 : Origin WC Y-Value
*     KWI1 : No of Pixel Columns
*     KWI2 : No of Pixel Rows
*     KWI3 : Pixel Array first dimension
*     KWI4 : Presence of Invalid Values (returned)
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
*
*  LOCALS
*  ------
*     DC equivalent of supplied WC origin
*
      REAL DCX,DCY
*
*  COMMENTS
*  --------
*     (Note:  We can't re-use KWI1 for the returned argument,
*     as we can't predict RISUB's usage thereof.
*     -it may initialise the return value before using the supplied value).
*
*---------------------------------------------------------------------



* Transform Origin
      CALL GKTWD(1,QWR1,QWR2,DCX,DCY)

* Invoke Device Driver to supply data
      CALL RISUB(FLOAT(INT(DCX)),FLOAT(INT(DCY)),
     :           KWI1,KWI2,KWI3,KWI4,IPIXA)

      END
