C# IL>=a, OL>=0
      SUBROUTINE GKIOEN
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             CJW / AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  Close the IO system
*
*  MAINTENANCE LOG
*  ---------------
*     13/06/83  CJW  Original version stabilized
*     27/06/83  CJW  Implement revised error handling precedure
*      7/11/83  CJW  Arguments removed - get from common
*
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKERR/    KERRFL
*
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gkfls.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*---------------------------------------------------------------------


*     WDT

      CALL GKIOCL ( KFWDT, 0, KWDFLU )

*     ERROR  MESSAGE

      CALL GKIOCL ( KFEMES, 0, KEMFLU )

*     ERROR  STREAM

      CALL GKIOCL ( KFERR, 0, KERRFL )

*     DATABASE

      CALL GKIOCL ( KFDATA, 0, KDBFLU )

*     CSS

      CALL GKIOCL ( KFCSS, 0, KCSFLU )

      END
