C# IL>=a, OL>=0
      SUBROUTINE GERHND (IER, KFUNC, IERFL )
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  Error Handling
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  Default error handler. Its only action is to call the error logger
*  (GERLOG). The routine may be replaced by the user.
*
*  MAINTENANCE LOG
*  ---------------
*     22/10/82  CJW  Original version stabilized
*     27/06/83  CJW  Implement revised error handling precedure
*                    (No change required)
*     26/07/83  CJW  Integer routine name
*
*  ARGUMENTS
*  ---------
*     INP   IER    Error Number
*     INP   KFUNC  Ident of GKS procedure
*     INP   IERFL  GKS Error File
*
      INTEGER IER, IERFL, KFUNC
*
*---------------------------------------------------------------------


      CALL GERLOG( IER, KFUNC, IERFL )

      END
