C# IL>=a, OL>=0
      SUBROUTINE GKSTQ ( IRSZ, IISZ, ICSZ )
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*      Inquire amount of stack space available.
*
*  MAINTENANCE LOG
*  ---------------
*     25/05/83  CJW  Original version stabilized
*     27/06/83  CJW  Implement revised error handling precedure
*                    (No change required)
*
*  ARGUMENTS
*  ---------
*     OUT   IRSZ   Amount of real stack available
*     OUT   IISZ   Amount of integer stack available
*     OUT   ICSZ   Amount of character stack available
*
      INTEGER  IRSZ, IISZ, ICSZ
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read     /STK/    Assorted stack variables
*
      INCLUDE '../include/gkstk.cmn'
*
*  COMMENTS
*  --------
*     (1) Returns the amount available if asked for in a single lump.
*         You should not expect to get as much if you split your request
*         into more than one call to GKSTAL.
*
*     (2) No character stack is provided at present - so ICSZ = 0.
*
*     (3) Allocation of one type of stack may effect the amount left
*         of another stack.  ( Does in current implementation)
*
*---------------------------------------------------------------------

      IRSZ = MAX(KSTK - (KIUSED + 1),0)

      IISZ = MAX(KSTK - (KRUSED + 1),0)

      ICSZ = 0

      END
