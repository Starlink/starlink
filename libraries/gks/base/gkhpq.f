C# IL>=a, OL>=0
      SUBROUTINE GKHPQ ( IRSZ, IISZ, ICSZ )
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
*      Inquire amount of heap space available.
*
*  MAINTENANCE LOG
*  ---------------
*     25/05/83  CJW   Original version stabilized
*     27/06/83  CJW   Implement revised error handling precedure
*                     (No change required)
*     13/02/87  PLP   Character heap now equivalenced to
*                     Integer/Real. Adjusted code accordingly
*     07/07/87  RMK   Changed order of INSERTs as KNIBYT now used
*                     by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     OUT   IRSZ   Amount of real heap available
*     OUT   IISZ   Amount of integer heap available
*     OUT   ICSZ   Amount of character heap available
*
      INTEGER  IRSZ, IISZ, ICSZ
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read     /HP/    Assorted heap variables
*
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkhp.cmn'
*
*  COMMENTS
*  --------
*     (1) Returns the amount available if asked for in a single lump.
*         You should not expect to get as much if you split your request
*         into more than one call to GKHPAL. ( Does NOT in current
*         implementation)
*
*     (2) Allocation of one type of heap may effect the amount left
*         of another heap.  ( Does in current implementation)
*
*---------------------------------------------------------------------

      IRSZ = KHPAVR

      IISZ = KHPAVI

      ICSZ = KHPAVI*KNIBYT

      END
