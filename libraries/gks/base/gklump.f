C# IL>=a, OL>=0
      SUBROUTINE GKLUMP(ITYPE, IUNIT, IWANT, INEED, IFRAC,
     :                     ITAKE, INDEX)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM

*  Type of routine:    UTILITY
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Allocate a lump of stack
*
*  MAINTENANCE LOG
*  ---------------
*     17/03/84  CJW   Original version stabilized
*     30/03/84  CJW   Correct call to GKSTAL (S43)
*     02/04/84  RSK   Added include GKHP.PAR*
*     26/04/84  CJW   Correct scaling
*     19/01/87  PKY   IS conversion. Error number changes.
*  ARGUMENTS
*  ---------
*     INP ITYPE - Type of stack
*     INP IUNIT - Storage to be a multiple of IUNIT
*     INP IWANT - Amount of storage requested in units of IUNIT
*     INP INEED - Minimum acceptable in units of IUNIT
*     INP IFRAC - Maximum fraction of stack to use
*     OUT ITAKE - Actual size of allocation in units of IUNIT
*     OUT INDEX - Stack pointer
*
      INTEGER ITYPE, IUNIT, IWANT, INEED, IFRAC, ITAKE, INDEX
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkhp.par'
*
*  LOCALS
*  ------
*     IRSIZE Current size of real stack
*     IISIZE Current size of integer stack
*     ICSIZE Current size of character stack
*
      INTEGER IRSIZE, IISIZE, ICSIZE
*
*  ERRORS
*  ------
*      300 Not enough stack
*    -2006 ITYPE invalid
*
*  COMMENTS
*  --------
*     The amount of stack space used is subject to
*
*     INEED<=ITAKE<=IWANT
*     ITAKE<=Stack_space
*     if Stack_space/IFRAC > INEED then ITAKE<=Stack_Space/IFRAC
*
*     Example :
*
*     To chunk a Polyline of 100 coordinate pairs, preferably
*     using less than one third of the available stack space -
*
*     ITYPE = KREALS
*     IUNIT = 2        (must be a whole number of x, y coordinates)
*     IWANT = 100      (pairs)
*     INEED = 2        (a polyline must have two points)
*     IFRAC = 3        (viz 1/3rd of the stack)
*
*---------------------------------------------------------------------

*     Find out how much stack is available

      CALL GKSTQ(IRSIZE, IISIZE, ICSIZE)
      IF (ITYPE .EQ. KREALS) THEN
         ITAKE = IRSIZE
      ELSE IF (ITYPE .EQ. KINTGS) THEN
         ITAKE = IISIZE
      ELSE IF (ITYPE .EQ. KCHARS) THEN
         ITAKE = ICSIZE
      ELSE
         CALL GKBUG(-2006, 'GKLUMP')
      END IF

*     Apply constraints - the MAX makes the fraction constraint
*     effective only if it exceeds the minimum allocation acceptable

      IF (KERROR .EQ. 0) THEN
         ITAKE = ITAKE / IUNIT
         ITAKE = MIN(ITAKE, IWANT, MAX(ITAKE/IFRAC, INEED))
         IF (ITAKE .LT. INEED) THEN
            KERROR = 300
         ELSE
            CALL GKSTAL(ITYPE, ITAKE * IUNIT, INDEX)
         END IF
      END IF

*     Return "No Allocation" in event of an error

      IF (KERROR .NE. 0) THEN
         ITAKE = 0
         INDEX = KNIL
      END IF

      END
