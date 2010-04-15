*---------------------------------------------------------------------
      SUBROUTINE GK1ASF (NR, RA, LCOORD, LCF, FCOORD)
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Select a format for a set of coordinates, sufficiently
*     long to contain them but not exceesively long.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*  ARGUMENTS
*  ---------
*     INP NR      - Number of Coordinates
*     INP RA      - Array of Coordinates
*     OUT LCOORD  - Length of each formatted Coordinate (incl. a space)
*     OUT LCF     - Length of Format code for each Coordinate
*     OUT FCOORD  - Format string for each Coordinate
*
      INTEGER NR, LCOORD, LCF
      REAL RA(NR)
      CHARACTER*(*) FCOORD
*
*  COMMON BLOCK USAGE
*  ------------------
*     none
*
*  LOCALS
*  ------
*     IR      Do loop index for coordinate check
*     ISTART  Index at which coordinate check begins
*
      INTEGER IR, ISTART
*
*  ALGORITHM
*  ---------
*     Check that each coordinate is in the range -99.9 to 999.9 .
*     if all are, return format F6.1 .
*     else,
*     check current and each of the remaining coordinates
*                             is in the range -999.9 to 999.9 .
*     if all are, return format F7.1 .
*     else,
*                 return format F11.1 .
*
*  COMMENTS
*  --------
*     Coordinates are assumed to be in Postscript Points. Since the
*     resolution of the workstations is only 8.33 rasters per point,
*     it is sufficient to render the coordinates to an accuracy of
*     a tenth of a point.
*
*     Hence formats of the form F6.1, F7.1 & F11.1 are used.
*
*---------------------------------------------------------------------
*
      ISTART = 1
*     Test for Format F6.1 (all coords within A4 rectangle will fit)
      DO 10 IR=ISTART,NR
        IF( RA(IR) .LT. -99.9) GOTO 15
        IF( RA(IR) .GT. 999.9) GOTO 15
   10 CONTINUE
*     Coordinates fit F6.1 Format
      FCOORD = '(F6.1) '
      LCOORD = 7
      LCF    = 7
      GOTO 999

   15 CONTINUE
*     Test for Format F7.1 (all coords within A3 rectangle will fit)
      ISTART = IR
      DO 20 IR=ISTART,NR
        IF( RA(IR) .LT. -999.9) GOTO 25
        IF( RA(IR) .GT. 9999.9) GOTO 25
   20 CONTINUE
*     Coordinates fit F7.1 Format
      FCOORD = '(F7.1) '
      LCOORD = 8
      LCF    = 7
      GOTO 999

   25 CONTINUE

*     Else return maximum format (few coords should have to go here)
      FCOORD = '(F11.1) '
      LCOORD = 12
      LCF    = 8

  999 CONTINUE
      END
