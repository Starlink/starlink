C# IL>=a, OL>=0
      SUBROUTINE GKPPMS(MTYPE,SIZE,FACT,REALSZ)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Determine the realised size of a maker in DC
*     so that the marker can be picked if the cursor
*     is within the circular area containing the marker
*     symbol, when GKMTYP is used.
*
*
*  MAINTENANCE LOG
*  ---------------
*     22/01/88  KEVP  Created
*     09/01/89  KEVP  Changed name from GKLPMS to GKPPMS
*
*  ARGUMENTS
*  ---------
*     INP MTYPE  - marker type
*     INP SIZE   - nominal marker size in DC
*     INP FACT   - marker size scale factor
*     OUT REALSZ - realised marker size in DC (as a radius)
*
      INTEGER MTYPE
      REAL    SIZE,FACT,REALSZ
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwdt.cmn'
      INCLUDE '../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     AMULT  - multiplication factor for absolute marker size.
*     MAXTYP - maximum number of marker types.
*     TYPSIZ - maximum fractional distance from marker point.
*
      INTEGER MAXTYP
      PARAMETER (MAXTYP=5)
      REAL TYPSIZ(MAXTYP), AMULT

*     Marker types are: . + * o x
      DATA TYPSIZ /0.0,0.5,0.56,0.54,0.7/
*
*  COMMENTS
*  --------
*     To add another markertype:
*        Increase MAXTYP by 1.
*        Add to TYPSIZ the distance of the furthest point of
*        marker from centre (using POSX and POSY coords in GKMTYP)
*
*---------------------------------------------------------------------


* Work out AMULT. Marker type 1 is a special case since it is
* always displayed as the smallest displayable dot.

      IF (MTYPE .EQ. 1) THEN
        REALSZ = 0.0
        GOTO 999
      ELSE
        AMULT = SIZE*FACT
      ENDIF

* Test to see if MARKER size is valid.
      IF (AMULT.LT.QMNMKS(KWKIX)) THEN
* MARKER size smaller than minimum size allowed. Set MARKER
* size to minimum size allowed.
        AMULT=QMNMKS(KWKIX)
      ELSE IF (AMULT.GT.QMXMKS(KWKIX)) THEN
* MARKER size larger than maximum size allowed. Set MARKER
* size to maximum size allowed.
        AMULT=QMXMKS(KWKIX)
      END IF

      REALSZ = AMULT*TYPSIZ(MTYPE)
  999 CONTINUE

      END
