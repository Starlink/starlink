C# IL>=a, OL>=0
      SUBROUTINE GKPSCB(NCROSS,INDEX,IORDER,IPLACE,NP,IOD,IPD,ISTART)
*
* (C) COPYRIGHT ICL & SERC  1989
*

*-------------------------------------------------------------------
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
*     Select a crossing of the clipping rectangle by the polygon
*     so that the clipped polygon can put together from the
*     minimum number of sections with the minimum quantity of reversal.
*
*  MAINTENANCE LOG
*  ---------------
*     23/02/89  KEVP  Original Version Stabilised
*     26/04/89  KEVP  Changed name from GKFCPB to GKPSCB
*     22/07/90  PLP   Commenting brought in line with standard format.
*
*  ARGUMENTS
*  ---------
*     INP  NCROSS  Number of crossings (of polygon in/out of rectangle)
*     INP  INDEX   The vertex indices of these crossings
*     INP  IORDER  The anticlockwise order of these crossings (rectangle)
*     INP  IPLACE  The placings of the crossings in IORDER
*     INP  NP      Number of vertices in unclipped polygon
*     INP  IOD     Odd, if first crossing is outwards (must be +ve)
*     INP  IPD     Odd, if lower-left corner of clipping rectangle is
*                  inside the polygon (must be +ve)
*     I/O  ISTART  INP a crossing in the polygon in (0:NCROSS-1)
*                  OUT the selected crossing in (0:NCROSS-1)
*
      INTEGER NCROSS,INDEX(NCROSS),IORDER(NCROSS),IPLACE(NCROSS)
      INTEGER IOD, IPD, ISTART, NP
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
*
*
*  LOCALS
*  ------
*     ICROSS  The identifier in (1:NCROSS) of the current crossing
*     IFCROS  The identifier of the first crossing of the section
*             used to define forward candidate crossing
*     IFCAND  The forward candidate crossing
*     IRCAND  The reverse candidate crossing
*     INDEXA  The index in the polygon vertex list of crossing IFCROS
*
*     IVERT   Vertex count in all sections
*     IREV    Vertex count in reversed sections
*
*     ICURPL  Current placing of crossing around boundary of clipping
*             rectangle (anticlockwise from lower-left)
*     IPAIR   Do loop index over pairs of crossings
*     LSECT   Length of section
*
      INTEGER ICROSS,IFCROS, IFCAND,IRCAND, INDEXA, IVERT,IREV,
     :        ICURPL,IPAIR, LSECT
*
*  STATEMENT FUNCTION
*  -----------------
      INTEGER IDIR, N
      IDIR(N) = 1 - 2*(N-2*(N/2))
*     1 if N is even, -1 if odd. (ie, (-1)**N )
*---------------------------------------------------------------------
*     Convert ISTART to (1:NCROSS)
      ISTART = ISTART + 1
*     Initialise the current polygon/rectangle edge crossing
      ICROSS = ISTART
*     and counts
      IVERT = 0
      IREV  = 0
*     current crossing index
      INDEXA = INDEX(ICROSS)
      IF(INDEXA .EQ. KNIL)THEN
*        quit if ISTART does not belong to a polygon
         ISTART = KNIL
         GOTO 999
      ENDIF
*     candidate start points
      IFCAND = KNIL
      IRCAND = KNIL


      DO 200 IPAIR = 1,NCROSS/2
*        Section of polygon inside of rectangle
         IFCROS = ICROSS
         ICROSS = ICROSS - IDIR(ICROSS+IOD)
         IF(ICROSS .EQ. NCROSS+1)THEN
*          end of crossings list reached
           ICROSS = 1
           LSECT = NP
         ELSEIF(ICROSS .EQ. 0)THEN
*          start of crossings list reached
           ICROSS = NCROSS
           LSECT = -NP
         ELSE
*          interior of crossings list
           LSECT = 0
         ENDIF
         LSECT = LSECT + INDEX(ICROSS) - INDEXA
         IF(LSECT .LT. 0)THEN
*          section traversed in reverse
           IVERT = IVERT - LSECT
           IREV  = IREV  - LSECT
           IF(IRCAND .EQ. KNIL)IRCAND = ICROSS
         ELSEIF(LSECT .GT. 0)THEN
*          section traversed forwards
           IVERT = IVERT + LSECT
           IF(IFCAND .EQ. KNIL)IFCAND = IFCROS
*        ELSE - zero length section
         ENDIF
*        Clip-induced edge section
         ICURPL = IPLACE(ICROSS)
         ICURPL = ICURPL - IDIR(ICURPL+IPD)
         IF(ICURPL .EQ. 0)ICURPL = NCROSS
         IF(ICURPL .EQ. NCROSS+1)ICURPL = 1
         ICROSS = IORDER(ICURPL)
         IF(ICROSS .EQ. ISTART)THEN
*           First crossing is returned to - ie traversal of polygon
*                                              is complete
            GOTO 250
         ENDIF
         INDEXA = INDEX(ICROSS)
  200 CONTINUE

  250 CONTINUE
      IF(IREV .GT. IVERT/2)THEN
*        Reverse traversal on clipped polygon by selecting
*        and outward crossing
         IF(IRCAND .NE. KNIL)THEN
            ISTART = IRCAND
         ELSE
*        No vertices inside clipping rectangle
            ISTART = ISTART + IDIR(ISTART+IOD)
            IF(ISTART .EQ. 0)ISTART = NCROSS
            IF(ISTART .EQ. NCROSS+1)ISTART = 1
         ENDIF
      ELSE
*        IF(IFCAND .NE. KNIL)ISTART = IFCAND
      ENDIF

*     Convert ISTART back to (0:NCROSS-1)
      ISTART = ISTART - 1
 999  CONTINUE
      END
