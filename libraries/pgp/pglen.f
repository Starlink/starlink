C*PGLEN -- find length of a string in a variety of units
C%void cpglen(int units, const char *string, float *xl, float *yl);
C+
      SUBROUTINE PGLEN (UNITS, STRING, XL, YL)
      REAL XL, YL
      INTEGER UNITS
      CHARACTER*(*) STRING
C
C Work out length of a string in x and y directions 
C
C Input
C  UNITS    :  0 => answer in normalized device coordinates
C              1 => answer in inches
C              2 => answer in mm
C              3 => answer in absolute device coordinates (dots)
C              4 => answer in world coordinates
C              5 => answer as a fraction of the current viewport size
C
C  STRING   :  String of interest
C Output
C  XL       :  Length of string in x direction
C  YL       :  Length of string in y direction
C
C--
C 15-Sep-1989 - new routine (Neil Killeen)
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
      REAL    D
C
      IF (PGNOTO('PGLEN')) RETURN
C
C   Work out length of a string in absolute device coordinates (dots)
C   and then convert
C
      CALL GRLEN (STRING, D)
C
      IF (UNITS.EQ.0) THEN
        XL = D / PGXSZ(PGID)
        YL = D / PGYSZ(PGID)
      ELSE IF (UNITS.EQ.1) THEN
        XL = D / PGXPIN(PGID)
        YL = D / PGYPIN(PGID)
      ELSE IF (UNITS.EQ.2) THEN
        XL = 25.4 * D / PGXPIN(PGID)
        YL = 25.4 * D / PGYPIN(PGID)
      ELSE IF (UNITS.EQ.3) THEN
        XL = D
        YL = D
      ELSE IF (UNITS.EQ.4) THEN
        XL = D / ABS(PGXSCL(PGID))
        YL = D / ABS(PGYSCL(PGID))
      ELSE IF (UNITS.EQ.5) THEN
        XL = D / PGXLEN(PGID)
        YL = D / PGYLEN(PGID)
      ELSE
        CALL GRWARN('Illegal value for UNITS in routine PGLEN')
      END IF
C
      RETURN
      END
