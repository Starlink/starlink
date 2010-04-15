      SUBROUTINE NDP_RANGE(ARRAY,IST,IEN,BADPIX,VMAX,VMIN)
C+
C
C   -----------------
C   N D P _ R A N G E
C   -----------------
C
C   Description
C   -----------
C   Returns the range of values in a REAL array. If the bad data flag is
C   set, magic value pixels are ignored.
C
C
C   Parameters
C   ----------
C   ARRAY   (> real array). Array to be inspected.
C   IST     (> integer). Start element number.
C   IEN     (> integer). End element number.
C   BADPIX  (> logical). Bad data flag.
C   VMAX    (< real). Maximum value.
C   VMIN    (< real). Minimum value.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   None.
C
C
C   INCLUDE statements
C   ------------------
C   INCLUDE 'MAGIC_VALUES'
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   VAX-specific statements
C   -----------------------
C   None.
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   25-NOV-1992   - Unix version (GOLDJIL)
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER IST,IEN
      LOGICAL BADPIX
      REAL    ARRAY(IEN),VMIN,VMAX
C
C     Local variables
C
      INTEGER I
C
      INCLUDE 'MAGIC_VALUES'
C
C     Check for an array which is entirely magic values
C
      IF(.NOT.BADPIX)THEN
        VMIN=ARRAY(IST)
      ELSE
        DO I=IST,IEN
          VMIN=ARRAY(I)
          IF(VMIN.GT.MAGIC_FLOAT)GO TO 10
        END DO
        VMIN=0.0
        VMAX=0.0
        GO TO 500
      END IF
   10 VMAX=VMIN
C
C     Loop through and find the actual range
C
      IF(IST.LT.IEN)THEN
        IF(.NOT.BADPIX)THEN
          DO I=IST+1,IEN
            VMIN=MIN(VMIN,ARRAY(I))
            VMAX=MAX(VMAX,ARRAY(I))
          END DO
        ELSE
          DO I=IST+1,IEN
            IF(ARRAY(I).GT.MAGIC_FLOAT)THEN
              VMIN=MIN(VMIN,ARRAY(I))
              VMAX=MAX(VMAX,ARRAY(I))
            END IF
          END DO
        END IF
      END IF
C
  500 CONTINUE
      END
