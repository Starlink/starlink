*----------------------------------------------------------------------

      SUBROUTINE COPY_MAPPOS (RA, DEC, RAM, DECM, MAP_RA, MAP_DEC)

*  Routine to decide whether to use first set of RA/Dec or second,
*  and to copy appropriate values into output set (MAP_RA, MAP_DEC).
*  All positions in standard dd,mm,ss,cc

      IMPLICIT NONE

*     Formal parameters:

      DOUBLE PRECISION   RA,    DEC
      DOUBLE PRECISION   RAM,   DECM
      DOUBLE PRECISION   MAP_RA, MAP_DEC

*     Local parameters

      INTEGER J
      LOGICAL MCZERO

*  Ok, go...

      MCZERO = .TRUE.

      IF (RAM.ne.0.D0 .or. DECM.ne.0.D0) MCZERO = .FALSE.

      IF (MCZERO) THEN
D       TYPE *, ' Map centre zero; use prototype header'
        MAP_RA  = RA
        MAP_DEC = DEC
      ELSE
D       TYPE *, ' Using map centre in map header'
        MAP_RA  = RAM
        MAP_DEC = DECM
      END IF

D     TYPE *, ' R.A. = ', MAP_RA
D     TYPE *, ' Dec. = ', MAP_DEC

      RETURN
      END

*----------------------------------------------------------------------

