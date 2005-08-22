*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused J
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

      LOGICAL MCZERO

*  Ok, go...

      MCZERO = .TRUE.

      IF (RAM.ne.0.D0 .or. DECM.ne.0.D0) MCZERO = .FALSE.

      IF (MCZERO) THEN
CD      PRINT *, ' Map centre zero; use prototype header'
        MAP_RA  = RA
        MAP_DEC = DEC
      ELSE
CD      PRINT *, ' Using map centre in map header'
        MAP_RA  = RAM
        MAP_DEC = DECM
      END IF

CD    PRINT *, ' R.A. = ', MAP_RA
CD    PRINT *, ' Dec. = ', MAP_DEC

      RETURN
      END

*----------------------------------------------------------------------

