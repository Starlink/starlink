      SUBROUTINE CALC_POSOFF (RAM, DECM, RA, DEC, DRA1, DDEC1,
     &                        RAOFF, DECOFF)

*  Routine to evaluate apparent position offset (in arcsec) given a
*  'map centre' (in normal hms.s form), a 'scan centre',  and offsets
*  (in arcseconds).

      IMPLICIT          NONE

*     Formal parameters:

      REAL*8            RAM         ! map centre R.A. (deg)
      REAL*8            DECM        ! map centre Dec.. (deg)
      REAL*8            RA          ! scan centre R.A. (deg)
      REAL*8            DEC         ! scan centre Dec.. (deg)
      REAL              DRA1        ! offset R.A. (arcsec)
      REAL              DDEC1       ! offset Dec. (arcsec)
      REAL              RAOFF       ! R.A. offset from map_centre (arcsec)
      REAL              DECOFF      ! Dec. offset from map_centre (arcsec)

*     Local variables

      DOUBLE PRECISION  APRAS       ! apparent R.A. (radians) of scan
      DOUBLE PRECISION  APDECS      ! apparent Dec. (radians) of scan

      DOUBLE PRECISION  APRAM       ! apparent R.A. (radians) of map
      DOUBLE PRECISION  APDECM      ! apparent Dec. (radians) of map

      DOUBLE PRECISION  DPI   / 3.141592654D0 /

*  Ok, go...

D     TYPE *, 'calc-posoff: RA', RA
D     TYPE *, 'calc-posoff: DEC', DEC
D     TYPE *, 'calc-posoff: RAM', RAM
D     TYPE *, 'calc-posoff: DECM', DECM
D     TYPE *, 'calc-posoff: DRA1, DDEC1: ', DRA1, DDEC1

*     First convert the scan position using standard utilities

      APRAS  = RA  * DPI/180.
      APDECS = DEC * DPI/180.

*     Then convert the map centre position: if equal to (0,0) assume
*     that this is old-format map and use prototype header values.

      APRAM  = RAM  * DPI/180.
      APDECM = DECM * DPI/180.

      IF (APRAM.EQ.0.D0 .and. APDECM.EQ.0.D0) THEN
        APRAM  = APRAS
        APDECM = APDECS
      END IF

C     Now find total offset of current spectrum in arcsecond

      RAOFF  = (648000.D0/DPI) * COS (APDECM) * (APRAS-APRAM) + DRA1
      DECOFF = (648000.D0/DPI) * (APDECS-APDECM) + DDEC1

D     TYPE *, ' -- calc_posoff --'
D     TYPE *, '    Total RA, Dec offsets (arcsec): ',RAOFF,DECOFF

      RETURN
      END
