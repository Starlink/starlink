      SUBROUTINE REPRES (LUREP, KRES)
*+
*
*  REPRES:  subroutine of COCO utility which reports the
*           report resolution.
*
*  Given:
*     LUREP     int      unit number for report
*     KRES      char     report resolution: H, M or L
*
*  P T Wallace   Starlink   1 June 1992
*-

      IMPLICIT NONE

      INTEGER LUREP
      CHARACTER KRES



      IF (KRES.EQ.'H') THEN
         WRITE (LUREP,'(/4X,''Report resolution is High''/)')
      ELSE IF (KRES.EQ.'M') THEN
         WRITE (LUREP,'(/4X,''Report resolution is Medium''/)')
      ELSE
         WRITE (LUREP,'(/4X,''Report resolution is Low''/)')
      END IF

      END
