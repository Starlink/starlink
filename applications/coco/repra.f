      SUBROUTINE REPRA (LUREP, KRA)
*+
*
*  REPRA:  subroutine of COCO utility which reports the
*          RA format
*
*  Given:
*     LUREP     int      unit number for report
*     KRA       char     RA format: H, or D
*
*  P T Wallace   Starlink   1 June 1992
*-

      IMPLICIT NONE

      INTEGER LUREP
      CHARACTER KRA



      IF (KRA.EQ.'H') THEN
         WRITE (LUREP,'(/4X,''RA is in Hours'')')
      ELSE
         WRITE (LUREP,'(/4X,''RA is in Degrees'')')
      END IF

      END
