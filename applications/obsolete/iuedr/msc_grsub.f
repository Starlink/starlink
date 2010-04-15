      SUBROUTINE MSC_GRSUB(NU, U1, DU, NV, V1, DV, UV_SL, MAXS, MAXL,
     :                     LMIN, LMAX, SMIN, SMAX, STATUS)

*+
*
*   Name:
*      SUBROUTINE MSC_GRSUB
*
*   Description:
*      The grid is used to generate a polygon which delineates its
*      coverage of the image.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          04-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER NU                ! number of U grid points

      REAL*8 U1                   ! start U-value
      REAL*8 DU                   ! U-step

      INTEGER NV                ! number of V grid points

      REAL*8 V1                   ! start V-value
      REAL*8 DV                   ! V-step

      EXTERNAL UV_SL            ! subroutine for (U,V) to (S,L) transform

      INTEGER MAXS              ! maximum number of samples per line
      INTEGER MAXL              ! maximum number of image lines

*   Export:
      INTEGER LMIN              ! minimum image line contained
      INTEGER LMAX              ! maximum image line contained
      INTEGER SMIN(MAXL)        ! minimum image sample per line
      INTEGER SMAX(MAXL)        ! maximum image sample per line
      INTEGER STATUS            ! status return

*   Local variables:
      INTEGER IP                ! point index
      INTEGER NP                ! number of polygon points

      REAL*8 LP(4*(4096 + 1))     ! l-coordinates
      REAL*8 SP(4*(4096 + 1))     ! s-coordinates
      REAL*8 UP(4*(4096 + 1))     ! u-coordinates
      REAL*8 VP(4*(4096 + 1))     ! v-coordinates

*   Define polygon
      CALL MSC_GRPOG(NU, U1, DU, NV, V1, DV, 4*(4096 + 1), UP, VP, NP,
     :               STATUS)

      IF (STATUS.NE.0) THEN

         CALL ERROUT('Error: msc_grpog failed in msc_grsub\\',
     :               STATUS)
         RETURN

      END IF

*   Distort polygon
      DO 100 IP = 1, NP

         CALL UV_SL(UP(IP), VP(IP), SP(IP), LP(IP))

 100  CONTINUE

*   Use polygon to form image subset
      CALL MSC_POSUB(NP, SP, LP, MAXS, MAXL, LMIN, LMAX, SMIN, SMAX)

      END
