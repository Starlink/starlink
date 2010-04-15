      SUBROUTINE MSC_GRPOG(NU, U1, DU, NV, V1, DV, MAXP, XP, YP, NP,
     :                     STATUS)

*+
*
*   Name:
*      SUBROUTINE MSC_GRPOG
*
*   Description:
*      The grid is used to generate a polygon which delineates its
*      coverage of the image.
*      A border of (DU,DV) around the grid is provided.
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
      INTEGER NU         ! number of U grid points

      REAL*8 U1            ! start U-value
      REAL*8 DU            ! U-step

      INTEGER NV         ! number of V grid points

      REAL*8 V1            ! start V-value
      REAL*8 DV            ! V-step

      INTEGER MAXP       ! maximum number of points in polygon

*   Export:
      REAL*8 XP(MAXP)      ! x-coordinates
      REAL*8 YP(MAXP)      ! y-coordinates

      INTEGER NP         ! actual number of points
      INTEGER STATUS     ! status flag
*   Local variables:
      INTEGER IU         ! loop index
      INTEGER IV         ! loop index

      REAL*8 U             ! U-value
      REAL*8 V             ! V-value

*   Check MAXP is large enough
      IF (MAXP.LT.(2*(NV + NU) + 4)) THEN

         CALL ERROUT('msc_grsub:  array too small\\', STATUS)
         RETURN

      END IF

      NP = 0

*   (U0,V1) to (U0,VN+1)
      U = U1 - DU
      V = V1 - DV

      DO 100 IV = 1, NV + 1

         V = V + DV
         NP = NP + 1
         XP(NP) = U
         YP(NP) = V

 100  CONTINUE

*   (U1,VN+1) to (UN+1,VN+1)
      DO 200 IU = 1, NU + 1

         U = U + DU
         NP = NP + 1
         XP(NP) = U
         YP(NP) = V

 200  CONTINUE

*   (UN+1,VN) to (UN+1,V0)
      DO 300 IV = 1, NV + 1

         V = V - DV
         NP = NP + 1
         XP(NP) = U
         YP(NP) = V

 300  CONTINUE

*   (UN,V0) to (U0,V0)
      DO 400 IU = 1, NU + 1

         U = U - DU
         NP = NP + 1
         XP(NP) = U
         YP(NP) = V

 400  CONTINUE

      STATUS = 0

      END
