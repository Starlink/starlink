      SUBROUTINE grf_LUTCS(WKID, LUTS, STATUS)

*+
*
*   Name:
*      SUBROUTINE grf_LUTCS
*
*   Description:
*      Define a stepped colour table for the current GKS workstation.
*
*   Authors:
*      Paul Rees
*
*   History:
*      Paul Rees     06-OCT-88     IUEDR Vn. 2.1
*         Original FORTRAN code.
*      Paul Rees     10-OCT-89     IUEDR Vn. 2.1
*         Final edit to merge with GRFSUB routines.
*
*   Method:
*      Call (GKS) GSCR to set up the colour table.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER WKID                   ! GKS workstation ID
      INTEGER LUTS(2)                ! range of look-up table indices

*   Export:
      INTEGER STATUS                 ! status return

*   Global constants:
      INTEGER ERR                    ! error status
      INTEGER OK                     ! OK status

      PARAMETER (ERR=-3, OK=0)

*   Local constants:
      INTEGER NCOLR                  ! number of separate colours

      PARAMETER (NCOLR=13)

*   Local variables:
      INTEGER CINDEX                 ! colour index
      INTEGER I                      ! loop index
      INTEGER IEND                   ! loop bound
      INTEGER ISTART                 ! loop bound
      INTEGER J                      ! loop index
      INTEGER NSTEP                  ! number of colour steps in table

      REAL HEATLUT(NCOLR, 3)         ! pseudo heat lookup table

*   Data:
      DATA ((HEATLUT(I, J), J = 1, 3), I = 1, NCOLR)/
     :      0.0, 0.0, 1.0,
     :      0.5, 0.0, 1.0,
     :      0.5, 0.5, 1.0,
     :      1.0, 0.5, 1.0,
     :      1.0, 0.0, 1.0,
     :      1.0, 0.0, 0.5,
     :      1.0, 0.0, 0.0,
     :      1.0, 0.5, 0.0,
     :      1.0, 1.0, 0.0,
     :      0.5, 1.0, 0.0,
     :      0.0, 1.0, 0.0,
     :      0.5, 1.0, 0.5,
     :      1.0, 1.0, 0.5/

*   Initialise STATUS
      STATUS = OK

*   Determine colour table step size
      NSTEP = (LUTS(2)-LUTS(1)+1)/NCOLR

*   Loop to load colour table
      DO I = 1, NCOLR-1

         DO J = 1, NSTEP
            CINDEX = (I - 1)*NSTEP + J - 1 + LUTS(1)
            CALL GSCR(WKID, CINDEX, HEATLUT(I, 1), HEATLUT(I, 2),
     :                HEATLUT(I, 3))
         END DO
      END DO

      ISTART = (NCOLR-1)*NSTEP + LUTS(1)
      IEND = LUTS(2)

*   Finish off
      DO I = ISTART, IEND
         CINDEX = I
         CALL GSCR(WKID, CINDEX, HEATLUT(NCOLR, 1),
     :             HEATLUT(NCOLR, 2), HEATLUT(NCOLR, 3))
      END DO

      END
