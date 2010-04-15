      SUBROUTINE grf_LUTG(WKID, LUTS, STATUS)

*+
*
*   Name:
*      SUBROUTINE grf_LUTG
*
*   Description:
*      Define a continuous grey scale for the current GKS workstation.
*
*   Authors:
*      Paul Rees
*
*   History:
*      Paul Rees     06-OCT-88     IUEDR Vn. 2.1
*         Original FORTRAN code.
*      Paul Rees     10-MAY-89     IUEDR Vn. 2.1
*         Final edit to merge with GRFSUB routines.
*
*   Method:
*      Call (GKS) GSCR to set up the colour table.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER WKID       ! GKS workstation ID
      INTEGER LUTS(2)    ! range of look-up table indices

*   Export:
      INTEGER STATUS     ! status return

*   Global constants:
      INTEGER ERR        ! error status
      INTEGER OK         ! OK status
      PARAMETER (ERR=-3, OK=0)

*   Local variables:
      REAL CONV          ! grey scale conversion factor
      REAL LUTVAL        ! grey scale colour intensity

      INTEGER CINDEX     ! colour index

*   Initialise STATUS
      STATUS = OK

*   Determine grey scale increment
      CONV = 1.0 / REAL(LUTS(2) - LUTS(1))

*   Loop to load grey scale
      DO CINDEX = LUTS(1), LUTS(2)
         LUTVAL = REAL(CINDEX - LUTS(1)) * CONV
         CALL GSCR(WKID, CINDEX, LUTVAL, LUTVAL, LUTVAL)
      END DO

      END
