      SUBROUTINE sgs_SHMK (S)
*+
*   - - - - -
*    S H M K
*   - - - - -
*
*   Specify marker size.
*
*   This routine is obsolete and does nothing other than store the
*   specified size in common.
*
*   Given:
*      S         r      marker height (=width)
*
*   Written to COMMON:
*      HMK       r      current marker height
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE
      REAL S

      INCLUDE 'sgscom'




      HMK=S

      END
