      SUBROUTINE sgs_SELCH (NCHDEV)
*+
*   - - - - - -
*    S E L C H
*   - - - - - -
*
*   Set the current SGS choice device.
*
*   Given:
*      NCHDEV      i      SGS choice device
*
*   Written to COMMON:
*      NCHODV      i      current SGS choice device
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INCLUDE 'sgscom'


      INTEGER NCHDEV



      NCHODV = NCHDEV

      END
