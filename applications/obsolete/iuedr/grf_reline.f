      SUBROUTINE grf_RELINE

*+
*
*   Name:
*      SUBROUTINE grf_RELINE
*
*   Description:
*      Get next line style (no parameter reads).
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings     05-MAY-82
*         AT4 version.
*      Paul Rees         14-JAN-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*         Conversion to GKS 7.2 graphics.
*      Paul Rees         09-MAY-89     IUEDR Vn. 2.1
*         Some restructuring and final conversion to SGP/16 style.
*
*   Method:
*      Reselect normal (axis) line styles.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global variables:
      INCLUDE 'CMGRAF'
      INCLUDE 'CMLINR'
      INCLUDE 'CMCOLR'

*   Set LINE style
      CALL GSLN( LINTYP(1) )

*   For COLOUR, set COLOUR
      CALL grf_PPALET( TIAXES )

      END
