      SUBROUTINE GRTOUP (DST, SRC)
*+
*     - - - - - - - -
*       G R T O U P     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Convert character string to upper case.
*
*   Given
*      SRC      c   Source character string
*
*   Returned
*      DST      c   Destination character string
*
*   D.L.Terrett  Starlink  Apr 1991
*+
      IMPLICIT NONE
      CHARACTER*(*) DST, SRC

      DST = SRC
      CALL CHR_UCASE(DST)
      END
