      SUBROUTINE GRQREP( ROUT, GKROU, IERR)
*+
*
*     - - - - - - -
*      G R Q R E P    (Internal routine)
*     - - - - - - -
*
*   Reports an error returned by a GKS inquiry routine
*
*   Given
*      ROUT       c      The name of the routine that called the GKS
*                        inquiry routine
*      GKROU      c      The name of the GKS inquiry routine that failed
*      IERR       i      The error status
*
*   D.L.Terrett  Starlink  Dec 1990
*+
      IMPLICIT NONE
      INCLUDE 'PGP_ERR'


      CHARACTER*(*) ROUT, GKROU
      INTEGER IERR

      CALL MSG_SETI( 'ERRNO', IERR)
      CALL MSG_SETC( 'GKROU', GKROU)
      CALL MSG_SETC( 'ROUT', ROUT)
      CALL ERR_REP( 'GRGQFA', 'GKS inquiry routine ^GKROU'//
     : ' returned error code ^ERRNO in routine ^ROUT', GRGQFA)

      END
