      SUBROUTINE HIRAD( STATUS )
*+
*  Name:
*     SUBROUTINE HIRAD

*  Description:
*     Define LBLS r-value grid.

*  History:
*     Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*     Paul Rees          04-NOV-88     IUEDR Vn. 2.0

*  Description:
*     LBLS r-value grid is defined, based on RL and RSAMP.
*     The EXTENDED and CONTINUUM parameters are used to determine
*     how (if at all) these parameters must be constrained to produce
*     the channels to be used.

*-

*  Implicit:
      IMPLICIT NONE

*  Export:
      INTEGER STATUS        ! status return

*  Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDISH'
      INCLUDE 'CMEXTP'
      INCLUDE 'CMDISP'

*  Local variables:
      REAL*8 RGAP           ! gap from order to inter-order
*.

*  Set EXTP undefined
      NOEXTP = .TRUE.

*  Distance to inter-order
      RGAP = DRDM / 2.0

*  Automatic determination of RL
      IF ( AUSLIT ) THEN
         RL( 2 ) = RGAP * 3.0
         RL( 2 ) = NINT( REAL( RL( 2 ) / RSAMP ) ) * RSAMP
         RL( 1 ) = -RL( 2 )
      END IF

*  Concessions for TRAL steals
      ROBJ( 1 ) = RL( 1 )
      ROBJ( 2 ) = RL( 2 )
      NBKG = 0

*  Set EXTP defined
      NOEXTP = .FALSE.
      STATUS = 0

      END
