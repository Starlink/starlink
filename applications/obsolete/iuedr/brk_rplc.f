      SUBROUTINE BRK_RPLC( NITER, NSM, NSIG, KS, NOKS, M1, M2 )

*+
*
*   Name:
*      SUBROUTINE BRK_RPLC
*
*   Description:
*      Replaces linear input array and associated quality
*      indicators with smoothed values.
*
*   History:
*      Ian Howarth        ??-AUG-84     IUEDR Vn. 1.3
*      Paul Rees          29-OCT-88     IUEDR Vn. 2.0
*      Martin Clayton     08-OCT-94     IUEDR Vn. 3.1-6
*
*   Method:
*      See Barker (1984).
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER NITER     ! number of smooths
      INTEGER NSM       ! number of channels over which smoothing is performed
      INTEGER NSIG      ! rejection threshold for bad points
      INTEGER M1        ! KS start
      INTEGER M2        ! KS end

*   Import-Export:
      REAL*8 KS(M1:M2)          ! array of values to be smoothed
      LOGICAL NOKS(M1:M2)     ! array of flags (T = no value)

*   Local variables:
      REAL*8 TKS(150)

      LOGICAL TNOKS(150)

      INTEGER I
      INTEGER K

*   Iterate NITER times
      DO I = 1, NITER

*      Calculate initial smoothed values
         CALL BRK_SM( KS, NOKS, M1, M2, TKS, TNOKS, NSM )

*      Flag aberrant values in input data
         CALL BRK_CHK( KS, NOKS, M1, M2, TKS, TNOKS, NSM, NSIG )

*      Calculate smoothed values with revised quality flags
         CALL BRK_SM( KS, NOKS, M1, M2, TKS, TNOKS, NSM )

*      Load input array with smoothed values
         DO K = M1, M2
            KS(K) = TKS(K)
            NOKS(K) = TNOKS(K)
         ENDDO
      ENDDO

      END
