      SUBROUTINE BRK_MERC( MR, M1, M2, TRAKED, KM, NOKM, KS, NOKS, OK )
*+
*  Name:
*     SUBROUTINE  BRK_MERC
*
*  Description:
*     Map ripple coefficients produced by calls to WOOF.
*
*  History:
*     Jack Giddings      28-AUG-84     IUEDR Vn. 1.3   Skeletal form.
*     Ian Howarth        ??-AUG-84     IUEDR Vn. 1.3
*     Paul Rees          30-OCT-88     IUEDR Vn. 2.0
*     Martin Clayton     03-OCT-94     IUEDR Vn. 3.1-6
*
*  Method:
*    This routine is given a set of ripple K values defined between
*    adjacent orders. Echelle orders and arrays have indices
*    in the range M1 to M2. The value KM(M) is the value of
*    K between orders M and M+1. This routine smooths and interpolates
*    in these to produce the array KS, where KS(M) is the K value
*    for the whole of order M. The NOKM and NOKS arrays are
*    Logical, and are TRUE when no Kx value is defined.
*    If something is wrong, OK is set FALSE; otherwise true.
*
*    See Barker (1984) for further details.
*
*-

*  Implicit:
      IMPLICIT NONE

*  Local Constants:
      INTEGER NITER               ! number of smoothing iterations
      INTEGER NSIG                ! rejection threshold
      INTEGER NSM                 ! number of smoothing channels
      PARAMETER (NITER=2, NSIG = 2, NSM = 5)

*  Import:
      INTEGER MR(2)               ! start, end orders (LOW, HIGH)
      INTEGER M1                  ! start order and index in KM, KS
      INTEGER M2                  ! end order and index in KS

      LOGICAL TRAKED(M1:M2)       ! whether order has been TRAKed

      REAL*8 KM(M1:M2 - 1)          ! ripple Ks for between M and M+1

      LOGICAL NOKM(M1:M2 - 1)     ! whether KM defined

*  Export:
      REAL*8 KS(M1:M2)              ! ripple Ks for individual orders
      REAL*8 KSM
      REAL*8 KSP

      LOGICAL NOKS(M1:M2)         ! whether KS defined
      LOGICAL NOKSM
      LOGICAL NOKSP

*  Status:
      LOGICAL OK                 ! whether KM is any good

*  Local variables:
      INTEGER M                   ! order number AND index
      INTEGER MLIM(2)
      INTEGER NGOOD
      INTEGER STATUS
*.

      STATUS = 0
*  Initialise OK
      OK = .TRUE.

*  Find first tracked order in data
      DO M = M1, M2
         IF ( TRAKED(M) ) THEN
            MLIM(1) = M
            GO TO 10
         END IF
      ENDDO

      MLIM(1) = 126
 10   CONTINUE

*  Find last tracked order in data
      DO M = M2, M1, -1
         IF ( TRAKED(M) ) THEN
            MLIM(2) = M
            GO TO 20
         END IF
      ENDDO

      MLIM(2) = 64
 20   CONTINUE

*  Ensure this range is within the range of the data available
      MLIM(1) = MAX(MLIM(1), MR(1))
      MLIM(2) = MIN(MLIM(2), MR(2))

*  Write summary to output
      CALL LINE_WRITI( '%p Orders in the range %i\\', MLIM(1) )
      CALL LINE_WRITI( ':%i used.\\', MLIM(2) )
      CALL PRTBUF( STATUS )

*  Load KS and NOKS arrays
      DO M = M1, M2 - 1
         KS(M) = KM(M)
         NOKS(M) = NOKM(M)
      ENDDO

      KS(M2) = KS(M2-1)
      NOKS(M2) = .TRUE.

*  Smooth
      CALL BRK_RPLC( NITER, NSM, NSIG, KS, NOKS, M1, M2 - 1 )

*  Interpolate 'half-integer' values to integer locations
*  For extrema, use end values

*  Load initial KSM, NOKSM, NGOOD
      KSM = KS(M1)
      NOKSM = NOKS(M1)
      NGOOD = 0

*  Loop to perform interpolation
      DO M = M1 + 1, M2 - 1
         KSP = KS(M)
         NOKSP = NOKS(M)
         KS(M) = (KSM + KSP) * 0.5
         NOKS(M) = NOKSP .OR. NOKSM
         IF ( M.LT.MLIM(1) .OR. M.GT.MLIM(2) ) NOKS(M) = .TRUE.
         IF ( .NOT. NOKS(M) ) NGOOD = NGOOD + 1
         KSM = KSP
         NOKSM = NOKSP
      ENDDO

      KS(M2) = KSM
      NOKS(M2) = NOKSM
      IF ( M2.LT.MLIM(1) .OR. M2.GT.MLIM(2) ) NOKS(M2) = .TRUE.
      IF ( .NOT. NOKSM ) NGOOD = NGOOD + 1

*  Write out summary.
      IF ( NGOOD .LT. 1 ) THEN
         CALL LINE_WCONT( '%p No orders optimised for K.\\', STATUS )

      ELSEIF ( NGOOD .EQ. 1 ) THEN
         CALL LINE_WCONT( '%p 1 order optimised for K.\\', STATUS )

      ELSE
         CALL LINE_WRITI( '%p %i orders optimised for K.\\', NGOOD )
      END IF
      CALL PRTBUF( STATUS )

      END
