      SUBROUTINE BRK_OVLAP( M1, N1, W1, F1, Q1, WCUT1,
     :                      M2, N2, W2, F2, Q2, WCUT2,
     :                      IM1W1, IM1W2, IM2W1, IM2W2 )

*+
*
*   Name:
*      SUBROUTINNE BRK_OVLAP
*
*   Description:
*      Calculates range of overlap of two adjacent orders.
*
*   History:
*      Ian Howarth        ??-AUG-84     IUEDR Vn. 1.3
*      Paul Rees          30-OCT-88     IUEDR Vn. 2.0
*      Martin Clayton     08-OCT-94     IUEDR Vn. 3.1-6
*
*   Method:
*      See Barker (1984).
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER M1          ! order number for order 1
      INTEGER M2          ! order number for order 2
      INTEGER N1          ! number of points in order 1
      INTEGER N2          ! number of points on order 2
      REAL*8 W1(N1)         ! array of wavelengths for order 1
      REAL*8 W2(N2)         ! array of wavelengths for order 2
      REAL*8 F1(N1)         ! array of fluxes for order 1
      REAL*8 F2(N2)         ! array of fluxes for order 2
      INTEGER Q1(N1)      ! array of quality indicators for order 1
      INTEGER Q2(N2)      ! array of quality indicators for order 2
      REAL*8 WCUT1(1:2)     ! cutoff wavelengths for order 1
      REAL*8 WCUT2(1:2)     ! cutoff wavelengths for order 2

*   Export:
      INTEGER IM1W1       ! IMiWj - index of start (j=1) or end (j=2)
      INTEGER IM1W2       !         of overlap in order indexed 'i'
      INTEGER IM2W1
      INTEGER IM2W2

*   Local variables:
      REAL*8 WC11
      REAL*8 WC12
      REAL*8 WC21
      REAL*8 WC22

      INTEGER I
      INTEGER NGOOD

*   Ensure at least one Angstrom of (pseudo-)overlap
      WC11 = WCUT1(1)
      WC22 = WCUT2(2)

      IF ( (WC22 - WC11) .GE. 1.0 ) THEN

*      For cases where orders overlap by 1.0 Angstrom or more
         WC12 = WC22
         WC21 = WC11

      ELSE

*      For cases where orders do not overlap by 1.0 Angstrom or more
         WC12 = WC11 + 1.0
         WC21 = WC22 - 1.0

      END IF

*   Ensure that WC12 and WC21 remain within the appropriate order
*   WC12 contains longward wavelength of the intended overlap in order 1
      WC12 = MIN(WC12, WCUT1(2))

*   WC21 contains shortward wavelength of the intended overlap in order 2
      WC21 = MAX(WC21, WCUT2(1))

*   Determine corresponding array indexes
*   IM2W1 contains start wavelength number of overlap region in order 2
      IM2W1 = 1

      DO WHILE ( W2(IM2W1).LT.WC21 .AND. IM2W1.LT.N2 )
         IM2W1 = IM2W1 + 1
      ENDDO

*   IM2W1 contains end wavelength number of overlap region in order 2
      IM2W2 = N2

      DO WHILE ( W2(IM2W2).GT.WC22 .AND. IM2W2.GT.1 )
         IM2W2 = IM2W2 - 1
      ENDDO

*   IM1W1 contains start wavelength number of overlap region in order 1
      IM1W1 = 1

      DO WHILE ( W1(IM1W1).LT.WC11 .AND. IM1W1.LT.N1 )
         IM1W1 = IM1W1 + 1
      ENDDO

*   IM1W2 contains end wavelength number of overlap region in order 1
      IM1W2 = N1

      DO WHILE ( W1(IM1W2).GT.WC12 .AND. IM2W2.GT.1 )
         IM1W2 = IM1W2 - 1
      ENDDO

*   Try to ensure at least 15 valid points available
*   (Important for long wavelength orders, where
*   editing data may have removed regions where
*   "cutwave" indicates numbers exist)

*   For order 2
      NGOOD = 0
      I = N2

      DO WHILE ( NGOOD.LT.15 .AND. I.GT.1 )
         IF ( Q2(I) .EQ. 0 ) NGOOD = NGOOD + 1
         I = I - 1
      ENDDO

      IM2W1 = MIN(I, IM2W1)

*   For order 1
      NGOOD = 0
      I = 1

      DO WHILE ( NGOOD.LT.15 .AND. I.LT.N1 )
         IF ( Q1(I) .EQ. 0 ) NGOOD = NGOOD + 1
         I = I + 1
      ENDDO

      IM1W2 = MAX(I, IM1W2)

      END
