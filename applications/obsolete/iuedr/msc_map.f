      SUBROUTINE MSC_MAP( NU, U1, DU, NV, V1, DV, UV_SL, SL_UV, MASK,
     :                    NS, NL, DATA, QUAL, LMIN, LMAX, SMIN, SMAX,
     :                    F, W, Q, STATUS )

*+
*
*   Name:
*      SUBROUTINE MSC_MAP
*
*   Description:
*      Map the image onto a regular grid in (U,V) space.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          04-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      Use convolution with a 2-D trapezium profile.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Import:
      INTEGER NU                 ! number of U grid points

      REAL*8 U1                  ! start U-value
      REAL*8 DU                  ! U-step

      INTEGER NV                 ! number of V grid points

      REAL*8 V1                  ! start V-value
      REAL*8 DV                  ! V-step

      EXTERNAL UV_SL             ! subroutine for (U,V) to (S,L) transform
      EXTERNAL SL_UV             ! subroutine for (S,L) to (U,V) transform

      INTEGER MASK               ! data quality acceptance mask
      INTEGER NS                 ! size of axis 1 (sample)
      INTEGER NL                 ! size of axis 2 (line)

      INTEGER*2 DATA(NS, NL)     ! image

      BYTE QUAL(NS, NL)          ! quality

      INTEGER LMIN               ! lowest LINE
      INTEGER LMAX               ! highest LINE
      INTEGER SMIN(NL)           ! start SAMPLE for each LINE
      INTEGER SMAX(NL)           ! end SAMPLE for each LINE

*   Export:
      REAL*8 F(NU, NV)           ! mapped signal
      REAL*8 W(NU, NV)           ! weights

      BYTE Q(NU, NV)             ! data quality

      INTEGER STATUS             ! status return

*   External references:
      INTEGER DQ_AND             ! data quality AND

*   Local variables:
      REAL*8 PU
      REAL*8 PV
      REAL*8 U                   ! U-value
      REAL*8 UBASE               ! half-width of U-profile base
      REAL*8 UBOT                ! U - UBASE
      REAL*8 UI
      REAL*8 UTOP                ! U + UBASE
      REAL*8 V                   ! V-value
      REAL*8 VBASE               ! half-width of V-profile base
      REAL*8 VBOT                ! V - VBASE
      REAL*8 VI
      REAL*8 VTOP                ! V + VBASE
      REAL*8 WP

      INTEGER IL                 ! loop index
      INTEGER IS                 ! loop index
      INTEGER IU                 ! loop index
      INTEGER IUF                !
      INTEGER IUL                !
      INTEGER IV                 ! loop index
      INTEGER IVF                !
      INTEGER IVL                !
      INTEGER LMN                ! local version of LMIN
      INTEGER LMX                ! local version of LMAX
      INTEGER N                  !
      INTEGER QI                 ! local data quality value
      INTEGER SMN(4096)          ! local version of SMIN
      INTEGER SMX(4096)          ! local version of SMAX

*   Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Zero out arrays
      DO IV = 1, NV
         DO IU = 1, NU
            F(IU, IV) = 0.0
            W(IU, IV) = 0.0
            Q(IU, IV) = 1
         END DO
      END DO

*   Define folding function parameters
      UBASE = ABS(DU)
      VBASE = ABS(DV)

*   Define image subset for grid (alone)
      CALL MSC_GRSUB( NU, U1, DU, NV, V1, DV, UV_SL, NS, NL, LMN, LMX,
     :                SMN, SMX, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: msc_grsub failed in msc_map\\', STATUS )
         RETURN

      ELSE IF ( LMN .GE. LMX ) THEN
         RETURN
      END IF

*   Merge grid subset with data subset
      CALL MSC_MESUB( NL, LMIN, LMAX, SMIN, SMAX, LMN, LMX, SMN, SMX )
      IF ( LMN .GE. LMX ) RETURN

*   Go through data
      DO IL = LMN, LMX
         DO IS = SMN(IL), SMX(IL)
            CALL DQ_UTOI(QUAL(IS, IL), QI)
            IF ( DATA(IS, IL).NE. - 32768 .AND.
     :                   (DQ_AND(QI, MASK).EQ.0) ) THEN
               CALL SL_UV(DBLE(IS), DBLE(IL), U, V)

*            Compute range in U-grid
               UBOT = U - UBASE
               UTOP = U + UBASE
               IUF = MAX(IFIX(REAL((UBOT - U1 + DU) / DU)) + 1, 1)
               IUL = MIN(IFIX(REAL((UTOP - U1 + DU) / DU)), NU)

*            Compute range in V-grid
               VBOT = V - VBASE
               VTOP = V + VBASE
               IVF = MAX(IFIX(REAL((VBOT - V1 + DV)/DV)) + 1, 1)
               IVL = MIN(IFIX(REAL((VTOP - V1 + DV)/DV)), NV)

*            Fold pixel intensity into grid
               DO IV = IVF, IVL
                  VI = (IV - 1) * DV + V1
                  DO IU = IUF, IUL
                     UI = (IU - 1) * DU + U1
                     PU = MAX(0.0d0, 1.0d0 - ABS((U - UI) / UBASE))
                     PV = MAX(0.0d0, 1.0d0 - ABS((V - VI) / VBASE))
                     WP = PU * PV
                     F(IU, IV) = F(IU, IV) + WP * DATA(IS, IL)
                     W(IU, IV) = W(IU, IV) + WP
                  END DO
               END DO
            END IF
         END DO
      END DO

*   Normalise F-values and create data quality
      N = 0
      DO IV = 1, NV
         DO IU = 1, NU
            IF ( W(IU, IV) .GT. 0.0 ) THEN
               N = N + 1
               F(IU, IV) = F(IU, IV) / W(IU, IV)
               Q(IU, IV) = 0
            END IF
         END DO
      END DO

      END
