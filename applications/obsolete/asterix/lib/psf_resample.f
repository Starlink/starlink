*+  PSF_RESAMPLE - Resample psf with offset
      SUBROUTINE PSF_RESAMPLE( NX, NY, IN, OX, OY, WORK, OUT, STATUS )
     :
*
*    Description :
*
*     Performs a bilinear interpolation on the input 2D array to give the
*     same grid offset by a specified vector.
*
*    History :
*
*     25 Apr 90 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER                  STATUS
*
*    Import :
*
      INTEGER                  NX,NY                      ! Array size
      REAL                     IN(NX,NY)                  ! Input array
      REAL                     OX,OY                      ! Offset vector
*
*    Workspace :
*
      REAL                     WORK(NX,NY,2)              ! Input array
*
*    Export :
*
      REAL                     OUT(NX,NY)                 ! Shifted array
*
*    Local variables :
*
      REAL                     W(-1:1,-1:1)
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Create forward and reverse masks
      CALL PSF_RESAMPLE_MASK( OX, OY, W )

*    Resample IN array to OUT
      CALL PSF_RESAMPLE_SHIFT( NX, NY, IN, W, OUT, STATUS )

      END



*+  PSF_RESAMPLE_MASK - Constructs a resample mask given a shift vector
      SUBROUTINE PSF_RESAMPLE_MASK( DX, DY, MASK )
*    Description :
*     <description of what the subroutine does - for user info>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     26 Feb 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      REAL                     DX, DY                     ! The shift vector
*
*    Export :
*
      REAL                     MASK(-1:1,-1:1)            ! The shift mask
*
*    Local variables :
*
      REAL                     AX,AY                      ! Absolute shifts

      INTEGER                  I,J                        ! Loops over mask
      INTEGER                  IC, JC                     ! Corner indices
*-

*    Create weighting factors
      DO J = -1, 1
        DO I = -1, 1
          MASK(I,J) = 0.0
        END DO
      END DO

*    Indices in weight array for the only corner being filled
      IC = SIGN(1.0,DX)
      JC = SIGN(1.0,DY)

*    Aboslute values of shifts
      AX = ABS(DX)
      AY = ABS(DY)

*    Set non-zero weights
      MASK(0,0) = (1.0-AX)*(1.0-AY)          ! Contribution from centre pixel
      MASK(IC,JC) = AX*AY                    ! The corner pixel
      MASK(IC,0) = AX*(1.0-AY)               ! Along X axis
      MASK(0,JC) = (1.0-AX)*AY               ! Along Y axis

      END




*+  PSF_RESAMPLE_SHIFT - Resample one array using a given mask
      SUBROUTINE PSF_RESAMPLE_SHIFT( NX, NY, IN, MASK, OUT, STATUS )
*
*    Description :
*
*    History :
*
*     25 Feb 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER                  STATUS
*
*    Import :
*
      INTEGER                  NX,NY                      ! Array size
      REAL                     IN(NX,NY)                  ! Input array
      REAL                     MASK(-1:1,-1:1)            ! Shift mask
*
*    Export :
*
      REAL                     OUT(NX,NY)                 ! Shifted array
*
*    Local variables :
*
      REAL                     MVAL			! Current mask value

      INTEGER                  I,J,II,JJ,PJJ
      LOGICAL                  FIRST
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      DO J = 1, NY
        DO I = 1, NX
          OUT(I,J) = 0.0
        END DO
      END DO

*    Loop over mask
      FIRST = .TRUE.
      DO JJ = -1, 1
        DO II = -1, 1

*        Mask value significant?
          MVAL = MASK(II,JJ)
          IF ( MVAL .NE. 0.0 ) THEN

            IF ( FIRST ) THEN
              FIRST = .FALSE.

*            For each row of input
              DO J = MAX(1,1+JJ), MIN(NY,NY+JJ)

                PJJ = J-JJ

*              For each pixel of row
                DO I = MAX(1,1+II), MIN(NX,NX+II)
                  OUT(I,J) = MVAL*IN(I-II,PJJ)
                END DO

              END DO

            ELSE

*            For each row of input
              DO J = MAX(1,1+JJ), MIN(NY,NY+JJ)

                PJJ = J-JJ

*              For each pixel of row
                DO I = MAX(1,1+II), MIN(NX,NX+II)
                  OUT(I,J) = OUT(I,J) + MVAL*IN(I-II,PJJ)
                END DO

              END DO

            END IF

          END IF

*      End loop over mask
        END DO
      END DO

      END
