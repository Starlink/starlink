*+  PSS_OSL_MER - Move error data from static to dynamic memory
      SUBROUTINE PSS_OSL_MER( NDAT, NLEV, IN, ISRC, OUT_PTR, STATUS )
*
*    Description :
*
*     Moves the 2D block of error data, IN, to the ISRC'th component of
*     the output error array pointed to by ^OUT_PTR[NDAT,NLEV,*].
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     11 Jul 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Import :
*
      INTEGER                  NDAT                    ! # items per level
      INTEGER                  NLEV                    ! # levels
      REAL                     IN(2,NLEV)              ! Error array
      INTEGER                  ISRC                    ! Source number
*
*    Import / Export :
*
      INTEGER                  OUT_PTR                 ! Ptr to mapped errors
*
*    Local variables :
*
      INTEGER                  ILEV                    ! Loop over levels
      INTEGER                  OUT_ADDR                ! Output address
*-

*    Check status
      IF ( STATUS .EQ. SAI__OK ) THEN

*      For each level
        DO ILEV = 1, NLEV

*        Find address of output slot
          OUT_ADDR = OUT_PTR + NDAT*VAL__NBR*((ISRC-1)*NLEV + (ILEV-1))

*        Move data
          CALL ARR_COP1R( NDAT, IN(1,ILEV), %VAL(OUT_ADDR), STATUS )

        END DO

      END IF

      END
