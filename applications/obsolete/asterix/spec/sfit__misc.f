*+  SFIT_MAPMODSTK - Allocate space for model stack
      SUBROUTINE SFIT_MAPMODSTK( NDS, PREDDAT, SPTR, STATUS )
*
*    Description :
*
*     Allocates dynamic memory for model stack
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      9 Mov 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER               NDS                    ! # observed datasets
      RECORD /PREDICTION/   PREDDAT(NDS)           ! Data predicted by model
*
*    Export :
*
      INTEGER               SPTR                   ! Stack pointer
*
*    Local variables :
*
      INTEGER               N                      ! Loop over datasets
      INTEGER               NMAX		   ! Length of longest data array
*-

*    Find length of longest data array
      NMAX=0
      DO N = 1, NDS
	IF ( PREDDAT(N).NMDAT .GT. NMAX ) NMAX = PREDDAT(N).NMDAT
      ENDDO
      CALL DYN_MAPR( 1, NMAX*MAXSTACK, SPTR, STATUS )

      END
