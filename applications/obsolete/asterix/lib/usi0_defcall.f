*+  USI0_DEFCALL - Define the CALLABLE Asterix parameters system
      SUBROUTINE USI0_DEFCALL( PARID, STATUS )
*    Description :
*     <description of what the subroutine does - for user info>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*     21 May 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USI0_PAR'
*
*    Export :
*
      INTEGER			PARID			! Parameter system id
*
*    Status :
*
      INTEGER 			STATUS
*
*    External references :
*
      EXTERNAL			USI_BLK
      EXTERNAL                  SPAR_GET0L
      EXTERNAL                  SPAR_GET0I
      EXTERNAL                  SPAR_GET0R
      EXTERNAL                  SPAR_GET0D
      EXTERNAL                  SPAR_GET0C
      EXTERNAL			SPAR_PARSE
      EXTERNAL			SPAR_STATE
*-

*    Define the new system
      CALL USI0_DEFSYS( 'CALLABLE', PARID, STATUS )

*    Scalar GET routines
      CALL USI0_DEFRTN( PARID, USI__F_GET0L, SPAR_GET0L, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_GET0I, SPAR_GET0I, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_GET0R, SPAR_GET0R, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_GET0D, SPAR_GET0D, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_GET0C, SPAR_GET0C, STATUS )

*    Odds and sodds
      CALL USI0_DEFRTN( PARID, USI__F_STATE, SPAR_STATE, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_PARSE, SPAR_PARSE, STATUS )

      END
