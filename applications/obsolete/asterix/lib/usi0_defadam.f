*+  USI0_DEFADAM - Define the ADAM parameters system
      SUBROUTINE USI0_DEFADAM( PARID, STATUS )
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
*     24 Nov 94 : Added _DELET method (DJA)
*     25 Nov 94 : Added _EXIST method (DJA)
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
      INCLUDE 'USI_CMN'
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
      EXTERNAL                  PAR_DEF0L
      EXTERNAL                  PAR_DEF0I
      EXTERNAL                  PAR_DEF0R
      EXTERNAL                  PAR_DEF0D
      EXTERNAL                  PAR_DEF0C
      EXTERNAL                  PAR_DEF1L
      EXTERNAL                  PAR_DEF1I
      EXTERNAL                  PAR_DEF1R
      EXTERNAL                  PAR_DEF1D
      EXTERNAL                  PAR_DEF1C
      EXTERNAL                  PAR_GET0L
      EXTERNAL                  PAR_GET0I
      EXTERNAL                  PAR_GET0R
      EXTERNAL                  PAR_GET0D
      EXTERNAL                  PAR_GET0C
      EXTERNAL                  PAR_GET1L
      EXTERNAL                  PAR_GET1I
      EXTERNAL                  PAR_GET1R
      EXTERNAL                  PAR_GET1D
      EXTERNAL                  PAR_GET1C
      EXTERNAL                  PAR_PUT0L
      EXTERNAL                  PAR_PUT0I
      EXTERNAL                  PAR_PUT0R
      EXTERNAL                  PAR_PUT0D
      EXTERNAL                  PAR_PUT0C
      EXTERNAL                  PAR_PUT1L
      EXTERNAL                  PAR_PUT1I
      EXTERNAL                  PAR_PUT1R
      EXTERNAL                  PAR_PUT1D
      EXTERNAL                  PAR_PUT1C
      EXTERNAL                  PAR_CANCL
      EXTERNAL                  PAR_PROMT
      EXTERNAL                  PAR_STATE
      EXTERNAL                  DAT_ASSOC
      EXTERNAL                  DAT_CREAT
      EXTERNAL                  DAT_DELET
      EXTERNAL                  DAT_EXIST
*-

*   Define the new system
      CALL USI0_DEFSYS( 'ADAM', PARID, STATUS )

*   Define the routines
*    Scalar DEF routines
      CALL USI0_DEFRTN( PARID, USI__F_DEF0L, PAR_DEF0L, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_DEF0I, PAR_DEF0I, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_DEF0R, PAR_DEF0R, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_DEF0D, PAR_DEF0D, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_DEF0C, PAR_DEF0C, STATUS )

*    Vector DEF routines
      CALL USI0_DEFRTN( PARID, USI__F_DEF1L, PAR_DEF1L, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_DEF1I, PAR_DEF1I, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_DEF1R, PAR_DEF1R, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_DEF1D, PAR_DEF1D, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_DEF1C, PAR_DEF1C, STATUS )

*    Scalar GET routines
      CALL USI0_DEFRTN( PARID, USI__F_GET0L, PAR_GET0L, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_GET0I, PAR_GET0I, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_GET0R, PAR_GET0R, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_GET0D, PAR_GET0D, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_GET0C, PAR_GET0C, STATUS )

*    Vector GET routines
      CALL USI0_DEFRTN( PARID, USI__F_GET1L, PAR_GET1L, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_GET1I, PAR_GET1I, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_GET1R, PAR_GET1R, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_GET1D, PAR_GET1D, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_GET1C, PAR_GET1C, STATUS )

*    Scalar PUT routines
      CALL USI0_DEFRTN( PARID, USI__F_PUT0L, PAR_PUT0L, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_PUT0I, PAR_PUT0I, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_PUT0R, PAR_PUT0R, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_PUT0D, PAR_PUT0D, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_PUT0C, PAR_PUT0C, STATUS )

*    Vector PUT routines
      CALL USI0_DEFRTN( PARID, USI__F_PUT1L, PAR_PUT1L, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_PUT1I, PAR_PUT1I, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_PUT1R, PAR_PUT1R, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_PUT1D, PAR_PUT1D, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_PUT1C, PAR_PUT1C, STATUS )

*    Odds and sodds
      CALL USI0_DEFRTN( PARID, USI__F_CANCL, PAR_CANCL, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_PROMT, PAR_PROMT, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_STATE, PAR_STATE, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_DASSOC, DAT_ASSOC, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_DCREAT, DAT_CREAT, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_DELET, DAT_DELET, STATUS )
      CALL USI0_DEFRTN( PARID, USI__F_DEXIST, DAT_EXIST, STATUS )

      END
