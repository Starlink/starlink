*+  USI_DCREAT - Create new object via parameter
      SUBROUTINE USI_DCREAT( PAR, TYPE, NDIM, DIMS, STATUS )
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
*    Global variables :
*
      INCLUDE 'USI_CMN'
*
*    Import :
*
      CHARACTER*(*)		PAR			! Parameter name
      CHARACTER*(*)		TYPE			! New object type
      INTEGER			NDIM			! Dimensionality
      INTEGER			DIMS(*)			! DImensions
*
*    Status :
*
      INTEGER 			STATUS
*
*    External references :
*
      EXTERNAL			USI_BLK
*
*    Local variables :
*
      INTEGER			RPTR			! Facility routine
*-

*    New error context
      CALL ERR_MARK

*    Check USI initialised
      IF ( .NOT .USI_SYINIT ) THEN
        CALL USI_INIT( STATUS )
      END IF

*    Locate the DCREAT facility in this context
      CALL USI0_LOCRTN( USI__F_DCREAT, RPTR, STATUS )

*    Invoke it
      CALL USI_DCREAT_E( %VAL(RPTR), PAR, TYPE, NDIM, DIMS, STATUS )

*    Restore error context
      CALL ERR_RLSE

      END



*+  USI_DCREAT_E - Invoke parameter cancelling routine
      SUBROUTINE USI_DCREAT_E( DCREAT_RTN, PAR, TYPE, NDIM, DIMS,
     :                         STATUS )
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
*
*    Import :
*
      EXTERNAL			DCREAT_RTN		! Cancel routine
      CHARACTER*(*)		PAR			! Parameter name
      CHARACTER*(*)		TYPE			! New object type
      INTEGER			NDIM			! Dimensionality
      INTEGER			DIMS(*)			! DImensions
*
*    Status :
*
      INTEGER 			STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Invoke the DCREAT routine
      CALL DCREAT_RTN( PAR, TYPE, NDIM, DIMS, STATUS )

      END
