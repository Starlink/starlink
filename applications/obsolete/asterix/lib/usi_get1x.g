*+  USI_GET1<T> - Get vector <TYPE> environment value
      SUBROUTINE USI_GET1<T>( PAR, MXVAL, VALUE, NACTVAL, STATUS )
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
      INTEGER			MXVAL			! Max values to read
*
*    Export :
*
      <TYPE>			VALUE(*)		! Default values
      INTEGER			NACTVAL			! Actual values read
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
      IF ( .NOT. USI_SYINIT ) THEN
        CALL USI_INIT( STATUS )
      END IF

*    Locate the CANCL facility in this context
      CALL USI0_LOCRTN( USI__F_GET1<T>, RPTR, STATUS )

*    Invoke it
      CALL USI_GET1<T>_E( %VAL(RPTR), PAR, MXVAL, VALUE, NACTVAL,
     :                    STATUS )

*    Restore error context
      CALL ERR_RLSE

      END



*+  USI_GET1<T>_E - Invoke parameter vector default routine
      SUBROUTINE USI_GET1<T>_E( GET_RTN, PAR, MXVAL, VALUE, NACTVAL,
     :                          STATUS )
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
      EXTERNAL			GET_RTN			! Get routine
      CHARACTER*(*)		PAR			! Parameter name
      INTEGER			MXVAL			! Max values to read
*
*    Export :
*
      <TYPE>			VALUE(*)		! Default values
      INTEGER			NACTVAL			! Actual values read
*
*    Status :
*
      INTEGER 			STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Invoke the GET routine
      CALL GET_RTN( PAR, MXVAL, VALUE, NACTVAL, STATUS )

      END
