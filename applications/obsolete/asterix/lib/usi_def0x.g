*+  USI_DEF0<T> - Set default <TYPE> environment value
      SUBROUTINE USI_DEF0<T>( PAR, VALUE, STATUS )
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
      INCLUDE 'USI0_PAR'
*
*    Import :
*
      CHARACTER*(*)		PAR			! Parameter name
      <TYPE>			VALUE			! Default value
*
*    Status :
*
      INTEGER 			STATUS
*
*    Local variables :
*
      INTEGER			RPTR			! Facility routine
*-

*    New error context
      CALL ERR_MARK

*    Locate the CANCL facility in this context
      CALL USI0_LOCRTN( USI__F_DEF0<T>, RPTR, STATUS )

*    Invoke it
      CALL USI_DEF0<T>_E( %VAL(RPTR), PAR, VALUE, STATUS )

*    Restore error context
      CALL ERR_RLSE

      END



*+  USI_DEF0<T>_E - Invoke parameter default routine
      SUBROUTINE USI_DEF0<T>_E( DEF_RTN, PAR, VALUE, STATUS )
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
      EXTERNAL			DEF_RTN			! Default routine
      CHARACTER*(*)		PAR			! Parameter name
      <TYPE>			VALUE			! Default value
*
*    Status :
*
      INTEGER 			STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Invoke the DEF routine
      CALL DEF_RTN( PAR, VALUE, STATUS )

      END
