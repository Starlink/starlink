*+  USI_PUT1<T> - Set vector <TYPE> environment value
      SUBROUTINE USI_PUT1<T>( PAR, NVALUE, VALUE, STATUS )
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
      INTEGER			NVALUE			! Number of values
      <TYPE>			VALUE(*)		! Parameter values
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

*    Locate the PUT1<T> facility in this context
      CALL USI0_LOCRTN( USI__F_PUT1<T>, RPTR, STATUS )

*    Invoke it
      CALL USI_PUT1<T>_E( %VAL(RPTR), PAR, NVALUE, VALUE, STATUS )

*    Restore error context
      CALL ERR_RLSE

      END



*+  USI_PUT1<T>_E - Invoke parameter put routine
      SUBROUTINE USI_PUT1<T>_E( PUT_RTN, PAR, NVALUE, VALUE, STATUS )
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
      EXTERNAL			PUT_RTN			! Default routine
      CHARACTER*(*)		PAR			! Parameter name
      INTEGER			NVALUE			! Number of values
      <TYPE>			VALUE(*)		! Parameter values
*
*    Status :
*
      INTEGER 			STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Invoke the PUT routine
      CALL PUT_RTN( PAR, NVALUE, VALUE, STATUS )

      END
