*+  USI_DEF1<T> - Set default vector <TYPE> environment value
      SUBROUTINE USI_DEF1<T>( PAR, NVALUE, VALUE, STATUS )
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
      <TYPE>			VALUE(*)		! Default values
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

*    Locate the vector DEF facility in this context
      CALL USI0_LOCRTN( USI__F_DEF1<T>, RPTR, STATUS )

*    Invoke it
      CALL USI_DEF1<T>_E( %VAL(RPTR), PAR, NVALUE, VALUE, STATUS )

*    Restore error context
      CALL ERR_RLSE

      END



*+  USI_DEF1<T>_E - Invoke parameter vector default routine
      SUBROUTINE USI_DEF1<T>_E( DEF_RTN, PAR, NVALUE, VALUE, STATUS )
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
      INTEGER			NVALUE			! Number of values
      <TYPE>			VALUE(*)		! Default values
*
*    Status :
*
      INTEGER 			STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Invoke the DEF routine
      CALL DEF_RTN( PAR, NVALUE, VALUE, STATUS )

      END
