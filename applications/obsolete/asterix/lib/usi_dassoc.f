*+  USI_DASSOC - Associate parameter with object
      SUBROUTINE USI_DASSOC( PAR, ACCESS, LOC, STATUS )
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
*    Import :
*
      CHARACTER*(*)		PAR			! Parameter name
      CHARACTER*(*)		ACCESS			! Parameter access mode
*
*    Export :
*
      CHARACTER*(DAT__SZLOC)	LOC			! Object locator
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

*    Locate the DASSOC facility in this context
      CALL USI0_LOCRTN( USI__F_DASSOC, RPTR, STATUS )

*    Invoke it
      CALL USI_DASSOC_E( %VAL(RPTR), PAR, ACCESS, LOC, STATUS )

*    Restore error context
      CALL ERR_RLSE

      END



*+  USI_DASSOC_E - Invoke parameter cancelling routine
      SUBROUTINE USI_DASSOC_E( DASSOC_RTN, PAR, ACCESS, LOC, STATUS )
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
*
*    Import :
*
      EXTERNAL			DASSOC_RTN		! Cancel routine
      CHARACTER*(*)		PAR			! Parameter name
      CHARACTER*(*)		ACCESS			! Parameter access mode
*
*    Export :
*
      CHARACTER*(DAT__SZLOC)	LOC			! Object locator
*
*    Status :
*
      INTEGER 			STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Invoke the DASSOC routine
      CALL DASSOC_RTN( PAR, ACCESS, LOC, STATUS )

      END
