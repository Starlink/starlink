*+  HDX_TOPLEV - Locate top-level object of LOC
      SUBROUTINE HDX_TOPLEV( LOC, TLOC, STATUS )
*
*    Description :
*
*     Returns a locator to the top-level object containing the object LOC.
*     If LOC is its a top-level object, then a clone of LOC is returned.
*
*    Method :
*    Deficiencies :
*
*     Bit of a shame to have to use HDS_TRACE just to get the depth of
*     object nesting.
*
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      7 Sep 91 : Original (DJA)
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
      CHARACTER*(DAT__SZLOC)             LOC            ! Input object
*
*    Export :
*
      CHARACTER*(DAT__SZLOC)             TLOC           ! The top-level object
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*200                      FILE,PATH      ! Trace info
      CHARACTER*(DAT__SZLOC)             ALOC           ! Temp locator

      INTEGER                            LEVELS         !
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Use HDS_TRACE to get number of levels
      CALL HDS_TRACE( LOC, LEVELS, PATH, FILE, STATUS )
      CALL DAT_CLONE( LOC, TLOC, STATUS )
      DO WHILE ( LEVELS .GT. 1 )
        CALL DAT_PAREN( TLOC, ALOC, STATUS )
        CALL DAT_ANNUL( TLOC, STATUS )
        TLOC = ALOC
        LEVELS = LEVELS - 1
      END DO

*    Exit
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', '...from HDX_TOPLEV', STATUS )
      END IF

      END
