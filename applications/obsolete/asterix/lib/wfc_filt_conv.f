*+  WFC_FILT_CONV - Convert dataset filter to CAL filter number
      SUBROUTINE WFC_FILT_CONV( DFILT, CFILT, STATUS )
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     19 Jul 90 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*
*    Import :
*
      INTEGER          DFILT
*
*    Export :
*
      INTEGER          CFILT
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      INTEGER                  CAL_FILT_S2N
*
*    Local constants :
*
      INTEGER                  MAXF
         PARAMETER             ( MAXF = 16)
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)   LOC
      CHARACTER*(DAT__SZLOC)   FLOC
      CHARACTER*3              RPS(MAXF)

      INTEGER                  NFIL
      INTEGER                  POS(MAXF)
*
*    Local data :
*
      SAVE NFIL, POS, RPS
      DATA NFIL/0/
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Run through already?
      IF ( NFIL .EQ. 0 ) THEN

*       Initialise
         CALL CIN_INIT( LOC, STATUS )

*       Locate filters stuff
         CALL DAT_FIND( LOC, 'FILTERS', FLOC, STATUS )

*       Get positions
         CALL CMP_GET1I( FLOC, 'POS', MAXF, POS, NFIL, STATUS )
         CALL CMP_GET1C( FLOC, 'RPS', MAXF, RPS, NFIL, STATUS )
         CALL DAT_ANNUL( FLOC, STATUS )

      END IF

*    Locate number in lookup table
      CFILT = 1
      DO WHILE ( ( DFILT .NE. POS(CFILT) ) .AND. ( CFILT .LE. 10 ) )
         CFILT = CFILT + 1
      END DO
      CFILT = CAL_FILT_S2N(RPS(CFILT))

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', '...from WFC_FILT_CONV', STATUS )
      END IF

      END
