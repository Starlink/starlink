*+  PSF_INIT - Initialise the PSF system
      SUBROUTINE PSF_INIT( STATUS )
*
*    Description :
*
*     Sets up PSF_CMN
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     25 Oct 89 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'PSF_CMN'
*
*    Status :
*
      INTEGER STATUS
*
*    External references :
*
      EXTERNAL 			PSF_BLK
*
*    Local variables :
*
      INTEGER                  	I               ! Loop over libraries/models
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Zero the library slots
      L_NLIB = 0

*    Reset psf slots
      DO I = 1, PSF_NMAX
        P_USED(I) = .FALSE.
      END DO

*    Reset default time/energy block
      TE_INIT = .FALSE.

*    Locate all the valid libraries defined by the library search path
      CALL PSF_LIBFIND( STATUS )

*    Psf system now initialised
      PSFINIT = .TRUE.

      END



*+  PSF_LIBFIND - Locate all libraries in search path
      SUBROUTINE PSF_LIBFIND( STATUS )
*
*    Description :
*
*     The environment variable AST_PSF_LIB_PATH is assumed to contain a
*     non-empty list of psf library names, separated by commas.
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      4 Mar 90 : Original (DJA)
*     19 Oct 92 : Only do library check in VMS (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'PSF_CMN'
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      INTEGER                  PSF_FINDR
*
*    Local variables :
*
      CHARACTER*80             LIBRARY_LIST          ! The list of libraries

      INTEGER                  BEG, IC               ! Character pointers
      INTEGER                  HANDLE                ! Handle to shareable lib
      INTEGER                  NLIB                  ! Library index
      INTEGER                  SHARE_PTR             ! Ptr to PSF_SHARE_INIT rtn

      LOGICAL                  OK                    ! Library exists
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get library list
      CALL PSX_GETENV( 'AST_PSF_LIB_PATH', LIBRARY_LIST, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Zero the library count
      NLIB = 0

*    Scan over library list getting library names
      IC = 1
      DO WHILE ( STATUS .EQ. SAI__OK )

*      Find word start
        CALL CHR_FIWS( LIBRARY_LIST, IC, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN

*        Get word end. This routine cannot fail if we are already at a
*        word start
          BEG = IC
          CALL CHR_FIWE( LIBRARY_LIST, IC, STATUS )

*        Check existance
          CALL PSF_LLOAD( LIBRARY_LIST(BEG:IC), OK, HANDLE, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

*        Library loaded ok?
          IF ( .NOT. OK ) THEN
            CALL MSG_SETC( 'LIB', LIBRARY_LIST(BEG:IC) )
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Psf library ^LIB does not exist',
     :                                                   STATUS )
            GOTO 99
          END IF

*        Store library name
          NLIB = NLIB + 1
          L_NAME(NLIB) = LIBRARY_LIST(BEG:IC)
          L_NLEN(NLIB) = IC - BEG + 1
          L_HAN(NLIB) = HANDLE

*        Locate initialisation routine
          STATUS = PSF_FINDR( 'PSF_SHARE_INIT', NLIB, SHARE_PTR )
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_PRNT( '! Cannot find PSF_SHARE_INIT in psf'/
     :                       /' library '//LIBRARY_LIST(BEG:IC) )
            GOTO 99
          END IF

*        Invoke initialisation routine
          CALL PSF_LIB_INIT_EXEC( %VAL(SHARE_PTR), L_NMOD(NLIB),
     :                            L_MODN(1,NLIB), STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

*        Next library
          IC = IC + 1

        END IF

      END DO
      STATUS = SAI__OK

*    No libraries
      IF ( NLIB .EQ. 0 ) THEN
        CALL MSG_PRNT( '! No valid psf libraries specified' )
        STATUS = SAI__ERROR
      ELSE
        L_NLIB = NLIB
      END IF

*    Tidy up
  99  IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_LIBFIND', STATUS )
      END IF

      END



*+  PSF_LIB_INIT_EXEC - Locate all libraries in search path
      SUBROUTINE PSF_LIB_INIT_EXEC(INIT_ROUTINE, NMOD, MODULES, STATUS )
*
*    Description :
*
*     Execute a psf shareable library initialisation routine. The routine
*     is passed by %VAL().
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     17 Dec 91 : Original (DJA)
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
      EXTERNAL          INIT_ROUTINE                ! A SHARE_INIT routine
*
*    Export :
*
      INTEGER           NMOD                        ! # modules
      CHARACTER*(*)     MODULES(*)                  ! Module names
*
*    Status :
*
      INTEGER STATUS
*-

      CALL INIT_ROUTINE( NMOD, MODULES, STATUS )

      END
