*+  PSS_INP_CLOSE - Close input files down
      SUBROUTINE PSS_INP_CLOSE( LAST, STATUS )
*
*    Description :
*
*     Reads the prime input dataset supplied to PSS by the user.
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     05 Jul 89 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Import :
*
      LOGICAL                  LAST                    ! Last input file?
*-

*    New error context
      CALL ERR_BEGIN( STATUS )

*    Unmap everything
      CALL BDA_UNMAPDATA_INT( IM_BDA, STATUS )

*    Free BDA locators
      CALL BDA_RELEASE_INT( IM_BDA, STATUS )

*    Close the file
      IF ( CP_MULTI ) THEN
        CALL HDS_CLOSE( IM_LOC, STATUS )
      ELSE
        CALL USI_ANNUL( IM_LOC, STATUS )
      END IF

*    Restore error context
      CALL ERR_END( STATUS )

      END

*+  PSS_INP_LOAD - Get information about binned dataset
      SUBROUTINE PSS_INP_LOAD( IFILE, STATUS )
*
*    Description :
*
*     Reads the prime input dataset supplied to PSS by the user.
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     05 Jul 89 : Original (DJA)
*     28 Feb 94 : Mask quality on input (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Structure definitions :
*
      INCLUDE 'SRCLIB(POI_STR)'
*
*    Global variables :
*
      INCLUDE 'PSS_ASTROM_CMN'
      INCLUDE 'PSS_CMN'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Import :
*
      INTEGER                  IFILE
*
*    Local variables :
*
      CHARACTER                ALOC*(DAT__SZLOC)       ! ASTERIX structure
      CHARACTER                HLOC*(DAT__SZLOC)       ! HEADER structure
      CHARACTER                PATH*80                 ! Object path

      INTEGER                  NDIM, DIMS(DAT__MXDIM)
      INTEGER                  NLEV                    ! Useless TRACE output

      LOGICAL                  OK                      ! Validity checks
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Associate psf
      IF ( IFILE .EQ. 1 ) THEN
        CALL PSF_ASSOCI( IM_LOC, PSF_HAN, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get background subtracted flag if present
      IF ( .NOT. IM_PRIM ) THEN
        CALL BDA_CHKAST_INT( IM_BDA, OK, STATUS )
        IF ( OK ) THEN
          CALL BDA_LOCAST_INT( IM_BDA, ALOC, STATUS )
          CALL PRO_GET( IM_LOC, 'BGND_SUBTRACTED',
     :                  IM_BGND_SUBTRACTED, STATUS )
          IF ( IM_BGND_SUBTRACTED ) THEN
            CALL MSG_PRNT( 'Image is background subtracted...' )
          END IF
        END IF
      END IF

*    Check and map data
      CALL BDA_CHKDATA_INT( IM_BDA, OK, BDS_NDIM, BDS_DIMS,STATUS )
      CALL ARR_SUMDIM( BDS_NDIM, BDS_DIMS, BDS_NELM )
      IF ( .NOT. OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', '! Invalid input data', STATUS )
      ELSE
        CALL BDA_MAPDATA_INT( IM_BDA, 'READ', IM_DATA_PTR, STATUS )
      END IF

*    If not an image, check that the product of the higher dimensions
*    is unity.
      IF ( BDS_NDIM .GT. 2 ) THEN
        IF ( BDS_NELM .NE. (BDS_DIMS(1)*BDS_DIMS(2)) ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Input is not an image or N-d array'/
     :              /' with degenerate non-spatial axes', STATUS )
        END IF
      END IF

*    Get data units
      CALL BDA_GETUNITS_INT( IM_BDA, IM_UNITS, STATUS )
      IF ( IM_UNITS .LE. ' ' ) IM_UNITS = 'count'

*    Look for quality
      CALL BDA_CHKQUAL_INT( IM_BDA, BDS_QUAL_OK, NDIM, DIMS, STATUS )
      IF ( BDS_QUAL_OK ) THEN
        CALL BDA_MAPMQUAL_INT( IM_BDA, 'READ', BDS_QUAL_PTR, STATUS )

*      Which way to use quality if present?
        IF ( BDS_QUAL_OK ) THEN
          CALL PAR_GET0L( 'QBAD', CP_NOBADQSRC, STATUS )
        END IF

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    See if there's VARIANCE present
      CALL BDA_CHKVAR_INT( IM_BDA, IM_VAR_OK, NDIM, DIMS, STATUS )
      IF ( IM_VAR_OK ) THEN
        CALL BDA_MAPVAR_INT( IM_BDA, 'READ', IM_VAR_PTR, STATUS )
        CALL PSS_CHK_VAR( BDS_NELM, BDS_QUAL_OK,
     :                    BDS_QUAL_PTR, %VAL(IM_VAR_PTR), STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check axes
      CALL PSS_GET_AXES( STATUS )

*    Only if first file
      IF ( IFILE .EQ. 1 ) THEN

*      PSF box sizes
        CALL PSS_PSF_INIT( STATUS )

*      Get slice
        CALL PSS_GET_SUBSET( STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get pointing info from header.
      CALL BDA_CHKHEAD_INT( IM_BDA, OK, STATUS )
      IF ( OK ) THEN
        CALL BDA_LOCHEAD_INT( IM_BDA, HLOC, STATUS )
        CALL POI_INIT( IM_LOC, GE_POINT, STATUS )
      ELSE
        STATUS = SAI__OK
        GE_POINT.OK =. FALSE.
      END IF

*    Get name etc
      CALL HDS_TRACE( IM_LOC, NLEV, PATH, IM_FILE, STATUS )
      IM_OK = ( STATUS .EQ. SAI__OK )

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_INP_LOAD', STATUS )
      END IF

      END

*+  PSS_INP_OPEN - Opens the next image dataset
      SUBROUTINE PSS_INP_OPEN( IFILE, NFILE, STATUS )
*
*    Description :
*
*     Uses the INP parameter to get the next input in mormal mode, or
*     gets the next image from the image list in multi mode.
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     05 Jul 89 : Original (DJA)
*     16 Jul 92 : MULTI mode added (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                  IFILE                   ! File number
      INTEGER                  NFILE                   ! Total # files
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      CHARACTER*132            IMAGE                   ! Image name
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      IM_OK = .FALSE.
      IM_DYNAMIC = .FALSE.
      IM_DATA_DYNAMIC = .FALSE.
      IM_BGND_SUBTRACTED = .FALSE.

*    Get input file locator
      IF ( CP_MULTI ) THEN

*      Get image name
        CALL PSS_MUL_NEXT( IFILE, NFILE, IMAGE, STATUS )

*      Open it
        CALL HDS_OPEN( IMAGE, 'READ', IM_LOC, STATUS )
        IM_PRIM = .FALSE.

      ELSE

*      Get input object from user
        CALL USI_ASSOCI( 'INP', 'READ', IM_LOC, IM_PRIM, STATUS )

      END IF

*    Get BDA identifier
      CALL BDA_FIND( IM_LOC, IM_BDA, STATUS )

*    Report errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_INP_OPEN', STATUS )
      END IF

      END
