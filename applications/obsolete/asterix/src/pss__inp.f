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
      INCLUDE 'ADI_PAR'
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

*  New error context
      CALL ERR_BEGIN( STATUS )

*  Close the file
      IF ( CP_MULTI ) THEN
        CALL ADI_FCLOSE( IM_ID, STATUS )
      ELSE
        CALL USI_ANNUL( 'INP', STATUS )
      END IF

*  Restore error context
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
      INCLUDE 'ADI_PAR'
      INCLUDE 'PSS_PAR'
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
      CHARACTER                PATH*80                 ! Object path

      INTEGER                  NLEV                    ! Useless TRACE output

      LOGICAL                  OK                      ! Validity checks
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Associate psf
      IF ( IFILE .EQ. 1 ) THEN
        CALL PSF_TASSOCI( IM_ID, PSF_HAN, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get background subtracted flag if present
      IF ( .NOT. IM_PRIM ) THEN
        CALL PRF_GET( IM_ID, 'BGND_SUBTRACTED',
     :                IM_BGND_SUBTRACTED, STATUS )
        IF ( IM_BGND_SUBTRACTED ) THEN
          CALL MSG_PRNT( 'Image is background subtracted...' )
        END IF
      END IF

*    Check and map data
      CALL BDI_CHK( IM_ID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( IM_ID, PSS__MXDIM, BDS_DIMS, BDS_NDIM, STATUS )
      IF ( .NOT. OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Invalid input data', STATUS )
      ELSE
        CALL BDI_MAPR( IM_ID, 'Data', 'READ', IM_DATA_PTR, STATUS )
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

*  Get data units
      CALL BDI_GET0C( IM_ID, 'Units', IM_UNITS, STATUS )
      IF ( IM_UNITS .LE. ' ' ) IM_UNITS = 'count'

*  Look for quality
      CALL BDI_CHK( IM_ID, 'Quality', BDS_QUAL_OK, STATUS )
      IF ( BDS_QUAL_OK ) THEN
        CALL BDI_MAPUB( IM_ID, 'MaskedQuality', 'READ',
     :                  BDS_QUAL_PTR, STATUS )

*    Which way to use quality if present?
        IF ( BDS_QUAL_OK ) THEN
          CALL USI_GET0L( 'QBAD', CP_NOBADQSRC, STATUS )
        END IF

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  See if there's VARIANCE present
      CALL BDI_CHK( IM_ID, 'Variance', IM_VAR_OK, STATUS )
      IF ( IM_VAR_OK ) THEN
        CALL BDI_MAPR( IM_ID, 'Variance', 'READ', IM_VAR_PTR, STATUS )
        CALL PSS_CHK_VAR( BDS_NELM, BDS_QUAL_OK,
     :                    BDS_QUAL_PTR, %VAL(IM_VAR_PTR), STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check axes
      CALL PSS_GET_AXES( STATUS )

*  Only if first file
      IF ( IFILE .EQ. 1 ) THEN

*    PSF box sizes
        CALL PSS_PSF_INIT( STATUS )

*    Get slice
        CALL PSS_GET_SUBSET( STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get pointing info from header.
      CALL WCI_GETIDS( IM_ID, GE_PIXID, GE_PRJID, GE_SYSID, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
        GE_OK = .TRUE.
      ELSE
        CALL ERR_ANNUL( STATUS )
        GE_OK =. FALSE.
      END IF

*  Get name etc
      CALL ADI_FTRACE( IM_ID, NLEV, PATH, IM_FILE, STATUS )
      IM_OK = ( STATUS .EQ. SAI__OK )

*  Tidy up
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
      INCLUDE 'ADI_PAR'
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

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      IM_OK = .FALSE.
      IM_DYNAMIC = .FALSE.
      IM_DATA_DYNAMIC = .FALSE.
      IM_BGND_SUBTRACTED = .FALSE.

*  Get input file identifier
      IF ( CP_MULTI ) THEN

*    Get image name
        CALL PSS_MUL_NEXT( IFILE, NFILE, IMAGE, STATUS )

*    Open it
        CALL ADI_FOPEN( IMAGE, 'BinDS|Array', 'READ', IM_ID, STATUS )
        IM_PRIM = .FALSE.

      ELSE

*    Get input object from user
        CALL USI_ASSOC( 'INP', 'BinDS|Array', 'READ', IM_ID, STATUS )

*    Primitive?
        CALL ADI_DERVD( IM_ID, 'BinDS', IM_PRIM, STATUS )
        IM_PRIM = (.NOT. IM_PRIM)

      END IF

*    Report errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_INP_OPEN', STATUS )
      END IF

      END
