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
      INCLUDE 'GRP_PAR'
      INCLUDE 'PAR_ERR'
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
      CHARACTER                 PATH*80                 ! Object path
      CHARACTER*132		SREGION			! Search region file

      REAL			BASE(2), SCALE(2)	! ARD mask bounds

      INTEGER			GRPID			! ARD identifier
      INTEGER			IAX			! Axis loop
      INTEGER			MPTR			! ARD mask data
      INTEGER                   NLEV                    ! Useless TRACE output
      INTEGER			NQPTR			! New quality array

      LOGICAL                   OK                      ! Validity checks
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Associate psf
      IF ( IFILE .EQ. 1 ) THEN
        CALL PSF_ASSOCI( IM_ID, PSF_HAN, STATUS )
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
      BDS_QUAL_DYNAMIC = .FALSE.
      CALL BDI_CHK( IM_ID, 'Quality', BDS_QUAL_OK, STATUS )
      IF ( BDS_QUAL_OK ) THEN
        CALL BDI_MAPUB( IM_ID, 'MaskedQuality', 'READ',
     :                  BDS_QUAL_PTR, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check axes
      CALL PSS_GET_AXES( STATUS )

*  Extra quality mask?
      CALL USI_GET0C( 'SREGION', SREGION, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Load the ARD file
        CALL ARX_OPEN( 'READ', GRPID, STATUS )
        CALL ARX_READ( 'SREGION', GRPID, STATUS )

*    Convert to a mask
        CALL DYN_MAPI( 2, BDS_DIMS, MPTR, STATUS )
        DO IAX = 1, 2
          BASE(IAX) = AX_BR(IAX) / AX_TOR(IAX)
          SCALE(IAX) = AX_DR(IAX) / AX_TOR(IAX)
        END DO
        CALL ARX_MASK( GRPID, BDS_DIMS, BASE, SCALE, AX_UNITS,
     :                 %VAL(MPTR), STATUS )

*    Make new quality array, marking as dynamic
        CALL DYN_MAPB( BDS_NDIM, BDS_DIMS, NQPTR, STATUS )
        BDS_QUAL_DYNAMIC = .TRUE.

*    Copy in dataset quality if present
        IF ( BDS_QUAL_OK ) THEN
          CALL ARR_COP1B( BDS_NELM, %VAL(BDS_QUAL_PTR), %VAL(NQPTR),
     :                    STATUS )
          CALL BDI_UNMAP( IM_ID, 'MaskedQuality', BDS_QUAL_PTR,
     :                    STATUS )
        END IF

*    Add in the masked quality, ORing it if the input data quality is
*    present
        CALL PSS_INP_ADQ( BDS_NELM, BDS_QUAL_OK, %VAL(MPTR),
     :                    %VAL(NQPTR), STATUS )

*    Close the ARD file and its mask
        CALL DYN_UNMAP( MPTR, STATUS )
        CALL ARX_CLOSE( GRPID, STATUS )

*    Quality is now present
        BDS_QUAL_PTR = NQPTR
        BDS_QUAL_OK = .TRUE.

*  No search region supplied?
      ELSE IF ( STATUS .NE. PAR__NULL ) THEN
        CALL ERR_ANNUL( STATUS )

      END IF

*  Which way to use quality if present?
      IF ( BDS_QUAL_OK ) THEN
        CALL USI_GET0L( 'QBAD', CP_NOBADQSRC, STATUS )
      END IF

*  See if there's VARIANCE present
      CALL BDI_CHK( IM_ID, 'Variance', IM_VAR_OK, STATUS )
      IF ( IM_VAR_OK ) THEN
        CALL BDI_MAPR( IM_ID, 'Variance', 'READ', IM_VAR_PTR, STATUS )
        CALL PSS_CHK_VAR( BDS_NELM, BDS_QUAL_OK,
     :                    BDS_QUAL_PTR, %VAL(IM_VAR_PTR), STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

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


*+  PSS_INP_ADQ - Add or OR ARD mask data with masked quality array
      SUBROUTINE PSS_INP_ADQ( N, ORIT, MASK, Q, STATUS )
*
*    Description :
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     18 Jul 96 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Import :
*
      INTEGER			N, MASK(*)
      LOGICAL			ORIT
*
*    Import/Export:
*
      BYTE			Q(*)
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      INTEGER			I			! Loop over masks
*-

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  OR the mask?
      IF ( ORIT ) THEN
        DO I = 1, N
          IF ( MASK(I) .LE. 0 ) THEN
            Q(I) = QUAL__BAD
          END IF
        END DO
      ELSE
        DO I = 1, N
          IF ( MASK(I) .GT. 0 ) THEN
            Q(I) = QUAL__GOOD
          ELSE
            Q(I) = QUAL__BAD
          END IF
        END DO
      END IF

      END
