*+ PSF_AXAF_INIT.F
      SUBROUTINE PSF_AXAF_INIT(PSID,STATUS)
*
*    Description :
*     Opens PSF cube
*     Reads some PSF details into global constants
*     Prompts user for energy value
*     Initiates the PSF system
*
*    Deficiencies :
*
*    Environment Parameters
*     AUX = Mean photon energy in KeV
*
*    Authors :
*     David Geddes
*
*    History :
*     31 Aug 99 : Original (DGED)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_HCUBE_CMN'        ! Common block and constants

*    Global variables :
*
*    Import :
*
      INTEGER PSID                   ! Handle into PSF system
*
*    Status :
*
      INTEGER STATUS
*
*    Local :
      CHARACTER*132          FNAME   ! File name
      CHARACTER*(DAT__SZLOC) HCLOC   ! Hyper cube locator

      INTEGER CDIMS(3)               ! Cube dimensions
      INTEGER L                      ! ?
      INTEGER LP                     ! Loop
      INTEGER N1                     ! Local integer
      INTEGER NVAL                   ! Number of values
      INTEGER PSF_RAD_X, PSF_RAD_Y   ! Radius of PSF

      REAL    ENERGY                 ! User input energy value
      REAL    ETEMP                  ! Temporary energy value
*
      EXTERNAL  PSF_AXAF_MARX        ! Name of PSF routine
      EXTERNAL  PSF_AXAF_MARX_HINT
*-
*    Locate cube
      CALL AST_PATH('AST_ETC','PSF_HYPER_CUBE','psf_hyper_cube',FNAME,
     :                                                         L,STATUS)
      CALL ADI_FOPEN(FNAME,'BinDS','READ',HX_CB_ID,
     :                                                  STATUS)
      CALL ADI1_GETLOC(HX_CB_ID,HCLOC,STATUS)

*    Abort if failed
      IF (STATUS .NE. SAI__OK) THEN
        CALL ERR_REP(' ','Unable to open AXAF cube',STATUS)
        GOTO 99
      ENDIF

*    Get the cube dimensions
      CALL CMP_MAPN( HCLOC,'LOCATOR_CUBE','_INTEGER','READ',3,
     :                                 HX_CB_RPTR, CDIMS(1),STATUS)
      HX_CB_NRBINX = CDIMS(1)
      HX_CB_NRBINY = CDIMS(2)
      HX_CB_NEBIN  = CDIMS(3)

*    Get PSF radius
      CALL CMP_GET0I(HCLOC, 'PSF_ARRAY_RAD_X', PSF_RAD_X,STATUS)
      CALL CMP_GET0I(HCLOC, 'PSF_ARRAY_RAD_Y', PSF_RAD_Y,STATUS)

*    Calculate array size
      HX_PSF_ARRAY_X =  PSF_RAD_X * 2 + 1
      HX_PSF_ARRAY_Y =  PSF_RAD_Y * 2 + 1

*   Get ENERGY vale
*    Get a mean photon energy
 10   CONTINUE
      CALL USI_PROMT('AUX','Mean photon energy in KeV',STATUS)
      CALL USI_GET0R('AUX',ENERGY,STATUS)

*    Check energy
      IF (ENERGY .LT. AXAF_ENERGY_MIN .OR. ENERGY .GT. AXAF_ENERGY_MAX)
     :                                                              THEN
         CALL ERR_REP('Error - Energy range out of bounds')
         GOTO 99
      ENDIF

*    Read values from the energy range
      CALL CMP_MAPV(HCLOC,'ENERGY_RANGE','_REAL','READ',
     :                           HX_ENERGY_RANGE,HX_CB_NEBIN,STATUS)

*    Energy bins run upto and include the range value.
*    For example using three bin : 1.5 5.5 10.0
*    Bins[1] = 0.0~1.5 Bin[2]=1.51~5.5 Bin[3]=5.51~10.0
      ETEMP=0.0
      HX_LOCATOR_POSE = 0
      DO LP = 1,HX_CB_NEBIN
        IF (ETEMP .LT. ENERGY) THEN
           HX_LOCATOR_POSE =  HX_LOCATOR_POSE + 1
           N1 = HX_LOCATOR_POSE * 4 - 4
           CALL ARR_COP1R(1,%VAL(HX_ENERGY_RANGE + N1),ETEMP,STATUS)
         ENDIF
      END DO

      CALL CMP_UNMAP(HCLOC,'LOCATOR_CUBE',STATUS)

*    Pointer to locator cube
      NVAL =  CDIMS(3) * CDIMS(2) * CDIMS(1)
      CALL CMP_MAPV(HCLOC,'LOCATOR_CUBE','_INTEGER','READ',
     :                                     HX_CB_LPTR,NVAL,STATUS)

*    Pointer to PSF size cube
      NVAL = HX_CB_NRBINX*HX_CB_NRBINY*HX_CB_NEBIN
      CALL CMP_MAPV(HCLOC,'PSF_SIZES','_INTEGER','READ',HX_PSF_PTR,
     :                                                  NVAL,STATUS)

*    Pointer to PSF routine
      CALL PSF0_SETRTN( PSID, 'Data', PSF_AXAF_MARX, STATUS )

*    Would prefer name from the parameter system
      HX_PSF_NAME = 'AXAF_MARX'

 99   IF (STATUS .NE. SAI__OK) THEN
         CALL AST_REXIT('PSF_AXAF_INIT',STATUS)
      ENDIF

      END

*+  PSF_AXAF_MARX_HINT - XRT PSPC psf hint handler
      SUBROUTINE PSF_AXAF_MARX_HINT( PSID, HINT, DATA, STATUS )
*
*    Description :
*
*     Return hints about the AXAF MARX psf.
*
*    Method :
*
*    Deficiencies :
*
*    Authors :
*     David Geddes
*
*    History :
*     31 Aug 99 : Original (DGED)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'MATH_PAR'
*
*    Import :
*
      INTEGER                 	PSID
      CHARACTER*(*)           	HINT		 	! Hint name
*
*    Export :
*
      BYTE			DATA(*)			! Hint data
*
*    Status :
*
      INTEGER                   STATUS
*
*   Local variables:
*
      CHARACTER*3		OPT			! Psf form
*-
*     PRINT *,'CHECKING HINT'

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Radial symmetry?
      IF ( HINT .EQ. PSF_H_RADSYM ) THEN

*    All our models are radially symmetric about on-axis direction
        CALL ARR_COP1L( 1, .TRUE., DATA, STATUS )

*  Position dependent?
      ELSE IF ( HINT .EQ. PSF_H_POSDEP ) THEN

*    Get the psf form
        CALL PSF0_GETID0C( PSID, 'Form', OPT, STATUS )

*    The ONAXIS3 psf isn't position dependent
        CALL ARR_COP1L( 1, (OPT.NE.'ON3'), DATA, STATUS )

*  Energy dependent?
      ELSE IF ( HINT .EQ. PSF_H_ENDEP ) THEN

*    They all vary with energy
        CALL ARR_COP1L( 1, .TRUE., DATA, STATUS )

*  Field size?
      ELSE IF ( HINT .EQ. PSF_H_FLDSIZ ) THEN

*    Write value
        CALL ARR_COP1R( 1,0.1*MATH__DTOR, DATA, STATUS )

      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'H', HINT )
        CALL ERR_REP( ' ', 'Unknown psf hint /^H/', STATUS )

      END IF

      END
