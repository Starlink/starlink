*+  ICHECK - perform pre-checking on data file
      SUBROUTINE ICHECK(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*     13 Mar 97 : V2.1-0 Original (RJV)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
      LOGICAL STR_SUB,STR_ABBREV
*    Local constants :
*    Local variables :
      CHARACTER*132		IFILE			! Input file name
      CHARACTER*80 LBL
      INTEGER IX,IY,IZ
      INTEGER I
      INTEGER NX,NY,NZ
      INTEGER MX,MY
      INTEGER IFID
      INTEGER NDIM,DIMS(ADI__MXDIM)
      INTEGER ZPTR
      REAL ZMIN,ZMAX
      LOGICAL CUBE
      LOGICAL DOK
*-
*  first invocation do global initialisation
      IF (.NOT.I_OPEN) THEN
        CALL AST_INIT()
      ELSE
        CALL USI_INIT()
      ENDIF

*  default is not a cube
      CUBE=.FALSE.
      CALL IMG_NBPUT0C('OPTIONS',' ',STATUS)

*  get input image
      CALL USI_GET0C( 'INP', IFILE, STATUS )
      CALL ADI_FOPEN( IFILE, 'BinDS', 'READ', IFID, STATUS )


      IF (STATUS.EQ.SAI__OK) THEN

*  check validity of data array
        CALL BDI_GETSHP( IFID, ADI__MXDIM, DIMS, NDIM, STATUS )
        CALL BDI_CHK( IFID, 'Data', DOK, STATUS )

*  only concerned here to determine if dataset is cube
        IF (STATUS.EQ.SAI__OK.AND.DOK.AND.NDIM.EQ.3) THEN


*  get axis labels and try and identify
          IX=0
          IY=0
          IZ=0
          DO I=1,3
            CALL BDI_AXGET0C(IFID,I,'Label',LBL,STATUS)
            IF (STR_ABBREV('X',LBL).OR.STR_SUB('X_',LBL)) THEN
              IX=I
            ELSEIF (STR_ABBREV('Y',LBL).OR.STR_SUB('Y_',LBL)) THEN
              IY=I
            ENDIF
          ENDDO

*  check for acceptable axis ordering
          IF (IX.EQ.1.AND.IY.EQ.2) THEN
            IZ=3
            NZ=DIMS(3)
            CUBE=.TRUE.
          ELSEIF (IX.EQ.2.AND.IY.EQ.3) THEN
            IZ=1
            NZ=DIMS(1)
            CUBE=.TRUE.
          ELSE
            IF(IX.EQ.0.OR.IY.EQ.0) THEN
              CALL MSG_PRNT('AST_ERR: cannot identify spacial axes')
            ELSE
              CALL MSG_PRNT('AST_ERR: axes are in weird order')
              CALL MSG_PRNT('         must be: x,y,*  or *,x,y')
            ENDIF
            STATUS=SAI__ERROR
          ENDIF


        ENDIF

*  if cube then pass a few statistics back to GUI
        IF (CUBE) THEN

          CALL MSG_PRNT('Dataset is a cube')

*  map z-axis and find range of values
          CALL BDI_AXMAPR( IFID,IZ,'Data','READ',ZPTR,STATUS )
          CALL ARR_RANG1R(NZ,%val(ZPTR),ZMIN,ZMAX,STATUS)
          CALL BDI_AXUNMAP(IFID,IZ,'Data',ZPTR,STATUS)

          CALL IMG_NBPUT0C('OPTIONS','CUBE',STATUS)
          CALL IMG_NBPUT0R('PAR_R1',ZMIN,STATUS)
          CALL IMG_NBPUT0R('PAR_R2',ZMAX,STATUS)
          CALL IMG_NBPUT0I('PAR_I1',1,STATUS)
          CALL IMG_NBPUT0I('PAR_I2',NZ,STATUS)

        ENDIF



        CALL ADI_FCLOSE( IFID, STATUS )
        CALL ADI_ERASE(IFID,STATUS)


      ENDIF

      CALL USI_CLOSE()

      END
