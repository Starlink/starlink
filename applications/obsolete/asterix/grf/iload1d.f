*+  ILOAD1D - load external 1D data into image processing system
      SUBROUTINE ILOAD1D(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     rjv@star.sr.bham.ac.uk
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*132		IFILE			! Input file name
      INTEGER IFID
      LOGICAL GCB
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ILOAD1D Version 2.1-0b')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing not active')
      ELSE


*  get input image
        CALL USI_GET0C( 'INP', IFILE, STATUS )
        CALL ADI_FOPEN( IFILE, 'BinDS', 'READ', IFID, STATUS )

*  see if GCB wanted
        CALL USI_GET0L('GCB',GCB,STATUS)

        IF (STATUS.EQ.SAI__OK) THEN

          CALL MSG_PRNT(' ')
          CALL MSG_PRNT('Checking data...')
          CALL IMG_CHECK1D(IFID,STATUS)

          IF (STATUS.EQ.SAI__OK) THEN
            CALL MSG_PRNT('Loading data....')
            CALL IMG_LOAD1D(IFID,STATUS)

            CALL GFX_DEF1DWND(I_N_1D,%val(I_APTR_1D),%val(I_DPTR_1D),
     :                        I_X1_1D,I_X2_1D,I_Y1_1D,I_Y2_1D,STATUS)

            CALL GCB_ATTACH('IMAGE',STATUS)
            CALL IMG_1DGCB(STATUS)

            IF (GCB) THEN
              CALL GCB_FLOAD(IFID,STATUS)
            ELSE
              CALL GCB_CLEAR(STATUS)
            ENDIF

            CALL GCB_CACHE(I_CACHE_1D,STATUS)

*  make sure GCB caching is synchronised
            IF (I_DISP) THEN
              CALL IMG_2DGCB(STATUS)
            ENDIF
          ENDIF

        ENDIF

        CALL ADI_FCLOSE(IFID,STATUS)
        CALL USI_ANNUL('INP',STATUS)

      ENDIF

      CALL USI_CLOSE()

      END
