*+  ISAVE - save current image to file
      SUBROUTINE ISAVE(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    21 May 91 : V1.2-1 saves contour levels (RJV)
*     5 Jun 91 : V1.2-2 saves only currently selected region (RJV)
*    20 Jan 93 : V1.7-0 save GRAFIX control stuff (RJV)
*    20 Sep 95 : V2.0-0 ADI port (DJA)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER			OFID,BID,DIMS(2)
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ISAVE Version 2.1-0b')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')

      ELSE

        DIMS(1)=I_IX2-I_IX1+1
        DIMS(2)=I_IY2-I_IY1+1
        CALL BDI_NEW( 'XYimage', 2, DIMS, 'REAL', BID, STATUS )
        CALL USI_CREAT('OUT',BID,OFID,STATUS)

        IF (STATUS.EQ.SAI__OK) THEN
          CALL GCB_ATTACH('IMAGE',STATUS)
          CALL IMG_2DGCB(STATUS)

          CALL MSG_PRNT(' ')
          CALL MSG_PRNT('Saving image...')
          CALL IMG_SAVE(OFID,STATUS)
        ENDIF

        CALL USI_CANCL('OUT',STATUS)

      ENDIF

      CALL USI_CLOSE()

      END
