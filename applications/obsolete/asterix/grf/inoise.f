*+  INOISE - adds gaussian noise to image
	SUBROUTINE INOISE(STATUS)
*
* Description:
* Method:
*
* Environment parameters :
*    SDEV         REAL         Standard deviation of noise to add
* Deficiencies :
* Bugs :
* Authors :
*	Richard Saxton 	1989 Mar 13
* History :
*      16 Sep 94 : V1.2-1 updates data min/max
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
* Global variables :
        INCLUDE 'IMG_CMN'
* Status :
      INTEGER STATUS
* Function declarations :
* Local constants :
* Local variables :
      REAL SD                                 !Standard deviation of noise
      LOGICAL UPDATE
* Local data :
* Version :
        CHARACTER*30 VERSION
        PARAMETER (VERSION = 'INOISE Version 1.2-1')
*
      CALL USI_INIT()

* Write version number
      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not open')
      ELSE
*
*
* Get standard deviation of noise.
	CALL USI_GET0R('SDEV', SD, STATUS)

*
        CALL IMG_COPY(STATUS)
        I_CAN_UNDO=.FALSE.

* Add gausian noise
        CALL INOISE_DOIT(SD,%VAL(I_DPTR_W),STATUS)

        CALL IMG_SWAP(STATUS)
        CALL IMG_MINMAX(STATUS)

        IF (STATUS.EQ.SAI__OK) THEN
          I_PROC_COUNT=I_PROC_COUNT+1
          I_CAN_UNDO=.TRUE.
          I_LAST_CMD='INOISE'

        ENDIF

      ENDIF

      CALL USI_CLOSE()

      END


        SUBROUTINE INOISE_DOIT(SD,IM,STATUS)
*
* Description:
*   Generates a random number from a gaussian distribution with a certain
*   standard deviation for each element of the data array.
*
*-Author:    Richard Saxton    1989-Mar-13
*
        IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

* Global variables :
        INCLUDE 'IMG_CMN'
*
* Import:
        REAL SD                              !Standard deviation of noise
*
* Import/export:
        REAL IM(I_NX,I_NY)
* Status:
        INTEGER STATUS
* Functions:
        REAL SLA_GRESID
* Local:
        INTEGER I,J
*
*-
      IF (STATUS.EQ.SAI__OK) THEN


        DO J=I_IY1,I_IY2
          DO I=I_IX1,I_IX2

*  add noise
              IM(I,J)=IM(I,J)+SLA_GRESID(SD)

*  update max and min
              IF (IM(I,J).LT.I_DMIN_W) THEN
                I_DMIN_W=IM(I,J)
              ENDIF
              IF (IM(I,J).GT.I_DMAX_W) THEN
                I_DMAX_W=IM(I,J)
              ENDIF

          ENDDO
        ENDDO

      ENDIF

      END
