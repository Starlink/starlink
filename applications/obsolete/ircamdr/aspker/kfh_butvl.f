*+  KFH_BUTVL - Returns number of button just pressed.
      SUBROUTINE KFH_BUTVL(BUTTON,STATUS)
*    Description :
*     This routine returns the number of the button pressed
*     on the trackerball box. If no button has been pressed
*     then a zero is returned.
*    Invocation :
*     CALL KFH_BUTVL(BUTTON,STATUS)
*    Parameters :
*     BUTTON = INTEGER
*           The number of the button pressed by the user.
*     STATUS = INTEGER
*           The status value on entry.
*    Method :
*     The trackerball box is sampled to see which button has
*     been pressed. If one has been pressed then the box is
*     reset.
*    Authors :
*     K.F.Hartley (RGVAD::KFH)
*    History :
*     8 August 1983: Original (RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Global Constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER BUTTON			! Number of the button
*					! pressed.
      INTEGER WKID			! GKS work station
*					! identifier.
*-

*
*    If the status is incorrect, then return to the main program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

         BUTTON = 0

         CALL SGS_ICURW(WKID)

         CALL GKS_SMCH(WKID,1,BUTTON)

         IF (BUTTON.NE.0) THEN

            CALL GKS_DSCH(WKID,1)
            CALL GKS_ENCH(WKID,1,2)

         ENDIF

      ENDIF

      END
