*+  KPG1_CROSS - Draws a cross at the specified pixel position.

      SUBROUTINE KPG1_CROSS( X, Y, SIZE, STATUS )
*
*    Description :
*
*     This routine accepts the co-ordinates of a point and draws a
*     cross of defined size centred at that point.  The SGS buffers
*     are flushed after the cross is drawn.
*
*     An SGS device must already be open.
*
*    Invocation :
*
*     CALL KPG1_CROSS( X, Y, SIZE, STATUS )
*
*    Arguments :
*
*     X = REAL( READ )
*         The X co-ordinate of the point.
*     Y = REAL( READ )
*         The Y co-ordinate of the point.
*     SIZE = REAL( READ )
*         The width and height of the cross in pixels
*     STATUS = INTEGER( READ )
*         The status on entry to this subroutine.
*
*    Method :
*
*     The co-ordinates of the ends of the arms of the cross are
*     calculated, then the cross is plotted.
*
*    Authors :
*
*     S.Chan ( RGVAD::KFH )
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     26 October 1983:  Original
*     1986 Sep 17 : Renamed from KFH_CROSS. Standardised to RAPI2D
*                   style; renamed parameters section to arguments and
*                   added access; relocated 'local' variables to
*                   import etc.; and tidied (RL.STAR::CUR).
*     1988 Sep 12 : Added SIZE argument and fixed typing bug
*                   (RL.STAR::CUR).
*     1991 Jun 18 : Renamed from CROSS (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT NONE            ! no default typing allowed

*    Global Constants :

      INCLUDE 'SAE_PAR'        ! SSE global definitions

*    Import :

      REAL
     :  X, Y,
     :  SIZE

*    Status :

      INTEGER STATUS

*    Local variables :

      REAL
     :  HSIZ,                  ! size of an arm of the cross
     :  LX,                    ! X co-ordinate - left arm of the cross
     :  RX,                    ! X co-ordinate - right arm of the cross
     :  TY,                    ! Y co-ordinate - upper arm of the cross
     :  BY                     ! Y co-ordinate - lower arm of the cross
*-

*    If the status is bad on entry, then return to the main program.

      IF ( STATUS .NE. SAI__OK ) RETURN

      HSIZ = 0.5 * SIZE

*    Calculate the extent of the cross.

      LX = X - HSIZ
      RX = X + HSIZ
      TY = Y + HSIZ
      BY = Y - HSIZ

*    Plot the cross...

      CALL SGS_LINE( LX, Y, RX, Y )
      CALL SGS_LINE( X, TY, X, BY )

*    ... now

      CALL SGS_FLUSH

      END
