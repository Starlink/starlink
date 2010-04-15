      SUBROUTINE ESP1_CRPT(FSET,XB,YB,STATUS)
*+
*  Name:
*     ESP1_CRPT
*
*  Purpose:
*     Report to the user the position of the cursor.
*
*  Language:
*     Starlink Fortran 77.
*
*  Invocation:
*     CALL ESP1_CRPT(FSET,XB,YB,STATUS)
*
*  Description:
*     This routine reports to the user the position of the cursor in
*     coordinates of the Current coordinate frame of the given frameset.
*
*  Arguments:
*     FSET = INTEGER (Given)
*        AST pointer to the frameset describing the coordinates.
*     XB = REAL (Given)
*        X position in Base coordinates of the frameset FSET.
*     YB = REAL (Given)
*        Y position in Base coordinates of the frameset FSET.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     9-NOV-1999 (MBT):
*        Original version.
*-

*  Type definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'AST_PAR'               ! Standard AST constants

*  Arguments Given:
      INTEGER FSET
      REAL XB
      REAL YB

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  Local variables:
      INTEGER FRAME                   ! AST pointer to Current frame

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Load Current coordinate values and Domain into MSG tokens.
      CALL ESP1_XYFMT(FSET,XB,YB,'X','Y','DOMAIN',STATUS)

*   Get coordinate symbol names.
      FRAME=AST_GETFRAME(FSET,AST__CURRENT,STATUS)
      CALL MSG_SETC('XSYMBOL',AST_GETC(FRAME,'Symbol(1)',STATUS))
      CALL MSG_SETC('YSYMBOL',AST_GETC(FRAME,'Symbol(2)',STATUS))
      CALL AST_ANNUL(FRAME,STATUS)

*   Output coordinates to user.
      CALL MSG_OUT(' ','  ^DOMAIN frame co-ordinates:  '//
     :             '^XSYMBOL = ^X,  ^YSYMBOL = ^Y', STATUS)

*   Ensure graphics/text synchronisation.
      CALL MSG_SYNC(STATUS)

*   Return.
      END

