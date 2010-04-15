      SUBROUTINE ESP1_XYFMT(FSET,XB,YB,XMSG,YMSG,DMNMSG,STATUS)
*+
*  Name:
*     ESP1_XYFMT
*
*  Purpose:
*     Format a position in Current coordinates.
*
*  Language:
*     Starlink Fortran 77.
*
*  Invocation:
*     CALL ESP1_XYFMT(FSET,XB,YB,XMSG,YMSG,DMNMSG,STATUS)
*
*  Description:
*     This routine takes positions in the Base frame and formats them
*     for output as Current coordinates.
*
*  Arguments:
*     FSET = INTEGER (Given)
*        AST pointer to the frameset describing the coordinates.
*     XB = REAL (Given)
*        X position in Base coordinates of the frameset FSET.
*     YB = REAL (Given)
*        Y position in Base coordinates of the frameset FSET.
*     XMSG = CHARACTER*(*) (Given)
*        Name of a MSG system token to hold the formatted X value.
*     YMSG = CHARACTER*(*) (Given)
*        Name of a MSG system token to hold the formatted Y value.
*     DMNMSG = CHARACTER*(*) (Given)
*        Name of a MSG system token to hold the Current Domain name.
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
      CHARACTER*(*) XMSG
      CHARACTER*(*) YMSG
      CHARACTER*(*) DMNMSG
      INTEGER FSET
      REAL XB
      REAL YB

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  Local variables:
      DOUBLE PRECISION X1             ! XB in double precision
      DOUBLE PRECISION XC             ! X coord in Current frame
      DOUBLE PRECISION Y1             ! YB in double precision
      DOUBLE PRECISION YC             ! Y coord in Current frame
      INTEGER MAP                     ! AST pointer to mapping Base->Current
      INTEGER FRAME                   ! AST pointer to Current frame

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Convert input values to double precision, required for AST
*   transformation.
      X1=DBLE(XB)
      Y1=DBLE(YB)

*   Convert Base coordinates to Current coordinates.
      MAP=AST_GETMAPPING(FSET,AST__BASE,AST__CURRENT,STATUS)
      CALL AST_TRAN2(MAP,1,X1,Y1,.TRUE.,XC,YC,STATUS)

*   Format Current coordinates.
      FRAME=AST_GETFRAME(FSET,AST__CURRENT,STATUS)
      CALL MSG_SETC(XMSG,AST_FORMAT(FRAME,1,XC,STATUS))
      CALL MSG_SETC(YMSG,AST_FORMAT(FRAME,2,YC,STATUS))

*   Get domain of Current frame of frameset.
      CALL MSG_SETC(DMNMSG,AST_GETC(FRAME,'Domain',STATUS))

*   Error exit label.
 99   CONTINUE

*   Tidy up AST objects.
      CALL AST_ANNUL(MAP,STATUS)
      CALL AST_ANNUL(FRAME,STATUS)

*   Return.
      END

