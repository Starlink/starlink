*+  KFH_DRCUR - Draws a cursor.
      SUBROUTINE KFH_DRCUR(CURX,CURY,BOXHT,BOXWDT,RADIUS,
     : CURTYP,PEN,STATUS)
*    Description :
*     This routine will draw a cursor in the overlay planes
*     of the ARGS. The cursor can either be a circle or a
*     rectangle, and can either be plotted in a defined pen
*     or unplotted so as to make way for the cursor being
*     plotted in a different position.
*    Invocation :
*     CALL KFH_DRCUR(CURX,CURY,BOXHT,BOXWDT,RADIUS,CURTYP,PEN,STATUS)
*    Parameters :
*     CURX = REAL
*           The x coordinate of the centre of the cursor.
*     CURY = REAL
*           The y coordinate of the centre of the cursor.
*     BOXHT = INTEGER
*           The half height of the rectangular cursor.
*     BOXWDT = INTEGER
*           The half width of the rectangular cursor.
*     RADIUS = INTEGER
*           The radius of the circular cursor.
*     CURTYP = CHAR*1
*           The cursor type - 'C' => circular
*                             'R' => rectangular.
*     PEN = INTEGER
*           The pen with which the cursor is to be drawn.
*           -1 is used to undraw the cursor.
*     STATUS = INTEGER
*           The status value on entry to this subroutine.
*    Method :
*     If the cursor is to be removed then an ARGS routine is
*     called to set up the pens to delete whatever is over-
*     written, otherwise the requested pen is set. The cursor
*     is then drawn using the relevant SGS routine.
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
*    Local Constants :
      INTEGER CLEAR			! The pen number used
      PARAMETER (CLEAR = -1)		! to signify that the
*					! cursor is to be
*					! cleared.
*    Local variables :
      INTEGER BOXHT			! The half height of the
*					! rectangular cursor.
      INTEGER BOXWDT			! The half width of the
*					! rectangular cursor.
      CHARACTER*1 CURTYP		! The cursor type.
      REAL CURX				! The x coordinate of
*					! the cursor.
      REAL CURY				! The y coordinate of
*					! the cursor.
      INTEGER PEN			! The pen with which
*					! the cursor is to be
*					! drawn.
      INTEGER RADIUS			! The radius of the
*					! circular cursor.

*-

*
*    If the status is bad, then return to the calling program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

         IF (PEN.EQ.CLEAR) THEN
            CALL ARGS_S1('ZDI1',0)
         ELSE
            CALL SGS_SPEN(PEN)
         ENDIF

         IF (CURTYP.EQ.'C') THEN
            CALL SGS_CIRCL(CURX,CURY,REAL(RADIUS))
         ELSE
            CALL SGS_BOX(CURX-REAL(BOXWDT),CURX+REAL(BOXWDT),
     :                   CURY-REAL(BOXHT) ,CURY+REAL(BOXHT))
         ENDIF

         CALL SGS_FLUSH

      ENDIF

      END
