      SUBROUTINE ESP1_S2PR(INDF,XSTR,YSTR,XB,YB,STATUS)
*+
*  Name:
*     ESP1_S2PR
*
*  Purpose:
*     Convert string values to Base coordinates.
*
*  Language:
*     Starlink Fortran 77.
*
*  Invocation:
*     CALL ESP1_S2PR(INDF,XSTR,YSTR,XB,YB,STATUS)
*
*  Description:
*     This routine takes two strings representing X and Y positions in
*     an NDF's Current frame coordinates and returns Base frame
*     coordinates of them.  A check is also made that the point
*     specified lies within the NDF.
*
*     If either of the strings cannot be successfully converted to
*     a number then XB and YB are not meaningful and STATUS is
*     set on exit.  If the point specified is not within the NDF then
*     STATUS is set on exit.
*
*  Arguments:
*     INDF = INTEGER (Given)
*        Identifier for the NDF which the coordinates describe.
*     XSTR = CHARACTER*(*) (Given)
*        String giving the X coordinate.
*     YSTR = CHARACTER*(*) (Given)
*        String giving the Y coordinate.
*     XB = REAL (Returned)
*        The X Base frame coordinate.
*     YB = REAL (Returned)
*        The Y Base frame coordinate.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Notes:
*     Encapsulating the whole transformation in one routine like this
*     keeps things tidy, but it means that the extraction of the WCS
*     component from the NDF and of the mapping from the WCS component
*     may have to be done multiple times, while if it were done in
*     the calling routine it need only be done once.  However, the
*     time taken as a proportion of the overall running time for any
*     current ESP application is always(?) going to be negligable,
*     so this trade-off seems worthwhile.
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     25-OCT-1999 (MBT):
*        Original version.
*-

*  Type definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'AST_PAR'               ! Standard AST system constants
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER INDF
      CHARACTER*(*) XSTR
      CHARACTER*(*) YSTR

*  Arguments Returned:
      REAL XB
      REAL YB

*  Status:
      INTEGER STATUS

*  Local variables:
      INTEGER CFRAME                  ! AST pointer to Current frame
      INTEGER CMAP                    ! AST pointer to conversion mapping
      INTEGER DIM(2)                  ! Dimensions of NDF
      INTEGER IWCS                    ! AST pointer to WCS component frameset
      INTEGER NCHR                    ! Number of characters converted
      INTEGER NDIM                    ! Number of NDF dimensions
      DOUBLE PRECISION XMAX           ! Highest pixel value in X direction
      DOUBLE PRECISION XMIN           ! Highest acceptable value in X direction
      DOUBLE PRECISION YMAX           ! Highest pixel value in Y direction
      DOUBLE PRECISION YMIN           ! Highest acceptable value in Y direction
      DOUBLE PRECISION X1             ! Intermediate converted X value
      DOUBLE PRECISION X2             ! Intermediate converted X value
      DOUBLE PRECISION Y1             ! Intermediate converted Y value
      DOUBLE PRECISION Y2             ! Intermediate converted Y value
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Enter AST context.
      CALL AST_BEGIN(STATUS)

*   Get the bounds of the NDF.
      CALL NDF_DIM(INDF,2,DIM,NDIM,STATUS)
      XMIN=1D0
      YMIN=1D0
      XMAX=DBLE(DIM(1))
      YMAX=DBLE(DIM(2))


*   Get the World Coordinate System component of the NDF.
      CALL NDF_GTWCS(INDF,IWCS,STATUS)

*   Get Current frame of WCS component.
      CFRAME=AST_GETFRAME(IWCS,AST__CURRENT,STATUS)

*   Get numeric values in the Current frame for the X coordinate
*   according to the kind of coordinate it is.  Check for
*   conversion errors.
      NCHR=AST_UNFORMAT(CFRAME,1,XSTR,X1,STATUS)
      IF (XSTR(NCHR+1:).NE.' '.OR.NCHR.EQ.0.OR.STATUS.NE.SAI__OK) THEN
         STATUS=SAI__ERROR
         CALL MSG_SETC('XSTR',XSTR)
         CALL ERR_REP(' ','X coordinate string "^XSTR" invalid.',STATUS)
         GO TO 99
      END IF

*   Do the same for the Y coordinate.
      NCHR=AST_UNFORMAT(CFRAME,2,YSTR,Y1,STATUS)
      IF (YSTR(NCHR+1:).NE.' '.OR.NCHR.EQ.0.OR.STATUS.NE.SAI__OK) THEN
         STATUS=SAI__ERROR
         CALL MSG_SETC('YSTR',YSTR)
         CALL ERR_REP(' ','Y coordinate string "^YSTR" invalid.',STATUS)
         GO TO 99
      END IF

*   Get the mapping between the Current frame and the Base frame.
*   The Base frame of a WCS component is the one in the GRID domain,
*   which has values of unity in the centre of the first pixel
*   stored.
      CMAP=AST_GETMAPPING(IWCS,AST__CURRENT,AST__BASE,STATUS)

*   Transform from Current frame to Base frame coordinates.
      CALL AST_TRAN2(CMAP,1,X1,Y1,.TRUE.,X2,Y2,STATUS)


      IF (STATUS.EQ.SAI__OK) THEN
*      If something went wrong with the conversion, then
*      skip the check below, since it may just (further) confuse things.

*      Check that the coordinates given fall within the bounds of the NDF.
*      Note this check restricts the coordinates to be between the centre
*      of the lowest pixel and the centre of the highest pixel in each
*      dimension, i.e. not within half a pixel of the edge of the array.
*      It's not clear to me (MBT) that this is correct, but I'm
*      re-implementing the constraints in the same way that they have been
*      previously operating within ESP.
         IF (X2.LT.XMIN.OR.X2.GT.XMAX.OR.Y2.LT.YMIN.OR.Y2.GT.YMAX) THEN
            STATUS=SAI__ERROR
            CALL MSG_SETD ('X', X2)
            CALL MSG_SETD ('Y', Y2)
            CALL MSG_SETD ('XMIN', XMIN)
            CALL MSG_SETD ('XMAX', XMAX)
            CALL MSG_SETD ('YMIN', YMIN)
            CALL MSG_SETD ('YMAX', YMAX)
            CALL ERR_REP(' ','converted coords (^X,^Y)'//
     :           ' outside NDF (^XMIN,^YMIN)..(^XMAX,^YMAX).',
     :           STATUS)
            GO TO 99
         END IF

*      Do type conversions.
         XB=REAL(X2)
         YB=REAL(Y2)
      ENDIF

*   Error exit label.
 99   CONTINUE

*   Exit AST context.
      CALL AST_END(STATUS)

*   Return.
      END

