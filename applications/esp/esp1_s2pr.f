      SUBROUTINE ESP1_S2PR(COSYS,INDF,XSTR,YSTR,XPIX,YPIX,STATUS)
*+
*  Name:
*     ESP1_S2PR
*
*  Purpose:
*     Convert string values to pixel coordinates.
*
*  Language:
*     Starlink Fortran 77.
*
*  Invocation:
*     CALL ESP1_S2PR(COSYS,INDF,XSTR,YSTR,XPIX,YPIX,STATUS)
*
*  Description:
*     This routine takes two strings representing X and Y positions in
*     an NDF and returns the pixel coordinates of them.  How the
*     transformation is done depends on the value of the COSYS argument.
*     A check is also made that the point specified lies within the 
*     NDF.
*
*     If either of the strings cannot be successfully converted to
*     a number then XPIX and YPIX are not meaningful and STATUS is 
*     set on exit.  If the point specified is not within the NDF then
*     STATUS is set on exit.
*
*  Arguments:
*     COSYS = CHARACTER*(*) (Given)
*        Option defining how the strings supply the coordinates.  Only
*        the first character is significant.  'W' means World, 'D' means
*        Data, and 'C' means Current coordinate frame (in the WCS 
*        component of the NDF).  In fact any value which is not one
*        of these three is interpreted the same as 'D'.
*     INDF = INTEGER (Given)
*        Identifier for the NDF which the coordinates describe.
*     XSTR = CHARACTER*(*) (Given)
*        String giving the X coordinate.
*     YSTR = CHARACTER*(*) (Given)
*        String giving the Y coordinate.
*     XPIX = REAL (Returned)
*        The X pixel coordinate.
*     YPIX = REAL (Returned)
*        The Y pixel coordinate.
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
*     so this trade-off was judged worthwhile.
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
      CHARACTER*(*) COSYS
      INTEGER INDF
      CHARACTER*(*) XSTR
      CHARACTER*(*) YSTR

*  Arguments Returned:
      REAL XPIX
      REAL YPIX

*  Status:
      INTEGER STATUS

*  Local variables:
      INTEGER CFRAME                  ! AST pointer to Current frame
      INTEGER CMAP                    ! AST pointer to conversion mapping
      INTEGER IWCS                    ! AST pointer to WCS component frameset
      INTEGER LBND(2)                 ! Lower bounds of NDF dimensions
      INTEGER NCHR                    ! Number of characters converted
      INTEGER NDIM                    ! Number of NDF dimensions
      INTEGER UBND(2)                 ! Upper bounds of NDF dimensions
      DOUBLE PRECISION XMAX           ! Highest pixel value in X direction
      DOUBLE PRECISION YMAX           ! Highest pixel value in Y direction
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
      CALL NDF_BOUND(INDF,2,LBND,UBND,NDIM,STATUS)
      XMAX=DBLE(UBND(1)-LBND(1)+1)
      YMAX=DBLE(UBND(2)-LBND(2)+1)

*   If we have coordinates in the current frame then interpret them 
*   according to the frameset they come from.
      IF (COSYS(1:1).EQ.'C') THEN

*      Get the World Coordinate System component of the NDF.
         CALL NDF_GTWCS(INDF,IWCS,STATUS)

*      Get Current frame of WCS component.
         CFRAME=AST_GETFRAME(IWCS,AST__CURRENT,STATUS)

*      Get numeric values in the Current frame for the X coordinate
*      according to the kind of coordinate it is.  Check for 
*      conversion errors.
         NCHR=AST_UNFORMAT(CFRAME,1,XSTR,X1,STATUS)
         IF (XSTR(NCHR+1:).NE.' '.OR.NCHR.EQ.0.OR.STATUS.NE.SAI__OK)
     :      THEN
            STATUS=SAI__ERROR
            CALL MSG_SETC('XSTR',XSTR)
            CALL ERR_REP(' ','X coordinate string "^XSTR" invalid.',
     :                   STATUS)
            GO TO 99
         END IF

*      Do the same for the Y coordinate.
         NCHR=AST_UNFORMAT(CFRAME,2,YSTR,Y1,STATUS)
         IF (YSTR(NCHR+1:).NE.' '.OR.NCHR.EQ.0.OR.STATUS.NE.SAI__OK)
     :      THEN
            STATUS=SAI__ERROR
            CALL MSG_SETC('YSTR',YSTR)
            CALL ERR_REP(' ','Y coordinate string "^YSTR" invalid.',
     :                   STATUS)
            GO TO 99
         END IF

*      Get the mapping between the Current frame and the Base frame.
*      The Base frame of a WCS component is the one in the GRID domain,
*      which has values of unity in the centre of the first pixel 
*      stored.
         CMAP=AST_GETMAPPING(IWCS,AST__CURRENT,AST__BASE,STATUS)

*      Transform from the Current to the Base coordinates.
         CALL AST_TRAN2(CMAP,1,X1,Y1,.TRUE.,X2,Y2,STATUS)

*   Otherwise, we have pixel-like coordinates.  Do a simple conversion
*   from strings to numbers and adjust if necessary.
      ELSE

*      Turn the X string straight into a number, checking for conversion 
*      errors.
         CALL CHR_CTOD(XSTR,X1,STATUS)
         IF (STATUS.NE.SAI__OK) THEN
            CALL MSG_SETC('XSTR',XSTR)
            CALL ERR_REP(' ','X coordinate string "^XSTR" invalid.',
     :                   STATUS)
            GO TO 99
         END IF

*      Do the same for the Y coordinate.
         CALL CHR_CTOD(YSTR,Y1,STATUS)
         IF (STATUS.NE.SAI__OK) THEN
            CALL MSG_SETC('YSTR',YSTR)
            CALL ERR_REP(' ','Y coordinate string "^YSTR" invalid.',
     :                   STATUS)
            GO TO 99
         END IF

*      Subtract base value for World coordinates, Data coordinates are
*      right already.
         IF (COSYS(1:1).EQ.'W') THEN
            X2=X1-REAL(LBND(1))+1D0
            Y2=Y1-REAL(LBND(2))+1D0
         ELSE
            X2=X1
            Y2=Y1
         END IF
      END IF

*   Check that the coordinates given fall within the bounds of the NDF.
*   Note this check restricts the coordinates to be between the centre
*   of the lowest pixel and the centre of the highest pixel in each 
*   dimension, i.e. not within half a pixel of the edge of the array.
*   It's not clear to me (MBT) that this is correct, but I'm
*   re-implementing the constraints in the same way that they have been 
*   previously operating within ESP.
      IF (X2.LT.1D0.OR.X2.GT.XMAX.OR.Y2.LT.1D0.OR.Y2.GT.YMAX)
     :   THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','Given coordinates are outside the NDF.',
     :                STATUS)
         GO TO 99
      END IF

*   Do type conversions.
      XPIX=REAL(X2)
      YPIX=REAL(Y2)
 
*   Error exit label.
 99   CONTINUE

*   Exit AST context.
      CALL AST_END(STATUS)

*   Return.
      END

