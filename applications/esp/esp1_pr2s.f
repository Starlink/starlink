      SUBROUTINE ESP1_PR2S(COSYS,INDF,XPIX,YPIX,XSTR,YSTR,XLEN,YLEN,
     :                     STATUS)
*+
*  Name:
*     ESP1_PR2S
*
*  Purpose:
*     Convert pixel coordinates to string values.
*
*  Language:
*     Starlink Fortran 77.
*
*  Invocation:
*     CALL ESP1_PR2S(COSYS,INDF,XPIX,YPIX,XSTR,YSTR,XLEN,YLEN,STATUS)
*
*  Description:
*     This routine takes two real values givinf X and Y pixel co-ordinates
*     in an NDF and returns strings representing them.  How the
*     transformation is done depends on the value of the COSYS 
*     argument.
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
*     XPIX = REAL (Given)
*        The X pixel coordinate.
*     YPIX = REAL (Given)
*        The Y pixel coordinate.
*     XSTR = CHARACTER*(*) (Returned)
*        String giving the X coordinate.
*     YSTR = CHARACTER*(*) (Returned)
*        String giving the Y coordinate.
*     XLEN = INTEGER (Returned)
*        Length of the non-blank part of XSTR.
*     YLEN = INTEGER (Returned)
*        Lenght of the non-blank part of YSTR.
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
*     26-OCT-1999 (MBT):
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
      REAL XPIX
      REAL YPIX

*  Arguments Returned:
      CHARACTER*(*) XSTR
      CHARACTER*(*) YSTR

*  Status:
      INTEGER STATUS

*  Local variables:
      INTEGER CFRAME                  ! AST pointer to Current frame
      INTEGER CMAP                    ! AST pointer to conversion mapping
      INTEGER IWCS                    ! AST pointer to WCS component frameset
      INTEGER LBND(2)                 ! Lower bounds of NDF dimensions
      INTEGER NDIM                    ! Number of NDF dimensions
      INTEGER UBND(2)                 ! Upper bounds of NDF dimensions
      INTEGER XLEN                    ! Length of converted X string
      INTEGER YLEN                    ! Length of converted Y string
      DOUBLE PRECISION X1             ! Intermediate converted X value
      DOUBLE PRECISION X2             ! Intermediate converted X value
      DOUBLE PRECISION Y1             ! Intermediate converted Y value
      DOUBLE PRECISION Y2             ! Intermediate converted Y value
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Do type conversions.
      X1=DBLE(XPIX)
      Y1=DBLE(YPIX)

*   If we have coordinates in the current frame then interpret them 
*   according to the frameset they come from.
      IF (COSYS(1:1).EQ.'C') THEN

*      Enter AST context.
         CALL AST_BEGIN(STATUS)

*      Get the World Coordinate System component of the NDF.
         CALL NDF_GTWCS(INDF,IWCS,STATUS)

*      Get the mapping between the Current frame and the Base frame.
*      The Base frame of a WCS component is the one in the GRID domain,
*      which has values of unity in the centre of the first pixel 
*      stored.
         CMAP=AST_GETMAPPING(IWCS,AST__CURRENT,AST__BASE,STATUS)

*      Get Current frame of WCS component.
         CFRAME=AST_GETFRAME(IWCS,AST__CURRENT,STATUS)

*      Transform from the Base to the Current coordinates.
         CALL AST_TRAN2(CMAP,1,X1,Y1,.FALSE.,X2,Y2,STATUS)

*      Format the coordinates we have now obtained for the axis in question.
         XSTR=AST_FORMAT(CFRAME,1,X2,STATUS)
         YSTR=AST_FORMAT(CFRAME,2,Y2,STATUS)

*      Get string lengths.
         XLEN=LEN(XSTR)
         YLEN=LEN(YSTR)

*      Exit AST context.
         CALL AST_END(STATUS)

*   Otherwise, we have pixel-like coordinates.
      ELSE

*      Add base value for World coordinates, Data coordinates are
*      right already.
         IF (COSYS(1:1).EQ.'W') THEN

*         Get the bounds of the NDF.
            CALL NDF_BOUND(INDF,2,LBND,UBND,NDIM,STATUS)

*         Shift values appropriately.
            X2=X1+DBLE(LBND(1))-1D0
            Y2=Y1+DBLE(LBND(2))-1D0
         ELSE
            X2=X1
            Y2=Y1
         END IF

*      Format the X value for output.
         CALL MSG_FMTD('X','F8.1',X2)
         CALL MSG_LOAD(' ','^X',XSTR,XLEN,STATUS)

*      Format the Y value for output.
         CALL MSG_FMTD('Y','F8.1',Y2)
         CALL MSG_LOAD(' ','^Y',YSTR,YLEN,STATUS)

      END IF

*   Return.
      END

