

*+  KFH_SCALE - Draws a block using all pens in sequence.
      SUBROUTINE KFH_SCALE
*    Description :
*     This routine draws a rectangular block on the ARGS
*     consisting of vertical lines drawn next to each other
*     one by each pen.
*    Invocation :
*     CALL KFH_SCALE
*    Method :
*     An array (BOX) is loaded with the rquired pen numbers,
*     and then a call is made to the GKS routine for displaying
*     an array of data.
*    Deficiencies :
*     None known.
*    Authors :
*     A.P.Horsfield (RGVAD::KFH)
*    History :
*     18 July 1983: Original (RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Local variables :
      INTEGER BOX(0:511,20)		! Array containing the
*					! image to be plotted.
      INTEGER I				! General variable.
      INTEGER J				! General variable.
*-

*
*    A rectangle of sides 512 by 512 is set up with the pen
*    number increasing monotonically with x coordinate from
*    0 to 255, and being independent of the y-coordinate.
*

      DO J =1,20,1
         DO I = 0,255,1
            BOX(I+I,J) = I
            BOX(I+I+1,J) = I
         ENDDO
      ENDDO

*
*    The rectangle is now displayed on the ARGS.
*

      CALL GKS_PXA(0.0,300.0,511.0,319.0,512,20,BOX)

*
*    All buffers are emptied.
*

      CALL SGS_FLUSH

      END
