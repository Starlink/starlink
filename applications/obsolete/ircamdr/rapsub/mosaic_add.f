
*+  MOSAIC_ADD - adds an image to a new array and records the overlap

      SUBROUTINE MOSAIC_ADD ( IN, IDIMSX, IDIMSY, XOFFSET, YOFFSET, OUT,
     :                        MASK, ODIMSX, ODIMSY, OVERLAP, STATUS)

*    Description :
*
*     This routine adds an image into a (usually larger) ouput image,
*     and is used in mosaicing arrays together. The offset of the
*     small array relative to the large one is given. For each pixel
*     of the small array, the value of that pixel is added to the
*     input value of the corresponding pixel in the big array, but
*     in a weighted fashion according to the number of previous
*     additions to that pixel, as recorded in the mask array. If a
*     successful addition takes place for a particular pixel, the
*     corresponding pixel in the mask array is incremented by 1 to
*     record this fact.
*
*    Invocation :
*
*     CALL MOSAIC_ADD ( IN, IDIMS, XOFFSET, YOFFSET, OUT, MASK, ODIMS,
*                       OVERLAP, STATUS )
*
*    Parameters :
*
*     IN( IDIMS( 1 ), IDIMS( 2 ) )  =  REAL( READ )
*          Input old image
*     IDIMS( 2 )  =  INTEGER( READ )
*          Dimensions of input image
*     XOFFSET  =  INTEGER( READ )
*          x offset of image from bottom left
*     YOFFSET  =  INTEGER( READ )
*          y offset of image from bottom left
*     OUT( ODIMS( 1 ), ODIMS( 2 ) )  =  REAL( UPDATE )
*          Data array containing merged image
*     MASK( ODIMS( 1 ), ODIMS( 2 ) )  =  REAL( UPDATE )
*          Array containing details of pixel contibutions
*     ODIMS( 2 )  =  INTEGER( READ )
*          Dimensions of output image
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     For all rows of input image
*        For all pixels of current row
*           Work out position of current input image pixel in big
*            output image
*           Add current input image pixel value to corresponding
*            output image pixel
*           Update mask value at same pixel
*        Endfor
*     Endfor
*     Return
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*
*    History :
*
*     16-09-1985 : First implementation (REVA::MJM)
*     20-11-1986 : Bug fix in dimensions (HILO::MJM)
*     10-12-1986 : Changed code slightly (UKTH::MJM)
*
*    Type Definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :    IDIMSX,             ! dimensions of input image
     :    IDIMSY,             ! dimensions of input image
     :    XOFFSET,                ! x offset of image
     :    YOFFSET,                ! y    "    "   "
     :    ODIMSX,              ! dimensions of output image
     :    ODIMSY              ! dimensions of output image

      REAL
     :    IN( IDIMSX, IDIMSY )      ! input image

*    Import - Export :

      REAL
     :    MASK( ODIMSX, ODIMSY )    ! mask array

      REAL
     :    OUT( ODIMSX, ODIMSY )     ! output image

      LOGICAL
     :	  OVERLAP                           ! average overlap region

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

      INTEGER
     :    X,                      ! x position of pixel in output array
     :    Y,                      ! y    "      "   "    "    "     "
     :    I, J                    ! array counters

*-
*    check status on entry - return if not ok
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    loop round each row of input image
      DO  J  =  1, IDIMSY

*       find location of current row in (big) output image
         Y  =  J + YOFFSET

*       loop round each pixel of current row
         DO  I  =  1, IDIMSX

*          find position of current pixel in (big) output image
            X  =  I + XOFFSET

*          add the current input array value into the output array and
*          update the mask value accordingly
	    IF( OVERLAP ) THEN
              OUT( X, Y )  =  OUT( X, Y ) + IN( I, J )
              MASK( X, Y )  =  MASK( X, Y ) + 1.0
	    ELSE
	      IF( MASK( X, Y ) .EQ. 0.0) THEN
                OUT( X, Y )  =  OUT( X, Y ) + IN( I, J )
                MASK( X, Y )  =  MASK( X, Y ) + 1.0
	      END IF
	    END IF

*       end of loop round pixels in current row of input image
         END DO

*    end of loop round all rows of input image
      END DO


*    return and end
      END
