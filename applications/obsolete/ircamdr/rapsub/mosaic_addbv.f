
*+  MOSAIC_ADDBV - adds an image to a new array and records the overlap -
*                  this version of the subroutine deals with bad pixels
*                  as defined by a given value in the input data itself

      SUBROUTINE MOSAIC_ADDBV ( IN, IDIMSX, IDIMSY, BAD_VALUE, XOFFSET,
     :	                        YOFFSET, OUT, MASK, ODIMSX, ODIMSY,
     :	                        OVERLAP, STATUS )

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
*     This version of the subroutine takes care of bad pixels - a
*     number BAD_VALUE is passed. On looping through the input data,
*     a is made check to see if the current input pixel is bad or not.
*     If it is, then the data is not included in the mosaic. If not,
*     then the valid data is added accordingly, the mask being updated.
*     Remembering that there will almost always be blank data positions
*     in a mosaic, where none of the input arrays actually lie, then
*     if no good data is found for a pixel that SHOULD have data, then
*     a bad pixel value is inserted in the output array at that position.
*     This procedure uses the mask array to check whether valid data has
*     been included at any point in the mosaic. If good data is found for
*     a point later, the bad value is deleted, and the good data inserted.
*
*    Invocation :
*
*     CALL MOSAIC_ADDBV ( IN, IDIMS, BAD_VALUE, XOFFSET, YOFFSET, OUT,
*                         MASK, ODIMS, OVERLAP, STATUS )
*
*    Parameters :
*
*     IN( IDIMS( 1 ), IDIMS( 2 ) )  =  REAL( READ )
*          Input old image
*     IDIMS( 2 )  =  INTEGER( READ )
*          Dimensions of input image
*     BAD_VALUE  =  REAL( READ )
*          Value in input image that denotes a bad pixel
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
*     OVERLAP = LOGICAL ( READ )
*          Defines action in overlap regions
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     For all rows of input image
*        For all pixels of current row
*           Work out position of current input image pixel in big
*            output image
*           If input image pixel is not bad then
*              If mask array is set to zero at this point then
*                 Output image value = input image value
*              Else
*                 New output value = Old output value + input value
*              Endif
*              Increment mask by one
*           Elseif mask array is set to zero at this point then
*              Set output array to bad value
*           Endif
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
*     20-11-1986 : Bug fix in dimensions (UKTH::MJM)
*     27-12-1986 : Bad pixel value version created (UKTH::MJM)
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
     :    ODIMSY               ! dimensions of output image

      REAL
     :    IN( IDIMSX, IDIMSY ),     ! input image
     :    BAD_VALUE               ! value that denotes a bad pixel in input

      LOGICAL
     :	  OVERLAP                 ! axction in overlap regions

*    Import - Export :

      REAL
     :    MASK( oDIMSX, oDIMSY )    ! mask array

      REAL
     :    OUT( ODIMSX, ODIMSY )     ! output image

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

*          if the current input pixel value is valid then
            IF ( IN( I, J ) .NE. BAD_VALUE ) THEN

*             check to see if the mask array indicates that no data has
*             yet been added to the current output point - as this point
*             is actually a valid point in the input image (as we are
*             looping round that image), then we don't want to leave it
*             set to zero, as would be the case for edge data created
*             when arrays overlap, in the spaces where no input arrays
*             actually lie
               IF ( MASK( X, Y ) .EQ. 0.0 ) THEN

*                this is a valid pixel to be included, but is the first
*                one at this point in the output, so just set the output
*                equal to the input and set the mask to 1
                  OUT( X, Y )  =  IN( I, J )
                  MASK( X, Y )  =  1.0

               ELSE

*                this is a valid pixel to be included, and is not the
*                first, so just add its value to the output and increment
*                the mask by 1
	          IF( OVERLAP ) THEN
                    OUT( X, Y )  =  OUT( X, Y ) + IN( I, J )
                    MASK( X, Y )  =  MASK( X, Y ) + 1.0
	          ELSE
	            IF( MASK( X, Y ) .EQ. 0.0) THEN
                      OUT( X, Y )  =  OUT( X, Y ) + IN( I, J )
                      MASK( X, Y )  =  MASK( X, Y ) + 1.0
	            END IF
	          END IF

               ENDIF

*          else if we have a bad pixel and the mask is still set to 0
            ELSE IF ( MASK( X, Y ) .EQ. 0.0 ) THEN

*             this is a valid input position (i.e. not edge data), but
*             the input value is bad, and there has been no previous
*             data added to the point - thus set the output array
*             value to the bad value. If good data is subsequently
*             available for this output position, the code above will
*             include it in the correct fashion, deleting the bad value
*             inserted here
               OUT( X, Y )  =  BAD_VALUE

*          end of if-input-value-is-ok check
            END IF

*       end of loop round pixels in current row of input image
         END DO

*    end of loop round all rows of input image
      END DO


*    return and end
      END
