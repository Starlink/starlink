*+  MOSCAD - adds a 2-d array to a new array and records the overlap

      SUBROUTINE MOSCAD ( INARR, IDIM1, IDIM2, XOFSET, YOFSET, ODIM1,
     :                    ODIM2, OUTARR, MASK, STATUS )
*
*    Description :
*
*     This routine adds a 2-d array into a (usually larger) output data
*     array, and is used to make a mosaic of arrays. The offset of the
*     small array relative to the large one is given. For each valid
*     pixel of the small array, the value of that pixel is added to
*     the input value of the corresponding pixel in the big array, but
*     in a weighted fashion according to the number of previous
*     additions to that pixel, as recorded in the mask array. If a
*     successful addition takes place for a particular pixel, the
*     corresponding pixel in the mask array is incremented by 1 to
*     record this fact.
*
*    Invocation :
*
*     CALL MOSCAD ( INARR, IDIM1, IDIM2, XOFSET, YOFSET, ODIM1, ODIM2,
*                   OUTARR, MASK, STATUS )
*
*    Arguments :
*
*     INARR( IDIM1, IDIM2 ) = REAL( READ )
*         Input old 2-d array
*     IDIM1 = INTEGER( READ )
*         The first dimension of the input 2-d array.
*     IDIM2 = INTEGER( READ )
*         The second dimension of the input 2-d array.
*     XOFSET = INTEGER( READ )
*         x offset of image from bottom left
*     YOFSET = INTEGER( READ )
*         y offset of image from bottom left
*     ODIM1 = INTEGER( READ )
*         The first dimension of the output 2-d array and mask.
*     ODIM2 = INTEGER( READ )
*         The second dimension of the output 2-d array and mask.
*     OUTARR( ODIM1, ODIM2 ) = REAL( UPDATE )
*         Data array containing merged image
*     MASK( ODIM1, ODIM2 ) = REAL( UPDATE )
*         Array containing details of pixel contibutions
*     STATUS = INTEGER( READ )
*         Global status
*
*    Method :
*
*     Check for error on entry
*     If o.k. then
*     For each pixel
*        Find position of current pixel in output array
*        If pixel is valid then
*           If the mask value is zero then
*              Output pixel takes input-pixel value
*              Set mask value to one
*           Else
*              Add the input-pixel value to the corresponding output
*                pixel
*              Increment the mask by one
*        Elseif the mask is bad then
*           Set the output pixel to be bad
*        Endif
*     Endfor
*     End
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*     Malcolm J. Currie RAL ( RAL::CUR )
*
*    History :
*
*     16-09-1985 : First implementation (REVA::MJM)
*     1986 Aug 15: Renamed from MOSAIC_ADD, arguments reordered (7th
*                  to 5th), completed the prologue, nearly conformed
*                  to Starlink programming standards (RL.STAR::CUR).
*     1986 Sep 4 : Renamed parameters section to arguments, applied
*                  bad-pixel handling (RL.STAR::CUR).
*     1988 May 31: Removed normalisation code so that optional
*                  averaging may be performed by the calling
*                  application ( RAL::CUR )
*     1989 Aug  7: Passed array dimensions as separate variables
*                  (RL.STAR::CUR).
*
*    Type Definitions :
*
      IMPLICIT  NONE           ! No default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'       ! SSE global definitions
      INCLUDE 'PRM_PAR'        ! PRIMDAT public constants

*    Import :

      INTEGER
     :  IDIM1, IDIM2,
     :  XOFSET,
     :  YOFSET,
     :  ODIM1, ODIM2

      REAL
     :  INARR( IDIM1, IDIM2 )

*    Import - Export :

      REAL
     :  OUTARR( ODIM1, ODIM2 )

      REAL
     :  MASK( ODIM1, ODIM2 )


*    Status :

      INTEGER  STATUS

*    Local variables :

      INTEGER
     :  X,                     ! x position of pixel in output array
     :  Y,                     ! y    "      "   "    "    "     "
     :  I, J                   ! array counters
*
*-

*    Check status on entry - return if not ok

      IF ( STATUS .EQ. SAI__OK ) THEN

*       Loop round each pixel in input array

         DO  J  =  1, IDIM2
            Y  =  J + YOFSET

            DO  I  =  1, IDIM1

*             Find position of current pixel in (bigger) output array

               X  =  I + XOFSET

*             If the current input pixel value is valid then

               IF ( INARR( I, J ) .NE. VAL__BADR ) THEN

*                Check to see if the mask array indicates that no data
*                have yet been added to the current output point - as
*                this point is actually a valid point in the input array
*                A mask value of zero at the end will indicate the
*                the spaces where no input arrays actually lie.

                  IF ( MASK( X, Y ) .EQ. 0.0 ) THEN

*                   This is a valid pixel to be included, but is the
*                   first one at this point in the output, so just set
*                   the output equal to the input and set the mask to 1

                     OUTARR( X, Y )  =  INARR( I, J )
                     MASK( X, Y )  =  1.0

                  ELSE

*                   This is a valid pixel to be included, and is not the
*                   first, so just add its value to the output and
*                   increment the mask by 1

                     OUTARR( X, Y )  =  OUTARR( X, Y ) + INARR( I, J )
                     MASK( X, Y )  =  MASK( X, Y ) + 1.0

                   END IF

*              Else if we have a bad pixel and the mask is still set to
*              0

                ELSE IF ( MASK( X, Y ) .EQ. 0.0 ) THEN

*                This is a valid input position (i.e. not overlapped),
*                but the input value is bad, and there has been no
*                previous data added to the point - thus set the output-
*                array value to the bad value. If good data is
*                subsequently available for this output position, the
*                code above will include it in the correct fashion,
*                deleting the bad value inserted here. Subsequent
*                bad pixels at the point will be ignored because the
*                mask will not be zero.

                  OUTARR( X, Y )  =  VAL__BADR

*             End of if-input-value-is-ok check

               END IF

*          End of loops for each pixel in the input array

            END DO
         END DO
      END IF

*    return and end

      END
