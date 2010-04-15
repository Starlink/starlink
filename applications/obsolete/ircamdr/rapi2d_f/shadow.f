*+  SHADOW - enhances edges in images using a shadow effect

      SUBROUTINE SHADOW ( STATUS )

*    Description :
*
*     This routine takes a 2d image and creates an enhanced
*     version of it. The enhancement is a shadow effect that
*     causes features in an image to appear as though they have
*     been illuminated from the side by some imaginary light
*     source. The enhancement is useful in locating edges and
*     fine detail in an image.
*
*    Invocation :
*
*     CALL SHADOW( STATUS )
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*           Image to be analysed
*     XSHIFT  =  REAL( READ )
*           Shift in x coordinate to be used in enhancement
*     YSHIFT  =  REAL( READ )
*           Shift in y coordinate to be used in enhancement
*     OUTPIC  =  IMAGE( WRITE )
*           Output enhanced image structure
*     OTITLE  =  CHARACTER( WRITE )
*           Title string for output image structure
*
*    Method :
*
*     Get input IMAGE structure and map the DATA_ARRAY component
*     Get x shift value
*     Get y shift value
*     Create output IMAGE structure, get OTITLE component, create
*       DATA_ARRAY component same size as input DATA_ARRAY
*     Map output DATA_ARRAY component
*     If no errors so far then
*        Call SHIFTS for each axis to set up the parameters for
*          the shifts requested
*        If both shifts are integer then
*           Call SHADOWSUB to enhance input DATA_ARRAY, into the
*             output DATA_ARRAY
*        Else
*           Create two workspaces same dimension as input/output arrays
*           Call SHIFTX to shift input array in x dimension into
*             first workspace
*           Call SHIFTY to shift first workspace in y dimension into
*             second workspace
*           Call SUBARR2D to subtract shifted second workspace from
*             input array, with output going to output DATA_ARRAY
*           Tidy up workspace arrays
*        Endif
*     Endif
*     Tidy up output structure
*     Tidy up input structure
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
*     09-01-1986 :  First implementation (REVA::MJM)
*     30-06-1986 : Changed call the SUBARRAY to SUBARR2D (REVA::MJM)
*     12-Apr-1994  Changed DAT, CMP calls to NDF (SKL@JACH)
*     12-Aug-1994  Changed input DIM arguments for SUBARR2D (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'NDF_PAR'
      INCLUDE  'NDF_ERR'

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local constants :

      INTEGER NDIMS               ! image dimensionality
      PARAMETER ( NDIMS = 2 )     ! 2d images only

*    Local variables :

      INTEGER
     :  DIMS( NDIMS ),   ! dimensions of input DATA_ARRAY
     :  NDIM,            ! number of dimensions from NDF_DIM
     :  NELEMENTS,       ! number of elements mapped by NDF_MAP
     :  PNTRI,           ! pointer to input DATA_ARRAY component
     :  PNTRO,           !    "     " output  "    "       "
     :  PLACE1,          ! place holder for temporary array 1
     :  PLACE2,          ! place holder for temporary array 2
     :  WKPNTR1,         ! pointer to first workspace array
     :  WKPNTR2,         !    "     " second    "       "
     :  INTXS,           ! integer x coord shift (rounded up)
     :  INTYS,           !    "    y   "     "       "     "
     :  LBND( 2 )        ! lower bounds for temporary arrays

      DATA LBND / 1, 1 /

      INTEGER                 ! locators for :
     :  LOCI,                 ! input data structure
     :  LOCO,                 ! output  "      "
     :  WKLOC1,               ! first workspace array
     :  WKLOC2                ! second    "       "

      REAL
     :  XSHIFT,          ! x coord shift used in enhancement
     :  YSHIFT,          ! y   "     "     "   "      "
     :  FRACX,           ! fractional part of shift in x coord
     :  FRACY            !      "       "   "   "    " y   "

      LOGICAL
     :  XWHOLE,          ! true if x shift is integer number of pixels
     :  YWHOLE,          !   "   " y   "    "    "      "     "    "
     :  XNEG,            ! true if x shift is negative
     :  YNEG             !   "   " y   "    "     "

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    first get locator to the input IMAGE type data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    map in its DATA_ARRAY component
      CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :               PNTRI, NELEMENTS, STATUS)

      CALL NDF_DIM( LOCI, NDIMS, DIMS, NDIM, STATUS )

*    get the x and y coordinate shifts to be used in enhancement -
*    if the x shift (say) is positive, positive features in the
*    original image will appear to be lit from the positive x
*    direction, that is to say the right. Similarly, if the y
*    shift is positive, the light source will appear to be shining
*    from above the image.
      CALL AIF_GET0R( 'XSHIFT', 1.0, 0.0, REAL( DIMS( 1 ) ),
     :                 XSHIFT, STATUS )
      CALL AIF_GET0R( 'YSHIFT', 1.0, 0.0, REAL( DIMS( 2 ) ),
     :                 YSHIFT, STATUS )

*    now get the output structure and title
      CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, DIMS, LOCO, STATUS )

*    map the output DATA_ARRAY component
      CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :               PNTRO, NELEMENTS, STATUS )

*    if no errors so far then proceed
      IF ( STATUS .EQ. SAI__OK ) THEN

*       evaluate the parameters for shifting to see if integer
*       shifts have been requested
         CALL SHIFTS( DIMS( 1 ), XSHIFT, 'X', INTXS, XWHOLE, XNEG,
     :                FRACX, STATUS )
         CALL SHIFTS( DIMS( 2 ), YSHIFT, 'Y', INTYS, YWHOLE, YNEG,
     :                FRACY, STATUS )

*       check for both the x and y shifts being integers
         IF( XWHOLE .AND. YWHOLE ) THEN

*          both shifts are integer - call SHADOWSUB to do the (now)
*          simple work
            CALL SHADOWSUB( %VAL( PNTRI ), DIMS(1), DIMS(2),
     :                      INT( XSHIFT ), INT( YSHIFT ), %VAL( PNTRO ),
     :                      STATUS )

         ELSE

*          one or other or both of the shifts requested are non-integral
*          so proceed accordingly. First create and map the two
*          workspaces needed by this portion of the program

            CALL NDF_TEMP( PLACE1, STATUS)
            CALL NDF_NEW( '_REAL', NDIMS, LBND, DIMS, PLACE1, WKLOC1,
     :                    STATUS )
            CALL NDF_MAP( WKLOC1, 'DATA', '_REAL', 'WRITE',
     :                    WKPNTR1, NELEMENTS, STATUS )

            CALL NDF_TEMP( PLACE2, STATUS)
            CALL NDF_NEW( '_REAL', NDIMS, LBND, DIMS, PLACE2, WKLOC2,
     :                    STATUS )
            CALL NDF_MAP( WKLOC2, 'DATA', '_REAL', 'WRITE',
     :                    WKPNTR2, NELEMENTS, STATUS )

*          if successful in this, continue
            IF ( STATUS .EQ. SAI__OK ) THEN

*             call SHIFTX to shift the input array in the x direction
*             into the first work array
               CALL SHIFTX( XNEG, XWHOLE, INTXS, FRACX, DIMS,
     :                      %VAL( PNTRI ), %VAL( WKPNTR1 ), STATUS )

*             now SHIFTY to shift the first work array in the y direction
*             into the second work array
               CALL SHIFTY( YNEG, YWHOLE, INTYS, FRACY, DIMS,
     :                      %VAL( WKPNTR1 ), %VAL( WKPNTR2 ), STATUS )

*             subtract the input array from the shifted second work
*             array - in this order, the correct lighting effect will
*             be acheived - output result into the output DATA_ARRAY
               CALL SUBARR2D( %VAL( WKPNTR2 ), %VAL( PNTRI ),
     :                        %VAL( PNTRO ), DIMS(1), DIMS(2), STATUS )

*          end of check to see that workspaces were properly created
            END IF

*          clear up the workspaces
            CALL NDF_ANNUL( WKLOC1, STATUS )
            CALL NDF_ANNUL( WKLOC2, STATUS )

*       end of IF( XWHOLE .AND. YWHOLE ) statement
         END IF

*    end of check to see that output structure was properly created
      END IF

*    clear up the output structure
      CALL NDF_ANNUL( LOCO, STATUS )

*    tidy up the input data structure
      CALL NDF_ANNUL( LOCI, STATUS )


*    end
      END

