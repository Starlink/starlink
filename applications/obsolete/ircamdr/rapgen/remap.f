

*+  REMAP - remaps image according to a new histogram key

      SUBROUTINE REMAP ( DIMS1, DIMS2, ARRAY, XSTART, YSTART, XFINISH,
     :                   YFINISH, VALMAX, VALMIN, NUMBIN,
     :                   MAP, STATUS )

*    Description :
*
*     This routine remaps all the pixels in a specified sub-array
*     of a 2-d image onto new values, according to the key given
*     which locates pixel intensities with respect to a transformed
*     histogram.
*
*    Invocation :
*
*     CALL REMAP( DIMS, ARRAY, XSTART, YSTART, XFINISH, YFINISH,
*                 VALMAX, VALMIN, NUMBIN, MAP, STATUS )
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Set the scaling factor
*     For all rows in specified subarray
*        For all pixels in current row
*           Work out which bin of old histogram current pixel
*             would come from
*           Locate equivalent bin in new histogram from key
*           Set current pixel to have the value corresponding
*             to the new histogram bin
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
*     Mark McCaughrean UoE (REVA::MJM)
*
*    History :
*
*     31-07-1985 : First SSE/ADAM implementation, with algorithm
*                : taken from Gonzalez and Wintz (UKTH::MARK)
*     14-04-1986 : Tidied up (REVA::MJM)
*     12-AUG-1994  Changed DIM arguments so that routine will compile(SKL@JACH)
*
*    Type Definitions :

      IMPLICIT  NONE             ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'         ! global SSE definitions

*    Import :

      INTEGER
     :    DIMS1, DIMS2,             ! dimensions of input array
     :    XSTART,                ! x start coord for sub-array
     :    YSTART,                ! y   "     "    "      "
     :    XFINISH,               ! x finish  "    "      "
     :    YFINISH,               ! y   "     "    "      "
     :    NUMBIN,                ! number of bins used in histogram
     :    MAP( NUMBIN )          ! array containing key to transform

      REAL
     :    VALMAX,        ! maximum value in included sub-array
     :    VALMIN         ! minimum   "    "     "        "

*    Import-Export :

      REAL
     :    ARRAY( DIMS1, DIMS2 )   ! input/output data array

*    Status :

      INTEGER  STATUS            ! global status parameter

*    Local variables :

      INTEGER
     :    OLDBIN,                ! array pointer to old histogram bin
     :    NEWBIN,                ! array pointer to new histogram bin
     :    I, J                   ! counters

      REAL
     :    SCALE                  ! scale factor used for choosing correct bin

*-
*    check for error on entry - return if not ok
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    next set the scaling factor, watching for too small value
      IF( ABS( VALMAX - VALMIN ) .GT. 1.0E-20 ) THEN
         SCALE  =  VALMAX - VALMIN
      ELSE
         SCALE  =  1.0
      END IF

*    now perform the intensity transform - loop round all rows in
*    in the specified sub-array first
      DO  J  =  YSTART, YFINISH

*       loop round all specified pixels in current row
         DO  I  =  XSTART, XFINISH

*          find bin number for particular point
            OLDBIN  =  NUMBIN * ( ARRAY( I, J ) - VALMIN ) / SCALE

*          check to see that bin is within range
            OLDBIN  =  MIN( NUMBIN, MAX( 1, OLDBIN ) )

*          locate new bin from key array
            NEWBIN  =  MAP( OLDBIN )

*          replace pixel with value associated with new bin
            ARRAY( I, J )  =  ( ( REAL( NEWBIN ) / REAL( NUMBIN ) ) *
     :                            SCALE ) + VALMIN

*       end of loop round pixels in current row
         END DO

*    end of loop round rows in specified subarray
      END DO


*    end and return
      END
