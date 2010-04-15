
*+  SQORST - squash or stretch a 2-D image in either or both axes
      SUBROUTINE SQORST( STATUS )
*    Description :
*     The output image, %OUTPIC, is produced by either squashing or stretching
*     the input image, %INPIC, in either or both of the X and Y axes. The
*     dimensions of the output image, %XDIM and %YDIM are given by the user.
*     The stretching is performed by keeping the edge pixels fixed and
*     calculating the intervening pixels by bi-linear interpolation. The
*     squashing is performed by calculating each pixel in the output image as
*     the mean of the corresponding pixels in the input image.
*    Parameters :
*     INPIC  = IMAGE( READ )
*           IMAGE structure containing the 2-D array to be squashed or
*           stretched.
*     XDIM   = INTEGER( READ )
*           First dimension for the output 2-D array.
*     YDIM   = INTEGER( READ )
*           Second dimension for the output 2-D array.
*     OUTPIC = IMAGE( WRITE )
*           IMAGE structure to contain the 2-D array after being squashed or
*           stretched.
*     OTITLE = CHAR*72( READ )
*           Will form the TITLE component for the output IMAGE structure.
*    Method :
*     Get input IMAGE type data structure and map DATA_ARRAY component
*     Inform user of the dimensions of the input array
*     Get the dimensions of the output array checking that they
*       are both greater than zero
*     Create output IMAGE type data structure containing DATA_ARRAY
*       component of appropriate dimensions and also create and get
*       a value for a TITLE component
*     Set up the workspace arrays
*     If 1st dimension of output array > 1st dimension of input array then
*        Stretch the input array and store the result in workspace
*     Else
*        Squash the input array and store the result in workspace
*     Endif
*     If 2nd dimension of output array > 2nd dimension of input array then
*        Stretch the workspace and store result in output array
*     Else
*        Squash the workspace and store result in output array
*     Endif
*     Tidy up the data structures
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     18/08/1983 : Original version                (ROE::ASOC5)
*     19/02/1984 : Modified to use TITLE component (ROE::ASOC5)
*     12-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     12-AUG-1994  Changed input DIM arguments for SQSHX/Y, STRX/Y (SKL@JACH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local constants :
      INTEGER NDIM
      PARAMETER ( NDIM = 2 )  ! dimensionality of input/output arrays
      INTEGER MAXDIM
      PARAMETER ( MAXDIM = 10000000 ) ! maximum dimensions for output array
*    Local variables :
      INTEGER                ! locators for :
     :  LOCI,                ! input data structure
     :  LOCO,                ! output data structure
     :  WLOC1,               ! workspace array to hold intermediate array
     :  WLOC2,               !      "      "   for weights, 1st dimension
     :  WLOC3,               !      "      "    "     "   , 2nd     "
     :  WLOC4,               !      "      "    " pixel limits, 1st dimension
     :  WLOC5                !      "      "    "   "      "  , 2nd     "
      INTEGER
     :  IDIMS( NDIM ),  ! dimensions of input DATA_ARRAY
     :  ODIMS( NDIM ),  !      "      " output DATA_ARRAY
     :  WDIMS1( NDIM ), !      "      " intermediate stage array
     :  WDIMS2( NDIM ), !      "      " pixel limits and weight arrays, 1st dim
     :  WDIMS3( NDIM ), !      "      "   "      "    "     "      "  , 2nd  "
     :  NDIMS,          ! number of dimensions from NDF_DIM
     :  NELEMENTS,      ! number of elements mapped by NDF_MAP
     :  LBND( 2 ),      ! lower bounds for temporary arrays
     :  PNTRI,  ! pointer to : input DATA_ARRAY
     :  PNTRO,  !            : output DATA_ARRAY
     :  PLACE1, ! place holder for temporary array 1
     :  PLACE2, ! place holder for temporary array 2
     :  PLACE3, ! place holder for temporary array 3
     :  PLACE4, ! place holder for temporary array 4
     :  PLACE5  ! place holder for temporary array 5
      INTEGER
     :  WPNTR1, !            : workspace array, intermediate array
     :  WPNTR2, !            :     "       "  , 1st dimension weights
     :  WPNTR3, !            :     "       "  , 2nd     "        "
     :  WPNTR4, !            :     "       "  , 1st dim. pixel limits
     :  WPNTR5  !            :     "       "  , 2nd  "     "      "

      DATA LBND / 1, 1 /
*-

*    get locator to input IMAGE type data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    check for error
      IF( STATUS .EQ. SAI__OK ) THEN

*       map input DATA_ARRAY component
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :      PNTRI, NELEMENTS, STATUS )

         CALL NDF_DIM( LOCI, NDIM, IDIMS, NDIMS, STATUS)

*       tell user what the dimensions of the input array are
         CALL MSG_SETI( 'IXDIM', IDIMS( 1 ) )
         CALL MSG_SETI( 'IYDIM', IDIMS( 2 ) )
         CALL MSG_OUT( 'IN_DIMS',
     :     'First dimension = ^IXDIM, Second dimension = ^IYDIM',
     :     STATUS )

*       get first dimension for output array
         CALL APPG0I( 'XDIM', IDIMS(1), 1, MAXDIM, ODIMS(1), STATUS )

*       get second dimension for output array
         CALL APPG0I( 'YDIM', IDIMS(2), 1, MAXDIM, ODIMS(2), STATUS )

*       create output IMAGE type data structure containing a DATA_ARRAY
*       component of dimensions ODIMS and create and get a value for
*       a TITLE component
         CALL CREOUT( 'OUTPIC', 'OTITLE', NDIM, ODIMS, LOCO, STATUS )

*       check for error
         IF( STATUS .EQ. SAI__OK ) THEN

*          map output DATA_ARRAY component
            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :        PNTRO, NELEMENTS, STATUS )

*          set up dimensions of the workspace arrays
            WDIMS1( 1 ) = ODIMS( 1 )
            WDIMS1( 2 ) = IDIMS( 2 )
            WDIMS2( 1 ) = ODIMS( 1 )
            WDIMS2( 2 ) = 2
            WDIMS3( 1 ) = ODIMS( 2 )
            WDIMS3( 2 ) = 2

*          create and map the workspace arrays
            CALL NDF_TEMP( PLACE1, STATUS )
            CALL NDF_NEW( '_REAL', NDIM, LBND, WDIMS1, PLACE1, WLOC1,
     :                    STATUS )
            CALL NDF_MAP( WLOC1, 'DATA', '_REAL', 'WRITE', WPNTR1,
     :                    NELEMENTS, STATUS )

            CALL NDF_TEMP( PLACE2, STATUS )
            CALL NDF_NEW( '_REAL', NDIM, LBND, WDIMS2, PLACE2, WLOC2,
     :                    STATUS )
            CALL NDF_MAP( WLOC2, 'DATA', '_REAL', 'WRITE', WPNTR2,
     :                    NELEMENTS, STATUS )

            CALL NDF_TEMP( PLACE3, STATUS )
            CALL NDF_NEW( '_REAL', NDIM, LBND, WDIMS3, PLACE3, WLOC3,
     :                    STATUS )
            CALL NDF_MAP( WLOC3, 'DATA', '_REAL', 'WRITE', WPNTR3,
     :                    NELEMENTS, STATUS )

            CALL NDF_TEMP( PLACE4, STATUS )
            CALL NDF_NEW( '_REAL', NDIM, LBND, WDIMS2, PLACE4, WLOC4,
     :                    STATUS )
            CALL NDF_MAP( WLOC4, 'DATA', '_REAL', 'WRITE', WPNTR4,
     :                    NELEMENTS, STATUS )

            CALL NDF_TEMP( PLACE5, STATUS )
            CALL NDF_NEW( '_REAL', NDIM, LBND, WDIMS3, PLACE5, WLOC5,
     :                    STATUS )
            CALL NDF_MAP( WLOC5, 'DATA', '_REAL', 'WRITE', WPNTR5,
     :                    NELEMENTS, STATUS )


*          check for error
            IF( STATUS .EQ. SAI__OK ) THEN

               IF( ODIMS( 1 ) .GT. IDIMS( 1 ) ) THEN

*                stretch input array in X direction storing result in workspace
                  CALL STRX( IDIMS(1), IDIMS(2), %VAL( PNTRI ),
     :              WDIMS1(1), WDIMS1(2), %VAL( WPNTR1 ),
     :              %VAL( WPNTR4 ), %VAL( WPNTR2 ), STATUS )
               ELSE

*                squash input array in X direction storing result in workspace
                  CALL SQSHX( IDIMS(1), IDIMS(2), %VAL( PNTRI ),
     :              WDIMS1(1), WDIMS1(2), %VAL( WPNTR1 ),
     :              %VAL( WPNTR4 ), %VAL( WPNTR2 ), STATUS )
               ENDIF

               IF( ODIMS( 2 ) .GT. IDIMS( 2 ) ) THEN

*                stretch workspace in Y direction storing result in output
*                array
                  CALL STRY( WDIMS1(1), WDIMS1(2), %VAL( WPNTR1 ),
     :              ODIMS(1), ODIMS(2), %VAL( PNTRO ),
     :              %VAL( WPNTR5 ), %VAL( WPNTR3 ), STATUS )
               ELSE

*                squash workspace in Y direction storing result in output array
                  CALL SQSHY( WDIMS1(1), WDIMS1(2), %VAL( WPNTR1 ),
     :              ODIMS(1), ODIMS(2), %VAL( PNTRO ),
     :              %VAL( WPNTR5 ), %VAL( WPNTR3 ), STATUS )
               ENDIF
            ENDIF

*          tidy up workspace and output structure
            CALL NDF_ANNUL( WLOC1, STATUS )
            CALL NDF_ANNUL( WLOC2, STATUS )
            CALL NDF_ANNUL( WLOC3, STATUS )
            CALL NDF_ANNUL( WLOC4, STATUS )
            CALL NDF_ANNUL( WLOC5, STATUS )
            CALL NDF_ANNUL( LOCO, STATUS )
         ENDIF

*       tidy up input structure
         CALL NDF_ANNUL( LOCI, STATUS )
      ENDIF

      END
