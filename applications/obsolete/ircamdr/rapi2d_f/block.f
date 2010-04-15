*+  BLOCK - smooth a 2-D image using a boxfilter
      SUBROUTINE BLOCK( STATUS )
*    Description :
*     Each pixel in the input image, %INPIC, is replaced by the average value
*     of the pixels in a box of side %BOXSIZ pixels centered on the pixel to
*     be replaced. At the image edge pixel replication is used to allow the
*     smoothing to be performed. %BOXSIZ must be an odd number in the range
*     3 to 15. The smoothed image is written to the output image, %OUTPIC,
*     which is given the title %OTITLE.
*    Parameters :
*     INPIC  = IMAGE( READ )
*           IMAGE structure containing 2-D array to be smoothed.
*     BOXSIZ = INTEGER( READ )
*           Size of the smoothing box in pixels.
*     OUTPIC = IMAGE( WRITE )
*           IMAGE structure to contain smoothed 2-D array.
*     OTITLE = CHAR*72( READ )
*           Will form the TITLE component of the output IMAGE structure.
*    Method :
*     Get input IMAGE data structure and map the DATA_ARRAY component
*     Get an odd number value for BOXSIZ in the range 3 to 15
*     Create output IMAGE data structure containing a DATA_ARRAY
*       component and a TITLE component for which a value is obtained
*     Map output DATA_ARRAY component
*     Get the workspace needed by the smoothing routine
*     Call RAPBLO to perform the smoothing
*     Tidy up input/output images and workspace
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     27/07/1983 : Original version                (ROE::ASOC5)
*     17/02/1984 : Modified to use TITLE component (ROE::ASOC5)
*     10-Mar-94    Changed DAT_, CMP_ calls to NDF_ (SKL@JACH)
*     12-Aug-1994  Changed input DIM arguments for RAPBLO (SKL@JACH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local constants :
      INTEGER NDIM
      PARAMETER ( NDIM = 2 ) ! dimensionality of input/output images
*    Local variables :
      INTEGER                ! locators for :
     :  LOCI,                ! input data structure
     :  LOCO,                ! output data structure
     :  WLOC1,               ! workspace for RAPBLO subroutine
     :  WLOC2                !      "     "     "        "
      INTEGER
     :  NELEMENTS,     ! Number of elements mapped
     :  PLACE,         ! Place holder for temporary NDF
     :  NDIMS,         ! number dimensions from NDF_DIM
     :  DIMS( NDIM ),  ! dimensions of the input/output DATA_ARRAYs
     :  WDIMS( NDIM ), ! dimensions of workspace array
     :  BOXSIZ,        ! size ( in pixels ) of the smoothing box
     :  PNTRI,         ! pointer to : input DATA_ARRAY
     :  PNTRO,         !            : output DATA_ARRAY
     :  WPNTR1,        !            : workspace array
     :  WPNTR2         !            :     "       "
*-

*    get locator to input IMAGE type data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    check for error
      IF( STATUS .EQ. SAI__OK ) THEN

*       map input DATA_ARRAY component
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :      PNTRI, NELEMENTS, STATUS )

*       get dimensions of array
         CALL NDF_DIM( LOCI, NDIM, DIMS, NDIMS, STATUS )

*       get an value for BOXSIZ which is odd and between 3 and 15
         CALL ODDGET( 'BOXSIZ', 5, 3, 15, BOXSIZ, STATUS )

*       create output IMAGE type data structure containing a DATA_ARRAY
*       component of dimensions DIMS and create and get a value for
*       a TITLE component
         CALL CREOUT( 'OUTPIC', 'OTITLE', NDIM, DIMS, LOCO, STATUS )

*       check for error
         IF( STATUS .EQ. SAI__OK ) THEN

*          map output DATA_ARRAY component
            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :        PNTRO, NELEMENTS, STATUS )

*          set up the workspace array dimensions
            WDIMS( 1 ) = DIMS( 1 )
            WDIMS( 2 ) = BOXSIZ

*          create and map workspace arrays

            CALL NDF_TEMP( PLACE, STATUS )
            CALL NDF_NEWP( '_REAL', NDIM, WDIMS, PLACE, WLOC1, STATUS )
            CALL NDF_MAP( WLOC1, 'DATA', '_REAL', 'WRITE', WPNTR1,
     :        NELEMENTS, STATUS )

            CALL NDF_TEMP( PLACE, STATUS )
            CALL NDF_NEWP( '_REAL', 1, WDIMS(1), PLACE, WLOC2, STATUS )
            CALL NDF_MAP( WLOC2, 'DATA', '_REAL', 'WRITE', WPNTR2,
     :        NELEMENTS, STATUS )

            IF( STATUS .EQ. SAI__OK ) THEN

*             pass everything to the rapid block smoothing routine
               CALL RAPBLO( BOXSIZ, DIMS(1), DIMS(2), %VAL(PNTRI),
     :           %VAL(PNTRO), %VAL(WPNTR1), %VAL(WPNTR2), STATUS )
            ENDIF

*          tidy up all the structures
            CALL NDF_ANNUL(  LOCO, STATUS )
            CALL NDF_ANNUL( WLOC1, STATUS )
            CALL NDF_ANNUL( WLOC2, STATUS )
         ENDIF

         CALL NDF_ANNUL(  LOCI, STATUS )
      ENDIF

      END
