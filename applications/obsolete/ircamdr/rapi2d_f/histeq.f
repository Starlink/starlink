
*+  HISTEQ - performs a histogram equalisation on an image

      SUBROUTINE HISTEQ( STATUS )

*    Description :
*
*     Histogram equalisation is performed on a user specified
*     sub-array of a 2-d image. The transformed image is output
*     to a new image.
*
*    Parameters :
*
*     INPIC   =  IMAGE( READ )
*           Image to be transformed
*     XSTART  =  INTEGER( READ )
*           x start coord of sub-array to be included
*     YSTART  =  INTEGER( READ )
*           y start coord of sub-array to be included
*     XFINISH =  INTEGER( READ )
*           x end coord of sub-array to be included
*     YFINISH =  INTEGER( READ )
*           x end coord of sub-array to be included
*     NUMBIN  =  INTEGER( READ )
*           Number of histogram bins to be used
*     OUTPIC  =  IMAGE( WRITE )
*           Transformed image
*     OTITLE  =  CHAR( READ )
*           Title string for new image
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get locator to input image structure
*     If no error so far then
*        Map in its DATA_ARRAY component
*        Output array dimensions to interface
*        Get coordinates of box to be histogram equalised
*        Get number of bins to be used in equalisation
*        Create a new image structure to hold output
*        If no error so far then
*           Map a DATA_ARRAY component
*           If no error so far then
*              Copy input array into output array
*              Call HISTEQSUB to perform the equalisation
*           Endif
*           Tidy up output structure
*        Endif
*        Tidy up input structure
*     Endif
*     End
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*
*    History :
*
*     31-07-1985 : First SSE/ADAM implementation (UKTH::MARK)
*     14-04-1986 : Tidied and modified (REVA::MJM)
*     11-MAR-94    Changed DAT_, CMP_ to NDF_ (SKL@JACH)
*     11-Aug-1994  Changed input DIMS for COPY2D and HISTEQSUB (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE               ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'           ! global SSE definitions
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'

*    Status :

      INTEGER STATUS              ! global status parameter

*    Local Constants :

      INTEGER NDIMS               ! image dimensionality
      INTEGER MAXBIN              ! maximum number of histogram bins allowed
      PARAMETER ( NDIMS = 2 )     ! 2-d only
      PARAMETER ( MAXBIN = 2048 ) ! should be enough

*    Local variables :

      INTEGER
     :  NELEMENTS,                ! number of elements mapped
     :  NDIM,                     ! dimensions from NDF_DIM
     :  IDIMS( NDIMS ),           ! dimensions of input image
     :  PNTRI,                    ! pointer to input DATA_ARRAY component
     :  PNTRO,                    ! pointer to output DATA_ARRAY component
     :  XSTART,                   ! x start coord of sub-array
     :  YSTART,                   ! y   "     "   "   "    "
     :  XFINISH,                  ! x end coord of sub-array
     :  YFINISH,                  ! y  "    "   "   "    "
     :  NUMBIN,                   ! number of bins in histogram
     :  OLDHIST( MAXBIN ),        ! histogram of image before equalisation
     :  MAP( MAXBIN )             ! key to histogram transformation

      INTEGER                     ! locators for :
     :  LOCI,                     ! input data structure
     :  LOCO                      ! output data structure

*-
*    check for error on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF


*    get a locator to input image data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    if no error so far then continue
      IF ( STATUS .EQ. SAI__OK ) THEN

*       map the DATA_ARRAY component of the input data structure
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )

*       get dimensions
         CALL NDF_DIM( LOCI, NDIMS, IDIMS, NDIM, STATUS )

*       tell user dimensions of input array
         CALL MSG_OUT( 'BLANK', ' ', STATUS )
         CALL MSG_SETI( 'XDIM', IDIMS(1) )
         CALL MSG_SETI( 'YDIM', IDIMS(2) )
         CALL MSG_OUT( 'INPUT_DIMS',
     :        'Image is ^XDIM by ^YDIM pixels', STATUS )
         CALL MSG_OUT( 'BLANK', ' ', STATUS )

*       get x and y start coords of sub-array to be processed
         CALL AIF_GET0I( 'XSTART', 1, 1, IDIMS(1), XSTART, STATUS )
         CALL AIF_GET0I( 'YSTART', 1, 1, IDIMS(2), YSTART, STATUS )

*       get x and y end coords of sub-array to be processed
         CALL AIF_GET0I( 'XFINISH', IDIMS(1), 1, IDIMS(1),
     :                    XFINISH, STATUS )
         CALL AIF_GET0I( 'YFINISH', IDIMS(2), 1, IDIMS(2),
     :                    YFINISH, STATUS )

*       also get the number of histogram bins to be used - suggest
*       the maximum allowable as the default
         CALL AIF_GET0I( 'NUMBIN', MAXBIN, 1, MAXBIN, NUMBIN, STATUS )

*       create an output image structure
         CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, IDIMS, LOCO, STATUS )

*       if no error so far then continue
         IF ( STATUS .EQ. SAI__OK ) THEN

*          map in a DATA_ARRAY component
            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                  PNTRO, NELEMENTS, STATUS )

*             check status before accessing pointers
               IF ( STATUS .EQ. SAI__OK ) THEN

*                copy the input array into the output array
                  CALL COPY2D( IDIMS(1), IDIMS(2), %VAL( PNTRI ),
     :                         %VAL( PNTRO ), STATUS )

*                call the working subroutine to perform the magic
                  CALL HISTEQSUB( IDIMS(1), IDIMS(2), %VAL( PNTRO ),
     :                         XSTART, YSTART, XFINISH, YFINISH,
     :                         NUMBIN, OLDHIST, MAP, STATUS )

               END IF

*          tidy up the output structure
            CALL NDF_ANNUL( LOCO, STATUS )

*       end of if-no-error-after-getting-output-structure check
         END IF

*       tidy up the input data structure
         CALL NDF_ANNUL( LOCI, STATUS )

*    end of if-no-error-after-getting-input-check
      END IF


*    end
      END
