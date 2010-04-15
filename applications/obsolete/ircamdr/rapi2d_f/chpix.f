
*+  CHPIX - replace bad pixels with user specified value

      SUBROUTINE CHPIX( STATUS )

*    Description :
*
*     User specified pixels in an array are replaced by new, user
*     specified values in a loop, until the user terminates it.
*
*    Parameters :
*     INPIC  = IMAGE( READ )
*           Image to be deglitched
*     OUTPIC = IMAGE( WRITE )
*           Deglitched version of the image
*     OTITLE = CHARACTER( READ )
*           Label for the output image
*     XCOORD = INTEGER( READ )
*           X coordinate of pixel to be deglitched
*     YCOORD = INTEGER( READ )
*           Y coordinate of pixel to be deglitched
*     NEWVAL = REAL( READ )
*           Value to replace old value in deglitched pixel
*     OLDVAL = REAL( WRITE )
*           Value of pixel before deglitching
*     AGAIN  = LOGICAL( READ )
*           Whether or not user is prompted for another pixel
*
*    Method :
*
*     Get input image from environment
*     If no error so far then
*        Map its DATA_ARRAY component
*        Output dimensions of array to interface
*        Create an output image structure
*        If no errors so far then
*           Map in a DATA_ARRAY component
*           If no error so far then
*              Copy input data array into output data array
*           Endif
*           Do while user wants another pixel deglitched and no error
*              Get coordinates of pixel to be changed
*              Get new value to be inserted in chosen pixel
*              If no error so far then
*                 Call CHPIXSUB to do actual changing of values
*                 Output old and new pixel values to interface
*              Endif
*              Cancel coordinate and value parameters
*           Enddo
*           Tidy up output image structure
*        Endif
*        Tidy up input image structure
*     Endif
*     End
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*
*    History :
*
*     28-06-1985 : Modified GLITCH (which calculates local mean
*                : around specified pixel, and replaces old value
*                : with this mean) to take a user specified value
*                : for the replacement, and called it CHPIX.
*                : (REVA::MJM)
*     09-12-1985 : Fixed error checking bug and implemented some
*                : AIF dynamic defaulting/checking calls (UKTH::MARK)
*     11-12-1985 : Fixed bug caused by continuous updating of read
*                : only data - changed program order (UKTH::MARK)
*     17-01-1986 : Improved error checking and tidied (REVA::MJM)
*     10-Mar-94    Changed DAT_, CMP_ calls to NDF_ (SKL@JACH)
*     11-Aug-1994  Changed input DIMS for COPY2D, CHPIXSUB  (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE               ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'           ! global SSE definitions
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'

*    Status :

      INTEGER STATUS              ! global status variable

*    Local Constants :

      INTEGER NDIMS               ! image dimensionality
      PARAMETER ( NDIMS = 2 )     ! defaulted to 2-d

*    Local variables :

      INTEGER
     :  IDIMS( NDIMS ), ! dimensions of input DATA_ARRAY
     :  NELEMENTS,      ! Number of elements mapped
     :  NDIM,           ! Number of dimensions from NDF_DIM
     :  PNTRI,          ! pointer to input DATA_ARRAY component
     :  PNTRO,          ! pointer to output DATA_ARRAY component
     :  XCOORD,         ! x coordinate of pixel to be deglitched
     :  YCOORD          ! y     "      "    "   "  "       "

      INTEGER                 ! locators for :
     :  LOCI,                 ! input data structure
     :  LOCO                  ! output data structure

      REAL
     :  OLDVAL,               ! old value of deglitched pixel
     :  NEWVAL                ! new   "   "       "       "

      LOGICAL
     :  AGAIN                 ! whether or not user wants to do another

*-
*    check status on entry - return if not o.k.
      IF( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    get a locator to input IMAGE type data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    if no error so far then continue
      IF ( STATUS .EQ. SAI__OK ) THEN

*       map the DATA_ARRAY component of the input data structure
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )

*       get dimensions of array
         CALL NDF_DIM( LOCI, NDIMS, IDIMS, NDIM, STATUS)

*       tell user dimensions of input array
         CALL MSG_SETI( 'XDIM', IDIMS(1) )
         CALL MSG_SETI( 'YDIM', IDIMS(2) )
         CALL MSG_OUT( 'INPUT_DIMS',
     :        'Image is ^XDIM by ^YDIM pixels', STATUS )

*       now create output IMAGE type data structure with DATA_ARRAY
*       component; also create and get value for TITLE component
         CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, IDIMS, LOCO, STATUS )

*       if no error so far then continue
         IF ( STATUS .EQ. SAI__OK ) THEN

*          map output DATA_ARRAY component
            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                     PNTRO, NELEMENTS, STATUS )

*          check for error before accessing pointer
            IF ( STATUS .EQ. SAI__OK ) THEN

*             copy old array into new array
               CALL COPY2D( IDIMS(1), IDIMS(2), %VAL( PNTRI ),
     :                      %VAL( PNTRO ), STATUS )

            END IF

*          initialise AGAIN logical
            AGAIN  = .TRUE.

*          start loop for deglitching

            DO WHILE ( AGAIN .AND. ( STATUS .EQ. SAI__OK ) )

*             get x and y coordinates of pixel to be deglitched
*             AIF call will ensure pixel is within array
               CALL MSG_OUT( 'BLANK', ' ', STATUS )
               CALL AIF_GET0I( 'XCOORD', 1, 1, IDIMS( 1 ),
     :                          XCOORD, STATUS )
               CALL AIF_GET0I( 'YCOORD', 1, 1, IDIMS( 2 ),
     :                          YCOORD, STATUS )

*             get new value for the chosen pixel from the interface,
               CALL PAR_GET0R( 'NEWVAL', NEWVAL, STATUS )

*             check for error before accessing pointer
               IF ( STATUS .EQ. SAI__OK ) THEN

                  CALL CHPIXSUB( IDIMS(1), IDIMS(2), %VAL( PNTRO ),
     :                           XCOORD, YCOORD, NEWVAL, OLDVAL,
     :                           STATUS )


*                report result to user
                  CALL MSG_OUT( 'BLANK', ' ',STATUS )
                  CALL MSG_SETR( 'OLDVAL', OLDVAL )
                  CALL MSG_OUT( 'INPUT_OLDVAL',
     :               'Old pixel value was = ^OLDVAL', STATUS )

                  CALL MSG_SETR( 'NEWVAL', NEWVAL )
                  CALL MSG_OUT( 'INPUT_NEWVAL',
     :               'New pixel value is  = ^NEWVAL', STATUS )
                  CALL MSG_OUT( 'BLANK', ' ',STATUS )

*             end of if-no-error-before-calling-subroutine check
               END IF

*             ask user if he wants another pixel deglitched
               CALL PAR_GET0L( 'AGAIN', AGAIN, STATUS )

*             cancel previous values of XCOORD, YCOORD, AGAIN
               CALL PAR_CANCL( 'XCOORD', STATUS )
               CALL PAR_CANCL( 'YCOORD', STATUS )
               CALL PAR_CANCL( 'NEWVAL', STATUS )
               CALL PAR_CANCL( 'AGAIN', STATUS )

*          end of do-while-another-pixel-to-be-deglitched loop
            END DO

*          tidy up the output data structure
            CALL NDF_ANNUL( LOCO, STATUS )

*       end of if-no-error-getting-output-data-frame check
         END IF

*       tidy up the input data structure
         CALL NDF_ANNUL( LOCI, STATUS )

*    end of if-no-error-getting-input-data-frame check
      END IF


*    end

      END
