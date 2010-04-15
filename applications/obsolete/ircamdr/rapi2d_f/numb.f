
*+  NUMB - count up number of elements of an image with absolute values > value

      SUBROUTINE NUMB ( STATUS )

*    Description :
*
*     This routine counts the number of points in an image that have
*     an absolute value greater than the specified value
*
*    Parameters :
*
*     INPIC  = IMAGE( READ )
*           Input image to be tested
*     VALUE  = REAL( READ )
*           Value against which absolute values of image elements will be tested
*
*    Method :
*
*     Check status on entry - return if not o.k.
*     Get input image structure to be examined
*     If no error then
*        Map its data array component
*        Get value to be tested against
*        If no error then
*           Call subroutine to count how many pixels in the image
*            are above the given threshold
*           Output number on return
*        Endif
*        Tidy up input image structure
*     Endif
*     Return
*
*    Authors :
*
*     Dave Baines (ROE::ASOC5)
*     Mark McCaughrean (REVA::MJM)
*
*    History :
*
*     22-02-1984 : Modified to use rearranged subroutine calls (ROE::ASOC5)
*     21-10-1985 : Tidied up to use new subroutine calls and conform
*                : to other programs (REVA::MJM)
*     03-07-1986 : More error checking (REVA::MJM)
*     24-11-1986 : Minor fix in output dimensions statement (HILO::MJM)
*     12-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     27-May-1994  Replaced explicit VAX max real value with NUM__MAXR
*                  from PRIMDAT (SKL@JACH)
*     15-Aug-1994  Changed input DIM arguments for NUMBS (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE             ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'         ! global SSE parameters
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'
      INCLUDE 'PRM_PAR'         ! PRIMDAT constants

*    Status :

      INTEGER STATUS            ! global status parameter

*    Local constants :

      INTEGER
     :    NDIMS                 ! input image dimensionality
      PARAMETER ( NDIMS  =  2 ) ! 2-d arrays only

      REAL
     :    MAXVAL                ! maximum allowable test value
      PARAMETER( MAXVAL  =  NUM__MAXR ) ! maximum real

*    Local variables :

      INTEGER
     :  DIMS( NDIMS ),          ! dimensions of input DATA_ARRAY
     :  NDIM,                   ! number dimensions from NDF_DIM
     :  NELEMENTS,              ! number elements mapped by NDF_MAP
     :  PNTRI,                  ! pointer to input DATA_ARRAY
     :  LOCI,                   ! input IMAGE structure
     :  NUMBER                  ! number of points above set limit

      REAL
     :  VALUE                   ! input value to be tested against


*-
*    check for error on entry - return if not o.k.
      IF( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    get locator to input IMAGE type data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    check for error before continuing
      IF ( STATUS .EQ. SAI__OK ) THEN

*       map input DATA_ARRAY component
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )

         CALL NDF_DIM( LOCI, NDIMS, DIMS, NDIM, STATUS)

*       get value to be tested against
         CALL AIF_GET0R( 'VALUE', 0.0, 0.0, MAXVAL, VALUE, STATUS )

*       check for error before accessing pointer
         IF ( STATUS .EQ. SAI__OK ) THEN

*          call NUMBS to count up the values
            CALL NUMBS( %VAL( PNTRI ), DIMS(1), DIMS(2), VALUE, NUMBER,
     :                  STATUS )

*          write out the image dimensions and the number of pixels with
*          absolute values greater than specified
            CALL MSG_OUT( 'BLANK', ' ', STATUS )
            CALL MSG_SETI( 'XDIM', DIMS( 1 ) )
            CALL MSG_SETI( 'YDIM', DIMS( 2 ) )
            CALL MSG_OUT( 'IMAGE_SIZE',
     : ' Image is ^XDIM by ^YDIM pixels', STATUS )
            CALL MSG_SETI( 'NUMBER', NUMBER )
            CALL MSG_SETR( 'VALUE', VALUE )
            CALL MSG_OUT( 'NUM_VALUE',
     : ' Number of elements with absolute values > ^VALUE = ^NUMBER',
     :   STATUS )
            CALL MSG_OUT( 'BLANK', ' ', STATUS )

*       end of if-error-before-accesing-pointers check
         END IF

*       tidy up all the input structure
         CALL NDF_ANNUL( LOCI, STATUS )

*    end of if-no-error-after-getting-input-structure check
      END IF


*    end
      END
