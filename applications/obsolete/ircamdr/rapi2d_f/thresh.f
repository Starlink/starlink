
*+  THRESH - create a thresholded version of an image (new values for pixels
*            outside thresholds are input)

      SUBROUTINE THRESH ( STATUS )

*    Description :
*
*     A thresholded version of the input image, specified by %INPIC,
*     is written to the output image, specified by %OUTPIC. The limits
*     for the thresholding are given by %THRLO and %THRHI, any values
*     in the input image above the value of %THRHI will be set to one
*     user specified value , and anything below the %THRLO will be set
*     set to another user specified value, in the output image.
*
*    Invocation :
*
*     CALL THRESH( STATUS )
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*           Image to be thresholded
*     OUTPIC  =  IMAGE( WRITE )
*           Thresholded version of the image
*     OTITLE  =  CHARACTER( READ )
*           Label for the output image
*     THRLO  =  REAL( READ )
*           This value defines the lower threshold
*     THRHI  =  REAL( READ )
*           This value defines the upper threshold
*     NEWLO  =  REAL( READ )
*           This defines the value to which all numbers below THRLO are set
*     NEWHI  =  REAL( READ )
*           This defines the value to which all numbers above THRHI are set
*
*    Method :
*
*     Check status on entry - return if not o.k.
*     Get input image structure
*     If no error then
*        Try to map a data array component in input structure
*        Get upper threshold value
*        Get value to replace pixels above upper threshold
*        Get lower threshold value
*        Get value to replace pixels below lower threshold
*        If no error then
*           Create output structure to hold processed image
*           If no error then
*              Map an output data array component
*              If no error then
*                 Call subroutine to do the thresholding
*              Endif
*              Unmap and tidy output structure
*           Endif
*        Endif
*        Unmap and tidy input structure
*     Endif
*     Return
*
*    Authors :
*
*     D.W.T.Baines (ROE::ASOC5)
*     Mark McCaughrean UoE (REVA::MJM)
*
*    History :
*
*     16-05-1983 : Original version (ROE::ASOC5)
*     22-02-1984 : Modified to include more error checking  (ROE::ASOC5)
*     03-06-1985 : Modified to allow user set values to which numbers
*                : are set if outside thresholds (REVA::MJM)
*     03-07-1986 : Tidied up (REVA::MJM)
*     12-Apr-1994  Changed DAT, CMP calls to NDF (SKL@JACH)
*     27-May-1994 Changed explicit max/min real value to NUM__MAXR/MINR
*                 from PRIMDAT (SKL@JACH)
*     15-Aug-1994  Changed input DIM arguments for THRESHSUB (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE                 ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'             ! SSE global definitions
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'
      INCLUDE 'PRM_PAR'             ! PRIMDAT constants

*    Status :

      INTEGER STATUS                ! global status parameter

*    Local Constants :

      INTEGER
     :    NDIMS                     ! image dimensionality
      PARAMETER ( NDIMS  =  2 )     ! 2-d images only

      REAL
     :    MAXVAL,                   ! maximum allowable threshold value
     :    MINVAL                    ! minimum     "         "       "
      PARAMETER( MAXVAL  = NUM__MAXR )! maximum real
      PARAMETER( MINVAL  = NUM__MINR )! minimum real

*    Local variables :

      INTEGER
     :    LOCI,                     ! locator for input data structure
     :    LOCO,                     ! locator for output data structure
     :    NELEMENTS,                ! number elements mapped by NDF_MAP
     :    IDIMS( NDIMS ),           ! dimensions of input data array
     :    NDIM,                     ! number of dimensions from NDF_DIM
     :    PNTRI,                    ! pointer to input data array component
     :    PNTRO                     !    "     " output  "    "       "

      REAL
     :    THRLO,                    ! lower threshold value
     :    THRHI,                    ! upper     "       "
     :    NEWLO,                    ! new value for pixels below THRLO
     :    NEWHI                     ! new value for pixels above THRHI

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF


*    get a locator to input IMAGE type data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    check for error
      IF( STATUS .EQ. SAI__OK ) THEN

*       map the DATA_ARRAY component of the input data structure
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                 PNTRI, NELEMENTS, STATUS )

         CALL NDF_DIM( LOCI, NDIMS, IDIMS, NDIM, STATUS )

*       get the low threshold value
         CALL AIF_GET0R( 'THRLO', 0.0, MINVAL, MAXVAL,
     :                    THRLO, STATUS )

*       get the value to which numbers below THRLO are set
         CALL AIF_GET0R( 'NEWLO', THRLO, MINVAL, MAXVAL,
     :                    NEWLO, STATUS )

*       get the high threshold value
         CALL AIF_GET0R( 'THRHI', 0.0, MINVAL, MAXVAL,
     :                    THRHI, STATUS )

*       get the value to which numbers above THRHI are set
         CALL AIF_GET0R( 'NEWHI', THRHI, MINVAL, MAXVAL,
     :                    NEWHI, STATUS )

*       check for error so far
         IF ( STATUS .EQ. SAI__OK ) THEN

*          create output image type data structure
            CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, IDIMS,
     :                    LOCO, STATUS )

*          check for error
            IF( STATUS .EQ. SAI__OK ) THEN

*             map output DATA_ARRAY component
               CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                        PNTRO, NELEMENTS, STATUS )

*             if there have been no errors then perform the thresholding
               IF ( STATUS .EQ. SAI__OK ) THEN

                  CALL THRESHSUB( %VAL( PNTRI ), %VAL( PNTRO ),
     :                    IDIMS(1), IDIMS(2), THRLO, THRHI, NEWLO,
     :                    NEWHI, STATUS )

               END IF

*             tidy up the output data structure
               CALL NDF_ANNUL( LOCO, STATUS )

*          end of if-no-error-getting-output-structure check
            END IF

*       end of if-no-error-after-getting-threshold-values check
         END IF

*       tidy up the input data structure
         CALL NDF_ANNUL( LOCI, STATUS )

*    end of if-no-error-getting-input-structure check
      END IF


*    end
      END
