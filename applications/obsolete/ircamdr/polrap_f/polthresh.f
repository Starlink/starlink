*+  POLTHRESH - create thresholded version of an 4 imageS (new values for
*               pixels outside thresholds are input)

      SUBROUTINE POLTHRESH ( STATUS )

*    Description :
*
*    Invocation :
*
*     CALL POLTHRESH( STATUS )
*
*    Parameters :
*
*     INPIC1  =  IMAGE( READ )
*           Image to be thresholded - 0 DEGREES
*     INPIC2  =  IMAGE( READ )
*           Image to be thresholded - 45 DEGREES
*     INPIC3  =  IMAGE( READ )
*           Image to be thresholded - 22.5 DEGREES
*     INPIC4  =  IMAGE( READ )
*           Image to be thresholded - 67.5 DEGREES
*     OUTPIC1  =  IMAGE( WRITE )
*           Thresholded version of the image - 0 DEGREES
*     OUTPIC2  =  IMAGE( WRITE )
*           Thresholded version of the image - 45 DEGREES
*     OUTPIC3  =  IMAGE( WRITE )
*           Thresholded version of the image - 22.5 DEGREES
*     OUTPIC4  =  IMAGE( WRITE )
*           Thresholded version of the image - 67.5 DEGREES
*     OTITLE  =  CHARACTER( READ )
*           Label for the output image
*     THRLO  =  REAL( READ )
*           This value defines the lower threshold
*
*    Method :
*
*     Check status on entry - return if not o.k.
*     Get input image structure
*     If no error then
*        Try to map a data array component in input structures
*        Get lower threshold value
*        If no error then
*           Create output structures to hold processed image
*           If no error then
*              Map an output data array components
*              If no error then
*                 Call subroutine to do the thresholding
*              Endif
*              Unmap and tidy output structures
*           Endif
*        Endif
*        Unmap and tidy input structures
*     Endif
*     Return
*
*    Authors :
*
*     D.W.T.Baines (ROE::ASOC5)
*     Mark McCaughrean UoE (REVA::MJM)
*     Colin Aspin (JACH::CAA)
*
*    History :
*
*     16-05-1983 : Original version (ROE::ASOC5)
*     22-02-1984 : Modified to include more error checking  (ROE::ASOC5)
*     03-06-1985 : Modified to allow user set values to which numbers
*                : are set if outside thresholds (REVA::MJM)
*     03-07-1986 : Tidied up (REVA::MJM)
*     16-10-1988 : created polthresh from thresh (JACH::CAA)
*     18-May-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     23-June-1994 Changed explicit max/min real value to NUM__MAXR/MINR
*                 from PRIMDAT (SKL@JACH)
*     11-AUG-1994  Changed input DIM arguments to POLTHRESHSUB (SKL@JACH)
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
     :    IDIMS1( NDIMS ),           ! dimensions of input data array
     :    IDIMS2( NDIMS ),           ! dimensions of input data array
     :    IDIMS3( NDIMS ),           ! dimensions of input data array
     :    IDIMS4( NDIMS ),           ! dimensions of input data array
     :    ACTDIM,                    ! actual dimensions from NDF_DIM
     :    NELEMENTS,                 ! number of elements mapped by NDF_MAP
     :    PNTRI1,                    ! pointer to input data array component
     :    PNTRI2,                    ! pointer to input data array component
     :    PNTRI3,                    ! pointer to input data array component
     :    PNTRI4,                    ! pointer to input data array component
     :    PNTRO1,                     !    "     " output  "    "       "
     :    PNTRO2,                     !    "     " output  "    "       "
     :    PNTRO3,                     !    "     " output  "    "       "
     :    PNTRO4                      !    "     " output  "    "       "
      INTEGER
     :	  SUMDIM1,                    ! sum for dimensions 1
     :	  SUMDIM2,                    ! sum for dimensions 2
     :	  NUMPTY,                      ! number of pixels below threshold
     :    LOCI1,                     ! input data structure
     :    LOCI2,                     ! input data structure
     :    LOCI3,                     ! input data structure
     :    LOCI4,                     ! input data structure
     :    LOCO1,                      ! output data structure
     :    LOCO2,                      ! output data structure
     :    LOCO3,                      ! output data structure
     :    LOCO4                       ! output data structure

      REAL
     :    THRLO,                    ! lower threshold value
     :	  RNUMPTY

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    get a locator to input IMAGE type data structure
      CALL GETINP( 'INPIC1', LOCI1, STATUS )
      CALL GETINP( 'INPIC2', LOCI2, STATUS )
      CALL GETINP( 'INPIC3', LOCI3, STATUS )
      CALL GETINP( 'INPIC4', LOCI4, STATUS )

*    check for error
      IF( STATUS .EQ. SAI__OK ) THEN

*       map the DATA_ARRAY component of the input data structure
*       and get dimensions

         CALL NDF_MAP( LOCI1, 'DATA', '_REAL', 'READ',
     :                  PNTRI1, NELEMENTS, STATUS )
         CALL NDF_DIM( LOCI1, NDIMS, IDIMS1, ACTDIM, STATUS )

         CALL NDF_MAP( LOCI2, 'DATA', '_REAL', 'READ',
     :                  PNTRI2, NELEMENTS, STATUS )
         CALL NDF_DIM( LOCI2, NDIMS, IDIMS2, ACTDIM, STATUS )

         CALL NDF_MAP( LOCI3, 'DATA', '_REAL', 'READ',
     :                  PNTRI3, NELEMENTS, STATUS )
         CALL NDF_DIM( LOCI3, NDIMS, IDIMS3, ACTDIM, STATUS )

         CALL NDF_MAP( LOCI4, 'DATA', '_REAL', 'READ',
     :                  PNTRI4, NELEMENTS, STATUS )
         CALL NDF_DIM( LOCI4, NDIMS, IDIMS4, ACTDIM, STATUS )

*       check that all images are the same size ...

	 SUMDIM1 = IDIMS1( 1) + IDIMS2( 1) + IDIMS3( 1) + IDIMS4( 1)
	 SUMDIM2 = IDIMS1( 2) + IDIMS2( 2) + IDIMS3( 2) + IDIMS4( 2)

	 IF( SUMDIM1 .EQ. IDIMS1( 1)*4 .AND.
     :	     SUMDIM2 .EQ. IDIMS2( 2)*4) THEN

*       tell user size of input images
	 CALL MSG_SETI( 'XS', IDIMS1( 1))
	 CALL MSG_SETI( 'YS', IDIMS1( 2))
	 CALL MSG_OUT( 'MESSAGE',
     :                 'Input images are ^XS by ^YS pixels in size',
     :	               STATUS)

*       get the low threshold value
         CALL AIF_GET0R( 'THRLO', 0.0, MINVAL, MAXVAL,
     :                    THRLO, STATUS )

*       check for error so far
         IF ( STATUS .EQ. SAI__OK ) THEN

*          create output image type data structure
            CALL CREOUT( 'OUTPIC1', 'OTITLE', NDIMS, IDIMS1,
     :                    LOCO1, STATUS )
            CALL CREOUT( 'OUTPIC2', 'OTITLE', NDIMS, IDIMS2,
     :                    LOCO2, STATUS )
            CALL CREOUT( 'OUTPIC3', 'OTITLE', NDIMS, IDIMS3,
     :                    LOCO3, STATUS )
            CALL CREOUT( 'OUTPIC4', 'OTITLE', NDIMS, IDIMS4,
     :                    LOCO4, STATUS )

*          check for error
            IF( STATUS .EQ. SAI__OK ) THEN

*             map output DATA_ARRAY component
               CALL NDF_MAP( LOCO1, 'DATA', '_REAL', 'WRITE',
     :                  PNTRO1, NELEMENTS, STATUS )
               CALL NDF_MAP( LOCO2, 'DATA', '_REAL', 'WRITE',
     :                  PNTRO2, NELEMENTS, STATUS )
               CALL NDF_MAP( LOCO3, 'DATA', '_REAL', 'WRITE',
     :                  PNTRO3, NELEMENTS, STATUS )
               CALL NDF_MAP( LOCO4, 'DATA', '_REAL', 'WRITE',
     :                  PNTRO4, NELEMENTS, STATUS )

*             if there have been no errors then perform the thresholding
               IF ( STATUS .EQ. SAI__OK ) THEN

                  CALL POLTHRESHSUB( %VAL( PNTRI1 ), %VAL( PNTRI2 ),
     :	                             %VAL( PNTRI3 ), %VAL( PNTRI4 ),
     :	                             %VAL( PNTRO1 ), %VAL( PNTRO2 ),
     :	                             %VAL( PNTRO3 ), %VAL( PNTRO4 ),
     :                               IDIMS1(1), IDIMS1(2), THRLO,
     :                               NUMPTY, STATUS )

*             tell user number pixel below threshold
	         CALL MSG_OUT( 'BLANK', ' ', STATUS)
	         RNUMPTY = 100.0*NUMPTY/REAL( IDIMS1( 1)*IDIMS1( 2))
	         CALL MSG_SETR( 'NUM', RNUMPTY)
	         CALL MSG_OUT( 'MESS',
     :	  'Percentage number of pixel below threshold = ^NUM % ...',
     :	           STATUS)
	         CALL MSG_OUT( 'BLANK', ' ', STATUS)

               END IF

*             tidy up the output data structure
               CALL NDF_ANNUL( LOCO1, STATUS )
               CALL NDF_ANNUL( LOCO2, STATUS )
               CALL NDF_ANNUL( LOCO3, STATUS )
               CALL NDF_ANNUL( LOCO4, STATUS )

*          end of if-no-error-getting-output-structure check
            END IF

*       end of if-no-error-after-getting-threshold-values check
         END IF

*       else of if-input-images-are-different-sizes
	 ELSE

	  CALL MSG_OUT( 'MESS',
     :                  'Error, input images different sizes ...',
     :	                STATUS)

*       end of if-input-images-are-different-sizes
	 END IF

*       tidy up the input data structure
         CALL NDF_ANNUL( LOCI1, STATUS )
         CALL NDF_ANNUL( LOCI2, STATUS )
         CALL NDF_ANNUL( LOCI3, STATUS )
         CALL NDF_ANNUL( LOCI4, STATUS )

*    end of if-no-error-getting-input-structure check
      END IF

*    end
      END
