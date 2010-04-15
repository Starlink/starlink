
*+  MOSCOR - determines correct d.c. sky offsets for a pair of mosaic images

      SUBROUTINE MOSCOR ( STATUS )

*    Description :
*
*     This routine determines the best d.c. sky offsets
*     for a pair of mosaic images.
*
*    Invocation :
*
*     CALL MOSCOR ( STATUS )
*
*    Parameters :
*
*     INPIC1  =  IMAGE( READ )
*         First image of mosaic pair
*     INPIC2  =  IMAGE( READ )
*         Second image of mosaic pair
*     NXOFF  =  INTEGER( READ )
*         Nominal x spatial offset between the images
*     NYOFF  =  INTEGER( READ )
*         Nominal y spatial offset between the images
*     USEWHAT = LITERAL ( READ)
*         Use mean, median or mode value
*     APPLY  =  LOGICAL( READ )
*         Apply correction to 2nd image
*     OUTPIC  =  STRUCTURE( READ )
*         Name of output image
*     MOSAIC  =  LOGICAL( READ )
*         Mosaic corrected images together
*     OUTPIC2  =  STRUCTURE( READ )
*         Name of output mosaiced image
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE (UKTH::MJM)
*
*    History :
*
*     06-01-1988 : First implementation (UKTH::MJM)
*     02-01-1990 : Modified this from MOFF (JACH::CAA)
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     23-Jun-1994  Changed STR$ to CHR_ (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE           ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'       ! global SSE definitions
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'
      INCLUDE 'CHR_ERR'

*    Status :

      INTEGER STATUS          ! global status parameter

*    Local Constants :

      INTEGER NDIMS           ! input image dimensionality
      PARAMETER ( NDIMS = 2 ) ! 2-d images only

*    Local variables :

      INTEGER
     :    LOCI1,              ! locator for first input data structure
     :    LOCI2,              ! locator for second  "     "      "
     :    LOCO,               ! locator for output IMAGE structure
     :    LOCO2,              ! locator for output IMAGE structure
     :    IDIMS1( NDIMS ),    ! dimensions of first input image
     :    IDIMS2( NDIMS ),    !      "      " second  "     "
     :    ACTDIM,             ! actual dimensions from NDF_DIM
     :    NELEMENTS,          ! number of elements mapped by NDF_MAP
     :    PNTRI1,             ! pointer to first input data array component
     :    PNTRI2,             !    "     " second  "     "    "       "
     :    NXOFF,              ! nominal x offset between the images
     :    NYOFF,              !    "    y    "      "     "     "
     :    ODIMS( NDIMS ),     ! dimensions of output DATA_ARRAYs
     :    ODIMS2( NDIMS ),    ! dimensions of output DATA_ARRAYs
     :    PNTRO,              ! pointer to output DATA_ARRAY
     :    PNTRO2              ! pointer to output DATA_ARRAY

      REAL
     :    DCOFF,              ! calculated d.c. offset between first and second
     :	  VALUEA,
     :	  VALUEB

      CHARACTER*80
     :    USEWHAT

      LOGICAL                 ! true if :
     :    APPLY,              ! correction is to be applied
     :    MOSAIC              ! mosaic is to be made

*-
*    check status on entry - if not ok then return
      IF( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    get first input image
      CALL GETINP( 'INPIC1', LOCI1, STATUS )

*    get second input image
      CALL GETINP( 'INPIC2', LOCI2, STATUS )

*    check for error before continuing
      IF ( STATUS .EQ. SAI__OK ) THEN

*       map the input images and get dimensions
         CALL NDF_MAP( LOCI1, 'DATA', '_REAL', 'READ',
     :                  PNTRI1, NELEMENTS, STATUS )
         CALL NDF_DIM( LOCI1, NDIMS, IDIMS1, ACTDIM, STATUS )

         CALL NDF_MAP( LOCI2, 'DATA', '_REAL', 'READ',
     :                  PNTRI2, NELEMENTS, STATUS )
         CALL NDF_DIM( LOCI2, NDIMS, IDIMS2, ACTDIM, STATUS )

*       check for error before continuing
         IF ( STATUS .EQ. SAI__OK ) THEN

*          get the nominal x and y offsets
            CALL AIF_GET0I( 'NXOFF', 0, -(IDIMS1(1)-1), (IDIMS1(1)-1),
     :                       NXOFF, STATUS )
            CALL AIF_GET0I( 'NYOFF', 0, -(IDIMS2(2)-1), (IDIMS1(2)-1),
     :                       NYOFF, STATUS )

*          ask if want to use mean, median or mode
            CALL PAR_GET0C( 'USEWHAT', USEWHAT, STATUS )
            CALL CHR_UCASE( USEWHAT )

*          ask if user wants to apply correction to 2nd image
            CALL PAR_GET0L( 'APPLY', APPLY, STATUS )

*          ask if user wants to mosaic corrected images together
	    IF( APPLY) THEN
              CALL PAR_GET0L( 'MOSAIC', MOSAIC, STATUS )
	    END IF

*          check for error before continuing
            IF ( STATUS .EQ. SAI__OK ) THEN

*             see if applying correction to 2nd image was requested
               IF ( APPLY ) THEN

*                set the output image dimensions
	          ODIMS( 1) = IDIMS2( 1)
	          ODIMS( 2) = IDIMS2( 2)

*                create the output image and get a title for it
                  CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS, LOCO,
     :	                       STATUS )

*                check for error
                  IF( STATUS .EQ. SAI__OK ) THEN

*                  find and map output DATA_ARRAY component
                    CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                            PNTRO, NELEMENTS, STATUS )

	          END IF

	          IF( MOSAIC) THEN

*                  set the output image dimensions
	            ODIMS2( 1) = MAX( IDIMS1( 1),
     :                           ( IDIMS2( 1)+ABS( NXOFF)))
	            ODIMS2( 2) = MAX( IDIMS1( 2),
     :                           ( IDIMS2( 2)+ABS( NYOFF)))

*                  create the output image and get a title for it
                    CALL CREOUT( 'OUTPIC2', 'OTITLE', NDIMS, ODIMS2,
     :	                         LOCO2, STATUS )

	            CALL MSG_OUT( 'BLANK', ' ', STATUS)

*                  check for error
                    IF( STATUS .EQ. SAI__OK ) THEN

*                     find and map output DATA_ARRAY component
                       CALL NDF_MAP( LOCO2, 'DATA', '_REAL', 'WRITE',
     :                               PNTRO2, NELEMENTS, STATUS )

	            END IF

	         END IF

               END IF

*             calculate the d.c. sky offset using this x,y offset
               CALL MOSCORSUB( %VAL( PNTRI1 ), IDIMS1( 1), IDIMS1( 2),
     :	                       %VAL( PNTRI2 ), IDIMS2( 1), IDIMS2( 2),
     :	                       NXOFF, NYOFF, USEWHAT, VALUEA, VALUEB,
     :	                       DCOFF, STATUS )

*             write resulting offset to interface
               CALL PAR_PUT0R( 'OFFSETA', VALUEA, STATUS)
               CALL PAR_PUT0R( 'OFFSETB', VALUEB, STATUS)
               CALL PAR_PUT0R( 'DCOFFSET', DCOFF, STATUS)

	       CALL MSG_OUT( 'BLANK', ' ', STATUS)

*             write out the results on return
               CALL MSG_SETR( 'OFFA', VALUEA )
               CALL MSG_SETR( 'OFFB', VALUEB )
	       CALL MSG_OUT( 'MESS',
     : '1st image value = ^OFFA, 2nd image value = ^OFFB',
     :	 STATUS)
	       CALL MSG_OUT( 'BLANK', ' ', STATUS)

               CALL MSG_SETR( 'DCOFF', DCOFF )
               CALL MSG_OUT( 'MOFF_RES2',
     : 'Sky offset from 1st to 2nd image (2nd-1st) is ^DCOFF',
     :   STATUS )
               CALL MSG_OUT( 'BLANK', ' ', STATUS )

*             see if applying correction to 2nd image was requested
               IF ( APPLY ) THEN

*                call subroutine to apply correction to 2nd image
	          CALL MOSCORSUB2( IDIMS2( 1), IDIMS2( 2),
     :	                           %VAL( PNTRI2), ODIMS( 1),
     :                             ODIMS( 2), %VAL( PNTRO),
     :	                           DCOFF, STATUS)

*                release the ouput image
                  CALL NDF_ANNUL( LOCO, STATUS )

	          IF( MOSAIC) THEN

	            CALL MOSCORSUB3( IDIMS1( 1), IDIMS1( 2),
     :                               %VAL( PNTRI1), IDIMS2( 1),
     :                               IDIMS2( 2), %VAL( PNTRI2),
     :	                             ODIMS2( 1), ODIMS2( 2),
     :                               %VAL( PNTRO2),
     :	                             NXOFF, NYOFF, DCOFF, STATUS)

*                  release the ouput image
                    CALL NDF_ANNUL( LOCO2, STATUS )

	          END IF

	       END IF

            END IF

	 END IF


*    end of if-no-error-after-getting-input-images check
      END IF

*    tidy the input images
      CALL NDF_ANNUL( LOCI1, STATUS )
      CALL NDF_ANNUL( LOCI2, STATUS )

*    end
      END
