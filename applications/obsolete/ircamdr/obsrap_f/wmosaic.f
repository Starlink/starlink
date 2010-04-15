*+  WMOSAIC - merges several non-congruent images into one output image
*             with weights

      SUBROUTINE WMOSAIC( STATUS )

*    Description :
*
*     Up to 50 non-congruent images may be input, along with their relative
*     offsets from the first image, and these are then mosaiced together
*     into one (usually larger) output frame. Where the frames overlap, a
*     a mean value is inserted into the output image. Bad pixels are
*     optionally handled - bad data in one input may be replaced by good
*     data from another. The user can specify weights to be used for each
*     image ...
*     e.g. OUT = (IMAGE1*WEIGHT1+IMAGE2*WEIGHT2+...)/(WEIGHT1+WEIGHT+...)
*
*    Parameters :
*
*     NUMBER  =  INTEGER( READ )
*           Number of images to be merged
*     INPIC( NUMBER )  =  IMAGE( READ )
*           Images to be mosaiced together
*     IDIMS( NDIMS, NUMBER )  =  INTEGER( READ )
*           Dimensions of the input images (need not be same)
*     USEBAD  =  LOGICAL( READ )
*           True if bad pixel replacement is to be done
*     VORM  =  CHAR( READ )
*           Set to V if bad value used, M if bad mask used
*     BADVAL  =  REAL( READ )
*           Value in input data taken to denote bad pixels
*     BADPIC  =  IMAGE( READ )
*           Image mapping the bad pixels - 1=bad, 0=good
*     OVERLAP = LOGICAL( READ)
*           Action in overlap regions
*     OUTPIC  =  IMAGE( WRITE )
*           Merged image
*     ODIMS( NDIMS )  =  INTEGER( WRITE )
*           Calculated output dimensions of merged image
*     OTITLE  =  CHARACTER( READ )
*           Label for the output image
*     XOFFSET( NUMBER )  =  INTEGER( READ )
*           x offset of Nth image from first
*     YOFFSET( NUMBER )  =  INTEGER( READ )
*           y offset of Nth image from first
*     WEIGHT( NUMBER)  =  REAL( READ )
*           weight of this image
*
*    Method :
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*
*    History :
*
*     24-12-1986 : Larger version of MOSAIC (50 frames max.) (UKTH::MJM)
*     27-12-1986 : Added bad pixel value option (UKTH::MJM)
*     18-05-1987 : Added option to average or select overlap values (UKTH::CAA)
*     07-03-1989 : Added weights to this version (JACH::CAA)
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE             ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'         ! SSE global variables
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'

*    Status :

      INTEGER STATUS            ! global status parameter

*    Local Constants :

      INTEGER
     :  NDIMS,                  ! dimensionality of images
     :  MAXFRAMES               ! maximum number of frames allowed

      PARAMETER ( NDIMS = 2 )   ! 2-d images only
      PARAMETER ( MAXFRAMES = 50 ) ! sensible (?) maximum

*    Local variables :

      INTEGER
     :    NUMBER,                      ! number of frames to be merged
     :    LOCI( MAXFRAMES ),           ! locator for input data structure
     :    LOCB,                        ! locator for bad pixel image structure
     :    LOCO,                        ! locator for output data structure
     :    LOCT,                        ! locator for temporary image mask
     :    PLACE,                       ! place holder for temporary NDF
     :    IDIMS( NDIMS, MAXFRAMES ),   ! dimensions of input DATA_ARRAYs
     :    DIMS( NDIMS ),               ! same (dummy variables)
     :    ACTDIM,                      ! actual dimensions from NDF_DIM
     :    NELEMENTS,                   ! number of elements mapped by NDF_MAP
     :    PNTRI( MAXFRAMES ),          ! pointers to input DATA_ARRAYs
     :    XOFFSET( MAXFRAMES ),        ! x offset of Nth frame from first
     :    YOFFSET( MAXFRAMES ),        ! y   "     "  "    "     "    "
     :    PNTRB,                       ! pointer to bad pixel DATA_ARRAY
     :    PNTRO                        !    "     " output        "
      INTEGER
     :    ODIMS( NDIMS ),              ! dimensions of output DATA_ARRAY
     :    LBND( 2 ),                   ! lower bounds temporary array
     :    PNTRT,                       ! pointer to image mask array
     :    MINX,                        ! minimum x offset from first frame
     :    MINY,                        !    "    y   "      "    "     "
     :    MAXX,                        ! maximum x   "      "    "     "
     :    MAXY,                        !    "    y   "      "    "     "
     :    NLOC,                        ! no. frames successfully 'located'
     :    NMAP,                        !  "     "        "        mapped
     :    I, J, K, L, M                ! counters

      DATA LBND / 1, 1 /

      REAL
     :    BADVAL,                      ! value used to denote bad pixels
     :    WEIGHT( MAXFRAMES )          ! weight for this image

      CHARACTER*20
     :    INPARAM( MAXFRAMES )         ! array containing INPICn parameters

      CHARACTER*1
     :    VORM                         ! V if bad values used, M if bad mask

      LOGICAL                          ! true if :
     :    USEBAD,                      ! bad pixel replacement to be done
     :    OVERLAP                      ! overlap select or average

*    Local data :

      DATA  INPARAM / 'INPIC1', 'INPIC2', 'INPIC3', 'INPIC4',
     :                'INPIC5', 'INPIC6', 'INPIC7', 'INPIC8',
     :                'INPIC9', 'INPIC10', 'INPIC11', 'INPIC12',
     :                'INPIC13', 'INPIC14', 'INPIC15', 'INPIC16',
     :                'INPIC17', 'INPIC18', 'INPIC19', 'INPIC20',
     :                'INPIC21', 'INPIC22', 'INPIC23', 'INPIC24',
     :                'INPIC25', 'INPIC26', 'INPIC27', 'INPIC28',
     :                'INPIC29', 'INPIC30', 'INPIC31', 'INPIC32',
     :                'INPIC33', 'INPIC34', 'INPIC35', 'INPIC36',
     :                'INPIC37', 'INPIC38', 'INPIC39', 'INPIC40',
     :                'INPIC41', 'INPIC42', 'INPIC43', 'INPIC44',
     :                'INPIC45', 'INPIC46', 'INPIC47', 'INPIC48',
     :                'INPIC49', 'INPIC50' /

*-
*    check status on entry - return if not ok
      IF( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    start by getting number of frames to be merged together
      CALL AIF_GET0I( 'NUMBER', 2, 2, MAXFRAMES, NUMBER, STATUS )

*    initialise counter variables
      I  =  0
      NLOC  =  0
      NMAP  =  0

*    now get the required number of DATA_ARRAYs
      DO WHILE ( I .LT. NUMBER .AND. STATUS .EQ. SAI__OK )

*       increment frame counter by one
         I  =  I + 1

*       tell user which number frame is required
         CALL MSG_SETI( 'NEXT', I )
         CALL MSG_OUT( 'NEXT_FRAME',
     :                 'Input frame number ^NEXT', STATUS )

*       get a locator to an IMAGE type data structure
         CALL GETINP( INPARAM( I ), LOCI( I ), STATUS )

*       check for error before mapping data
         IF ( STATUS .EQ. SAI__OK ) THEN

*          frame successfully located - increment counter by one
            NLOC  =  NLOC + 1

*          map in its DATA_ARRAY component and get dimensions
            CALL NDF_MAP( LOCI( I ), 'DATA', '_REAL', 'READ',
     :                  PNTRI( I ), NELEMENTS, STATUS )
            CALL NDF_DIM( LOCI( I ), NDIMS, DIMS, ACTDIM, STATUS )

*          check for error before continuing
            IF ( STATUS .EQ. SAI__OK ) THEN

*             array successfully mapped - increment counter by one
               NMAP  =  NMAP + 1

*             copy current input image dimensions to store
               IDIMS( 1, I )  =  DIMS( 1 )
               IDIMS( 2, I )  =  DIMS( 2 )

*             for all but the first frame, get the offsets of the current
*             frame from the first, setting first offsets to zero, and then
*             cancel parameter
               IF( I .NE. 1 ) THEN
                  CALL PAR_GET0I( 'XOFFSET', XOFFSET( I ), STATUS )
                  CALL PAR_GET0I( 'YOFFSET', YOFFSET( I ), STATUS )
                  CALL PAR_CANCL( 'XOFFSET', STATUS )
                  CALL PAR_CANCL( 'YOFFSET', STATUS )
               ELSE
                  XOFFSET( 1 )  =  0
                  YOFFSET( 1 )  =  0
               END IF

*          get the weight of the image
	       CALL PAR_GET0R( 'WEIGHT', WEIGHT( I), STATUS)
	       CALL PAR_CANCL( 'WEIGHT', STATUS)

*          end of if-no-error-before-getting-offsets check
            END IF

*       end of if-no-error-before-mapping-data check
         END IF

*    end of loop whilst more frames to get and no error
      END DO

*    see if the user wants to do bad pixel replacement
      CALL PAR_GET0L( 'USEBAD', USEBAD, STATUS )

*    if bad pixel replacement is wanted
      IF ( USEBAD .AND. STATUS .EQ. SAI__OK ) THEN

*       see which method is to be used - by recognition of a particular
*       value in the input data itself (V), or by an auxiliary mask image
*       that defines bad pixel in the input data
         CALL AIF_CHOIC( 'VORM', 'V,v,M,m', VORM, STATUS )

*       force to upper-case
         CALL UPCASE( VORM, VORM, STATUS )

*       act accordingly
         IF ( VORM .EQ. 'V' .AND. STATUS .EQ. SAI__OK ) THEN

*          get the value that will be taken to denote a bad pixel if and
*          when it is found in the input data
            CALL PAR_GET0R( 'BADVAL', BADVAL, STATUS )

         ELSE IF ( VORM .EQ. 'M' .AND. STATUS .EQ. SAI__OK ) THEN

*          try to get hold of the structure holding the bad pixel array
            CALL GETINP( 'BADPIC', LOCB, STATUS )

*          if no error then continue
            IF ( STATUS .EQ. SAI__OK ) THEN

*             map the data component and get dimensions
               CALL NDF_MAP( LOCB, 'DATA', '_REAL', 'READ',
     :                  PNTRB, NELEMENTS, STATUS )
               CALL NDF_DIM( LOCB, NDIMS, DIMS, ACTDIM, STATUS )

*          end of if-no-error-before-mapping-bad-pixel-array check
            END IF

*       end of if-bad-value-recognition-to-be-used check
         END IF

*    end of if-bad-pixel-replacement-wanted check
      END IF

*    see if the user wants to select overlap pixels or average them
      CALL PAR_GET0L( 'OVERLAP', OVERLAP, STATUS )

*    continue if no error so far
      IF ( STATUS .EQ. SAI__OK ) THEN

*       work out the size of the output frame to be created -
*       first sort out the maximum and minimum offsets :
*       initialise the maxima and minima values
         MINX  =  0
         MINY  =  0
         MAXX  =  0
         MAXY  =  0

*       loop round for each input frame
          DO  J  =  1, NUMBER

*          compare the current x offset with minimum found so far -
*          if it is smaller, then make it the new minimum
            IF( XOFFSET( J ) .LT. MINX ) THEN
               MINX  =  XOFFSET( J )
            ENDIF

*          similarly for the minimum y offset
            IF( YOFFSET( J ) .LT. MINY ) THEN
               MINY  =  YOFFSET( J )
            ENDIF

*          just use MAX function for the maxima, as we don't need
*          to record which frame was the maximum one - maxima are
*          found from the offset plus frame size relative to first
*          frame
            MAXX  =  MAX( MAXX, IDIMS( 1, J ) + XOFFSET( J ) )
            MAXY  =  MAX( MAXY, IDIMS( 2, J ) + YOFFSET( J ) )

*       end of loop round all input frame offsets
         END DO

*       calculate size of output frame from the extrema in the
*       offset values
         ODIMS( 1 )  =  MAXX - MINX
         ODIMS( 2 )  =  MAXY - MINY

*       inform user of output array dimensions
         CALL MSG_SETI( 'NEWXDIM', ODIMS( 1 ) )
         CALL MSG_SETI( 'NEWYDIM', ODIMS( 2 ) )
         CALL MSG_OUT( 'NEWDIMS',
     :    'Output array size is ^NEWXDIM by ^NEWYDIM', STATUS )

*       redefine offsets to be relative to extreme minimum image in each
*       direction as given by FRAMEMINX and FRAMEMINY - if FRAMEMINX or
*       FRAMEMINY are anything other than 1 (i.e. not the first frame),
*       then MINX and/or MINY will be negative, and thus subtracting (say)
*       MINX from XOFFSET( K ) will make XOFFSET( K ) bigger.
         DO  K  =  1, NUMBER
            XOFFSET( K )  =  XOFFSET( K ) - MINX
            YOFFSET( K )  =  YOFFSET( K ) - MINY
         END DO

*       now get the output array
         CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS, LOCO, STATUS )

*       if no error so far then continue
         IF ( STATUS .EQ. SAI__OK ) THEN

*          map a DATA_ARRAY component
            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                    PNTRO, NELEMENTS, STATUS )

*          check status before continuing
            IF ( STATUS .EQ. SAI__OK ) THEN

*             create some temporary workspace to hold the image mask
               CALL NDF_TEMP( PLACE, STATUS )
               CALL NDF_NEW( '_REAL', NDIMS, LBND, ODIMS, PLACE, LOCT,
     :                       STATUS )

*             check status before continuing
               IF ( STATUS .EQ. SAI__OK ) THEN

*                map the pointer of this workspace
                  CALL NDF_MAP( LOCT, 'DATA', '_REAL', 'WRITE',
     :                          PNTRT, NELEMENTS, STATUS )

*                check status before accessing pointers
                  IF ( STATUS .EQ. SAI__OK ) THEN

*                   set all the pixels of both the output array and the
*                   image mask to be zero using ZERO2D from KERGEN
                     CALL ZERO2D( ODIMS( 1), ODIMS( 2), %VAL( PNTRO ),
     :                            STATUS )
                     CALL ZERO2D( ODIMS( 1), ODIMS( 2), %VAL( PNTRT ),
     :                            STATUS )

*                   see if bad pixel replacement is wanted - we need to
*                   call different subroutines if it is
                     IF ( USEBAD ) THEN

*                      see if it is by value
                        IF ( VORM .EQ. 'V' ) THEN

*                         add each image into the (big) output image, updating
*                         the image mask at the same time, using MOSAIC_ADDBV,
*                         which does bad pixel replacement also
                           DO  L  =  1, NUMBER

*                            copy back the current input array dimensions
                              DIMS( 1 )  =  IDIMS( 1, L )
                              DIMS( 2 )  =  IDIMS( 2, L )

*                            call the working subroutine
                              CALL WMOSAIC_ADDBV( %VAL( PNTRI( L ) ),
     :                         DIMS( 1), DIMS( 2), BADVAL, XOFFSET( L ),
     :                         YOFFSET( L ), WEIGHT( L ), %VAL( PNTRO ),
     :                         %VAL( PNTRT ), ODIMS( 1), ODIMS( 2),
     :	                       OVERLAP, STATUS )

                           END DO

*                      else it is to be done by mask
                        ELSE

*                         add each image into the (big) output image, updating
*                         the image mask at the same time, using MOSAIC_ADDBM,
*                         which does bad pixel replacement also
                           DO  L  =  1, NUMBER

*                            copy back the current input array dimensions
                              DIMS( 1 )  =  IDIMS( 1, L )
                              DIMS( 2 )  =  IDIMS( 2, L )

*                            call the working subroutine
                              CALL WMOSAIC_ADDBM( %VAL( PNTRI( L ) ),
     :                         %VAL( PNTRB ), DIMS( 1), DIMS( 2),
     :                         XOFFSET( L ), YOFFSET( L ), WEIGHT( L),
     :                         %VAL( PNTRO ), %VAL( PNTRT ),
     :                         ODIMS( 1), ODIMS( 2), OVERLAP,
     :	                       STATUS )

                           END DO

*                      end of if-bad-value-recognition-to-be-used check
                        END IF

*                   else bad pixel replacement is NOT wanted
                     ELSE

*                      add each image into the (big) output image, updating
*                      the image mask at the same time, using MOSAIC_ADD
                        DO  L  =  1, NUMBER

*                         copy back the current input array dimensions
                           DIMS( 1 )  =  IDIMS( 1, L )
                           DIMS( 2 )  =  IDIMS( 2, L )

*                         call the working subroutine
                           CALL WMOSAIC_ADD(
     :                      %VAL( PNTRI( L ) ), DIMS( 1),
     :                      DIMS( 2), XOFFSET( L ), YOFFSET( L ),
     :	                    WEIGHT( L ), %VAL( PNTRO ), %VAL( PNTRT ),
     :	                    ODIMS( 1), ODIMS( 2), OVERLAP, STATUS)

                        END DO

*                   end of if-bad-pixel-replacement-to-be-done check
                     END IF

*                   now call MOSAIC_DIV to divide each value in the output
*                   array by the number in the corresponding pixel in the
*                   mask array i.e. by the number of images that were added
*                   into that pixel
                     CALL MOSAIC_DIV( %VAL( PNTRO ), ODIMS( 1),
     :                                ODIMS( 2), %VAL( PNTRT ), STATUS )

*                end of if-no-error-before-accessing-pointers check
                  END IF


*             end of if-no-error-after-getting-workspace check
               END IF

*             annul locator to workspace
               CALL NDF_ANNUL( LOCT, STATUS )

*          end of if-no-error-after-mapping-output-array check
            END IF


*       end of if-no-error-after-creating-output check
         END IF

*       annul the locator to the output structure
         CALL NDF_ANNUL( LOCO, STATUS )

*    end of if-no-error-before-creating-output check
      END IF

*    tidy up the bad pixel image if it was used
      IF ( VORM .EQ. 'M' ) THEN

         CALL NDF_ANNUL( LOCB, STATUS )

      END IF

*    tidy up the input images - the number tidied depends on the number
*    input and mapped - errors in the program may cause this to differ
*    from the number of input images requested - first loop round all
*    the images successfully 'located'
      DO  M  =  1, NLOC

*       annul locator and associated parameter to this image
         CALL NDF_ANNUL( LOCI( M ), STATUS )
         CALL PAR_CANCL( INPARAM( M ), STATUS )

*    end of loop round all located images
      END DO


*    return and end
      END
