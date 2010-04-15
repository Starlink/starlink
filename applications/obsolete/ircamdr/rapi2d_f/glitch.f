
*+  GLITCH - replace bad pixels with local median

      SUBROUTINE GLITCH( STATUS )

*    Description :
*
*     This routine removes bad pixels as specified by the user, and
*     replaces them with the local median. It can be used iteratively
*     until the user is finished cleaning his image. The user specifies
*     position of glitches by x and y coordinates, or by giving the a
*     file name which contains a free-format list giving the x and y
*     positions of the pixels to be deglitched.
*     The glitch list file should comprise the following
*
*       Glitch list for SBRC FPA#005
*       22  45
*       19  56
*       2   30
*       .   .
*       .   .
*       .   .
*       <EOF>
*
*     i.e. a header string that is output to the user, followed by x,y
*     free-format pixel position pairs, terminated just by the end-of-
*     file marker. The header string is output to the user.
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*           Image to be deglitched
*     OUTPIC  =  IMAGE( WRITE )
*           Deglitched version of the image
*     OTITLE  =  CHAR( READ )
*           Label for the output image
*     WHERE  =  CHAR( READ )
*           Source of glitch positions - Interface or File
*     FILENAME  =  CHAR( READ )
*           File containing the free-format glitch list
*     XCOORD  =  INTEGER( READ )
*           x coordinate of pixel to be deglitched
*     YCOORD  =  INTEGER( READ )
*           y coordinate of pixel to be deglitched
*     AGAIN  =  LOGICAL( READ )
*           Whether or not user is prompted for another pixel
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get input image structure
*     If no error so far then
*        Map the DATA_ARRAY component
*        Create an output image structure of the same dimensions as input
*        If no error so far then
*           Map a DATA_ARRAY component
*           If no error so far then
*              Copy input image into output image
*              Ask user whether glitch list is to come from a file
*               or one at a time from the interface
*              If a file list is to be used then
*                 Get name of file containing list
*                 Call GLITCHLIST to do the work
*                 If error on return then
*                    Something up with given file - report error
*                 Endif
*              Else user wants to specify glitches from interface
*                 Do while user wants to do another deglitch
*                    Get coordinates of pixel to be removed
*                    If no error so far then
*                       Call GLITCHSUB to do the work
*                       Output old and new pixel values
*                    Endif
*                    Find out if user wants to deglitch another pixel
*                    Cancel pixel coordinate parameters
*                 Enddo
*              Endif
*           Endif
*           Tidy output structure
*        Endif
*        Tidy input structure
*     Endif
*     End
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*
*    History :
*
*     07-06-1985 : Started A-task implementation (REVA::MJM)
*     28-06-1985 : Changed to continually update the input array
*                : until no more deglitching done, then followed
*                : by request for output array, and copying over.
*                : (REVA::MJM)
*     09-12-1985 : Fixed error checking bug and implemented AIF
*                : dynamic defaulting/checking calls (UKTH::MARK)
*     11-12-1985 : Fixed bug caused by continuous updating of read
*                : only array - changed program order (UKTH::MARK)
*     11-04-1986 : Tidied up and increased error checking (REVA::MJM)
*     24-06-1986 : Included option to deglitch from a free-format list
*                : in a file (REVA::MJM)
*     10-MAR-94    Changed DAT_, CMP_ to NDF_
*
*    Type Definitions :

      IMPLICIT NONE               ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'           ! global SSE definitions
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'           ! global SSE definitions

*    Status :

      INTEGER STATUS              ! global status variable

*    Local Constants :

      INTEGER NDIMS               ! image dimensionality
      PARAMETER ( NDIMS = 2 )     ! defaulted to 2-d

*    Local variables :

      INTEGER
     :    IDIMS( NDIMS ),         ! dimensions of input DATA_ARRAY
     :    NELEMENTS,              ! number of elements mapped
     :    NDIM,                   ! dimensions from NDF_DIM
     :    PNTRI,                  ! pointer to input DATA_ARRAY component
     :    PNTRO,                  ! pointer to output DATA_ARRAY component
     :    XCOORD,                 ! x coordinate of pixel to be deglitched
     :    YCOORD                  ! y     "      "    "   "  "       "

      INTEGER                     ! locators for :
     :    LOCI,                   ! input data structure
     :    LOCO                    ! output data structure

      REAL
     :    OLDVAL,                 ! old value of deglitched pixel
     :    NEWVAL,                 ! new   "   "       "       "
     :    BADVAL

      CHARACTER*1
     :    WHERE                   ! source of glitches - Interface or File

      CHARACTER*80
     :    FILENAME                ! name of file containing glitch list

      LOGICAL                     ! true if :
     :    AGAIN                   ! user wants to do another pixel

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
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
         CALL NDF_DIM( LOCI, NDIMS, IDIMS, NDIM, STATUS )

*       tell user dimensions of input array
         CALL MSG_SETI( 'XDIM', IDIMS(1) )
         CALL MSG_SETI( 'YDIM', IDIMS(2) )
         CALL MSG_OUT( 'INPUT_DIMS',
     :        'Image is ^XDIM by ^YDIM pixels', STATUS )
         CALL MSG_OUT( 'BLANK', ' ', STATUS )

*       now create output IMAGE type data structure with DATA_ARRAY
*       component; also create and get value for DATA_LABEL component
         CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, IDIMS, LOCO, STATUS )

*       if no error so far then continue
         IF ( STATUS .EQ .SAI__OK ) THEN

*          map output DATA_ARRAY component
            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                     PNTRO, NELEMENTS, STATUS )

*          check for error before accessing pointer and entering loop
            IF ( STATUS .EQ. SAI__OK ) THEN

*             copy old array into new array
               CALL COPY2D( IDIMS(1), IDIMS(2), %VAL( PNTRI ),
     :                      %VAL( PNTRO ), STATUS )

*             now find out whether user wants to specify each pixel from
*             the interface, or all at once from a free-format list
               CALL AIF_CHOIC( 'WHERE', 'F,I,f,i,A,a', WHERE, STATUS )

*             force the result to uppercase
               CALL UPCASE( WHERE, WHERE, STATUS )

*             act accordingly
               IF ( WHERE .EQ. 'F' ) THEN

*                user wants to use a free-format list to specify the glitch
*                positions - get the name of the file containing the list
                  CALL PAR_GET0C( 'FILENAME', FILENAME, STATUS )

*                pass all the necessary information to the working
*                subroutine to do the processing
                  CALL GLITCHLIST( IDIMS(1), IDIMS(2), %VAL( PNTRO ),
     :                             FILENAME, STATUS )

*                on return, check the status
                  IF ( STATUS .NE. SAI__OK ) THEN

*                   there was an error in opening the given free-format
*                   file - output error
                     CALL ERR_REP( 'BAD_FILE',
     : ' Something wrong with specified glitch list file - no changes',
     :   STATUS )
                     CALL ERR_FLUSH( STATUS )

*                end of if-error-on-return-from-GLITCHLIST check
                  END IF

*             else user wants to specify each position by hand
               ELSEIF( WHERE .EQ. 'I' ) THEN

*                initialise AGAIN logical
                  AGAIN  = .TRUE.

*                start loop for deglitching
                  DO WHILE ( AGAIN .AND. STATUS .EQ. SAI__OK )

*                   get x and y coordinates of pixel to be deglitched
                     CALL MSG_OUT( 'BLANK', ' ', STATUS )
                     CALL AIF_GET0I( 'XCOORD', 1, 1, IDIMS( 1 ),
     :                                XCOORD, STATUS )
                     CALL AIF_GET0I( 'YCOORD', 1, 1, IDIMS( 2 ),
     :                                YCOORD, STATUS )

*                   check for error before accessing pointer
                     IF ( STATUS .EQ. SAI__OK ) THEN

*                      call GLITCHSUB to work out and put in replacement value
                        CALL GLITCHSUB( IDIMS(1), IDIMS(2),
     :                       %VAL( PNTRO ), XCOORD,
     :                       YCOORD, OLDVAL, NEWVAL, STATUS )

*                      report result to user
                        CALL MSG_OUT( 'BLANK', ' ', STATUS )
                        CALL MSG_SETR( 'OLDVAL', OLDVAL )
                        CALL MSG_OUT( 'INPUT_OLDVAL',
     :         ' Old pixel value was = ^OLDVAL', STATUS )

                        CALL MSG_SETR( 'NEWVAL', NEWVAL )
                        CALL MSG_OUT( 'INPUT_NEWVAL',
     :         ' New pixel value is  = ^NEWVAL', STATUS )
                        CALL MSG_OUT( 'BLANK', ' ', STATUS )

*                   end of if-no-error-before-calling-GLITCHSUB check
                     END IF

*                   ask user if he wants another pixel deglitched
                     CALL PAR_GET0L( 'AGAIN', AGAIN, STATUS )

*                   cancel previous values of XCOORD, YCOORD, AGAIN
                     CALL PAR_CANCL( 'XCOORD', STATUS )
                     CALL PAR_CANCL( 'YCOORD', STATUS )
                     CALL PAR_CANCL( 'AGAIN', STATUS )

*                go back to top of loop to check if user wanted another doing
                  END DO

               ELSE

                  CALL PAR_GET0R( 'BADVAL', BADVAL, STATUS )

                  IF ( STATUS .EQ. SAI__OK ) THEN

                     CALL GLITCHAUTO( IDIMS(1), IDIMS(2), %VAL( PNTRO ),
     :                                BADVAL, STATUS )

                  END IF


*             end of if-positions-to-be-specified-from-a-file check
               END IF

*          end of if-no-error-after-mapping-output check
            END IF

*          tidy up the output data structure
            CALL NDF_ANNUL( LOCO, STATUS )

*       end of if-no-error-after-getting-output-structure check
         END IF

*       tidy up the input data structure
         CALL NDF_ANNUL( LOCI, STATUS )

*    end of if-no-error-after-getting-input-structure check
      END IF


*    end
      END
