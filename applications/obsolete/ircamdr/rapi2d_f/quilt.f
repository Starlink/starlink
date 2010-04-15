
*+  QUILT - smart version of mosaicing routine

      SUBROUTINE QUILT ( STATUS )

*    Description :
*
*     This routine provides a more sophisticated version of the MOSAIC
*     software for adding many images together into one large output.
*     The images to be added and their respective offsets from a central
*     frame can be input from the interface one by one, or from a free-
*     format file all at once.
*     Bad pixel handling is provided for. This is either by recognition
*     of a specified bad pixel value in each input frame, or by reference
*     to a mask array showing the bad pixel layout for all the frames.
*     Only like-sized frames may be input. The reason for this is that
*     it is difficult to work out how big the output frame needs to be
*     until all the input frames and their offsets have been read in. By
*     confining the frames to be the same size, only the maximum and
*     minimum x and y offsets from the central frame need be input by
*     the user, then the output image size can be worked out from these
*     numbers along with the size of the central frame.
*
*    Invocation :
*
*     CALL QUILT( STATUS )
*
*    Parameters :
*
*     WHERE  =  CHAR( READ )
*         Whether input comes from a F(ile) or the I(nterface)
*     FNAME  =  CHAR( READ )
*         Name of file holding input information
*     NUMBER  =  INTEGER( READ )
*         Number of frames to be mosaiced together
*     INPICI  =  IMAGE( READ )
*         Central image (offset 0,0)
*     MAXX  =  INTEGER( READ )
*         Maximum x offset of any image from central image (must be >= 0)
*     MAXY  =  INTEGER( READ )
*         Maximum y offset of any image from central image (must be >= 0)
*     MINX  =  INTEGER( READ )
*         Minimum x offset of any image from central image (must be =< 0)
*     MINY  =  INTEGER( READ )
*         Minimum y offset of any image from central image (must be =< 0)
*     USEBAD  =  LOGICAL( READ )
*         Whether or not bad pixel handling is wanted
*     OVERLAP  = LOGICAL( READ )
*         Whether overlap region averaged or not ...
*     BADMETH  =  CHAR( READ )
*         Method of bad pixel handling - by V(alue) or M(ask)
*     BADVAL  =  REAL( READ )
*         Value taken to represent bad pixels in data
*     BADPIC  =  IMAGE( READ )
*         Image holding bad pixel mask
*     OUTPIC  =  IMAGE( WRITE )
*         Output image
*     OTITLE  =  CHAR( READ )
*         Title string for output image
*     CURPIC  =  IMAGE( READ )
*         Current image being added to mosaic
*     OFFSETX  =  INTEGER( READ )
*         x offset of current image from central one
*     OFFSETY  =  INTEGER( READ )
*         y offset of current image from central one
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get information input method - File or Interface
*     If from a File then
*        Get name of File holding information
*        Call MFOPEN to open File, read number of frames, max and
*         min x,y offsets, and get locator to central frame
*        If status bad on return then
*           Set up error message that problem was found with file
*        Endif
*     Else from Interface
*        Get number of frames
*        Get central frame
*        Get max and min x,y offsets from this frame
*     Endif
*     If no error so far then
*        Map in the central image
*        If no error so far then
*           Work out size of output image and workspace
*           Ask if bad pixel handling wanted
*           If so then
*              Ask whether by Value or Mask
*              If by Value then
*                 Get value recognised as flagging bad pixels
*              Else by Mask
*                 Get Mask to show bad pixel layout
*                 If status is ok then
*                    Map it onto pointer
*                    Set flag to indicate bad mask got ok
*                 Endif
*              Endif
*           Endif
*           If no error so far then
*              Create output and workspace
*              If no error so far then
*                 Map both output and workspace
*                 If no error so far then
*                    Zero output image and workspace frame
*                    Redefine offsets of central frame to be with respect to
*                     the lower left corner of the output array
*                    If bad pixel handling then
*                       If by Value then
*                          Call MOSAIC_ADDBV to add in central frame
*                       Else
*                          Call MOSAIC_ADDBM to add in central frame
*                       Endif
*                    Else
*                       Call MOSAIC_ADD to add in central frame
*                    Endif
*                    Output message saying central frame done ok
*                    Initialise loop counter to 1 so we look at frames
*                     from 2 to NUMBER
*                    Do while more frames to be input and status is ok
*                       Increment loop counter by one
*                       If input by File then
*                          Call MFNEXT to get offsets and locator to next image
*                          If error on return then
*                             Set error message up
*                          Else if offsets outside range then
*                             Set up error message
*                             Set status bad
*                          Endif
*                       Else
*                          Get offsets and next image locator from interface
*                          Cancel parameters ready for next loop
*                       Endif
*                       If no error so far then
*                          Map next image
*                          If no error so far then
*                             If current image has same dimensions as
*                              central image then
*                                Redefine the offsets of the current frame to
*                                 be with respect to the lower left corner of
*                                 the output frame
*                                If bad pixel handling then
*                                   If by Value then
*                                      Call MOSAIC_ADDBV to insert current frame
*                                   Else by Mask
*                                      Call MOSAIC_ADDBM to insert current frame
*                                   Endif
*                                Else no bad pixel handling
*                                   Call MOSAIC_ADD to insert next frame
*                                Endif
*                                Output message saying that current frame done
*                             Else
*                                Output message that current frame is not same
*                                 size as central and will not be included
*                             Endif
*                          Endif
*                          Tidy up current image
*                       Endif
*                    Enddo
*                    Call MOSAIC_DIV to normalise output image with mask
*                 Endif
*                 Tidy up output image and workspace
*              Endif
*           Endif
*           If the bad mask was got and used ok then
*              Tidy it up
*           Endif
*        Endif
*        Tidy up central image
*     Endif
*     If input came from File then
*        Close file
*     Endif
*     End
*
*    Deficiencies :
*
*     Works with like-sized images only and uses Fortran i/o for getting
*     stuff from a file. Also, SUBPAR_ calls are used in some of the sub-
*     routines to associate a string from the file with an HDS structure.
*     LIB$_ calls are used too. Nasty stuff.
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*
*    History :
*
*     29-12-1986 : First implementation (from MOSAIC) (REVA::MJM)
*     12-APR-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*
*    Type definitions :
*zz

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'NDF_PAR'
      INCLUDE  'NDF_ERR'

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local Constants :

      INTEGER
     :    NDIMS                   ! dimensionality of input images
      PARAMETER( NDIMS  =  2 )    ! 2-d images only

*    Local variables :

      INTEGER
     :    LUN,                    ! Fortran unit number for File
     :    NUMBER,                 ! number of input images to be mosaiced
     :    NELEMENTS,              ! number elements mapped by NDF_MAP
     :    NDIM,                   ! number of dimensions from NDF_DIM
     :    MAXI( 2 ),              ! maximum x,y offsets
     :    MINI( 2 ),              ! minimum  "     "
     :    IDIMS( NDIMS ),         ! dimensions of central  image
     :    ODIMS( NDIMS ),         !      "      " output     "
     :    CDIMS( NDIMS ),         !      "      " current input image
     :    PNTRI,                  ! pointer to central image
     :    PNTRO,                  !    "     " output    "
     :    PNTRM,                  !    "     " bad mask  "
     :    PNTRW,                  !    "     " workspace for addition mask
     :    PNTRC,                  !    "     " current input image
     :    OFFSET( 2 ),            ! x,y offsets from central to current
     :    PLACE,                  ! place holder for temporary array
     :    LBND( 2 ),              ! lower bounds for temporary array
     :    I                       ! counter

      DATA LBND / 1, 1 /

      INTEGER                     ! locators to :
     :    LOCI,                   ! central image structure
     :    LOCO,                   ! output    "       "
     :    LOCM,                   ! bad mask  "       "
     :    LOCW,                   ! workspace         "
     :    LOCC                    ! current   "       "

      REAL
     :    BADVAL                  ! value taken to represent bad pixels

      CHARACTER*1
     :    WHERE,                  ! source of input - F(ile) or I(nterface)
     :    BADMETH                 ! method of bad pixel handling - by V(alue)
                                  ! or by bad M(ask)

      CHARACTER*80
     :    FNAME                   ! name of file holding input information

      LOGICAL                     ! true if :
     :    USEBAD,                 ! bad pixel handling is wanted
     :	  OVERLAP,                ! if overlap regions are averaged or not
     :    MASKOK                  ! bad pixel mask found ok

*    Local data :

      USEBAD  =  .FALSE.
      OVERLAP = .TRUE.
      MASKOK  =  .FALSE.
      BADVAL  =   0.0

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    ask whether input information is to come from a File or from the
*    Interface
      CALL AIF_CHOIC( 'WHERE', 'F,I,f,i', WHERE, STATUS )

*    force to upper-case
      CALL UPCASE( WHERE, WHERE, STATUS )

*    check to see what was chosen
      IF ( WHERE .EQ. 'F' ) THEN

*       input from a File - get the file name to be used
         CALL PAR_GET0C( 'FNAME', FNAME, STATUS )

*       open the file and get the initial information from it, namely
*       the number of frames to be input, the max and min x,y offsets,
*       and a locator to the central frame
         CALL MFOPEN( FNAME, 'INPICI', LUN, LOCI, NUMBER, MAXI,
     :                MINI, STATUS )

*       check status - will be set not ok if it was not possible
*       to find the central frame or some other screw-up
         IF ( STATUS .NE. SAI__OK ) THEN

*          output message to this effect
            CALL MSG_OUT( 'BLANK', ' ', STATUS )
            CALL MSG_SETC( 'FILEN', FNAME )
            CALL MSG_OUT( 'CEN_ERR',
     : ' Some problem with file : ^FILEN - aborting', STATUS )

*       end of if-some-problem-with-File check
         END IF

*    else input from the interface
      ELSE

*       get the number of frames to be input
         CALL AIF_GET0I( 'NUMBER', 2, 2, 250, NUMBER, STATUS )

*       get a locator to the central frame
         CALL GETINP( 'INPICI', LOCI, STATUS )

*       get the max and min x,y offsets from the central frame
         CALL AIF_GET0I( 'MAXX', 0, 0, 10000, MAXI( 1 ), STATUS )
         CALL AIF_GET0I( 'MAXY', 0, 0, 10000, MAXI( 2 ), STATUS )
         CALL AIF_GET0I( 'MINX', 0, -10000, 0, MINI( 1 ), STATUS )
         CALL AIF_GET0I( 'MINY', 0, -10000, 0, MINI( 2 ), STATUS )

*    end of if-input-from-File check
      END IF

*    check status before continuing
      IF ( STATUS .EQ. SAI__OK ) THEN

*       map the central frame in
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )

         CALL NDF_DIM( LOCI, NDIMS, IDIMS, NDIM, STATUS)

*       check status before continuing
         IF ( STATUS .EQ. SAI__OK ) THEN

*          work out size of the output frame and workspace array
            ODIMS( 1 )  =  ( MAXI( 1 )+IDIMS( 1 ) ) - MINI( 1 )
            ODIMS( 2 )  =  ( MAXI( 2 )+IDIMS( 2 ) ) - MINI( 2 )

*          ask whether bad pixel handling is wanted
            CALL PAR_GET0L( 'USEBAD', USEBAD, STATUS )

*          check to see if it is wanted
            IF ( USEBAD ) THEN

*             ask which method is wanted - by Value or Mask
               CALL AIF_CHOIC( 'BADMETH', 'V,M,v,m', BADMETH, STATUS )

*             force to upper-case
               CALL UPCASE( BADMETH, BADMETH, STATUS )

*             check to see which method is wanted
               IF ( BADMETH .NE. 'M' ) THEN

*                get the value to be taken as indicating bad pixels
                  CALL PAR_GET0R( 'BADVAL', BADVAL, STATUS )

*             else mask method wanted
               ELSE

*                get locator to mask
                  CALL GETINP( 'BADPIC', LOCM, STATUS )

*                check status here
                  IF ( STATUS .EQ. SAI__OK ) THEN

*                   map the mask in
                     CALL NDF_MAP( LOCM, 'DATA', '_REAL', 'READ',
     :                           PNTRM, NELEMENTS, STATUS)

*                   set a flag to indicate that the mask has been got
                     MASKOK  =  .TRUE.

*                 else failed to get mask ok
                   ELSE

*                    set flag to indicate this
                      MASKOK  =  .FALSE.

*                 end of if-no-error-after-getting-mask check
                   END IF

*             end of if-method-is-by-Value check
               END IF

*          end of if-bad-pixel-handling-wanted check
            END IF

*          ask user if want to average the overlap region or not
	    CALL PAR_GET0L( 'OVERLAP', OVERLAP, STATUS)

*          check status before continuing
            IF ( STATUS .EQ. SAI__OK ) THEN

*             create output frame and workspace
               CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS, LOCO,
     :                       STATUS )

               CALL NDF_TEMP( PLACE, STATUS)
               CALL NDF_NEW( '_REAL', NDIMS, LBND, ODIMS, PLACE,
     :                       LOCW, STATUS )


*             check status before continuing
               IF ( STATUS .EQ. SAI__OK ) THEN

*                output a progress message
                  CALL MSG_OUT( 'BLANK', ' ', STATUS )
                  CALL MSG_OUT( 'PROG1',
     : ' Now creating and zeroing output image and workspace ...',
     :   STATUS )
                  CALL MSG_OUT( 'BLANK', ' ', STATUS )

*                map both output and workspace
                  CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                           PNTRO, NELEMENTS, STATUS )
                  CALL NDF_MAP( LOCW, 'DATA', '_REAL', 'WRITE',
     :                           PNTRW, NELEMENTS, STATUS )

*                check status before continuing
                  IF ( STATUS .EQ. SAI__OK ) THEN

*                   zero the output image and workspace
                     CALL ZERO2D( ODIMS( 1), ODIMS( 2),
     :                            %VAL( PNTRO ), STATUS )
                     CALL ZERO2D( ODIMS( 1), ODIMS( 2),
     :                            %VAL( PNTRW ), STATUS )

*                   redfine offsets of central frame to be with respect
*                   to the lower left corner of the output array
                     OFFSET( 1 )  =  -MINI( 1 )
                     OFFSET( 2 )  =  -MINI( 2 )

*                   now check if we are handling bad pixels
                     IF ( USEBAD ) THEN

*                      we are - check method
                        IF ( BADMETH .NE. 'M' ) THEN

*                         by value - insert central frame into output
                           CALL MOSAIC_ADDBV( %VAL( PNTRI ), IDIMS( 1),
     :                       IDIMS( 2), BADVAL, OFFSET( 1 ),
     :                       OFFSET( 2 ), %VAL( PNTRO ), %VAL( PNTRW ),
     :	                     ODIMS( 1), ODIMS( 2), OVERLAP, STATUS )

*                      else by mask
                        ELSE

*                         insert central frame into output
                           CALL MOSAIC_ADDBM( %VAL( PNTRI ),
     :                       %VAL( PNTRM ), IDIMS( 1), IDIMS( 2),
     :	                     OFFSET( 1 ), OFFSET( 2 ), %VAL( PNTRO ),
     :	                     %VAL( PNTRW ), ODIMS( 1), ODIMS( 2),
     :	                     OVERLAP, STATUS )

*                      end of if-bad-method-is-by-value check
                        END IF

*                   else not bad pixel handling at all
                     ELSE

*                      insert central frame into output
                        CALL MOSAIC_ADD( %VAL( PNTRI ), IDIMS( 1),
     :                    IDIMS( 2), OFFSET( 1 ), OFFSET( 2 ),
     :	                   %VAL( PNTRO ), %VAL( PNTRW ), ODIMS( 1),
     :	                   ODIMS( 2), OVERLAP, STATUS )

*                   end of if-bad-pixels-are-being-handled check
                     END IF

*                   output message indicating success at central frame
                     CALL MSG_OUT( 'CEN_DONE',
     :          ' Central frame was processed ok ... ', STATUS )

*                   initialise counter at 1 - we have already done first frame
                     I  =  1

*                   now loop round all requested input frames
                     DO WHILE ( I.LT.NUMBER .AND. STATUS.EQ.SAI__OK )

*                      increment counter
                        I  =  I + 1

*                      check if input from File
                        IF ( WHERE .EQ. 'F' ) THEN

*                         get locator and offsets to next frame
                           CALL MFNEXT( LUN,  'CURPIC', LOCC,
     :                                  OFFSET, STATUS )

*                         check status on return
                           IF ( STATUS .NE. SAI__OK ) THEN

*                            set up error message that will be flushed
*                            on exit
                              CALL MSG_SETI( 'I', I )
                              CALL ERR_REP( 'MFN_ERR',
     : ' Some error in reading frame ^I from file - aborting', STATUS )

*                         check offsets returned
                           ELSEIF ( OFFSET( 1 ) .GT. MAXI( 1 ) .OR.
     :                              OFFSET( 1 ) .LT. MINI( 1 ) .OR.
     :                              OFFSET( 2 ) .GT. MAXI( 2 ) .OR.
     :                              OFFSET( 2 ) .LT. MINI( 2 ) ) THEN

*                            set up error message and set bad status
                              STATUS  =  SAI__ERROR
                              CALL MSG_SETI( 'I', I )
                              CALL ERR_REP( 'MFN_OFF',
     : ' Offsets obtained for frame ^I were outside range - aborting',
     :   STATUS )

*                         end of if-error-on-return-from-MFNEXT check
                           END IF

*                      else input is from the Interface
                        ELSE

*                         get locator and offsets to next frame
                           CALL MSG_SETI( 'I', I )
                           CALL MSG_OUT( 'CUR_IN',
     : ' Next image will be number ^I :', STATUS )
                           CALL GETINP( 'CURPIC', LOCC, STATUS )
                           CALL AIF_GET0I( 'OFFSETX', 0, MINI( 1 ),
     :                        MAXI( 1 ), OFFSET( 1 ), STATUS )
                           CALL AIF_GET0I( 'OFFSETY', 0, MINI( 2 ),
     :                        MAXI( 2 ), OFFSET( 2 ), STATUS )

*                         cancel parameters ready for next go
                           CALL PAR_CANCL( 'CURPIC', STATUS )
                           CALL PAR_CANCL( 'OFFSETX', STATUS )
                           CALL PAR_CANCL( 'OFFSETY', STATUS )

*                      end of if-input-is-from-File check
                        END IF

*                      check status before continuing
                        IF ( STATUS .EQ. SAI__OK ) THEN

*                         map the current frame in
                           CALL NDF_MAP( LOCC, 'DATA', '_REAL',
     :                        'READ', PNTRC, NELEMENTS, STATUS)

                           CALL NDF_DIM( LOCC, NDIMS, CDIMS, NDIM,
     :                                   STATUS )

*                         check status before continuing
                           IF ( STATUS .EQ. SAI__OK ) THEN

*                            check that the current frame is the same size
*                            as the central frame
                              IF ( CDIMS( 1 ) .EQ. IDIMS( 1 ) .AND.
     :                             CDIMS( 2 ) .EQ. IDIMS( 2 ) ) THEN

*                               redefine the offsets to be with respect to
*                               the lower left corner of the output array
                                 OFFSET( 1 )  =
     :                                       OFFSET( 1 ) - MINI( 1 )
                                 OFFSET( 2 )  =
     :                                       OFFSET( 2 ) - MINI( 2 )

*                               check if we are bad pixel handling
                                 IF ( USEBAD ) THEN

*                                  check which method we are using
                                    IF ( BADMETH .NE. 'M' ) THEN

*                                     by value - insert the current frame
*                                     into the output accordingly
                                       CALL MOSAIC_ADDBV( %VAL( PNTRC ),
     :                                   CDIMS( 1), CDIMS( 2), BADVAL,
     :	                                 OFFSET( 1 ), OFFSET( 2 ),
     :                                   %VAL( PNTRO ), %VAL( PNTRW ),
     :	                                 ODIMS( 1), ODIMS( 2), OVERLAP,
     :	                                 STATUS )

*                                  else by mask
                                    ELSE

*                                     insert current frame into output
                                       CALL MOSAIC_ADDBM( %VAL( PNTRC ),
     :                                   %VAL( PNTRM ), CDIMS( 1),
     :	                                 CDIMS( 2), OFFSET( 1 ),
     :                                   OFFSET( 2 ), %VAL( PNTRO ),
     :	                                 %VAL( PNTRW ), ODIMS( 1),
     :                                   ODIMS( 2), OVERLAP, STATUS )

*                                  end of if-by-value check
                                    END IF

*                               else not handling bad pixels
                                 ELSE

*                                  insert current frame into output without
*                                  any bad pixel handling
                                    CALL MOSAIC_ADD( %VAL( PNTRC ),
     :                                CDIMS( 1), CDIMS( 2),
     :	                              OFFSET( 1 ), OFFSET( 2 ),
     :                                %VAL( PNTRO ), %VAL( PNTRW ),
     :	                              ODIMS( 1), ODIMS( 2),
     :                                OVERLAP, STATUS )

*                               end of if-handling-bad-pixels check
                                 END IF

*                            output message to the effect that the current
*                            frame was processed ok
                              CALL MSG_SETI( 'I', I )
                              CALL MSG_OUT( 'CUR_OK',
     : ' Frame number ^I was processed ok ...', STATUS )

*                            else current frame is not same size
*                            as central frame
                              ELSE

*                               output message to that effect
                                 CALL MSG_OUT( 'BLANK', ' ', STATUS )
                                 CALL MSG_SETI( 'I', I )
                                 CALL MSG_OUT( 'CUR_ERR1',
     : ' Frame number ^I is different size to central one :',
     :   STATUS )
                                 CALL MSG_SETI( 'IXDIM', IDIMS(1) )
                                 CALL MSG_SETI( 'IYDIM', IDIMS(2) )
                                 CALL MSG_OUT( 'CUR_ERR2',
     : ' Central frame is ^IXDIM by ^IYDIM pixels,', STATUS )
                                 CALL MSG_SETI( 'CXDIM', CDIMS(1) )
                                 CALL MSG_SETI( 'CYDIM', CDIMS(2) )
                                 CALL MSG_OUT( 'CUR_ERR3',
     : ' current frame is ^CXDIM by ^CYDIM pixels - leaving it out',
     :   STATUS )

*                            end of if-dimensions-match check
                              END IF

*                         end of if-no-error-after-mapping-current check
                           END IF

*                         tidy current image
                           CALL NDF_ANNUL( LOCC, STATUS )

*                      end of if-no-error-before-mapping-current check
                        END IF

*                   end of loop round all requested input images
                     END DO

*                   output message to let user know what is going on
                     CALL MSG_OUT( 'BLANK', ' ', STATUS )
                     CALL MSG_OUT( 'PROG2',
     :           ' Now normalising output image ...', STATUS )
                     CALL MSG_OUT( 'BLANK', ' ', STATUS )

*                   now normalise output image with respect to the values
*                   stored in the mask in the workspace
                     CALL MOSAIC_DIV( %VAL( PNTRO ), ODIMS( 1),
     :                                ODIMS( 2), %VAL( PNTRW ),
     :                                STATUS )

*                end of if-no-error-after-mapping-output-and-workspace check
                  END IF

*                tidy up output and workspace
                  CALL NDF_ANNUL( LOCO, STATUS )
                  CALL NDF_ANNUL( LOCW, STATUS )

*             end of if-no-error-after-creating-output-and-workspace check
               END IF

*          end of if-no-error-before-creating-output-and-workspace check
            END IF

*          tidy up the bad mask if it has been got and used
            IF ( USEBAD .AND. MASKOK ) THEN

*             tidy it up
               CALL NDF_ANNUL( LOCM, STATUS )

*          end of if-bad-mask-used check
            END IF

*       end of if-no-error-after-mapping-central-frame check
         END IF

*       tidy up the central frame
         CALL NDF_ANNUL( LOCI, STATUS )

*    end of if-no-error-after-getting-central-frame check
      END IF

*    check to see if the input was from a File
      IF ( WHERE .EQ. 'F' ) THEN

*       close the File
         CALL MFCLOSE( LUN, STATUS )

*    end of if-input-was-from-a-File check
      END IF


*    return and end
      END
