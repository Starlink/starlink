*+  GLITCH - Replaces bad pixels in a 2-d data array with the local
*            median

      SUBROUTINE GLITCH( STATUS )
*
*    Description :
*
*     This application removes bad pixels from a 2-d data array, stored
*     in the input IMAGE structure, and replaces them with the local
*     median of the eight (or less at the edges) neighbouring pixels.
*     At least three defined pixels must be in the neighbourhood,
*     otherwise the resultant pixel becomes bad.
*
*     There are three modes of use:
*     1. The application can be used interactively until the user is
*        finished cleaning his array. The user specifies the position of
*        `glitches' or bad pixels by their x and y pixel indices.
*     2. The user can give the name of a file which contains a free-
*        format list giving the x and y positions of the pixels to be
*        deglitched. The glitch list file should look like the following
*        example:
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
*        i.e. a header string that is output to the user, followed by
*        integer x-y pixel position pairs, terminated just by the
*        end-of-file marker. The header string is output to the user.
*     3. The bad (undefined/magic) pixels are automatically deglitched.
*        (Edge, especially corner pixels will need checking if the
*        density of bad pixels is high, because of the constraints
*        mentioned above. Such conditions may require a repeat dose of
*        this application, probably with option 1 or 2.)
*
*     The magic-value method is used for processing bad data.
*
*    Invocation :
*
*     CALL GLITCH( STATUS )
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*         IMAGE structure containing the 2-d data array to be
*           deglitched.
*     OUTPIC  =  IMAGE( WRITE )
*         Output IMAGE structure containing the deglitched version of
*           the data array.
*     OTITLE  =  CHAR( READ )
*         Title for the output IMAGE structure.
*     WHERE  =  CHAR( READ )
*         Source of glitch positions: 'Interface' or 'File' or 'Bad'.
*     FILENAME  =  CHAR( READ )
*         File containing the free-format glitch list.
*     XCOORD  =  INTEGER( READ )
*         x pixel index of the pixel to be deglitched.
*     YCOORD  =  INTEGER( READ )
*         y pixel index of the pixel to be deglitched.
*     AGAIN  =  LOGICAL( READ )
*         Whether or not user is prompted for another pixel.
*
*    Arguments:
*
*     STATUS  = INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not of o.k.
*     Get input IMAGE structure
*     If no error so far then
*        Map the data-array component
*        If no error so far then
*           Create an output IMAGE structure of the same dimensions as
*             input
*           Propagate NDF MORE from the input data file
*           If no error so far then
*              Map a data-array component
*              If no error so far then
*                 Copy input array into output array
*                 If returns with bad status then
*                    Report error context
*                 Else
*                    Ask user whether glitch list is to come from a file
*                      or one at a time from the interface
*                    If a file list is to be used then
*                       Call GLTCLT to do the work
*                    Else if user wants to specify glitches from
*                      interface then
*                       Do while user wants to do another deglitch
*                          Get co-ordinates of pixel to be removed
*                          If no error so far then
*                             Call GLTCSB to do the work
*                             If no error on return then
*                               Output old and new pixel values
*                             Endif
*                          Else
*                             Report error context
*                          Endif
*                          Find out if user wants to deglitch another
*                            pixel
*                          Cancel pixel co-ordinate parameters
*                       Enddo
*                    Else if magic-value pixels are to be deglitched
*                      then
*                       Deglitch bad pixels
*                       If no error on return then
*                          Report number of bad pixels deglitched
*                       Endif
*                    Endif
*                 Endif
*                 Unmap output data array
*              Else
*                 Report error context
*              Endif
*              Tidy output structure
*           Else
*              Report error context
*           Endif
*           Unmap input data array
*        Else
*           Report error context
*        Endif
*        Tidy input structure
*     Else
*        Report error context
*     Endif
*     End
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
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
*     1986 Aug 5 : Renamed algorithm subroutines (GLTCLT and GLTCSB).
*                  Correctly ordered arguments in GLTCLT (2nd to 3rd)
*                  and GLTCSB (2nd to 4th) (RAL::CUR).
*     1986 Aug 29: Added arguments section to the prologue, nearly
*                  conformed to Starlink standards (RAL::CUR).
*     1987 Oct 15: Reordered tidying and extra status check
*                  (RAL::CUR).
*     1988 Mar 17: Referred to `array' rather than `image'. Added
*                  automatic removal of bad pixels (RAL::CUR).
*     1988 Jun 6 : More reporting of error context (RAL::CUR)
*     1988 Jun 30: Obtain filename in GLTCLT (RAL::CUR).
*     1989 Jun 13: Allow for processing primitive NDFs (RAL::CUR)
*     1989 Jul 27: Passed array dimensions as separate variables
*                  to COPY2D, GLTBSB, GLTCLT and GLTCSB (RAL::CUR).
*     1991 Oct 25: Propagates NDF AXIS, HISTORY, LABEL and UNITS
*                  (RAL::CUR).
*     1992 Feb 25: Limited processing of simple NDFs (RAL::CUR).
*     1992 Mar  3: Replaced AIF parameter-system calls by the extended
*                  PAR library (RAL::CUR).
*
*    Type Definitions :
      IMPLICIT NONE            ! no default typing allowed

*    Global constants :
      INCLUDE 'SAE_PAR'        ! global SSE definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PRM_PAR'        ! magic-value definitions
      INCLUDE 'PAR_ERR'        ! parameter-system errors

*    Status :
      INTEGER STATUS

*    Local Constants :
      INTEGER NDIMS            ! array dimensionality
      PARAMETER ( NDIMS = 2 )  ! defaulted to 2-d

*    Local variables :
      INTEGER 
     :  DIMS( NDIMS ),         ! dimensions of input data array
     :  NODEGL,                ! number of deglitched bad pixels
     :  ORIGIN( DAT__MXDIM ),  ! Origin of the data array
     :  PNTRI,                 ! pointer to input data-array component
     :  PNTRO,                 ! pointer to output data-array component
     :  XCOORD,                ! x coordinate of pixel to be deglitched
     :  YCOORD                 ! y     "      "    "   "  "       "

      REAL 
     :  OLDVAL,                ! old value of deglitched pixel
     :  NEWVAL                 ! new   "   "       "       "

      CHARACTER*(DAT__SZLOC)   ! locators for:
     :  LOCDI,                 ! structure containing the input data
                               ! array
     :  LOCDO,                 ! structure containing the output data
                               ! array
     :  LOCI,                  ! input data structure
     :  LOCO                   ! output data structure

      CHARACTER * ( DAT__SZNAM )
     :  DNAMEI,                ! Name of the input data-array component
     :  DNAMEO                 ! Name of the output data-array component

      CHARACTER*9
     :  WHERE                  ! glitch mode - Interface, File or Bad

      LOGICAL                  ! true if :
     :  AGAIN                  ! user wants to do another pixel

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    get a locator to input IMAGE type data structure

      CALL KPG1_GETIM( 'INPIC', LOCI, LOCDI, DNAMEI, ORIGIN, STATUS )
 
*    if no error so far then continue

      IF ( STATUS .EQ. SAI__OK ) THEN

*       map the data-array component of the input data structure

         CALL CMP_MAPN( LOCDI, DNAMEI, '_REAL', 'READ', NDIMS,
     :                  PNTRI, DIMS, STATUS )

*       if no error so far then continue

         IF ( STATUS .EQ. SAI__OK ) THEN

*          tell user dimensions of input array

            CALL MSG_SETI( 'XDIM', DIMS( 1 ) )
            CALL MSG_SETI( 'YDIM', DIMS( 2 ) )
            CALL MSG_OUT( 'INPUT_DIMS',
     :                    'Array is ^XDIM by ^YDIM pixels', STATUS )
            CALL MSG_OUT( 'BLANK', ' ', STATUS )

*          now create output IMAGE type data structure with DATA_ARRAY 
*          component; also create and get value for TITLE component

            CALL KPG1_CROUT( 'OUTPIC', 'OTITLE', NDIMS, DIMS,
     :                       ORIGIN, LOCO, LOCDO, DNAMEO, STATUS )

*          propagate NDF components from the input data file

            CALL KPG1_IMPRG( LOCI, 'AXIS,UNITS', LOCO, STATUS )

*          if no error so far then continue

            IF ( STATUS .EQ .SAI__OK ) THEN

*             map output data-array component

               CALL CMP_MAPN( LOCDO, DNAMEO, '_REAL', 'WRITE',
     :                        NDIMS, PNTRO, DIMS, STATUS )

*             check for error before accessing pointer and entering loop

               IF ( STATUS .EQ. SAI__OK ) THEN

*                copy old array into new array 

                  CALL COPY2D( DIMS( 1 ), DIMS( 2 ), %VAL( PNTRI ),
     :                         %VAL( PNTRO ), STATUS )

                  IF ( STATUS .EQ. SAI__OK ) THEN

*                   now find out whether user wants to specify each
*                   pixel from the interface, or all at once from a
*                   free-format list

                     CALL PAR_CHOIC( 'WHERE', 'Bad',
     :                               'File,Interface,Bad', .FALSE.,
     :                               WHERE, STATUS )

*                   act accordingly

                     IF ( WHERE .EQ. 'FILE' .AND. STATUS .EQ. SAI__OK )
     :                 THEN

*                      user wants to use a free-format list to specify
*                      the glitch positions - pass all the necessary
*                      information to the working subroutine to do the
*                      processing

                        CALL GLTCLT( DIMS( 1 ), DIMS( 2 ), 'FILENAME',
     :                               %VAL( PNTRO ), STATUS )

*                   else user wants to specify each position by hand

                     ELSE IF ( WHERE .EQ. 'INTERFACE' .AND.
     :                         STATUS .EQ. SAI__OK ) THEN

*                      initialise AGAIN logical

                        AGAIN  = .TRUE.

*                      start loop for deglitching

                        DO WHILE ( AGAIN .AND. STATUS .EQ. SAI__OK )

*                         get x and y co-ordinates of pixel to be
*                         deglitched

                           CALL MSG_OUT( 'BLANK', ' ', STATUS )
                           CALL PAR_GDR0I( 'XCOORD', 1, 1, DIMS( 1 ),
     :                                     .FALSE., XCOORD, STATUS )
                           CALL PAR_GDR0I( 'YCOORD', 1, 1, DIMS( 2 ),
     :                                     .FALSE., YCOORD, STATUS )

*                         check for error before accessing pointer

                           IF ( STATUS .EQ. SAI__OK ) THEN

*                            call GLTCSB to work out and put in
*                            replacement value

                              CALL GLTCSB( DIMS( 1 ), DIMS( 2 ), XCOORD,
     :                                     YCOORD, %VAL( PNTRO ),
     :                                     OLDVAL, NEWVAL, STATUS )

*                            on return, check the status

                              IF ( STATUS .EQ. SAI__OK ) THEN

                                 CALL MSG_OUT( 'BLANK', ' ', STATUS )

*                               check to see whether old pixel value was
*                               undefined or not, so that the
*                               appropriate token is created

                                 IF ( OLDVAL .EQ. VAL__BADR ) THEN
                                    CALL MSG_SETC( 'OLDVAL', 'INVALID' )
                                 ELSE
                                    CALL MSG_SETR( 'OLDVAL', OLDVAL )
                                 END IF

*                               report result to user

                                 CALL MSG_OUT( 'INPUT_OLDVAL', 'Old '/
     :                                        /'pixel value was = '/
     :                                        /'^OLDVAL', STATUS )
   
                                 CALL MSG_SETR( 'NEWVAL', NEWVAL )
                                 CALL MSG_OUT( 'INPUT_NEWVAL', 'New '/
     :                                        /'pixel value is  = '/
     :                                        /'^NEWVAL', STATUS )
                                 CALL MSG_OUT( 'BLANK', ' ', STATUS )
  
*                            end of if-error-on-return-from-GLTCSB check

                              END IF

                           ELSE

                              IF ( STATUS .NE. PAR__ABORT .AND.
     :                             STATUS .NE. PAR__NULL ) THEN

*                               announce the error

                                 CALL ERR_REP( 'ERR_GLITCH_PAR1',
     :                             'GLITCH: Error obtaining '/
     :                             /'co-ordinates', STATUS )
                              END IF

*                         end of if-no-error-getting-co-ordinates check

                           END IF

*                         ask user if he wants another pixel deglitched

                           CALL PAR_GET0L( 'AGAIN', AGAIN, STATUS )

*                         cancel previous values of XCOORD, YCOORD,
*                         AGAIN

                           CALL PAR_CANCL( 'XCOORD', STATUS )
                           CALL PAR_CANCL( 'YCOORD', STATUS )
                           CALL PAR_CANCL( 'AGAIN', STATUS )

*                      go back to top of loop to check if user wanted
*                      another doing

                        END DO

                     ELSE IF ( WHERE .EQ. 'BAD' .AND.
     :                         STATUS .EQ. SAI__OK ) THEN

*                      Deglitch all the bad (magic) pixels

                        CALL GLTBSB( DIMS( 1 ), DIMS( 2 ),
     :                               %VAL( PNTRO ), NODEGL, STATUS )

*                      on return, check the status

                        IF ( STATUS .EQ. SAI__OK ) THEN

*                         report the number of bad pixels that were
*                         deglitched

                           CALL MSG_OUT( 'BLANK', ' ', STATUS )
                           CALL MSG_SETI( 'NODEGL', NODEGL )
                           CALL MSG_OUT( 'GLITCH_NODGL', ' Number of '/
     :                                   /'invalid pixels deglitched '/
     :                                   /' =  ^NODEGL', STATUS )
                           CALL MSG_OUT( 'BLANK', ' ', STATUS )

*                      end of bad-status-from-GLTBSB check

                        END IF

                     ELSE

                        IF ( STATUS .NE. PAR__ABORT .AND.
     :                       STATUS .NE. PAR__NULL ) THEN

*                         announce the error

                           CALL ERR_REP( 'ERR_GLITCH_PAR2',
     :                       'GLITCH: Error obtaining mode - aborting',
     :                       STATUS )
                        END IF

*                   end of mode-of-operation check

                     END IF

*                end of bad-status-from-COPY2D check

                  END IF

*                unmap output data array

                  CALL CMP_UNMAP( LOCDO, DNAMEO, STATUS )

               ELSE

                  CALL ERR_REP( 'ERR_GLITCH_NOMPO',
     :              'GLITCH: Error occurred whilst trying to map '/
     :              /'output frame', STATUS )

*             end of if-no-error-after-mapping-output check

               END IF

*             tidy up the output data structures

               CALL DAT_ANNUL( LOCDO, STATUS )
               CALL DAT_ANNUL( LOCO, STATUS )

            ELSE

               IF ( STATUS .NE. PAR__ABORT ) THEN
                  CALL ERR_REP( 'ERR_GLITCH_NOFRO',
     :              'GLITCH: Error occurred whilst trying to access '/
     :              /'output frame', STATUS )
               END IF

*          end of if-no-error-after-getting-output-structure check

            END IF

*          unmap input data array

            CALL CMP_UNMAP( LOCDI, DNAMEI, STATUS )

         ELSE

            CALL ERR_REP( 'ERR_GLITCH_NOMPI',
     :        'GLITCH: Error occurred whilst trying to map input '/
     :        /'frame', STATUS )

*       end of if-no-error-after-mapping-input check

         END IF

*       tidy up the input data structures

         CALL DAT_ANNUL( LOCDI, STATUS )
         CALL DAT_ANNUL( LOCI, STATUS )

      ELSE

         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'ERR_GLITCH_NOFRI',
     :        'GLITCH: Error occurred whilst trying to access input '/
     :        /'frame', STATUS )
         END IF

*    end of if-no-error-after-getting-input-structure check

      END IF

*    end

      END

