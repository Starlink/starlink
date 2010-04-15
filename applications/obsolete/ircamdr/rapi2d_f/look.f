
*+  LOOK - allows output of image sub-array values to screen or file

      SUBROUTINE LOOK ( STATUS )

*    Description :
*
*     This routine allows the user to output the values in a specified
*     sub-array of an image either to the screen or to a serial file.
*     Three options are allowed - Peep gives a fixed 7x7 box on the
*     screen, centred on a given pixel; Examine gives an N x M box on
*     screen, with lower left pixel as given; List outputs the specified
*     sub-array to a text file (max width 132 characters) of given name.
*
*    Invocation :
*
*     CALL LOOK( STATUS )
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*         Image to be inspected
*     XCEN  =  INTEGER( READ )
*         x centre of box to be examined on screen
*     YCEN  =  INTEGER( READ )
*         y centre of box to be examined on screen
*     XLOW  =  INTEGER( READ )
*         x coord of lower left of sub-array to be output
*     YLOW  =  INTEGER( READ )
*         y coord of lower left of sub-array to be output
*     XSIZE  =  INTEGER( READ )
*         x size of box to be output
*     YSIZE  =  INTEGER( READ )
*         y size of box to be output
*     FILENAME  =  CHAR( READ )
*         Name of file to be used for output of sub-array
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get locator to input image structure
*     If no error then
*        Map the data array component
*        Output image dimensions to user
*        Do while another inspection wanted and no error
*           Get choice of Peep, Examine, or List
*           If Peep then
*              Get x and y coords of centre of 7x7 box to be peeped
*              If no error then
*                 Call subroutine to output 7x7 box to screen
*              Endif
*              Ask if another inspection wanted
*              Clear parameters before looping
*           Else if Examine wanted then
*              Get x and y lower bounds and size of box to be output to screen
*              If no error then
*                 Call subroutine to output box to screen
*              Endif
*              Ask if another inspection wanted
*              Clear parameters before looping
*           Else if List wanted then
*              Get x and y lower bounds and size of box to be listed
*              Get file name for listing
*              If no error then
*                 Call subroutine to output sub-array to listing file
*                 If error on return then
*                    Output error message and flush error to reset status
*                 Endif
*              Endif
*              Ask if another inspection is wanted
*              Clear parameters before looping
*           Endif
*        Enddo
*        Clean up input data structure
*     Endif
*     End
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
*     11-06-1986 : First implementation (REVA::MJM)
*     15-06-1986 : Modified to include Examine option (REVA::MJM)
*     11-MAR-94    Changed DAT_, CMP_ to NDF_ (SKL@JACH)
*     15-Aug-1994  Chamged input DIM arguments for LISTSUB, PEEPSUB (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'NDF_PAR'
      INCLUDE  'NDF_ERR'

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local Constants :

      INTEGER
     :    NDIMS                   ! dimensionality of input image
      PARAMETER( NDIMS = 2 )      ! default to 2-d

*    Local variables :

      INTEGER
     :    LOCI,                   ! locator for input image structure
     :    NELEMENTS,              ! number of elements mapped
     :    NDIM,                   ! dimensions from NDF_DIM
     :    DIMS( NDIMS ),          ! array dimensions
     :    PNTRI,                  ! pointer to data array component
     :    XCEN,                   ! x centre of 7x7 box to be peeped at
     :    YCEN,                   ! y    "    "  "   "   "  "    "    "
     :    XSIZE,                  ! x size of box to be inspected
     :    YSIZE,                  ! y   "   "  "   "  "     "
     :    XLOW,                   ! x lower left of sub-image to be inspected
     :    YLOW,                   ! y   "     "   "  "    "    "  "     "
     :    XUPP,                   ! x upper right "  "    "    "  "     "
     :    YUPP                    ! y   "     "   "  "    "    "  "     "

      CHARACTER*80
     :    CHOICELIST,             ! list of choices - Peep, Examine or List
     :    FILENAME                ! name of file to be used for listing

      CHARACTER*1
     :    CHOICE                  ! choice made by user

      LOGICAL
     :    ANOTHER                 ! true if another inspection wanted

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    start by getting input image
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    check for error before continuing
      IF ( STATUS .EQ. SAI__OK ) THEN

*       map the data array component
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )

*       get dimensions
         CALL NDF_DIM( LOCI, NDIMS, DIMS, NDIM, STATUS )

*       output array dimensions to user
         CALL MSG_SETI( 'XDIM', DIMS( 1 ) )
         CALL MSG_SETI( 'YDIM', DIMS( 2 ) )
         CALL MSG_OUT( 'INPUT_DIMS',
     :      'Image is ^XDIM by ^YDIM pixels', STATUS )

*       initialise loop flag and other variables
         ANOTHER  =  .TRUE.
         XCEN  =  NINT( REAL( DIMS( 1 ) ) / 2.0 )
         YCEN  =  NINT( REAL( DIMS( 2 ) ) / 2.0 )

*       loop round doing output until requested to stop
         DO WHILE ( ANOTHER .AND. STATUS .EQ. SAI__OK )

*          set up the list of choices allowed
            CHOICELIST  =  'P,E,L'

*          get type of output required - Peep, Examine or List
            CALL MSG_OUT( 'BLANK', ' ', STATUS )
            CALL AIF_CHOIC( 'CHOICE', CHOICELIST, CHOICE, STATUS )
            CALL MSG_OUT( 'BLANK', ' ', STATUS )

*          force string to upper case
            CALL UPCASE( CHOICE, CHOICE, STATUS )

*          act accordingly - check first for a Peep
            IF ( CHOICE .EQ. 'P' ) THEN

*             get the coordinates of the centre of the box to be output
*             to the screen
               CALL AIF_GET0I( 'XCEN', XCEN, 1, DIMS(1), XCEN, STATUS )
               CALL AIF_GET0I( 'YCEN', YCEN, 1, DIMS(2), YCEN, STATUS )

*             work out lower bounds and box size for 7x7 box centred on
*             input coordinates
               XLOW  =  XCEN - 3
               YLOW  =  YCEN - 3
               XSIZE  =  7
               YSIZE  =  7

*             check status before accessing pointer
               IF ( STATUS .EQ. SAI__OK ) THEN

*                call subroutine to output to the screen a 7x7 box
*                centred on XCEN,YCEN
                  CALL PEEPSUB( %VAL( PNTRI ), DIMS(1), DIMS(2), XLOW,
     :                          YLOW, XSIZE, YSIZE, STATUS )

               END IF

*             ask user if he wants to do another inspection
               CALL MSG_OUT( 'BLANK', ' ', STATUS )
               CALL PAR_GET0L( 'ANOTHER', ANOTHER, STATUS )

*             cancel the parameters before next use
               CALL PAR_CANCL( 'CHOICE', STATUS )
               CALL PAR_CANCL( 'XCEN', STATUS )
               CALL PAR_CANCL( 'YCEN', STATUS )
               CALL PAR_CANCL( 'ANOTHER', STATUS )

*          else check if an Examine is wanted
            ELSE IF ( CHOICE .EQ. 'E' ) THEN

*             get the x and y lower bounds and box size to be examined
               CALL AIF_GET0I( 'XLOW', 1, 1, DIMS( 1 ), XLOW, STATUS )
               CALL AIF_GET0I( 'YLOW', 1, 1, DIMS( 2 ), YLOW, STATUS )
               CALL AIF_GET0I( 'XSIZE', MIN( DIMS( 2 ), 9 ),
     :                          1, 9, XSIZE, STATUS )
               CALL AIF_GET0I( 'YSIZE', DIMS( 2 ) - YLOW + 1, 1,
     :                          DIMS( 2 ), YSIZE, STATUS )

*             check for error before calling subroutine
               IF ( STATUS .EQ. SAI__OK ) THEN

*                call subroutine to output box to screen
                  CALL PEEPSUB( %VAL( PNTRI ), DIMS(1), DIMS(2),
     :                          XLOW, YLOW, XSIZE, YSIZE, STATUS )

               END IF

*             ask if another inspection is wanted
               CALL MSG_OUT( 'BLANK', ' ', STATUS )
               CALL PAR_GET0L( 'ANOTHER', ANOTHER, STATUS )

*             cancel parameters ready for another inspection
               CALL PAR_CANCL( 'CHOICE', STATUS )
               CALL PAR_CANCL( 'XLOW', STATUS )
               CALL PAR_CANCL( 'YLOW', STATUS )
               CALL PAR_CANCL( 'XSIZE', STATUS )
               CALL PAR_CANCL( 'YSIZE', STATUS )
               CALL PAR_CANCL( 'ANOTHER', STATUS )

*          else check if a List is wanted
            ELSE IF ( CHOICE .EQ. 'L' ) THEN

*             get the x and y lower bounds and box size to be examined
               CALL AIF_GET0I( 'XLOW', 1, 1, DIMS( 1 ), XLOW, STATUS )
               CALL AIF_GET0I( 'YLOW', 1, 1, DIMS( 2 ), YLOW, STATUS )
               CALL AIF_GET0I( 'XSIZE', DIMS( 1 ) - XLOW + 1, 1,
     :                          DIMS( 1 ), XSIZE, STATUS )
               CALL AIF_GET0I( 'YSIZE', DIMS( 2 ) - YLOW + 1, 1,
     :                          DIMS( 2 ), YSIZE, STATUS )

*             work out the upper bounds from the lower bounds and box size
               XUPP  =  XLOW + XSIZE - 1
               YUPP  =  YLOW + YSIZE - 1

*             get the filename to be used for output
               CALL PAR_GET0C( 'FILENAME', FILENAME, STATUS )

*             force file name to upper case
               CALL UPCASE( FILENAME, FILENAME, STATUS )

*             check status before accessing pointer
               IF ( STATUS .EQ. SAI__OK ) THEN

*                call subroutine to output image listing to file
                  CALL LISTSUB( %VAL( PNTRI ), DIMS(1), DIMS(2),
     :                           XLOW, YLOW, XUPP, YUPP, FILENAME,
     :                           STATUS )

*                check for bad status on return - this may be
*                because the requested file name was invalid and
*                the open statement in LISTSUB failed - flush out
*                error and continue
                  IF ( STATUS .NE. SAI__OK ) THEN

                     CALL MSG_SETC( 'FILE', FILENAME )
                     CALL ERR_REP( 'BAD_FILE',
     : ' Problem opening file name ^FILE - invalid', STATUS )
                     CALL ERR_FLUSH( STATUS )

                  END IF

               END IF

*             see if user wants another output doing
               CALL MSG_OUT( 'BLANK', ' ', STATUS )
               CALL PAR_GET0L( 'ANOTHER', ANOTHER, STATUS )

*             cancel parameters ready for another inspection
               CALL PAR_CANCL( 'CHOICE', STATUS )
               CALL PAR_CANCL( 'XLOW', STATUS )
               CALL PAR_CANCL( 'YLOW', STATUS )
               CALL PAR_CANCL( 'XSIZE', STATUS )
               CALL PAR_CANCL( 'YSIZE', STATUS )
               CALL PAR_CANCL( 'FILENAME', STATUS )
               CALL PAR_CANCL( 'ANOTHER', STATUS )

*          end of if-peep-else-if-list check
            END IF

*       end of loop round whilst another inspection required
         END DO

*       clean up input data structure
         CALL NDF_ANNUL( LOCI, STATUS )

*    end of if-status-ok-after-getting-input-image-locator check
      END IF


*    return and end
      END
