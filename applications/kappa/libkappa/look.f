*+  LOOK - Outputs the values of a sub-array of a 2-d data array to the
*          screen or a text file

      SUBROUTINE LOOK ( STATUS )
*
*    Description :
*
*     This routine reports to user or writes to a text file the values
*     in a specified sub-array of the 2-d data array in the input IMAGE
*     structure. Three options are allowed 
*       [Peep]    - gives a fixed 7x7 box on the screen, centred on a
*                   given pixel;
*       [Examine] - gives an NxM box on the screen, with the lower-left
*                   pixel as specified;
*       [List]    - outputs the specified sub-array to a text file
*                   (maximum width 132 characters) of a defined name.
*                   Beware that the List option can produce very large
*                   files if it is not used sensibly.
*
*     The magic-value method is used for processing bad data. Bad pixels
*     are denoted in the display or file by INVALID rather than a
*     numerical value.
*
*    Invocation :
*
*     CALL LOOK( STATUS )
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*         IMAGE structure containing the 2-d data array to be inspected
*     CHOICE = LITERAL( READ )
*         The means by which the data array is examined.  The options
*           are 'Peep' --- reports the values in a 7x7 pixel region;
*           'Examine' --- reports the values of a region whose size is
*           defined by the user; and 'List' is similar to 'Examine',
*           but it generates a listing to a text file.
*     XCEN  =  INTEGER( READ )
*         x centre of box to be examined on the screen.
*     YCEN  =  INTEGER( READ )
*         y centre of box to be examined on the screen.
*     XLOW  =  INTEGER( READ )
*         x pixel index of the lower left of the sub-array to be output.
*     YLOW  =  INTEGER( READ )
*         y pixel index of the lower left of the sub-array to be output.
*     XSIZE  =  INTEGER( READ )
*         x size of the sub-array to be output.
*     YSIZE  =  INTEGER( READ )
*         y size of the sub-array to be output.
*     FILENAME  =  FILENAME( READ )
*         Name of the file to be used for the output of the sub-array
*           listing.
*     ANOTHER = LOGICAL( READ )
*         If true, another look at the data is required.
*
*    Arguments:
*
*     STATUS  = INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get locator to input IMAGE structure
*     If no error then 
*        Map the data-array component
*        If no error then 
*           Output image dimensions to user
*           Do while another inspection wanted and no error
*              Get choice of Peep, Examine, or List
*              If Peep then
*                 Get x and y co-ords of centre of 7x7 box to be peeped
*                 If no error then 
*                    Call subroutine to output 7x7 box to screen
*                    If status is bad on return then
*                       Report context of PEEPSB error
*                    Endif
*                 Else
*                    Report error
*                 Endif
*                 Ask if another inspection wanted
*                 Clear parameters before looping
*              Else if Examine wanted then
*                 Get x and y lower bounds and size of box to be output
*                   to screen
*                 If no error then
*                    Call subroutine to output box to screen
*                    If status is bad on return then
*                       Report context of PEEPSB error
*                    Endif
*                 Else
*                    Report error
*                 Endif
*                 Ask if another inspection wanted
*                 Clear parameters before looping
*              Else if List wanted then
*                 Get x and y lower bounds and size of box to be listed
*                 If no error then
*                    Call subroutine to output sub-array to listing file
*                 Else
*                    Report error
*                 Endif
*                 Ask if another inspection is wanted
*                 Clear parameters before looping
*              Else
*                 Report error
*              Endif
*           Enddo
*           Unmap input data array
*        Else
*           Report error
*        Endif
*        Clean up input data structure
*     Else
*        Report error
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
*     Malcolm J. Currie   STARLINK( RAL::CUR )
*
*    History :
*
*     11-06-1986 : First implementation (REVA::MJM)
*     15-06-1986 : Modified to include Examine option (REVA::MJM)
*     1986 Aug 7 : Renamed algorithmic routines to PEEPSB and LISTSB
*                  (RAL::CUR).
*     1986 Aug 29: Added argument section to prologue, and nearly
*                  conformed to Starlink standards (RAL::CUR).
*     1987 Oct 15: Reordered tidying and extra status check (RAL::CUR).
*     1988 Mar 17: Referred to `array' rather than `image' (RAL::CUR).
*     1988 Jun 4 : More reporting of error context (RAL::CUR).
*     1988 Jun 30: File name obtained in LISTSB (RAL::CUR).
*     1988 Dec  6: Improved Examine XSIZE default (RAL::CUR).
*     1989 Jul 27: Passed array dimensions to PEEPSB and LISTSB as
*                  separate variables (RAL::CUR).
*     1990 Feb 22: Modified the calling sequence for the new
*                  more-modular version of LISTSB (RAL::CUR).
*     1991 Jun 11: Revised calling sequence for PEEPSB (RAL::CUR).
*     1992 Feb 25: Limited processing of simple NDFs (RAL::CUR).
*     1992 Mar  3: Replaced AIF parameter-system calls by the extended
*                  PAR library (RAL::CUR).
*
*    Type definitions :

      IMPLICIT  NONE           ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PAR_ERR'        ! parameter-system errors

*    Status :

      INTEGER  STATUS

*    Local Constants :

      INTEGER
     :  NDIMS                  ! dimensionality of input array
      PARAMETER( NDIMS = 2 )   ! default to 2-d

*    Local variables :

      INTEGER
     :  DIMS( NDIMS ),         ! array dimensions
     :  I,                     ! Loop counter
     :  LBND( NDIMS ),         ! Lower bounds of the array
     :  ORIGIN( DAT__MXDIM ),  ! Origin of the data array
     :  PNTRI,                 ! pointer to data array component
     :  XCENDF,                ! x default centre of box
     :  YCENDF,                ! y default centre of box 
     :  XCEN,                  ! x centre of 7x7 box to be peeped at
     :  YCEN,                  ! y    "    "  "   "   "  "    "    "
     :  XSIZE,                 ! x size of box to be inspected
     :  YSIZE,                 ! y   "   "  "   "  "     "
     :  XLOW,                  ! x lower left of sub-array
     :  YLOW,                  ! y   "     "   "  "    "
     :  XUPP,                  ! x upper right "  "    "
     :  YUPP                   ! y   "     "   "  "    "

      CHARACTER*80
     :  CHOILS                 ! list of choices - Peep, Examine or
                               ! List

      CHARACTER*10
     :  CHOICE                 ! choice made by user

      CHARACTER*(DAT__SZLOC)   ! Locator for :
     :  LOCDI,                 ! structure containing the input data
                               ! array
     :  LOCI                   ! Input data structure

      CHARACTER * ( DAT__SZNAM )
     :  DNAMEI                 ! Name of the input data-array component

      LOGICAL                  ! true if:
     :  ANOTHR                 ! another inspection is wanted

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    By definition the lower bounds are 1 until converted to use NDF_.

      DO  I = 1, NDIMS
         LBND( I ) = 1
      END DO

*    start by getting input IMAGE structure

      CALL KPG1_GETIM( 'INPIC', LOCI, LOCDI, DNAMEI, ORIGIN, STATUS )

*    check for error before continuing

      IF ( STATUS .EQ. SAI__OK ) THEN

*       map the data array component

         CALL CMP_MAPN( LOCDI, DNAMEI, '_REAL', 'READ', NDIMS,
     :                  PNTRI, DIMS, STATUS )

*       check for error before continuing

         IF ( STATUS .EQ. SAI__OK ) THEN

*          output array dimensions to user

            CALL MSG_SETI( 'XDIM', DIMS( 1 ) )
            CALL MSG_SETI( 'YDIM', DIMS( 2 ) )
            CALL MSG_OUT( 'INPUT_DIMS',
     :                    'Array is ^XDIM by ^YDIM pixels', STATUS )

*          initialise loop flag and defaults

            ANOTHR  =  .TRUE.
            XCENDF  =  NINT( REAL( DIMS( 1 ) ) / 2.0 )
            YCENDF  =  NINT( REAL( DIMS( 2 ) ) / 2.0 )

*          loop round doing output until requested to stop

            DO WHILE ( ANOTHR .AND. STATUS .EQ. SAI__OK )

*             set up the list of choices allowed

               CHOILS  =  'Peep,Examine,List'

*            get type of output required - Peep, Examine or List

               CALL MSG_OUT( 'BLANK', ' ', STATUS )
               CALL PAR_CHOIC( 'CHOICE', 'Peep', CHOILS, .TRUE., CHOICE,
     :                         STATUS )
               CALL MSG_OUT( 'BLANK', ' ', STATUS )

*             force string to upper case

               CALL CHR_UCASE( CHOICE )

*             act accordingly - check first for a Peep

               IF ( CHOICE( 1:1 ) .EQ. 'P' .AND.
     :              STATUS .EQ. SAI__OK ) THEN 

*                get the pixel indices of the centre of the box to be
*                output to the screen

                  CALL PAR_GDR0I( 'XCEN', XCENDF, 1, DIMS(1), .FALSE.,
     :                            XCEN, STATUS )
                  CALL PAR_GDR0I( 'YCEN', YCENDF, 1, DIMS(2), .FALSE.,
     :                            YCEN, STATUS )

*                check status before accessing pointer

                  IF ( STATUS .EQ. SAI__OK ) THEN

*                   Make the current values the dynamic defaults

                     XCENDF = XCEN
                     YCENDF = YCEN

*                   work out lower bounds and box size for 7x7 box
*                   centred on input pixel indices

                     XLOW  =  XCEN - 3
                     YLOW  =  YCEN - 3
                     XSIZE  =  7
                     YSIZE  =  7

*                   call subroutine to output to the screen a 7x7 box
*                   centred on XCEN, YCEN

                     CALL PEEPSB( %VAL( PNTRI ), LBND, DIMS( 1 ),
     :                            DIMS( 2 ), XLOW, YLOW, XSIZE, YSIZE,
     :                            STATUS )

*                   report context of any error

                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL ERR_REP( 'ERR_LOOK_SUB1',
     :                    'LOOK : Error occurred in Peep mode',
     :                    STATUS )
                     END IF

                  ELSE

                     IF ( STATUS .NE. PAR__ABORT .AND.
     :                    STATUS .NE. PAR__NULL ) THEN

*                      announce the error

                        CALL ERR_REP( 'ERR_LOOK_PAR1',
     :                    'LOOK : Error obtaining centre in Peep '/
     :                    /'mode - aborting', STATUS )
                     END IF

*                end of check-status-from-getting-centre check

                  END IF

*                ask user if he wants to do another inspection

                  CALL MSG_OUT( 'BLANK', ' ', STATUS )
                  CALL PAR_GET0L( 'ANOTHER', ANOTHR, STATUS )

*                cancel the parameters before next use

                  CALL PAR_CANCL( 'CHOICE', STATUS )
                  CALL PAR_CANCL( 'XCEN', STATUS )
                  CALL PAR_CANCL( 'YCEN', STATUS )
                  CALL PAR_CANCL( 'ANOTHER', STATUS )

*             else check if an Examine is wanted

               ELSE IF ( CHOICE( 1:1 ) .EQ. 'E' .AND.
     :                   STATUS .EQ. SAI__OK ) THEN

*                get the x and y lower bounds and box size to be
*                examined

                  CALL PAR_GDR0I( 'XLOW', 1, 1, DIMS( 1 ), .FALSE.,
     :                            XLOW, STATUS )
                  CALL PAR_GDR0I( 'YLOW', 1, 1, DIMS( 2 ), .FALSE.,
     :                            YLOW, STATUS )
                  CALL PAR_GDR0I( 'XSIZE', MIN( DIMS( 2 ) - XLOW + 1,
     :                             9 ), 1, 9, .FALSE., XSIZE, STATUS )
                  CALL PAR_GDR0I( 'YSIZE', DIMS( 2 ) - YLOW + 1, 1,
     :                             DIMS( 2 ), .FALSE., YSIZE, STATUS )

*                check for error before calling subroutine

                  IF ( STATUS .EQ. SAI__OK ) THEN

*                   call subroutine to output box to screen

                     CALL PEEPSB( %VAL( PNTRI ), LBND, DIMS( 1 ),
     :                            DIMS( 2 ), XLOW, YLOW, XSIZE, YSIZE,
     :                            STATUS )

*                   report context of any error

                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL ERR_REP( 'ERR_LOOK_SUB2',
     :                    'LOOK : Error occurred in Examine mode',
     :                    STATUS )
                     END IF

                  ELSE

                     IF ( STATUS .NE. PAR__ABORT .AND.
     :                    STATUS .NE. PAR__NULL ) THEN

*                      announce the error

                        CALL ERR_REP( 'ERR_LOOK_PAR2',
     :                    'LOOK : Error obtaining box limits in '/
     :                    /'Examine mode - aborting', STATUS )
                     END IF

*                end of check-status-from-getting-box-limits check

                  END IF

*                ask if another inspection is wanted

                  CALL MSG_OUT( 'BLANK', ' ', STATUS )
                  CALL PAR_GET0L( 'ANOTHER', ANOTHR, STATUS )

*                cancel parameters ready for another inspection

                  CALL PAR_CANCL( 'CHOICE', STATUS )
                  CALL PAR_CANCL( 'XLOW', STATUS )
                  CALL PAR_CANCL( 'YLOW', STATUS )
                  CALL PAR_CANCL( 'XSIZE', STATUS )
                  CALL PAR_CANCL( 'YSIZE', STATUS )
                  CALL PAR_CANCL( 'ANOTHER', STATUS )

*             else check if a List is wanted

               ELSE IF ( CHOICE( 1:1 ) .EQ. 'L' .AND.
     :                   STATUS .EQ. SAI__OK ) THEN 

*                get the x and y lower bounds and box size to be
*                examined

                  CALL PAR_GDR0I( 'XLOW', 1, 1, DIMS( 1 ), .FALSE.,
     :                            XLOW, STATUS )
                  CALL PAR_GDR0I( 'YLOW', 1, 1, DIMS( 2 ), .FALSE.,
     :                            YLOW, STATUS )
                  CALL PAR_GDR0I( 'XSIZE', DIMS( 1 ) - XLOW + 1, 1, 
     :                             DIMS( 1 ), .FALSE., XSIZE, STATUS )
                  CALL PAR_GDR0I( 'YSIZE', DIMS( 2 ) - YLOW + 1, 1,
     :                             DIMS( 2 ), .FALSE., YSIZE, STATUS )

                  IF ( STATUS .EQ. SAI__OK ) THEN

*                   work out the upper bounds from the lower bounds and
*                   box size

                     XUPP  =  XLOW + XSIZE - 1
                     YUPP  =  YLOW + YSIZE - 1

*                   call subroutine to output array listing to file

                     CALL LISTSB( %VAL( PNTRI ), DIMS( 1 ), DIMS( 2 ),
     :                            XLOW, YLOW, XUPP, YUPP, .TRUE., 0,
     :                            'FILENAME', STATUS )

                  ELSE

                     IF ( STATUS .NE. PAR__ABORT .AND.
     :                    STATUS .NE. PAR__NULL ) THEN

*                      announce the error

                        CALL ERR_REP( 'ERR_LOOK_PAR3',
     :                    'LOOK : Error obtaining box limits in List '/
     :                    /'mode - aborting', STATUS )
                     END IF

*                end of check-status-from-getting-box-limits check

                  END IF

*                see if user wants another output doing

                  CALL MSG_OUT( 'BLANK', ' ', STATUS )
                  CALL PAR_GET0L( 'ANOTHER', ANOTHR, STATUS )

*                cancel parameters ready for another inspection.
*                FILENAME is cancelled by the listing routine.

                  CALL PAR_CANCL( 'CHOICE', STATUS )
                  CALL PAR_CANCL( 'XLOW', STATUS )
                  CALL PAR_CANCL( 'YLOW', STATUS )
                  CALL PAR_CANCL( 'XSIZE', STATUS )
                  CALL PAR_CANCL( 'YSIZE', STATUS )
                  CALL PAR_CANCL( 'ANOTHER', STATUS )

               ELSE

                  IF ( STATUS .NE. PAR__ABORT .AND.
     :                 STATUS .NE. PAR__NULL ) THEN

*                   announce the error

                     CALL ERR_REP( 'ERR_LOOK_MODE',
     :                 'LOOK : Mode not recognised - aborting',
     :                 STATUS )
                  END IF

*             end of if-mode check

               END IF

*          end of loop round whilst another inspection required

            END DO

*          unmap input data array

            CALL CMP_UNMAP( LOCDI, DNAMEI, STATUS )

         ELSE

            CALL ERR_REP( 'ERR_LOOK_NOMPI',
     :        'LOOK : Error occurred whilst trying to map input frame',
     :        STATUS )

*       end of if-no-error-after-mapping-input-array check

         END IF

*       clean up input data structures

         CALL DAT_ANNUL( LOCDI, STATUS )
         CALL DAT_ANNUL( LOCI, STATUS )

      ELSE

         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'ERR_LOOK_NOFRI',
     :        'LOOK : Error occurred whilst trying to access input '/
     :        /'frame', STATUS )
         END IF

*    end of if-status-ok-after-getting-input-array-locator check

      END IF

*    return and end

      END

