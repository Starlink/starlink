*+  MSTATS - Does cumulative statistics on a 2-d sub-array over
*            a sequence of 2-d data arrays

      SUBROUTINE MSTATS ( STATUS )
*
*    Description :
*
*     This routine is used for the statistics of multiple 2-d data
*     arrays. The data arrays must have the same dimensions and reside
*     in IMAGE structures. The user is asked to specify a number of
*     data arrays (up to a fixed limit) either by naming each file
*     or defining a sequence of IMAGE structures (frames). If the latter
*     option is chosen the files must adopt the following naming scheme:
*     groupnamennnn, where nnnn is a four-digit number, and groupname is
*     the collective name for the set of arrays, e.g. ORION0001. Missing
*     container files, data arrays, or data arrays of the wrong
*     dimensions are skipped. The maximum number of data files is 1000.
*
*     Then either Single pixel or Box mode is chosen. In the former
*     case the pixel of interest is specified, and in the latter, the
*     sub-array of interest. In the Single pixel mode, the value for the
*     same pixel is pulled out of each array in sequence, and this
*     sequence of values is then statistically analysed over the
*     sequence of input frames. The resultant values (mean, median
*     and standard deviation) are reported directly to the user.
*
*     In Box mode, a choice of statistics is selected.  The alternatives
*     are mean and standard deviation (the default), or median. The
*     statistic(s) are formed over the sequence of arrays at each pixel
*     position in the box. The output is in the form of one or two 2-d
*     data arrays, each being the size of the defined sub-array and
*     contains a chosen statistic (mean, standard deviation or median)
*     in each pixel. Each output data array is stored in an IMAGE
*     structure.
*     
*     The magic-value method is used for processing bad data.
*
*    Invocation :
*
*     CALL MSTATS( STATUS )
*
*    Parameters :
*
*     INMODE  =  CHAR( READ )
*         Mode of data array input, the alternatives being
*           'Sequential' or 'Random'.
*     PIXMODE  =  CHAR( READ )
*         Mode of calculation, the alternatives being a 'Single'
*           pixel or a 'Box' of pixels.
*     INPIC  =  IMAGE( READ )
*         One of the sequence of input IMAGE structures.
*     FIRSTFILE  =  CHAR( READ )
*         Name of first container filename in the sequence.
*     NUMSEQ  =  INTEGER( READ )
*         Number of sequential frames to be processed.
*     NUMRAN  =  INTEGER( READ )
*         Number of random frames to be processed.
*     XPIX  = INTEGER( READ )
*         x pixel index of the pixel to be used in Single mode.
*     YPIX  = INTEGER( READ )
*         y pixel index of the pixel to be used in Single mode.
*     ORDRST  =  LOGICAL( READ )
*         If true ordered statistics will be computed in Box mode,
*           currently only the median, otherwise the mean and standard
*           deviation are derived.
*     XSTART  =  INTEGER( READ )
*         x start pixel index of the sub-arrays to be analysed.
*     YSTART  =  INTEGER( READ )
*         y start pixel index of the sub-arrays to be analysed.
*     XSIZE  =  INTEGER( READ )
*         x size of the sub-array to be analysed.
*     YSIZE  =  INTEGER( READ )
*         y size of the sub-array to be analysed.
*     MEDIAN  =  IMAGE( WRITE )
*         Output IMAGE structure containing array of medians for Box
*           option.
*     OMTITLE  =  CHAR( READ )
*         Title string for IMAGE structure containing the median array.
*     MEAN  =  IMAGE( WRITE )
*         Output IMAGE structure containing array of means for Box
*           option.
*     MTITLE  =  CHAR( READ )
*         Title string for IMAGE structure containing the mean array.
*     STDDEV  =  IMAGE( WRITE )
*         Output IMAGE structure containing standard deviations for Box
*           option.
*     STITLE  =  CHAR( READ )
*         Title string for IMAGE structure containing the standard-
*           deviation array.
*
*    Arguments:
*
*     STATUS  = INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Ask whether input mode is sequential or random
*     If an error has occurred report it and abort
*     If sequential then
*        Get first filename in sequence
*        Get number of following frames in sequence
*        If an error has occurred report it and abort
*        Get ADAM internal pointer associated with input frames
*        Stick first filename into ADAM pointer
*        See if given structure exists
*        If not then
*           Output error message and return
*        Endif
*     Else random mode wanted
*        Get number of frames wanted
*        If an error has occurred report it and abort
*        Get locator to first IMAGE structure
*        If error then
*           Report and return
*        Endif
*     Endif
*     If error this far then
*        Return
*     Endif
*     See if there is a data array component in first structure
*     If not then
*        Output error message, annul locator and return
*     Endif
*     Get the size of the data array component in the first frame
*     If component is a scalar then
*        Output error message, set bad status, annul locator and return
*     Endif
*     Ask which mode - Single pixel or Box mode
*     If Single pixel mode then
*        Get x and y pixel index of pixel to be looked at
*        Calculate what this means in terms of a 1x1 box
*     Else Box mode wanted
*        Find out whether or not ordered statistics required
*        Get x and y lower bounds and box size required
*     Endif
*     If there was an error getting box position then
*     If not then
*        Output error message, annul locator and return
*     Else
*        Map some 3-d workspace to hold the sequence of 2-d boxes
*        If error then
*           Report error, tidy input structure and return
*        Endif
*     Endif
*     Do for the number of requested frames and whilst the status is
*      o.k. and whilst not finished
*        Increment frame counter
*        Reset the skip flag to false
*        If this is not the first frame then
*           If input mode is sequential
*              Get next filename in sequence
*              If error on return then
*                 Set finished flag to true
*                 Set skip flag to true
*                 Output message to indicate error
*                 Set flag to indicate no data got from current frame
*              Else
*                 Associate the filename with the image parameter
*                 See if the given frame exists
*                 If it does not then
*                    Set skip flag to true
*                    Set flag indicating that current slice is missing
*                    Output message to indicate error
*                    Set flag to indicate no data got from current frame
*                 Endif
*              Endif
*           Else mode is Random mode
*              Get locator to next desired array
*              If error on return then
*                 Set skip flag to true
*                 Output message to indicate error
*                 Set flag to indicate no data got from current frame
*              Endif
*           Endif
*        Endif
*        If no error and skip flag is set false
*           See if there is a data array in current structure
*           If not there then
*              Set skip to true
*              Output message to same effect
*              Flush the error
*              Set flag saying that current array was not got ok
*           Else
*              Get shape of data array
*              If no error so far then
*                 See if the data array has correct dimensions
*                 If not then
*                    Set skip to true
*                    Output message to same effect
*                    Flush the error
*                    Set flag saying that current array was not got ok
*                 Else
*                    Map the current data array
*                    If error on return then
*                       Set skip flag to true
*                       Output message to indicate error
*                       Set flag to indicate no data got from current
*                         frame
*                    Endif
*                 Endif
*              Endif
*           Endif
*           If status ok and skip is false
*              Copy 2-d sub-array into correct slice of workspace
*              If no error so far then
*                 Output message to indicate success at this point
*                  Set flag to indicate data got ok for this frame
*                 Increment good data frame counter
*              Endif
*              Unmap the current data array component
*           Endif
*           Annul the locator to the current structure
*        Endif
*        Cancel the parameter that points at the current array
*     Endfor
*     If status ok and data from more than one frame was got ok then
*        If pixel mode is Single then
*           Call subroutines to do 3-d stats into single output
*             variables
*           If no error then report the results
*        Else we are in Box mode
*           If ordered statistics required then
*              Create array to hold medians
*              If no error then
*                 Map median-data-array component
*                 If no error then
*                    Call subroutine to do the 3-d statistics and place
*                      the results into output array
*                    If no error then
*                       Tell user how many input frames actually went
*                         into output
*                    Endif
*                    Unmap output data array
*                 Else
*                    Report error context
*                 Endif
*                 Clean up output frame
*              Else
*                 Annul frame of medians
*                 Report error context
*              Endif
*           Else
*              Initialise array flags
*              Create array to hold means
*              If an error has occurred set array obtained flag to
*                'mean'
*              Create array to hold standard deviations
*              If no error then
*                 Map mean-data array component
*                 If an error has occurred set array mapped flag to
*                   'mean'
*                 Map standard-deviation-data array component
*                 If no error then
*                    Call subroutine to do the 3-d statistics and place
*                      the results into output arrays
*                    If no error then
*                       Tell user how many input frames actually went
*                         into output
*                    Endif
*                    Unmap output data arrays
*                 Else
*                    Unmap frame of means if it was mapped successfully
*                      (i.e. error occurred mapping std. deviation
*                      frame)
*                    Report error context
*                 Endif
*                 Clean up output frames
*              Else
*                 Annul frame of means if it was created successfully
*                   (i.e. error occurred creating std. deviation frame)
*                 Report error context
*              Endif
*           Endif
*        Endif
*     Else if status ok and one or less frames got ok
*        Output error message indicating that stats cannot be done on
*         less than two frames
*     Else if status is par__null or par__abort then
*        Output message to that effect
*     Else
*        An unknown gremlin has occurred - output message
*     Endif
*     Return
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*     Malcolm Currie  STARLINK ( RAL::CUR )
*
*    History :
*
*     03-06-1986 : First implementation (REVA::MJM)
*     15-06-1986 : Major upgrade for result image outputting (REVA::MJM)
*     1986 Aug 7 : Renamed routines NEXT_NAME, SLICE2TO3, STATS3D to
*                  NXTNAM, SLC2T3, STAT3D respectively. SLC2T3 parameter
*                  order corrected (7th to 9th) (RAL::CUR).
*     1986 Sep 1 : Added arguments section, nearly conformed to Starlink
*                  standards, replaced UPCASE by CHR_UCASE and tidied
*                  (RAL::CUR).
*     1987 Oct 17: Two extra status checks (RAL::CUR)
*     1988 Feb 19: Added check for equality of arrays' dimensions 
*                  (RAL::CUR).
*     1988 Mar 16: Tidied workspace using AIF_ANTMP (RAL::CUR).
*     1988 Mar 17: Referred to `array' rather than `image'
*                  (RAL::CUR)
*     1988 Jun 21: More reporting of error context and tidying
*                  (RAL::CUR)
*     1989 May 26: Added ordered statistics option (RAL::CUR).
*     1989 Aug  8: Passed array dimensions as separate variables
*                  to MED3D, SLC2T3 and STAT3D (RAL::CUR).
*     1989 Dec 21: Workspace managed by AIF_TEMP (RAL::CUR).
*     1990 Feb 22: Replaced SUBPAR calls by AIF_PTFNM (RAL::CUR).
*     1992 Feb 26: Limited processing of simple NDFs (RAL::CUR).
*     1992 Mar  3: Replaced AIF parameter-system calls by the extended
*                  PAR library (RAL::CUR).
*
*    Type definitions :

      IMPLICIT  NONE           ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PAR_ERR'        ! parameter system error codes

*    Status :

      INTEGER  STATUS

*    Local Constants :

      INTEGER
     :  NDIMS,                 ! Dimensionality of input/output frames
     :  MAXNUM,                ! Maximum number of frames in sequence
     :  NWKDIM                 ! Dimensionality of work array
      PARAMETER( NDIMS = 2 )   ! 2-d arrays only
      PARAMETER( MAXNUM = 1000 ) ! largish default set
      PARAMETER( NWKDIM = 3 )  ! 3-d workspace needed

*    Local variables :

      INTEGER
     :  ACTDIM,                ! Dummy to receive return from CMP_SHAPE
     :  DIMS( NDIMS ),         ! Dimensions of primary input frame
     :  DIMSN( NDIMS ),        ! Dimensions of other input frames
     :  DATAIN,                ! Number of frames from which data were
                               ! got
     :  FRAMNM,                ! Total number of frames processed so far
     :  GODNUM,                ! Number of slices in workspace from
                               ! which data were included
     :  I,                     ! Loop counter
     :  MDPNTR,                ! Pointer to median array data
     :  MPNTR,                 ! Pointer to mean array data
     :  NUMSEQ                 ! Number of frames

      INTEGER
     :  ODIMS( NDIMS ),        ! Dimensions of output arrays
     :  ORIGIN( DAT__MXDIM ),  ! Origin of the data array
     :  PNTRI,                 ! Pointer to input data
     :  SPNTR,                 ! Pointer to standard deviation data
     :  WKDIMS( NWKDIM ),      ! Dimensions of workspace array
     :  WKPNTR,                ! Pointer to workspace array
     :  WTARR( MAXNUM ),       ! Weighting of each frame for median
                               ! (only equal weighting used)
     :  XPIX,                  ! x co-ord of pixel used in Single mode
     :  YPIX,                  ! y   "    "   "     "   "    "     "
     :  XSTART,                ! x start co-ord of sub-array to be used
     :  YSTART,                ! y   "     "    "  "    "    "  "   "
     :  XSIZE,                 ! x size of         "    "    "  "   "
     :  YSIZE                  ! y   "   "         "    "    "  "   "

      REAL                        
     :  MEAN,                  ! Mean value returned in Single pixel
                               ! mode
     :  MEDIAN,                ! Median value returned in Single pixel
                               ! mode
     :  STDDEV                 ! Standard deviation in Single pixel mode

      LOGICAL                  ! True if :
     :  FINISH,                ! Sequence processing halted after NXTNAM
                               ! comes up with bad next filename
     :  GOODAT( MAXNUM ),      ! Entry corresponding to given frame in
                               ! sequence is true if data was found ok
     :  ORDRST,                ! Ordered statistics to be found
     :  SEQUEN,                ! Sequential file input was requested
     :  SINGLE,                ! Single pixel mode was requested
     :  SKIP,                  ! Some reason is found to skip the
                               ! current data frame
     :  WRKOBT                 ! Workspace obtained successfully

      CHARACTER*(DAT__SZLOC)   ! Locators for :
     :  LOCDI,                 ! structure containing the input data
                               ! array
     :  LOCDO1,                ! structure containing the output median
                               ! array
     :  LOCDO2,                ! structure containing the output mean
                               ! array
     :  LOCDO3,                ! structure containing the output
                               ! standard-deviation array
     :  LOCI,                  ! Input frame structure
     :  WKLOC,                 ! Workspace structure
     :  MLOC,                  ! Output mean image stucture
     :  MDLOC,                 ! Output median image stucture
     :  SLOC                   ! Output standard deviation image
                               ! structure

      CHARACTER * ( DAT__SZNAM )
     :  DNAMEI,                ! Name of the input data-array component
     :  DNAMEO                 ! Name of the output data-array component

      CHARACTER
     :  INMODE*10,             ! Frame input mode - Sequential or Random
     :  PIXMOD*6,              ! Pixel mode - Single or Box
     :  MFRSTO*5,              ! Type of output array where an error
                               ! occurred mapping it
     :  GFRSTO*5               ! Type of output array where an error
                               ! occurred creating it

      CHARACTER*80            
     :  CURNAM,                ! Current filename of frame to be
                               ! processed
     :  FILNAM                 ! Filename of next frame to be processed

*-
*    Check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Start by getting file input mode - Sequential or Random

      CALL PAR_CHOIC( 'INMODE', 'Sequential', 'Sequential,Random',
     :                .FALSE., INMODE, STATUS )

*    Check status and abort if bad

      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'MSTATS_MODE',
     :        'MSTATS: Error obtaining mode of operation.', STATUS )
         END IF
         GOTO 999
      END IF

      WRKOBT = .FALSE.

*    Act accordingly

      IF ( INMODE( 1:1 ) .EQ. 'S' ) THEN

*       Set flag to indicate that sequential mode is wanted

         SEQUEN  =  .TRUE.

*       Files are to be sequential - get the first input filename as a
*       character string

         CALL PAR_GET0C( 'FIRSTFILE', FILNAM, STATUS )

*       Get the number of sequential frames to be taken - e.g. if the
*       first file is F0001 and the last is F0010, then NUMSEQ should
*       be input as ten

         CALL PAR_GDR0I( 'NUMSEQ', 10, 1, MAXNUM, .FALSE., NUMSEQ,
     :                   STATUS )

*       Check status and abort if bad

         IF ( STATUS .NE. SAI__OK ) THEN
            IF ( STATUS .NE. PAR__ABORT ) THEN
               CALL ERR_REP( 'MSTATS_PARS',
     :           'MSTATS: Error obtaining parameters in Sequence mode.',
     :           STATUS )
            END IF
            GOTO 999
         END IF

*       Write the file name to the parameter INPIC, i.e. the parameter
*       to be used for the sequence of frames.

         CALL AIF_PTFNM( 'INPIC', FILNAM, STATUS )

*       Try to get a locator to the structure - use DAT_EXIST here
*       instead of DAT_ASSOC, as latter will reprompt if the structure
*       does not exist - if the latter fails, STATUS = PAR__ERROR is
*       returned

         CALL DAT_EXIST( 'INPIC', 'READ', LOCI, STATUS )

*       Check for error here

         IF ( STATUS .EQ. PAR__ERROR ) THEN

*          First frame does not exist - output error message and return

            CALL ERR_REP( 'MSTATS_1STD',
     :        'MSTATS: Problem getting a locator to given first '/
     :        /'file - aborting.', STATUS )
            GOTO 999

         END IF

*       Now see whether a data-array component exists, and return its
*       name and a locator to a structure containing it.

         CALL KPG1_GETIM( ' ', LOCI, LOCDI, DNAMEI, ORIGIN, STATUS )

*    Else input mode is Random 

      ELSE

*       Set flag to indicate random input mode

         SEQUEN  =  .FALSE.

*       Get the number of random frames to be taken (including first)

         CALL PAR_GDR0I( 'NUMRAN', 10, 1, MAXNUM, .FALSE., NUMSEQ,
     :                   STATUS )

*       Check status and abort if bad

         IF ( STATUS .NE. SAI__OK ) THEN
            IF ( STATUS .NE. PAR__ABORT ) THEN
               CALL ERR_REP( 'MSTATS_PARR',
     :           'MSTATS: Error occurred in Random mode.', STATUS )
            END IF
            GOTO 999
         END IF

*       Get locator to first frame

         CALL KPG1_GETIM( 'INPIC', LOCI, LOCDI, DNAMEI, ORIGIN, STATUS )

*       Check for error

         IF ( STATUS .NE. SAI__OK ) THEN

            CALL ERR_REP( 'MSTATS_N1ST',
     :        'MSTATS: Error whilst attempting to get locator to '/
     :        /'first frame.', STATUS )
            GOTO 999
         END IF

*    End of if-input-mode-is-sequential check

      END IF

*    Get the shape of the data-array component.

      CALL CMP_SHAPE( LOCDI, DNAMEI, NDIMS, DIMS, ACTDIM, STATUS )

*    Exit when there has been an error.

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( LOCI, STATUS )
         GOTO 999
      END IF

*    Check that there really is an array.

      IF ( ACTDIM .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'MSTATS_NOTARR',
     :     'MSTATS: First data array is actually a scalar.', STATUS )
  
         CALL DAT_ANNUL( LOCI, STATUS )
         GOTO 999
      END IF        

*    Use the returned shape to set up sensible defaults and limits for
*    the box size or pixel position as defined below - first ask user
*    whether Single pixel or Box mode is wanted

      CALL PAR_CHOIC( 'PIXMODE', 'Single', 'Single,Box', .TRUE., PIXMOD,
     :                STATUS )

      IF ( STATUS .EQ. PAR__ABORT ) THEN
         CALL DAT_ANNUL( LOCI, STATUS )
         GOTO 999
      END IF

*    Continue according to result

      IF ( PIXMOD( 1:1 ) .EQ. 'S' ) THEN

*       Single pixel mode wanted - set up flag

         SINGLE  =  .TRUE.

*       Get the position of the single pixel to be examined

         CALL PAR_GDR0I( 'XPIX', 1, 1, DIMS( 1 ), .FALSE., XPIX,
     :                   STATUS )
         CALL PAR_GDR0I( 'YPIX', 1, 1, DIMS( 2 ), .FALSE., YPIX,
     :                   STATUS )

*       As the same basic subroutine is used whether in Single pixel
*       or Box mode, set up the right arguments for its call

         XSTART  =  XPIX
         YSTART  =  YPIX
         XSIZE   =  1
         YSIZE   =  1

*    Else Box mode is wanted

      ELSE

*       Find out what kind of statistics required

         CALL PAR_GTD0L( 'ORDRST', .FALSE., .TRUE., ORDRST, STATUS )
         IF ( ORDRST ) THEN
            CALL MSG_OUT( 'STAT_TYPE', 'Ordered statistics to be '/
     :        /'computed', STATUS )
         END IF

*       Set flag

         SINGLE  =  .FALSE.

*       Get box bounds

         CALL PAR_GDR0I( 'XSTART', 1, 1, DIMS( 1 ), .TRUE., XSTART,
     :                    STATUS )
         CALL PAR_GDR0I( 'YSTART', 1, 1, DIMS( 2 ), .TRUE., YSTART,
     :                    STATUS )
         CALL PAR_GDR0I( 'XSIZE', ( DIMS( 1 ) - XSTART + 1 ), 1, 
     :                   ( DIMS( 1 ) - XSTART + 1 ), .TRUE., XSIZE,
     :                   STATUS )
         CALL PAR_GDR0I( 'YSIZE', ( DIMS( 2 ) - YSTART + 1 ), 1,
     :                   ( DIMS( 2 ) - YSTART + 1 ), .TRUE., YSIZE,
     :                   STATUS )

*    End of if-pixel-mode-is-Single-pixel check

      END IF


*    Check for error here

      IF ( STATUS .NE. SAI__OK ) THEN

*       Report context and annul locator to initial frame

         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'MSTATS_BOXPAR',
     :        'MSTATS: Error occurred whilst obtaining sub-array '/
     :        /'bounds.', STATUS )
         END IF

         CALL DAT_ANNUL( LOCI, STATUS )
         GOTO 999
      ELSE

*       Get the workspace needed for the processing - we need 3-d
*       workspace which is XSIZE by YSIZE by NUMSEQ, to hold the
*       sequence of sub-arrays - set up workspace dimensions first

         WKDIMS( 1 )  =  XSIZE
         WKDIMS( 2 )  =  YSIZE
         WKDIMS( 3 )  =  NUMSEQ

*       Create the temporary space

         CALL AIF_GETVM( '_REAL', NWKDIM, WKDIMS, WKPNTR, WKLOC,
     :                   STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN

            CALL ERR_REP( 'MSTATS_WSP',
     :        'MSTATS : Unable to get workspace.', STATUS )
            CALL AIF_ANTMP( WKLOC, STATUS )
            CALL DAT_ANNUL( LOCI, STATUS )
            GOTO 999
         ELSE
            WRKOBT = .TRUE.

         END IF

*    End of error check before mapping workspace

      END IF


*    Initialise variables before entering main loop

      FRAMNM  =  0
      DATAIN   =  0
      FINISH  =  .FALSE.

*    Now we can start the main loop

      DO WHILE ( STATUS .EQ. SAI__OK .AND. .NOT. FINISH .AND.
     :           FRAMNM .LT. NUMSEQ )

*       Increment the frame counter to keep track of where we are
*       in sequence

         FRAMNM  =  FRAMNM + 1

*       Reset the frame skip flag to false

         SKIP  =  .FALSE.

*       Check to see if this is the first frame or not

         IF ( FRAMNM .NE. 1 ) THEN

*          It is not the first frame - we need the next frame in
*          sequence, as only the first frame would have been got by
*          this point - get next frame according to input mode

            IF ( SEQUEN ) THEN

*             Sequential mode - get next name in sequence

               CURNAM = FILNAM
               CALL NXTNAM( CURNAM, FILNAM, STATUS )

*             Check for error on return

               IF ( STATUS .NE. SAI__OK ) THEN

*                The next filename in sequence is invalid - terminate
*                sequence here by setting skip flag true, finished flag
*                true, and outputting error message, followed by a flush

                  SKIP  =  .TRUE.
                  FINISH  =  .TRUE.
                  CALL MSG_SETI( 'BNUM', FRAMNM )
                  CALL MSG_SETC( 'BNAME', FILNAM )
                  CALL ERR_REP( 'MSTATS_BNXT',
     :              'MSTATS: Frame no. ^BNUM  has invalid name '/
     :              /'^BNAME - terminating here.', STATUS )
                  CALL ERR_FLUSH( STATUS )

*                Set flag to indicate bad data

                  GOODAT( FRAMNM )  =  .FALSE.

*             Else the next file name is ok - continue

               ELSE

*                Write the next file name of the sequence to the
*                parameter INPIC.

                  CALL AIF_PTFNM( 'INPIC', FILNAM, STATUS )

*                See if this structure exists

                  CALL DAT_EXIST( 'INPIC', 'READ', LOCI, STATUS )

*                Check that there is a data array.

                  CALL KPG1_GETIM( ' ', LOCI, LOCDI, DNAMEI, ORIGIN,
     :                             STATUS )

*                Check for error on return

                  IF ( STATUS .NE. SAI__OK ) THEN

*                   No such frame - skip it and output message to that
*                   effect

                     SKIP  =  .TRUE.
                     CALL MSG_SETI( 'BNUM', FRAMNM )
                     CALL MSG_SETC( 'BNAME', FILNAM )
                     CALL ERR_REP( 'MSTATS_BNDA',
     :                 'MSTATS: Frame no. ^BNUM : ^BNAME - does '/
     :                 /'not exist or does not have a data array - '/
     :                 /'skipping it.', STATUS )
                     CALL ERR_FLUSH( STATUS )

*                   Set flag to indicate bad data

                     GOODAT( FRAMNM )  =  .FALSE.

*                End of if-error-after-associating-filename-with-
*                parameter check

                  END IF

*             End of if-next-filename-in-sequence-is-no-good check

               END IF

*          Else input mode must be Random

            ELSE

*             Get locator to next frame

               CALL KPG1_GETIM( 'INPIC', LOCI, LOCDI, DNAMEI, ORIGIN,
     :                          STATUS )

*             Check for error

               IF ( STATUS .NE. SAI__OK ) THEN

                  IF ( STATUS .NE. PAR__ABORT ) THEN
                     SKIP  =  .TRUE.
                     CALL MSG_SETI( 'BNUM', FRAMNM )
                     CALL ERR_REP( 'MSTATS_NTHF',
     :                 'MSTATS: Error whilst attempting to get '/
     :                 /'locator to frame ^BNUM.', STATUS )
                     CALL ERR_FLUSH( STATUS )

*                   Set flag to indicate bad data

                     GOODAT( FRAMNM ) = .FALSE.
                  ELSE

                     CALL DAT_ANNUL( LOCI, STATUS )
                     GOTO 980
                  END IF

*             End of if-error-obtaining-next-frame check

               END IF

*          End of if-input-mode-is-sequential-check

            END IF

*       End of if-this-is-not-the-first-frame check

         END IF

         CALL MSG_SETI( 'BNUM', FRAMNM )

*       Now we can proceed if we have no errors and we are not skipping
*       the current frame

         IF ( STATUS .EQ. SAI__OK .AND. .NOT. SKIP ) THEN

*          Check that the array has the same dimension as the primary
*          (first) data array

            CALL CMP_SHAPE( LOCDI, DNAMEI, NDIMS, DIMSN,
     :                      ACTDIM, STATUS )

            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( DIMSN( 1 ) .NE. DIMS( 1 ) .OR.
     :              DIMSN( 2 ) .NE. DIMS( 2 ) ) THEN

*                Data array does not have the same dimensions - skip
*                it with error

                  STATUS = SAI__ERROR
                  SKIP  =  .TRUE.
                  CALL ERR_REP( 'MSTATS_WDIM',
     :              'MSTATS: Frame no. ^BNUM has wrong '/
     :              /'dimensions - skipping it.', STATUS )
                  CALL ERR_FLUSH( STATUS )

*                Set flag to indicate bad data

                  GOODAT( FRAMNM )  =  .FALSE.
               ELSE

*                Map the data array

                  CALL CMP_MAPN( LOCDI, DNAMEI, '_REAL', 'READ',
     :                           NDIMS, PNTRI, DIMS, STATUS )

*                Check for an error

                  IF ( STATUS .NE. SAI__OK ) THEN

*                   Skip frame with error output

                     SKIP  =  .TRUE.
                     CALL ERR_REP( 'MSTATS_NOMPI',
     :                 'MSTATS: Error occurred whilst trying to '/
     :                 /'map input frame ^BNUM.', STATUS )
                     CALL ERR_FLUSH( STATUS )

*                   Set flag to indicate bad data

                     GOODAT( FRAMNM )  =  .FALSE.
                  END IF

*             End of has-data-array-the-correct-dimensions check

               END IF

*          End of no-error-determining-the-shape-of-data-array check

            END IF

*          Proceed if we are not to skip this frame

            IF ( STATUS .EQ. SAI__OK .AND. .NOT. SKIP ) THEN

*             Copy the required sub-array of the current array into
*             the proper place in the 3-d workspace

               CALL SLC2T3( %VAL( PNTRI ), DIMS( 1 ), DIMS( 2 ), XSTART,
     :                      YSTART, XSIZE, YSIZE, WKDIMS( 1 ),
     :                      WKDIMS( 2 ), WKDIMS( 3 ), FRAMNM,
     :                      %VAL( WKPNTR ), STATUS )
 
               IF ( STATUS .EQ. SAI__OK ) THEN
       
*                We have some data - set a flag to true to show this,
*                and increment the success counter

                  GOODAT( FRAMNM )  =  .TRUE.
                  DATAIN  =  DATAIN + 1

*                Output a message to this effect too

                  CALL MSG_SETI( 'GNUM', FRAMNM )
                  CALL MSG_OUT( 'GOOD_DATA', ' Frame no. ^GNUM - data '/
     :              /'obtained successfully - continuing ... ', STATUS )

               ELSE
                  GOODAT( FRAMNM )  =  .FALSE.
               END IF

*             Unmap the data array component before looping

               CALL CMP_UNMAP( LOCDI, DNAMEI, STATUS )

*          End of if-eveything-ok-before-trying-to-copy-data check

            END IF

*          Annul the locator pointing to the current structures

            CALL DAT_ANNUL( LOCDI, STATUS )
            CALL DAT_ANNUL( LOCI, STATUS )

*       End of if-no-error-before-looking-for-data-array-in-frame check

         END IF

*       Cancel the parameter association with the current frame

         CALL PAR_CANCL( 'INPIC', STATUS )

*    End of loop round all requested input frames

      END DO


*    So, now we should have a 3-d workspace filled with the 2-d
*    sub-arrays from the sequence of input frames. Where there was a
*    problem and a frame was skipped, there will be no data in the 3-d
*    workspace, and this will be indicated by a GOODAT = .FALSE. entry
*    at the requisite point in that array. We will proceed now only if
*    at least two valid slices of data were read in

      IF ( STATUS .EQ. SAI__OK .AND. DATAIN .GT. 1 ) THEN

*       Set the default weighting

         DO  I = 1, FRAMNM
            WTARR( I ) = 1
         END DO

*       See which pixel mode we are in

         IF ( SINGLE ) THEN

*          We are in single pixel mode - call the subroutines that do
*          the 3-d stats, with the resultant mean, standard deviation
*          and median being returned to single variables

            CALL STAT3D( %VAL( WKPNTR ), WKDIMS( 1 ), WKDIMS( 2 ),
     :                   WKDIMS( 3 ), GOODAT, FRAMNM, MEAN, STDDEV,
     :                   GODNUM, STATUS )

            CALL MED3D( %VAL( WKPNTR ), WKDIMS( 1 ), WKDIMS( 2 ),
     :                  WKDIMS( 3 ), WTARR, GOODAT, FRAMNM,
     :                  MEDIAN, GODNUM, STATUS )

            IF ( STATUS .EQ. SAI__OK ) THEN

*             On return, output the results to the user

               CALL MSG_OUT( 'BLANK', ' ', STATUS )
               CALL MSG_SETI( 'GOODNUM', GODNUM )
               CALL MSG_OUT( 'SINGLE_GOODNUM', ' Number of arrays '/
     :           /'finally used       =  ^GOODNUM', STATUS )

               CALL MSG_SETI( 'XPIX', XPIX )
               CALL MSG_SETI( 'YPIX', YPIX )
               CALL MSG_OUT( 'SINGLE_PIXEL', ' Pixel position used in '/
     :           /'each array   =  ^XPIX,^YPIX', STATUS )
               CALL MSG_SETR( 'MEAN', MEAN )
               CALL MSG_OUT( 'SINGLE_MEAN', ' Mean value for pixel '/
     :           /'               =  ^MEAN', STATUS )
               CALL MSG_SETR( 'STDDEV', STDDEV )
               CALL MSG_OUT( 'SINGLE_STDDEV', ' Standard deviation '/
     :           /'over the frames  =  ^STDDEV', STATUS )
               CALL MSG_SETR( 'MEDIAN', MEDIAN )
               CALL MSG_OUT( 'SINGLE_MEDIAN', ' Median value for '/
     :           /'pixel              =  ^MEDIAN', STATUS )
               CALL MSG_OUT( 'BLANK', ' ', STATUS )
            END IF

*       Else we are in Box mode

         ELSE

            GFRSTO = ' '

*          Set up dimensions of output arrays

            ODIMS( 1 )  =  XSIZE
            ODIMS( 2 )  =  YSIZE

*          Origin is undefined for a series of images, so use the
*          default.

            DO  I = 1, NDIMS
               ORIGIN( I ) = 1
            END DO

            IF ( ORDRST ) THEN

*             Try to create the output structure to hold the array
*             of medians.  Note since the origin information is the
*             same, all the output IMAGE files will have the same
*             data-array component name.

               CALL KPG1_CROUT( 'MEDIAN', 'OMTITLE', NDIMS, ODIMS,
     :                          ORIGIN, MDLOC, LOCDO1, DNAMEO, STATUS )

*             Check status before mapping

               IF ( STATUS .EQ. SAI__OK ) THEN
 
*                Map data array component

                  CALL CMP_MAPN( LOCDO1, DNAMEO, '_REAL', 'WRITE',
     :                           NDIMS, MDPNTR, ODIMS, STATUS )

*                Check for error before continuing

                  IF ( STATUS .EQ. SAI__OK ) THEN

*                   Call the 3-d-median subroutine, which forms the
*                   stats for each pixel individually across the 3-d
*                   cube, depositing the results in the output array

                     CALL MED3D( %VAL( WKPNTR ), WKDIMS( 1 ),
     :                           WKDIMS( 2 ), WKDIMS( 3 ), WTARR,
     :                           GOODAT, FRAMNM, %VAL( MDPNTR ), GODNUM,
     :                           STATUS )

                     IF ( STATUS .EQ. SAI__OK ) THEN

*                      On return, output the final number of valid
*                      sub-arrays that went to make up the 2-d median
*                      array

                        CALL MSG_OUT( 'BLANK', ' ', STATUS )
                        CALL MSG_SETI( 'GOODNUM', GODNUM )
                        CALL MSG_OUT( 'BOX_GOODNUM', ' Number of '/
     :                    /'arrays finally used       =  ^GOODNUM',
     :                    STATUS )
                        CALL MSG_OUT( 'BOX_MESSAGE', ' Results in '/
     :                    /'median array as given', STATUS )
                        CALL MSG_OUT( 'BLANK', ' ', STATUS )
                     END IF

*                   Unmap output data arrays

                     CALL CMP_UNMAP( LOCDO1, DNAMEO, STATUS )

                  ELSE

*                   Output error message

                     CALL MSG_SETC( 'ARRAYTYPE', 'median' )
                     CALL ERR_REP( 'MSTATS_MAPO',
     :                 'MSTATS : Problem with mapping ^ARRAYTYPE '/
     :                 /'array.', STATUS )

*                End of if-error-after-mapping-data-array check

                  END IF

*                Tidy up the output structures

                  CALL DAT_ANNUL( LOCDO1, STATUS )
                  CALL DAT_ANNUL( MDLOC, STATUS )

               ELSE

*                Annul mean frame if it was obtained successfully

                  IF ( GFRSTO .EQ. 'sigma' ) THEN
                     CALL DAT_ANNUL( LOCDO2, STATUS )
                     CALL DAT_ANNUL( MLOC, STATUS )
                  END IF

*                Output error message

                  IF ( STATUS .NE. PAR__ABORT ) THEN
                     CALL MSG_SETC( 'ARRAYTYPE', 'median' )
                     CALL ERR_REP( 'MSTATS_CRFR',
     :                 'MSTATS: Problem with creating ^ARRAYTYPE '/
     :                 /'frame.', STATUS )
                  END IF

*             End of if-error-after-creating-output-structure check

               END IF

*          Compute mean and standard-deviation arrays

            ELSE
               GFRSTO = 'sigma'
               MFRSTO = 'sigma'

*             Try to create two output structures to hold the arrays
*             corresponding to the means and standard deviations

               CALL KPG1_CROUT( 'MEAN', 'MTITLE', NDIMS, ODIMS,
     :                          ORIGIN, MLOC, LOCDO2, DNAMEO, STATUS )
               IF ( STATUS .NE. SAI__OK ) GFRSTO = ' mean'
               CALL KPG1_CROUT( 'STDDEV', 'STITLE', NDIMS, ODIMS,
     :                          ORIGIN, SLOC, LOCDO3, DNAMEO, STATUS )

*             Check status before mapping

               IF ( STATUS .EQ. SAI__OK ) THEN
 
*                Map data array components for both

                  CALL CMP_MAPN( LOCDO2, DNAMEO, '_REAL', 'WRITE',
     :                           NDIMS, MPNTR, ODIMS, STATUS )
                  IF ( STATUS .NE. SAI__OK ) MFRSTO = ' mean'
                  CALL CMP_MAPN( LOCDO3, DNAMEO, '_REAL', 'WRITE',
     :                           NDIMS, SPNTR, ODIMS, STATUS )

*                Check for error before continuing

                  IF ( STATUS .EQ. SAI__OK ) THEN

*                   Call the 3-d-statistics subroutine, which forms the
*                   stats for each pixel individually across the 3-d
*                   cube, depositing the results in the two output
*                   arrays

                     CALL STAT3D( %VAL( WKPNTR ), WKDIMS( 1 ),
     :                            WKDIMS( 2 ), WKDIMS( 3 ), GOODAT, 
     :                            FRAMNM, %VAL( MPNTR ), %VAL( SPNTR ),
     :                            GODNUM, STATUS )

                     IF ( STATUS .EQ. SAI__OK ) THEN

*                      On return, output the final number of valid
*                      sub-arrays that went to make up the 2-d mean and
*                      standard deviation arrays

                        CALL MSG_OUT( 'BLANK', ' ', STATUS )
                        CALL MSG_SETI( 'GOODNUM', GODNUM )
                        CALL MSG_OUT( 'BOX_GOODNUM', ' Number of '/
     :                    /'arrays finally used       =  ^GOODNUM',
     :                    STATUS )
                        CALL MSG_OUT( 'BOX_MESSAGE', ' Results in '/
     :                    /'mean and standard deviation arrays as '/
     :                    /'given', STATUS )
                        CALL MSG_OUT( 'BLANK', ' ', STATUS )
                     END IF

*                   Unmap output data arrays

                     CALL CMP_UNMAP( LOCDO2, DNAMEO, STATUS )
                     CALL CMP_UNMAP( LOCDO3, DNAMEO, STATUS )

                  ELSE

*                   Unmap mean frame if it was mapped successfully

                     IF ( MFRSTO .EQ. 'sigma' )
     :                 CALL CMP_UNMAP( LOCDO2, DNAMEO, STATUS )

*                   Output error message

                     CALL MSG_SETC( 'ARRAYTYPE', MFRSTO )
                     CALL ERR_REP( 'MSTATS_MAPO',
     :                'MSTATS : Problem with mapping ^ARRAYTYPE array.',
     :                STATUS )

*                End of if-error-after-mapping-data-arrays check

                  END IF

*                Tidy up the output structures

                  CALL DAT_ANNUL( LOCDO2, STATUS )
                  CALL DAT_ANNUL( LOCDO3, STATUS )
                  CALL DAT_ANNUL( MLOC, STATUS )
                  CALL DAT_ANNUL( SLOC, STATUS )

               ELSE

*                Annul mean frame if it was obtained successfully

                  IF ( GFRSTO .EQ. 'sigma' ) THEN
                     CALL DAT_ANNUL( LOCDO2, STATUS )
                     CALL DAT_ANNUL( MLOC, STATUS )
                  END IF

*                Output error message

                  IF ( STATUS .NE. PAR__ABORT ) THEN
                     CALL MSG_SETC( 'ARRAYTYPE', GFRSTO )
                     CALL ERR_REP( 'MSTATS_CRFR',
     :                 'MSTATS: Problem with creating ^ARRAYTYPE '/
     :                 /'frame.', STATUS )
                  END IF

*             End of if-error-after-creating-output-structures check

               END IF

*          End of ordered-statistics check

            END IF

*       End of if-Single-pixel-mode check

         END IF

*    Perhaps only one (or less) valid 2-d sub-array finally made into
*    the 3-d workspace

      ELSE IF ( STATUS .EQ. SAI__OK .AND. DATAIN .LE. 1 ) THEN

*       Output a message to that effect

         STATUS = SAI__ERROR
         CALL ERR_REP( 'MSTATS_NEND',
     :     'MSTATS: Not enough data taken in on which to do '/
     :     /'statistics - aborting.', STATUS )

*    Check for null parameters having been input somewhere above

      ELSE IF ( STATUS .EQ. PAR__NULL .OR. 
     :          STATUS .EQ. PAR__ABORT ) THEN

*       Output error message and return

         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_REP( 'MSTATS_OTP',
     :        'MSTATS: Problem with parameters - aborting.', STATUS )
         END IF

*    Else a gremlin

      ELSE

*       Tell user in not so many words

         CALL ERR_REP( 'MSTATS_GREM',
     :     'MSTATS: Unknown gremlin has cropped up - aborting.',
     :     STATUS )

*    End of if-one-or-less-2d-sub-arrays-were-included check

      END IF

 980  CONTINUE

*    Tidy workspace

      IF ( WRKOBT ) CALL AIF_ANTMP( WKLOC, STATUS )

 999  CONTINUE

*    end and return

      END
