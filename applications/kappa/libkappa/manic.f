*+  MANIC - Converts all or part of a data array from one dimensionality
*           to another 

      SUBROUTINE MANIC( STATUS )
*
*    Description :
*
*     This application copies or converts all or part of a 1, 2 or
*     3-dimensional data array to one or more output data arrays, each
*     of 1, 2 or 3 dimensions. All data arrays are stored in IMAGE
*     structures. Windows may be set in any of the dimensions of the
*     input data array. All or part of the input array may be projected
*     on to any of the rectangular planes or axes of the input before
*     being written to an output array; or a 1- or 2-dimensional data
*     array may be grown to more dimensions to fill an output data
*     array. Many output data arrays, each of a different configuration
*     if required, may be extracted from a single input data array with
*     one call to the routine. 
*
*    Invocation :
*
*     CALL MANIC( STATUS )
*
*    Parameters :
*
*     INPUT      = IMAGE( READ )
*         IMAGE structure holding the input data array.
*     ONDIM      = INTEGER( READ )
*         Dimensionality of an output data array.
*     XLIMITS(2) = INTEGER( READ )
*         The X-axis window on the input data array to be used in
*           forming an output data array.
*     YLIMITS(2) = INTEGER( READ )
*         The Y-axis window on the input data array to be used in
*           forming an output data array.
*     ZLIMITS(2) = INTEGER( READ )
*         The Z-axis window on the input data array to be used in
*           forming an output data array.
*     XRANGE(2)  = INTEGER( READ )
*         The X-axis range for summation in the input data array in
*           forming an output data array.
*     YRANGE(2)  = INTEGER( READ )
*         The Y-axis range for summation in the input data array in
*           forming an output data array.
*     ZRANGE(2)  = INTEGER( READ )
*         The Z-axis range for summation in the input data array in
*           forming an output data array.
*     EPLANE     = CHAR( READ )
*         Plane to be extracted from the input 3-d data array.  The
*           options are 'XY', 'YZ', 'ZX', 'YX', 'ZY', 'XZ'.
*     GPLANE     = CHAR( READ )
*         Input 2-d data array forms this plane when being grown into
*           a 3-d data array.  The options are 'XY', 'YZ', 'ZX', 'YX',
*           'ZY', 'XZ'.
*     ELINE1     = CHAR( READ )
*         Axis of input 2-d data array to be extracted to form an output
*           1-d data array.  The alternatives are 'X' or 'Y'.
*     ELINE2     = CHAR( READ )
*         Axis of input 3-d data array to be extracted to form an output
*           1-d data array.  The options are 'X', 'Y', 'Z'.
*     GLINE1     = CHAR( READ )
*         Input 1-d data array will form this axis of an output 2-d
*           data array.  The alternatives are 'X' or 'Y'.
*     GLINE2     = CHAR( READ )
*         Input 1-d data array will form this axis of an output 3-d
*           data array.  The options are 'X', 'Y', 'Z'.
*     XDIM       = INTEGER( READ )
*         X-dimension of output 2-d or 3-d data array grown from input
*           1-d or 2-d data array.
*     YDIM       = INTEGER( READ )
*         Y-dimension of output 2-d or 3-d data array grown from input
*           1-d or 2-d data array.
*     ZDIM       = INTEGER( READ )
*         Z-dimension of output 2-d or 3-d data array grown from input
*           1-d or 2-d data array.
*     OUTPUT     = IMAGE( WRITE )
*         IMAGE structure to hold an output data array.
*     OTITLE     = CHAR( WRITE )
*         Title for IMAGE structure holding an output data array.
*     LOOP       = LOGICAL( READ )
*         Extract or grow further output data arrays from the same input
*           data array.
*
*    Arguments:
*
*     STATUS  = INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get input IMAGE type data structure
*     Find out shape of input data-array component
*     If array is 1 to 3-dimensional then
*        Initialise LOOP to .TRUE. to start off the loop
*        Do while LOOP = .TRUE. and no errors occur
*           Tell user shape of input array
*           Get dimensionality of output array
*           If no error so far then
*              If output array has more dimensions the input array then
*                 Call MAMORE to set up output array dimensions and
*                   input array slice limits.
*              Else if output array has same no. of dimensions as input
*                array then
*                 Call MASAME to set up output array dimensions and
*                   input array slice limits.
*              Else
*                 Output array must have fewer dimensions than input
*                   array so call MALESS to set up output array
*                   dimensions and input array slice limits.
*              Endif
*           Endif
*           If no error then
*              Get locator to and then map input array slice
*              If no error then
*                 Create output data structure containing a DATA_ARRAY
*                   component of appropriate dimensionality and
*                   dimensions, and also create and get a value for a
*                   TITLE component
*                 Propagate NDF MORE from the input data file
*                 If no error then
*                    Map output data-array component
*                    If no error then
*                       If CASE is one where output array has fewer
*                         dimensions than input then
*                          Find dimension of workspace arrays
*                          Obtain and map workspace arrays
*                       Endif
*                       If no error so far then
*                          Perform input to output transformation
*                            according to value of CASE
*                          Tidy second work array if exists
*                       Else
*                          Report error
*                       Endif
*                       Tidy first work array if exists
*                       Unmap output data array
*                    Else
*                       Report error context
*                    Endif
*                    Tidy output structure
*                 Else
*                    Report error context
*                 Endif
*                 Unmap slice
*              Else
*                 Report error context
*              Endif
*           Else
*              Report error context
*           Endif
*           Tidy up the slice
*           Ask user if to repeat process for same input array
*           If loop then
*              Cancel parameters if necessary
*           Endif
*        Enddo
*        Tidy up input data structure
*     Else
*        Report error context
*     Endif
*     End
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     C D Pike    (RGO::CDP)
*     Roger Wood  (RGO::RW)
*     Dave Baines (ROE::ASOC5)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     15/08/1981 : Original version    (RGO::CDP)
*     30/03/1983 : Amended  version    (RGO::RW)
*     21/02/1984 : Revised SSE version (ROE::ASOC5)
*     1986 Aug 7 : Standardised prologue formatting. Status check on
*                  entry added (RAL::CUR).
*     1986 Aug 29: Renamed APP routines into AIF_. Added argument
*                  section to prologue and tidied. Revised output
*                  of dimensions via DIMLST routine (RAL::CUR).
*     1986 Oct 30: Allowed for bad pixel handling in routines MA3TO1,
*                  MA3TO2 and MA2TO1 for which workspace of the
*                  appropriate dimension is now found (RAL::CUR).
*     1987 Oct 15: Reordered tidying and extra status checks
*                  (RAL::CUR).
*     1988 Mar 16: Substituted AIF_ANTMP to annul workspace
*                  (RAL::CUR).
*     1988 Mar 17: Referred to `array' rather than `image'
*                  (RAL::CUR).
*     1988 Jun 20: More reporting of error context (RAL::CUR).
*     1989 Jun 13: Allow for processing primitive NDFs (RAL::CUR).
*     1989 Aug  8: Passed array dimensions as separate variables
*                  to COPY2D, COPY3D, MA1TO2, MA1TO3, MA2TO1, MA2TO3,
*                  MA3TO1 and MA3TO2 (RAL::CUR).
*     1989 Dec 21: Workspace managed by AIF_TEMP (RAL::CUR).
*     1991 Oct 25: Propagates UNITS, LABEL and HISTORY (RAL::CUR).
*     1992 Feb 25: Limited processing of simple NDFs (RAL::CUR).
*     1992 Mar  3: Replaced AIF parameter-system calls by the extended
*                  PAR library (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT NONE            ! no impicit typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'        ! global SSE definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PAR_ERR'        ! parameter-system errors

*    Status :

      INTEGER STATUS

*    Local constants :

      INTEGER MXSIZE
      PARAMETER ( MXSIZE = 1000 )

*    Local variables :

      CHARACTER*( DAT__SZLOC ) ! locators to :
     :  DLOCI,                 ! input primitive data array
     :  LOCDI,                 ! structure containing the input data
                               ! array
     :  LOCDO,                 ! structure containing the output data
                               ! array
     :  LOCI,                  ! input data structure
     :  LOCO,                  ! output data structure
     :  WLOC1,                 ! workspace for MA3TO1, MA3TO2 and MA2TO1
     :  WLOC2,                 !     "      "    "       "     "    "
     :  SLICE                  ! slice of input data_array

      CHARACTER * ( DAT__SZNAM )
     :  DNAMEI,                ! Name of the input data-array component
     :  DNAMEO                 ! Name of the output data-array component

      INTEGER
     :  I,                     ! loop counter
     :  IDIMS( DAT__MXDIM ),   ! input array dimensions
     :  ODIMS( DAT__MXDIM ),   ! output  "        "
     :  SDIMS( DAT__MXDIM ),   ! input array slice dimensions
     :  UPPER( 3 ),            ! upper bounds for slice
     :  LOWER( 3 ),            ! lower    "    "    "
     :  MDIM,                  ! work variable used to compute dimension
                               ! of workspace arrays
     :  WDIM,                  ! dimension of workspace arrays
     :  NCDIM,                 ! number characters in dimension list
     :  ORIGIN( DAT__MXDIM ),  ! Origin of the data array
     :  PNTRI,                 ! pointer to input data-array component
     :  PNTRO,                 !    "     " output    "          "
     :  WPNTR1,                !    "     " workspace
     :  WPNTR2,                !    "     " workspace
     :  INDIM,                 ! dimensionality of input array
     :  ONDIM,                 !        "        " output  "
     :  MODE,                  ! decision parameter for subroutines
     :  CASE                   ! type of extraction/projection to be
                               ! performed

      CHARACTER*20
     :  DIMSTR                 ! list of dimensions of an input array

      LOGICAL
     :  LOOP                   ! will be .true. if further output arrays
                               ! to be created
*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    get a locator to input IMAGE type data structure

      CALL KPG1_GETIM( 'INPUT', LOCI, LOCDI, DNAMEI, ORIGIN, STATUS )

*    origin information is lost.

      DO  I = 1, DAT__MXDIM
         ORIGIN( I ) = 1
      END DO

*    get a locator to the input data-array object.

      CALL DAT_FIND( LOCDI, DNAMEI, DLOCI, STATUS )

*    enquire shape of input data-array component

      INDIM = 0
      CALL DAT_SHAPE( DLOCI, DAT__MXDIM, IDIMS, INDIM, STATUS )

*    check for error

      IF ( STATUS .EQ. SAI__OK ) THEN

*       check that dimensionality is within allowed range

         IF ( ( INDIM .GT. 0 ) .AND. ( INDIM .LT. 4 ) ) THEN

*          initialise LOOP to .TRUE. to start off the loop

            LOOP = .TRUE.

*          repeat the loop as long as LOOP is 'Y' and no errors occur

            DO WHILE ( LOOP .AND. ( STATUS .EQ. SAI__OK ) )

*             tell user dimensionality and dimensions of input array

               CALL MSG_SETI( 'INDIM', INDIM )
               CALL MSG_OUT( 'INPUT_INDIM',
     :           'Array is ^INDIM -dimensional', STATUS )
               CALL DIMLST( INDIM, IDIMS, NCDIM, DIMSTR, STATUS )
               CALL MSG_SETC( 'DIMSTR', DIMSTR( 1:NCDIM ) )
               CALL MSG_OUT( 'INPUT_DIM',
     :           'Dimensions are ^DIMSTR', STATUS )

*             enquire number of dimensions for output array

               CALL PAR_GDR0I( 'ONDIM', INDIM, 1, 3, .TRUE., ONDIM,
     :                         STATUS )

*             deal with the three basic cases, output array with more,
*             same or less dimensions than the input array

               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( ONDIM .GT. INDIM ) THEN

                     CALL MAMORE( 'XLIMITS', 'YLIMITS', 'ZLIMITS',
     :                            'XDIM', 'YDIM', 'ZDIM', 'GLINE1',
     :                            'GLINE2', 'GPLANE', MXSIZE, INDIM,
     :                            IDIMS, ONDIM, ODIMS, LOWER, UPPER,
     :                            CASE, MODE, STATUS )

                  ELSE IF ( ONDIM .EQ. INDIM ) THEN

*                   here CASE is the same as the dimensionality of the
*                   arrays

                     CASE = INDIM
                     CALL MASAME( 'XLIMITS', 'YLIMITS', 'ZLIMITS',
     :                            INDIM, IDIMS, ODIMS, LOWER, UPPER,
     :                            STATUS )
                  ELSE

                     CALL MALESS( 'XLIMITS', 'YLIMITS', 'ZLIMITS',
     :                            'XRANGE', 'YRANGE', 'ZRANGE',
     :                            'ELINE1', 'ELINE2', 'EPLANE', INDIM,
     :                            IDIMS, ONDIM, ODIMS, LOWER, UPPER,
     :                            CASE, MODE, STATUS )
                  END IF
               END IF

               IF ( STATUS .EQ. SAI__OK ) THEN

*                get a locator to the required slice of the input array

                  CALL DAT_SLICE( DLOCI, INDIM, LOWER, UPPER, SLICE,
     :                            STATUS )

*                map the input array slice

                  CALL DAT_MAPN( SLICE, '_REAL', 'READ', INDIM, PNTRI,
     :                           SDIMS, STATUS )

*                check for error

                  IF ( STATUS .EQ. SAI__OK ) THEN

*                   create the output IMAGE type data structure, create
*                   and get a value for a TITLE component and create a
*                   data-array component of dimensionality ONDIM and
*                   dimensions DIMS(ONDIM)

                     CALL KPG1_CROUT( 'OUTPUT', 'OTITLE', ONDIM, ODIMS,
     :                                ORIGIN, LOCO, LOCDO, DNAMEO,
     :                                STATUS )

*                   propagate UNITS, LABEL, HISTORY and extensions from
*                   the input data file

                     CALL KPG1_IMPRG( LOCI, 'UNITS', LOCO, STATUS )

*                   check for error

                     IF ( STATUS .EQ. SAI__OK ) THEN

*                      map the output data-array component

                        CALL CMP_MAPN( LOCDO, DNAMEO, '_REAL', 'WRITE',
     :                                 ONDIM, PNTRO, ODIMS, STATUS )

*                      check for error

                        IF ( STATUS .EQ. SAI__OK ) THEN

*                         when the array dimension is less work arrays
*                         are needed to compute normalised output
*                         arrays which allow for bad pixels

                           IF ( CASE .EQ. 4 .OR. CASE .EQ. 5 .OR.
     :                          CASE .EQ. 7) THEN

*                            workspace dimension is the product of the
*                            largest two input dimensions

                              IF ( CASE .EQ. 4 ) THEN
                                 MDIM = MIN( IDIMS(1), IDIMS(2),
     :                                       IDIMS(3) )
                                 WDIM = ( IDIMS(1) * IDIMS(2) *
     :                                    IDIMS(3) ) / MDIM
                              ELSE

*                               workspace dimension is the largest
*                               input-array dimension

                                 WDIM = -1
                                 DO  I = 1, INDIM
                                    WDIM = MAX( WDIM, IDIMS( I ) )
                                 END DO
                              END IF

*                            create and map workspace arrays

                              CALL AIF_GETVM( '_REAL', 1, WDIM, WPNTR1,
     :                                        WLOC1, STATUS )
                              CALL AIF_GETVM( '_INTEGER', 1, WDIM,
     :                                        WPNTR2, WLOC2, STATUS )
                           END IF

*                         check for error

                           IF ( STATUS .EQ. SAI__OK ) THEN

*                            perform the reshaping of the array

*                            1-d to 1-d
 
                              IF ( CASE .EQ. 1 ) THEN

                                 CALL COPY1D( SDIMS, %VAL( PNTRI ),
     :                                        %VAL( PNTRO ), STATUS )

*                            2-d to 2-d
 
                              ELSE IF ( CASE .EQ. 2 ) THEN

                                 CALL COPY2D( SDIMS( 1 ), SDIMS( 2 ),
     :                                        %VAL( PNTRI ),
     :                                        %VAL( PNTRO ), STATUS )

*                            3-d to 3-d
 
                              ELSE IF ( CASE .EQ. 3 ) THEN

                                 CALL COPY3D( SDIMS( 1 ), SDIMS( 2 ),
     :                                        SDIMS( 3 ), %VAL( PNTRI ),
     :                                        %VAL( PNTRO ), STATUS )

                              ELSE IF ( CASE .EQ. 4 ) THEN

                                 CALL MA3TO2( MODE, SDIMS( 1 ),
     :                                        SDIMS( 2 ), SDIMS( 3 ),
     :                                        %VAL( PNTRI ), ODIMS( 1 ),
     :                                        ODIMS( 2 ), WDIM,
     :                                        %VAL(WPNTR1),
     :                                        %VAL( WPNTR2 ),
     :                                        %VAL( PNTRO ), STATUS )

                              ELSE IF ( CASE .EQ. 5 ) THEN

                                 CALL MA3TO1( MODE, SDIMS( 1 ),
     :                                        SDIMS( 2 ), SDIMS( 3 ),
     :                                        %VAL( PNTRI ), ODIMS( 1 ),
     :                                        WDIM, %VAL( WPNTR1 ),
     :                                        %VAL( WPNTR2 ),
     :                                        %VAL( PNTRO ), STATUS )

                              ELSE IF ( CASE .EQ. 6 ) THEN

                                 CALL MA2TO3( MODE, SDIMS( 1 ),
     :                                        SDIMS( 2 ), %VAL( PNTRI ),
     :                                        ODIMS( 1 ), ODIMS( 2 ),
     :                                        ODIMS( 3 ), %VAL( PNTRO ),
     :                                        STATUS )

                              ELSE IF ( CASE .EQ. 7 ) THEN

                                 CALL MA2TO1( MODE, SDIMS( 1 ),
     :                                        SDIMS( 2 ), %VAL( PNTRI ),
     :                                        ODIMS( 1 ), WDIM,
     :                                        %VAL( WPNTR1 ),
     :                                        %VAL( WPNTR2 ), 
     :                                        %VAL( PNTRO ), STATUS )

                              ELSE IF ( CASE .EQ. 8 ) THEN

                                 CALL MA1TO3( MODE, SDIMS( 1 ),
     :                                        %VAL( PNTRI ), ODIMS( 1 ),
     :                                        ODIMS( 2 ), ODIMS( 3 ),
     :                                        %VAL( PNTRO ), STATUS )

                              ELSE IF ( CASE .EQ. 9 ) THEN

                                 CALL MA1TO2( MODE, SDIMS( 1 ),
     :                                        %VAL( PNTRI ), ODIMS( 1 ),
     :                                        ODIMS( 2 ), %VAL( PNTRO ),
     :                                        STATUS )
                              END IF

*                            tidy second work array

                              IF ( CASE .EQ. 4 .OR. CASE .EQ. 5 .OR.
     :                             CASE .EQ. 7 ) THEN

                                 CALL AIF_ANTMP( WLOC2, STATUS )
                              END IF

                           ELSE

                              CALL ERR_REP( 'ERR_MANIC_WSP', 
     :                          'MANIC: Unable to get workspace for '/
     :                          /'averaging', STATUS )

*                         end of creating-and-mapping-workspace check

                           END IF

*                         tidy first work array

                           IF ( CASE .EQ. 4 .OR. CASE .EQ. 5 .OR.
     :                          CASE .EQ. 7 ) THEN

                              CALL AIF_ANTMP( WLOC1, STATUS )

                           END IF

*                         unmap output data array

                           CALL CMP_UNMAP( LOCDO, DNAMEO, STATUS )

                        ELSE

                           CALL ERR_REP( 'ERR_MANIC_NOMPO',
     :                       'MANIC: Error occurred whilst trying to '/
     :                       /'map output frame', STATUS )

*                     end of if-no-error-after-mapping-output-data-array
*                     check

                        END IF

*                      tidy up output structures

                        CALL DAT_ANNUL( LOCDO, STATUS )
                        CALL DAT_ANNUL( LOCO, STATUS )

                     ELSE

                        IF ( STATUS .NE. PAR__ABORT ) THEN
                           CALL ERR_REP( 'ERR_MANIC_NOFRO',
     :                       'MANIC: Error occurred whilst trying to '/
     :                       /'access output frame', STATUS )
                        END IF

*                   end of if-no-error-after-creating-output-structure
*                   check

                     END IF

*                   unmap the slice

                     CALL DAT_UNMAP( SLICE, STATUS )

                  ELSE

                     CALL ERR_REP( 'ERR_MANIC_NOMPI',
     :                 'MANIC: Error occurred whilst trying to map '/
     :                 /'input frame', STATUS )

*                end of mapping-slice-pointer check

                  END IF

               ELSE

                  IF ( STATUS .NE. PAR__ABORT .AND.
     :                 STATUS .NE. PAR__NULL ) THEN
                     CALL ERR_REP( 'ERR_MANIC_PAR',
     :                 'MANIC: Error occurred whilst obtaining pixel '/
     :                 /'limits', STATUS )
                  END IF

*             end of no-error-getting-pixel-limitsd check

               END IF

*             tidy up the input slice

               CALL DAT_ANNUL( SLICE, STATUS )

*             enquire if user wishes to make another array from same
*             input

               CALL PAR_GET0L( 'LOOP', LOOP, STATUS )

*             if reply was Yes then cancel all parameters except INPUT

               IF ( LOOP .AND. ( STATUS .EQ. SAI__OK ) ) THEN

                  CALL DAT_CANCL(  'OUTPUT', STATUS )
                  CALL PAR_CANCL(   'ONDIM', STATUS )
                  CALL PAR_CANCL(  'OTITLE', STATUS )
                  CALL PAR_CANCL( 'XLIMITS', STATUS )
                  CALL PAR_CANCL( 'YLIMITS', STATUS )
                  CALL PAR_CANCL( 'ZLIMITS', STATUS )
                  CALL PAR_CANCL(  'XRANGE', STATUS )
                  CALL PAR_CANCL(  'YRANGE', STATUS )
                  CALL PAR_CANCL(  'ZRANGE', STATUS )
                  CALL PAR_CANCL(  'EPLANE', STATUS )
                  CALL PAR_CANCL(  'GPLANE', STATUS )
                  CALL PAR_CANCL(  'ELINE1', STATUS )
                  CALL PAR_CANCL(  'ELINE2', STATUS )
                  CALL PAR_CANCL(  'GLINE1', STATUS )
                  CALL PAR_CANCL(  'GLINE2', STATUS )
                  CALL PAR_CANCL(    'XDIM', STATUS )
                  CALL PAR_CANCL(    'YDIM', STATUS )
                  CALL PAR_CANCL(    'ZDIM', STATUS )
                  CALL PAR_CANCL(    'LOOP', STATUS )
               END IF

*          end of current-'manic' loop

            END DO
         ELSE

*          input array has wrong dimensionality

            STATUS = SAI__ERROR
            CALL MSG_SETI( 'INDIM', INDIM )
            CALL ERR_REP( 'ERR_MANIC_WRDIM',
     :        'MANIC: Array is ^INDIM dimensional, not 1,2 or 3-d',
     :        STATUS )

*       end of input-data-array-dimension check

         END IF

*       tidy up the input structures

         CALL DAT_ANNUL( DLOCI, STATUS )
         CALL DAT_ANNUL( LOCDI, STATUS )
         CALL DAT_ANNUL(  LOCI, STATUS )

      ELSE

         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'ERR_MANIC_NOFRI',
     :        'MANIC: Error occurred whilst trying to access input '/
     :        /'frame', STATUS )
         END IF

*    end of if-no-error-after-getting-input-structure check

      END IF

      END
