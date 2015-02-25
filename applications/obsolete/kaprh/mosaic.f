      SUBROUTINE MOSAIC ( STATUS )
*+
*  Name:
*     MOSAIC

*  Purpose:
*     Mosaic up to 20 non-congruent 2-d data arrays.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation
*     CALL MOSAIC( STATUS )

*  Description:
*     Up to 20 non-congruent 2-d data arrays may be input, along with
*     their relative offsets from the first data array, and these are
*     then made into a mosaic into one (usually larger) output 2-d data
*     array. Where the frames overlap, either the mean value or just the
*     sum is inserted into the output data array. Normally averaging is
*     performed. All data arrays are stored in IMAGE structures.
*
*     The magic-value method is used for processing bad data.  Bad
*     pixels are excluded from the averaging in overlap areas. Output
*     pixels that have been mapped or correspond to one or more input
*     arrays, yet have no good pixels contributing, are set to bad.
*     Pixels in the output data array not mapped by any of the input
*     arrays are set to zero.

*  ADAM Parameters:
*     NUMBER = _INTEGER (READ)
*         Number of data arrays to be merged
*     AVERAGE  = _LOGICAL (READ)
*         If true overlap regions are averaged, alternatively, they are
*         summed
*     INPICn = IMAGE (READ)
*         nth IMAGE structure containing a data array to be a
*           constituent of a mosaic
*     OUTPIC = IMAGE (WRITE)
*         Output IMAGE structure containing the merged data array
*     OTITLE = LITERAL (READ)
*         Title for the output IMAGE structure
*     XOFFSET =  _INTEGER (READ)
*         x offset of nth data array from the first, in the sense of the
*         x origin of the nth data array minus the x origin of the
*         first.
*     YOFFSET =  _INTEGER (READ)
*         y offset of nth data array from the first, in the sense of the
*         y origin of the nth data array minus the y origin of the
*         first.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Related Applications:
*     CCDPACK: MAKEMOS.

*  Authors:
*     MJC: Malcolm J. Currie  STARLINK
*     Mark McCaughrean UOE (REVA::MJM)
*     {enter_new_authors_here}

*  History:
*     16-09-1985 : First implementation (REVA::MJM)
*     1986 Aug 7 : Renamed algorithm subroutines (MOSAIC_ADD to MOSCAD,
*                  MOSAIC_DIV to MOSCDV). Correctly ordered arguments
*                  in MOSCAD (7th to 5th). Added invocation to prologue
*                  (RL.STAR::CUR).
*     1986 Aug 29: Completed prologue (method and arguments), added
*                  status checking, and NUMREA variable so that the
*                  correct tidying of input structures is done. Nearly
*                  conformed to Starlink standards (RL.STAR::CUR).
*     1987 Oct 16: Reordered tidying and extra status checks
*                  (RL.STAR::CUR)
*     1988 Mar 16: Substituted AIF_ANTMP to annul workspace
*                  (RL.STAR::CUR).
*     1988 Mar 17: Referred to `array' rather than `image'
*                  (RL.STAR::CUR)
*     1988 May 31: Separated normalisation operation, and added
*                  average option (RL.STAR::CUR).
*     1988 Jun 21: More reporting of error context, added good data
*                  flag and checking (RL.STAR::CUR)
*     1988 Oct 20: Bug fix in offsets introduced 1988 May 31
*                  (RL.STAR::CUR)
*     1989 Jun 13: Allow for processing primitive NDFs (RL.STAR::CUR)
*     1989 Aug  7: Passed array dimensions as separate variables
*                  to MOSCAD, MOSCDV and ZERO2D (RL.STAR::CUR).
*     1989 Dec 21: Workspace managed by AIF_TEMP (RL.STAR::CUR).
*     1990 Jan 13: Input data now unmapped on exit (RL.STAR::CUR).
*     1991 Oct 25: Propagates UNITS, LABEL and HISTORY from primary
*                  NDF (RAL::CUR).
*     1992 Feb 25: Limited processing of simple NDFs (RAL::CUR).
*     1992 Mar  3: Replaced AIF parameter-system calls by the extended
*                  PAR library (RAL::CUR).
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*
*    Type Definitions :

      IMPLICIT NONE            ! no implicit typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'        ! global SSE definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PAR_ERR'        ! parameter-system errors

*    Status :

      INTEGER STATUS

*    Local Constants :

      INTEGER
     :  NDIMS,                 ! dimensionality of arrays
     :  MXFRAM                 ! maximum number of frames allowed

      PARAMETER ( NDIMS = 2 )
      PARAMETER ( MXFRAM = 20 )

*    Local variables :

      INTEGER
     :  NUMBER,                ! number of frames to be merged
     :  NUMREA,                ! number of frames located and mapped
     :  FIRSTG,                ! the number of the first data array
                               ! read and mapped sucessfully
     :  IDIMS( NDIMS, MXFRAM ),! dimensions of input DATA_ARRAYs
     :  ORIGIN( DAT__MXDIM ),  ! Origin of the data array
     :  PNTRI( MXFRAM ),       ! pointers to input DATA_ARRAYs
     :  XOFSET( MXFRAM ),      ! x offset of Nth frame from first
     :  YOFSET( MXFRAM ),      ! y   "     "  "    "     "    "
     :  PNTRO,                 ! pointer to output DATA_ARRAY
     :  ODIMS( NDIMS ),        ! dimensions of output DATA_ARRAY
     :  PNTRT,                 ! pointer to data-array mask array
     :  MINX,                  ! minimum x offset from first frame
     :  MINY,                  !    "    y   "      "    "     "
     :  MAXX,                  ! maximum x   "      "    "     "
     :  MAXY,                  !    "    y   "      "    "     "
     :  I, J, K, L, M          ! counters

      CHARACTER*(DAT__SZLOC)   ! locators for :
     :  LOCDI( MXFRAM ),       ! structure containing the input data
                               ! array
     :  LOCDO,                 ! structure containing the output data
                               ! array
     :  LOCI( MXFRAM ),        ! input data structure
     :  LOCO,                  ! output data structure
     :  LOCT                   ! temporary array mask

      CHARACTER * ( DAT__SZNAM )
     :  DNAMEI( MXFRAM ),      ! Name of the input data-array components
     :  DNAMEO                 ! Name of the output data-array component

      LOGICAL                  ! true if :
     :  AVERGE,                ! overlap regions are averaged
     :  GOODAT( MXFRAM )       ! entry corresponding to given frame in
                               ! sequence is true if data and x,y
                               ! offsets were found ok

      CHARACTER*20
     :  INPARM( MXFRAM )       ! array containing INPICn parameters

*    Local data :

      DATA  INPARM / 'INPIC1', 'INPIC2', 'INPIC3', 'INPIC4',
     :               'INPIC5', 'INPIC6', 'INPIC7', 'INPIC8',
     :               'INPIC9', 'INPIC10', 'INPIC11', 'INPIC12',
     :               'INPIC13', 'INPIC14', 'INPIC15', 'INPIC16',
     :               'INPIC17', 'INPIC18', 'INPIC19', 'INPIC20' /

*-

*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    start by getting number of frames to be merged

      CALL PAR_GDR0I( 'NUMBER', 2, 2, MXFRAM, .FALSE., NUMBER, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN

         IF ( STATUS .NE.  PAR__ABORT ) THEN
            CALL ERR_REP( 'ERR_MOSAIC_PAR',
     :        'MOSAIC: Error obtaining the number of frames', STATUS )
         END IF
         GOTO 999
      END IF

*    Ask user if want to average the overlap region or not

      CALL PAR_GTD0L( 'AVERAGE', .TRUE., .TRUE., AVERGE, STATUS )

*    Check status before continuing

      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'ERR_MOSAIC_PAR2',
     :        'MOSAIC: Error obtaining how to process '/
     :        /'overlapping data arrays', STATUS )
         END IF
         GOTO 999
      END IF

*    initialise data ok flags

      DO  I = 1, MXFRAM
         GOODAT( I ) = .FALSE.
      END DO
      FIRSTG = 1

*    now get the required number of DATA_ARRAYs

      DO  I = 1, NUMBER

*       tell user which number frame is required

         CALL MSG_SETI( 'FRAMENO', I )
         CALL MSG_OUT( 'NEXT_FRAME',
     :                 'Input frame number ^FRAMENO', STATUS )

*       get a locator to an IMAGE-type data structure then cancel
*       parameter.  Do not need individual origins as this information
*       is ignored.

         CALL KPG1_GETIM( INPARM( I ), LOCI( I ), LOCDI( I ),
     :                    DNAMEI( I ), ORIGIN, STATUS )

*       Report error including frame number

         IF ( STATUS .NE. SAI__OK ) THEN
            IF ( STATUS .NE. PAR__ABORT ) THEN

*             Report error and set flag for bad data

               CALL MSG_SETI( 'FRAMENO', I )
               CALL ERR_REP( 'ERR_MOSAIC_NOFRI',
     :           'MOSAIC: Error occurred whilst trying to access '/
     :           /'input frame number ^FRAMENO.', STATUS )
               CALL ERR_FLUSH( STATUS )

               GOODAT( I ) = .FALSE.

*             increment number of the first good array if current value
*             is not to be included in the calculations

               IF ( I .EQ. FIRSTG ) FIRSTG = FIRSTG + 1
            ELSE

*             Store number of arrays read if an abort has been requested

               NUMREA = I - 1
               GOTO 998
            END IF
         ELSE

*          map in its DATA_ARRAY component

            CALL CMP_MAPN( LOCDI( I ), DNAMEI( I ), '_REAL', 'READ',
     :                     NDIMS, PNTRI( I ), IDIMS( 1, I ), STATUS )

*          Check for an error

            IF ( STATUS .NE. SAI__OK ) THEN

*             Tidy current frame

               CALL DAT_ANNUL( LOCDI( I ), STATUS )
               CALL DAT_ANNUL( LOCI( I ), STATUS )
               CALL PAR_CANCL( INPARM( I ), STATUS )

               IF ( STATUS .NE. PAR__ABORT ) THEN

*                Report error and set flag for bad data

                  CALL MSG_SETI( 'FRAMENO', I )
                  CALL ERR_REP( 'ERR_MOSAIC_NOMPI',
     :              'MOSAIC: Error occurred whilst trying to map '/
     :               /'input frame number ^FRAMENO.', STATUS )
                  CALL ERR_FLUSH( STATUS )

                  GOODAT( I ) = .FALSE.

*                increment number of the first good array if current
*                value is not to be included in the calculations

                  IF ( I .EQ. FIRSTG ) FIRSTG = FIRSTG + 1
               ELSE

*             Store number of arrays read if an abort has been requested

                  NUMREA = I - 1
                  GOTO 998
               END IF
            ELSE

*             for all but the first frame, get the offsets of the
*             current frame from the first, setting first offsets to
*             zero, and then cancel parameter

               IF ( I .NE. FIRSTG ) THEN
                  CALL PAR_GET0I( 'XOFFSET', XOFSET( I ), STATUS )
                  CALL PAR_GET0I( 'YOFFSET', YOFSET( I ), STATUS )

*                Check for an error

                  IF ( STATUS .NE. SAI__OK ) THEN

*                   Tidy current frame

                     CALL CMP_UNMAP( LOCDI( I ), DNAMEI( I ), STATUS )
                     CALL DAT_ANNUL( LOCDI( I ), STATUS )
                     CALL DAT_ANNUL( LOCI( I ), STATUS )
                     CALL PAR_CANCL( INPARM( I ), STATUS )

                     IF ( STATUS .NE. PAR__ABORT ) THEN

*                      Report error and set flag for bad data

                        CALL MSG_SETI( 'FRAMENO', I )
                        CALL ERR_REP( 'ERR_MOSAIC_OFFSET',
     :                    'MOSAIC: Error occurred whilst trying to '/
     :                    /'get offset for input frame number '/
     :                    /'^FRAMENO.', STATUS )
                        CALL ERR_FLUSH( STATUS )

                        GOODAT( I ) = .FALSE.
                     ELSE

*                      Store number of arrays read if an abort has been
*                      requested

                        NUMREA = I - 1
                        GOTO 998
                     END IF

                  ELSE

*                    the data array and offsets are all in order so set
*                    the good data flag for this frame

                      GOODAT( I ) = .TRUE.

*                end of error-getting-offsets check

                  END IF

*                cancel parameters for next frame

                  CALL PAR_CANCL( 'XOFFSET', STATUS )
                  CALL PAR_CANCL( 'YOFFSET', STATUS )

               ELSE

*               no offset for first frame

                  XOFSET( 1 ) = 0
                  YOFSET( 1 ) = 0

*                the data array and offsets are all in order so set
*                the good data flag for this frame

                  GOODAT( I ) = .TRUE.

*             end of first-frame check

               END IF

*          end of if-error-mapping-input-array check

            END IF

*       end of if-error-after-getting-input-structure check

         END IF

      END DO

      NUMREA = NUMBER

*    work out the size of the output frame to be created -
*    first sort out the maximum and minimum offsets.

*    initialise the maxima and minima values

      MINX = 0
      MINY = 0
      MAXX = 0
      MAXY = 0

*    loop round for each good input frame

       DO  J = 1, NUMBER

         IF ( GOODAT( J ) ) THEN

*          compare the current x and y offsets with minima found so far

            MINX = MIN ( XOFSET( J ), MINX )
            MINY = MIN ( YOFSET( J ), MINY )

*          just use MAX function for the maxima; maxima are found
*          from the offset plus frame size relative to first frame

            MAXX = MAX( MAXX, IDIMS( 1, J ) + XOFSET( J ) )
            MAXY = MAX( MAXY, IDIMS( 2, J ) + YOFSET( J ) )

*       end of valid-data-and-offsets check

         END IF

      END DO

*    calculate size of output frame from the extrema in the
*    offset values

      ODIMS( 1 ) = MAXX - MINX
      ODIMS( 2 ) = MAXY - MINY

*    inform user of output array dimensions

      CALL MSG_SETI( 'NEWXDIM', ODIMS( 1 ) )
      CALL MSG_SETI( 'NEWYDIM', ODIMS( 2 ) )
      CALL MSG_OUT( 'NEWDIMS',
     :    'Output array size is ^NEWXDIM by ^NEWYDIM', STATUS )

*    watch for zero dimensions, meaning there were no valid input
*    frames

      IF ( ODIMS( 1 ) .EQ. 0 .OR. ODIMS( 2 ) .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ERR_MOSAIC_OUTDIM',
     :     'MOSAIC: Output dimensions error. Check input frames',
     :     STATUS )

         GOTO 998
      END IF


*    redefine offsets to be relative to the origin in the output array
*    so that the offsets will not be negative

      DO  K = 1, NUMBER
         IF ( GOODAT( K ) ) THEN
            XOFSET( K ) = XOFSET( K ) - MINX
            YOFSET( K ) = YOFSET( K ) - MINY
         END IF
      END DO

*    Origin is undefined (in IMAGE context) for a series of images, so
*    use the default.

      DO  I = 1, NDIMS
         ORIGIN( I ) = 1
      END DO

*    now get the output array

      CALL KPG1_CROUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS, ORIGIN,
     :                 LOCO, LOCDO, DNAMEO, STATUS )

*    propagate UNITS, LABEL, HISTORY and extensions from the
*    primary input data file

      CALL KPG1_IMPRG( LOCI( 1 ), 'UNITS', LOCO, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN

*       map the output DATA_ARRAY component

         CALL CMP_MAPN( LOCDO, DNAMEO, '_REAL', 'WRITE', NDIMS,
     :                  PNTRO, ODIMS, STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN

*          create some temporary workspace to hold the array mask

            CALL AIF_GETVM( '_REAL', NDIMS, ODIMS, PNTRT, LOCT, STATUS )

            IF ( STATUS .EQ. SAI__OK ) THEN

*             set all the pixels of both the output array and the array
*             mask to be zero

               CALL KPG1_FILLR( 0.0, ODIMS( 1 ) * ODIMS( 2 ),
     :                          %VAL( PNTRO ), STATUS )
               CALL KPG1_FILLR( 0.0, ODIMS( 1 ) * ODIMS( 2 ),
     :                          %VAL( PNTRT ), STATUS )

               IF ( STATUS .EQ. SAI__OK ) THEN

*                add each array into the (big) output array, updating
*                the array mask at the same time, using MOSCAD

                  DO  L = 1, NUMBER
                     IF ( GOODAT( L ) ) THEN

                        CALL MOSCAD( %VAL( PNTRI( L ) ), IDIMS( 1, L ),
     :                               IDIMS( 2, L ),
     :                               XOFSET( L ), YOFSET( L ),
     :                               ODIMS( 1 ), ODIMS( 2 ),
     :                               %VAL( PNTRO ), %VAL( PNTRT ),
     :                               STATUS )
                     END IF
                  END DO

*                Now apply normalisation by dividing by the mask where
*                more than one input pixel has contributed to the
*                output pixel.

                  IF ( STATUS .EQ. SAI__OK .AND. AVERGE ) THEN
                     CALL MOSCDV( ODIMS( 1 ), ODIMS( 2 ), %VAL( PNTRT ),
     :                            %VAL( PNTRO ), STATUS )
                  END IF

*             end of no-error-zeroing-arrays check

               END IF

*             unmap the temporary workspace and annul its locator

               CALL AIF_ANTMP( LOCT, STATUS )

            ELSE

               CALL ERR_REP( 'ERR_MOSAIC_WSP',
     :           'MOSAIC: Unable to get workspace for array mask',
     :           STATUS )

*          end of creating-and-mapping-workspace check

            END IF

*          unmap the output data array

            CALL CMP_UNMAP( LOCDO, DNAMEO, STATUS )

         ELSE
            CALL ERR_REP( 'ERR_MOSAIC_NOMPO',
     :        'MOSAIC: Error occurred whilst trying to map output '/
     :        /'frame', STATUS )

*       end of if-no-error-after-mapping-output-data-array check

         END IF

*       annul the locators to the output structures

         CALL DAT_ANNUL( LOCDO, STATUS )
         CALL DAT_ANNUL( LOCO, STATUS )

      ELSE

         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'ERR_MOSAIC_NOFRO',
     :        'MOSAIC: Error occurred whilst trying to access output '/
     :        /'frame', STATUS )
         END IF

*    end of if-no-error-after-creating-output-structure check

      END IF

*    unmap all the input data arrays and annul the locators

 998  CONTINUE
      DO  M = 1, NUMREA
         CALL CMP_UNMAP( LOCDI( M ), DNAMEI( M ), STATUS )
         CALL DAT_ANNUL( LOCDI( M ), STATUS )
         CALL DAT_ANNUL( LOCI( M ), STATUS )
         CALL PAR_CANCL( INPARM( M ), STATUS )
      END DO

 999  CONTINUE

*    return and end

      END
