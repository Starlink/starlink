*+  ZAPLIN - replaces bad columns/rows by interpolation from either side

      SUBROUTINE ZAPLIN( STATUS )

*    Description :
*
*      This routine allows the user to remove bad columns or rows from
*    an image by specifying the rows or columns, at the moment from
*    the interface, but eventually from interactive placing of a cursor
*    over the image on a display screen.
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*           Image to be cleaned up
*     OUTPIC  =  IMAGE( WRITE )
*           Cleaned up version of the image
*     OTITLE  =  CHARACTER( READ )
*           Label for the output image
*     ROWCOL  =  CHARACTER( READ )
*           Whether or not it is rows or columns to be cleaned
*     START  =  INTEGER( READ )
*           First row/column to be cleaned
*     FINISH  =  INTEGER( READ )
*           Last row/column to be cleaned
*     NOISE  =  LOGICAL( READ )
*           Whether or not Poisson noise is to be added to zapped data
*     AGAIN  =  LOGICAL( READ )
*           Whether or not user is prompted for another row/column
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get input structure holding data to be processed
*     If no error then
*        Try to map the data array component from the input structure
*        If no error then
*           Output dimensions of input data array
*           Create an output structure to hold processed data
*           If no error then
*              Map a new data array component
*              Copy input data array into output data array
*              Ask whether Poisson noise is to be added to the interpolated data
*              Do while another zap wanted and no error
*                 Ask whether rows or columns to be zapped
*                 If rows then
*                    Get start and finish rows of band to be replaced
*                    If band encompasses whole image then
*                       Output error message
*                       Set valid zap flag false
*                    Else
*                       Set valid zap flag true
*                    Endif
*                 Else columns to be zapped
*                    Get start and finish columns of band to be replaced
*                    If band encompasses whole image then
*                       Output error message
*                       Set valid zap flag false
*                    Else
*                       Set valid zap flag true
*                    Endif
*                 Endif
*                 If valid zap flag true and no error then
*                    Call subroutine to zap data
*                 Endif
*                 Cancel parameters before looping
*              Enddo
*              Unmap and tidy output data structure
*           Endif
*        Endif
*        Unmap and tidy input data structure
*     Endif
*     End
*
*    Authors :
*
*     Mark McCaughrean UoE (REVA::MJM)
*
*    History :
*
*     01-07-1985 : First implementation, using row/column coordinates
*                : read from the user directly, rather than by use of
*                : an interactive cursor on a display screen.
*                : (REVA::MJM)
*     09-12-1985 : Added Poisson noise option and corrected
*                : error checking bug (UKTH::MARK)
*     11-12-1985 : Fixed bug caused by continuous updating of read
*                : only data - changed program order (UKTH::MARK)
*     06-07-1986 : Tidied and more error checking (REVA::MJM)
*     12-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     11-AUG-1994  Changed input DIMS to COPY2D and ZAPLINSUB (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE              ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'          ! global SSE parameters
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'

*    Status :

      INTEGER STATUS             ! global status variable

*    Local Constants :

      INTEGER NDIMS              ! image dimensionality
      PARAMETER ( NDIMS  =  2 )  ! 2-d images only

*    Local variables :

      INTEGER
     :    IDIMS( NDIMS ),        ! dimensions of input DATA_ARRAY
     :    NDIM,                  ! number dimensions from NDF_DIM
     :    NELEMENTS,             ! number of elements mapped
     :    PNTRI,                 ! pointer to input DATA_ARRAY component
     :    PNTRO,                 ! pointer to output DATA_ARRAY component
     :    START,                 ! first row/column to be cleaned
     :    FINISH,                ! last row/column to be cleaned
     :    LOCI,                  ! input data structure
     :    LOCO                   ! output data structure

      CHARACTER*1
     :    ROWCOL                 ! whether it is row or columns to be cleaned

      LOGICAL                    ! true if :
     :    AGAIN,                 ! user wants to do another line zap
     :    NOISE,                 ! Poisson noise to be added
     :    VALRC,                 ! input row/column is valid
     :    ROW                    ! rows to be removed - else columns

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    get a locator to input IMAGE type data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    check for error here
      IF ( STATUS .EQ. SAI__OK ) THEN

*       map the DATA_ARRAY component of the input data structure
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )

         CALL NDF_DIM( LOCI, NDIMS, IDIMS, NDIM, STATUS )

*       check for error here
         IF( STATUS .EQ. SAI__OK ) THEN

*          tell user dimensions of input array
            CALL MSG_SETI( 'XDIM', IDIMS(1) )
            CALL MSG_SETI( 'YDIM', IDIMS(2) )
            CALL MSG_OUT( 'INPUT_DIMS',
     :         'Image is ^XDIM by ^YDIM pixels', STATUS )

*          now create output image type data structure
            CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, IDIMS,
     :                    LOCO, STATUS )

*          check for error here
            IF ( STATUS .EQ. SAI__OK ) THEN

*             map output DATA_ARRAY component
               CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                        PNTRO, NELEMENTS, STATUS )

*             copy the input array into the output array
               CALL COPY2D( IDIMS(1), IDIMS(2), %VAL( PNTRI ),
     :                      %VAL( PNTRO ), STATUS )

*             find out whether or not Poisson noise is to be added
               CALL PAR_GET0L( 'NOISE', NOISE, STATUS )

*             initialise AGAIN and VALRC logicals
               AGAIN   =  .TRUE.
               VALRC   =  .TRUE.

*             start loop for deglitching
               DO WHILE ( AGAIN .AND. STATUS .EQ. SAI__OK )

*                find out whether rows or columns to be cleaned
                  CALL MSG_OUT( 'BLANK', ' ', STATUS )
                  CALL AIF_CHOIC( 'ROWCOL', 'R,C', ROWCOL, STATUS )

*                force to upper case
                  CALL UPCASE( ROWCOL, ROWCOL, STATUS )

*                set ROW logical accordingly
                  IF( ROWCOL .EQ. 'R' ) THEN
                     ROW  =  .TRUE.
                  ELSE
                     ROW  =  .FALSE.
                  END IF

*                now get the actual areas to be replaced
                  IF( ROW ) THEN

*                   get start and finish row numbers - default finish to
*                   be same as start i.e. only one row zapped
                     CALL AIF_GET0I( 'STARTROW', 1, 1, IDIMS( 2 ),
     :                          START, STATUS )
                     CALL AIF_GET0I( 'FINISHROW', START, 1, IDIMS( 2 ),
     :                          FINISH, STATUS )

*                   check if valid - the AIF call has made sure both START
*                   and FINISH are in the array, but we must check for the
*                   case where the whole array has been requested
                     IF( START .EQ. 1 .AND. FINISH .EQ. IDIMS(2) .OR.
     :                   START .EQ. IDIMS(2) .AND. FINISH .EQ. 1 ) THEN

*                      asking to zap whole image - report error
                        CALL MSG_SETI( 'START', START )
                        CALL MSG_SETI( 'FINISH', FINISH )

                        CALL MSG_OUT( 'ERR_WHOLE',
     :             'Bad rows - given values for Start (^START) '/
     :             /'and Finish (^FINISH) cover whole image - '/
     :             /'cannot zap entire image.', STATUS )

*                      set VALRC to be invalid
                        VALRC = .FALSE.

                     ELSE

*                      rows are valid
                        VALRC = .TRUE.

*                   end of if-input-rows-cover-whole-array check
                     END IF

                  ELSE

*                   must be columns - get start and finish numbers - default
*                   finish to be same as start i.e. only one column zapped
                     CALL AIF_GET0I( 'STARTCOL', 1, 1, IDIMS( 1 ),
     :                          START, STATUS )
                     CALL AIF_GET0I( 'FINISHCOL', START, 1, IDIMS( 1 ),
     :                          FINISH, STATUS )

*                   check if valid - the AIF call has made sure both START
*                   and FINISH are in the array, but we must check for the
*                   case where the whole array has been requested
*                   columns - check if valid
                     IF( START .EQ. 1 .AND. FINISH .EQ. IDIMS(1) .OR.
     :                   START .EQ. IDIMS(1) .AND. FINISH .EQ. 1 ) THEN

*                      asking to zap whole image - report error
                        CALL MSG_SETI( 'START', START )
                        CALL MSG_SETI( 'FINISH', FINISH )

                        CALL MSG_OUT( 'ERR_WHOLE',
     :             'Bad columns - given values for Start (^START) '/
     :             /'and Finish (^FINISH) cover whole image - '/
     :             /'cannot zap entire image.', STATUS )

*                      set VALRC to be invalid
                        VALRC = .FALSE.

                     ELSE

*                      columns are valid
                        VALRC = .TRUE.

*                   end of if-columns-cover-whole-array check
                     END IF

*                end of if-rows-selected check
                  END IF

*                if a valid row/column, then call ZAPLINSUB to do work
                  IF ( VALRC .AND. STATUS .EQ. SAI__OK ) THEN

                     CALL ZAPLINSUB( IDIMS(1), IDIMS(2), %VAL( PNTRO ),
     :                         START, FINISH, ROW, NOISE, STATUS )

                  ENDIF

*                ask user if he wants more rows/columns cleaned
                  CALL PAR_GET0L( 'AGAIN', AGAIN, STATUS )

*                cancel previous values of ROWCOL, START, FINISH
                  CALL PAR_CANCL( 'ROWCOL', STATUS )
                  CALL PAR_CANCL( 'AGAIN', STATUS )
                  IF ( ROW ) THEN
                     CALL PAR_CANCL( 'STARTROW', STATUS )
                     CALL PAR_CANCL( 'FINISHROW', STATUS )
                  ELSE
                     CALL PAR_CANCL( 'STARTCOL', STATUS )
                     CALL PAR_CANCL( 'FINISHCOL', STATUS )
                  END IF

*             end of while-no-error-and-another-zap-wanted loop
               END DO

*             tidy up the output data structure
               CALL NDF_ANNUL( LOCO, STATUS )

*          end of if-no-error-after-creating-output-structure check
            END IF

*       end of if-no-error-after-mapping-input-data-array check
         END IF

*       tidy up the input data structure
         CALL NDF_ANNUL( LOCI, STATUS )

*    end of if-no-error-after-getting-input-structure check
      END IF


*    end
      END
