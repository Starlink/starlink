      SUBROUTINE QUILT ( STATUS )
*+
*  Name:
*     QUILT

*  Purpose:
*     Generates a mosaic from equally sized 2-d data arrays, optionally
*     specified from an ASCII file

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation
*     CALL QUILT( STATUS )

*  Description:
*     This routine provides a more-sophisticated version of the MOSAIC
*     application for combining many 2-d data arrays into one large
*     output data array. All the data arrays are stored in IMAGE
*     structures.  The pixels in overlapping regions may be averaged or
*     summed.
*
*     The names of IMAGE structures to be concatenated and their
*     respective offsets of their data arrays from a central
*     data array can be input either one by one from the interface, or
*     all at once from a free-format file.  The format of the file is
*     as follows:
*
*
*        Mosaic title                        ! header
*        central_image                       ! name of central IMAGE
*        125                                 ! total no. frames
*        345  229                            ! maximum x-y offsets
*        -356  -232                          ! minimum x-y offsets
*        image_2                             ! subsequent IMAGE and
*        35  34                              ! its x-y offsets
*        image_3
*        36  -33
*        .
*        .
*        .
*        .
*
*     Only like-sized data arrays may be input. The reason for this is
*     that it is difficult to work out how big the output data array
*     needs to be until all the input data arrays and their offsets have
*     been read in. By confining the data arrays to be the same size,
*     only the maximum and minimum x and y offsets from the central data
*     array need be input by the user, then the output image size can be
*     worked out from these numbers along with the size of the central
*     data array.
*
*     Bad pixels are processed by the magic-value method.

*  ADAM Parameters:
*     WHERE = LITERAL (READ)
*         Whether input comes from an ASCII 'File' or from the
*         'Interface'.
*     FNAME = LITERAL (READ)
*         Name of the ASCII file holding the input information to define
*         the mosaic.
*     NUMBER = _INTEGER (READ)
*         Number of data arrays to form the mosaic.
*     INPICI = IMAGE (READ)
*         IMAGE structure containing the central data array (offset
*         0,0).
*     MAXX = _INTEGER (READ)
*         Maximum x offset of any data array from the central data array
*         (must be >= 0) (Interface mode).
*     MAXY = _INTEGER (READ)
*         Maximum y offset of any data array from the central data array
*         (must be >= 0) (Interface mode).
*     MINX = _INTEGER (READ)
*         Minimum x offset of any data array from the central data array
*         (must be =< 0) (Interface mode).
*     MINY = _INTEGER (READ)
*         Minimum y offset of any data array from the central data array
*         (must be =< 0) (Interface mode).
*     AVERAGE = _LOGICAL (READ)
*         If true overlap regions are averaged, alternatively, they are
*         summed.
*     OUTPIC = IMAGE (WRITE)
*         Output IMAGE structure.
*     OTITLE = LITERAL (READ)
*         Title string for output IMAGE structure.
*     CURPIC = IMAGE (READ)
*         IMAGE containing the current data array being concatenated to
*         the mosaic.
*     OFFSETX = _INTEGER (READ)
*         x offset of current data array from the central one (Interface
*         mode)
*     OFFSETY = _INTEGER (READ)
*         y offset of current data array from the central one (Interface
*         mode)

*  Deficiencies :
*     Works with like-sized images only and uses Fortran i/o for getting
*     stuff from a file.

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
*     29-12-1986 : First implementation (from MOSAIC) (REVA::MJM)
*     1988 May 29: KAPPA version, using magic-value bad pixels only
*                  ( RAL::CUR ).
*     1989 Jun 13: Allow for processing primitive NDFs (RL.STAR::CUR)
*     1989 Aug  7: Passed array dimensions as separate variables
*                  to MOSCAD, MOSCDV and ZERO2D (RL.STAR::CUR).
*     1989 Dec 21: Workspace managed by AIF_TEMP (RL.STAR::CUR).
*     1991 Oct 25: Propagates UNITS, LABEL and HISTORY (RAL::CUR).
*     1992 Feb 26: Limited processing of simple NDFs (RAL::CUR).
*     1992 Mar  3: Replaced AIF parameter-system calls by the extended
*                  PAR library (RAL::CUR).
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*    Type definitions :

      IMPLICIT  NONE           ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PAR_ERR'        ! parameter-system errors

*    Status :

      INTEGER  STATUS          ! global status parameter

*    Local Constants :

      INTEGER
     :  NDIMS                  ! dimensionality of input data arrays
      PARAMETER( NDIMS = 2 ) ! 2-d arrays only

*    Local variables :

      INTEGER
     :  FD,                    ! file description
     :  NUMBER,                ! number of input images to form mosaic
     :  MAXI( 2 ),             ! maximum x-y offsets
     :  MINI( 2 ),             ! minimum  "     "
     :  IDIMS( NDIMS ),        ! dimensions of central data array
     :  ODIMS( NDIMS ),        !      "      " output     "
     :  CDIMS( NDIMS ),        !      "      " current input data array
     :  ORIGIN( DAT__MXDIM ),  ! Origin of the data array
     :  PNTRI,                 ! pointer to central data array
     :  PNTRO,                 !    "     " output      "
     :  PNTRW,                 !    "     " workspace for addition mask
     :  PNTRC,                 !    "     " current input data array
     :  OFFSET( 2 ),           ! x-y offsets from central to current
     :  I                      ! counter

      CHARACTER*(DAT__SZLOC)   ! locators to :
     :  LOCDC,                 ! structure containing the current input
                               ! data array
     :  LOCDI,                 ! structure containing the input central
                               ! data array
     :  LOCDO,                 ! structure containing the output data
                               ! array
     :  LOCI,                  ! central IMAGE structure
     :  LOCO,                  ! output    "       "
     :  LOCW,                  ! workspace         "
     :  LOCC                   ! current   "       "

      CHARACTER * ( DAT__SZNAM )
     :  DNAMEC,                ! Name of the current input data-array
                               ! component
     :  DNAMEI,                ! Name of the input central data-array
                               ! component
     :  DNAMEO                 ! Name of the output data-array component

      CHARACTER*9
     :  WHERE                  ! source of input - File or Interface

      LOGICAL                  ! true if :
     :	AVERGE                 ! overlap regions are averaged

*-
*    Check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Ask whether input information is to come from a File or from the
*    Interface

      CALL PAR_CHOIC( 'WHERE', 'File', 'File,Interface', .FALSE.,
     :                WHERE, STATUS )

*    Check to see what was chosen

      IF ( WHERE .EQ. 'FILE' ) THEN

*       Get the file name, open the file and get the initial information
*       from it, namely the number of data arrays to be input, the
*       maximum and minimum x-y offsets, and a locator to the central
*       data array

         CALL MFOPEN( 'FNAME', 'INPICI', FD, LOCI, NUMBER, MAXI,
     :                MINI, STATUS )

         CALL KPG1_GETIM( ' ', LOCI, LOCDI, DNAMEI, ORIGIN, STATUS )

*       Check status - will be set not ok if it was not possible
*       to find the central data array or some other error opening or
*       reading the file

         IF ( STATUS .NE. SAI__OK .AND. STATUS .NE. PAR__ABORT ) THEN

*          Output message to this effect

            CALL MSG_OUT( 'BLANK', ' ', STATUS )
            CALL ERR_REP( 'ERR_QUILT_GTFILE',
     :        'QUILT: Some problem with the ASCII file - aborting',
     :        STATUS )

*       End of if-some-problem-with-File check

         END IF

*    Else input from the interface

      ELSE

*       Get the number of data arrays to be input

         CALL PAR_GDR0I( 'NUMBER', 2, 2, 250, .FALSE., NUMBER, STATUS )

*       Get the max and min x-y offsets from the central data array

         CALL PAR_GDR0I( 'MAXX', 0, 0, 10000, .FALSE., MAXI( 1 ),
     :                   STATUS )
         CALL PAR_GDR0I( 'MAXY', 0, 0, 10000, .FALSE., MAXI( 2 ),
     :                   STATUS )
         CALL PAR_GDR0I( 'MINX', 0, -10000, 0, .FALSE., MINI( 1 ),
     :                   STATUS )
         CALL PAR_GDR0I( 'MINY', 0, -10000, 0, .FALSE., MINI( 2 ),
     :                   STATUS )

*       Report error context

         IF ( STATUS .NE. SAI__OK ) THEN
            IF ( STATUS .NE. PAR__ABORT ) THEN
                CALL ERR_REP( 'ERR_QUILT_PAR',
     :            'QUILT: Error obtaining parameters', STATUS )
            END IF
         ELSE

*          Get a locator to the central data array

            CALL KPG1_GETIM( 'INPICI', LOCI, LOCDI, DNAMEI, ORIGIN,
     :                       STATUS )

            IF ( STATUS .NE. SAI__OK ) THEN
               IF ( STATUS .NE. PAR__ABORT ) THEN
                   CALL ERR_REP( 'ERR_QUILT_NOFRIC',
     :               'QUILT: Error accessing central data array',
     :               STATUS )
               END IF
            END IF

*       End of no-error-getting-initial-parameters check

         END IF

*    End of if-input-from-File check

      END IF

*    Check status before continuing

      IF ( STATUS .EQ. SAI__OK ) THEN

*       Map the central data array in

         CALL CMP_MAPN( LOCDI, DNAMEI, '_REAL', 'READ', NDIMS,
     :                  PNTRI, IDIMS, STATUS )

*       Check status before continuing

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'ERR_QUILT_NOMPIC',
     :        'QUILT: Error occurred whilst trying to map central '/
     :        /'data array', STATUS )
            CALL ERR_FLUSH( STATUS )

         ELSE

*          Work out size of the output data array and workspace array

            ODIMS( 1 ) = ( MAXI( 1 ) + IDIMS( 1 ) ) - MINI( 1 )
            ODIMS( 2 ) = ( MAXI( 2 ) + IDIMS( 2 ) ) - MINI( 2 )

*          Ask user if want to average the overlap region or not

            CALL PAR_GTD0L( 'AVERAGE', .TRUE., .TRUE., AVERGE, STATUS )

*          Check status before continuing

            IF ( STATUS .NE. SAI__OK ) THEN
               IF ( STATUS .NE. PAR__ABORT ) THEN
                  CALL ERR_REP( 'ERR_QUILT_PAR2',
     :              'QUILT: Error obtaining how to process '/
     :              /'overlapping data arrays', STATUS )
               END IF
            ELSE

*             Output a progress message

               CALL MSG_OUT( 'BLANK', ' ', STATUS )
               CALL MSG_OUT( 'PROG1',
     :           ' Now creating and zeroing output data array '/
     :           /'and workspace ...', STATUS )
               CALL MSG_OUT( 'BLANK', ' ', STATUS )

*             Origin is undefined (IMAGE-type context) for a series of
*             images, so use the default.

               DO  I = 1, NDIMS
                  ORIGIN( I ) = 1
               END DO

*             Create output data array and workspace

               CALL KPG1_CROUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS,
     :                          ORIGIN, LOCO, LOCDO, DNAMEO, STATUS )

*             Propagate UNITS, LABEL, HISTORY and extensions from the
*             input data file.

               CALL KPG1_IMPRG( LOCI, 'UNITS', LOCO, STATUS )

*             Check status before continuing

               IF ( STATUS .NE. SAI__OK ) THEN
                  IF ( STATUS .NE. PAR__ABORT ) THEN
                     CALL ERR_REP( 'ERR_QUILT_NOFRO',
     :                 'QUILT: Error occurred whilst trying to access '/
     :                 /'output data array', STATUS )
                  END IF

               ELSE

*                Map output data array

                  CALL CMP_MAPN( LOCDO, DNAMEO, '_REAL', 'WRITE',
     :                           NDIMS, PNTRO, ODIMS, STATUS )

                  IF ( STATUS .NE. SAI__OK ) THEN

                     CALL ERR_REP( 'ERR_QUILT_NOMPO',
     :                 'QUILT: Error occurred whilst trying to map '/
     :                 /'output data array', STATUS )
                  ELSE

*                   Get and map workspace

                     CALL AIF_GETVM( '_REAL', NDIMS, ODIMS, PNTRW, LOCW,
     :                              STATUS )

*                   Check status before continuing

                     IF ( STATUS .NE. SAI__OK ) THEN

                        CALL ERR_REP( 'ERR_QUILT_WSP',
     :                    'QUILT: Unable to get workspace for array '/
     :                    /'mask', STATUS )

                     ELSE

*                      Zero the output image and workspace

                        CALL KPG1_FILLR( 0.0, ODIMS( 1 ) * ODIMS( 2 ),
     :                                   %VAL( PNTRO ), STATUS )
                        CALL KPG1_FILLR( 0.0, ODIMS( 1 ) * ODIMS( 2 ),
     :                                   %VAL( PNTRW ), STATUS )

*                      Redefine offsets of central data array to be with
*                      respect to the lower left corner of the output
*                      array

                        OFFSET( 1 ) = -MINI( 1 )
                        OFFSET( 2 ) = -MINI( 2 )

*                      Insert central data array into output

                        CALL MOSCAD( %VAL( PNTRI ), IDIMS( 1 ),
     :                               IDIMS( 2 ), OFFSET( 1 ),
     :                               OFFSET( 2 ), ODIMS( 1 ),
     :                               ODIMS( 2 ), %VAL( PNTRO ),
     :                               %VAL( PNTRW ), STATUS )

*                      Output message indicating success at central
*                      data array

                        CALL MSG_OUT( 'CEN_DONE',
     :                    ' Central data array was processed correctly',
     :                    STATUS )

*                      Initialise counter at 1 - we have already done
*                      first data array

                        I = 1

*                      Now loop round all requested input data arrays

                        DO WHILE ( I .LT. NUMBER .AND.
     :                             STATUS .EQ. SAI__OK )

*                         Increment counter

                           I = I + 1

*                         Check if input from File

                           IF ( WHERE .EQ. 'FILE' ) THEN

*                            Get locator and offsets to the next data
*                            array

                              CALL MFNEXT( FD, 'CURPIC', LOCC,
     :                                     OFFSET, STATUS )

*                            Check that there is a data array, and
*                            obtain its name and a locator to the
*                            structure containing it.
                              CALL KPG1_GETIM( ' ', LOCC, LOCDC, DNAMEC,
     :                                         ORIGIN, STATUS )

*                            Check status on return

                              IF ( STATUS .NE. SAI__OK .AND.
     :                             STATUS .NE. PAR__ABORT ) THEN

*                               Report error context

                                 CALL MSG_SETI( 'I', I )
                                 CALL ERR_REP( 'ERR_QUILT_MFN',
     :                             'QUILT: Some error in reading data '/
     :                             /'array ^I from file - aborting',
     :                             STATUS )

*                            Check offsets returned

                              ELSE IF ( OFFSET( 1 ) .GT. MAXI( 1 ) .OR.
     :                                  OFFSET( 1 ) .LT. MINI( 1 ) .OR.
     :                                  OFFSET( 2 ) .GT. MAXI( 2 ) .OR.
     :                                  OFFSET( 2 ) .LT. MINI( 2 ) )THEN

*                               Set up error message and set bad status

                                 STATUS = SAI__ERROR
                                 CALL MSG_SETI( 'I', I )
                                 CALL ERR_REP( 'ERR_QUILT_MFNOFF',
     :                             'QUILT: Offsets obtained for data '/
     :                             /'array ^I were outside range '/
     :                             /'- aborting', STATUS )

*                            End of if-error-on-return-from-MFNEXT check

                              END IF

*                         Else input is from the Interface

                           ELSE

*                            Get locator and offsets to next data array

                              CALL MSG_SETI( 'I', I )
                              CALL MSG_OUT( 'CUR_IN',
     :                          ' Next data array will be number ^I :',
     :                          STATUS )

                              CALL KPG1_GETIM( 'CURPIC', LOCC, LOCDC,
     :                                         DNAMEC, ORIGIN, STATUS )

*                            Check for an error

                              IF ( STATUS .NE. SAI__OK ) THEN
                                 IF ( STATUS .NE. PAR__ABORT ) THEN

*                                  Report error context

                                    CALL MSG_SETI( 'I', I )
                                    CALL ERR_REP( 'ERR_QUILT_NOFRI',
     :                                'QUILT: Error occurred whilst '/
     :                                /'trying to access input data '/
     :                                /'array number ^I.', STATUS )
                                  END IF
                              ELSE

                                 CALL PAR_GDR0I( 'OFFSETX', 0,
     :                                           MINI( 1 ), MAXI( 1 ),
     :                                           .FALSE.,
     :                                           OFFSET( 1 ), STATUS )
                                 CALL PAR_GDR0I( 'OFFSETY', 0,
     :                                           MINI( 2 ), MAXI( 2 ),
     :                                           .FALSE.,
     :                                           OFFSET( 2 ), STATUS )

*                               Check status before continuing

                                 IF ( STATUS .NE. SAI__OK ) THEN
                                    IF ( STATUS .NE. PAR__ABORT ) THEN
                                       CALL ERR_REP( 'ERR_QUILT_PAR3',
     :                                   'QUILT: Error obtaining '/
     :                                   /'offsets for data array ^I',
     :                                   STATUS )
                                    END IF
                                 END IF

*                               Cancel parameters ready for next go

                                 CALL PAR_CANCL( 'OFFSETX', STATUS )
                                 CALL PAR_CANCL( 'OFFSETY', STATUS )

*                            End of current-data-array-accessed check

                              END IF

*                         End of if-input-is-from-File check

                           END IF

*                         Check status before continuing

                           IF ( STATUS .EQ. SAI__OK ) THEN

*                            Map the current data array

                              CALL CMP_MAPN( LOCDC, DNAMEC,
     :                                       '_REAL', 'READ', NDIMS,
     :                                       PNTRC, CDIMS, STATUS )

*                            Check status before continuing

                              IF ( STATUS .NE. SAI__OK ) THEN
                                 CALL ERR_REP( 'ERR_QUILT_NOMPI',
     :                             'QUILT: Error occurred whilst '/
     :                             /'trying to map the current '/
     :                             /'data array', STATUS )
                              ELSE

*                               Check that the current data array is the
*                               same size as the central data array

                                 IF ( CDIMS( 1 ) .EQ. IDIMS( 1 ) .AND.
     :                                CDIMS( 2 ) .EQ. IDIMS( 2 ) ) THEN

*                                  Redefine the offsets to be with
*                                  respect to the lower left corner of
*                                  the output data array

                                    OFFSET( 1 ) = OFFSET( 1 ) -
     :                                              MINI( 1 )
                                    OFFSET( 2 ) = OFFSET( 2 ) -
     :                                              MINI( 2 )

*                                  Insert the current data array into
*                                  the output data array accordingly

                                    CALL MOSCAD( %VAL( PNTRC ),
     :                                           CDIMS( 1 ), CDIMS( 2 ),
     :                                           OFFSET( 1 ),
     :                                           OFFSET( 2 ),
     :                                           ODIMS( 1 ), ODIMS( 2 ),
     :                                           %VAL( PNTRO ),
     :                                           %VAL( PNTRW ), STATUS )

*                                  Output message to the effect that the
*                                  current data array was processed ok

                                    CALL MSG_SETI( 'I', I )
                                    CALL MSG_OUT( 'CUR_OK',
     :                                ' Data array number ^I was '/
     :                                /'processed correctly', STATUS )

*                               Else current data array is not same size
*                               as central data array

                                 ELSE

*                                  Output message to that effect

                                    CALL MSG_OUT( 'BLANK', ' ', STATUS )
                                    CALL MSG_SETI( 'I', I )
                                    CALL MSG_OUT( 'CUR_ERR1',
     :                                ' Data array number ^I has '/
     :                                /'different size from the '/
     :                                /'central one :', STATUS )

                                    CALL MSG_SETI( 'IXDIM', IDIMS( 1 ) )
                                    CALL MSG_SETI( 'IYDIM', IDIMS( 2 ) )
                                    CALL MSG_OUT( 'CUR_ERR2',
     :                               ' Central data array is ^IXDIM '/
     :                               /'by ^IYDIM pixels,', STATUS )

                                    CALL MSG_SETI( 'CXDIM', CDIMS( 1 ) )
                                    CALL MSG_SETI( 'CYDIM', CDIMS( 2 ) )
                                    CALL MSG_OUT( 'CUR_ERR3',
     :                                ' Current data array is ^CXDIM '/
     :                                /'by ^CYDIM pixels - leaving it '/
     :                                /'out', STATUS )

*                               End of if-dimensions-match check

                                 END IF

*                            End of if-no-error-after-mapping-current
*                            check

                              END IF

*                            Tidy current IMAGE structure

                              CALL CMP_UNMAP( LOCDC, DNAMEC,
     :                                        STATUS )

*                         End of if-no-error-before-mapping-current
*                         check

                           END IF

*                         Tidy up the current addition to the mosaic.

                           CALL DAT_ANNUL( LOCDC, STATUS )
                           CALL DAT_ANNUL( LOCC, STATUS )
                           IF ( WHERE .NE. 'FILE' )
     :                       CALL PAR_CANCL( 'CURPIC', STATUS )

*                      End of loop round all requested input data arrays

                        END DO

                        IF ( STATUS .EQ. SAI__OK .AND. AVERGE ) THEN

*                         Output message to let user know what is going
*                         on

                           CALL MSG_OUT( 'BLANK', ' ', STATUS )
                           CALL MSG_OUT( 'PROG2',
     :                       ' Now normalising output data array',
     :                       STATUS )
                           CALL MSG_OUT( 'BLANK', ' ', STATUS )

*                         Now normalise output array by the mask where
*                         more than one input pixel has contributed to
*                         the output pixel.

                           CALL MOSCDV( ODIMS( 1 ), ODIMS( 2 ),
     :                                  %VAL( PNTRW ), %VAL( PNTRO ),
     :                                  STATUS )
                        END IF

*                      Tidy up workspace

                        CALL DAT_ANNUL( LOCW, STATUS )

*                   End of error-creating-and-mapping-workspace check

                     END IF

*                   Unmap output data array

                     CALL CMP_UNMAP( LOCDO, DNAMEO, STATUS )

*                End of error-mapping-output-data-array check

                  END IF

*             End of if-no-error-after-creating-output-data-array check

               END IF

*             Tidy up output structures

               CALL DAT_ANNUL( LOCDO, STATUS )
               CALL DAT_ANNUL( LOCO, STATUS )

*          End of if-no-error-before-creating-output-and-workspace check

            END IF

            CALL CMP_UNMAP( LOCDI, DNAMEI, STATUS )

*       End of if-no-error-after-mapping-central-data array check

         END IF

*       Tidy up the central data array structures

         CALL DAT_ANNUL( LOCDI, STATUS )
         CALL DAT_ANNUL( LOCI, STATUS )

*    End of if-no-error-after-getting-central-data array check

      END IF

*    Check to see if the input was from a File

      IF ( WHERE .EQ. 'FILE' ) THEN

*       Close the File

         CALL FIO_CLOSE( FD, STATUS )

*    End of if-input-was-from-a-File check

      END IF

*    Return and end

      END

