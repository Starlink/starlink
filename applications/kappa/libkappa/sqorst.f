*+  SQORST - Squashes or stretches a 2-d data array in either or both
*            axes
*
      SUBROUTINE SQORST( STATUS )
*
*    Description :
*
*     The output data array, written to an IMAGE structure, is produced
*     by either squashing or stretching the 2-d data array, in the input
*     IMAGE structure, in either or both of the x and y axes. The
*     dimensions of the output data array, are given by the user. The
*     stretching is performed by keeping the edge pixels fixed and
*     calculating the intervening pixels by bi-linear interpolation. The
*     squashing is performed by calculating each pixel in the output
*     array as the mean of the corresponding pixels in the input array.
*
*     The magic-value method is used for processing bad data.
*
*    Invocation :
*
*     CALL SQORST( STATUS )
*
*    Parameters :
*
*     INPIC  = IMAGE( READ )
*         IMAGE structure containing the 2-d data array to be squashed
*           or stretched.
*     XDIM   = INTEGER( READ )
*         First dimension for the output 2-d data array.
*     YDIM   = INTEGER( READ )
*         Second dimension for the output 2-d data array.
*     OUTPIC = IMAGE( WRITE )
*         IMAGE structure to contain the 2-d data array after being
*           squashed or stretched.
*     OTITLE = CHAR( READ )
*         Will form the TITLE component for the output IMAGE structure.
*
*    Arguments:
*
*     STATUS  = INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get input data structure from environment
*     If no error then
*        Map DATA_ARRAY component onto pointer
*        If no error then
*           Output input array dimensions to user
*           Get the dimensions of the output array checking that they
*             are both greater than zero
*           If no error so far then
*              Create output IMAGE type data structure
*              Propagate NDF MORE from the input data file
*              If no error then
*                 Map output data array
*                 If no error then
*                    Set up the workspace arrays
*                       If 1st dimension of output array > 1st dimension
*                         of input array then
*                          Stretch the input array and store the result
*                            in workspace
*                       Else
*                          Squash the input array and store the result
*                            in workspace
*                       Endif
*                       If 2nd dimension of output array > 2nd dimension
*                         of input array then
*                          Stretch the workspace and store result in
*                            output array
*                       Else
*                          Squash the workspace and store result in
*                            output array
*                       Endif
*                       Tidy last workspace
*                    Else
*                       Report error context
*                    Endif
*                    Unmap output data array and tidy remaining work
*                      space
*                 Endif
*                 Annul output structure
*              Else
*                 Report error context
*              Endif
*           Else
*              Report error context
*           Endif
*           Unmap input data array
*        Else
*           Report error context
*        Endif
*        Annul input structure
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
*     Dave Baines (ROE::ASOC5)
*     Malcolm  J. Currie  STARLINK (RAL::CUR)
*
*    History :
*
*     18/08/1983 : Original version                (ROE::ASOC5)
*     19/02/1984 : Modified to use TITLE component (ROE::ASOC5)
*     1986 Aug 8 : Standardised prologue formatting. Added status check
*                  on entry (RAL::CUR).
*     1986 Sep 1 : Added arguments section to prologue, renamed APPG0I
*                  routine to AIF_GET0I and tidied (RAL::CUR).
*     1987 Oct 16: Reordered tidying and extra status checks
*                  (RAL::CUR)
*     1988 Feb 19: Extra error report (RAL::CUR).
*     1988 Mar 16: Substituted AIF_ANTMP to annul workspace (RAL::CUR).
*     1988 Mar 17: Referred to `array' rather than `image' (RAL::CUR).
*     1988 Jun 20: More reporting of error context (RAL::CUR).
*     1989 Jun 13: Allow for processing primitive NDFs (RAL::CUR).
*     1989 Aug  7: Passed array dimensions as separate variables
*                  to STRX, STRY, SQSHX and SQSHY (RAL::CUR).
*     1989 Dec 21: Workspace managed by AIF_TEMP (RAL::CUR).
*     1990 Jun 25: Fixed bug that occurred when an output dimension is
*                  the same as the input (RAL::CUR).
*     1991 Oct 25: Propagates UNITS, LABEL, and HISTORY (RAL::CUR).
*     1992 Feb 25: Limited processing of simple NDFs (RAL::CUR).
*     1992 Mar  3: Replaced AIF parameter-system calls by the extended
*                  PAR library (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT NONE            ! no implicit typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'        ! global SSE definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PAR_ERR'        ! parameter-system errors

*    Status :

      INTEGER STATUS

*    Local constants :

      INTEGER NDIM             ! dimensionality of input/output arrays
      PARAMETER ( NDIM = 2 )
      INTEGER MAXDIM           ! maximum dimensions for output array
      PARAMETER ( MAXDIM = 10000000 ) 

*    Local variables :

      CHARACTER*(DAT__SZLOC)   ! locators for :
     :  LOCDI,                 ! structure containing the input data
                               ! array
     :  LOCDO,                 ! structure containing the output data
                               ! array
     :  LOCI,                  ! input data structure
     :  LOCO,                  ! output data structure
     :  WLOC1,                 ! workspace to hold intermediate array
     :  WLOC2,                 !      "    for weights, 1st dimension
     :  WLOC3,                 !      "     "     "   , 2nd     "
     :  WLOC4,                 !      "     "    " pixel limits, 1st
                               !                               dimension
     :  WLOC5                  !      "     "    "   "      "  , 2nd
                               !                               dimension

      CHARACTER * ( DAT__SZNAM )
     :  DNAMEI,                ! Name of the input data-array component
     :  DNAMEO                 ! Name of the output data-array component

      INTEGER
     :  I,                     ! loop counter
     :  ORIGIN( DAT__MXDIM ),  ! Origin of the data array
     :  IDIMS( NDIM ),         ! dimensions of input DATA_ARRAY
     :  ODIMS( NDIM ),         !      "      " output DATA_ARRAY
     :  WDIMS1( NDIM ),        !      "      " intermediate stage array
     :  WDIMS2( NDIM ),        !      "      " pixel limits and weight
*                              ! arrays, 1st dimension
     :  WDIMS3( NDIM ),        ! dimensions of pixel limits and weight
*                              ! arrays, 2nd dimension
                               ! pointer to:
     :  PNTRI,                 ! input DATA_ARRAY
     :  PNTRO,                 ! output DATA_ARRAY
     :  WPNTR1,                ! workspace array, intermediate array
     :  WPNTR2,                !     "       "  , 1st dimension weights
     :  WPNTR3,                !     "       "  , 2nd     "        "
     :  WPNTR4,                !     "       "  , 1st dim. pixel limits
     :  WPNTR5                 !     "       "  , 2nd  "     "      "

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    get locator to input IMAGE type data structure

      CALL KPG1_GETIM( 'INPIC', LOCI, LOCDI, DNAMEI, ORIGIN, STATUS )

*    check for error

      IF ( STATUS .EQ. SAI__OK ) THEN

*       map input DATA_ARRAY component

         CALL CMP_MAPN( LOCDI, DNAMEI, '_REAL', 'READ', NDIM,
     :                  PNTRI, IDIMS, STATUS )

*       check for error

         IF ( STATUS .EQ. SAI__OK ) THEN

*          tell user what the dimensions of the input array are

            CALL MSG_SETI( 'IXDIM', IDIMS( 1 ) )
            CALL MSG_SETI( 'IYDIM', IDIMS( 2 ) )
            CALL MSG_OUT( 'IN_DIMS', 'First dimension = ^IXDIM, Second'/
     :                    /' dimension = ^IYDIM', STATUS )

*          get first dimension for output array

            CALL PAR_GDR0I( 'XDIM', IDIMS(1), 1, MAXDIM, .FALSE.,
     :                      ODIMS(1), STATUS )

*          get second dimension for output array

            CALL PAR_GDR0I( 'YDIM', IDIMS(2), 1, MAXDIM, .FALSE.,
     :                      ODIMS(2), STATUS )

*          check for error

            IF ( STATUS .EQ. SAI__OK ) THEN

*             reset the origin since it has become undefined

               DO I = 1, NDIM
                  ORIGIN( I ) = 1
               END DO

*             create output IMAGE type data structure containing a
*             DATA_ARRAY component of dimensions ODIMS, and create and
*             get a value for a TITLE component

               CALL KPG1_CROUT( 'OUTPIC', 'OTITLE', NDIM, ODIMS,
     :                          ORIGIN, LOCO, LOCDO, DNAMEO, STATUS )

*             propagate UNITS, LABEL, HISTORY and extensions from the
*             input data file

               CALL KPG1_IMPRG( LOCI, 'UNITS', LOCO, STATUS )

*             check for error

               IF ( STATUS .EQ. SAI__OK ) THEN

*                map output DATA_ARRAY component

                  CALL CMP_MAPN( LOCDO, DNAMEO, '_REAL', 'WRITE',
     :                           NDIM, PNTRO, ODIMS, STATUS )

*                check for error

                  IF ( STATUS .EQ. SAI__OK ) THEN

*                   set up dimensions of the workspace arrays

                     WDIMS1( 1 ) = ODIMS( 1 )
                     WDIMS1( 2 ) = IDIMS( 2 )
                     WDIMS2( 1 ) = ODIMS( 1 )
                     WDIMS2( 2 ) = 2
                     WDIMS3( 1 ) = ODIMS( 2 )
                     WDIMS3( 2 ) = 2

*                   create and map the workspace arrays

                     CALL AIF_GETVM( '_REAL', NDIM, WDIMS1, WPNTR1,
     :                               WLOC1, STATUS )
                     CALL AIF_GETVM( '_REAL', NDIM, WDIMS2, WPNTR2,
     :                               WLOC2, STATUS )
                     CALL AIF_GETVM( '_REAL', NDIM, WDIMS3, WPNTR3,
     :                               WLOC3, STATUS )
                     CALL AIF_GETVM( '_INTEGER', NDIM, WDIMS2, WPNTR4,
     :                               WLOC4, STATUS )
                     CALL AIF_GETVM( '_INTEGER', NDIM, WDIMS3, WPNTR5,
     :                               WLOC5, STATUS )

*                   check for error

                     IF ( STATUS .EQ. SAI__OK ) THEN

                        IF ( ODIMS( 1 ) .GT. IDIMS( 1 ) ) THEN

*                         stretch input array in X direction storing
*                         the result in workspace

                           CALL STRX( IDIMS( 1 ), IDIMS( 2 ),
     :                                %VAL( PNTRI ), WDIMS1( 1 ),
     :                                WDIMS1( 2 ), %VAL( WPNTR1 ),
     :                                %VAL( WPNTR4 ), %VAL( WPNTR2 ),
     :                                STATUS )

                        ELSE IF ( ODIMS( 1 ) .LT. IDIMS( 1 ) ) THEN

*                         squash input array in X direction storing the
*                         result in workspace

                           CALL SQSHX( IDIMS( 1 ), IDIMS( 2 ),
     :                                 %VAL( PNTRI ), WDIMS1( 1 ),
     :                                 WDIMS1( 2 ), %VAL( WPNTR1 ),
     :                                 %VAL( WPNTR4 ), %VAL( WPNTR2 ),
     :                                 STATUS )
                        ELSE

*                         Just copy the data as no reconfiguration
*                         is required in the x direction.

                           CALL COPY2D( IDIMS( 1 ), IDIMS( 2 ),
     :                                  %VAL( PNTRI ), %VAL( WPNTR1 ),
     :                                  STATUS )

                        END IF

                        IF ( ODIMS( 2 ) .GT. IDIMS( 2 ) ) THEN

*                         stretch workspace in Y direction storing the
*                         result in output array

                           CALL STRY( WDIMS1( 1 ), WDIMS1( 2 ),
     :                                %VAL( WPNTR1 ), ODIMS( 1 ),
     :                                ODIMS( 2 ), %VAL( PNTRO ),
     :                                %VAL( WPNTR5 ), %VAL( WPNTR3 ),
     :                                STATUS )

                        ELSE IF ( ODIMS( 2 ) .LT. IDIMS( 2 ) ) THEN

*                         squash workspace in Y direction storing the
*                         result in output array

                           CALL SQSHY( WDIMS1( 1 ), WDIMS1( 2 ),
     :                                 %VAL( WPNTR1 ), ODIMS( 1 ),
     :                                 ODIMS( 2 ), %VAL( PNTRO ),
     :                                 %VAL( WPNTR5 ), %VAL( WPNTR3 ),
     :                                 STATUS )
                        ELSE

*                         Just copy the data as no reconfiguration
*                         is required in the y direction.

                           CALL COPY2D( ODIMS( 1 ), ODIMS( 2 ),
     :                                  %VAL( WPNTR1 ), %VAL( PNTRO ),
     :                                  STATUS )

                        END IF

*                      tidy up last workspace

                        CALL AIF_ANTMP( WLOC5, STATUS )

                     ELSE

                        CALL ERR_REP( 'ERR_SQORST_WSP',
     :                    'SQORST: Unable to get workspace for '/
     :                    /'squashing', STATUS )

*                   end of if-no-error-getting-and-mapping-workspace
*                   check

                     END IF

*                   tidy up rest of workspace

                     CALL AIF_ANTMP( WLOC1, STATUS )
                     CALL AIF_ANTMP( WLOC2, STATUS )
                     CALL AIF_ANTMP( WLOC3, STATUS )
                     CALL AIF_ANTMP( WLOC4, STATUS )

*                   unmap output data array

                     CALL CMP_UNMAP( LOCDO, DNAMEO, STATUS )

                  ELSE

                     CALL ERR_REP( 'ERR_SQORST_NOMPO',
     :                 'SQORST: Error occurred whilst trying to map '/
     :                 /'output frame', STATUS )

*                end of if-no-error-after-mapping-output-data-array
*                check

                  END IF

*                tidy up output structures

                  CALL DAT_ANNUL( LOCDO, STATUS )
                  CALL DAT_ANNUL( LOCO, STATUS )

               ELSE

                  CALL ERR_REP( 'ERR_SQORST_NOOI',
     :              'SQORST: Unable to create output array, perhaps '/
     :              /'too large for your disk or virtual-memory '/
     :              /'quotas', STATUS )

*             end of if-no-error-after-creating-output-structure check

               END IF

            ELSE

               IF ( STATUS .NE. PAR__ABORT .AND.
     :              STATUS .NE. PAR__NULL ) THEN

*                announce the error

                  CALL ERR_REP( 'ERR_SQORST_PAR',
     :              'SQORST: Error obtaining dimensions of output '/
     :              /'array', STATUS )
               END IF

*          end of if-no-error-after-getting-output-array-size check

            END IF

*          unmap input data array

            CALL CMP_UNMAP( LOCDI, DNAMEI, STATUS )

         ELSE

            CALL ERR_REP( 'ERR_SQORST_NOMPI',
     :        'SQORST: Error occurred whilst trying to map input frame',
     :        STATUS )

*       end of if-no-error-after-mapping-input-data-array check

         END IF

*       tidy up the input structures

         CALL DAT_ANNUL( LOCDI, STATUS )
         CALL DAT_ANNUL( LOCI, STATUS )

      ELSE

         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'ERR_SQORST_NOFRI',
     :        'SQORST: Error occurred whilst trying to access input '/
     :        /'frame', STATUS )
         END IF

*    end of if-no-error-after-getting-input-structure check

      END IF

      END
