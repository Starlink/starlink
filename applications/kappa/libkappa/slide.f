*+  SLIDE - Realigns a 2-d data array via an x,y shift

      SUBROUTINE SLIDE( STATUS )
*
*    Description :
*
*     The data array in the input IMAGE structure is shifted, in either
*     or both of the x and y axes, to produce the new array, in the
*     output image structure. The shifts in x and y are either input as
*     absolute x and y shifts by the user, or alternatively, are
*     calculated from the co-ordinates of two points provided by the
*     user. These are a fiducial point, with co-ordinates %FIDX, %FIDY,
*     and a standard object, with co-ordinates %OBJX, %OBJY. The shift
*     in x is then given by %FIDX - %OBJX and the shift in y is given by
*     %FIDY - %OBJY. The output data array is padded with zeros in the
*     regions not occupied by the shifted input array.  Fractional
*     shifts are computed by bilinear interpolation.
*
*     The magic-value method is used for processing bad data.
*
*    Invocation :
*
*     CALL SLIDE( STATUS )
*
*    Parameters :
*
*     INPIC  = IMAGE( READ )
*         IMAGE structure containing the 2-d data array to be shifted.
*     STYPE  = CHAR( READ )
*         The sort of shift is to be used. The choice is 'Relative',
*           or 'Absolute'.
*     ABSX   = REAL( READ )
*         Absolute x shift in pixels (Absolute shift)
*     ABSY   = REAL( READ )
*         Absolute y shift in pixels (Absolute shift)
*     FIDX   = REAL( READ )
*         X-co-ordinate of the fiducial point. (Relative shift)
*     FIDY   = REAL( READ )
*         Y-co-ordinate of the fiducial point. (Relative shift)
*     OBJX   = REAL( READ )
*         X-co-ordinate of the standard object. (Relative shift)
*     OBJY   = REAL( READ )
*         Y-co-ordinate of the standard object. (Relative shift)
*     OUTPIC = IMAGE( WRITE )
*         IMAGE structure to contain the 2-d data array after being
*           shifted.
*     OTITLE = CHAR( READ )
*         Will be used as the TITLE component for the output IMAGE
*           structure.
*
*    Arguments:
*
*     STATUS  = INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get input IMAGE-type data structure
*     If no error then
*        Map data-array component
*        If no error then
*           Input which type of shift is to be used - Relative or
*             Absolute
*           If Absolute then
*              Input absolute x and y shifts
*           Else
*              Input x and y co-ordinates of the fiducial point.
*              Input x and y co-ordinates of the "standard object"
*           Endif
*           If no error calculate the shifts in the x and y directions
*           If no error then
*              Write out the values of the shifts to the user.
*              Call SHIFTS for each x axis to set up the parameters for
*                performing the x shift.
*              If status is bad on return then
*                 Report context
*              Else
*                 Call SHIFTS for each y axis to set up the parameters
*                   for performing the y shift.
*                 If status is bad on return then
*                       Report context
*                 Else
*                    Create output IMAGE type data structure with
*                      DATA_ARRAY component of the same dimensions as
*                      input DATA_ARRAY component and get a TITLE
*                      component for it.
*                    Propagate NDF MORE from the input data file
*                    If no error then
*                       Map output data array
*                       If no error then
*                          Create workspace of same dimensions as input/
*                            output arrays.
*                          If no error then
*                             Call SHIFTX to move the input array,
*                               shifted in the x direction, into the
*                               workspace.
*                             Call SHIFTY to move the workspace,
*                               shifted in the y direction, into the
*                               output array.
*                             Tidy workspace
*                          Else
*                             Report error
*                          Endif
*                          Unmap output data array
*                       Else
*                          Report error
*                       Endif
*                       Annul output structure
*                    Else
*                       Report error
*                    Endif
*                 Endif
*              Endif
*           Else
*              Report error
*           Endif
*           Unmap input data array
*        Else
*           Report error
*        Endif
*        Annul input structure
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
*     Dave Baines (ROE::ASOC5)
*     Mark McCaughrean (REVA::MJM)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*     18/08/1983 : Original version                   (ROE::ASOC5)
*     19/02/1984 : Modified to use new SHIFTS routine (ROE::ASOC5)
*     03/06/1985 : Modified to allow Relative or Absolute
*                : shifting                           (REVA::MJM)
*     1986 Aug 8 : Standardised prologue formatting. Added status check
*                  on entry (RL.STAR::CUR).
*     1986 Sep 1 : Added arguments section to the prologue and tidied
*                  (RL.STAR::CUR).
*     1987 Oct 16: Reordered tidying and extra status checks
*                  (RL.STAR::CUR)
*     1988 Mar 16: Substituted AIF_ANTMP to annul workspace
*                  (RL.STAR::CUR).
*     1988 Mar 17: Referred to `array' rather than `image'
*                  (RL.STAR::CUR)
*     1988 May 31: More reporting of error context (RL.STAR::CUR)
*     1989 Jun 13: Allow for processing primitive NDFs (RL.STAR::CUR)
*     1989 Aug  7: Passed array dimensions as separate variables
*                  to SHIFTX and SHIFTY (RL.STAR::CUR).
*     1989 Dec 21: Workspace managed by AIF_TEMP (RL.STAR::CUR).
*     1991 Oct 25: Propagates UNITS, LABEL, and HISTORY (RAL::CUR).
*     1992 Feb 25: Limited processing of simple NDFs (RL.STAR::CUR)
*     1992 Mar  3: Replaced AIF parameter-system calls by the extended
*                  PAR library (RAL::CUR).
*     1995 Oct 29: Renamed from SHIFT to avoid name clash with the
*                  C-shell shift built-in function (mjc@star.rl.ac.uk).
*
*    Type Definitions :

      IMPLICIT NONE            ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'        ! global SSE definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PAR_ERR'        ! parameter-system errors

*    Status :

      INTEGER STATUS

*    Local constants :

      INTEGER NDIM
      PARAMETER ( NDIM = 2 )   ! dimensionality of input/output arrays

*    Local variables :

      CHARACTER*(DAT__SZLOC)   ! locators for :
     :  LOCDI,                 ! structure containing the input data
                               ! array
     :  LOCDO,                 ! structure containing the output data
                               ! array
     :  LOCI,                  ! input data structure
     :  LOCO,                  ! output data structure
     :  WLOC                   ! workspace array

      CHARACTER * ( DAT__SZNAM )
     :  DNAMEI,                ! Name of the input data-array component
     :  DNAMEO                 ! Name of the output data-array component

      INTEGER
     :  DIMS( NDIM ),          ! dimensions of the input/output
                               ! DATA_ARRAYs
     :  I,                     ! loop counter
     :  ORIGIN( DAT__MXDIM ),  ! Origin of the data array
     :  PNTRI,                 ! pointer to : input DATA_ARRAY
     :  PNTRO,                 !            : output DATA_ARRAY
     :  WPNTR,                 !            : workspace array
     :  INTXS,                 ! integer number of pixels for shift 
                               ! in X direction
     :  INTYS                  ! integer number of pixels for shift 
                               ! in Y direction

      REAL
     :  ABSX,                  ! absolute X shift
     :  ABSY,                  !     "    Y   "
     :  FIDX,                  ! fiducial point X co-ordinate
     :  FIDY,                  !     "      "   Y      "
     :  OBJX,                  ! standard object X co-ordinate
     :  OBJY,                  !     "      "    Y     "
     :  XSHIFT,                ! value of shift in X direction
     :  YSHIFT,                !   "   "    "    " Y     "
     :  FRACX,                 ! fractional part of shift in X direction
     :  FRACY                  !      "       "   "   "    " Y     "

      LOGICAL                  ! true if:
     :  ABSOLU,                ! if shift is absolute
     :  XWHOLE,                ! shift in X is whole number of pixels
     :  YWHOLE,                !   "    " Y  "   "      "    "    "
     :  XNEG,                  !   "    " X  " negative
     :  YNEG                   !   "    " Y  "     "

      CHARACTER*8
     :  STYPE                  ! type of shifting to be used: Relative
                               ! or Absolute
*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    get locator to input IMAGE type data structure

      CALL KPG1_GETIM( 'INPIC', LOCI, LOCDI, DNAMEI, ORIGIN, STATUS )

*    check for error

      IF ( STATUS .EQ. SAI__OK ) THEN

*       map input DATA_ARRAY component

         CALL CMP_MAPN( LOCDI, DNAMEI, '_REAL', 'READ', NDIM,
     :                  PNTRI, DIMS, STATUS )

*       check for error

         IF ( STATUS .EQ. SAI__OK ) THEN

*          get type of shift - Relative or Absolute

            CALL PAR_CHOIC( 'STYPE', 'Absolute', 'Absolute,Relative',
     :                      .TRUE., STYPE, STATUS )

*          now set logical accordingly

            IF ( STYPE( 1:1 ) .EQ. 'R' ) THEN  
               ABSOLU = .FALSE.
            ELSE
               ABSOLU = .TRUE.
            ENDIF

*          now get shift amounts, depending on type of shift

            IF ( ABSOLU ) THEN

*             get x and y shifts

               CALL PAR_GET0R( 'ABSX', ABSX, STATUS )
               CALL PAR_GET0R( 'ABSY', ABSY, STATUS )

*             set x and y shifts accordingly

               XSHIFT = ABSX
               YSHIFT = ABSY

            ELSE

*             get position of fiducial point

               CALL PAR_GET0R( 'FIDX', FIDX, STATUS )
               CALL PAR_GET0R( 'FIDY', FIDY, STATUS )

*             get position of standard object

               CALL PAR_GET0R( 'OBJX', OBJX, STATUS )
               CALL PAR_GET0R( 'OBJY', OBJY, STATUS )

               IF ( STATUS .EQ. SAI__OK ) THEN

*                calculate X and Y shifts

                  XSHIFT = FIDX - OBJX
                  YSHIFT = FIDY - OBJY
               END IF

            END IF

            IF ( STATUS .EQ. SAI__OK ) THEN

*             tell user what the X and Y shifts are

               CALL MSG_SETR( 'XSHIFT', XSHIFT )
               CALL MSG_SETR( 'YSHIFT', YSHIFT )
               CALL MSG_OUT( 'SHIFT_XY', 'Shift in X = ^XSHIFT, Shift '/
     :                       /'in Y = ^YSHIFT', STATUS )

*             call SHIFTS to set up information for SHIFTX and SHIFTY

               CALL SHIFTS( DIMS( 1 ), XSHIFT, 'X', INTXS, XWHOLE, XNEG,
     :                      FRACX, STATUS )

*             if return with bad status, warn user and continue as
*             far as possible

               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_REP( 'ERR_SLIDE_SUB1',
     :              'SLIDE: Error occurred in SHIFTS for x shift. ',
     :              STATUS )
               ELSE

*                evaluate the parameter for shifting to see if an
*                integer y shift has been requested

                  CALL SHIFTS( DIMS( 2 ), YSHIFT, 'Y', INTYS, YWHOLE,
     :                         YNEG, FRACY, STATUS )

*                if return with bad status, warn user and continue as
*                far as possible

                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_REP( 'ERR_SLIDE_SUB2',
     :                 'SLIDE: Error occurred in SHIFTS for y shift.',
     :                 STATUS )
                  ELSE

*                   reset the origin since it has become undefined

                     DO I = 1, NDIM
                        ORIGIN( I ) = 1
                     END DO

*                   create the output IMAGE structure and get a title
*                   for it

                     CALL KPG1_CROUT( 'OUTPIC', 'OTITLE', NDIM, DIMS,
     :                                ORIGIN, LOCO, LOCDO, DNAMEO,
     :                                STATUS )

*                   propagate UNITS, LABEL, HISTORY and extensions from
*                   the input data file

                     CALL KPG1_IMPRG( LOCI, 'UNITS', LOCO, STATUS )

*                   check for error

                     IF ( STATUS .EQ. SAI__OK ) THEN

*                      map output DATA_ARRAY component

                        CALL CMP_MAPN( LOCDO, DNAMEO, '_REAL', 'WRITE',
     :                                 NDIM, PNTRO, DIMS, STATUS )

*                      check for error

                        IF ( STATUS .EQ. SAI__OK ) THEN

*                         create and map the workspace array

                           CALL AIF_GETVM( '_REAL', NDIM, DIMS, WPNTR,
     :                                     WLOC, STATUS )

                           IF ( STATUS .EQ. SAI__OK ) THEN

*                            shift input array into workspace in X
*                            direction

                              CALL SHIFTX( XNEG, XWHOLE, INTXS, FRACX,
     :                                     DIMS( 1 ), DIMS( 2 ),
     :                                     %VAL( PNTRI ), %VAL( WPNTR ),
     :                                     STATUS )

*                            shift workspace into output array in Y
*                            direction

                              CALL SHIFTY( YNEG, YWHOLE, INTYS, FRACY,
     :                                     DIMS( 1 ), DIMS( 2 ),
     :                                     %VAL( WPNTR ), %VAL( PNTRO ),
     :                                     STATUS )

*                            tidy up workspace

                              CALL AIF_ANTMP( WLOC, STATUS )

                           ELSE

                              CALL ERR_REP( 'ERR_SLIDE_WSP',
     :                          'SLIDE: Unable to get workspace for '/
     :                          /'shifting', STATUS )

*                         end of getting-and-mapping-workspace check

                           END IF

*                         unmap output array

                           CALL CMP_UNMAP( LOCDO, DNAMEO, STATUS )

                        ELSE

                           CALL ERR_REP( 'ERR_SLIDE_NOMPO',
     :                       'SLIDE: Error occurred whilst trying to '/
     :                       /'map output frame', STATUS )

*                      end of if-no-error-mapping-output-data-array
*                      check

                        END IF

*                      tidy up output structures

                        CALL DAT_ANNUL( LOCDO, STATUS )
                        CALL DAT_ANNUL( LOCO, STATUS )

                     ELSE

                        IF ( STATUS .NE. PAR__ABORT ) THEN
                           CALL ERR_REP( 'ERR_SLIDE_NOFRO',
     :                       'SLIDE: Error occurred whilst trying to '/
     :                       /'access output frame', STATUS )
                        END IF

*                   end of if-no-error-after-creating-output-structure
*                   check

                     END IF

*                end of bad-status-return-from-y-SHIFTS check

                  END IF

*             end of bad-status-return-from-x-SHIFTS check

               END IF

            ELSE

               IF ( STATUS .NE. PAR__ABORT .AND.
     :              STATUS .NE. PAR__NULL ) THEN

*                announce the error

                  CALL ERR_REP( 'ERR_SLIDE_PAR',
     :              'SLIDE: Error obtaining pixel shifts - aborting',
     :              STATUS )
               END IF

*          end of no-error-getting-parameters check

            END IF

*          unmap input data array

            CALL CMP_UNMAP( LOCDI, DNAMEI, STATUS )

         ELSE

            CALL ERR_REP( 'ERR_SLIDE_NOMPI',
     :        'SLIDE: Error occurred whilst trying to map the input '/
     :        /'frame', STATUS )

*       end of if-no-error-after-mapping-input-data-array check

         END IF

*       tidy up the input structures

         CALL DAT_ANNUL( LOCDI, STATUS )
         CALL DAT_ANNUL( LOCI, STATUS )

      ELSE

         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'ERR_SLIDE_NOFRI',
     :        'SLIDE: Error occurred whilst trying to access input '/
     :        /'frame', STATUS )
         END IF

*    end of if-no-error-after-getting-input-structure check

      END IF

      END
