*+  OUTSET - Sets pixels outside a specified circle in a 2-d data array
*            to a specified value

      SUBROUTINE OUTSET ( STATUS )
*
*    Description :
*
*     A circle of a given centre and diameter within the 2-d data array
*     of the input IMAGE structure is specified.  The data array written
*     to the output IMAGE structure, is a copy of the input data array
*     except pixels outside the circle are set to a specified value.
*
*    Invocation :
*
*     CALL OUTSET( STATUS )
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*         IMAGE structure containing the array to be modified
*     OUTPIC  =  IMAGE( WRITE )
*         Output IMAGE structure containing the modified array
*     OTITLE  =  CHARACTER( READ )
*         Title for the output IMAGE structure
*     XCENTRE  =  REAL( READ )
*         x co-ordinate of the centre of the circle to be used
*     YCENTRE  =  REAL( READ )
*         y co-ordinate of the centre of the circle to be used
*     DIAMETER  =  REAL( READ )
*         Diameter of the circle to be used
*     NEWVAL  =  LITERAL( READ )
*         Value to replace old values in the pixels outside the circle.
*         If this is set to 'Bad' the magic-value is used.
*
*    Arguments:
*
*     STATUS  = INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check status on entry - return if not o.k.
*     Get input IMAGE structure
*     If no error then
*        Try to map a data-array component
*        If no error then
*           Output dimensions of input array
*           Get x,y centre and diameter of circle to be taken, and
*            value to be substituted for pixels outside the circle
*           If no error then
*              Create an output structure to hold processed image
*              Propagate NDF QUALITY and MORE from the input data file
*              If no error then 
*                 Map a data-array component in the output structure
*                 If no error then
*                    Call working subroutine to copy input data array
*                      into output one, except outside defined circle
*                      where input new value is substituted
*                    Unmap output data array
*                 Else
*                    Report error
*                 Endif
*                 Annul output IMAGE structure
*              Else
*                 Report error
*              Endif
*           Else
*              Report error
*           Endif
*           Unmap input data array
*        Else
*           Report error
*        Endif
*        Tidy up input structure
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
*     Mark McCaughrean UoE (REVA::MJM)
*     Malcolm Currie  STARLINK (RAL::CUR)
*
*    History :
*
*     17-09-1985 : First implementation (REVA::MJM)
*     03-07-1986 : Tidied and more error checking (REVA::MJM)
*     1986 Aug 7 : Renamed algorithmic routine to OTSTSB, reordered
*                  arguments (2nd to 7th). (RAL::CUR).
*     1986 Aug 29: Added arguments section to the prologue, nearly
*                  conformed to Starlink standards (RAL::CUR).
*     1988 Mar 17: Referred to `array' rather than `image'
*                  (RAL::CUR)
*     1988 May 31: More reporting of error context (RAL::CUR)
*     1989 Jun 13: Allow for processing primitive NDFs (RAL::CUR)
*     1989 Aug  8: Passed array dimensions as separate variables
*                  to OTSTSB (RAL::CUR).
*     1990 Oct  9: x-y centre is co-ordinates rather than pixel
*                  indices; added bad-value option for the replacement
*                  value (RAL::CUR).
*     1991 Oct 25: Propagates AXIS, UNITS, LABEL, and HISTORY
*                  (RAL::CUR).
*     1992 Feb 25: Limited processing of simple NDFs (RAL::CUR).
*     1992 Mar  3: Replaced AIF parameter-system calls by the extended
*                  PAR library (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT NONE            ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PAR_ERR'        ! parameter-system errors
      INCLUDE 'PRM_PAR'        ! Bad-pixel and extreme value constants

*    Status :

      INTEGER STATUS

*    Local Constants :

      INTEGER 
     :  NDIMS                  ! array dimensionality
      PARAMETER ( NDIMS  =  2 )! 2-d arrays only

*    Local variables :

      INTEGER 
     :  IDIMS( NDIMS ),        ! dimensions of input DATA_ARRAY
     :  ORIGIN( DAT__MXDIM ),  ! Origin of the data array
     :  PNTRI,                 ! pointer to input DATA_ARRAY component
     :  PNTRO                  ! pointer to output DATA_ARRAY component

      REAL 
     :  DIAMTR,                ! diameter of circle to be used
     :  NEWVAL,                ! new value for points outside circle
     :  XCENTR,                ! x co-ordinate of circle centre
     :  YCENTR                 ! y     "      "    "      "

      CHARACTER*20
     :  CNWVAL                 ! new value for pixels outside the circle

      CHARACTER*(DAT__SZLOC)   ! locators for :
     :  LOCDI,                 ! structure containing the input data
                               ! array
     :  LOCDO,                 ! structure containing the output data
                               ! array
     :  LOCI,                  ! input data structure
     :  LOCO                   ! output data structure

      CHARACTER * ( DAT__SZNAM )
     :  DNAMEI,                ! Name of the input data-array component
     :  DNAMEO                 ! Name of the output data-array component


*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    get a locator to input IMAGE type data structure

      CALL KPG1_GETIM( 'INPIC', LOCI, LOCDI, DNAMEI, ORIGIN, STATUS )
 
*    check status before continuing

      IF ( STATUS .EQ. SAI__OK ) THEN

*       try to map a data-array component of the input data structure

         CALL CMP_MAPN( LOCDI, DNAMEI, '_REAL', 'READ', NDIMS,
     :                  PNTRI, IDIMS, STATUS )
      
*       check status before continuing

         IF ( STATUS .EQ. SAI__OK ) THEN

*          tell user dimensions of input array

            CALL MSG_SETI( 'XDIM', IDIMS( 1 ) )
            CALL MSG_SETI( 'YDIM', IDIMS( 2 ) )
            CALL MSG_OUT( 'INPUT_DIMS', ' Array is ^XDIM by ^YDIM '/
     :                    /'pixels', STATUS )
            CALL MSG_OUT( 'BLANK', ' ', STATUS )


*          get x and y co-ordinates of centre of circle to be used

            CALL PAR_GDR0R( 'XCENTRE', 0.5 * REAL( IDIMS( 1 ) ), 0.0,
     :                      REAL( IDIMS( 1 ) ), .FALSE., XCENTR,
     :                      STATUS )
            CALL PAR_GDR0R( 'YCENTRE', 0.5 * REAL( IDIMS( 2 ) ), 0.0,
     :                      REAL( IDIMS( 2 ) ), .FALSE., YCENTR,
     :                      STATUS )

*          get the diameter of the circle setting the maximum to be
*          just big enough to encompass the whole array

            CALL PAR_GDR0R( 'DIAMETER', 10.0, 0.0,
     :                      SQRT(2.0)*MAX( IDIMS( 1 ), IDIMS( 2 ) ),
     :                      .FALSE., DIAMTR, STATUS )

*          get replacement value for pixels outside defined circle

            CALL PAR_MIX0R( 'NEWVAL', '0.0', VAL__MINR, VAL__MAXR,
     :                      'Bad', .FALSE., CNWVAL, STATUS )

*          It may be the bad-pixel value.

            IF ( CNWVAL .EQ. 'BAD' ) THEN
               NEWVAL = VAL__BADR
            ELSE

*             Convert the output numeric string to its numeric value.

               CALL CHR_CTOR( CNWVAL, NEWVAL, STATUS )
            END IF

*          check for error before continuing

            IF ( STATUS .EQ. SAI__OK ) THEN

*             now get an output array to contain modified data

               CALL KPG1_CROUT( 'OUTPIC', 'OTITLE', NDIMS, IDIMS,
     :                          ORIGIN, LOCO, LOCDO, DNAMEO, STATUS )

*             propagate AXIS, QUALITY, UNITS, LABEL, HISTORY and
*             extensions from the input data file

               CALL KPG1_IMPRG( LOCI, 'AXIS,QUALITY,UNITS', LOCO,
     :                          STATUS )

*             check error before continuing

               IF ( STATUS .EQ. SAI__OK ) THEN

*                map an output data-array component

                  CALL CMP_MAPN( LOCDO, DNAMEO, '_REAL', 
     :                           'WRITE', NDIMS, PNTRO, IDIMS, STATUS )

*                check status before accessing pointers

                  IF ( STATUS .EQ. SAI__OK ) THEN

*                   now call the subroutine that does the actual work

                     CALL OTSTSB( %VAL( PNTRI ), IDIMS( 1 ), IDIMS( 2 ),
     :                            XCENTR, YCENTR, DIAMTR, NEWVAL,
     :                            %VAL( PNTRO ), STATUS )

*                   unmap output data array

                     CALL CMP_UNMAP( LOCDO, DNAMEO, STATUS )

                  ELSE

                     CALL ERR_REP( 'ERR_REPSET_NOMPO',
     :                 'OUTSET : Error occurred whilst trying to map '/
     :                 /'output frame', STATUS )

                  END IF

*                Tidy up the output structures

                  CALL DAT_ANNUL( LOCDO, STATUS )
                  CALL DAT_ANNUL( LOCO, STATUS )

               ELSE

                  IF ( STATUS .NE. PAR__ABORT ) THEN
                     CALL ERR_REP( 'ERR_REPSET_NOFRO',
     :                 'OUTSET : Error occurred whilst trying to '/
     :                 /'access output frame', STATUS )
                  END IF

*             end of if-error-after-getting-output-structure check

               END IF

            ELSE

               IF ( STATUS .NE. PAR__ABORT .AND.
     :              STATUS .NE. PAR__NULL ) THEN

*                announce the error

                  CALL ERR_REP( 'ERR_REPSET_PAR',
     :              'OUTSET : Error obtaining parameters - aborting',
     :              STATUS )
               END IF

*          end of if-no-error-after-getting-parameters check

            END IF

*          unmap input data array

            CALL CMP_UNMAP( LOCDI, DNAMEI, STATUS )

         ELSE

            CALL ERR_REP( 'ERR_REPSET_NOMPI',
     :        'OUTSET : Error occurred whilst trying to map input '/
     :        /'frame', STATUS )

*       end of if-no-error-after-mapping-input-array check

         END IF

*       tidy up the input structures

         CALL DAT_ANNUL( LOCDI, STATUS )
         CALL DAT_ANNUL( LOCI, STATUS )

      ELSE

         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'ERR_REPSET_NOFRI',
     :        'OUTSET : Error occurred whilst trying to access input '/
     :        /'frame', STATUS )
         END IF

*    end of if-error-after-getting-input-structure check

      END IF

*    end

      END
