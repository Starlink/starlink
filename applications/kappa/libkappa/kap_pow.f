*+  POW - Takes the specified power of each pixel of a data array

      SUBROUTINE KAP_POW ( STATUS )
*
*    Description :
*
*     This routine takes the specified power of each pixel of the
*     data array in the input IMAGE structure. The result goes into a
*     new output array, also in an IMAGE structure.
*
*     The magic-value method is used for processing bad data.  Output
*     pixels become bad if the result raising to the specified power is
*     undefined or out of range.  Negative pixel values will only
*     generate good output pixels when the power is an even integer.
*
*    Invocation :
*
*     CALL KAP_POW( STATUS )
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*         Input IMAGE structure containing the data array to be
*           processed
*     POWER  =  REAL( READ )
*         Power to be taken of each input data-array pixel
*     OUTPIC  =  IMAGE( WRITE )
*         Output IMAGE structure holding result of the processed data
*           array
*     OTITLE  =  CHAR( READ )
*         Title string for the output IMAGE structure
*
*    Arguments:
*
*     STATUS  = INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get locator for input structure
*     If no error then
*        Determine shape of input structure's data array
*        Map data-array component
*        If no error then
*           Get power to be used
*           If no error then
*              Create output IMAGE structure
*              Propagate NDF QUALITY and MORE from the input data file
*              If no error then
*                 Map a new data-array component in output
*                 If no error then
*                    Call subroutine to take power of array,
*                      results going into output array
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
*     Mark McCaughrean UoE ( REVA::MJM )
*     Malcolm Currie RAL ( UK.AC.RL.STAR::CUR )
*
*    History :
*
*     03-07-1986 : First implementation (REVA::MJM)
*     1986 Aug 7 : Renamed algorithm subroutine (POWARR), changed new
*                  2nd argument to total number of pixels (RAL::CUR).
*     1986 Aug 29: Add argument section, generalised to vector and
*                  tidied (RAL::CUR).
*     1987 Oct 16: Reordered tidying and used CMP_SHAPE (RAL::CUR).
*     1988 Mar 17: Referred to `array' rather than `image'
*                  (RAL::CUR).
*     1988 May 31: More reporting of error context (RAL::CUR).
*     1989 Jun 13: Allow for processing primitive NDFs (RAL::CUR).
*     1991 Oct 25: Propagates AXIS, LABEL, and HISTORY (RAL::CUR).
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
     :  MXDIMS                 ! maximum dimensionality of input arrays
      PARAMETER( MXDIMS = DAT__MXDIM )

      REAL
     :  MAXPOW,                ! maximum power allowed 
     :  MINPOW                 ! minimum   "      "     
      PARAMETER( MAXPOW  =  100. )
      PARAMETER( MINPOW  = -100. )

*    Local variables :

      INTEGER
     :  DIMTOT,                ! number of pixels in an array
     :  NDIMS,                 ! number of dimensions of input array
     :  IDIMS( MXDIMS ),       ! dimensions of input array
     :  ORIGIN( DAT__MXDIM ),  ! Origin of the data array
     :  PNTRI,                 ! pointer to input data
     :  PNTRO                  !    "     " output data

      REAL
     :  POWER                  ! power to be taken of each pixel
                               ! of input array

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

*    try to get the input IMAGE structure

      CALL KPG1_GETIM( 'INPIC', LOCI, LOCDI, DNAMEI, ORIGIN, STATUS )

*    if no error then continue

      IF ( STATUS .EQ. SAI__OK ) THEN

*       get shape of input array

         CALL CMP_SHAPE( LOCDI, DNAMEI, MXDIMS, IDIMS, NDIMS, STATUS )

*       map the data-array component of the input structure

         CALL CMP_MAPV( LOCDI, DNAMEI, '_REAL', 'READ',
     :                  PNTRI, DIMTOT, STATUS )

*       if no error then continue

         IF ( STATUS .EQ. SAI__OK ) THEN

*          try to get the power to apply to each input array pixel

            CALL PAR_GDR0R( 'POWER', 2.0, MINPOW, MAXPOW, .FALSE.,
     :                      POWER, STATUS )

*          if no error then continue

            IF ( STATUS .EQ. SAI__OK ) THEN

*             create an output IMAGE structure

               CALL KPG1_CROUT( 'OUTPIC', 'OTITLE', NDIMS, IDIMS,
     :                          ORIGIN, LOCO, LOCDO, DNAMEO, STATUS )

*             propagate AXIS, QUALITY, LABEL, HISTORY and extensions
*             from the input data file

               CALL KPG1_IMPRG( LOCI, 'AXIS,QUALITY', LOCO, STATUS )

*             if no error then continue

               IF ( STATUS .EQ. SAI__OK ) THEN
     
*                map a data-array component

                  CALL CMP_MAPV( LOCDO, DNAMEO, '_REAL',
     :                           'WRITE', PNTRO, DIMTOT, STATUS )

*                check status before accessing pointers

                  IF ( STATUS .EQ. SAI__OK ) THEN

*                   call working subroutine to take power of each pixel
*                   of input array, result going into output array

                     CALL POWARR( %VAL( PNTRI ), DIMTOT, POWER,
     :                            %VAL( PNTRO ), STATUS )

*                   unmap output data array

                     CALL CMP_UNMAP( LOCDO, DNAMEO, STATUS )

                  ELSE

                     CALL ERR_REP( 'ERR_POW_NOMPO',
     :                 'POW : Error occurred whilst trying to map '/
     :                 /'output frame', STATUS )

*                end of if-no-error-before-accessing-pointers check

                  END IF

*                tidy up output structures

                  CALL DAT_ANNUL( LOCDO, STATUS )
                  CALL DAT_ANNUL( LOCO, STATUS )

               ELSE

                  IF ( STATUS .NE. PAR__ABORT ) THEN
                     CALL ERR_REP( 'ERR_POW_NOFRO',
     :                 'POW : Error occurred whilst trying to access '/
     :                 /'output frame', STATUS )
                  END IF

*             end of if-no-error-after-creating-output check

               END IF

            ELSE

               IF ( STATUS .NE. PAR__ABORT .AND.
     :              STATUS .NE. PAR__NULL ) THEN

*                announce the error

                  CALL ERR_REP( 'ERR_POW_PAR',
     :              'POW : Error obtaining scalar - aborting',
     :              STATUS )
               END IF

*          end of if-no-error-after-getting-scalar check

            END IF

*          unmap input data array

            CALL CMP_UNMAP( LOCDI, DNAMEI, STATUS )

         ELSE

            CALL ERR_REP( 'ERR_POW_NOMPI',
     :        'POW : Error occurred whilst trying to map or find the '/
     :        /'shape of the input frame', STATUS )

*       end of if-no-error-after-mapping-input-image check

         END IF

*       tidy up the input structures

         CALL DAT_ANNUL( LOCDI, STATUS )
         CALL DAT_ANNUL( LOCI, STATUS )

      ELSE

         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'ERR_POW_NOFRI',
     :        'POW : Error occurred whilst trying to access input '/
     :        /'frame', STATUS )
         END IF

*    end of if-no-error-after-getting-input-image check

      END IF

*    return and end

      END

