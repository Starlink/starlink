*+  LOG10 - Takes the base-10 logarithm of each pixel of a data array

      SUBROUTINE KAP_LOG10( STATUS )
*
*    Description :
*
*     This routine takes the logarithm to base 10 of each pixel of
*     a data array. The result goes into a new output data array.
*     Both data arrays are stored in the IMAGE format.
*
*     The magic-value method is used for processing bad data.
*
*    Invocation :
*
*     CALL KAP_LOG10( STATUS )
*
*    Parameters :
*
*     INPIC  =  IMAGE( READ )
*         Input IMAGE structure containing the data array
*     BASE  =  REAL( READ )
*         Base of logarithm to be taken of each input data array pixel
*     OUTPIC  =  IMAGE( WRITE )
*         Output IMAGE structure holding result of processed data
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
*           Get base of the logarithm to be used
*           If no error then
*              Create output IMAGE structure
*              Propagate NDF QUALITY and MORE from the input data file
*              If no error then
*                 Map a new data-array component in output
*                 If no error then
*                    Call subroutine to take logarithm of array,
*                     results going into output array
*                    Unmap output data array
*                 Else
*                    Report error
*                 Endif
*                 Annul output IMAGE structure
*              Else
*                 Report error
*              Endif
*           Endif
*           Unmap input data array
*        Else
*           Report error
*        Endif
*        Annul input IMAGE structure
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
*     02-07-1986 : First implementation (REVA::MJM)
*     1986 Aug 7 : Renamed algorithm subroutine (LOGARR), changed new
*                  2nd argument to total number of pixels. Corrected
*                  default base to 10. (RAL::CUR)
*     1986 Aug 29: Added argument section, generalised to vectors and
*                  tidied (RAL::CUR).
*     1987 Oct 15: Reordered tidying and used CMP_SHAPE (RAL::CUR)
*     1988 Mar 17: Referred to `array' rather than `image'
*                  (RAL::CUR)
*     1988 May 30: More reporting of error context (RAL::CUR)
*     1989 Jun 13: Allow for processing primitive NDFs (RAL::CUR)
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
     :  MXBASE,                ! maximum base allowed for logarithm
     :  MNBASE                 ! minimum   "     "     "      "   
      PARAMETER( MXBASE  =  1000 )
      PARAMETER( MNBASE  =  0.001 )

*    Local variables :

      INTEGER
     :  DIMTOT,                ! number of pixels in input array
     :  IDIMS( MXDIMS ),       ! dimensions of input array
     :  NDIMS,                 ! number of dimensions of input array
     :  ORIGIN( DAT__MXDIM ),  ! Origin of the data array
     :  PNTRI,                 ! pointer to input data
     :  PNTRO                  !    "     " output data

      REAL
     :  BASE                   ! base of logarithm to be taken of
                               ! each pixel of input array

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

*       get shape of first array

         CALL CMP_SHAPE( LOCDI, DNAMEI, MXDIMS, IDIMS, NDIMS, STATUS )

*       map the data-array component of the input structure

         CALL CMP_MAPV( LOCDI, DNAMEI, '_REAL', 'READ',
     :                  PNTRI, DIMTOT, STATUS )

*       if no error then continue

         IF ( STATUS .EQ. SAI__OK ) THEN

*          try to get the logarithm base 

            CALL PAR_GDR0R( 'BASE', 10.0, MNBASE, MXBASE, .TRUE., BASE,
     :                       STATUS )

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

*                   call working subroutine to take the log of each pixel
*                   of input array, result going into output array

                     CALL LOGARR( %VAL( PNTRI ), DIMTOT, BASE,
     :                            %VAL( PNTRO ), STATUS )

*                   unmap output data array

                     CALL CMP_UNMAP( LOCDO, DNAMEO, STATUS )

                  ELSE

                     CALL ERR_REP( 'ERR_LOG10_NOMPO',
     :                 'LOG10: Error occurred whilst trying to map '/
     :                 /'output frame', STATUS )

*                end of if-no-error-before-accessing-pointers check

                  END IF

*                tidy up output structures

                  CALL DAT_ANNUL( LOCDO, STATUS )
                  CALL DAT_ANNUL( LOCO, STATUS )

               ELSE

                  IF ( STATUS .NE. PAR__ABORT ) THEN
                     CALL ERR_REP( 'ERR_LOG10_NOFRO',
     :                 'LOG10: Error occurred whilst trying to '/
     :                 /'access output frame', STATUS )
                  END IF

*             end of if-no-error-after-creating-output check

               END IF


*          end of if-no-error-after-getting-base check

            END IF

*          unmap input data array

            CALL CMP_UNMAP( LOCDI, DNAMEI, STATUS )

         ELSE

            CALL ERR_REP( 'ERR_LOG10_NOMPI',
     :        'LOG10: Error occurred whilst trying to map or find '/
     :        /'the shape of input frame', STATUS )

*       end of if-no-error-after-mapping-input-array check

         END IF

*       tidy up the input structures

         CALL DAT_ANNUL( LOCDI, STATUS )
         CALL DAT_ANNUL( LOCI, STATUS )

      ELSE

         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'ERR_LOG10_NOFRI',
     :        'LOG10: Error occurred whilst trying to access input '/
     :        /'frame', STATUS )
         END IF

*    end of if-no-error-after-getting-input-array check

      END IF

*    return and end

      END
