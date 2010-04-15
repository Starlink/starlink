*+  RED4_INOT - Invert a bad pixel mask.
      SUBROUTINE RED4_INOT( STATUS )
*    Description :
*     This routine inverts a bad pixel mask. Each element of the output
*     mask is set BAD if the input mask is GOOD, and GOOD if the input
*     mask is bad.
*
*     Command parameters -
*
*     INPUT  The name of the structure containing the input mask.
*
*     OUTPUT The name of the result of the operation.  This can
*            be the same as for INPUT.  If not, a new structure
*            is created, with everything but the data array
*            a direct copy of the first mask.
*
*    Invocation :
*     CALL RED4_INOT( STATUS )
*    Parameters :
*     STATUS    = INTEGER( UPDATE )
*           Global status. This must be ADAM__OK on entry.
*           If this routine completes successfully, the STATUS
*           will be ADAM__OK on exit. Any other value indicates
*           an error.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*     17-May-1990: Original version.                           (SMB)
*     18-May-1990: 'CGS4_MASKS:' inserted before the name of
*                  each mask.                                  (SMB)
*      4-Sep-1990: ERR_OUT replaced with ERR_REP.              (SMB)
*     19-Feb-1993: Conform to error strategy                   (PND)
*     16-dec-1993: Generalise for any input filename           (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Status :
      INTEGER
     :  STATUS               ! Global status
*    Local constants :
      INTEGER
     :  MAXDIM               ! The maximum acceptable number of
                             ! dimensions in a bad pixel mask
      PARAMETER ( MAXDIM = 2 )
*    Local variables :
      CHARACTER*80
     :  INPUT,               ! Name of input mask
     :  OUTPUT               ! Name of resultant mask
      INTEGER
     :  NDIM,                ! The number of dimensions in the mask
     :  DIMS( MAXDIM ),      ! The dimensions of the bad pixel mask
     :  NELM,                ! The number of elements in the bad pixel mask
     :  ADDRESS,             ! Address of virtual memory
     :  INPUT_PTR,           ! Address of mapped data array from INPUT
     :  OUTPUT_PTR,          ! Address of mapped data array from OUTPUT
     :  INPUT_SLOT,          ! Slot for mapped data array from INPUT
     :  OUTPUT_SLOT          ! Slot for mapped data array from OUTPUT
*-

*   Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the names of the INPUT and OUTPUT structures,
*   using INPUT as a dynamic default for OUTPUT.
      CALL PAR_GET0C( 'INPUT', INPUT, STATUS )
      CALL RED4_CHECK_INPUT( INPUT, STATUS )

      CALL PAR_DEF0C( 'OUTPUT', INPUT, STATUS )
      CALL PAR_GET0C( 'OUTPUT', OUTPUT, STATUS )
      CALL RED4_CHECK_INPUT( OUTPUT, STATUS )

*   Check this has worked
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Open DSA
         CALL DSA_OPEN( STATUS )

*      Open the input data structure
         CALL DSA_NAMED_INPUT( 'INPUT', INPUT, STATUS )

*      Obtain the dimensions of the data array in INPUT
         CALL DSA_DATA_SIZE( 'INPUT', MAXDIM, NDIM, DIMS,
     :      NELM, STATUS )

*      Open the output structure, using the INPUT structure as
*      a template. (This will only happen if the STATUS is still ok
*      at this point).
         CALL DSA_NAMED_OUTPUT( 'OUTPUT', OUTPUT, 'INPUT', 0,
     :      0, STATUS )

*      Check that everything has worked up to this point
         IF ( STATUS .EQ. ADAM__OK ) THEN

*         Map the data arrays in the input and output structures
*         as BYTE arrays.
            CALL DSA_MAP_DATA( 'INPUT', 'READ', 'BYTE', ADDRESS,
     :         INPUT_SLOT, STATUS )
            INPUT_PTR = ADDRESS
            CALL DSA_MAP_DATA('OUTPUT', 'WRITE', 'BYTE', ADDRESS,
     :         OUTPUT_SLOT, STATUS )
            OUTPUT_PTR = ADDRESS

*         Check the data arrays have been mapped successfully
            IF ( STATUS .EQ. ADAM__OK ) THEN

*            Invert the INPUT mask and write the result to OUTPUT.
               CALL RED4_NOTMASK( DIMS(1), DIMS(2),
     :           %val(INPUT_PTR), %val(OUTPUT_PTR), STATUS )
            ELSE

               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_INOT: Error mapping '/
     :           /'data arrays', STATUS )
            END IF
         ELSE

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_INOT: Error opening DSA '/
     :        /'and data structures', STATUS )
         END IF

*      Close DSA. Note that this will unmap any data and workspace
*      mapped above. It will attempt to execute regardless of the
*      status.
         CALL DSA_CLOSE( STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_INOT: Error obtaining input parameters', STATUS )
      END IF

      END
