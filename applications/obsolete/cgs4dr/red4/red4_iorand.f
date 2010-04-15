*+  RED4_IORAND - Combine two bad pixel masks with logical operation.
      SUBROUTINE RED4_IORAND( OPERATION, STATUS )
*    Description :
*     This routine combines two bad pixel masks to produce a third
*     using the specified logical operation as follows:-
*
*     IAND     - The resultant mask will contain a BAD value only
*                in places where both input masks contain a BAD value.
*                All other values will be GOOD.
*
*     IOR      - The resultant mask will contain a BAD value in places
*                where either of the input masks contain a BAD value.
*                The resultant mask will be GOOD only in places where
*                both input masks are good.
*
*     IEOR     - The resultant mask will contain a BAD value in places
*                where one of the input masks contain a BAD value and
*                the other a GOOD value. If both masks contain a GOOD
*                or a BAD value, the resultant mask will be GOOD.
*
*     Command parameters -
*
*     IMAGE1 The name of the structure containing the first mask.
*
*     IMAGE2 The name of the structure containing the second mask.
*
*     OUTPUT The name of the result of the operation.  This can
*            be the same as for IMAGE1.  If not, a new structure
*            is created, with everything but the data array
*            a direct copy of the first mask.
*
*    Invocation :
*     CALL RED4_IORAND( OPERATION, STATUS )
*    Parameters :
*     OPERATION     = CHARACTER*(*)( READ )
*           The logical operation to be preformed.
*           Currently supported operations are 'IAND', 'IOR' and 'IEOR'
*           (see description).
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
*     17-May-1990: Original version.                               (SMB)
*     18-May-1990: 'CGS4_MASKS:' inserted before the name of
*                  each mask.                                      (SMB)
*      4-Sep-1990: ERR_OUT replaced with ERR_REP.                  (SMB)
*     24-Feb-1991: Some error reporting mistakes fixed, which would
*                  have made this routine fail under ADAM V1.9.    (SMB)
*     22-Feb-1993: Conform to error strategy                       (PND)
*     16-DEC-1993: Generalise for any input files                  (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Import :
      CHARACTER*(*)
     :  OPERATION            ! Name of logical operation
*    Status :
      INTEGER
     :  STATUS               ! Global status
*    Global variables :
*    Local constants :
      INTEGER
     :  MAXDIM               ! The maximum acceptable number of
                             ! dimensions in a bad pixel mask
      PARAMETER ( MAXDIM = 3 )
*    Local variables :
      CHARACTER*80
     :  IMAGE1,              ! Name of first mask
     :  IMAGE2,              ! Name of second mask
     :  OUTPUT               ! Name of resultant mask
      INTEGER
     :  NDIM,                ! The number of dimensions in the mask
     :  DIMS( MAXDIM ),      ! The dimensions of the bad pixel mask
     :  NELM,                ! The number of elements in the bad pixel mask
     :  ADDRESS,             ! Address of virtual memory
     :  IMAGE1_PTR,          ! Address of mapped data array from IMAGE1
     :  IMAGE2_PTR,          ! Address of mapped data array from IMAGE2
     :  OUTPUT_PTR,          ! Address of mapped data array from OUTPUT
     :  IMAGE1_SLOT,         ! Slot for mapped data array from IMAGE1
     :  IMAGE2_SLOT,         ! Slot for mapped data array from IMAGE2
     :  OUTPUT_SLOT          ! Slot for mapped data array from OUTPUT
*-

*   Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the names of the IMAGE1, IMAGE2 and OUTPUT structures,
*   using IMAGE1 as a dynamic default for OUTPUT.
      CALL PAR_GET0C( 'IMAGE1', IMAGE1, STATUS )
      CALL RED4_CHECK_INPUT( IMAGE1, STATUS )

      CALL PAR_GET0C( 'IMAGE2', IMAGE2, STATUS )
      CALL RED4_CHECK_INPUT( IMAGE2, STATUS )

      CALL PAR_DEF0C( 'OUTPUT', IMAGE1, STATUS )
      CALL PAR_GET0C( 'OUTPUT', OUTPUT, STATUS )
      CALL RED4_CHECK_INPUT( OUTPUT, STATUS )

*   Check this has worked
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Open DS
         CALL DSA_OPEN( STATUS )

*      Open the two input data structures
         CALL DSA_NAMED_INPUT( 'IMAGE1', IMAGE1, STATUS )
         CALL DSA_NAMED_INPUT( 'IMAGE2', IMAGE2, STATUS )

*      Obtain the dimensions of the data array in IMAGE1
         CALL DSA_DATA_SIZE( 'IMAGE1', MAXDIM, NDIM, DIMS,
     :     NELM, STATUS )

*      Check that the sizes of the data arrays contained in these
*      structures, and the range and units of the axes match up.
*      (If any do not, a bad status will be returned).
*      (Note that the actual dimensions of the data arrays are not
*      checked to be the same, so for example a 62x58 mask can be
*      combined with a 58x62 mask. Perhaps this check should be
*      included).
         CALL DSA_MATCH_SIZES( 'IMAGE1', 'IMAGE2', STATUS )
         CALL DSA_MATCH_AXES( 'IMAGE1', 'IMAGE2', STATUS )

*      Open the output structure, using the IMAGE1 structure as
*      a template. (This will only happen if the STATUS is still ok
*      at this point).
         CALL DSA_NAMED_OUTPUT( 'OUTPUT', OUTPUT, 'IMAGE1',
     :     0, 0, STATUS )

*      Check that everything has worked up to this point
         IF ( STATUS .EQ. ADAM__OK ) THEN

*         Map the data arrays in the two input structures and in
*         the output structure as BYTE arrays.
            CALL DSA_MAP_DATA( 'IMAGE1', 'READ', 'BYTE', ADDRESS,
     :         IMAGE1_SLOT, STATUS )
            IMAGE1_PTR = ADDRESS
            CALL DSA_MAP_DATA('IMAGE2', 'READ', 'BYTE', ADDRESS,
     :         IMAGE2_SLOT, STATUS )
            IMAGE2_PTR = ADDRESS
            CALL DSA_MAP_DATA('OUTPUT', 'WRITE', 'BYTE', ADDRESS,
     :         OUTPUT_SLOT, STATUS )
            OUTPUT_PTR = ADDRESS

*         Check the data arrays have been mapped successfully
            IF ( STATUS .EQ. ADAM__OK ) THEN

*            Carry out the required operation.
               IF ( OPERATION .EQ. 'IAND' ) THEN

                  CALL RED4_ANDMASK( DIMS(1), DIMS(2),
     :              %val(IMAGE1_PTR), %val(IMAGE2_PTR),
     :              %val(OUTPUT_PTR), STATUS )
               ELSE IF ( OPERATION .EQ. 'IOR' ) THEN

                  CALL RED4_ORMASK( DIMS(1), DIMS(2),
     :              %val(IMAGE1_PTR), %val(IMAGE2_PTR),
     :              %val(OUTPUT_PTR), STATUS )
               ELSE IF ( OPERATION .EQ. 'IEOR' ) THEN

                  CALL RED4_EORMASK( DIMS(1), DIMS(2),
     :              %val(IMAGE1_PTR), %val(IMAGE2_PTR),
     :              %val(OUTPUT_PTR), STATUS )
               ELSE

                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'OPERATION', OPERATION )
                  CALL ERR_REP( ' ', 'RED4_IORAND: Invalid '/
     :              /'operation ^OPERATION', STATUS )
               END IF
            ELSE

               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_IORAND: Error mapping '/
     :           /'data arrays', STATUS )
            END IF
         ELSE

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_IORAND: Error opening DSA '/
     :        /'and data structures', STATUS )
         END IF

*      Close DSA. Note that this will unmap any data and workspace
*      mapped above. It will attempt to execute regardless of the
*      status.
         CALL DSA_CLOSE( STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_IORAND: Error obtaining input parameters', STATUS )
      END IF

      END
