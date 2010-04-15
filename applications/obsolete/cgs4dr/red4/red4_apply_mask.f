*+  RED4_APPLY_MASK - Apply a bad pixel mask to a data set
      SUBROUTINE RED4_APPLY_MASK( STATUS )
*    Description :
*     This routine applies a bad pixel mask to a data set. It copies
*     the data array from a bad pixel mask into the data quality array
*     of a specified data set. If the quality is BAD (1) then it sets
*     the associated data value to zero.
*    Invocation :
*     CALL RED4_APPLY_MASK( STATUS )
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
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*     29-Apr-1993: Original version.                           (PND)
*      9-Nov-1994: Attempt to make portable                    (AB)
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
*    Global variables :
*    External references:
      INTEGER CHR_LEN        ! CHR string length finding function
*    Local constants :
      INTEGER
     :  MAXDIM               ! The maximum acceptable number of
                             ! dimensions in a bad pixel mask
      PARAMETER ( MAXDIM = 3 )
*    Local variables :
      CHARACTER*80
     :  MASK,                ! Name of bad pixel mask
     :  DATA                 ! Name of data to which mask is to be applied
      CHARACTER*20 LPREFIX   ! Prefix to apply
      LOGICAL
     :  OVERWRITE,           ! TRUE if existing quality array can be overwritten
     :  QUALITY              ! TRUE if a data quality array already exists
      INTEGER
     :  CLEN,                ! The length of a string
     :  CPOS,                ! Position in a string
     :  NDIM,                ! The number of dimensions in the mask
     :  DIMS( MAXDIM ),      ! The dimensions of the bad pixel mask
     :  NELM,                ! The number of elements in the bad pixel mask
     :  ADDRESS,             ! Address of virtual memory
     :  MASK_PTR,            ! Address of mapped data array from MASK
     :  DATA_PTR,            ! Address of mapped data array from DATA
     :  DATA_SLOT,           ! Slot for mapped data array from DATA
     :  QUALITY_PTR,         ! Address of mapped data quality array from DATA
     :  MASK_SLOT,           ! Slot for mapped data array from MASK
     :  QUALITY_SLOT         ! Slot for mapped data quality array from DATA
*-

*   Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the names of the bad pixel mask and the data structure
      CALL PAR_GET0C( 'MASK', MASK, STATUS )
      CLEN = CHR_LEN( MASK )
      CPOS = INDEX( MASK, ':')
      IF (CPOS .EQ. 0) CPOS = INDEX( MASK, '/')

      IF ( CPOS .EQ. 0 ) THEN
         CALL RED4_GET_PREFIX ('MASK', LPREFIX, STATUS)
         MASK = LPREFIX(:CHR_LEN(LPREFIX))// MASK(1:CLEN)
      END IF
      CALL PAR_GET0C( 'DATA', DATA, STATUS )

*   Determine if an existing data quality array is to be overwritten.
      CALL PAR_GET0L( 'OVERWRITE', OVERWRITE, STATUS )

*   Check this has worked
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Open DSA
         CALL DSA_OPEN( STATUS )

*      Open the bad pixel mask and the data structure
         CALL RED4_CHECK_INPUT( MASK, STATUS )
         CALL DSA_NAMED_INPUT( 'MASK', MASK, STATUS )
         CALL RED4_CHECK_INPUT( DATA, STATUS )
         CALL DSA_NAMED_INPUT( 'DATA', DATA, STATUS )

*      Tell DSA that a data quality array will be used
         CALL DSA_USE_QUALITY( 'DATA', STATUS )

*      Obtain the dimensions of the data array in the bad pixel mask.
         CALL DSA_DATA_SIZE( 'MASK', MAXDIM, NDIM, DIMS, NELM, STATUS )

*      Check that the sizes and dimensions of the  arrays
         CALL DSA_MATCH_SIZES( 'MASK', 'DATA', STATUS )
         CALL DSA_MATCH_DIMENSION( 'MASK', 1, 'DATA', 1, STATUS )

*      Check for the presence of a quality array in the data structure.
         CALL DSA_SEEK_QUALITY( 'DATA', QUALITY, STATUS )

*      Check that everything has worked up to this point
         IF ( STATUS .EQ. ADAM__OK ) THEN

*         If a quality array exists, it can only be overwritten
*         if the OVERWRITE flag is TRUE. In this case, give a
*         warning but carry on.
            IF ( QUALITY .AND. OVERWRITE ) THEN

               CALL MSG_OUT( ' ', 'The existing quality array in '/
     :           /'the data structure will be overwritten.',
     :           STATUS )
            END IF

*         The program can only continue if there is no existing
*         quality array, or if it has been authorised to overwrite
*         an existing quality array.
            IF ( ( .NOT.QUALITY ) .OR. ( OVERWRITE ) ) THEN

*            Map the data array in the bad pixel mask structure and
*            the data quality array in the data structure.
               CALL DSA_MAP_DATA( 'MASK', 'READ', 'BYTE', ADDRESS,
     :            MASK_SLOT, STATUS )
               MASK_PTR = ADDRESS
               CALL DSA_MAP_DATA( 'DATA', 'UPDATE', 'FLOAT', ADDRESS,
     :            DATA_SLOT, STATUS )
               DATA_PTR = ADDRESS
               CALL DSA_MAP_QUALITY( 'DATA', 'WRITE', 'BYTE',
     :            ADDRESS, QUALITY_SLOT, STATUS )
               QUALITY_PTR = ADDRESS

*            Check the arrays have been mapped successfully
               IF ( STATUS .EQ. ADAM__OK ) THEN

*               Copy the bad pixel mask to the data quality array.
                  CALL GEN_MOVE( NELM, %val(MASK_PTR),
     :              %val(QUALITY_PTR) )

*               Set the data array value to zero if quality is bad (1)
                  CALL GEN_ZDATA( NELM, %val(DATA_PTR),
     :              %val(QUALITY_PTR) )

               ELSE

                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_APPLY_MASK: Error '/
     :              /'mapping arrays from data structures', STATUS )
               END IF
            ELSE

               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_APPLY_MASK: Data quality '/
     :           /'array already exists', STATUS )
               CALL ERR_REP( ' ', 'RED4_APPLY_MASK: '/
     :            /'To overwrite this specify '/
     :           /'OVERWRITE=TRUE', STATUS )
            END IF
         ELSE

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_APPLY_MASK: Error opening '/
     :        /'DSA and data structures', STATUS )
         END IF

*      Close DSA. Note that this will unmap any data and workspace
*      mapped above. It will attempt to execute regardless of the
*      status.
         CALL DSA_CLOSE( STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_APPLY_MASK: '/
     :     /'Error obtaining input parameters', STATUS )
      END IF

      END
