*+ RED4_CLEAN_OBS
      SUBROUTINE RED4_CLEAN_OBS (STATUS)
*    Description :
*     This routine will take any file with data and error arrays and set
*     the quality to bad over those points of the array where the
*     data is less than a certain number times the error. It is designed
*     for cleaning up flat-field observations and will set unilluminated
*     areas of the array to "bad".
*    Invocation :
*     CALL RED4_CLEAN_OBS (STATUS)
*    Parameters :
*    Method :
*    Deficiencies :
*     Perhaps the total number of pixels set bad should be reported ?
*    Bugs :
*    Authors :
*     John Lightfoot (REVAD::JFL)
*     Steven Beard (REVAD::SMB)
*     Phil Daly    (JACH::PND)
*    History :
*     1989:  Original version, with kludge.                       (JFL)
*     20-Nov-1989: Kludge removed from GEN_CLEANE by making it more
*                  general. The kludge is now in this routine !   (SMB)
*      9-Mar-1990: Remove the kludge by allowing the minimum
*                  acceptable data value to be specified as
*                  parameter. MAXDIM parameter added and set to 3.
*                  (Arrays were originally dimensioned to 10).    (SMB)
*     24-Apr-1990: Because of memory corruption problems, the
*                  code needs to be compiled with array bounds
*                  checking switched on. The Figaro dynamic
*                  memory functions (DYN_ELEMENT, DYNAMIC_MEM,
*                  DYN_INCREMENT) would not allow this. Code
*                  modified to use %val() instead.                (SMB)
*     21-Aug-1990: Code restructured. More comments and error
*                  checks added. Bug fix: Call to DSA_OPEN was
*                  missed out !!                                  (SMB)
*      3-Sep-1990: Modified to make use of .FITS structure.       (SMB)
*      1-Oct-1991: Change to GEN_CLEANV.                          (PND)
*     18-Feb-1993: Conform to error strategy                      (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Status :
      INTEGER STATUS
*    Input :
*    Local Constants :
      INTEGER MAXDIM
      PARAMETER ( MAXDIM = 3 )
*    Local variables :
      LOGICAL ERRORS                     ! T if data array has associated errors
      INTEGER NDIM                       ! the dimensions of the data array
      INTEGER DIMS( MAXDIM )             !                "
      INTEGER ADDRESS                    ! DSA mapping stuff
      INTEGER DATA_SLOT                  !  for data
      INTEGER DATA_DATA                  !      "
      INTEGER VAR_SLOT                   !  for variances
      INTEGER DATA_VAR                   !      "
      INTEGER QUAL_SLOT                  !  for quality
      INTEGER DATA_QUAL                  !      "
      INTEGER NELM                       ! number of elements in array
      REAL CUT                           ! S/N cut below which quality
*                                              is set to bad
      REAL TLOW                          ! Data value below which quality
*                                              is set to bad
      CHARACTER*80 DATA                  ! the name of the data file
*    Internal References :
*    Local data :
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the name of the data structure to be "cleaned"
      CALL PAR_GET0C( 'DATA', DATA, STATUS )

*   Get the value of CUT and TLOW
      CALL PAR_GET0R( 'CUT', CUT, STATUS )
      CALL PAR_GET0R( 'TLOW', TLOW, STATUS )

*   Check this has worked.
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Open DSA.
         CALL DSA_OPEN( STATUS )

*      Open the data structure and check it has an error array.
         CALL RED4_CHECK_INPUT( DATA, STATUS )
         CALL DSA_NAMED_INPUT ('DATA', DATA, STATUS)
         CALL DSA_SEEK_ERRORS ('DATA', ERRORS, STATUS)

*      Check this has worked.
         IF ( STATUS .EQ. ADAM__OK ) THEN

*         Make sure the data structure has an error array, otherwise
*         it will be impossible to calculate the signal-to-noise.
            IF ( .NOT. ERRORS ) THEN

               CALL MSG_OUT( ' ', 'RED4_CLEAN_OBS - Data structure '/
     :           /'does not have an error array.', STATUS )
            ELSE

*            Find the size of the data array.
               CALL DSA_DATA_SIZE ('DATA', MAXDIM, NDIM, DIMS,
     :            NELM, STATUS)

*            Tell DSA that a quality array will be used to flag bad values.
               CALL DSA_USE_QUALITY ('DATA', STATUS)

*            Map in the data, variance and quality arrays.
               CALL DSA_MAP_DATA ('DATA', 'UPDATE', 'FLOAT',
     :            ADDRESS, DATA_SLOT, STATUS)
               DATA_DATA = ADDRESS
               CALL DSA_MAP_VARIANCE ('DATA', 'UPDATE', 'FLOAT',
     :            ADDRESS, VAR_SLOT, STATUS)
               DATA_VAR = ADDRESS
               CALL DSA_MAP_QUALITY ('DATA', 'UPDATE', 'BYTE',
     :           ADDRESS, QUAL_SLOT, STATUS)
               DATA_QUAL = ADDRESS

*           If all the arrays have been mapped successfully, call
*           the GENE routine to reject data with an unacceptable
*           signal to noise ratio.
               IF ( STATUS .EQ. ADAM__OK ) THEN

                  CALL GEN_CLEANV( NELM, %val(DATA_DATA),
     :              %val(DATA_VAR), %val(DATA_QUAL), CUT,
     :              TLOW, .TRUE., .FALSE., 0.0 )
               ENDIF

*            If everything has been successful, record the fact that
*            this observation has been CLEANed in the .FITS
*            structure of the observation file, and record the S/N
*            cut and TLOW used.
               CALL DSA_PUT_FITS_C( 'DATA', 'COMMENT',
     :           'This observation has a S/N cut applied', ' ', STATUS )

               CALL DSA_PUT_FITS_F( 'DATA', 'SNCUT', CUT, ' ', STATUS )

               CALL DSA_PUT_FITS_F( 'DATA', 'TLOW', TLOW, ' ', STATUS )
            END IF
         ENDIF

*      Close DSA. This will tidy up all the workspace mapped above
         CALL DSA_CLOSE( STATUS )
      ELSE

*      The input parameters could not be obtained.
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_CLEAN_OBS: '/
     :     /'Error obtaining input parameters', STATUS )
      END IF

      END
