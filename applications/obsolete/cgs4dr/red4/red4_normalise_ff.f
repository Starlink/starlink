*+ RED4_NORMALISE_FF - Normalise a flat field observation.
      SUBROUTINE RED4_NORMALISE_FF (STATUS)
*    Description :
*     This subroutine normalises a flat-field observation and writes
*     the result to a new observation.
*    Invocation :
*     CALL RED4_NORMALISE_FF (STATUS)
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*    Method :
*     <description of how the subroutine works>
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*      2-May-1990: Original version.                               (SMB)
*      3-May-1990: Modified to include error handling.             (SMB)
*     10-Jul-1990: FF_NORM changed to NORM_METHOD.                 (SMB)
*      3-Sep-1990: Modified to make use of FITS structure.         (SMB)
*     24-Feb-1991: Some error reporting mistakes fixed, which would
*                  have made this routine fail under ADAM V1.9.    (SMB)
*     31-Jul-1991: Smoothing added as an alternate method of
*                  normalising from polynomial fitting.            (SMB)
*     22-Feb-1993: Conform to error strategy                       (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Status :
      INTEGER STATUS
*    External references :
      CHARACTER*2 GEN_NTH             ! Figaro "Nth" determination function
*                                         (i.e. 1st, 2nd, 3rd, 4th ...)
*    Global variables :
*    Local Constants :
      INTEGER MAXDIM
      PARAMETER ( MAXDIM = 3 )
*    Local variables :
      CHARACTER*80
     :  INPUT,                        ! Name of input observation
     :  OUTPUT                        ! Name of output observation
      CHARACTER*20
     :  NORM_METHOD                   ! Normalisation method
      INTEGER
     :  ORDER,                        ! Polynomial order to be used
     :  BOXSIZE,                      ! Smooth box size in pixels.
     :  NDIM,                         ! Number of dimensions
     :  DIMS( MAXDIM ),               ! Dimensions
     :  NELM,                         ! Number of elements
     :  ADDRESS,                      ! Address for mapped data and workspace
     :  DATA_SLOT,                    ! Slot for mapped data array
     :  DATA_PTR,                     ! Address of mapped data array
     :  VAR_SLOT,                     ! Slot for mapped variance array
     :  VAR_PTR,                      ! Address of mapped variance array
     :  QUAL_SLOT,                    ! Slot for mapped quality array
     :  QUAL_PTR                      ! Address of mapped quality array
      INTEGER
     :  SPECTRUM_SLOT,                   ! Slot for spectrum workspace
     :  SPECTRUM_PTR,                    ! Address of spectrum workspace
     :  SUM_SLOT,                        ! Slot for sum workspace
     :  SUM_PTR,                         ! Address of sum workspace
     :  SQ_SLOT,                         ! Slot for spectrum quality
     :  SQ_PTR,                          ! Address for spectrum quality
     :  SM_SLOT,                         ! Slot for smoothed spectrum
     :  SM_PTR,                          ! Address for smoothed spectrum
     :  SMQ_SLOT,                        ! Slot for smoothed spectrum quality
     :  SMQ_PTR,                         ! Address for smoothed spectrum quality
     :  X_SLOT,                          ! Slot for X workspace
     :  X_PTR,                           ! Address of X workspace
     :  Y_SLOT,                          ! Slot for Y workspace
     :  Y_PTR                            ! Address of Y workspace
*    Internal References :
*    Local data :
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the name of the input observation
      CALL PAR_GET0C( 'INPUT', INPUT, STATUS )

*   Use this as a default for the output structure
*   and then obtain the name of the output structure.
      CALL PAR_DEF0C( 'OUTPUT', INPUT, STATUS )
      CALL PAR_GET0C( 'OUTPUT', OUTPUT, STATUS )

*   Obtain the normalisation method required
      CALL PAR_GET0C( 'NORM_METHOD', NORM_METHOD, STATUS )

*   Check the parameters has been obtained successfully.
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Check the normalisation method required and obtain any
*      parameters relevant to each method.
         IF ( NORM_METHOD .EQ. 'POLYFIT' ) THEN

*         Polynomial fit. Obtain the order required.
*         Report an error if this could not be obtained.
            CALL PAR_GET0I( 'ORDER', ORDER, STATUS )

            IF ( STATUS .NE. ADAM__OK ) THEN

               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_NORMALISE_FF: '/
     :            /'Error obtaining %ORDER parameter', STATUS )
            END IF
         ELSE IF ( NORM_METHOD .EQ. 'SMOOTH' ) THEN

*         Smoothing. Obtain the box size required.
*         Report an error if this could not be obtained.
            CALL PAR_GET0I( 'BOXSIZE', BOXSIZE, STATUS )

            IF ( STATUS .NE. ADAM__OK ) THEN

               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_NORMALISE_FF: '/
     :           /'Error obtaining %BOXSIZE', STATUS )
            END IF
         ELSE

*         Unknown normalisation method.
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'METHOD', NORM_METHOD )
            CALL ERR_REP( ' ', 'RED4_NORMALISE_FF: '/
     :        /'Unknown normalisation method ^METHOD', STATUS )
         END IF

*      Check there have been no errors so far.
         IF ( STATUS .EQ. ADAM__OK ) THEN

*         Open DSA.
            CALL DSA_OPEN( STATUS )

*         Attempt to open the input structure, and indicate
*         that a quality array will be used for handling bad pixels.
            CALL RED4_CHECK_INPUT( INPUT, STATUS )
            CALL DSA_NAMED_INPUT( 'INPUT', INPUT, STATUS )
            CALL DSA_USE_QUALITY( 'INPUT', STATUS )

*         Attempt to open the output structure, using the input
*         structure as a template. (The flags given will force
*         creation of a new data structure and copying of existing
*         data arrays.)
            CALL DSA_NAMED_OUTPUT( 'OUTPUT', OUTPUT, 'INPUT',
     :        0, 1, STATUS )
            CALL DSA_USE_QUALITY( 'OUTPUT', STATUS )

*         Map the data array, variance array and quality array
*         and obtain their size.
            CALL DSA_MAP_DATA( 'OUTPUT', 'UPDATE', 'FLOAT', ADDRESS,
     :        DATA_SLOT,  STATUS )
            DATA_PTR = ADDRESS
            CALL DSA_MAP_VARIANCE( 'OUTPUT', 'UPDATE', 'FLOAT',
     :        ADDRESS, VAR_SLOT, STATUS )
            VAR_PTR = ADDRESS
            CALL DSA_MAP_QUALITY( 'OUTPUT', 'UPDATE', 'BYTE',
     :        ADDRESS, QUAL_SLOT,  STATUS )
            QUAL_PTR = ADDRESS
            CALL DSA_DATA_SIZE( 'OUTPUT', MAXDIM, NDIM, DIMS, NELM,
     :        STATUS )

*         Obtain some workspace for the normalisation routine
            CALL DSA_GET_WORK_ARRAY( DIMS(1), 'FLOAT', ADDRESS,
     :        SPECTRUM_SLOT, STATUS )
            SPECTRUM_PTR = ADDRESS
            CALL DSA_GET_WORK_ARRAY( DIMS(1), 'INT', ADDRESS,
     :        SUM_SLOT, STATUS )
            SUM_PTR = ADDRESS

*         Normalise the data using the method specified.
*         (Note that if there have been any errors DSA will have already
*         reported them).
            IF ( NORM_METHOD .EQ. 'POLYFIT' ) THEN

               CALL DSA_GET_WORK_ARRAY( DIMS(1), 'FLOAT',
     :           ADDRESS, X_SLOT, STATUS )
               X_PTR = ADDRESS
               CALL DSA_GET_WORK_ARRAY( DIMS(1), 'FLOAT',
     :           ADDRESS, Y_SLOT, STATUS )
               Y_PTR = ADDRESS

               CALL MSG_SETI( 'ORDER', ORDER )
               CALL MSG_SETC( 'NTH', GEN_NTH(ORDER) )
               CALL MSG_OUT( ' ', 'Normalising with '/
     :           /'^ORDER^NTH order polynomial.', STATUS )

*            Normalise the reduced observation array using a polynomial fit.
*            (The external status check is to prevent the routine crashing
*            with an "adjustable array dimension error" if any of the arrays
*            are not mapped).
               IF ( STATUS .EQ. ADAM__OK ) THEN

                  CALL RED4_NORMALISE_FIT( DIMS(1), DIMS(2), ORDER,
     :              %val(DATA_PTR), %val(VAR_PTR), %val(QUAL_PTR),
     :              %val(SPECTRUM_PTR), %val(SUM_PTR),
     :              %val(X_PTR), %val(Y_PTR), STATUS )
               END IF
            ELSE IF ( NORM_METHOD .EQ. 'SMOOTH' ) THEN

               CALL DSA_GET_WORK_ARRAY( DIMS(1), 'BYTE',
     :           ADDRESS, SQ_SLOT, STATUS )
               SQ_PTR = ADDRESS
               CALL DSA_GET_WORK_ARRAY( DIMS(1), 'FLOAT',
     :           ADDRESS, SM_SLOT, STATUS )
               SM_PTR = ADDRESS
               CALL DSA_GET_WORK_ARRAY( DIMS(1), 'BYTE',
     :           ADDRESS, SMQ_SLOT, STATUS )
               SMQ_PTR = ADDRESS
               CALL MSG_SETI( 'BOXSIZE', BOXSIZE )
               CALL MSG_OUT( ' ', 'Normalising with '/
     :           /'^BOXSIZE-pixel box smooth.', STATUS )

*            Normalise the reduced observation array using a box smooth.
*            (The external status check is to prevent the routine crashing
*            with an "adjustable array dimension error" if any of the arrays
*            are not mapped).
               IF ( STATUS .EQ. ADAM__OK ) THEN

                  CALL RED4_NORMALISE_SMOOTH( DIMS(1), DIMS(2),
     :              BOXSIZE,
     :              %val(DATA_PTR), %val(VAR_PTR), %val(QUAL_PTR),
     :              %val(SPECTRUM_PTR), %val(SUM_PTR), %VAL(SQ_PTR),
     :              %val(SM_PTR), %val(SMQ_PTR), STATUS )
               END IF
            END IF

*         If the normalisation has worked, changed the data units
*         to 'Normalised number' and set the NORMALIS item in the
*         FITS structure.
            CALL DSA_SET_DATA_INFO( 'OUTPUT', 1, 'Normalised number',
     :        0, 0.0D0, STATUS)
            CALL DSA_PUT_FITS_C( 'OUTPUT', 'NORMALIS', 'Yes', ' ',
     :        STATUS )

*         Close DSA. (This routine will unmap all the workspace mapped
*         above).
            CALL DSA_CLOSE( STATUS )
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_NORMALISE_FF: '/
     :     /'Error obtaining %INPUT, %OUTPUT and '/
     :     /'%NORM_METHOD parameters', STATUS )
      END IF

      END
