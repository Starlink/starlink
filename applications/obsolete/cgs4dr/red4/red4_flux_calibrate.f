*+  RED4_FLUX_CALIBRATE, flux calibrates a standard divided spectrum
      SUBROUTINE RED4_FLUX_CALIBRATE( STATUS )
*    Description :
*     This routine flux calibrates a suitable standard divided image.
*     CALL RED4_FLUX_CALIBRATE( STATUS )
*    Parameters :
*     STATUS    = INTEGER( UPDATE )
*           Global ADAM status.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     P N Daly (JACH::PND)
*    History :
*      2-Sep-1992: Original version.                                 (PND)
*     29-Sep-1992: USE DEXPTIME rather than EXPOSED!                 (PND)
*     19-Feb-1993: Conform to error strategy                         (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'             ! Contains SAI__ERROR
      INCLUDE 'PRM_PAR'
*    Status :
      INTEGER STATUS
      INTEGER DSA_STATUS
*    External references :
      INTEGER CHR_LEN               ! Character length determining function
      INTEGER DSA_TYPESIZE          ! DSA type size inquiry function
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'     ! RED4 common block
*    Local Constants :
      INTEGER NINFO                 ! Number of information items
      PARAMETER ( NINFO = 2 )
      INTEGER DSA__OK               ! Kosher DS status
      PARAMETER ( DSA__OK = 0 )
*    Local variables :
      INTEGER
     :  FLOATSIZE,                  ! Bytes per element of 'FLOAT' array
     :  CLEN,                       ! Non-blank length of character string
     :  GLEN,                       ! Non-blank length of character string
     :  CPOS,                       ! Position in character string
     :  NDIM,                       ! No. of. dimensions in data arrays
     :  DIMS( RMAXDIM ),            ! Dimensions of group data arrays
     :  NELM,                       ! Number of elements in data arrays
     :  GDATA_SLOT,                 ! Input group data array slot
     :  GDATA_PTR,                  ! Input group data array pointer
     :  GVAR_SLOT,                  ! Input group variance array slot
     :  GVAR_PTR                    ! Input group variance array pointer
      INTEGER
     :  GQUAL_SLOT,                 ! Input group quality array slot
     :  GQUAL_PTR,                  ! Input group quality array pointer
     :  ODATA_SLOT,                 ! Output group data array slot
     :  ODATA_PTR,                  ! Output group data array pointer
     :  OVAR_SLOT,                  ! Output group variance array slot
     :  OVAR_PTR,                   ! Output group variance array pointer
     :  OQUAL_SLOT,                 ! Output group quality array slot
     :  OQUAL_PTR,                  ! Output group quality array pointer
     :  NLOW,                       ! Number of low values in GEN_CLIPF
     :  NHIGH                       ! Number of high values in GEN_CLIPF
      REAL
     :  GRP_EXPOSED,                ! Exposure time of group
     :  STD_EXPOSED,                ! Exposure time of standard
     :  RFLAMBDA,                   ! Reference wavelength of standard
     :  TEFF,                       ! Effective temperature of standard
     :  FLUX,                       ! Input flux
     :  FLAM,                       ! Calculated flux at magnitude
     :  RATIO_EXP                   ! Ratio of exposure times
      CHARACTER*32
     :  LABEL,                      ! Character buffer for data label.
     :  UNIN,                       ! Character buffer for data units.
     :  UNOUT,                      ! Character buffer for data units.
     :  DATA_INFO( NINFO )          ! Data label and units.
      CHARACTER*4
     :  BAND,                       ! Flux band
     :  COMMENT                     ! Null comment for FITS read
      CHARACTER*80
     :  GROUP,                      ! The name of the input group file.
     :  OUTPUT,                     ! The name of the output group file.
     :  STANDARD_NAME               ! The name of the suitable STANDARD
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Obtain all the parameters for this routine
      CALL PAR_GET0C( 'INPUT', GROUP, STATUS )
      CALL RED4_CHECK_INPUT( GROUP, STATUS )
      GLEN = CHR_LEN( GROUP )
      CALL PAR_DEF0C( 'OUTPUT', GROUP(1:GLEN)//'_fc', STATUS )
      CALL PAR_GET0C( 'OUTPUT', OUTPUT, STATUS )
      CALL PAR_GET0R( 'FLUX', FLUX, STATUS )
      CALL PAR_GET0C( 'BAND', BAND, STATUS )
      CALL PAR_GET0C( 'INPUT_UNITS', UNIN, STATUS )
      CALL PAR_GET0C( 'OUTPUT_UNITS', UNOUT, STATUS )

*   Check this has worked.
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Open DSA and check status
         DSA_STATUS = DSA__OK
         CALL DSA_OPEN( DSA_STATUS )
         IF ( DSA_STATUS .NE. DSA__OK ) THEN
             STATUS = SAI__ERROR
             CALL ERR_REP( ' ', 'RED4_FLUX_CALIBRATE: Error opening DSA', STATUS )
         END IF

*      Tell DSA about the input and use it's quality array
         CALL DSA_NAMED_INPUT( 'GRPFILE', GROUP, DSA_STATUS )
         IF ( DSA_STATUS .NE. DSA__OK ) THEN
             STATUS = SAI__ERROR
             CALL MSG_SETC( 'GROUP', GROUP )
             CALL ERR_REP( ' ', 'RED4_FLUX_CALIBRATE: Error opening ^GROUP', STATUS )
         END IF
         CALL DSA_USE_QUALITY( 'GRPFILE', DSA_STATUS )

*     Open the output file
         CALL DSA_NAMED_OUTPUT( 'OUTFILE', OUTPUT, 'GRPFILE', 0, 1, DSA_STATUS )
         IF ( DSA_STATUS .NE. DSA__OK ) THEN
             STATUS = SAI__ERROR
             CALL MSG_SETC( 'OUTPUT', OUTPUT )
             CALL ERR_REP( ' ', 'RED4_FLUX_CALIBRATE: Error opening ^OUTPUT', STATUS )
         END IF

*      Get the floating point unit size
         FLOATSIZE = DSA_TYPESIZE( 'FLOAT', DSA_STATUS )

*      Get the name of the STANDARD used from the group
         CALL DSA_GET_FITS_C( 'GRPFILE', 'STDUSED', 0, STANDARD_NAME, COMMENT, DSA_STATUS )
         IF ( STANDARD_NAME .EQ. ' ' ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'GROUP', GROUP )
            CALL ERR_REP( ' ', 'RED4_FLUX_CALIBRATE: GROUP has not been divided by a standard!', STATUS )
         END IF

*     Open the STANDARD file
         CALL DSA_NAMED_INPUT( 'STDFILE', STANDARD_NAME, DSA_STATUS )
         IF ( DSA_STATUS .NE. DSA__OK ) THEN
             STATUS = SAI__ERROR
             CALL MSG_SETC( 'STANDARD', STANDARD_NAME )
             CALL ERR_REP( ' ', 'RED4_FLUX_CALIBRATE: Error opening ^STANDARD', STATUS )
         END IF

*     Get some FITS items from both group and standard
         CALL DSA_GET_FITS_F( 'GRPFILE', 'DEXPTIME', 0, GRP_EXPOSED, COMMENT, DSA_STATUS )
         CALL DSA_GET_FITS_F( 'STDFILE', 'DEXPTIME', 0, STD_EXPOSED, COMMENT, DSA_STATUS )
         CALL DSA_GET_FITS_F( 'STDFILE', 'RFLAMBDA', 0, RFLAMBDA, COMMENT, DSA_STATUS )
         CALL DSA_GET_FITS_F( 'STDFILE', 'TEFF', 0, TEFF, COMMENT, DSA_STATUS )

*     Tell the user what we've found
         IF ( VERBOSE ) THEN
            CALL MSG_SETR( 'GRP_EXPOSED', GRP_EXPOSED )
            CALL MSG_OUT( ' ', 'The group exposure time is ^GRP_EXPOSED', STATUS )
            CALL MSG_SETR( 'STD_EXPOSED', STD_EXPOSED )
            CALL MSG_OUT( ' ', 'The standard exposure time is ^STD_EXPOSED', STATUS )
            CALL MSG_SETR( 'RFLAMBDA', RFLAMBDA )
            CALL MSG_OUT( ' ', 'Reference wavelength is ^RFLAMBDA microns', STATUS )
            CALL MSG_SETR( 'TEFF', TEFF )
            CALL MSG_OUT( ' ', 'Effective temperature is ^TEFF Kelvin', STATUS )
         END IF

*     Convert flux or magnitude into selected units
         CALL RED4_FLUX_CALIBRATE_2( FLUX, BAND, UNIN, UNOUT, RFLAMBDA, FLAM, STATUS )

*     Take exposure times into account
         RATIO_EXP = GRP_EXPOSED / STD_EXPOSED
         FLAM      = FLAM / RATIO_EXP

         IF ( VERBOSE ) THEN
            CALL MSG_SETR( 'FLAM', FLAM )
            CALL MSG_OUT( ' ', 'The image will be multiplied by ^FLAM', STATUS )
         END IF

*     Has all this worked?
         IF ( (STATUS.EQ.SAI__OK) .AND. (DSA_STATUS.EQ.DSA__OK) ) THEN

*         Obtain the size of the data array in the input group (which
*         will also be the same size as the one in the output group).
            CALL DSA_DATA_SIZE( 'GRPFILE', RMAXDIM, NDIM, DIMS, NELM, DSA_STATUS )

*         Map the data, variance and quality arrays in the input structure.
            CALL DSA_MAP_DATA( 'GRPFILE', 'READ', 'FLOAT', GDATA_PTR, GDATA_SLOT, DSA_STATUS )
            CALL DSA_MAP_VARIANCE( 'GRPFILE', 'READ', 'FLOAT', GVAR_PTR, GVAR_SLOT, DSA_STATUS )
            CALL DSA_MAP_QUALITY( 'GRPFILE', 'READ', 'BYTE', GQUAL_PTR, GQUAL_SLOT, DSA_STATUS )

*         Map the data, quality and variance arrays of output
            CALL DSA_MAP_DATA( 'OUTFILE', 'WRITE', 'FLOAT', ODATA_PTR, ODATA_SLOT, DSA_STATUS )
            CALL DSA_MAP_QUALITY( 'OUTFILE', 'WRITE', 'BYTE', OQUAL_PTR, OQUAL_SLOT, DSA_STATUS )
            CALL DSA_MAP_VARIANCE( 'OUTFILE', 'WRITE', 'FLOAT', OVAR_PTR, OVAR_SLOT, DSA_STATUS )

*         Initialise the variance array to zero. DSA occasionally
*         maps an array full of junk, and this can crash DSA_CLOSE
*         if the array contains negative values.
            IF ( DSA_STATUS .EQ. DSA__OK ) CALL GEN_FILL( FLOATSIZE*NELM, 0, %val(OVAR_PTR) )

*      Check everything has worked so far.
            IF ( (STATUS.EQ.SAI__OK) .AND. (DSA_STATUS.EQ.DSA__OK) ) THEN

*            Do the multiplication propagating variance
               CALL GEN_MULCAFV( %val(GDATA_PTR), NELM, FLAM,
     :            %val(ODATA_PTR), %val(GQUAL_PTR), %val(OQUAL_PTR),
     :            %val(GVAR_PTR), %val(OVAR_PTR), .TRUE., .FALSE.,
     :            0.0, .TRUE. )

*            Clip the variance array to ensure there are no negative
*            values produced by rounding errors. (These will crash
*            DSA_CLOSE when an attempt is made to square root the
*            variance array).
               CALL GEN_CLIPF( %val(OVAR_PTR), NELM, 0.0, VAL__MAXR, NLOW, NHIGH, %val(OVAR_PTR) )

*            Convert the data label and units in the output file
*            to <Current-label>/STANDARD and flux units.
               DATA_INFO(1) = UNOUT
               LABEL = OUTPUT
               CLEN = MAX( 1, CHR_LEN( LABEL ) )
               DATA_INFO(2) = ' '
               CALL CHR_PUTC( LABEL(1:CLEN), DATA_INFO(2), CPOS )
               CALL DSA_SET_DATA_INFO( 'OUTFILE', NINFO, DATA_INFO, 0, 0.0D0, DSA_STATUS )
            END IF
         END IF

*      Close DSA. This routine will attempt to tidy up and unmap any
*      workspace mapped above. It should be called even if an error
*      has occurred.
         CALL DSA_CLOSE( DSA_STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_FLUX_CALIBRATE: '/
     :     /'Error obtaining some input parameters', STATUS )
      END IF

      END
