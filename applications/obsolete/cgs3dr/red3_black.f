C+
      SUBROUTINE RED3_BLACK (STATUS)
C
C     R E D 3 _ B L A C K
C
C     Creates a 1D spectrum corresponding to a black body of given
C     temperature, normalised to to a given flux at a given reference
C     wavelength. The name of a data spectrum is also required to provide
C     a wavelength grid on which to put the black body spectrum. The black
C     body spectrum is created with error and data quality arrays, containing
C     zeroes.
C
C     Only produces F_lambda spectra
C
C     Command parameter -
C
C     TEMPLATE (>) Name of structure containing 1D spectrum to provide a
C                  template wavelength grid (wavelengths in microns)
C
C     BB_TEMP  (>) [REAL] Temperature of required spectrum
C
C     REFWAVE  (>) [REAL] Wavelength at which the spectrum is to be normalised
C                  (microns)
C     REFFLUX  (>) [REAL] Flux at normalisation wavelength
C
C     OUTPUT   (>) Name of output file to receive black body spectrum
C
C     Author: J.Lightfoot (REVAD::JFL) adapted from routine by Alan Bridger
C     removed adamdefns, adamerrs for unix porting (KK) 30-Nov-1995
*     27-Feb-96: rename from red4_
C-
C
      IMPLICIT  NONE
C
C    ADAM stuff
C
      INCLUDE 'SAE_PAR'

C
C    Input parameters
C
      INTEGER  STATUS
C
C    Local variables
C
      LOGICAL
     :    EXIST                 ! Is AXIS1 array present in template file?
      INTEGER
     :    NDIM,                 ! Number of dimensions of axis1 array
     :    DIMS (11),            ! dimensions of axis1 array
     :    NELM,                 ! number of elements in axis1 array
     :    ADDRESS,              ! DSA pointer stuff
     :    A1_SLOT,              ! input x-axis
     :    A1_PTR,               !   "
     :    D_SLOT,               ! output data
     :    D_PTR,                !   "
     :    V_SLOT,               ! output variances
     :    V_PTR,                !   "
     :    Q_SLOT,               ! output quality
     :    Q_PTR,                !   "
     :    OUT_A1_SLOT,          ! output x-axis
     :    OUT_A1_PTR            !   "
      REAL
     :    TEMPERATURE,          ! temperature of black body spectrum
     :    REFWAVE,              ! reference wavelength for normalisation
     :    REFFLUX               ! flux at reference wavelength
      CHARACTER
     :    TEMPLATE*80,          ! name of file with template wavelength axis
     :    OUTPUT*80,            ! name of file to contain output BB spectrum
     :    CHAR_ARRAY(2)*32      ! data and axis labels

      IF ( STATUS .NE. SAI__OK) RETURN
C
C    Initial settings
C
      CALL DSA_OPEN (STATUS)
C
C    open the wavelength scale file, check that it has an axis1 array,
C    find its size and map it in
C
      CALL PAR_GET0C ('TEMPLATE', TEMPLATE, STATUS)
      CALL DSA_NAMED_INPUT ('TEMPLATE', TEMPLATE, STATUS)
      CALL DSA_SEEK_AXIS ('TEMPLATE', 1, EXIST, STATUS)
      IF (.NOT. EXIST) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL DSA_WRUSER ('Template file does not have an '//
     :         'axis1 array.\N')
            STATUS = SAI__ERROR
         ENDIF
      ENDIF
      CALL DSA_AXIS_SIZE ('TEMPLATE', 1, 10, NDIM, DIMS, NELM, STATUS)
      IF (NDIM .NE. 1) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL DSA_WRUSER ('Template file does not have 1-D '//
     :         'axis1 array.\N')
            STATUS = SAI__ERROR
         ENDIF
      ENDIF
      CALL DSA_MAP_AXIS_DATA ('TEMPLATE', 1, 'READ', 'FLOAT',
     :   ADDRESS, A1_SLOT, STATUS)
      A1_PTR = ADDRESS
C
C    read temperature, reference wavelength, reference flux
C
      CALL PAR_GET0R ('BB_TEMP', TEMPERATURE, STATUS)
      CALL PAR_GET0R ('REFWAVE', REFWAVE, STATUS)
      CALL PAR_GET0R ('REFFLUX', REFFLUX, STATUS)
C
C    open simple output file and map in its arrays
C
      CALL PAR_GET0C ('OUTPUT', OUTPUT, STATUS)
      CALL DSA_NAMED_OUTPUT ('OUTPUT', OUTPUT, ' ', 0, 0, STATUS)
      CALL DSA_SIMPLE_OUTPUT ('OUTPUT', 'D,A,Q,E', 'FLOAT', 1,
     :   DIMS(1), STATUS)
      CALL DSA_USE_QUALITY ('OUTPUT', STATUS)
      CALL DSA_MAP_DATA ('OUTPUT', 'WRITE', 'FLOAT', ADDRESS,
     :   D_SLOT, STATUS)
      D_PTR = ADDRESS
      CALL DSA_MAP_VARIANCE ('OUTPUT', 'WRITE', 'FLOAT', ADDRESS,
     :   V_SLOT, STATUS)
      V_PTR = ADDRESS
      CALL DSA_MAP_QUALITY ('OUTPUT', 'WRITE', 'BYTE', ADDRESS,
     :   Q_SLOT, STATUS)
      Q_PTR = ADDRESS
      CALL DSA_MAP_AXIS_DATA ('OUTPUT', 1, 'WRITE', 'FLOAT', ADDRESS,
     :   OUT_A1_SLOT, STATUS)
      OUT_A1_PTR = ADDRESS
C
C    set the data and axes labels
C
      CHAR_ARRAY (2) = 'Normalised Black-Body'
      CHAR_ARRAY (1) = ' '
      CALL DSA_SET_DATA_INFO ('OUTPUT', 2, CHAR_ARRAY, 0, 0, STATUS)
      CHAR_ARRAY (2) = 'Wavelength'
      CHAR_ARRAY (1) = 'microns'
      CALL DSA_SET_AXIS_INFO ('OUTPUT', 1, 2, CHAR_ARRAY, 0, 0,
     :   STATUS)
      CHAR_ARRAY (2) = 'Flux'
      CHAR_ARRAY (1) = ' '
      CALL DSA_SET_AXIS_INFO ('OUTPUT', 2, 2, CHAR_ARRAY, 0, 0,
     :   STATUS)
C
C    copy the wavelength array
C
      IF (STATUS .EQ. SAI__OK) THEN
         CALL GEN_MOVE (4*DIMS(1), %VAL(A1_PTR), %VAL(OUT_A1_PTR))
      ENDIF
C
C    call routine to create black body
C
      IF (STATUS .EQ. SAI__OK) THEN
         CALL GEN_BBSPC (REFWAVE, TEMPERATURE, %VAL(OUT_A1_PTR),
     :      DIMS(1), %VAL(D_PTR))
      ENDIF
C
C    multiply the BB by the reference flux
C
      IF (STATUS .EQ. SAI__OK) THEN
         CALL GEN_MULCAF (%VAL(D_PTR), DIMS(1), REFFLUX, %VAL(D_PTR))
      ENDIF
C
C    fill error and quality arrays with zeros and good quality
C
      IF (STATUS .EQ. SAI__OK) THEN
         CALL GEN_CFILL (1, DIMS(1), 0.0, %VAL(V_PTR))
         CALL GEN_FILL (DIMS(1), 0, %VAL(Q_PTR))
      ENDIF
C
C    write descriptive items to FITS objects
C
      CALL DSA_SET_OBJECT ('OUTPUT', 'Normalised Black-Body', STATUS)
      CALL DSA_PUT_FITS_F ('OUTPUT', 'BBTEMP', TEMPERATURE,
     :   'Temperature of black-body', STATUS)
      CALL DSA_PUT_FITS_F ('OUTPUT', 'REFWAV', REFWAVE,
     :   'Reference wavelength (microns)', STATUS)

      CALL DSA_CLOSE (STATUS)

      END
