C+
C                     D S A _ R E S H A P E _ A R R A Y
C
C  Routine name:
C     DSA_RESHAPE_ARRAY
C
C  Function:
C     Reshapes a named existing data array to specified dimensions
C
C  Description:
C     This routine creates changes the dimensions of a data array.
C     This routine understands about structured array types and
C     can handle them.  The array should already exist.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_RESHAPE_ARRAY (ENV,NAME,NDIM,DIMS,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) ENV         (Fixed string,descr) The name of the environment
C                     for the array - that is, ENV is the DTA_ system
C                     name of the structure that is to contain the new
C                     array.  This can be blank, with NAME containing the
C                     whole name of the array.
C     (>) NAME        (Fixed string,descr) The name of the array to
C                     be created.  If ENV is non-blank, this should just be
C                     the object name - it should not include the environment
C                     part.  If ENV is blank, NAME should be the whole name
C                     of the array, although without any dimension information.
C     (>) NDIM        (Integer,ref) The new number of dimensions for the array.
C     (>) DIMS        (Integer array,ref) The new dimensions for the array.
C     (!) STATUS      (Integer,ref) Status code.  If bad status is passed
C                     to it, this routine returns immediately.
C
C  External variables used:
C     Only common variables internal to the DSA system
C
C  External subroutines / functions used:
C     DTA_ERROR, DTA_RNVAR, DTA_TYVAR, DTA_STRUC, DSA_WRNAME, DSA_WRUSER,
C     DSA_ENCDIM, ICH_LEN
C
C  Prior requirements:
C     DSA_OPEN should have been called to initialise the system.  The
C     environment for the array should already exist.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C
C  Note: This is an DSA internal routine and should not be called
C     directly from outside the DSA system.  At present it can only
C     handle a limitied number of structured array types.
C-
C  Subroutine / function details:
C     DTA_ERROR       Get text for DTA error code
C     DTA_RNVAR       Change name or shape of named data object
C     DTA_TYVAR       Get type of named data object
C     DTA_STRUC       See if named data object is a structure.
C     DSA_WRNAME      Output full name of data object to user
C     DSA_WRUSER      Output message to user
C     DSA_ENCDIM      Encode dimension information in a string
C     ICH_LEN         Position of last non-blank char in string
C
C  Common variable details:
C     (<) DTA_CODE    (Integer) Last DTA system error code
C
C  History:
C     18th Jan 1990  Original version.  KS / AAO.
C     26th Apr 1990  Modified to support some SGP38 structured arrays.  KS/AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C     3rd  Sep 1992     Call DSA_ENCDIM instead of FIG_ENCDIM. HME/UoE
C+
      SUBROUTINE DSA_RESHAPE_ARRAY (ENV,NAME,NDIM,DIMS,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NDIM, DIMS(*), STATUS
      CHARACTER*(*) ENV, NAME
C
C     Functions used
C
      INTEGER ICH_LEN
C
C     DSA system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     DSA system common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      CHARACTER ARRAY_NAME*80       ! Name of array to reshape
      INTEGER   DTA_STATUS          ! Status from DTA routines
      INTEGER   IPT                 ! Pointer to dimension string
      LOGICAL   KNOWN               ! True if structured type is known to us
      INTEGER   LENAME              ! Length of ARRAY_NAME excluding dimensions
      INTEGER   LENGTH              ! Length of OBJECT
      CHARACTER OBJECT*80           ! Generated object name
      CHARACTER STRING*64           ! Used for error text & dimensions
      LOGICAL   STRUCT              ! Indicates array is structured
      CHARACTER TYPE*16             ! Array type
      CHARACTER VARIANT*16          ! Structure variant type - ignored
C
C     If bad status passed, return immediately
C
      IF (STATUS.NE.0) RETURN
C
C     First create the name of the array.
C
      IF (ENV.EQ.' ') THEN
         OBJECT=NAME
      ELSE
         OBJECT=ENV(:ICH_LEN(ENV))//NAME
      END IF
      LENGTH=ICH_LEN(OBJECT)
C
C     Now see if this is a structured array or a primitive. This also
C     serves to check that it really does already exist.
C
      CALL DTA_STRUC (OBJECT(:LENGTH),STRUCT,DTA_STATUS)
      IF (DTA_STATUS.EQ.0) THEN
C
C        Array exists.
C
         IF (STRUCT) THEN
C
C           It's a structure.  So we have to see if it's a structured type
C           that we know about.  We also get the name of the actual array
C           in the structure.
C
            CALL DTA_TYVAR (OBJECT(:LENGTH),TYPE,DTA_STATUS)
            IF (DTA_STATUS.EQ.0) THEN
               CALL DSA__STRUCT_ARRAY (OBJECT,LENGTH,TYPE,VARIANT,
     :                                  ARRAY_NAME,LENAME,KNOWN,STATUS)
               IF (.NOT.KNOWN) THEN
                  CALL DSA_WRUSER ('Error attempting to change the '//
     :                                               'dimensions of ')
                  CALL DSA_WRNAME(OBJECT)
                  CALL DSA_WRUSER (
     :                     '.  It is a structured array of type "')
                  CALL DSA_WRUSER (TYPE(:ICH_LEN(TYPE)))
                  CALL DSA_WRUSER ('" which this implementation of '//
     :                   'the structure routines cannot handle. ')
                  CALL DSA_WRFLUSH
                  STATUS=DSA__INVTYP
               END IF
            END IF
         ELSE
            ARRAY_NAME=OBJECT
            LENAME=LENGTH
         END IF
C
C        We now have ARRAY_NAME(:LENAME) giving the name of the actual array
C        that has to be reshaped.  We add the dimension information to it
C        and reshape it.  STATUS non-zero indicates that we couldn't handle
C        the type.  The only major complication comes from having to support
C        the type 'COMPLEX_ARRAY', which has two arrays which have to be
C        reshaped individually.  We probably ought to layer this slightly
C        better, but we simply allow this routine to know what the name of
C        the imaginary array will be in this case.  Note that just changing
C        the dimensions of the main array is fine for the `scaled' array
C        types - it won't do for 'sparse' arrays, when the time comes to
C        support them.
C
         IF (STATUS.EQ.0) THEN
            IPT=LENAME+1
            IF (NDIM.GT.0) CALL DSA_ENCDIM(ARRAY_NAME,NDIM,DIMS,IPT)
            CALL DTA_RNVAR (ARRAY_NAME(:LENAME),ARRAY_NAME,DTA_STATUS)
            IF (DTA_STATUS.EQ.0) THEN
               IF (TYPE.EQ.'COMPLEX_ARRAY') THEN
                  ARRAY_NAME=OBJECT(:LENGTH)//'.IMAGINARY'
                  IPT=LENGTH+11
                  IF (NDIM.GT.0) THEN
                     CALL DSA_ENCDIM(ARRAY_NAME,NDIM,DIMS,IPT)
                  END IF
                  CALL DTA_RNVAR (ARRAY_NAME(:LENAME),ARRAY_NAME,
     :                                                      DTA_STATUS)
               END IF
            END IF
         END IF
      END IF
C
C     Well, did it work?  STATUS non-zero => an error we've already
C     reported, DTA_STATUS zero => all OK, DTA_STATUS non-zero => some
C     DTA system error.
C
      IF ((STATUS.EQ.0).AND.(DTA_STATUS.NE.0)) THEN
         CALL DSA_WRUSER('Unable to reshape the data array "')
         CALL DSA_WRUSER(NAME(:ICH_LEN(NAME)))
         CALL DSA_WRUSER('" to dimensions ')
         IPT=1
         CALL DSA_ENCDIM(STRING,NDIM,DIMS,IPT)
         CALL DSA_WRUSER(STRING(:IPT))
         IF (ENV.NE.' ') THEN
            CALL DSA_WRUSER(' in the structure ')
            CALL DSA_WRNAME(ENV)
         END IF
         CALL DSA_WRNAME('. ')
         CALL DTA_ERROR(DTA_STATUS,STRING)
         CALL DSA_WRUSER(STRING(:ICH_LEN(STRING)))
         CALL DSA_WRUSER('. ')
         CALL DSA_WRFLUSH
         DTA_CODE=DTA_STATUS
         STATUS=DSA__DTAERR
      END IF
C
      END
