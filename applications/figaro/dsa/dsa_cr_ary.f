C+
C                     D S A _ C R E A T E _ A R R A Y
C
C  Routine name:
C     DSA_CREATE_ARRAY
C
C  Function:
C     Creates a named data array of specified type and dimensions
C
C  Description:
C     This routine creates a data array.  This can be a complicated
C     operation if the array type is not one of the primitive types,
C     and a structure has to be created.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_CREATE_ARRAY (ENV,NAME,TYPE,NDIM,DIMS,STATUS)
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
C     (>) TYPE        (Fixed string, descr) The type of the array to be
C                     created.  This can be a primitive or a structured
C                     type accepted by DSA.  This should be in upper case.
C     (>) NDIM        (Integer,ref) The number of dimensions for the array.
C     (>) DIMS        (Integer array,ref) The dimensions of the array.
C     (!) STATUS      (Integer,ref) Status code.  If bad status is passed
C                     to it, this routine returns immediately.
C
C  External variables used:
C     Only common variables internal to the DSA system
C
C  External subroutines / functions used:
C     DTA_CRNAM, DTA_CRVAR, DTA_WRVARC, DSA_TYPESIZE, DSA_WRNAME, DSA_WRUSER
C     DSA_REF_SLOT, DSA__TYPE_DETAILS
C
C  Prior requirements:
C     DSA_OPEN should have been called to initialise the system.  The
C     environment for the array should already exist.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 8th February 1995.
C
C  Note: This is an DSA internal routine and should not be called
C     directly from outside the DSA system.  At present it only
C     supports a limited number of the structured array types defined
C     in Starlink's SGP 38 paper.
C-
C  Subroutine / function details:
C     DTA_CRNAM       Create name of data object
C     DTA_CRVAR       Create named data object
C     DTA_WRVARC      Write a character string to an object
C     DSA_TYPESIZE    Get element size for a specified type
C     DSA_WRNAME      Output full name of data object to user
C     DSA_WRUSER      Output message to user
C     DSA_REF_SLOT    Look up a reference name in common tables
C     DSA__TYPE_DETAILS Takes apart a DSA type specification
C     DSA_ENCDIM      Encode dimension information in a string
C
C  Common variable details:
C     (<) DTA_CODE    (Integer) Last DTA system error code
C
C  History:
C     7th July 1988  Original version.  KS / AAO.
C     16th Jan 1990  Now allows ENV to be blank - this is easier for
C                    calling routines that get the whole object name
C                    from DSA__ routines.
C     26th Apr 1990  Support for some SGP38 structured arrays added. KS/AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C     3rd  Sep 1992  Call DSA_ENCDIM instead of FIG_ENCDIM. HME/UoE
C     8th  Feb 1995  Comments modified to clarify what happens with a
C                    structured array of type 'SIMPLE'. KS/AAO.
C     25th Jul 1996  Catenations for Linux.  MJCL/Starlink, UCL.
C+
      SUBROUTINE DSA_CREATE_ARRAY (ENV,NAME,TYPE,NDIM,DIMS,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NDIM, DIMS(*), STATUS
      CHARACTER*(*) ENV, NAME, TYPE
C
C     Functions used
C
      INTEGER DSA_TYPESIZE, ICH_LEN
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
      CHARACTER BASIC_TYPE*16       ! DSA type name for structure
      INTEGER   BYTES               ! Size of data elements
      CHARACTER DATA_TYPE*16        ! DTA type for main data array(s)
      INTEGER   DTA_STATUS          ! Status from DTA routines
      INTEGER   IDOT                ! Position of '.', if any in REF_NAME
      INTEGER   IPT                 ! Pointer to dimension string
      LOGICAL   KNOWN               ! True if type is known
      CHARACTER OBJECT*80           ! Generated object name
      CHARACTER REF_NAME*16         ! Reference name for file
      INTEGER   REF_SLOT            ! Reference slot number for file
      CHARACTER STRING*64           ! Used for error text & dimensions
      CHARACTER STRUCT_TYPE*16      ! Type for structure for array
      CHARACTER STRUCTURE*120       ! Full name for environment
      CHARACTER VARIANT*16          ! Variant type for structured array
C
C     If bad status passed, return immediately
C
      IF (STATUS.NE.0) RETURN
C
C     Take the type spec apart and see if it's something we can handle.
C     This handles the syntax of the type spec, but still requires
C     that this routine knows exactly how to handle the various types.
C     To handle the cases where the type interpretation depends on the
C     file structure, we need the reference name and hence the reference
C     slot number for the file, which we have to dig out of NAME and ENV.
C
      IF (ENV.EQ.' ') THEN
         REF_NAME=NAME
      ELSE
         REF_NAME=ENV
      END IF
      IDOT=INDEX(REF_NAME,'.')
      IF (IDOT.GT.0) REF_NAME(IDOT:)=' '
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500     ! Error exit
      CALL DSA__TYPE_DETAILS (REF_SLOT,TYPE,BASIC_TYPE,VARIANT,
     :                                     DATA_TYPE,STRUCT_TYPE,KNOWN)
      IF (.NOT.KNOWN) THEN
         STATUS=DSA__INVTYP
      ELSE IF (STRUCT_TYPE.EQ.' ') THEN
C
C        For a primitive array, create the name, then create the array
C        First, we check that this really is a primitive type.  Note that
C        DTA_CRNAM does not work if ENV or NAME are blank, so we use
C        DSA_ENCDIM in that case.
C
         BYTES=DSA_TYPESIZE(TYPE,STATUS)
         IF (STATUS.EQ.0) THEN
            IF (ENV.NE.' ') THEN
               CALL DTA_CRNAM(ENV,NAME,NDIM,DIMS,OBJECT,DTA_STATUS)
            ELSE
               OBJECT=NAME
               IPT=ICH_LEN(OBJECT)+1
               IF (NDIM.GT.0) CALL DSA_ENCDIM(OBJECT,NDIM,DIMS,IPT)
            END IF
            IF (DTA_STATUS.EQ.0) THEN
               CALL DTA_CRVAR(OBJECT,TYPE,DTA_STATUS)
            END IF
         END IF
C
      ELSE
C
C        For a structured array, create the structure, then the
C        components, one by one.  This code is a bit messy - for a
C        'CSTRUCT' it creates .BSCALE,.BZERO,.DATA; for a 'SCALED'
C        array it creates .VARIANT,.SCALE,.ZERO,.DATA;  for a
C        'COMPLEX' array it creates .VARIANT,.REAL,.IMAGINARY.
C        For a 'SIMPLE' type it creates .VARIANT,.DATA. Those
C        are all the types we know about at present.  Note that this
C        needs to be changed at the same time as DSA__TYPE_DETAILS,
C        if new structured types are added.
C
         IF (ENV.NE.' ') THEN
            CALL DTA_CRNAM(ENV,NAME,0,0,STRUCTURE,DTA_STATUS)
         ELSE
            STRUCTURE=NAME
         END IF
         CALL DTA_CRVAR(STRUCTURE,STRUCT_TYPE,DTA_STATUS)
         IF (DTA_STATUS.EQ.0) THEN
            IF (BASIC_TYPE.NE.'CSTRUCT') THEN
               CALL DTA_CRNAM(STRUCTURE,'VARIANT',1,16,OBJECT,
     :                                                       DTA_STATUS)
               CALL DTA_CRVAR(OBJECT,'CHAR',DTA_STATUS)
               CALL DTA_CRNAM(STRUCTURE,'VARIANT',0,0,OBJECT,DTA_STATUS)
               CALL DTA_WRVARC(OBJECT,16,VARIANT,DTA_STATUS)
            END IF
            IF (BASIC_TYPE.EQ.'CSTRUCT') THEN
               CALL DTA_CRNAM(STRUCTURE,'BSCALE',0,0,OBJECT,DTA_STATUS)
               CALL DTA_CRVAR(OBJECT,'DOUBLE',DTA_STATUS)
               IF (DTA_STATUS.EQ.0) THEN
                  CALL DTA_CRNAM(STRUCTURE,'BZERO',0,0,OBJECT,
     :                                                     DTA_STATUS)
                  CALL DTA_CRVAR(OBJECT,'DOUBLE',DTA_STATUS)
               END IF
            END IF
            IF (BASIC_TYPE.EQ.'SCALED') THEN
               CALL DTA_CRNAM(STRUCTURE,'SCALE',0,0,OBJECT,DTA_STATUS)
               CALL DTA_CRVAR(OBJECT,'DOUBLE',DTA_STATUS)
               IF (DTA_STATUS.EQ.0) THEN
                  CALL DTA_CRNAM(STRUCTURE,'ZERO',0,0,OBJECT,DTA_STATUS)
                  CALL DTA_CRVAR(OBJECT,'DOUBLE',DTA_STATUS)
               END IF
            END IF
            IF (DTA_STATUS.EQ.0) THEN
               IF (BASIC_TYPE.NE.'COMPLEX') THEN
                  CALL DTA_CRNAM(STRUCTURE,'DATA',NDIM,DIMS,OBJECT,
     :                                                     DTA_STATUS)
               ELSE
                  CALL DTA_CRNAM(STRUCTURE,'REAL',NDIM,DIMS,OBJECT,
     :                                                     DTA_STATUS)
               END IF
               IF (DTA_STATUS.EQ.0) THEN
                  CALL DTA_CRVAR(OBJECT,DATA_TYPE,DTA_STATUS)
               END IF
            END IF
            IF ((DTA_STATUS.EQ.0).AND.(BASIC_TYPE.EQ.'COMPLEX')) THEN
               CALL DTA_CRNAM(STRUCTURE,'IMAGINARY',NDIM,DIMS,OBJECT,
     :                                                     DTA_STATUS)
               IF (DTA_STATUS.EQ.0) THEN
                  CALL DTA_CRVAR(OBJECT,DATA_TYPE,DTA_STATUS)
               END IF
            END IF
         END IF
      END IF
C
C     Well, did it work?  STATUS non-zero => invalid type, DTA_STATUS
C     non-zero => DTA system error.
C
  500 CONTINUE
      IF ((STATUS.NE.0).OR.(DTA_STATUS.NE.0)) THEN
         CALL DSA_WRUSER('Unable to create the data array "')
         CALL DSA_WRUSER(NAME(:ICH_LEN(NAME)))
         STRING='" of type "'//TYPE(:ICH_LEN(TYPE))
         CALL DSA_WRUSER(STRING)
         CALL DSA_WRUSER('" and dimensions ')
         IPT=1
         CALL DSA_ENCDIM(STRING,NDIM,DIMS,IPT)
         CALL DSA_WRUSER(STRING(:IPT))
         IF (ENV.NE.' ') THEN
            CALL DSA_WRUSER(' in the structure ')
            CALL DSA_WRNAME(ENV)
         END IF
         CALL DSA_WRNAME('. ')
         IF (STATUS.NE.0) THEN
            CALL DSA_WRUSER('Invalid type')
         ELSE
            CALL DTA_ERROR(DTA_STATUS,STRING)
            CALL DSA_WRUSER(STRING(:ICH_LEN(STRING)))
         END IF
         CALL DSA_WRUSER('. ')
         CALL DSA_WRFLUSH
         DTA_CODE=DTA_STATUS
         STATUS=DSA__DTAERR
      END IF
C
      END
