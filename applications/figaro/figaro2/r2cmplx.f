      SUBROUTINE R2CMPLX
*+
*  Name:
*     R2CMPLX

*  Purpose:
*     Creates a complex data structure from a real data array

*  Language:
*     Fortran 77

*  Type of Module:
*     Figaro application

*  Invocation:
*     CALL R2CMPLX

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Creates a complex data structure from a real data structure.
*     RCMPLX sets the imaginary part of the complex data to
*     zero.  It can be set subsequently using the I2CMPLX command.
*
*     The output data follows the input in structure, except that the
*     data array is of type DOUBLE.  A zero-filled imaginary data array
*     is also created. Any axis structures are retained.

*  Usage:
*     r2cmplx rdata cdata

*  ADAM Parameters:
*     RDATA = FILE (Read)
*        RDATA is the name of an existing file that contains a data
*        array that is to become the real part of the complex data
*        structure that is created by R2CMPLX.
*     CDATA = FILE (Write)
*        CDATA is the name of the complex data structure to be created.
*        Its real part will come from the structure specified as RDATA,
*        and its imaginary part will be set to zero.  If CDATA is the
*        same as RDATA, RDATA will be transformed into a complex
*        structure; otherwise, a new file is created.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     jm: Jo Murray (RAL, Starlink)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     01 Oct 1986 (ks):
*        Original version.
*     21 Feb 1989 (jm):
*        Modified to use DSA_ routines Dynamic memory handling changed
*        to use DYN_ routines
*     08 May 1990 (ks):
*        Re-instated call to FIG_CHCOPY to pad data properly when
*        dimensions changed.  Sorted local variables definitions.
*     29 Sep 1992 (hme):
*        INCLUDE changed. Call PAR_WRUSER rather than DSA_WRUSER. Call
*        VEC_RTOD rather than the discontinued CNV_FMTCNV.
*     20 Apr 1995 (hme):
*        Map imaginary part for write access rather than update. Usually
*        it does not exist beforehand and mapping it for update can
*        crash the application. It appears that the output real part can
*        be mapped for write access as well, since the input data are
*        mapped and copied in any case. It is sometimes necessary to map
*        output real part for write in order to avoid the crash.
*     13 Mar 1996 (hme):
*        Adapt to the FDA library.
*        Map complex in single call.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      IMPLICIT NONE
C
C     Functions
C
      INTEGER   DSA_TYPESIZE! Number of bytes in element of given type
      INTEGER   DYN_ELEMENT ! Dynamic memory element for give address
      LOGICAL   FIG_SCRCHK  ! True if data array is linearly spaced
      LOGICAL   GEN_CHKNSF  ! True if array is values 1..N
      REAL      GEN_ELEMF   ! Gets a value from an array
      CHARACTER GEN_NTH*2   ! Returns 'st','th','rd' etc appropriate to N
      CHARACTER ICH_CI*12   ! Return an integer as a character string.
      INTEGER   ICH_ENCODE  ! Encodes a value into a character string.
      INTEGER   ICH_LEN     ! Position of last non-blank char in string.
C
C     Local variables
C
      INTEGER   ADDRESS      ! Virtual address for data array
      INTEGER   ADDRESS2     ! Virtual address for data array
      INTEGER   APTR         ! Dynamic memory element for axis data
      INTEGER   BDOUBP       ! Number of bytes per item of type 'DOUBLE'
      INTEGER   BFLOAT       ! Number of bytes per item of type 'FLOAT'
      INTEGER   BYTES        ! Bytes required for an array
      LOGICAL   CHANGE       ! True if change of shape is required
      INTEGER   COPYDATA     ! Indicates data arrays to be copied to new file
      INTEGER   DIMS(10)     ! Image dimensions
      INTEGER   DIMS0(10)    ! Original data dimensions
      LOGICAL   EXIST        ! Used to check for existence of axis structures
      INTEGER   I            ! Do loop variable
      INTEGER   IERR         ! Returned by VEC_
      INTEGER   IGNORE       ! Status for VEC_
      INTEGER   INVOKE       ! Dummy function value
      INTEGER   IRPTR        ! Dynamic memory element for input real data
      INTEGER   IPTR         ! Dynamic memory element for output imaginary data
      LOGICAL   JUSTNS       ! True if axis structure contains just 1...n
      LOGICAL   LINEAR       ! True if axis co-ords are linear
      INTEGER   NBAD         ! Bad format conversions - ignored
      INTEGER   NCH          ! Number of characters in a string
      INTEGER   NDIM         ! Number of image dimensions
      INTEGER   NELM         ! Number of elements in output real data
      INTEGER   NELM0        ! Number of elements in input real data
      INTEGER   NPTR         ! Next character to use in string
      INTEGER   PREV         ! Lower acceptable dimension for NAG routines
      INTEGER   RPTR         ! Dynamic memory element for real output data
      INTEGER   SLOT         ! Slot number for mapped data - ignored
      INTEGER   SLOT1        ! Slot number for mapped data - ignored
      LOGICAL   SPACED       ! Used to format user messages
      INTEGER   SPTR         ! Dynamic memory element for spectrum data
      INTEGER   STATUS       ! Running status for DSA routines
      CHARACTER STRING*80    ! Used to format user messages
      REAL      VEND         ! First data element in an axis array
      REAL      VSTART       ! Last data element in an extrapolated axis array
      DOUBLE PRECISION XY1   ! First data element in an axis array
      DOUBLE PRECISION XYDEL ! Increment between axis data array elements
      DOUBLE PRECISION XYLST ! Last data element in an input axis array
C
C     Parameters controlling the way DSA_OUTPUT opens the spectrum file
C
      INTEGER   NEW_FILE
      PARAMETER (NEW_FILE=1)
C
C     Dynamic memory common - defines DYNAMIC_MEM
C
      INCLUDE 'DYNAMIC_MEMORY'
C     
C     Initial values
C
      CHANGE=.FALSE.
      STATUS=0
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
C
C     Get name of real data and open the file.  
C
      CALL DSA_INPUT ('RDATA','RDATA',STATUS)
C
C     Get size of data in RDATA
C
      CALL DSA_DATA_SIZE ('RDATA',10,NDIM,DIMS0,NELM,STATUS)
C
C     We are going to force each dimension of the data to a value
C     that the NAG routines will accept.
C
      NELM=1
      NELM0=1
      DO I=1,NDIM
         CALL FIG_C06CHK(DIMS0(I),DIMS(I),PREV)
         IF (DIMS(I).NE.DIMS0(I)) THEN
            IF (.NOT.CHANGE) CALL PAR_WRUSER(' ',INVOKE)
            STRING=ICH_CI(I)
            NPTR=ICH_LEN(STRING)+1
            STRING(NPTR:)=GEN_NTH(I)//' dimension of input data ('//
     :                                                  ICH_CI(DIMS0(I))
            NPTR=ICH_LEN(STRING)+1
            STRING(NPTR:)=') cannot be used for an FFT'
            CALL PAR_WRUSER(STRING(:NPTR+26),INVOKE)
            STRING='because of the way it factorises, so will '//
     :                                      'be zero extended to '
            INVOKE=ICH_ENCODE(STRING,FLOAT(DIMS(I)),64,0,NPTR)
            CALL PAR_WRUSER(STRING(:NPTR-1)//'.',INVOKE)
            STRING=' (The closest acceptable lower value would be '//
     :                                                   ICH_CI(PREV)
            NPTR=ICH_LEN(STRING)+1
            STRING(NPTR:)='.)'
            CALL PAR_WRUSER(STRING(:NPTR+2),INVOKE)
            CHANGE=.TRUE.
         END IF
         NELM0=NELM0*DIMS0(I)
         NELM=NELM*DIMS(I)
      END DO
      IF (CHANGE) THEN
         CALL PAR_WRUSER('Warning - This may introduce spurious FFT '
     :      //'results, especially if the data does not go '
     :      //'smoothly to zero at the ends.',INVOKE)
      END IF
C
C     Get name of resulting spectrum and copy original if transform
C     is not to be in situ.
C
      IF(CHANGE)THEN
         COPYDATA=1
      ELSE
         COPYDATA=0
      ENDIF
      CALL DSA_OUTPUT('CDATA','CDATA','RDATA',COPYDATA,NEW_FILE,STATUS)
C
C     In the case where the array shape has been changed the data 
C     structure is not copied from the input file. The call 
C     to DSA_RESHAPE_DATA is necessary to copy the 
C     information in this structure and reshape objects such as 
C     the data quality array.
C
      IF(CHANGE)THEN
         CALL DSA_RESHAPE_DATA('CDATA','RDATA',NDIM,DIMS,STATUS)
      ENDIF
C
C     The data array is now forced to be of type 'DOUBLE'.
C
      CALL DSA_COERCE_DATA_ARRAY('CDATA','DOUBLE',NDIM,DIMS,STATUS)
C
C     Warn if the input data was already a complex structure.
C
      CALL DSA_SEEK_IMAGINARY('RDATA',EXIST,STATUS)
      IF(STATUS.NE.0)GOTO 500
      IF(EXIST)THEN
         CALL PAR_WRUSER(
     :      ' Warning: Input structure had an imaginary array',INVOKE)
      ENDIF
C
C     Map the input real array.
C
      CALL DSA_MAP_DATA('RDATA','READ','FLOAT',ADDRESS,SLOT,STATUS)
      IRPTR=DYN_ELEMENT(ADDRESS)
C
C     Map the output real and imaginary arrays.  Note that the imaginary
C     is mapped first to avoid possible problems with some data formats.
C
C     CALL DSA_MAP_IMAGINARY('CDATA','WRITE','DOUBLE',ADDRESS,SLOT,
C    :                        STATUS)
C     IPTR=DYN_ELEMENT(ADDRESS)
C     CALL DSA_MAP_DATA('CDATA','WRITE','DOUBLE',ADDRESS,SLOT,STATUS)
C     RPTR=DYN_ELEMENT(ADDRESS)
      CALL DSA_MAP_COMPLEX('CDATA','WRITE','DOUBLE',
     :   ADDRESS,ADDRESS2,SLOT,STATUS)
      IPTR=DYN_ELEMENT(ADDRESS2)
      RPTR=DYN_ELEMENT(ADDRESS)
      IF(STATUS.NE.0)GOTO 500
C
C     Copy the real data from the input array into the output array.
C     If no format change is involved, then this is a simple type
C     conversion.  Otherwise, the data has to be re-dimensioned as
C     well.  If no real input data is used, just zero the array.
C
      IF (CHANGE) THEN
         CALL FIG_CHCOPY(NDIM,DIMS0,DIMS,NELM0,NELM,
     :                 DYNAMIC_MEM(IRPTR),DYNAMIC_MEM(RPTR))
      ELSE
         IGNORE=0
         CALL VEC_RTOD(.FALSE.,NELM,DYNAMIC_MEM(IRPTR),
     :      DYNAMIC_MEM(RPTR),IERR,NBAD,IGNORE)
         IF (IGNORE.NE.0) CALL ERR_ANNUL(IGNORE)
C        CALL CNV_FMTCNV('FLOAT','DOUBLE',DYNAMIC_MEM(IRPTR),
C    :                 DYNAMIC_MEM(RPTR),NELM,NBAD)
      END IF
C
C     Clear out the imaginary data
C
      BDOUBP=DSA_TYPESIZE('DOUBLE',STATUS)
      IF(STATUS.NE.0)GOTO 500
      CALL GEN_FILL(NELM*BDOUBP,0,DYNAMIC_MEM(IPTR))
C
C     Any axis structures may need to be extrapolated, if this is 
C     possible.
C
      IF (CHANGE) THEN
         SPACED=.FALSE.
C
C        Loop through the various axis structures.
C
         DO I=1,MIN(NDIM,6)
            CALL DSA_SEEK_AXIS('RDATA',I,EXIST,STATUS)
            IF(STATUS.NE.0)GOTO 500
            IF(EXIST)THEN
C
C           Map the existing data array.  
C
               CALL DSA_MAP_AXIS_DATA('RDATA',I,'READ','FLOAT',ADDRESS,
     :                                 SLOT1,STATUS)
               APTR=DYN_ELEMENT(ADDRESS)
               IF(STATUS.NE.0)GOTO 500
C
C              Having mapped it, get enough data from it to be able
C              to create the new array, if it is linear.
C
               LINEAR=FIG_SCRCHK(DIMS0(I),DYNAMIC_MEM(APTR))
               IF (LINEAR) JUSTNS=GEN_CHKNSF(DYNAMIC_MEM(APTR),DIMS0(I))
               XY1=GEN_ELEMF(DYNAMIC_MEM(APTR),1)
               XYLST=GEN_ELEMF(DYNAMIC_MEM(APTR),DIMS0(I))
C
C              Unmap array 
C
               CALL DSA_UNMAP(SLOT1,STATUS)
      
C              What happens now depends on whether the array was linear
C
               IF (.NOT.LINEAR) THEN
C
C                 If it wasn't linear, inform the user that this axis 
C                 structure will be omitted from the output structure.
C
                  IF (.NOT.SPACED) THEN
                     CALL PAR_WRUSER(' ',INVOKE)
                     SPACED=.TRUE.
                  END IF
                  STRING=ICH_CI(I)
                  NCH=ICH_LEN(STRING)
                  CALL PAR_WRUSER(
     :               'The data in the AXIS('//STRING(1:NCH)//
     :               ') array is non-linear and so cannot '//
     :               'be extrapolated. It  will be omitted '//
     :               'from the output structure.',INVOKE)
               ELSE
C
C                 The array was linear, so we can extrapolate it.  First
C                 we create a new array of the right size and map it.
C
                  CALL DSA_RESHAPE_AXIS('CDATA',I,'RDATA',I,1,DIMS(I),
     :                                   STATUS)
                  
                  CALL DSA_MAP_AXIS_DATA('CDATA',I,'UPDATE','FLOAT',
     :                                    ADDRESS,SLOT,STATUS)
                  APTR=DYN_ELEMENT(ADDRESS)
                  IF(STATUS.NE.0)GOTO 500
C
C                 Now fill the new array.  Treat the case of the numbers
C                 1..N separately, for accuracy.
                  
                  IF (JUSTNS) THEN
                     CALL GEN_NFILLF(DIMS(I),DYNAMIC_MEM(APTR))
                  ELSE
                     XYDEL=(XYLST-XY1)/DBLE(DIMS0(I)-1)
                     VSTART=XY1
                     VEND=XY1+XYDEL*(DIMS(I)-1)
                     CALL FIG_WFILL(VSTART,VEND,.FALSE.,DIMS(I),
     :                              DYNAMIC_MEM(APTR))
                  END IF
                  CALL DSA_UNMAP(SLOT1,STATUS)
               END IF
            END IF
         END DO
      END IF

  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)
      END
