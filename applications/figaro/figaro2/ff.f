C+
      SUBROUTINE FF
C
C     F F
C
C     Applies a flat field correction to an image.
C
C     Command parameters -
C
C     IMAGE  The name of the structure containing the image.
C
C     FLAT   The name of the structure containing the flat
C            field data.
C
C     ORDER  The order of the fit to be applied to the flat
C            field data profile.
C
C     OUTPUT The name of the result of the operation.  This can
C            be the same as for IMAGE.  If not, a new structure
C            is created, with everything but the data a direct
C            copy of the input.
C
C                                      KS / CIT 31st MAy 1983
C
C     Modified:
C
C     3rd  Aug 1987  Revised DSA_ routines - some spec changed. Dynamic
C                    memory handling now done by DYN_ routines.
C     26th Mar 1991  KS / AAO.  Use of 'UPDATE' and 'WRITE' corrected in
C                    mapping calls.
C     5th  Oct 1992  HME / UoE, Starlink.  INCLUDE changed, TABs
C                    removed.
C     26th Mar 1997  JJL / Southampton, Starlink. Error propagation included.
C     27th Jul 1997  MJCL / Starlink, UCL.  Initialise VIEXIST and VFEXIST
C                    to .FALSE.
C+
      IMPLICIT NONE
C
C     Functions
C
      INTEGER DYN_ELEMENT,DSA_TYPESIZE
C
C     Local variables
C
      INTEGER      ADDRESS      ! Address of dynamic memory element
      INTEGER      BYTES        ! Number of bytes of workspace required
      INTEGER      DIMS(10)     ! Sizes of dimensions of data
      INTEGER      FPTR         ! Dynamic-memory pointer to flat data array
      INTEGER      FSLOT        ! Map slot number flat field data array
      INTEGER      IORDER       ! The order of the fit
      INTEGER      NDIM         ! Number of dimensions in data
      INTEGER      NELM         ! Total number of elements in data
      INTEGER      NLINE        ! Size of 2nd dimension
      INTEGER      NPIX         ! Size of 1st dimension
      INTEGER      OPTR         ! Dynamic-memory pointer to output data array
      INTEGER      OSLOT        ! Map slot number for output data array
      INTEGER      STATUS       ! Running status for DSA_ routines
      REAL         VALUE        ! Temporary real number
      LOGICAL      VEXIST       ! Does variance exist in both files?
      LOGICAL      VFEXIST      ! Does flat field contain a variance array?
      LOGICAL      VIEXIST      ! Does the data contain a variance array?
      INTEGER      VFPTR        ! Dyn-mem pointer to flat field variance array
      INTEGER      VFSLOT       ! Map slot number of variance
      INTEGER      VOPTR        ! Dynamic-mem pointer to output variance array
      INTEGER      VSLOT        ! Map slot number of variance
      INTEGER      WPTR         ! Dynamic-memory pointer to workspace
      INTEGER      WSLOT        ! Map slot number of workspace
C
C     Dynamic memory support - defines DYNAMIC_MEM
C
      INCLUDE 'DYNAMIC_MEMORY'
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get input name
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE('IMAGE',10,NDIM,DIMS,NELM,STATUS)
      NLINE=DIMS(2)
      NPIX=DIMS(1)
C
C     Get flat field name
C
      CALL DSA_INPUT('FLAT','FLAT',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get dimensions of flat field data
C
      CALL DSA_MATCH_SIZES('IMAGE','FLAT',STATUS)
      IF (STATUS.NE.0) GOTO 500

C
C     Now see if there is any error information
C
      VIEXIST = .FALSE.
      VFEXIST = .FALSE.
      CALL DSA_SEEK_VARIANCE ('IMAGE',VIEXIST,STATUS)
      CALL DSA_SEEK_VARIANCE ('FLAT',VFEXIST,STATUS)
      IF (VIEXIST.AND.VFEXIST) THEN 
         VEXIST=.TRUE.
         CALL PAR_WRUSER('Variances will be propagated',STATUS)
      ENDIF
C
C     Get order for fit
C
      CALL PAR_RDVAL('ORDER',1.,7.,2.,' ',VALUE)
      IORDER=NINT(VALUE)
C
C     Get output structure name and map data
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',ADDRESS,OSLOT,
     :                                                       STATUS)
      OPTR=DYN_ELEMENT(ADDRESS)
      IF (VEXIST) THEN
          CALL DSA_MAP_VARIANCE('OUTPUT','UPDATE','FLOAT',ADDRESS,
     :                                                 VSLOT,STATUS)
          VOPTR=DYN_ELEMENT(ADDRESS)
      ENDIF
      IF (STATUS.NE.0) GO TO 500
C
C
C     Map flat field data
C
      CALL DSA_MAP_DATA ('FLAT','READ','FLOAT',ADDRESS,FSLOT,STATUS)
      FPTR=DYN_ELEMENT(ADDRESS)
      IF (VEXIST) THEN
          CALL DSA_MAP_VARIANCE ('FLAT','READ','FLOAT',ADDRESS,
     :                            VFSLOT,STATUS)
           VFPTR=DYN_ELEMENT(ADDRESS)
      ENDIF
      IF (STATUS.NE.0) GO TO 500
C
C     Get workspace for flattening routine
C
      BYTES=NPIX*3*DSA_TYPESIZE('FLOAT',STATUS)
      CALL DSA_GET_WORKSPACE (BYTES,ADDRESS,WSLOT,STATUS)
      WPTR=DYN_ELEMENT(ADDRESS)
      IF (STATUS.NE.0) GO TO 500
C
C     Flatten the image
C
      CALL FIG_FLAT2D(DYNAMIC_MEM(OPTR),DYNAMIC_MEM(VOPTR),VEXIST,
     :                DYNAMIC_MEM(FPTR),DYNAMIC_MEM(VFPTR),NPIX,
     :                NLINE,IORDER,DYNAMIC_MEM(WPTR),
     :                DYNAMIC_MEM(WPTR+NPIX*4),DYNAMIC_MEM(WPTR+NPIX*8))
C
C     Tidy up
C
500   CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END
