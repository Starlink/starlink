C+
      SUBROUTINE XCOPI
C
C     X C O P I
C
C     The main use of this routine is to set the axis data in
C     a data file to values intermediate between those of
C     two other files.  Typically, the first file will be a
C     spectrum, the others will be arcs taken either side
C     of the spectrum.  The whole axis structure from the first
C     'arc' file replaces that from the original file, and then
C     the axis data array is modified to give the required
C     intermediate result.
C
C     Command parameters -
C
C     SPECTRUM    (Character) The file name of the spectrum whose
C                 data is to be combined with the axis data from the
C                 arcs.
C
C     ARC         (Character) The file name of the arc spectrum - ie
C                 the spectrum whose axis information is to be used.
C
C     ARC2        (Character) The file name of the second arc
C                 spectrum whose axis array is to be used.
C
C     FRACTION    (Numeric) The fractional value controlling the
C                 weight given to the different arc data arrays.
C                 If 0., the output data is the same as for ARC,
C                 and the simpler XCOPY routine should be used.
C                 If 1., the output data is the same as for ARC2.
C                 If .5, the output is the average of the two arcs.
C                 Etc.
C
C     OUTPUT      (Character) The file name for the resulting data
C                 structure.  If this is the same as SPECTRUM, the
C                 axis data in SPECTRUM will be changed in situ.
C
C     Command keywords - None
C
C     User variables used - None
C
C                                             KS / CIT 1st July 1983
C     Modified:
C
C     12th Sept 1985  KS / AAO. Now works in double precision, so as
C                     to be able to cope with high resolution data.
C     21st Oct  1988  JM / RAL. Modified to use DSA_ routines
C                     Dynamic-memory handling changed to use
C                     DYN_ routines
C     6th Feb. 1991   JMS / AAO. Added STATUS checks to support user
C                     requested aborts.
C     23rd Sep 1992   HME / UoE, Starlink.  INCLUDE changed. Call
C                     PAR_WRUSER rather than DSA_WRUSER.
C     1st Feb 1994    HME / UoE, Starlink.  Fix bug whereby the first
C                     arc was ignored and the x data of the spectrum
C                     (usually numbers 1...N) used instead.
C     29th July 1996  MJCL / Starlink, UCL.  PAR_ABORT checking.
C     2005 June 15    MJC / Starlink  Use CNF_PVAL for pointers to
C                     mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT    ! (F)PAR abort flag
C
C     Local variables
C

      INTEGER   APTR1      ! Dynamic-memory pointer to ARC axis data
      INTEGER   APTR       ! Dynamic-memory pointer to ARC2 axis data
      INTEGER   DIMS(5)    ! Axis data dimensions
      LOGICAL   EXIST      ! Used to check for existence of axis data
      REAL      FRAC       ! Fractional weight given to ARC2
      INTEGER   NDIM       ! Number of dimensions in axis data
      INTEGER   NX         ! Number of elements in ARC axis data array
      INTEGER   NX2        ! Number of elements in ARC2 axis data array
      INTEGER   OPTR       ! Dynamic-memory pointer to SPECT/ARC axis data
      INTEGER   SLOT       ! Slot number
      INTEGER   STATUS     ! Running status for DSA_ routines
      CHARACTER TYPE*32    ! Data type for axis data
C
C     Initial values
C
      STATUS=0
      EXIST=.FALSE.
      TYPE='DOUBLE'
C
C     Open DSA
C
      CALL DSA_OPEN(STATUS)
      IF(STATUS.NE.0) GOTO 500
C
C     Get value of SPECTRUM and open the file
C
      CALL DSA_INPUT('SPECT','SPECTRUM',STATUS)
C
C     Get value of ARC and open the file
C
      CALL DSA_INPUT('ARC','ARC',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Check ARC axis data-array exists and if so, get size.
C
      CALL DSA_SEEK_AXIS('ARC',1,EXIST,STATUS)
      IF(.NOT.EXIST)THEN
         CALL PAR_WRUSER('Axis data array does not exist',STATUS)
         GOTO 500
      ENDIF
      CALL DSA_AXIS_SIZE ('ARC',1,5,NDIM,DIMS,NX,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Likewise for ARC2
C
      CALL DSA_INPUT('ARC2','ARC2',STATUS)
      IF (STATUS.NE.0) GOTO 500

      CALL DSA_SEEK_AXIS('ARC2',1,EXIST,STATUS)
      IF(.NOT.EXIST)THEN
         CALL PAR_WRUSER('Axis data array does not exist',STATUS)
         GOTO 500
      ENDIF
      CALL DSA_AXIS_SIZE ('ARC2',1,5,NDIM,DIMS,NX2,STATUS)
      IF(STATUS.NE.0) GOTO 500
C
C     Check arc dimensions match
C
      IF(NX.NE.NX2)THEN
         CALL PAR_WRUSER('The two arcs have different dimensions',
     :      STATUS)
         GOTO 500
      ENDIF
C
C     Get value of FRACTION
C
      CALL PAR_RDVAL('FRACTION',0.,1.,0.5,' ',FRAC)
      IF ( PAR_ABORT() ) GO TO 500
C
C     Create OUTPUT file which is a copy of input SPECT file
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','SPECT',0,0,STATUS)
C
C     Reshape axis to that of ARC file and map axis data for update
C
      CALL DSA_RESHAPE_AXIS('OUTPUT',1,'ARC',1,NDIM,DIMS,STATUS)
      CALL DSA_MAP_AXIS_DATA('OUTPUT',1,'UPDATE',TYPE,OPTR,SLOT,STATUS)
C
C     The output file contains the data from ARC1. Now we have to
C     modify that given the ARC2 data and FRAC.
C
C     That's of course not true. The output file is a copy of the
C     spectrum, not the first arc.
C
      CALL DSA_MAP_AXIS_DATA('ARC',1,'READ',TYPE,APTR1,SLOT,STATUS)
      IF(STATUS.NE.0) GOTO 500
      CALL DSA_MAP_AXIS_DATA('ARC2',1,'READ',TYPE,APTR,SLOT,STATUS)
      IF(STATUS.NE.0) GOTO 500
C
C     Calculate the modified axis data
C
      CALL FIG_FRACAV(%VAL(CNF_PVAL(APTR1)),%VAL(CNF_PVAL(APTR)),
     :                NX,FRAC,%VAL(CNF_PVAL(OPTR)))

  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)

      END
