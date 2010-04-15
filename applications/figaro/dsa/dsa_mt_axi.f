C+
C                     D S A _ M A T C H _ A X I S
C
C  Routine name:
C     DSA_MATCH_AXIS
C
C  Function:
C     Checks that the axis information for two specific axes match.
C
C  Description:
C     This routine checks that the axis information for a specific axis
C     in one structure matches that for a specific axis in another
C     structure.  It checks a) that the units, if any are given, are
C     the same for each axis, b) that the axis data, if it exists, is
C     the same for each axis.  If there are any discrepancies, an error
C     message is output, and bad status is returned.  This routine is
C     intended for use where no further processing is to be performed
C     on the axis data, but where the user should be warned of any possible
C     incompatibility.  If all axes in a structure are to be compared with
C     the corresponding axes in another, then DSA_MATCH_AXES should
C     be used instead.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_MATCH_AXIS (REF_NAME,AXIS,REF_NAME2,AXIS2,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME      (Fixed string,descr) The reference name used to
C                       refer to the first data structure.
C     (>) AXIS          (Integer,ref) The axis number in the first
C                       data structure.
C     (>) REF_NAME2     (Fixed string,descr) The reference name used to
C                       refer to the second data structure.
C     (>) AXIS2         (Integer,ref) The axis number in the second
C                       data structure.
C     (!) STATUS        (Integer,ref) Status value.  If bad status is
C                       passed, this routine returns immediately.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     CNF_PVAL, ICH_LEN, ICH_CI, DSA_DATA_SIZE, DSA_WRUSER,
C     DSA_MATCH_AXIS, DSA_GET_ACTUAL_NAME
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.  Both
C     structures should have been opened by, for example, DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variables used:
C     (>) MAX_AXES   (Integer parameter) Maximum number of axes
C
C  Subroutine / function details:
C     CNF_PVAL         Full pointer to dynamically allocated memory
C     ICH_LEN          Position of last non-blank char in string
C     ICH_CI           Formats an integer into a string
C     DYN_ELEMENT      Dynamic array element corresponding to given address
C     DSA_DATA_SIZE    Get dimensions of main data array
C     DSA_WRUSER       Output message to user
C     DSA_MATCH_AXIS   Compare data for a single axis
C     DSA_GET_ACTUAL_NAME  Get full structure name
C
C  History:
C     15th Jul 1987  Original version.  KS / AAO.
C     8th  Dec 1989  Text of error message fixed (was using same ref name
C                    for both structures when values differed).  KS/AAO.
C     15th Feb 1991  Missing \N added to DSA_WRUSER call. KS/AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C     7th  Oct 1992  HME / UoE, Starlink.  When removing spaces from the
C                    2nd units this routine used to look for spaces in
C                    UNITS2 but then fiddled with UNITS, corrupting the
C                    latter and making them unequal all the time.
C     2005 June 3    Replace DYNAMIC_MEMORY with %VAL(CNF_PVAL(ADDRESS))
C                    contruct for 64-bit addressing.  MJC / Starlink
C+
      SUBROUTINE DSA_MATCH_AXIS (REF_NAME,AXIS,REF_NAME2,AXIS2,STATUS)
C
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Parameters
C
      INTEGER AXIS, AXIS2, STATUS
      CHARACTER*(*) REF_NAME, REF_NAME2
C
C     Functions used
C
      INTEGER ICH_FOLD, ICH_LEN
      REAL GEN_ELEMF
      CHARACTER ICH_CI*8, ICH_CF*16
C
C     DSA_ system common.  Defines MAX_AXES
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ system error codes.
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      INTEGER   ADDRS1                     ! Virtual memory address
      INTEGER   ADDRS2                     ! Virtual memory address
      INTEGER   DIMS(MAX_AXES)             ! Dimensions of first structure
      INTEGER   DIMS2(MAX_AXES)            ! Dimensions of second structure
      INTEGER   DUMMY                      ! Dummy argument, unused
      INTEGER   ERROR1                     ! First discrepant element number
      LOGICAL   EXIST                      ! True if first axis array exits
      LOGICAL   EXIST2                     ! True if second axis array exits
      INTEGER   I                          ! Loop index
      INTEGER   IGNORE                     ! Unmap status, ignored
      INTEGER   IPT                        ! Pointer into UNITS
      INTEGER   IPT2                       ! Pointer into UNITS2
      INTEGER   LEN1                       ! Significant chars in UNITS
      INTEGER   LEN2                       ! Significant chars in UNITS2
      LOGICAL   MATCH                      ! Indicates unit strings match
      INTEGER   NDIM                       ! First structure, # dimensions
      INTEGER   NDIM2                      ! Second structure, # dimensions
      INTEGER   NELM                       ! First structure, # elements
      INTEGER   NELM2                      ! Second structure, # elements
      INTEGER   NERR                       ! Number of discrepant elements
      CHARACTER NUMBER*16                  ! Used to format numbers
      LOGICAL   OK                         ! Used to flag mismatches
      INTEGER   SLOT                       ! Map call slot, first array
      INTEGER   SLOT2                      ! Map call slot, second array
      CHARACTER STRUCTURE*128              ! Full structure name
      CHARACTER UNITS*32                   ! Units for first axis
      CHARACTER UNITS2*32                  ! Units for second axis
C
C     Names of axis data structures - note that this routine assumes that
C     they are all single character names, and can only support up to AXIS=6.
C
      CHARACTER AXIS_NAMES*6
      DATA AXIS_NAMES/'XYTUVW'/
C
C     Return immediately if bad status passed
C
      IF (STATUS.NE.0) RETURN
C
C     Clear mapping flags
C
      SLOT=0
      SLOT2=0
C
C     Get units for both axes and compare them (the comparison is a
C     little crude - the strings are converted to upper case, and
C     blanks removed before they are compared).
C
      CALL DSA_GET_AXIS_INFO (REF_NAME,AXIS,1,UNITS,0,DUMMY,STATUS)
      CALL DSA_GET_AXIS_INFO (REF_NAME2,AXIS2,1,UNITS2,0,DUMMY,STATUS)
      IF (STATUS.NE.0) GO TO 500       ! Error exit
      LEN1=ICH_FOLD(UNITS)
      LEN2=ICH_FOLD(UNITS2)
      IPT=0
      DO I=1,LEN1
         IF (UNITS(I:I).NE.' ') THEN
            IPT=IPT+1
            UNITS(IPT:IPT)=UNITS(I:I)
         END IF
      END DO
      IPT2=0
      DO I=1,LEN2
         IF (UNITS2(I:I).NE.' ') THEN
            IPT2=IPT2+1
            UNITS2(IPT2:IPT2)=UNITS2(I:I)
         END IF
      END DO
      IF (IPT.NE.IPT2) THEN
         MATCH=.FALSE.
      ELSE
         IF (IPT.GT.0) THEN
            MATCH=UNITS(:IPT).EQ.UNITS2(:IPT2)
         ELSE
            MATCH=.TRUE.
         END IF
      END IF
      IF (.NOT.MATCH) THEN
         CALL DSA_WRUSER('The units for the ')
         CALL DSA_WRUSER(AXIS_NAMES(AXIS:AXIS))
         CALL DSA_WRUSER('-axis in ')
         CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,STATUS)
         CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER (' (')
         CALL DSA_GET_AXIS_INFO (REF_NAME,AXIS,1,UNITS,0,DUMMY,STATUS)
         CALL DSA_WRUSER (UNITS(:LEN1))
         CALL DSA_WRUSER(') are not the same as those for the ')
         CALL DSA_WRUSER(AXIS_NAMES(AXIS2:AXIS2))
         CALL DSA_WRUSER('-axis in ')
         CALL DSA_GET_ACTUAL_NAME (REF_NAME2,STRUCTURE,STATUS)
         CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER (' (')
         CALL DSA_GET_AXIS_INFO (REF_NAME2,AXIS2,1,UNITS2,0,DUMMY,
     :                                                         STATUS)
         CALL DSA_WRUSER (UNITS2(:LEN2))
         CALL DSA_WRUSER (')')
         CALL DSA_WRFLUSH
         STATUS=DSA__UNIDIF
      END IF
C
C     See if the axis data arrays exist.
C
      CALL DSA_SEEK_AXIS (REF_NAME,AXIS,EXIST,STATUS)
      CALL DSA_SEEK_AXIS (REF_NAME2,AXIS2,EXIST2,STATUS)
      IF (STATUS.NE.0) GO TO 500       ! Error exit
C
C     If neither exist, we can quit now. If both exist, we need to
C     compare their data.  If only one exists,  we could just check it
C     against the numbers 1..N, but in the interests of keeping this
C     routine simple (!) we map both anyway, knowing that DSA_MAP_AXIS_DATA
C     will give us the numbers 1..N, and trying not to care about the
C     overheads.
C
      IF ((.NOT.EXIST).AND.(.NOT.EXIST2)) GO TO 500    ! Break out to exit.
C
C     If we get here, at least one exists. Get dimensions of both arrays.
C
      CALL DSA_AXIS_SIZE (REF_NAME,AXIS,MAX_AXES,NDIM,DIMS,NELM,STATUS)
      CALL DSA_AXIS_SIZE (REF_NAME2,AXIS2,MAX_AXES,NDIM2,DIMS2,NELM2,
     :                                                          STATUS)
      IF (STATUS.NE.0) GO TO 500       ! Error exit
C
C     Compare them.  First, the number of dimensions.
C
      IF (NDIM2.NE.NDIM) THEN
         CALL DSA_WRUSER ('The ')
         CALL DSA_WRUSER(AXIS_NAMES(AXIS:AXIS))
         CALL DSA_WRUSER('-axis data array in ')
         CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,STATUS)
         CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER (' is ')
         NUMBER=ICH_CI(NDIM)
         CALL DSA_WRUSER (NUMBER(:ICH_LEN(NUMBER)))
         CALL DSA_WRUSER ('-dimensional, whereas that for the ')
         CALL DSA_WRUSER(AXIS_NAMES(AXIS2:AXIS2))
         CALL DSA_WRUSER('-axis in ')
         CALL DSA_GET_ACTUAL_NAME (REF_NAME2,STRUCTURE,STATUS)
         CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER (' is ')
         NUMBER=ICH_CI(NDIM2)
         CALL DSA_WRUSER (NUMBER(:ICH_LEN(NUMBER)))
         CALL DSA_WRUSER ('-dimensional. ')
         CALL DSA_WRFLUSH
         STATUS=DSA__BADDIM
         GO TO 500                   ! Error exit.
      END IF
C
C     Now the actual dimensions.
C
      OK=.TRUE.
      DO I=1,NDIM
         IF (DIMS(I).NE.DIMS2(I)) OK=.FALSE.
      END DO
      IF (.NOT.OK) THEN
         CALL DSA_WRUSER ('The ')
         CALL DSA_WRUSER(AXIS_NAMES(AXIS:AXIS))
         CALL DSA_WRUSER('-axis data array in ')
         CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,STATUS)
         CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER (' has dimensions (')
         DO I=1,NDIM
            NUMBER=ICH_CI(DIMS(I))
            IF (I.GT.1) CALL DSA_WRUSER(',')
            CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
         END DO
         CALL DSA_WRUSER('), whereas that for the ')
         CALL DSA_WRUSER(AXIS_NAMES(AXIS:AXIS))
         CALL DSA_WRUSER('-axis in ')
         CALL DSA_GET_ACTUAL_NAME (REF_NAME2,STRUCTURE,STATUS)
         CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER (' has dimensions (')
         DO I=1,NDIM2
            NUMBER=ICH_CI(DIMS2(I))
            IF (I.GT.1) CALL DSA_WRUSER(',')
            CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
         END DO
         CALL DSA_WRUSER(').')
         CALL DSA_WRFLUSH
         STATUS=DSA__BADDIM
         GO TO 500                   ! Error exit.
      END IF
C
C     OK, they're the same size.  Now see if they're the same data.
C
      CALL DSA_MAP_AXIS_DATA (REF_NAME,AXIS,'READ','FLOAT',ADDRS1,
     :                        SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500        ! Error exit
      CALL DSA_MAP_AXIS_DATA (REF_NAME2,AXIS2,'READ','FLOAT',ADDRS2,
     :                        SLOT2,STATUS)
      IF (STATUS.NE.0) GO TO 500        ! Error exit
      CALL DSA_COMPAF (%VAL(CNF_PVAL(ADDRS1)),%VAL(CNF_PVAL(ADDRS2)),
     :                 NELM,NERR,ERROR1)
      IF (NERR.GT.0) THEN
         CALL DSA_WRUSER ('The ')
         CALL DSA_WRUSER(AXIS_NAMES(AXIS:AXIS))
         CALL DSA_WRUSER('-axis data array in ')
         CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,STATUS)
         CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER(' differs from the ')
         CALL DSA_WRUSER(AXIS_NAMES(AXIS2:AXIS2))
         CALL DSA_WRUSER('-axis data array in ')
         CALL DSA_GET_ACTUAL_NAME (REF_NAME2,STRUCTURE,STATUS)
         CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER('. ')
         IF (NERR.EQ.1) THEN
            CALL DSA_WRUSER('Only one element differs significantly, #')
         ELSE
            NUMBER=ICH_CI(NERR)
            CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
            CALL DSA_WRUSER(
     :          ' elements differ significantly, the first being #')
         END IF
         NUMBER=ICH_CI(ERROR1)
         CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
         CALL DSA_WRUSER('. (')
         NUMBER=ICH_CF(GEN_ELEMF(%VAL(CNF_PVAL(ADDRS1)),ERROR1))
         CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
         CALL DSA_WRUSER(' vs. ')
         NUMBER=ICH_CF(GEN_ELEMF(%VAL(CNF_PVAL(ADDRS2)),ERROR1))
         CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
         CALL DSA_WRUSER(').')
         CALL DSA_WRFLUSH
         STATUS=DSA__DATDIF
         GO TO 500              ! Error exit
      END IF
C
C     Exit, releasing any mapped data.
C
  500 CONTINUE
      IF (SLOT.NE.0) THEN
         IGNORE=0
         CALL DSA_UNMAP(SLOT,IGNORE)
      END IF
      IF (SLOT2.NE.0) THEN
         IGNORE=0
         CALL DSA_UNMAP(SLOT2,IGNORE)
      END IF
C
      END
