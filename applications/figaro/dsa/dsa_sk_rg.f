C+
C                     D S A _ S E E K _ R A N G E
C
C  Routine name:
C     DSA_SEEK_RANGE
C
C  Function:
C     Determines whether valid max and min values are known.
C
C  Description:
C     Determines whether or not the main data array for a structure
C     has known maximum and minimum values.  If it does, these can
C     be obtained efficiently by a call to DSA_GET_RANGE - such a
C     call without the range values being known would result in what may
C     be an inneficient and unnecessary scan of the actual data, so an
C     application can use this routine to see if it wants to accept the
C     overhead of such a call.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_SEEK_RANGE (REF_NAME,VALID,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name used to
C                      identify the structure.
C     (<) VALID        (Logical,ref) Set true if valid range values
C                      are associated with the structure's data.
C     (!) STATUS       (Integer,ref) Status code.  If bad status is
C                      passed, this routine returns immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     DSA_REF_SLOT, DSA__RANGE_STRUCT_NAME, DTA_RDVARI
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     structure in question should have been already opened.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_REF_SLOT            Look up reference name in common tables.
C     DSA__RANGE_STRUCT_NAME  Get name of range substructure.
C     DTA_RDVARI              Read an integer from a data object.
C
C  History:
C     30th July 1987    Original version.  KS / AAO.
C     1st March 1990    Modified to use DSA__ routines instead of assuming
C                       the original Figaro data format.
C  Note:
C     See notes to DSA_SET_RANGE for assumptions about where range info is held.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_SEEK_RANGE (REF_NAME,EXIST,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL EXIST
      INTEGER STATUS
      CHARACTER*(*) REF_NAME
C
C     Local variables
C
      INTEGER   DTA_STATUS                  ! DTA_ routine status codes
      INTEGER   LENGTH                      ! Length of structure name
      CHARACTER NAME*80                     ! DTA_ name for range structure
      INTEGER   REF_SLOT                    ! Reference table slot #
      INTEGER   VALID                       ! Value of validity flag
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Assume it doesn't exist
C
      EXIST=.FALSE.
C
C     Look up the reference name in the tables and get the name of the
C     sub-structure containing the range information.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500          ! Error exit
      CALL DSA__RANGE_STRUCT_NAME (REF_SLOT,NAME,LENGTH)
C
C     Generate the name of the validity flag and see if we can
C     read it.
C
      CALL DTA_RDVARI(NAME(:LENGTH)//'.VALID',1,VALID,DTA_STATUS)
      IF (DTA_STATUS.EQ.0) EXIST=VALID.NE.0
C
C     Exit
C
  500 CONTINUE
C
      END
