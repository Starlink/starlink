C+
C                  D S A _ _ N T H _ D A T A _ I T E M
C
C  Routine name:
C     DSA__NTH_DATA_ITEM
C
C  Function:
C     Returns the name of the nth item in a data structure.
C
C  Description:
C     This routine, given the name of the data environment for a
C     structure - something probably returned by DSA__DATA_ENV_NAME -
C     returns the name of the nth item in that environment.  In the
C     original Figaro data structure design, this is just a call
C     to DTA_NMVAR, since everything in the data environment is
C     connected with the array.  For NDF format files, this is also
C     a call to DTA_NMVAR, but the items are also filtered to indicate
C     whether or not they are connected directly with the data array
C     (VARIANCE and UNITS are, for example, but MORE and HISTORY are not)
C     or not.  So this returns an 'EXIST' flag that indicates that
C     there is such an nth item (once EXIST is returned false there
C     is no point in calling this routine with higher index values),
C     and also a 'USE' flag which is set if the item named is associated
C     with the data.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__NTH_DATA_ITEM (REF_SLOT,ENV_NAME,IPOS,EXIST,USE,NAME)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The reference slot of the structure
C                        in question.
C     (>) ENV_NAME       (Fixed string,descr) The environment name. This
C                        could be determined from REF_SLOT, but it's
C                        easier if it's passed.
C     (>) IPOS           (Integer,ref) The item number in question.
C     (<) EXIST          (Logical,ref) Returned true if such an IPOSth
C                        item exists.
C     (<) USE            (Logical,ref) Returned true if the item in
C                        question is directly associated with the data.
C     (<) NAME           (Fixed string,descr) The name of the nth object.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:
C     DSA__NDF_TOP_NAME, DTA_NMVAR.
C
C  Prior requirements:
C     The structure must have been opened already and the environment
C     should exist.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (>) NDF_FORMAT    (Logical array) Indicates structure format is Starlink's
C                       NDF format (described in SGP38).  If false, format is
C                       original Figaro format (DST files).
C
C  External routine details:
C     DTA_NMVAR          Get name of nth item in a structure.
C     DSA__NDF_TOP_NAME  Check on name in top level NDF structure.
C
C  History:
C     26th Feb  1990.   Original version.  KS / AAO.
C     12th Mar  1990.   WARN parameter added to DSA__NDF_TOP_NAME call.  KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version is for the both the original Figaro data structures
C     and the Starlink defined NDF structures.
C+
      SUBROUTINE DSA__NTH_DATA_ITEM (REF_SLOT,ENV_NAME,IPOS,
     :                                                  EXIST,USE,NAME)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL EXIST, USE
      INTEGER REF_SLOT, IPOS
      CHARACTER*(*) ENV_NAME, NAME
C
C     DSA system common
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      LOGICAL   AXIS_ITEM          ! Set if item is part of axis data - ignored
      INTEGER   DTA_STATUS         ! Status returned by DTA_NMVAR
C
C     Get name of nth item.
C
      CALL DTA_NMVAR (ENV_NAME,IPOS,NAME,DTA_STATUS)
      EXIST=DTA_STATUS.EQ.0
      IF (EXIST) THEN
C
         IF (NDF_FORMAT(REF_SLOT)) THEN
C
C           For NDF format this operation is complicated by the fact
C           that the things in question aren't collected in a single
C           structure but are at top level together with a load of other
C           stuff that shouldn't be used.  So we have to check that.
C           Note that DSA__NDF_TOP_NAME will output a warning message if
C           it doesn't recognise the name.
C
            CALL DSA__NDF_TOP_NAME (ENV_NAME,NAME,.TRUE.,USE,AXIS_ITEM)
         ELSE
C
C           For the original Figaro format, it's easy.
C
            USE=.TRUE.
         END IF
      END IF
C
      END
