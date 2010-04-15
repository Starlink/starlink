C+
      SUBROUTINE TSP_MAP_VSLICE (LOC,NDIMS,LOWER,UPPER,MODE,
     :    PTR,LOC2,STATUS)
C
C                               T S P _ M A P _ V S L I C E
C
C  Routine name:
C     TSP_MAP_VSLICE
C
C  Function:
C     Map a slice of the variance array of an NDF structure
C
C  Description:
C     Given a locator to an NDF structure map a slice of its VARIANCE array.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL TSP_MAP_VSLICE (LOC,NDIMS,LOWER,UPPER,MODE,PTR,LOC2,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) LOC		(Fixed string,descr) A locator to the NDF
C                       structure.
C     (>) NDIMS         (Integer,ref) The number of dimensions.
C     (>) LOWER         (Integer array,ref) Array of lower bounds
C                       for the slice.
C     (>) UPPER         (Integer array,ref) Array of upper bounds
C                       for the slice.
C     (>) MODE          (Fixed string,descr) The access mode,
C                       'READ','WRITE' or 'UPDATE'.
C     (<) PTR           (Integer,ref) Pointer to the mapped data.
C     (<) LOC2		(Fixed string,descr) The locator to the
C                       mapped data object - needed so that it can
C                       be unmapped.
C     (!) STATUS	(Integer,ref) The Status
C
C  External subroutines / functions used:
C
C     Various HDS routines
C
C  Support: Jeremy Bailey, JAC
C
C  Version date: 28/2/1988
C
C-
C  Subroutine / function details:
C
C  History:
C     28/2/1988   Original version.  JAB / AAO.
C     20/8/1991   Return NDF__VUDEF if variance not defined.  JAB / AAO.
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'NDF_ERR'
C
C     Parameters
C
      CHARACTER*(DAT__SZLOC) LOC
      INTEGER NDIMS
      INTEGER LOWER(NDIMS), UPPER(NDIMS)
      CHARACTER*(*) MODE
      CHARACTER*(DAT__SZLOC) LOC2
      INTEGER PTR
      INTEGER STATUS
C
C     Local variables
C
      INTEGER ID,ID2,LENGTH
      LOGICAL PRESENT
      logical oldflag

C
      IF (STATUS .EQ. SAI__OK) THEN

          call ndf_trace(.true.,oldflag)
          CALL NDF_IMPRT(LOC,ID,STATUS)
          CALL NDF_STATE(ID,'VARIANCE',PRESENT,STATUS)
          IF (PRESENT) THEN
              CALL NDF_SECT(ID,NDIMS,LOWER,UPPER,ID2,STATUS)
              CALL NDF_MAP(ID2,'VARIANCE','_REAL',MODE,PTR,
     :             LENGTH,STATUS)
          ELSE
              STATUS = NDF__VUDEF
          ENDIF

      END IF
C
      END

