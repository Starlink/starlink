C+
C                      D S A _ _ C R E A T E _ A R R A Y
C
C  Routine name:
C     DSA__CREATE_ARRAY
C
C  Function:
C     Creates an array of given size and type.
C
C  Description:
C     This routine creates an array of a given size and type, assuming
C     that such an array does not already exist.  If the create fails,
C     no error message is output, but the status of the create operation
C     is returned.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__CREATE_ARRAY (NAME,NDIM,DIMS,TYPE,DTA_STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NAME       (Fixed string,descr) The DTA system name of the array
C                    to be created.  Must not have dimension information.
C     (>) NDIM       (Integer,ref) The number of dimensions of the array.
C                    Must be 1 or greater.
C     (>) DIMS       (Integer array,ref) The dimensions of the array.
C     (>) TYPE       (Fixed string,descr) The type of array to be created.
C                    At present, must be a DTA system primitive type.
C     (<) DTA_STATUS (Integer,ref) DTA system status from the create call.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     DTA_CRVAR, DSA_ENCDIM, ICH_LEN
C
C  Prior requirements:
C     The environment for the array must exist.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     ICH_LEN       Position of last non-blank character in string
C     DSA_ENCDIM    Encode a dimension spec into a string
C     DTA_CRVAR     Create a data object
C
C  History:
C     15th Dec  1989.   Original version.  KS / AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C     3rd  Sep 1992     Call DSA_ENCDIM instead of FIG_ENCDIM. HME/UoE
C+
      SUBROUTINE DSA__CREATE_ARRAY (NAME,NDIM,DIMS,TYPE,DTA_STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NDIM,DIMS(NDIM),DTA_STATUS
      CHARACTER*(*) NAME,TYPE
C
C     Functions used
C
      INTEGER ICH_LEN
C
C     Local variables
C
      INTEGER   IPT                ! Character to start dims at.
      CHARACTER OBJECT*128         ! Name of the array, including dims.
C
C     Encode the dimension at the end of the array name.  (Note that
C     DTA_CRNAM won't do this, since we can't separate the environment
C     from the object name - not without going to some trouble)
C
      OBJECT=NAME
      IPT=ICH_LEN(OBJECT)+1
      CALL DSA_ENCDIM (OBJECT,NDIM,DIMS,IPT)
C
C     Create the array.
C
      CALL DTA_CRVAR (OBJECT,TYPE,DTA_STATUS)
C
      END
