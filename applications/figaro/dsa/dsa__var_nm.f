C+
C                    D S A _ _ V A R I A N C E _ N A M E
C
C  Routine name:
C     DSA__VARIANCE_NAME
C
C  Function:
C     Returns the name of the variance array in a structure.
C
C  Description:
C     This routine, given the refrenece slot number for a structure,
C     returns the DTA system name of its error array, should it
C     contain one.  Whether or not the array actually exists is not
C     the province of this routine, which is just a repository of
C     naming information.  Some formats use uncertainty arrays rather
C     than variance arrays to store error information; in this case
C     the name of the uncertainty array will be returned, and the
C     error type code value returned will indicate this.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__VARIANCE_NAME (REF_SLOT,NAME,LENGTH,ERR_CODE)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (<) NAME           (Fixed string,descr) The DTA system name of the
C                        variance array in the structure.
C     (<) LENGTH         (Integer,ref) The number of significant characters
C                        in NAME.
C     (<) ERR_CODE       (Integer,ref) Code indicating type of error array
C                        used by the format.  Returns UNCERTAINTY_CODE or
C                        VARIANCE_CODE as defined in DSA_COMMON.INC
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:  DSA__ERROR_NAME.
C
C  Prior requirements:
C     The structure must have been opened already and REF_SLOT must
C     be valid.  This is not tested by this routine.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  History:
C     17th Jan  1990.   Original version.  KS / AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version supports both the original Figaro data structures
C     and Starlink's NDF format.
C+
      SUBROUTINE DSA__VARIANCE_NAME (REF_SLOT,NAME,LENGTH,ERR_CODE)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, LENGTH, ERR_CODE
      CHARACTER*(*) NAME
C
C     The information returned by this routine is the same as that
C     returned by DSA__ERROR_NAME, so we just call that.
C
      CALL DSA__ERROR_NAME (REF_SLOT,NAME,LENGTH,ERR_CODE)
C
      END
