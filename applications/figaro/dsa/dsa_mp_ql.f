C+
C                   D S A _ M A P _ Q U A L I T Y
C
C  Routine name:
C     DSA_MAP_QUALITY
C
C  Function:
C     Maps the quality data array in a structure.
C
C  Description:
C     This routine maps the quality data array in a structure, returning
C     the address of the mapped array.  The whole array is mapped.  If
C     there is in fact no quality data array, then one will be created
C     if the mapping specified 'WRITE' or 'UPDATE' and filled with zeros.
C     If the main data array contains flagged values, and the application
C     has not indicated through a call to DSA_USE_FLAGGED_VALUES that it
C     wants to handle these directly, then any flagged values in the
C     data array will be removed and the corresponding elements of the
C     quality array will be set. (Note that this setting only happens once
C     both data array and quality array are mapped, so if DSA_MAP_DATA is
C     called afer this routine, the quality array will not reflect the
C     flagged data values until DSA_MAP_DATA is called - this also means
C     that if the file contains flagged data, both map routines - DATA
C     and QUALITY - must be called even if the application only intends to
C     use the quality array).  Note that in all cases this routine
C     does return an address that may be used as the start of a quality
C     array.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_MAP_QUALITY (REF_NAME,MODE,TYPE,ADDRESS,SLOT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (>) MODE         (Fixed string,descr) One of 'READ','WRITE', or
C                      'UPDATE', indicating the way the data is going to
C                      be accessed.  Only the first character is significant.
C     (>) TYPE         (Fixed string,descr) The type of data array to be
C                      mapped onto the structure array.  This can be 'BYTE',
C                      'CHAR','FLOAT','DOUBLE','SHORT','USHORT' or 'INT'.
C                      If type conversion is needed, it will be performed
C                      automatically.
C     (<) ADDRESS      (Integer,ref) The address of the mapped array.
C     (<) SLOT         (Integer,ref) A handle value associated with this
C                      mapping call, which may be used later to unmap
C                      the data explicitly.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used:
C     Only common variables used internally by the DSA_ routines.
C
C  External subroutines / functions used:
C     DSA_ACT_MAP_QUALITY, DSA_REF_SLOT, DSA_GET_ACTUAL_NAME, DSA_WRUSER
C     ICH_LEN
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     data structure must have been opened, eg by DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 7th February 1995
C-
C  Common variable details:
C     (>) DATA_SLOT    (Integer array) Map call slot used for data mapping.
C     (!) USE_QUALITY  (Logical array) Indicates quality data will be used.
C
C  Subroutine / function details:
C     ICH_LEN       Position of last non-blank char in string.
C     DSA_ACT_MAP_QUALITY Perform actual mapping of quality array.
C     DSA_REF_SLOT  Look up reference name in common tables.
C     DSA_GET_ACTUAL_NAME Get full name corresponding to reference name
C     DSA_WRUSER    Output message to user.
C
C  History:
C     22nd July 1988.   Original version.  KS / AAO.
C     8th  Sept 1989.   Call to DSA_MAP_ARRAY now sets propagation flag
C                       false.  KS/AAO.
C     11th Dec  1989.   Now sets QUAL_UPDATE flag on a write or update
C                       mapping.  KS/AAO.
C     21st Feb  1990.   Uses DSA__QUAL_NAME to remove assumptions about
C                       file format details.  KS/AAO.
C     22nd Apr  1991.   Now uses DSA__CREATE_QUAL_ENV to create the
C                       environment for the quality array - allows support
C                       of structured arrays including BADBITS values. KS/AAO.
C     1st  May  1991.   Now gets BADBITS value and tests to see if this
C                       is likely to cause problems. KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992     Remove unused variable declarations. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C     16th Jun 1993     Now sets 'quality exist' flag if array created. KS/AAO,
C     26th Oct 1994     Now uses new calling sequence for DSA_MAP_ARRAY. KS/AAO.
C      3rd Feb 1995     Now supports files with both flagged data values and
C                       quality arrays. KS/AAO.
C      7th Feb 1995     DSA_MAP_QUALITY now renamed to DSA_ACT_MAP_QUALITY
C                       and this new DSA_MAP_QUALITY becomes merely a
C                       wrap-up for that routine. KS/AAO.
C+
      SUBROUTINE DSA_MAP_QUALITY (REF_NAME,MODE,TYPE,ADDRESS,SLOT,
     :                                                       STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) REF_NAME, MODE, TYPE
      INTEGER ADDRESS, SLOT, STATUS
C
C     Functions used
C
      INTEGER ICH_LEN
C
C     DSA_ common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   IGNORE                      ! Dummy status argument
      INTEGER   REF_SLOT                    ! Reference table slot #
      CHARACTER STRUCTURE*128               ! Name of structure
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     All that this routine needs to do is set the USE_QUALITY flag
C     in the tables and to call DSA_ACT_MAP_QUALITY. For this it
C     needs the reference slot number - this is a shame, since
C     DSA_ACT_MAP_QUALITY looks this up too - we could avoid this
C     by giving DSA_ACT_MAP_QUALITY the reference slot as an additional
C     argument to be used only if non-zero. One day, maybe.
C
C     We also check that the sequence used by this program is
C     correct, given the sometimes awkward way applications can call
C     the various USE and MAP routines.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
C
      IF ((DATA_SLOT(REF_SLOT).NE.0).AND.
     :                          (.NOT.USE_QUALITY(REF_SLOT))) THEN
         CALL DSA_WRUSER ('Warning: this program is mapping the '//
     :               'quality array in ')
         IGNORE=0
         CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,IGNORE)
         CALL DSA_WRUSER (STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER ('after mapping the data array and without '
     :      //'having called DSA_USE_QUALITY. ')
         CALL DSA_WRUSER ('This is not a supported sequence and may '
     :      //'produce incorrect results on some files.')
         CALL DSA_WRFLUSH
      END IF
C
      USE_QUALITY(REF_SLOT)=.TRUE.
C
      CALL DSA_ACT_MAP_QUALITY (REF_NAME,MODE,TYPE,ADDRESS,SLOT,STATUS)
C
      END
