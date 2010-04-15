C+
C                      D S A _ S E E K _ E R R O R S
C
C  Routine name:
C     DSA_SEEK_ERRORS
C
C  Function:
C     Determines whether or not an error data array exists.
C
C  Description:
C     This routine looks to see if a data structure contains a data
C     array - or other information that could generate a data array -
C     giving the error information for that structure.  Although it
C     is quite indifferent as to the existence or non-existence of
C     an error array, it will output a warning message if it finds
C     an error array whose size does not match that of the main data
C     array.  It will treat a non-matching error array as not existing.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_SEEK_ERRORS (REF_NAME,EXIST,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (<) EXIST        (Logical,ref) True if there is error data available.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     DSA_REF_SLOT, DSA_ARRAY_EXIST, DSA_ARRAY_SIZE, DSA_MAIN_SIZE,
C     DSA__ERROR_NAME, DSA_WRUSER, ICH_LEN
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     data structure should have been opened, eg by DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_REF_SLOT     Look up reference name in common tables.
C     DSA_ARRAY_EXIST  Determine if a named array exists.
C     DSA_ARRAY_SIZE   Determine dimensions of a named array.
C     DSA_MAIN_SIZE    Determine dimensions of main data array.
C     DSA_WRUSER       Output message string to user.
C     DSA_ERROR_NAME   Get name of array containing error information.
C     ICH_LEN          Position of last non-blank char in string.
C
C  History:
C     22nd June  1987.   Original version.  KS / AAO.
C     12th March 1990.   Now uses DSA__ routines rather than assuming the
C                        original Figaro data format.  KS/AAO.
C     21st Aug 1992      Automatic portability modifications
C                        ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992      "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_SEEK_ERRORS (REF_NAME,EXIST,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL EXIST
      CHARACTER*(*) REF_NAME
      INTEGER STATUS
C
C     Functions
C
      INTEGER ICH_LEN
C
C     Local variables
C
      INTEGER   DIMS(10)                    ! Dimensions of error array
      INTEGER   DIMS2(10)                   ! Dimensions of main data array
      INTEGER   ERR_CODE                    ! Error array type code - ignored
      CHARACTER ERROR*64                    ! Error string
      INTEGER   I                           ! Loop index through dimensions
      INTEGER   IGNORE                      ! Dummy status argument
      INTEGER   LENGTH                      ! Object name length
      LOGICAL   MATCH                       ! Flags whether dimensions match
      INTEGER   NDIM                        ! # of dimensions in error array
      INTEGER   NDIM2                       ! # of dimensions in main array
      CHARACTER OBJ_NAME*128                ! DTA_ name of data object
      INTEGER   REF_SLOT                    ! Reference table slot #
      CHARACTER STRUCTURE*128               ! Full name of structure
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Look up the reference name in the tables and generate the name
C     of the error array.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
      CALL DSA__ERROR_NAME (REF_SLOT,OBJ_NAME,LENGTH,ERR_CODE)
C
C     See if it exists.
C
      CALL DSA_ARRAY_EXIST(OBJ_NAME(:LENGTH),EXIST,STATUS)
C
C     If it exists, get its dimensions and check them against those of
C     the main data array.
C
      IF ((STATUS.EQ.0).AND.EXIST) THEN
         CALL DSA_ARRAY_SIZE(OBJ_NAME(:LENGTH),10,
     :                                    NDIM,DIMS,ERROR,STATUS)
         CALL DSA_MAIN_SIZE(REF_SLOT,.FALSE.,10,NDIM2,DIMS2,ERROR,
     :                                                        STATUS)
         IF (STATUS.NE.0) THEN
            IGNORE=0
            CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,IGNORE)
            CALL DSA_WRUSER(
     :        'Unable to check error array dimensions against those ')
            CALL DSA_WRUSER('of the main data array in the structure ')
            CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
            CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
            CALL DSA_WRUSER('.')
            CALL DSA_WRFLUSH
         ELSE
            MATCH=NDIM.EQ.NDIM2
            IF (MATCH) THEN
               DO I=1,NDIM
                  IF (DIMS(I).NE.DIMS2(I)) THEN
                     MATCH=.FALSE.
                     GO TO 340        ! Break out of loop
                  END IF
               END DO
  340          CONTINUE
            END IF
            IF (.NOT.MATCH) THEN
               IGNORE=0
               CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,IGNORE)
               CALL DSA_WRUSER('The structure ')
               CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
               CALL DSA_WRUSER(' has an error array whose dimensions ')
               CALL DSA_WRUSER(
     :                  'do not match those of its main data array.')
               CALL DSA_WRFLUSH
               EXIST=.FALSE.
            END IF
         END IF
      END IF
C
C     Exit
C
      END
