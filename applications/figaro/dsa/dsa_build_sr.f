C+
C                   D S A _ B U I L D _ S T R U C T U R E
C
C  Routine name:
C     DSA_BUILD_STRUCTURE
C
C  Function:
C     Builds a defined structure in an existing empty structure object.
C
C  Description:
C     This is a utility routine for DSA_ internal use, that takes an
C     existing - probably newly created - structure object, and builds
C     in it a structure of a type defined through a structure definition
C     file and whose description is in the structure common tables.
C     Structuring the routines in this way allows the top level object
C     for this structure to be either a file or an object in a file.
C
C  Language:
C     FORTRAN
C
C  Call:
C      CALL DSA_BUILD_STRUCTURE (OBJECT_NAME,REF_SLOT,STRUCT_NO,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) OBJECT_NAME    (Fixed string,descr) The name of the existing
C                        empty structure object.  This should be its
C                        DTA_ object name.
C     (>) REF_SLOT       (Integer,ref) The index in the common tables
C                        for the overall structure being created - ie
C                        the slot number corresponding to the reference
C                        name for the structure.
C     (>) STRUCT_NO      (Integer,ref) The number in the common tables
C                        of the structure definition.
C     (!) STATUS         (Integer,ref) Status code.  If bad status is
C                        passed to it, this routine returns immediately.
C
C  External subroutines / functions used:
C
C     ICH_FOLD, ICH_LEN, DSA_WRUSER, DSA_WRNAME, DTA_TYVAR, DTA_ERROR,
C     DTA_STRUC, DTA_NMVAR, DSA_TRANSLATE_VAR, DSA_DEF_STRUCT_TYPE,
C     DTA_CRVAR
C
C  Prior requirements:
C     A structure definition file has to have been processed, usually by
C     DSA_READ_STRUCT_DEF, and the top level object has to have been
C     created.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C
C  History:
C     1st Sept 1987   Original version.  KS / AAO.
C     15th Jan 1990   Crude modification to allow structured arrays
C                     added, and REF_SLOT argument added to help support
C                     multiple file types.  KS/AAO.
C     3rd  Mar 1991   Now allows variable names to be used in element
C                     types. KS/AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_BUILD_STRUCTURE (OBJECT_NAME,REF_SLOT,STRUCT_NO,
     :                                                           STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, STRUCT_NO, STATUS
      CHARACTER*(*) OBJECT_NAME
C
C     Functions used
C
      INTEGER ICH_FOLD, ICH_LEN
C
C     Local variables
C
      CHARACTER CHR*1            ! First char of condition
      CHARACTER CONDITION*32     ! Condition to be evaluated for element
      INTEGER   CURR_POSN        ! Current element definition in common
      CHARACTER CURRENT_TYPE*32  ! Current element type
      INTEGER   DTA_STATUS       ! Status return from DTA_ routines
      CHARACTER ELEMENT*32       ! Element name
      INTEGER   END_POSN         ! Last line in common for current structure
      CHARACTER ERROR*64         ! Error description from DTA_ system
      LOGICAL   FINISHED         ! Indicates structure created
      INTEGER   INVOKE           ! Dummt function value
      INTEGER   LENAME           ! Length of current structure name
      CHARACTER NAME*80          ! Current structure name
      INTEGER   STACK_POINTER    ! Pointer to structure stacks
      LOGICAL   STRUCT           ! Indicates element is a structure
      INTEGER   STRUCT_SLOT      ! Slot number in common for current structure
      CHARACTER TYPE*32          ! Type of structure or element
C
C     Stack used for recursive structure creation
C
      INTEGER STACK_SIZE                ! Elements in stack
      PARAMETER (STACK_SIZE=10)
      INTEGER CURR_STACK(STACK_SIZE)    ! Current position stack
      INTEGER END_STACK(STACK_SIZE)     ! End of current structure stack
      INTEGER LENAME_STACK(STACK_SIZE)  ! Length of structure name stack
C
C     DSA_ system general common
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     DSA_ system structre common
C
      INCLUDE 'DSA_STRUCTURE'
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Just make sure STRUCT_NO is valid.  (A safety check)
C
      IF ((STRUCT_NO.LE.0).OR.(STRUCT_NO.GT.STRUCTS_DEFINED)) THEN
         CALL DSA_WRUSER ('Invalid structure number passed to '//
     :          'DSA_BUILD_STRUCT.  Internal DSA_ system error.')
         CALL DSA_WRFLUSH
         STATUS=DSA__INTERR
         GO TO 500    ! Error exit
      END IF
C
C     Make sure that the passed data object a) exists, b) is of the correct
C     type, c) is a structure, and d) is empty.
C
      CALL DTA_TYVAR (OBJECT_NAME,TYPE,DTA_STATUS)
      IF (DTA_STATUS.NE.0) THEN
         CALL DSA_WRUSER ('Unable to build structure in ')
         CALL DSA_WRNAME (OBJECT_NAME)
         CALL DSA_WRUSER ('. ')
         CALL DTA_ERROR (DTA_STATUS,ERROR)
         CALL DSA_WRUSER (ERROR(:ICH_LEN(ERROR)))
         CALL DSA_WRUSER ('.')
         CALL DSA_WRFLUSH
         DTA_CODE=DTA_STATUS
         STATUS=DSA__DTAERR
         GO TO 500     ! Error exit
      END IF
      IF (TYPE.NE.STRUCT_TYPE(STRUCT_NO)) THEN
         CALL DSA_WRUSER ('Warning.  The type ("')
         CALL DSA_WRUSER (TYPE(:ICH_LEN(TYPE)))
         CALL DSA_WRUSER ('") of the existing object ')
         CALL DSA_WRNAME (OBJECT_NAME)
         CALL DSA_WRUSER (
     :     ' does not match that of the structure to be built in it ("')
         CALL DSA_WRUSER (
     :         STRUCT_TYPE(STRUCT_NO)(:ICH_LEN(STRUCT_TYPE(STRUCT_NO))))
         CALL DSA_WRUSER ('").')
         CALL DSA_WRFLUSH
      END IF
      CALL DTA_STRUC (OBJECT_NAME,STRUCT,DTA_STATUS)
      IF (.NOT.STRUCT) THEN
         CALL DSA_WRUSER ('A structure cannot be built in ')
         CALL DSA_WRNAME (OBJECT_NAME)
         CALL DSA_WRUSER (', since it is a primitive object.')
         CALL DSA_WRFLUSH
         STATUS=DSA__BLDERR
         GO TO 500         ! Error exit
      END IF
      CALL DTA_NMVAR (OBJECT_NAME,1,NAME,DTA_STATUS)
      IF (DTA_STATUS.EQ.0) THEN
         CALL DSA_WRUSER ('Warning. A new structure is to be built in ')
         CALL DSA_WRNAME (OBJECT_NAME)
         CALL DSA_WRUSER (', but this is not an empty structure.')
         CALL DSA_WRFLUSH
      END IF
C
C     Creating a hierarchial structure is really a recursive operation.
C     We tackle this here by using stacks to hold our current position
C     in the structure definition, the position of the last element
C     of the structure we're building, and the name of the structure
C     being built.  Once we create an object, we check to see if it's a
C     structure, and if it is we push our three values and start on the
C     new structure.  I wish I could have written this in C.  (The name
C     of the structure being built is saved just as a pointer to the
C     end of the name, since the name just expands and contracts - eg
C     STRUCT, STRUCT.SUB1, STRUCT.SUB1.SUB2, STRUCT.SUB1, etc.)
C
      STACK_POINTER=0
      CURR_POSN=STRUCT_START(STRUCT_NO)
      END_POSN=STRUCT_END(STRUCT_NO)
      NAME=OBJECT_NAME
      LENAME=ICH_LEN(NAME)
      FINISHED=.FALSE.
      DO WHILE (.NOT.FINISHED)
         DO WHILE (CURR_POSN.LE.END_POSN)
C
C           Evaluate the condition for the new element, if any.
C
            CONDITION=ELEMENT_COND(CURR_POSN)
            IF (CONDITION.NE.' ') THEN
               CALL DSA_TRANSLATE_VAR(CONDITION,STATUS)
               IF (STATUS.NE.0) GO TO 500    ! Error exit
            END IF
            CHR=CONDITION(1:1)
            INVOKE=ICH_FOLD(CHR)
            IF ((CHR.EQ.'F').OR.(CHR.EQ.'N').OR.(CHR.EQ.'0')) THEN
C
C              Condition for creation of element is false.  Move on
C              to the next element.
C
               CURR_POSN=CURR_POSN+1
            ELSE
C
C              Create new element.  Translate any variables in the name,
C              then see if it's type is in fact a defined structure type.
C              The crude bit in the handling of structure arrays comes in
C              here: the simplest way to do it is just to allow different
C              elements of the arrays to appear in the definition file as
C              if they were separate structures, which works as long as the
C              highest numbered elements are defined first (otherwise the
C              array won't be big enough).  However, it means that if we've
C              already created .AXIS[2], when we try to create .AXIS[1] we
C              will get bad status (already exists) which we have to ignore.
C
               ELEMENT=ELEMENT_NAME(CURR_POSN)
               CALL DSA_TRANSLATE_VAR (ELEMENT,STATUS)
               IF (STATUS.NE.0) GO TO 500      ! Error exit
               CURRENT_TYPE=ELEMENT_TYPE(CURR_POSN)
               CALL DSA_TRANSLATE_VAR (CURRENT_TYPE,STATUS)
               IF (STATUS.NE.0) GO TO 500      ! Error exit
               CALL DSA_DEF_STRUCT_TYPE (REF_SLOT,
     :                   CURRENT_TYPE,.FALSE.,TYPE,STRUCT_SLOT,STATUS)
               IF (STRUCT_SLOT.EQ.0) TYPE=CURRENT_TYPE
               CALL DTA_CRVAR(NAME(:LENAME)//ELEMENT,TYPE,DTA_STATUS)
               IF (DTA_STATUS.NE.0) THEN
                  CALL DTA_ERROR(DTA_STATUS,ERROR)
                  CALL DTA_STRUC (NAME(:LENAME)//ELEMENT,STRUCT,
     :                                                     DTA_STATUS)
                  IF ((DTA_STATUS.NE.0).OR.(.NOT.STRUCT)) THEN
                     CALL DSA_WRUSER('Unable to create element ')
                     CALL DSA_WRUSER(ELEMENT(:ICH_LEN(ELEMENT)))
                     CALL DSA_WRUSER(' of ')
                     CALL DSA_WRNAME(NAME)
                     CALL DSA_WRUSER('. ')
                     CALL DTA_ERROR(DTA_STATUS,ERROR)
                     CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
                     CALL DSA_WRUSER('.')
                     CALL DSA_WRFLUSH
                     STATUS=DSA__DTAERR
                     DTA_CODE=DTA_STATUS
                     GO TO 500       ! Error exit
                  END IF
               END IF
C
C              See if that element was a structure.
C
               CALL DTA_STRUC (NAME(:LENAME)//ELEMENT,STRUCT,
     :                                                     DTA_STATUS)
               IF (STRUCT) THEN
C
C                 If it was a structure, but one not defined in the
C                 definition file, we will not have got an error message from
C                 the previous call to DSA_DEF_STRUCT_TYPE since we have to
C                 call that with the `must exist' parameter set false - since
C                 at that point we don't know it's a structure.  So the call
C                 here to DSA_DEF_STRUCT_TYPE is simply to force an error
C                 message to be output if it wasn't defined.
C
                  IF (STRUCT_SLOT.EQ.0) THEN
                     CALL DSA_DEF_STRUCT_TYPE (REF_SLOT,
     :                      CURRENT_TYPE,.TRUE.,TYPE,STRUCT_SLOT,STATUS)
                     STATUS=DSA__BLDERR
                     GO TO 500       ! Error exit.
                  END IF
C
C                 It is a defined structure, so push everything onto the
C                 stack, and set up for the new structure.
C
                  STACK_POINTER=STACK_POINTER+1
                  IF (STACK_POINTER.GT.STACK_SIZE) THEN
                     CALL DSA_WRUSER('Ran out of internal stack size '//
     :                                 'trying to build the structure ')
                     CALL DSA_WRNAME(NAME(:LENAME))
                     CALL DSA_WRUSER(
     :                  '.  Structure definition is too complicated.')
                     CALL DSA_WRFLUSH
                     STATUS=DSA__BLDERR
                     GO TO 500      ! Error exit
                  END IF
                  CURR_STACK(STACK_POINTER)=CURR_POSN
                  END_STACK(STACK_POINTER)=END_POSN
                  LENAME_STACK(STACK_POINTER)=LENAME
                  CURR_POSN=STRUCT_START(STRUCT_SLOT)
                  END_POSN=STRUCT_END(STRUCT_SLOT)
                  NAME=NAME(:LENAME)//ELEMENT
                  LENAME=ICH_LEN(NAME)
               ELSE
C
C                 Not a structure, so move on to the next element
C
                  CURR_POSN=CURR_POSN+1
               END IF
            END IF
         END DO
C
C        End of structure reached.  Pop stack and either continue
C        with previous structure, or - when stack empty - finish.
C
         IF (STACK_POINTER.LE.0) THEN
            FINISHED=.TRUE.        ! Force main loop exit
         ELSE
            CURR_POSN=CURR_STACK(STACK_POINTER)+1
            END_POSN=END_STACK(STACK_POINTER)
            LENAME=LENAME_STACK(STACK_POINTER)
            NAME(LENAME+1:)=' '
            STACK_POINTER=STACK_POINTER-1
         END IF
      END DO
C
C     Exit
C
  500 CONTINUE
C
      END
