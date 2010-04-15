      SUBROUTINE DSA3_BUILD (OBJECT_NAME,STRUCT_NO,STATUS)
*+
*  Name:
*     DSA3_BUILD

*  Purpose:
*     Build a defined structure in an existing empty structure object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA3_BUILD( OBJECT_NAME, STRUCT_NO, STATUS )

*  Description:
*     This is a utility routine for DSA_ internal use, that takes an
*     existing - probably newly created - structure object, and builds
*     in it a structure of a type defined through a structure definition
*     file and whose description is in the structure common tables.

*  Arguments:
*     OBJECT_NAME = CHARACTER * ( * ) (Given)
*        The name of the existing empty structure object. This should be
*        its DTA_ object name.
*     STRUCT_NO = INTEGER (Given)
*        The number in the common tables of the structure definition.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     01 Sep 1987 (ks):
*        Original version (DSA_BUILD_STRUCTURE).
*     15 Jan 1990 (ks):
*        Crude modification to allow structured arrays added, and
*        REF_SLOT argument added to help support multiple file types.
*     03 Mar 1991 (ks):
*        Now allows variable names to be used in element types.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     05 Mar 1996 (hme):
*        FDA library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Global Variables:
      INCLUDE 'DSA_STRUCTURE'    ! Global structure variables

*  Arguments Given:
      CHARACTER * ( * ) OBJECT_NAME
      INTEGER STRUCT_NO

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER STACK_SIZE         ! Elements in stack
      PARAMETER ( STACK_SIZE = 10 )

*  Local Variables:
      LOGICAL FINISHED           ! Indicates structure created
      LOGICAL STRUCT             ! Indicates element is a structure
      LOGICAL THERE              ! Whether structure exists
      INTEGER CURR_POSN          ! Current element definition in common
      INTEGER DTA_STATUS         ! Status return from DTA_ routines
      INTEGER END_POSN           ! Last line in common for current structure
      INTEGER LENAME             ! Length of current structure name
      INTEGER NCOMP              ! Number of HDS components
      INTEGER STACK_POINTER      ! Pointer to structure stacks
      INTEGER STRUCT_SLOT        ! Slot number in common for current structure
      INTEGER CURR_STACK(   STACK_SIZE ) ! Current position stack
      INTEGER END_STACK(    STACK_SIZE ) ! End of current structure stack
      INTEGER LENAME_STACK( STACK_SIZE ) ! Length of structure name stack
      CHARACTER * ( 1 ) CHR      ! First char of condition
      CHARACTER * ( 32 ) CONDITION ! Condition to be evaluated for element
      CHARACTER * ( 32 ) CURRENT_TYPE ! Current element type
      CHARACTER * ( 32 ) ELEMENT ! Element name
      CHARACTER * ( 80 ) NAME    ! Current structure name
      CHARACTER * ( 32 ) TYPE    ! Type of structure or element
      CHARACTER * ( DAT__SZLOC ) LOC ! HDS locator
      CHARACTER * ( DAT__SZTYP ) HDSTYP ! HDS data type

*  Internal References:
      INTEGER CHR_LEN            ! Used length of string

*.

*  Return immediately on bad status
      IF (STATUS.NE.0) RETURN

*  Just make sure STRUCT_NO is valid.  (A safety check)
      IF ((STRUCT_NO.LE.0).OR.(STRUCT_NO.GT.STRUCTS_DEFINED)) THEN
         CALL ERR_MARK
            STATUS=SAI__ERROR
            CALL ERR_REP('FDA_E059','DSA3_BUILD: Invalid structure '//
     :         'number. Internal DSA system error.',STATUS)
            CALL ERR_FLUSH(STATUS)
         CALL ERR_RLSE
         STATUS=1
         GO TO 500
      END IF

*  Make sure that the passed data object a) exists, b) is of the correct
*  type, c) is a structure, and d) is empty.
      CALL ERR_MARK
         STATUS=SAI__OK
         CALL DTA1_THERE(OBJECT_NAME,THERE,STATUS)
         IF (.NOT.THERE) THEN
            STATUS=SAI__ERROR
            CALL MSG_SETC('FDA_T016',OBJECT_NAME)
            CALL ERR_REP('FDA_E060','DSA3_BUILD: '//
     :         'Unable to build structure in ^FDA_T016,'//
     :         'because it does not exist.',STATUS)
            GO TO 400
         END IF
         CALL DTA1_LOC(OBJECT_NAME,LOC,STATUS)
         CALL DAT_TYPE(LOC,HDSTYP,STATUS)
         IF ( HDSTYP(:5) .EQ. '_CHAR' ) THEN
            TYPE='CHAR'
         ELSE
            CALL DSA1_DSATYP(HDSTYP,TYPE,STATUS)
            IF (STATUS.NE.SAI__OK) THEN
               CALL ERR_ANNUL(STATUS)
               TYPE=HDSTYP
            END IF
         END IF
         IF (TYPE.NE.STRUCT_TYPE(STRUCT_NO)) THEN
            CALL MSG_SETC('FDA_T003',TYPE)
            CALL MSG_SETC('FDA_T016',OBJECT_NAME)
            CALL MSG_SETC('FDA_T004',STRUCT_TYPE(STRUCT_NO))
            CALL MSG_OUT('FDA_M013','Warning. The type (^FDA_T003) '//
     :         'of the existing object ^FDA_T016 '//
     :         'does not match that of the structure to be built '//
     :         'in it (^FDA_T004).',STATUS)
         END IF
         CALL DAT_STRUC(LOC,STRUCT,STATUS)
         IF (.NOT.STRUCT) THEN
            STATUS=SAI__ERROR
            CALL MSG_SETC('FDA_T016',OBJECT_NAME)
            CALL ERR_REP('FDA_E061','DSA3_BUILD: '//
     :         'A structure cannot be built in ^FDA_T016, '//
     :         'since it is a primitive object.',STATUS)
            GO TO 400
         END IF
         CALL DAT_NCOMP(LOC,NCOMP,STATUS)
         IF (NCOMP.GT.0) THEN
            CALL MSG_SETC('FDA_T016',OBJECT_NAME)
            CALL MSG_OUT('FDA_M014','Warning. A new structure is to'//
     :         'be built in ^FDA_T016, but this is not an empty '//
     :         'structure.',STATUS)
         END IF
 400     CONTINUE
         CALL DAT_ANNUL(LOC,STATUS)
         IF (STATUS.NE.SAI__OK) THEN
            CALL ERR_FLUSH(STATUS)
            STATUS=1
            GO TO 500
         END IF
      CALL ERR_RLSE

*  Creating a hierarchial structure is really a recursive operation.
*  We tackle this here by using stacks to hold our current position
*  in the structure definition, the position of the last element
*  of the structure we're building, and the name of the structure
*  being built.  Once we create an object, we check to see if it's a
*  structure, and if it is we push our three values and start on the
*  new structure.  I wish I could have written this in C.  (The name
*  of the structure being built is saved just as a pointer to the
*  end of the name, since the name just expands and contracts - eg
*  STRUCT, STRUCT.SUB1, STRUCT.SUB1.SUB2, STRUCT.SUB1, etc.)
      STACK_POINTER=0
      CURR_POSN=STRUCT_START(STRUCT_NO)
      END_POSN=STRUCT_END(STRUCT_NO)
      NAME=OBJECT_NAME
      LENAME=CHR_LEN(NAME)
      FINISHED=.FALSE.
      DO WHILE (.NOT.FINISHED)
         DO WHILE (CURR_POSN.LE.END_POSN)

*        Evaluate the condition for the new element, if any.
            CONDITION=ELEMENT_COND(CURR_POSN)
            IF (CONDITION.NE.' ') THEN
               CALL DSA3_TRNVAR(CONDITION,STATUS)
               IF (STATUS.NE.0) GO TO 500
            END IF
            CHR=CONDITION(1:1)
            CALL CHR_UCASE(CHR)
            IF ((CHR.EQ.'F').OR.(CHR.EQ.'N').OR.(CHR.EQ.'0')) THEN

*           Condition for creation of element is false.  Move on
*           to the next element.
               CURR_POSN=CURR_POSN+1
            ELSE

*           Create new element.  Translate any variables in the name,
*           then see if it's type is in fact a defined structure type.
*           The crude bit in the handling of structure arrays comes in
*           here: the simplest way to do it is just to allow different
*           elements of the arrays to appear in the definition file as
*           if they were separate structures, which works as long as the
*           highest numbered elements are defined first (otherwise the
*           array won't be big enough).  However, it means that if we've
*           already created .AXIS[2], when we try to create .AXIS[1] we
*           will get bad status (already exists) which we have to ignore.
               ELEMENT=ELEMENT_NAME(CURR_POSN)
               CALL DSA3_TRNVAR (ELEMENT,STATUS)
               IF (STATUS.NE.0) GO TO 500
               CURRENT_TYPE=ELEMENT_TYPE(CURR_POSN)
               CALL DSA3_TRNVAR (CURRENT_TYPE,STATUS)
               IF (STATUS.NE.0) GO TO 500
               CALL DSA3_DEFTYP (
     :                   CURRENT_TYPE,.FALSE.,TYPE,STRUCT_SLOT,STATUS)
               IF (STRUCT_SLOT.EQ.0) TYPE=CURRENT_TYPE
               CALL DTA_CRVAR(NAME(:LENAME)//ELEMENT,TYPE,DTA_STATUS)
               IF (DTA_STATUS.NE.0) THEN
                  CALL DTA_STRUC (NAME(:LENAME)//ELEMENT,STRUCT,
     :                                                     DTA_STATUS)
                  IF ((DTA_STATUS.NE.0).OR.(.NOT.STRUCT)) THEN
                     CALL ERR_MARK
                        STATUS=SAI__ERROR
                        CALL MSG_SETC('FDA_T017',ELEMENT)
                        CALL MSG_SETC('FDA_T016',NAME)
                        CALL ERR_REP('FDA_E062','DSA3_BUILD: '//
     :                     'Unable to create element ^FDA_T017, '//
     :                     'of ^FDA_T016.',STATUS)
                        CALL ERR_FLUSH(STATUS)
                     CALL ERR_RLSE
                     STATUS=1
                     GO TO 500
                  END IF
               END IF

*           See if that element was a structure.
               CALL DTA_STRUC (NAME(:LENAME)//ELEMENT,STRUCT,
     :                                                     DTA_STATUS)
               IF (STRUCT) THEN

*              If it was a structure, but one not defined in the
*              definition file, we will not have got an error message from
*              the previous call to DSA_DEF_STRUCT_TYPE since we have to
*              call that with the `must exist' parameter set false - since
*              at that point we don't know it's a structure.  So the call
*              here to DSA_DEF_STRUCT_TYPE is simply to force an error
*              message to be output if it wasn't defined.
                  IF (STRUCT_SLOT.EQ.0) THEN
                     CALL DSA3_DEFTYP (
     :                      CURRENT_TYPE,.TRUE.,TYPE,STRUCT_SLOT,STATUS)
                     STATUS=1
                     GO TO 500
                  END IF

*              It is a defined structure, so push everything onto the
*              stack, and set up for the new structure.
                  STACK_POINTER=STACK_POINTER+1
                  IF (STACK_POINTER.GT.STACK_SIZE) THEN
                     CALL ERR_MARK
                        STATUS=SAI__ERROR
                        CALL MSG_SETC('FDA_T016',NAME)
                        CALL ERR_REP('FDA_E063','DSA3_BUILD: '//
     :                     'Ran out of internal stack size trying '//
     :                     'trying to build the structure ^FDA_T016. '//
     :                     'Structure definition is too complicated.',
     :                     STATUS)
                        CALL ERR_FLUSH(STATUS)
                     CALL ERR_RLSE
                     STATUS=1
                     GO TO 500
                  END IF
                  CURR_STACK(STACK_POINTER)=CURR_POSN
                  END_STACK(STACK_POINTER)=END_POSN
                  LENAME_STACK(STACK_POINTER)=LENAME
                  CURR_POSN=STRUCT_START(STRUCT_SLOT)
                  END_POSN=STRUCT_END(STRUCT_SLOT)
                  NAME=NAME(:LENAME)//ELEMENT
                  LENAME=CHR_LEN(NAME)
               ELSE

*              Not a structure, so move on to the next element
                  CURR_POSN=CURR_POSN+1
               END IF
            END IF
         END DO

*     End of structure reached.  Pop stack and either continue
*     with previous structure, or - when stack empty - finish.
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

*  Exit
  500 CONTINUE

      END
