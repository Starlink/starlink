      SUBROUTINE DSA_ADD_STRUCTURE (REF_NAME,ELEMENT_ID,TYPE_ID,STATUS)
*+
*  Name:
*     DSA_ADD_STRUCTURE

*  Purpose:
*     Add a defined sub-structure to a structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_ADD_STRUCTURE( DSAREF, ELEMID, TYPED, STATUS )

*  Description:
*     A structure definition file can equate element identifiers with
*     the name of a specific sub-structure. Given the name of an already
*     opened structure, this routine will create in that structure the
*     sub-structure associated with such an element identifier.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the structure.
*     ELEMID = CHARACTER * ( * ) (Given)
*        The element identifier for the sub-structure to be created.
*     TYPEID = CHARACTER * ( * ) (Given)
*        The type of structure to be created. This is a type identifier
*        as defined in the structure definition file, not the specific
*        data system type (although they may well be the same).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     04 Sep 1987 (ks):
*        Original version.
*     18 Mar 1988 (ks):
*        Comments expanded.
*     11 Jan 1990 (ks):
*        Now passes ref slot to DSA_BUILD_STRUCTURE.
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

*  Arguments Given:
      CHARACTER * ( * ) REF_NAME
      CHARACTER * ( * ) ELEMENT_ID
      CHARACTER * ( * ) TYPE_ID

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DTA_STATUS         ! Status return from DTA_ routines
      INTEGER STRUCT_NO          ! Common table entry for structure def
      CHARACTER * ( 80 ) OBJECT_NAME ! DTA_ system name of structure to create
      CHARACTER * ( 16 ) TYPE    ! Structure type

*.

*  Return immediately on bad status
      IF (STATUS.NE.0) RETURN

*  Get DTA_ system name of structure to be created.
      CALL DSA_ELEMENT_NAME (REF_NAME,ELEMENT_ID,OBJECT_NAME,STATUS)

*  Look up the type identifier in the structure definition tables
      CALL DSA3_DEFTYP (TYPE_ID,.TRUE.,TYPE,STRUCT_NO,STATUS)
      IF (STATUS.NE.0) GO TO 500

*  Create the structure
      CALL DTA_CRVAR (OBJECT_NAME,TYPE,DTA_STATUS)
      IF (DTA_STATUS.NE.0) THEN
         CALL ERR_MARK
            STATUS=SAI__ERROR
            CALL MSG_SETC('FDA_T016',OBJECT_NAME)
            CALL ERR_REP('FDA_E064','DSA_ADD_STRUCTURE: '//
     :         'Unable to create the structure ^FDA_T016.',STATUS)
            CALL ERR_FLUSH(STATUS)
         CALL ERR_RLSE
         STATUS=1
         GO TO 500
      END IF

*  Now fill it up
      CALL DSA3_BUILD (OBJECT_NAME,STRUCT_NO,STATUS)

*  Exit

  500 CONTINUE

      END



      SUBROUTINE DSA3_DEFTYP (STRUCTURE_ID,MUST_EXIST,
     :   TYPE,STRUCT_NO,STATUS)
*+
*  Name:
*     DSA3_DEFTYP

*  Purpose:
*     Get the type of a structure defined by a structure definition file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA3_DEFTYP( STRUCTURE_ID, MUST_EXIST,
*        TYPE, STRUCT_NO, STATUS )

*  Description:
*     If a set of structure definitions has been read in by
*     DSA_READ_STRUCT_DEF, this routine will determine the actual type
*     corresponding to a specific structure identifier. It also acts as
*     a test to see if the type has in fact been defined.

*  Arguments:
*     STRUCTURE_ID = CHARACTER * ( * ) (Given)
*        The name used to refer to the structure in the definition file.
*     MUST_EXIST = LOGICAL (Given)
*        If true, the structure must have been defined and an error is
*        reported if it does not. If false, no error messages are output
*        and non-existence is signalled just by setting STRUCT_NO to
*        zero.
*     TYPE = CHARACTER * ( * ) (Returned)
*        The structure type. This is a type that can be passed to a DTA_
*        routine.
*     STRUCT_NO = INTEGER (Returned)
*        The number of the entry in the common tables that refers to
*        this structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     01 Sep 1987 (ks):
*        Original version (DSA_DEF_STRUCT_TYPE).
*     15 Jan 1990 (ks):
*        Modified to handle different formats.
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

*  Global Variables:
      INCLUDE 'DSA_STRUCTURE'    ! Global structure variables

*  Arguments Given:
      CHARACTER * ( * ) STRUCTURE_ID
      LOGICAL MUST_EXIST

*  Arguments Returned:
      CHARACTER * ( * ) TYPE
      INTEGER STRUCT_NO

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL WRONG_VARIANT      ! True if defined, but for different variant
      INTEGER I                  ! Loop index through structures
      INTEGER VARIANT            ! Variant code matching file type
      CHARACTER * ( 32 ) STRUCT_UC ! Upper case version of STRUCTURE_ID

*  Internal References:
      INTEGER CHR_LEN            ! Used length of string

*.

*  Return immediately on bad status
      IF (STATUS.NE.0) RETURN

*  Make sure we have an upper case version of STRUCTURE_ID
      STRUCT_UC=STRUCTURE_ID
      CALL CHR_UCASE(STRUCT_UC)

*  See what file type we are looking for.
      VARIANT=NDF_TYPE
      WRONG_VARIANT=.FALSE.

*  Now search through the defined structure names, looking
*  for one that matches.
      STRUCT_NO=0
      DO I=1,STRUCTS_DEFINED
         IF (STRUCT_UC.EQ.STRUCT_NAMES(I)) THEN
            IF ((STRUCT_TYPE_FLAG(I).EQ.ANY_TYPE).OR.
     :           (STRUCT_TYPE_FLAG(I).EQ.VARIANT)) THEN
               STRUCT_NO=I
               TYPE=STRUCT_TYPE(I)
               GO TO 320     ! Break out of I loop
            END IF
            WRONG_VARIANT=.TRUE.
         END IF
      END DO
  320 CONTINUE

*  Possibly error report if not found.
      IF ((STRUCT_NO.EQ.0).AND.MUST_EXIST) THEN
         CALL ERR_MARK
            STATUS=SAI__ERROR
            IF (.NOT.WRONG_VARIANT) THEN
               CALL MSG_SETC('FDA_T018','The structure identifier '//
     :            STRUCT_UC(:CHR_LEN(STRUCT_UC))//
     :            ' has not been defined.')
            ELSE
               CALL MSG_SETC('FDA_T018','The structure identifier '//
     :            STRUCT_UC(:CHR_LEN(STRUCT_UC))//
     :            ' has not been defined.'//
     :            '(Although it is defined For DST format files.)')
            END IF
            CALL ERR_REP('FDA_E065','DSA3_DEFTYP: ^FDA_T018',STATUS)
            CALL ERR_FLUSH(STATUS)
         CALL ERR_RLSE
         STATUS=1
      END IF

      END
