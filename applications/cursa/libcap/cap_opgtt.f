      SUBROUTINE CAP_OPGTC (CI, QNAME, DEFLT, VALUE, STATUS)
*+
*  Name:
*     CAP_OPGTC
*  Purpose:
*     Optionally attempt to get a parameter from a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_OPGTC (CI, QNAME, DEFLT; VALUE; STATUS)
*  Description:
*     Optionally attempt to get a parameter from a catalogue.
*     If a value cannot be obtained then a supplied default is adopted
*     and no error is reported.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     QNAME  =  CHARACTER*(*) (Given)
*        Name of the parameter in the catalogue (if it exists).
*     DEFLT  =  CHARACTER*(*) (Given)
*        Supplied for the default.
*     VALUE  =  CHARACTER*(*) (Returned)
*        Value obtained for the parameter.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set the error context.
*     Attempt to get an identifier for the parameter.
*     If ok then
*       Determine the type of identifier obtained.
*       If the identifier is for a parameter then
*         Attempt to get a value for the parameter.
*       else
*         Set the status.
*       end if
*     end if
*     If the status is not ok then
*       Annul the error.
*       Adopt the default value.
*     end if
*     End the error context.
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     20/4/01 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'     ! CAT parametric constants.
*  Arguments Given:
      INTEGER
     :  CI
      CHARACTER
     :  QNAME*(*)
      CHARACTER*(*)
     :  DEFLT
*  Arguments Returned:
      CHARACTER*(*)
     :  VALUE
*  Status:
      INTEGER STATUS   ! Global status.
*  Local Variables:
      INTEGER
     :  QI,            ! Parameter identifier.
     :  IDTYPE         ! Type of identifier.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Set the error context.

         CALL ERR_MARK

*
*       Attempt to get an identifier for the parameter and proceed if ok.

         CALL CAT_TIDNT (CI, QNAME, QI, STATUS)
         IF (STATUS .EQ. SAI__OK) THEN

*
*          Check that the identifier is for a parameter.

            CALL CAT_TIDTP (QI, IDTYPE, STATUS)
            IF (STATUS .EQ. SAI__OK) THEN
               IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*                Attempt to get a value for the parameter.

                  CALL CAT_TIQAC (QI, 'VALUE', VALUE, STATUS)

               ELSE
                  STATUS = SAI__ERROR

               END IF
            END IF
         END IF

*
*       If any error occurred then annul the error and adopt the
*       suggested default value.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)

            VALUE = DEFLT
         END IF

*
*       End the error context.

         CALL ERR_RLSE

      END IF

      END
      SUBROUTINE CAP_OPGTD (CI, QNAME, DEFLT, VALUE, STATUS)
*+
*  Name:
*     CAP_OPGTD
*  Purpose:
*     Optionally attempt to get a parameter from a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_OPGTD (CI, QNAME, DEFLT; VALUE; STATUS)
*  Description:
*     Optionally attempt to get a parameter from a catalogue.
*     If a value cannot be obtained then a supplied default is adopted
*     and no error is reported.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     QNAME  =  CHARACTER*(*) (Given)
*        Name of the parameter in the catalogue (if it exists).
*     DEFLT  =  DOUBLE PRECISION (Given)
*        Supplied for the default.
*     VALUE  =  DOUBLE PRECISION (Returned)
*        Value obtained for the parameter.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set the error context.
*     Attempt to get an identifier for the parameter.
*     If ok then
*       Determine the type of identifier obtained.
*       If the identifier is for a parameter then
*         Attempt to get a value for the parameter.
*       else
*         Set the status.
*       end if
*     end if
*     If the status is not ok then
*       Annul the error.
*       Adopt the default value.
*     end if
*     End the error context.
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     20/4/01 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'     ! CAT parametric constants.
*  Arguments Given:
      INTEGER
     :  CI
      CHARACTER
     :  QNAME*(*)
      DOUBLE PRECISION
     :  DEFLT
*  Arguments Returned:
      DOUBLE PRECISION
     :  VALUE
*  Status:
      INTEGER STATUS   ! Global status.
*  Local Variables:
      INTEGER
     :  QI,            ! Parameter identifier.
     :  IDTYPE         ! Type of identifier.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Set the error context.

         CALL ERR_MARK

*
*       Attempt to get an identifier for the parameter and proceed if ok.

         CALL CAT_TIDNT (CI, QNAME, QI, STATUS)
         IF (STATUS .EQ. SAI__OK) THEN

*
*          Check that the identifier is for a parameter.

            CALL CAT_TIDTP (QI, IDTYPE, STATUS)
            IF (STATUS .EQ. SAI__OK) THEN
               IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*                Attempt to get a value for the parameter.

                  CALL CAT_TIQAD (QI, 'VALUE', VALUE, STATUS)

               ELSE
                  STATUS = SAI__ERROR

               END IF
            END IF
         END IF

*
*       If any error occurred then annul the error and adopt the
*       suggested default value.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)

            VALUE = DEFLT
         END IF

*
*       End the error context.

         CALL ERR_RLSE

      END IF

      END
      SUBROUTINE CAP_OPGTI (CI, QNAME, DEFLT, VALUE, STATUS)
*+
*  Name:
*     CAP_OPGTI
*  Purpose:
*     Optionally attempt to get a parameter from a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_OPGTI (CI, QNAME, DEFLT; VALUE; STATUS)
*  Description:
*     Optionally attempt to get a parameter from a catalogue.
*     If a value cannot be obtained then a supplied default is adopted
*     and no error is reported.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     QNAME  =  CHARACTER*(*) (Given)
*        Name of the parameter in the catalogue (if it exists).
*     DEFLT  =  INTEGER (Given)
*        Supplied for the default.
*     VALUE  =  INTEGER (Returned)
*        Value obtained for the parameter.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set the error context.
*     Attempt to get an identifier for the parameter.
*     If ok then
*       Determine the type of identifier obtained.
*       If the identifier is for a parameter then
*         Attempt to get a value for the parameter.
*       else
*         Set the status.
*       end if
*     end if
*     If the status is not ok then
*       Annul the error.
*       Adopt the default value.
*     end if
*     End the error context.
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     20/4/01 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'     ! CAT parametric constants.
*  Arguments Given:
      INTEGER
     :  CI
      CHARACTER
     :  QNAME*(*)
      INTEGER
     :  DEFLT
*  Arguments Returned:
      INTEGER
     :  VALUE
*  Status:
      INTEGER STATUS   ! Global status.
*  Local Variables:
      INTEGER
     :  QI,            ! Parameter identifier.
     :  IDTYPE         ! Type of identifier.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Set the error context.

         CALL ERR_MARK

*
*       Attempt to get an identifier for the parameter and proceed if ok.

         CALL CAT_TIDNT (CI, QNAME, QI, STATUS)
         IF (STATUS .EQ. SAI__OK) THEN

*
*          Check that the identifier is for a parameter.

            CALL CAT_TIDTP (QI, IDTYPE, STATUS)
            IF (STATUS .EQ. SAI__OK) THEN
               IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*                Attempt to get a value for the parameter.

                  CALL CAT_TIQAI (QI, 'VALUE', VALUE, STATUS)

               ELSE
                  STATUS = SAI__ERROR

               END IF
            END IF
         END IF

*
*       If any error occurred then annul the error and adopt the
*       suggested default value.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)

            VALUE = DEFLT
         END IF

*
*       End the error context.

         CALL ERR_RLSE

      END IF

      END
      SUBROUTINE CAP_OPGTL (CI, QNAME, DEFLT, VALUE, STATUS)
*+
*  Name:
*     CAP_OPGTL
*  Purpose:
*     Optionally attempt to get a parameter from a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_OPGTL (CI, QNAME, DEFLT; VALUE; STATUS)
*  Description:
*     Optionally attempt to get a parameter from a catalogue.
*     If a value cannot be obtained then a supplied default is adopted
*     and no error is reported.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     QNAME  =  CHARACTER*(*) (Given)
*        Name of the parameter in the catalogue (if it exists).
*     DEFLT  =  LOGICAL (Given)
*        Supplied for the default.
*     VALUE  =  LOGICAL (Returned)
*        Value obtained for the parameter.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set the error context.
*     Attempt to get an identifier for the parameter.
*     If ok then
*       Determine the type of identifier obtained.
*       If the identifier is for a parameter then
*         Attempt to get a value for the parameter.
*       else
*         Set the status.
*       end if
*     end if
*     If the status is not ok then
*       Annul the error.
*       Adopt the default value.
*     end if
*     End the error context.
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     20/4/01 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'     ! CAT parametric constants.
*  Arguments Given:
      INTEGER
     :  CI
      CHARACTER
     :  QNAME*(*)
      LOGICAL
     :  DEFLT
*  Arguments Returned:
      LOGICAL
     :  VALUE
*  Status:
      INTEGER STATUS   ! Global status.
*  Local Variables:
      INTEGER
     :  QI,            ! Parameter identifier.
     :  IDTYPE         ! Type of identifier.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Set the error context.

         CALL ERR_MARK

*
*       Attempt to get an identifier for the parameter and proceed if ok.

         CALL CAT_TIDNT (CI, QNAME, QI, STATUS)
         IF (STATUS .EQ. SAI__OK) THEN

*
*          Check that the identifier is for a parameter.

            CALL CAT_TIDTP (QI, IDTYPE, STATUS)
            IF (STATUS .EQ. SAI__OK) THEN
               IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*                Attempt to get a value for the parameter.

                  CALL CAT_TIQAL (QI, 'VALUE', VALUE, STATUS)

               ELSE
                  STATUS = SAI__ERROR

               END IF
            END IF
         END IF

*
*       If any error occurred then annul the error and adopt the
*       suggested default value.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)

            VALUE = DEFLT
         END IF

*
*       End the error context.

         CALL ERR_RLSE

      END IF

      END
      SUBROUTINE CAP_OPGTR (CI, QNAME, DEFLT, VALUE, STATUS)
*+
*  Name:
*     CAP_OPGTR
*  Purpose:
*     Optionally attempt to get a parameter from a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_OPGTR (CI, QNAME, DEFLT; VALUE; STATUS)
*  Description:
*     Optionally attempt to get a parameter from a catalogue.
*     If a value cannot be obtained then a supplied default is adopted
*     and no error is reported.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     QNAME  =  CHARACTER*(*) (Given)
*        Name of the parameter in the catalogue (if it exists).
*     DEFLT  =  REAL (Given)
*        Supplied for the default.
*     VALUE  =  REAL (Returned)
*        Value obtained for the parameter.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set the error context.
*     Attempt to get an identifier for the parameter.
*     If ok then
*       Determine the type of identifier obtained.
*       If the identifier is for a parameter then
*         Attempt to get a value for the parameter.
*       else
*         Set the status.
*       end if
*     end if
*     If the status is not ok then
*       Annul the error.
*       Adopt the default value.
*     end if
*     End the error context.
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     20/4/01 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'     ! CAT parametric constants.
*  Arguments Given:
      INTEGER
     :  CI
      CHARACTER
     :  QNAME*(*)
      REAL
     :  DEFLT
*  Arguments Returned:
      REAL
     :  VALUE
*  Status:
      INTEGER STATUS   ! Global status.
*  Local Variables:
      INTEGER
     :  QI,            ! Parameter identifier.
     :  IDTYPE         ! Type of identifier.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Set the error context.

         CALL ERR_MARK

*
*       Attempt to get an identifier for the parameter and proceed if ok.

         CALL CAT_TIDNT (CI, QNAME, QI, STATUS)
         IF (STATUS .EQ. SAI__OK) THEN

*
*          Check that the identifier is for a parameter.

            CALL CAT_TIDTP (QI, IDTYPE, STATUS)
            IF (STATUS .EQ. SAI__OK) THEN
               IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*                Attempt to get a value for the parameter.

                  CALL CAT_TIQAR (QI, 'VALUE', VALUE, STATUS)

               ELSE
                  STATUS = SAI__ERROR

               END IF
            END IF
         END IF

*
*       If any error occurred then annul the error and adopt the
*       suggested default value.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)

            VALUE = DEFLT
         END IF

*
*       End the error context.

         CALL ERR_RLSE

      END IF

      END
