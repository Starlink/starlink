      SUBROUTINE CAP_PGETC (CI, QNAME, PARNAM, CANCL, VALUE, STATUS)
*+
*  Name:
*     CAP_PGETC
*  Purpose:
*     Attempt to get a parameter from catalogue or environment.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PGETC (CI, QNAME, PARNAM, CANCL; VALUE; STATUS)
*  Description:
*     Attempt to get a parameter from a catalogue or the environment.
*     Firstly an attempt is made to get the parameter from a catalogue.
*     If the catalogue does not contain a parameter of the required
*     name a value is obtained from the ADAM environment.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     QNAME  =  CHARACTER*(*) (Given)
*        Name of the parameter in the catalogue (if it exists).
*     PARNAM  =  CHARACTER*(*) (Given)
*        Name of the corresponding ADAM environment parameter.
*     CANCL  =  LOGICAL (Given)
*        Flag; cancel the ADAM parameter after the value?  It is
*        coded as follows:
*        .TRUE.  -  cancel the parameter,
*        .FALSE. -  do not cancel the parameter.
*     VALUE  =  CHARACTER*(*) (Returned)
*        Value obtained for the parameter.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to get an identifier for the parameter.
*     If ok then
*       Determine the type of identifier obtained.
*       If the identifier is not for a parameter then
*         Set an error.
*       end if
*     end if
*     If ok then
*       Attempt to get a value for the parameter.
*     else
*       Annul the status.
*       Attempt to get a value from the environment.
*       If required then
*         Cancel the environment parameter.
*       end if
*     end if
*     Report any error.
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     27/5/97 (ACD): Original version.
*     18/4/01 (ACD): Modified to check that the identifier obtained is
*        for a parameter.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'     ! CAT parametric constants.
c      INCLUDE 'CAT_ERR'     ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI
      CHARACTER
     :  QNAME*(*),
     :  PARNAM*(*)
      LOGICAL
     :  CANCL
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
*       Attempt to get an identifier for the parameter and if ok then
*       check its type.  If it does not correspond to a parameter then
*       set the status.

         CALL CAT_TIDNT (CI, QNAME, QI, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            CALL CAT_TIDTP (QI, IDTYPE, STATUS)
            IF (IDTYPE .NE. CAT__QITYP) THEN
               STATUS = SAI__ERROR
            END IF
         END IF

*
*       If all is ok then attempt to get the value of the parameter from
*       the catalogue.  Otherwise annul the error and attempt to get a
*       value from the environment, cancelling it if required.


         IF (STATUS .EQ. SAI__OK) THEN
            CALL CAT_TIQAC (QI, 'VALUE', VALUE, STATUS)

         ELSE
            CALL ERR_ANNUL (STATUS)

            CALL PAR_GET0C (PARNAM, VALUE, STATUS)

            IF (CANCL) THEN
               CALL PAR_CANCL (PARNAM, STATUS)
            END IF
         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_SETC ('QNAME', QNAME)
            CALL ERR_REP ('CAP_PGETC_ERR', 'Failed to obtain '/
     :        /'parameter ^QNAME.', STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAP_PGETD (CI, QNAME, PARNAM, CANCL, VALUE, STATUS)
*+
*  Name:
*     CAP_PGETD
*  Purpose:
*     Attempt to get a parameter from catalogue or environment.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PGETD (CI, QNAME, PARNAM, CANCL; VALUE; STATUS)
*  Description:
*     Attempt to get a parameter from a catalogue or the environment.
*     Firstly an attempt is made to get the parameter from a catalogue.
*     If the catalogue does not contain a parameter of the required
*     name a value is obtained from the ADAM environment.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     QNAME  =  CHARACTER*(*) (Given)
*        Name of the parameter in the catalogue (if it exists).
*     PARNAM  =  CHARACTER*(*) (Given)
*        Name of the corresponding ADAM environment parameter.
*     CANCL  =  LOGICAL (Given)
*        Flag; cancel the ADAM parameter after the value?  It is
*        coded as follows:
*        .TRUE.  -  cancel the parameter,
*        .FALSE. -  do not cancel the parameter.
*     VALUE  =  DOUBLE PRECISION (Returned)
*        Value obtained for the parameter.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to get an identifier for the parameter.
*     If ok then
*       Determine the type of identifier obtained.
*       If the identifier is not for a parameter then
*         Set an error.
*       end if
*     end if
*     If ok then
*       Attempt to get a value for the parameter.
*     else
*       Annul the status.
*       Attempt to get a value from the environment.
*       If required then
*         Cancel the environment parameter.
*       end if
*     end if
*     Report any error.
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     27/5/97 (ACD): Original version.
*     18/4/01 (ACD): Modified to check that the identifier obtained is
*        for a parameter.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'     ! CAT parametric constants.
c      INCLUDE 'CAT_ERR'     ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI
      CHARACTER
     :  QNAME*(*),
     :  PARNAM*(*)
      LOGICAL
     :  CANCL
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
*       Attempt to get an identifier for the parameter and if ok then
*       check its type.  If it does not correspond to a parameter then
*       set the status.

         CALL CAT_TIDNT (CI, QNAME, QI, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            CALL CAT_TIDTP (QI, IDTYPE, STATUS)
            IF (IDTYPE .NE. CAT__QITYP) THEN
               STATUS = SAI__ERROR
            END IF
         END IF

*
*       If all is ok then attempt to get the value of the parameter from
*       the catalogue.  Otherwise annul the error and attempt to get a
*       value from the environment, cancelling it if required.


         IF (STATUS .EQ. SAI__OK) THEN
            CALL CAT_TIQAD (QI, 'VALUE', VALUE, STATUS)

         ELSE
            CALL ERR_ANNUL (STATUS)

            CALL PAR_GET0D (PARNAM, VALUE, STATUS)

            IF (CANCL) THEN
               CALL PAR_CANCL (PARNAM, STATUS)
            END IF
         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_SETC ('QNAME', QNAME)
            CALL ERR_REP ('CAP_PGETD_ERR', 'Failed to obtain '/
     :        /'parameter ^QNAME.', STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAP_PGETI (CI, QNAME, PARNAM, CANCL, VALUE, STATUS)
*+
*  Name:
*     CAP_PGETI
*  Purpose:
*     Attempt to get a parameter from catalogue or environment.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PGETI (CI, QNAME, PARNAM, CANCL; VALUE; STATUS)
*  Description:
*     Attempt to get a parameter from a catalogue or the environment.
*     Firstly an attempt is made to get the parameter from a catalogue.
*     If the catalogue does not contain a parameter of the required
*     name a value is obtained from the ADAM environment.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     QNAME  =  CHARACTER*(*) (Given)
*        Name of the parameter in the catalogue (if it exists).
*     PARNAM  =  CHARACTER*(*) (Given)
*        Name of the corresponding ADAM environment parameter.
*     CANCL  =  LOGICAL (Given)
*        Flag; cancel the ADAM parameter after the value?  It is
*        coded as follows:
*        .TRUE.  -  cancel the parameter,
*        .FALSE. -  do not cancel the parameter.
*     VALUE  =  INTEGER (Returned)
*        Value obtained for the parameter.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to get an identifier for the parameter.
*     If ok then
*       Determine the type of identifier obtained.
*       If the identifier is not for a parameter then
*         Set an error.
*       end if
*     end if
*     If ok then
*       Attempt to get a value for the parameter.
*     else
*       Annul the status.
*       Attempt to get a value from the environment.
*       If required then
*         Cancel the environment parameter.
*       end if
*     end if
*     Report any error.
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     27/5/97 (ACD): Original version.
*     18/4/01 (ACD): Modified to check that the identifier obtained is
*        for a parameter.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'     ! CAT parametric constants.
c      INCLUDE 'CAT_ERR'     ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI
      CHARACTER
     :  QNAME*(*),
     :  PARNAM*(*)
      LOGICAL
     :  CANCL
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
*       Attempt to get an identifier for the parameter and if ok then
*       check its type.  If it does not correspond to a parameter then
*       set the status.

         CALL CAT_TIDNT (CI, QNAME, QI, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            CALL CAT_TIDTP (QI, IDTYPE, STATUS)
            IF (IDTYPE .NE. CAT__QITYP) THEN
               STATUS = SAI__ERROR
            END IF
         END IF

*
*       If all is ok then attempt to get the value of the parameter from
*       the catalogue.  Otherwise annul the error and attempt to get a
*       value from the environment, cancelling it if required.


         IF (STATUS .EQ. SAI__OK) THEN
            CALL CAT_TIQAI (QI, 'VALUE', VALUE, STATUS)

         ELSE
            CALL ERR_ANNUL (STATUS)

            CALL PAR_GET0I (PARNAM, VALUE, STATUS)

            IF (CANCL) THEN
               CALL PAR_CANCL (PARNAM, STATUS)
            END IF
         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_SETC ('QNAME', QNAME)
            CALL ERR_REP ('CAP_PGETI_ERR', 'Failed to obtain '/
     :        /'parameter ^QNAME.', STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAP_PGETL (CI, QNAME, PARNAM, CANCL, VALUE, STATUS)
*+
*  Name:
*     CAP_PGETL
*  Purpose:
*     Attempt to get a parameter from catalogue or environment.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PGETL (CI, QNAME, PARNAM, CANCL; VALUE; STATUS)
*  Description:
*     Attempt to get a parameter from a catalogue or the environment.
*     Firstly an attempt is made to get the parameter from a catalogue.
*     If the catalogue does not contain a parameter of the required
*     name a value is obtained from the ADAM environment.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     QNAME  =  CHARACTER*(*) (Given)
*        Name of the parameter in the catalogue (if it exists).
*     PARNAM  =  CHARACTER*(*) (Given)
*        Name of the corresponding ADAM environment parameter.
*     CANCL  =  LOGICAL (Given)
*        Flag; cancel the ADAM parameter after the value?  It is
*        coded as follows:
*        .TRUE.  -  cancel the parameter,
*        .FALSE. -  do not cancel the parameter.
*     VALUE  =  LOGICAL (Returned)
*        Value obtained for the parameter.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to get an identifier for the parameter.
*     If ok then
*       Determine the type of identifier obtained.
*       If the identifier is not for a parameter then
*         Set an error.
*       end if
*     end if
*     If ok then
*       Attempt to get a value for the parameter.
*     else
*       Annul the status.
*       Attempt to get a value from the environment.
*       If required then
*         Cancel the environment parameter.
*       end if
*     end if
*     Report any error.
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     27/5/97 (ACD): Original version.
*     18/4/01 (ACD): Modified to check that the identifier obtained is
*        for a parameter.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'     ! CAT parametric constants.
c      INCLUDE 'CAT_ERR'     ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI
      CHARACTER
     :  QNAME*(*),
     :  PARNAM*(*)
      LOGICAL
     :  CANCL
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
*       Attempt to get an identifier for the parameter and if ok then
*       check its type.  If it does not correspond to a parameter then
*       set the status.

         CALL CAT_TIDNT (CI, QNAME, QI, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            CALL CAT_TIDTP (QI, IDTYPE, STATUS)
            IF (IDTYPE .NE. CAT__QITYP) THEN
               STATUS = SAI__ERROR
            END IF
         END IF

*
*       If all is ok then attempt to get the value of the parameter from
*       the catalogue.  Otherwise annul the error and attempt to get a
*       value from the environment, cancelling it if required.


         IF (STATUS .EQ. SAI__OK) THEN
            CALL CAT_TIQAL (QI, 'VALUE', VALUE, STATUS)

         ELSE
            CALL ERR_ANNUL (STATUS)

            CALL PAR_GET0L (PARNAM, VALUE, STATUS)

            IF (CANCL) THEN
               CALL PAR_CANCL (PARNAM, STATUS)
            END IF
         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_SETC ('QNAME', QNAME)
            CALL ERR_REP ('CAP_PGETL_ERR', 'Failed to obtain '/
     :        /'parameter ^QNAME.', STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAP_PGETR (CI, QNAME, PARNAM, CANCL, VALUE, STATUS)
*+
*  Name:
*     CAP_PGETR
*  Purpose:
*     Attempt to get a parameter from catalogue or environment.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PGETR (CI, QNAME, PARNAM, CANCL; VALUE; STATUS)
*  Description:
*     Attempt to get a parameter from a catalogue or the environment.
*     Firstly an attempt is made to get the parameter from a catalogue.
*     If the catalogue does not contain a parameter of the required
*     name a value is obtained from the ADAM environment.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     QNAME  =  CHARACTER*(*) (Given)
*        Name of the parameter in the catalogue (if it exists).
*     PARNAM  =  CHARACTER*(*) (Given)
*        Name of the corresponding ADAM environment parameter.
*     CANCL  =  LOGICAL (Given)
*        Flag; cancel the ADAM parameter after the value?  It is
*        coded as follows:
*        .TRUE.  -  cancel the parameter,
*        .FALSE. -  do not cancel the parameter.
*     VALUE  =  REAL (Returned)
*        Value obtained for the parameter.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to get an identifier for the parameter.
*     If ok then
*       Determine the type of identifier obtained.
*       If the identifier is not for a parameter then
*         Set an error.
*       end if
*     end if
*     If ok then
*       Attempt to get a value for the parameter.
*     else
*       Annul the status.
*       Attempt to get a value from the environment.
*       If required then
*         Cancel the environment parameter.
*       end if
*     end if
*     Report any error.
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     27/5/97 (ACD): Original version.
*     18/4/01 (ACD): Modified to check that the identifier obtained is
*        for a parameter.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'     ! CAT parametric constants.
c      INCLUDE 'CAT_ERR'     ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI
      CHARACTER
     :  QNAME*(*),
     :  PARNAM*(*)
      LOGICAL
     :  CANCL
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
*       Attempt to get an identifier for the parameter and if ok then
*       check its type.  If it does not correspond to a parameter then
*       set the status.

         CALL CAT_TIDNT (CI, QNAME, QI, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            CALL CAT_TIDTP (QI, IDTYPE, STATUS)
            IF (IDTYPE .NE. CAT__QITYP) THEN
               STATUS = SAI__ERROR
            END IF
         END IF

*
*       If all is ok then attempt to get the value of the parameter from
*       the catalogue.  Otherwise annul the error and attempt to get a
*       value from the environment, cancelling it if required.


         IF (STATUS .EQ. SAI__OK) THEN
            CALL CAT_TIQAR (QI, 'VALUE', VALUE, STATUS)

         ELSE
            CALL ERR_ANNUL (STATUS)

            CALL PAR_GET0R (PARNAM, VALUE, STATUS)

            IF (CANCL) THEN
               CALL PAR_CANCL (PARNAM, STATUS)
            END IF
         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_SETC ('QNAME', QNAME)
            CALL ERR_REP ('CAP_PGETR_ERR', 'Failed to obtain '/
     :        /'parameter ^QNAME.', STATUS)
         END IF

      END IF

      END
