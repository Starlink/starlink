      SUBROUTINE CAT1_CKMUT (IDTYPE, ATTRIB, MUTBLE, STATUS)
*+
*  Name:
*     CAT1_CKMUT
*  Purpose:
*     Check whether a given attribute is mutable or not.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_CKMUT (IDTYPE, ATTRIB; MUTBLE; STATUS)
*  Description:
*     Check whether a given attribute is mutable or not.
*
*     For parameters and columns there is a pre-defined list of 
*     attributes which are mutable.
*  Arguments:
*     IDTYPE  =  INTEGER (Given)
*        Code for the type of the identifier (parameter, column, etc.)
*        to which the attribute belongs.
*     ATTRIB  =  CHARACTER*(*) (Given)
*        Name of the attribute.
*     MUTBLE  =  LOGICAL (Returned)
*        Flag indicating whether or not the attribute is mutable, coded
*        as follows:
*        .TRUE.  - the attribute is mutable,
*        .FALSE. - the attribute is immutable.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Explicitly check for each of the possibilities.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     5/8/93  (ACD): Original version.
*     31/3/95 (ACD): Added handling of expressions.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      INTEGER
     :  IDTYPE
      CHARACTER
     :  ATTRIB*(*)
*  Arguments Returned:
      LOGICAL
     :  MUTBLE
*  Status:
      INTEGER STATUS             ! Global status
*.

      IF (STATUS .EQ. CAT__OK) THEN

         MUTBLE = .FALSE.

*
*       Columns.

         IF (IDTYPE .EQ. CAT__FITYP) THEN
            IF (ATTRIB .EQ. 'DATE'  .OR.
     :          ATTRIB .EQ. 'UNITS'  .OR.
     :          ATTRIB .EQ. 'EXFMT'  .OR.
     :          ATTRIB .EQ. 'PRFDSP'  .OR.
     :          ATTRIB .EQ. 'COMM') THEN
               MUTBLE = .TRUE.
            END IF

*
*       Parameters.

         ELSE IF (IDTYPE .EQ. CAT__QITYP) THEN
            IF (ATTRIB .EQ. 'DATE'  .OR.
     :          ATTRIB .EQ. 'UNITS'  .OR.
     :          ATTRIB .EQ. 'EXFMT'  .OR.
     :          ATTRIB .EQ. 'PRFDSP'  .OR.
     :          ATTRIB .EQ. 'COMM'  .OR.
     :          ATTRIB .EQ. 'VALUE') THEN
               MUTBLE = .TRUE.
            END IF

*
*       Expressions.

         ELSE IF (IDTYPE .EQ. CAT__EITYP) THEN
            IF (ATTRIB .EQ. 'NAME'  .OR.
     :          ATTRIB .EQ. 'DATE'  .OR.
     :          ATTRIB .EQ. 'UNITS'  .OR.
     :          ATTRIB .EQ. 'EXFMT'  .OR.
     :          ATTRIB .EQ. 'PRFDSP'  .OR.
     :          ATTRIB .EQ. 'COMM') THEN
               MUTBLE = .TRUE.
            END IF
         END IF

      END IF

      END
