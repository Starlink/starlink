      SUBROUTINE ANT_FGETB (CID, NROW, NULL, VALUE, STATUS)
*+
*  Name:
*     ANT_FGETB
*  Purpose:
*     Get information for a field in the current row or a parameter.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL ANT_FGETB (CID, NROW; NULL, VALUE; STATUS)
*  Description:
*     Get information for a field in the current row or a parameter.
*
*     Note: only D, I and C versions of this routine exist.
*  Arguments:
*     CID  =  INTEGER (ENTRY)
*        Column or parameter identifier.
*     NROW  =  INTEGER (ENTRY)
*        Row for which the value is to be retrieved.
*     NULL  =  LOGICAL (EXIT)
*        Flag indicating whether the value was null or not.
*     VALUE  =  BYTE (EXIT)
*        Value obtained for the field or parameter.
*     STATUS = INTEGER (UPDATE)
*        The global status.
*  Prior Requirements:
*     <...>
*  Side Effects:
*     <...>
*  Algorithm:
*     Get the type of the identifier.
*     If the identifier is a column then
*       Get the value for the field of the column in the current row.
*     else if the identifier is a vector column element then
*       Get the identifier for the vector column to which the identifier
*       refers.
*       Get the vector element to which the identifier refers.
*       Get the value of the vector column element.
*     else if the identifier is a parameter then
*       Get the value of the parameter.
*     else
*       Set the null flag.
*     end if
*  Implementation Deficiencies:
*     <...>

*  Copyright:
*     Copyright (C) 1993, 1994 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     24/6/93 (ACD): Original version.
*     2/2/94  (ACD): Added vector column elements.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CID,
     :  NROW
*  Arguments Returned:
      LOGICAL
     :  NULL
      BYTE
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  IDTYPE,  ! Type of the identifier.
     :  CIB,     ! Identifier of base column for vector column element.
     :  ELEM     ! Vector element corresponding to vector identifier.
*.

      IF (STATUS .EQ. 0) THEN

*
*       Get the type of the identifier.

         CALL CAT_TIDTP (CID, IDTYPE, STATUS)

         IF (IDTYPE .EQ. CAT__FITYP) THEN

*
*          The identifier is a column.  Get the value for the field
*          in the requested row.

            CALL CAT1_FIOB (.TRUE., CID, 1, NROW, VALUE, NULL, STATUS)

         ELSE IF (IDTYPE .EQ. CAT__FETYP) THEN

*
*          The identifier is a vector column element.  Determine the
*          identifier of the base column and the array element to which
*          it corresponds, and then get the value.

            CALL CAT_TIQAI (CID, 'BASEID', CIB, STATUS)
            CALL CAT_TIQAI (CID, 'ELEM', ELEM, STATUS)

            CALL CAT1_FIOB (.TRUE., CIB, ELEM, NROW, VALUE, NULL,
     :        STATUS)

         ELSE IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*          The identifier is a parameter; get its value.

            CALL CAT_TIQAB (CID, 'VALUE', VALUE, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN
               NULL = .FALSE.
            ELSE
               NULL = .TRUE.
            END IF

         ELSE

*
*          The identifier is neither a column nor a parameter; set
*          null flag.

            NULL = .TRUE.

         END IF

      END IF

      END
      SUBROUTINE ANT_FGETC (CID, NROW, NULL, VALUE, STATUS)
*+
*  Name:
*     ANT_FGETC
*  Purpose:
*     Get information for a field in the current row or a parameter.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL ANT_FGETC (CID, NROW; NULL, VALUE; STATUS)
*  Description:
*     Get information for a field in the current row or a parameter.
*
*     Note: only D, I and C versions of this routine exist.
*  Arguments:
*     CID  =  INTEGER (ENTRY)
*        Column or parameter identifier.
*     NROW  =  INTEGER (ENTRY)
*        Row for which the value is to be retrieved.
*     NULL  =  LOGICAL (EXIT)
*        Flag indicating whether the value was null or not.
*     VALUE  =  CHARACTER*(*) (EXIT)
*        Value obtained for the field or parameter.
*     STATUS = INTEGER (UPDATE)
*        The global status.
*  Prior Requirements:
*     <...>
*  Side Effects:
*     <...>
*  Algorithm:
*     Get the type of the identifier.
*     If the identifier is a column then
*       Get the value for the field of the column in the current row.
*     else if the identifier is a vector column element then
*       Get the identifier for the vector column to which the identifier
*       refers.
*       Get the vector element to which the identifier refers.
*       Get the value of the vector column element.
*     else if the identifier is a parameter then
*       Get the value of the parameter.
*     else
*       Set the null flag.
*     end if
*  Implementation Deficiencies:
*     <...>
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     24/6/93 (ACD): Original version.
*     2/2/94  (ACD): Added vector column elements.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CID,
     :  NROW
*  Arguments Returned:
      LOGICAL
     :  NULL
      CHARACTER*(*)
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  IDTYPE,  ! Type of the identifier.
     :  CIB,     ! Identifier of base column for vector column element.
     :  ELEM     ! Vector element corresponding to vector identifier.
*.

      IF (STATUS .EQ. 0) THEN

*
*       Get the type of the identifier.

         CALL CAT_TIDTP (CID, IDTYPE, STATUS)

         IF (IDTYPE .EQ. CAT__FITYP) THEN

*
*          The identifier is a column.  Get the value for the field
*          in the requested row.

            CALL CAT1_FIOC (.TRUE., CID, 1, NROW, VALUE, NULL, STATUS)

         ELSE IF (IDTYPE .EQ. CAT__FETYP) THEN

*
*          The identifier is a vector column element.  Determine the
*          identifier of the base column and the array element to which
*          it corresponds, and then get the value.

            CALL CAT_TIQAI (CID, 'BASEID', CIB, STATUS)
            CALL CAT_TIQAI (CID, 'ELEM', ELEM, STATUS)

            CALL CAT1_FIOC (.TRUE., CIB, ELEM, NROW, VALUE, NULL,
     :        STATUS)

         ELSE IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*          The identifier is a parameter; get its value.

            CALL CAT_TIQAC (CID, 'VALUE', VALUE, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN
               NULL = .FALSE.
            ELSE
               NULL = .TRUE.
            END IF

         ELSE

*
*          The identifier is neither a column nor a parameter; set
*          null flag.

            NULL = .TRUE.

         END IF

      END IF

      END
      SUBROUTINE ANT_FGETD (CID, NROW, NULL, VALUE, STATUS)
*+
*  Name:
*     ANT_FGETD
*  Purpose:
*     Get information for a field in the current row or a parameter.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL ANT_FGETD (CID, NROW; NULL, VALUE; STATUS)
*  Description:
*     Get information for a field in the current row or a parameter.
*
*     Note: only D, I and C versions of this routine exist.
*  Arguments:
*     CID  =  INTEGER (ENTRY)
*        Column or parameter identifier.
*     NROW  =  INTEGER (ENTRY)
*        Row for which the value is to be retrieved.
*     NULL  =  LOGICAL (EXIT)
*        Flag indicating whether the value was null or not.
*     VALUE  =  DOUBLE PRECISION (EXIT)
*        Value obtained for the field or parameter.
*     STATUS = INTEGER (UPDATE)
*        The global status.
*  Prior Requirements:
*     <...>
*  Side Effects:
*     <...>
*  Algorithm:
*     Get the type of the identifier.
*     If the identifier is a column then
*       Get the value for the field of the column in the current row.
*     else if the identifier is a vector column element then
*       Get the identifier for the vector column to which the identifier
*       refers.
*       Get the vector element to which the identifier refers.
*       Get the value of the vector column element.
*     else if the identifier is a parameter then
*       Get the value of the parameter.
*     else
*       Set the null flag.
*     end if
*  Implementation Deficiencies:
*     <...>
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     24/6/93 (ACD): Original version.
*     2/2/94  (ACD): Added vector column elements.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CID,
     :  NROW
*  Arguments Returned:
      LOGICAL
     :  NULL
      DOUBLE PRECISION
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  IDTYPE,  ! Type of the identifier.
     :  CIB,     ! Identifier of base column for vector column element.
     :  ELEM     ! Vector element corresponding to vector identifier.
*.

      IF (STATUS .EQ. 0) THEN

*
*       Get the type of the identifier.

         CALL CAT_TIDTP (CID, IDTYPE, STATUS)

         IF (IDTYPE .EQ. CAT__FITYP) THEN

*
*          The identifier is a column.  Get the value for the field
*          in the requested row.

            CALL CAT1_FIOD (.TRUE., CID, 1, NROW, VALUE, NULL, STATUS)

         ELSE IF (IDTYPE .EQ. CAT__FETYP) THEN

*
*          The identifier is a vector column element.  Determine the
*          identifier of the base column and the array element to which
*          it corresponds, and then get the value.

            CALL CAT_TIQAI (CID, 'BASEID', CIB, STATUS)
            CALL CAT_TIQAI (CID, 'ELEM', ELEM, STATUS)

            CALL CAT1_FIOD (.TRUE., CIB, ELEM, NROW, VALUE, NULL,
     :        STATUS)

         ELSE IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*          The identifier is a parameter; get its value.

            CALL CAT_TIQAD (CID, 'VALUE', VALUE, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN
               NULL = .FALSE.
            ELSE
               NULL = .TRUE.
            END IF

         ELSE

*
*          The identifier is neither a column nor a parameter; set
*          null flag.

            NULL = .TRUE.

         END IF

      END IF

      END
      SUBROUTINE ANT_FGETI (CID, NROW, NULL, VALUE, STATUS)
*+
*  Name:
*     ANT_FGETI
*  Purpose:
*     Get information for a field in the current row or a parameter.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL ANT_FGETI (CID, NROW; NULL, VALUE; STATUS)
*  Description:
*     Get information for a field in the current row or a parameter.
*
*     Note: only D, I and C versions of this routine exist.
*  Arguments:
*     CID  =  INTEGER (ENTRY)
*        Column or parameter identifier.
*     NROW  =  INTEGER (ENTRY)
*        Row for which the value is to be retrieved.
*     NULL  =  LOGICAL (EXIT)
*        Flag indicating whether the value was null or not.
*     VALUE  =  INTEGER (EXIT)
*        Value obtained for the field or parameter.
*     STATUS = INTEGER (UPDATE)
*        The global status.
*  Prior Requirements:
*     <...>
*  Side Effects:
*     <...>
*  Algorithm:
*     Get the type of the identifier.
*     If the identifier is a column then
*       Get the value for the field of the column in the current row.
*     else if the identifier is a vector column element then
*       Get the identifier for the vector column to which the identifier
*       refers.
*       Get the vector element to which the identifier refers.
*       Get the value of the vector column element.
*     else if the identifier is a parameter then
*       Get the value of the parameter.
*     else
*       Set the null flag.
*     end if
*  Implementation Deficiencies:
*     <...>
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     24/6/93 (ACD): Original version.
*     2/2/94  (ACD): Added vector column elements.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CID,
     :  NROW
*  Arguments Returned:
      LOGICAL
     :  NULL
      INTEGER
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  IDTYPE,  ! Type of the identifier.
     :  CIB,     ! Identifier of base column for vector column element.
     :  ELEM     ! Vector element corresponding to vector identifier.
*.

      IF (STATUS .EQ. 0) THEN

*
*       Get the type of the identifier.

         CALL CAT_TIDTP (CID, IDTYPE, STATUS)

         IF (IDTYPE .EQ. CAT__FITYP) THEN

*
*          The identifier is a column.  Get the value for the field
*          in the requested row.

            CALL CAT1_FIOI (.TRUE., CID, 1, NROW, VALUE, NULL, STATUS)

         ELSE IF (IDTYPE .EQ. CAT__FETYP) THEN

*
*          The identifier is a vector column element.  Determine the
*          identifier of the base column and the array element to which
*          it corresponds, and then get the value.

            CALL CAT_TIQAI (CID, 'BASEID', CIB, STATUS)
            CALL CAT_TIQAI (CID, 'ELEM', ELEM, STATUS)

            CALL CAT1_FIOI (.TRUE., CIB, ELEM, NROW, VALUE, NULL,
     :        STATUS)

         ELSE IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*          The identifier is a parameter; get its value.

            CALL CAT_TIQAI (CID, 'VALUE', VALUE, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN
               NULL = .FALSE.
            ELSE
               NULL = .TRUE.
            END IF

         ELSE

*
*          The identifier is neither a column nor a parameter; set
*          null flag.

            NULL = .TRUE.

         END IF

      END IF

      END
      SUBROUTINE ANT_FGETL (CID, NROW, NULL, VALUE, STATUS)
*+
*  Name:
*     ANT_FGETL
*  Purpose:
*     Get information for a field in the current row or a parameter.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL ANT_FGETL (CID, NROW; NULL, VALUE; STATUS)
*  Description:
*     Get information for a field in the current row or a parameter.
*
*     Note: only D, I and C versions of this routine exist.
*  Arguments:
*     CID  =  INTEGER (ENTRY)
*        Column or parameter identifier.
*     NROW  =  INTEGER (ENTRY)
*        Row for which the value is to be retrieved.
*     NULL  =  LOGICAL (EXIT)
*        Flag indicating whether the value was null or not.
*     VALUE  =  LOGICAL (EXIT)
*        Value obtained for the field or parameter.
*     STATUS = INTEGER (UPDATE)
*        The global status.
*  Prior Requirements:
*     <...>
*  Side Effects:
*     <...>
*  Algorithm:
*     Get the type of the identifier.
*     If the identifier is a column then
*       Get the value for the field of the column in the current row.
*     else if the identifier is a vector column element then
*       Get the identifier for the vector column to which the identifier
*       refers.
*       Get the vector element to which the identifier refers.
*       Get the value of the vector column element.
*     else if the identifier is a parameter then
*       Get the value of the parameter.
*     else
*       Set the null flag.
*     end if
*  Implementation Deficiencies:
*     <...>
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     24/6/93 (ACD): Original version.
*     2/2/94  (ACD): Added vector column elements.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CID,
     :  NROW
*  Arguments Returned:
      LOGICAL
     :  NULL
      LOGICAL
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  IDTYPE,  ! Type of the identifier.
     :  CIB,     ! Identifier of base column for vector column element.
     :  ELEM     ! Vector element corresponding to vector identifier.
*.

      IF (STATUS .EQ. 0) THEN

*
*       Get the type of the identifier.

         CALL CAT_TIDTP (CID, IDTYPE, STATUS)

         IF (IDTYPE .EQ. CAT__FITYP) THEN

*
*          The identifier is a column.  Get the value for the field
*          in the requested row.

            CALL CAT1_FIOL (.TRUE., CID, 1, NROW, VALUE, NULL, STATUS)

         ELSE IF (IDTYPE .EQ. CAT__FETYP) THEN

*
*          The identifier is a vector column element.  Determine the
*          identifier of the base column and the array element to which
*          it corresponds, and then get the value.

            CALL CAT_TIQAI (CID, 'BASEID', CIB, STATUS)
            CALL CAT_TIQAI (CID, 'ELEM', ELEM, STATUS)

            CALL CAT1_FIOL (.TRUE., CIB, ELEM, NROW, VALUE, NULL,
     :        STATUS)

         ELSE IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*          The identifier is a parameter; get its value.

            CALL CAT_TIQAL (CID, 'VALUE', VALUE, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN
               NULL = .FALSE.
            ELSE
               NULL = .TRUE.
            END IF

         ELSE

*
*          The identifier is neither a column nor a parameter; set
*          null flag.

            NULL = .TRUE.

         END IF

      END IF

      END
      SUBROUTINE ANT_FGETR (CID, NROW, NULL, VALUE, STATUS)
*+
*  Name:
*     ANT_FGETR
*  Purpose:
*     Get information for a field in the current row or a parameter.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL ANT_FGETR (CID, NROW; NULL, VALUE; STATUS)
*  Description:
*     Get information for a field in the current row or a parameter.
*
*     Note: only D, I and C versions of this routine exist.
*  Arguments:
*     CID  =  INTEGER (ENTRY)
*        Column or parameter identifier.
*     NROW  =  INTEGER (ENTRY)
*        Row for which the value is to be retrieved.
*     NULL  =  LOGICAL (EXIT)
*        Flag indicating whether the value was null or not.
*     VALUE  =  REAL (EXIT)
*        Value obtained for the field or parameter.
*     STATUS = INTEGER (UPDATE)
*        The global status.
*  Prior Requirements:
*     <...>
*  Side Effects:
*     <...>
*  Algorithm:
*     Get the type of the identifier.
*     If the identifier is a column then
*       Get the value for the field of the column in the current row.
*     else if the identifier is a vector column element then
*       Get the identifier for the vector column to which the identifier
*       refers.
*       Get the vector element to which the identifier refers.
*       Get the value of the vector column element.
*     else if the identifier is a parameter then
*       Get the value of the parameter.
*     else
*       Set the null flag.
*     end if
*  Implementation Deficiencies:
*     <...>
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     24/6/93 (ACD): Original version.
*     2/2/94  (ACD): Added vector column elements.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CID,
     :  NROW
*  Arguments Returned:
      LOGICAL
     :  NULL
      REAL
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  IDTYPE,  ! Type of the identifier.
     :  CIB,     ! Identifier of base column for vector column element.
     :  ELEM     ! Vector element corresponding to vector identifier.
*.

      IF (STATUS .EQ. 0) THEN

*
*       Get the type of the identifier.

         CALL CAT_TIDTP (CID, IDTYPE, STATUS)

         IF (IDTYPE .EQ. CAT__FITYP) THEN

*
*          The identifier is a column.  Get the value for the field
*          in the requested row.

            CALL CAT1_FIOR (.TRUE., CID, 1, NROW, VALUE, NULL, STATUS)

         ELSE IF (IDTYPE .EQ. CAT__FETYP) THEN

*
*          The identifier is a vector column element.  Determine the
*          identifier of the base column and the array element to which
*          it corresponds, and then get the value.

            CALL CAT_TIQAI (CID, 'BASEID', CIB, STATUS)
            CALL CAT_TIQAI (CID, 'ELEM', ELEM, STATUS)

            CALL CAT1_FIOR (.TRUE., CIB, ELEM, NROW, VALUE, NULL,
     :        STATUS)

         ELSE IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*          The identifier is a parameter; get its value.

            CALL CAT_TIQAR (CID, 'VALUE', VALUE, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN
               NULL = .FALSE.
            ELSE
               NULL = .TRUE.
            END IF

         ELSE

*
*          The identifier is neither a column nor a parameter; set
*          null flag.

            NULL = .TRUE.

         END IF

      END IF

      END
      SUBROUTINE ANT_FGETW (CID, NROW, NULL, VALUE, STATUS)
*+
*  Name:
*     ANT_FGETW
*  Purpose:
*     Get information for a field in the current row or a parameter.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL ANT_FGETW (CID, NROW; NULL, VALUE; STATUS)
*  Description:
*     Get information for a field in the current row or a parameter.
*
*     Note: only D, I and C versions of this routine exist.
*  Arguments:
*     CID  =  INTEGER (ENTRY)
*        Column or parameter identifier.
*     NROW  =  INTEGER (ENTRY)
*        Row for which the value is to be retrieved.
*     NULL  =  LOGICAL (EXIT)
*        Flag indicating whether the value was null or not.
*     VALUE  =  INTEGER*2 (EXIT)
*        Value obtained for the field or parameter.
*     STATUS = INTEGER (UPDATE)
*        The global status.
*  Prior Requirements:
*     <...>
*  Side Effects:
*     <...>
*  Algorithm:
*     Get the type of the identifier.
*     If the identifier is a column then
*       Get the value for the field of the column in the current row.
*     else if the identifier is a vector column element then
*       Get the identifier for the vector column to which the identifier
*       refers.
*       Get the vector element to which the identifier refers.
*       Get the value of the vector column element.
*     else if the identifier is a parameter then
*       Get the value of the parameter.
*     else
*       Set the null flag.
*     end if
*  Implementation Deficiencies:
*     <...>
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     24/6/93 (ACD): Original version.
*     2/2/94  (ACD): Added vector column elements.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CID,
     :  NROW
*  Arguments Returned:
      LOGICAL
     :  NULL
      INTEGER*2
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  IDTYPE,  ! Type of the identifier.
     :  CIB,     ! Identifier of base column for vector column element.
     :  ELEM     ! Vector element corresponding to vector identifier.
*.

      IF (STATUS .EQ. 0) THEN

*
*       Get the type of the identifier.

         CALL CAT_TIDTP (CID, IDTYPE, STATUS)

         IF (IDTYPE .EQ. CAT__FITYP) THEN

*
*          The identifier is a column.  Get the value for the field
*          in the requested row.

            CALL CAT1_FIOW (.TRUE., CID, 1, NROW, VALUE, NULL, STATUS)

         ELSE IF (IDTYPE .EQ. CAT__FETYP) THEN

*
*          The identifier is a vector column element.  Determine the
*          identifier of the base column and the array element to which
*          it corresponds, and then get the value.

            CALL CAT_TIQAI (CID, 'BASEID', CIB, STATUS)
            CALL CAT_TIQAI (CID, 'ELEM', ELEM, STATUS)

            CALL CAT1_FIOW (.TRUE., CIB, ELEM, NROW, VALUE, NULL,
     :        STATUS)

         ELSE IF (IDTYPE .EQ. CAT__QITYP) THEN

*
*          The identifier is a parameter; get its value.

            CALL CAT_TIQAW (CID, 'VALUE', VALUE, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN
               NULL = .FALSE.
            ELSE
               NULL = .TRUE.
            END IF

         ELSE

*
*          The identifier is neither a column nor a parameter; set
*          null flag.

            NULL = .TRUE.

         END IF

      END IF

      END
