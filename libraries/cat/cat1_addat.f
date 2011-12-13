      SUBROUTINE CAT1_ADDAB (ID, NAME, MUTBLE, VALUE, STATUS)
*+
*  Name:
*     CAT1_ADDAB
*  Purpose:
*     Add an attribute of type B to the list of atributes.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ADDAB (ID, NAME, MUTBLE, VALUE; STATUS)
*  Description:
*     Add an attribute of type B to the list of atributes.
*  Arguments:
*     ID  =  INTEGER (Given)
*         Identifier to the component to which the attribute belongs.
*     NAME  =  CHARACTER*(*) (Given)
*         Name of the attribute.
*     MUTBLE  =  LOGICAL  (Given)
*         Flag indicating whether or not the attribute is mutable.
*     VALUE  =  BYTE  (Given)
*         Value of the attribute.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there is space in the attribute arrays then
*       Add the details for the attribute
*     else
*       Set the return status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
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
*     4/7/93  (ACD): First working version.
*     23/1/94 (ACD): Modified error reporting.
*     6/3/95  (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     27/5/98 (ACD): Removed individual counts for attribute values of
*        differing data types.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT1_ATTRB_CMN'    ! Attributes common block.
*  Arguments Given:
      INTEGER
     :  ID
      CHARACTER*(*)
     :  NAME
      LOGICAL
     :  MUTBLE
      BYTE
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*.

      IF (STATUS .EQ. CAT__OK) THEN

C        write(17, 2000) id, name, mutble
C2000    format(1x, 'id, name, mutble: ', I6, 1X, A, 1X, L5 )
C        write(17, 2001) natt__cat1, nattB__cat1
C2001    format(1x, 'natt__cat1, nattB__cat1: ', I6, I6 )

*
*       Check whether there is space for any more attributes, both in
*       total and of the appropriate data type.

         IF (NATT__CAT1    .LT.  CAT1__MXATT) THEN

*
*          Increment the number of attributes, both in total and of the
*          appropriate data type.

            NATT__CAT1 = NATT__CAT1 + 1

*
*          Add the details for the attribute.

            ATTID__CAT1(NATT__CAT1) = ID
            ATTNM__CAT1(NATT__CAT1) = NAME
            ATTMU__CAT1(NATT__CAT1) = MUTBLE
            ATTYP__CAT1(NATT__CAT1) = CAT__TYPEB

            ATTVB__CAT1(NATT__CAT1) = VALUE

         ELSE

*
*          Unable to create the attribute; set the status and report an
*          error.

            STATUS = CAT__MAXAT

            CALL CAT1_ERREP ('CAT1_ADDAB_MAT', 'Error adding an '/
     :        /'attribute to the attribute list.', STATUS)

         END IF

      END IF

      END
      SUBROUTINE CAT1_ADDAC (ID, NAME, MUTBLE, VALUE, STATUS)
*+
*  Name:
*     CAT1_ADDAC
*  Purpose:
*     Add an attribute of type C to the list of atributes.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ADDAC (ID, NAME, MUTBLE, VALUE; STATUS)
*  Description:
*     Add an attribute of type C to the list of atributes.
*  Arguments:
*     ID  =  INTEGER (Given)
*         Identifier to the component to which the attribute belongs.
*     NAME  =  CHARACTER*(*) (Given)
*         Name of the attribute.
*     MUTBLE  =  LOGICAL  (Given)
*         Flag indicating whether or not the attribute is mutable.
*     VALUE  =  CHARACTER*(*)  (Given)
*         Value of the attribute.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there is space in the attribute arrays then
*       Add the details for the attribute
*     else
*       Set the return status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     24/6/93 (ACD): Original version.
*     4/7/93  (ACD): First working version.
*     23/1/94 (ACD): Modified error reporting.
*     6/3/95  (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     27/5/98 (ACD): Removed individual counts for attribute values of
*        differing data types.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT1_ATTRB_CMN'    ! Attributes common block.
*  Arguments Given:
      INTEGER
     :  ID
      CHARACTER*(*)
     :  NAME
      LOGICAL
     :  MUTBLE
      CHARACTER*(*)
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*.

      IF (STATUS .EQ. CAT__OK) THEN

C        write(17, 2000) id, name, mutble
C2000    format(1x, 'id, name, mutble: ', I6, 1X, A, 1X, L5 )
C        write(17, 2001) natt__cat1, nattC__cat1
C2001    format(1x, 'natt__cat1, nattC__cat1: ', I6, I6 )

*
*       Check whether there is space for any more attributes, both in
*       total and of the appropriate data type.

         IF (NATT__CAT1    .LT.  CAT1__MXATT) THEN

*
*          Increment the number of attributes, both in total and of the
*          appropriate data type.

            NATT__CAT1 = NATT__CAT1 + 1

*
*          Add the details for the attribute.

            ATTID__CAT1(NATT__CAT1) = ID
            ATTNM__CAT1(NATT__CAT1) = NAME
            ATTMU__CAT1(NATT__CAT1) = MUTBLE
            ATTYP__CAT1(NATT__CAT1) = CAT__TYPEC

            ATTVC__CAT1(NATT__CAT1) = VALUE

         ELSE

*
*          Unable to create the attribute; set the status and report an
*          error.

            STATUS = CAT__MAXAT

            CALL CAT1_ERREP ('CAT1_ADDAC_MAT', 'Error adding an '/
     :        /'attribute to the attribute list.', STATUS)

         END IF

      END IF

      END
      SUBROUTINE CAT1_ADDAD (ID, NAME, MUTBLE, VALUE, STATUS)
*+
*  Name:
*     CAT1_ADDAD
*  Purpose:
*     Add an attribute of type D to the list of atributes.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ADDAD (ID, NAME, MUTBLE, VALUE; STATUS)
*  Description:
*     Add an attribute of type D to the list of atributes.
*  Arguments:
*     ID  =  INTEGER (Given)
*         Identifier to the component to which the attribute belongs.
*     NAME  =  CHARACTER*(*) (Given)
*         Name of the attribute.
*     MUTBLE  =  LOGICAL  (Given)
*         Flag indicating whether or not the attribute is mutable.
*     VALUE  =  DOUBLE PRECISION  (Given)
*         Value of the attribute.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there is space in the attribute arrays then
*       Add the details for the attribute
*     else
*       Set the return status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     24/6/93 (ACD): Original version.
*     4/7/93  (ACD): First working version.
*     23/1/94 (ACD): Modified error reporting.
*     6/3/95  (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     27/5/98 (ACD): Removed individual counts for attribute values of
*        differing data types.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT1_ATTRB_CMN'    ! Attributes common block.
*  Arguments Given:
      INTEGER
     :  ID
      CHARACTER*(*)
     :  NAME
      LOGICAL
     :  MUTBLE
      DOUBLE PRECISION
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*.

      IF (STATUS .EQ. CAT__OK) THEN

C        write(17, 2000) id, name, mutble
C2000    format(1x, 'id, name, mutble: ', I6, 1X, A, 1X, L5 )
C        write(17, 2001) natt__cat1, nattD__cat1
C2001    format(1x, 'natt__cat1, nattD__cat1: ', I6, I6 )

*
*       Check whether there is space for any more attributes, both in
*       total and of the appropriate data type.

         IF (NATT__CAT1    .LT.  CAT1__MXATT) THEN

*
*          Increment the number of attributes, both in total and of the
*          appropriate data type.

            NATT__CAT1 = NATT__CAT1 + 1

*
*          Add the details for the attribute.

            ATTID__CAT1(NATT__CAT1) = ID
            ATTNM__CAT1(NATT__CAT1) = NAME
            ATTMU__CAT1(NATT__CAT1) = MUTBLE
            ATTYP__CAT1(NATT__CAT1) = CAT__TYPED

            ATTVD__CAT1(NATT__CAT1) = VALUE

         ELSE

*
*          Unable to create the attribute; set the status and report an
*          error.

            STATUS = CAT__MAXAT

            CALL CAT1_ERREP ('CAT1_ADDAD_MAT', 'Error adding an '/
     :        /'attribute to the attribute list.', STATUS)

         END IF

      END IF

      END
      SUBROUTINE CAT1_ADDAI (ID, NAME, MUTBLE, VALUE, STATUS)
*+
*  Name:
*     CAT1_ADDAI
*  Purpose:
*     Add an attribute of type I to the list of atributes.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ADDAI (ID, NAME, MUTBLE, VALUE; STATUS)
*  Description:
*     Add an attribute of type I to the list of atributes.
*  Arguments:
*     ID  =  INTEGER (Given)
*         Identifier to the component to which the attribute belongs.
*     NAME  =  CHARACTER*(*) (Given)
*         Name of the attribute.
*     MUTBLE  =  LOGICAL  (Given)
*         Flag indicating whether or not the attribute is mutable.
*     VALUE  =  INTEGER  (Given)
*         Value of the attribute.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there is space in the attribute arrays then
*       Add the details for the attribute
*     else
*       Set the return status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     24/6/93 (ACD): Original version.
*     4/7/93  (ACD): First working version.
*     23/1/94 (ACD): Modified error reporting.
*     6/3/95  (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     27/5/98 (ACD): Removed individual counts for attribute values of
*        differing data types.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT1_ATTRB_CMN'    ! Attributes common block.
*  Arguments Given:
      INTEGER
     :  ID
      CHARACTER*(*)
     :  NAME
      LOGICAL
     :  MUTBLE
      INTEGER
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*.

      IF (STATUS .EQ. CAT__OK) THEN

C        write(17, 2000) id, name, mutble
C2000    format(1x, 'id, name, mutble: ', I6, 1X, A, 1X, L5 )
C        write(17, 2001) natt__cat1, nattI__cat1
C2001    format(1x, 'natt__cat1, nattI__cat1: ', I6, I6 )

*
*       Check whether there is space for any more attributes, both in
*       total and of the appropriate data type.

         IF (NATT__CAT1    .LT.  CAT1__MXATT) THEN

*
*          Increment the number of attributes, both in total and of the
*          appropriate data type.

            NATT__CAT1 = NATT__CAT1 + 1

*
*          Add the details for the attribute.

            ATTID__CAT1(NATT__CAT1) = ID
            ATTNM__CAT1(NATT__CAT1) = NAME
            ATTMU__CAT1(NATT__CAT1) = MUTBLE
            ATTYP__CAT1(NATT__CAT1) = CAT__TYPEI

            ATTVI__CAT1(NATT__CAT1) = VALUE

         ELSE

*
*          Unable to create the attribute; set the status and report an
*          error.

            STATUS = CAT__MAXAT

            CALL CAT1_ERREP ('CAT1_ADDAI_MAT', 'Error adding an '/
     :        /'attribute to the attribute list.', STATUS)

         END IF

      END IF

      END
      SUBROUTINE CAT1_ADDAL (ID, NAME, MUTBLE, VALUE, STATUS)
*+
*  Name:
*     CAT1_ADDAL
*  Purpose:
*     Add an attribute of type L to the list of atributes.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ADDAL (ID, NAME, MUTBLE, VALUE; STATUS)
*  Description:
*     Add an attribute of type L to the list of atributes.
*  Arguments:
*     ID  =  INTEGER (Given)
*         Identifier to the component to which the attribute belongs.
*     NAME  =  CHARACTER*(*) (Given)
*         Name of the attribute.
*     MUTBLE  =  LOGICAL  (Given)
*         Flag indicating whether or not the attribute is mutable.
*     VALUE  =  LOGICAL  (Given)
*         Value of the attribute.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there is space in the attribute arrays then
*       Add the details for the attribute
*     else
*       Set the return status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     24/6/93 (ACD): Original version.
*     4/7/93  (ACD): First working version.
*     23/1/94 (ACD): Modified error reporting.
*     6/3/95  (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     27/5/98 (ACD): Removed individual counts for attribute values of
*        differing data types.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT1_ATTRB_CMN'    ! Attributes common block.
*  Arguments Given:
      INTEGER
     :  ID
      CHARACTER*(*)
     :  NAME
      LOGICAL
     :  MUTBLE
      LOGICAL
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*.

      IF (STATUS .EQ. CAT__OK) THEN

C        write(17, 2000) id, name, mutble
C2000    format(1x, 'id, name, mutble: ', I6, 1X, A, 1X, L5 )
C        write(17, 2001) natt__cat1, nattL__cat1
C2001    format(1x, 'natt__cat1, nattL__cat1: ', I6, I6 )

*
*       Check whether there is space for any more attributes, both in
*       total and of the appropriate data type.

         IF (NATT__CAT1    .LT.  CAT1__MXATT) THEN

*
*          Increment the number of attributes, both in total and of the
*          appropriate data type.

            NATT__CAT1 = NATT__CAT1 + 1

*
*          Add the details for the attribute.

            ATTID__CAT1(NATT__CAT1) = ID
            ATTNM__CAT1(NATT__CAT1) = NAME
            ATTMU__CAT1(NATT__CAT1) = MUTBLE
            ATTYP__CAT1(NATT__CAT1) = CAT__TYPEL

            ATTVL__CAT1(NATT__CAT1) = VALUE

         ELSE

*
*          Unable to create the attribute; set the status and report an
*          error.

            STATUS = CAT__MAXAT

            CALL CAT1_ERREP ('CAT1_ADDAL_MAT', 'Error adding an '/
     :        /'attribute to the attribute list.', STATUS)

         END IF

      END IF

      END
      SUBROUTINE CAT1_ADDAR (ID, NAME, MUTBLE, VALUE, STATUS)
*+
*  Name:
*     CAT1_ADDAR
*  Purpose:
*     Add an attribute of type R to the list of atributes.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ADDAR (ID, NAME, MUTBLE, VALUE; STATUS)
*  Description:
*     Add an attribute of type R to the list of atributes.
*  Arguments:
*     ID  =  INTEGER (Given)
*         Identifier to the component to which the attribute belongs.
*     NAME  =  CHARACTER*(*) (Given)
*         Name of the attribute.
*     MUTBLE  =  LOGICAL  (Given)
*         Flag indicating whether or not the attribute is mutable.
*     VALUE  =  REAL  (Given)
*         Value of the attribute.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there is space in the attribute arrays then
*       Add the details for the attribute
*     else
*       Set the return status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     24/6/93 (ACD): Original version.
*     4/7/93  (ACD): First working version.
*     23/1/94 (ACD): Modified error reporting.
*     6/3/95  (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     27/5/98 (ACD): Removed individual counts for attribute values of
*        differing data types.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT1_ATTRB_CMN'    ! Attributes common block.
*  Arguments Given:
      INTEGER
     :  ID
      CHARACTER*(*)
     :  NAME
      LOGICAL
     :  MUTBLE
      REAL
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*.

      IF (STATUS .EQ. CAT__OK) THEN

C        write(17, 2000) id, name, mutble
C2000    format(1x, 'id, name, mutble: ', I6, 1X, A, 1X, L5 )
C        write(17, 2001) natt__cat1, nattR__cat1
C2001    format(1x, 'natt__cat1, nattR__cat1: ', I6, I6 )

*
*       Check whether there is space for any more attributes, both in
*       total and of the appropriate data type.

         IF (NATT__CAT1    .LT.  CAT1__MXATT) THEN

*
*          Increment the number of attributes, both in total and of the
*          appropriate data type.

            NATT__CAT1 = NATT__CAT1 + 1

*
*          Add the details for the attribute.

            ATTID__CAT1(NATT__CAT1) = ID
            ATTNM__CAT1(NATT__CAT1) = NAME
            ATTMU__CAT1(NATT__CAT1) = MUTBLE
            ATTYP__CAT1(NATT__CAT1) = CAT__TYPER

            ATTVR__CAT1(NATT__CAT1) = VALUE

         ELSE

*
*          Unable to create the attribute; set the status and report an
*          error.

            STATUS = CAT__MAXAT

            CALL CAT1_ERREP ('CAT1_ADDAR_MAT', 'Error adding an '/
     :        /'attribute to the attribute list.', STATUS)

         END IF

      END IF

      END
      SUBROUTINE CAT1_ADDAW (ID, NAME, MUTBLE, VALUE, STATUS)
*+
*  Name:
*     CAT1_ADDAW
*  Purpose:
*     Add an attribute of type W to the list of atributes.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ADDAW (ID, NAME, MUTBLE, VALUE; STATUS)
*  Description:
*     Add an attribute of type W to the list of atributes.
*  Arguments:
*     ID  =  INTEGER (Given)
*         Identifier to the component to which the attribute belongs.
*     NAME  =  CHARACTER*(*) (Given)
*         Name of the attribute.
*     MUTBLE  =  LOGICAL  (Given)
*         Flag indicating whether or not the attribute is mutable.
*     VALUE  =  INTEGER*2  (Given)
*         Value of the attribute.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there is space in the attribute arrays then
*       Add the details for the attribute
*     else
*       Set the return status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     24/6/93 (ACD): Original version.
*     4/7/93  (ACD): First working version.
*     23/1/94 (ACD): Modified error reporting.
*     6/3/95  (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     27/5/98 (ACD): Removed individual counts for attribute values of
*        differing data types.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT1_ATTRB_CMN'    ! Attributes common block.
*  Arguments Given:
      INTEGER
     :  ID
      CHARACTER*(*)
     :  NAME
      LOGICAL
     :  MUTBLE
      INTEGER*2
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*.

      IF (STATUS .EQ. CAT__OK) THEN

C        write(17, 2000) id, name, mutble
C2000    format(1x, 'id, name, mutble: ', I6, 1X, A, 1X, L5 )
C        write(17, 2001) natt__cat1, nattW__cat1
C2001    format(1x, 'natt__cat1, nattW__cat1: ', I6, I6 )

*
*       Check whether there is space for any more attributes, both in
*       total and of the appropriate data type.

         IF (NATT__CAT1    .LT.  CAT1__MXATT) THEN

*
*          Increment the number of attributes, both in total and of the
*          appropriate data type.

            NATT__CAT1 = NATT__CAT1 + 1

*
*          Add the details for the attribute.

            ATTID__CAT1(NATT__CAT1) = ID
            ATTNM__CAT1(NATT__CAT1) = NAME
            ATTMU__CAT1(NATT__CAT1) = MUTBLE
            ATTYP__CAT1(NATT__CAT1) = CAT__TYPEW

            ATTVW__CAT1(NATT__CAT1) = VALUE

         ELSE

*
*          Unable to create the attribute; set the status and report an
*          error.

            STATUS = CAT__MAXAT

            CALL CAT1_ERREP ('CAT1_ADDAW_MAT', 'Error adding an '/
     :        /'attribute to the attribute list.', STATUS)

         END IF

      END IF

      END
