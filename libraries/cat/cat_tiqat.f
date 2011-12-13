      SUBROUTINE CAT_TIQAB (GI, ATTRIB, VALUE, STATUS)
*+
*  Name:
*     CAT_TIQAB
*  Purpose:
*     Inquire the value of a single attribute for a component.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TIQAB (GI, ATTRIB; VALUEB; STATUS)
*  Description:
*     Inquire the value of a single attribute for a component.  If the
*     value of an array is inquired, the value of the first element is
*     returned.
*  Arguments:
*     GI  =  INTEGER (Given)
*        Generic component identifier.
*     ATTRIB  =  CHARACTER*(*) (Given)
*        Name of the attribute of the component.
*     VALUE  =  BYTE (Returned)
*        Value of the named attribute.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the identifier is valid then
*       Attempt to locate the attribute.
*       If it is found then
*         Determine its type.
*         Obtain its value.
*         Perform any necessary type conversion.
*         If an error occurred then
*           Set the status.
*         end if
*       else
*         Set the status.
*       end if
*     else
*       Set the status.
*     end if
*     Report any error.
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
*     3/5/93   (ACD): Prologue only.
*     24/6/93  (ACD): First implementation.
*     11/10/93 (ACD): First stable version.
*     24/1/94  (ACD): Modified error reporting.
*     8/2/94   (ACD): Changed parameters for data type codes.
*     14/8/96  (ACD): Remove non-printable characters from returned
*        CHARACTER attributes.
*     27/5/98  (ACD): Speeded up search for the attribute.
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
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
      INCLUDE 'CAT1_ATTRB_CMN'    ! Attributes common block.
*  Arguments Given:
      INTEGER
     :  GI
      CHARACTER*(*)
     :  ATTRIB
*  Arguments Returned:
      BYTE
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  LOOP,   ! Loop index.
     :  ATELM,  ! Element in attribute arrays.
     :  ATYPE   ! Data type for attribute.
      LOGICAL
     :  FOUND,  ! Flag: has the attribute been found?
     :  MORE,   ! Flag; continue hunting for attribute?
     :  CONVOK  ! Flag: error in the type conversion?

*
*    The following variables hold the value obtained for the named
*    attribute in the data type of the attribute.  In a given invocation
*    only the variable corresponding to the data type of the attribute
*    is used.

      BYTE             VALUB ! Unsigned byte value.
      BYTE             VALB  ! Byte value.
      INTEGER*2        VALUW ! Unsigned word value.
      INTEGER*2        VALW  ! Word value.
      INTEGER          VALI  ! Integer value.
      REAL             VALR  ! Real value.
      DOUBLE PRECISION VALD  ! Double precision value.
      LOGICAL          VALL  ! Logical value.
      CHARACTER        VALC*(CAT1__SZATS)   ! Character value.

*
*    The following variables hold a local of the returned value,
*    converted to data type B.  In the instantiation of the routine
*    of type B only the variable LVALB is used.  The purpose of
*    having the variables at all is to convert the local fixed length
*    string to the returned fixed length string.

      BYTE      LVALUB
      BYTE      LVALB
      INTEGER*2 LVALUW
      INTEGER*2 LVALW
      INTEGER   LVALI
      REAL      LVALR
      DOUBLE PRECISION LVALD
      LOGICAL   LVALL
      CHARACTER LVALC*(CAT1__SZATS)
*.

      IF (STATUS .EQ. CAT__OK) THEN

C        write(17, 1000) gi, attrib, status
C1000    format(1x, 'TIQAB: gi, attrib, status: ',
C    :     i5, 1x, a, 1x, i10 )

*
*       Check that the identifier is valid.

         IF (GI .GT. 0  .AND.  GI .LE. NIDS__CAT1) THEN

*
*          Attempt to locate the attribute.

            FOUND = .FALSE.
            MORE = .TRUE.
            LOOP = IDATT__CAT1(GI) - 1

            DO WHILE (MORE)
               LOOP = LOOP + 1

               IF (GI .EQ. ATTID__CAT1(LOOP)  .AND.
     :             ATTRIB .EQ. ATTNM__CAT1(LOOP) ) THEN
                  ATELM = LOOP
                  FOUND = .TRUE.
                  MORE = .FALSE.
               END IF

               IF (LOOP .GE. NATT__CAT1) THEN
                  MORE = .FALSE.
               END IF
            END DO

*
*          Proceed if the attribute has been found.

            IF (FOUND) THEN

*
*             Determine the type of the attribute and obtain its value.

               ATYPE = ATTYP__CAT1(ATELM)

               IF (ATYPE .EQ. CAT__TYPEUB) THEN
                  VALUB = ATTVV__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEB) THEN
                  VALB = ATTVB__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEUW) THEN
                  VALUW = ATTVU__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEW) THEN
                  VALW = ATTVW__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEI) THEN
                  VALI = ATTVI__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPER) THEN
                  VALR = ATTVR__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPED) THEN
                  VALD = ATTVD__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEL) THEN
                  VALL = ATTVL__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEC) THEN
                  VALC = ATTVC__CAT1(ATELM)
                  CALL CHR_CLEAN(VALC)

               END IF

*
*             Perform the type conversion.

               CALL CAT1_TCNVT (ATYPE, VALUB, VALB, VALUW, VALW, VALI,
     :           VALR, VALD, VALL, VALC,
     :           CAT__TYPEB, LVALUB, LVALB, LVALUW, LVALW, LVALI, LVALR,
     :           LVALD, LVALL, LVALC, CONVOK, STATUS)

               VALUE = LVALB

               IF (STATUS .NE. CAT__OK) THEN
                  STATUS = CAT__TYPCV
               END IF

            ELSE
               STATUS = CAT__NOATT
            END IF

         ELSE
            STATUS = CAT__INVID
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_TIQAB_ERR', 'CAT_TIQAB: error '/
     :        /'inquiring the value of an attribute.', STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_TIQAC (GI, ATTRIB, VALUE, STATUS)
*+
*  Name:
*     CAT_TIQAC
*  Purpose:
*     Inquire the value of a single attribute for a component.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TIQAC (GI, ATTRIB; VALUEC; STATUS)
*  Description:
*     Inquire the value of a single attribute for a component.  If the
*     value of an array is inquired, the value of the first element is
*     returned.
*  Arguments:
*     GI  =  INTEGER (Given)
*        Generic component identifier.
*     ATTRIB  =  CHARACTER*(*) (Given)
*        Name of the attribute of the component.
*     VALUE  =  CHARACTER*(*) (Returned)
*        Value of the named attribute.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the identifier is valid then
*       Attempt to locate the attribute.
*       If it is found then
*         Determine its type.
*         Obtain its value.
*         Perform any necessary type conversion.
*         If an error occurred then
*           Set the status.
*         end if
*       else
*         Set the status.
*       end if
*     else
*       Set the status.
*     end if
*     Report any error.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93   (ACD): Prologue only.
*     24/6/93  (ACD): First implementation.
*     11/10/93 (ACD): First stable version.
*     24/1/94  (ACD): Modified error reporting.
*     8/2/94   (ACD): Changed parameters for data type codes.
*     14/8/96  (ACD): Remove non-printable characters from returned
*        CHARACTER attributes.
*     27/5/98  (ACD): Speeded up search for the attribute.
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
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
      INCLUDE 'CAT1_ATTRB_CMN'    ! Attributes common block.
*  Arguments Given:
      INTEGER
     :  GI
      CHARACTER*(*)
     :  ATTRIB
*  Arguments Returned:
      CHARACTER*(*)
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  LOOP,   ! Loop index.
     :  ATELM,  ! Element in attribute arrays.
     :  ATYPE   ! Data type for attribute.
      LOGICAL
     :  FOUND,  ! Flag: has the attribute been found?
     :  MORE,   ! Flag; continue hunting for attribute?
     :  CONVOK  ! Flag: error in the type conversion?

*
*    The following variables hold the value obtained for the named
*    attribute in the data type of the attribute.  In a given invocation
*    only the variable corresponding to the data type of the attribute
*    is used.

      BYTE             VALUB ! Unsigned byte value.
      BYTE             VALB  ! Byte value.
      INTEGER*2        VALUW ! Unsigned word value.
      INTEGER*2        VALW  ! Word value.
      INTEGER          VALI  ! Integer value.
      REAL             VALR  ! Real value.
      DOUBLE PRECISION VALD  ! Double precision value.
      LOGICAL          VALL  ! Logical value.
      CHARACTER        VALC*(CAT1__SZATS)   ! Character value.

*
*    The following variables hold a local of the returned value,
*    converted to data type C.  In the instantiation of the routine
*    of type C only the variable LVALC is used.  The purpose of
*    having the variables at all is to convert the local fixed length
*    string to the returned fixed length string.

      BYTE      LVALUB
      BYTE      LVALB
      INTEGER*2 LVALUW
      INTEGER*2 LVALW
      INTEGER   LVALI
      REAL      LVALR
      DOUBLE PRECISION LVALD
      LOGICAL   LVALL
      CHARACTER LVALC*(CAT1__SZATS)
*.

      IF (STATUS .EQ. CAT__OK) THEN

C        write(17, 1000) gi, attrib, status
C1000    format(1x, 'TIQAC: gi, attrib, status: ',
C    :     i5, 1x, a, 1x, i10 )

*
*       Check that the identifier is valid.

         IF (GI .GT. 0  .AND.  GI .LE. NIDS__CAT1) THEN

*
*          Attempt to locate the attribute.

            FOUND = .FALSE.
            MORE = .TRUE.
            LOOP = IDATT__CAT1(GI) - 1

            DO WHILE (MORE)
               LOOP = LOOP + 1

               IF (GI .EQ. ATTID__CAT1(LOOP)  .AND.
     :             ATTRIB .EQ. ATTNM__CAT1(LOOP) ) THEN
                  ATELM = LOOP
                  FOUND = .TRUE.
                  MORE = .FALSE.
               END IF

               IF (LOOP .GE. NATT__CAT1) THEN
                  MORE = .FALSE.
               END IF
            END DO

*
*          Proceed if the attribute has been found.

            IF (FOUND) THEN

*
*             Determine the type of the attribute and obtain its value.

               ATYPE = ATTYP__CAT1(ATELM)

               IF (ATYPE .EQ. CAT__TYPEUB) THEN
                  VALUB = ATTVV__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEB) THEN
                  VALB = ATTVB__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEUW) THEN
                  VALUW = ATTVU__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEW) THEN
                  VALW = ATTVW__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEI) THEN
                  VALI = ATTVI__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPER) THEN
                  VALR = ATTVR__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPED) THEN
                  VALD = ATTVD__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEL) THEN
                  VALL = ATTVL__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEC) THEN
                  VALC = ATTVC__CAT1(ATELM)
                  CALL CHR_CLEAN(VALC)

               END IF

*
*             Perform the type conversion.

               CALL CAT1_TCNVT (ATYPE, VALUB, VALB, VALUW, VALW, VALI,
     :           VALR, VALD, VALL, VALC,
     :           CAT__TYPEC, LVALUB, LVALB, LVALUW, LVALW, LVALI, LVALR,
     :           LVALD, LVALL, LVALC, CONVOK, STATUS)

               VALUE = LVALC

               IF (STATUS .NE. CAT__OK) THEN
                  STATUS = CAT__TYPCV
               END IF

            ELSE
               STATUS = CAT__NOATT
            END IF

         ELSE
            STATUS = CAT__INVID
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_TIQAC_ERR', 'CAT_TIQAC: error '/
     :        /'inquiring the value of an attribute.', STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_TIQAD (GI, ATTRIB, VALUE, STATUS)
*+
*  Name:
*     CAT_TIQAD
*  Purpose:
*     Inquire the value of a single attribute for a component.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TIQAD (GI, ATTRIB; VALUED; STATUS)
*  Description:
*     Inquire the value of a single attribute for a component.  If the
*     value of an array is inquired, the value of the first element is
*     returned.
*  Arguments:
*     GI  =  INTEGER (Given)
*        Generic component identifier.
*     ATTRIB  =  CHARACTER*(*) (Given)
*        Name of the attribute of the component.
*     VALUE  =  DOUBLE PRECISION (Returned)
*        Value of the named attribute.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the identifier is valid then
*       Attempt to locate the attribute.
*       If it is found then
*         Determine its type.
*         Obtain its value.
*         Perform any necessary type conversion.
*         If an error occurred then
*           Set the status.
*         end if
*       else
*         Set the status.
*       end if
*     else
*       Set the status.
*     end if
*     Report any error.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93   (ACD): Prologue only.
*     24/6/93  (ACD): First implementation.
*     11/10/93 (ACD): First stable version.
*     24/1/94  (ACD): Modified error reporting.
*     8/2/94   (ACD): Changed parameters for data type codes.
*     14/8/96  (ACD): Remove non-printable characters from returned
*        CHARACTER attributes.
*     27/5/98  (ACD): Speeded up search for the attribute.
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
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
      INCLUDE 'CAT1_ATTRB_CMN'    ! Attributes common block.
*  Arguments Given:
      INTEGER
     :  GI
      CHARACTER*(*)
     :  ATTRIB
*  Arguments Returned:
      DOUBLE PRECISION
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  LOOP,   ! Loop index.
     :  ATELM,  ! Element in attribute arrays.
     :  ATYPE   ! Data type for attribute.
      LOGICAL
     :  FOUND,  ! Flag: has the attribute been found?
     :  MORE,   ! Flag; continue hunting for attribute?
     :  CONVOK  ! Flag: error in the type conversion?

*
*    The following variables hold the value obtained for the named
*    attribute in the data type of the attribute.  In a given invocation
*    only the variable corresponding to the data type of the attribute
*    is used.

      BYTE             VALUB ! Unsigned byte value.
      BYTE             VALB  ! Byte value.
      INTEGER*2        VALUW ! Unsigned word value.
      INTEGER*2        VALW  ! Word value.
      INTEGER          VALI  ! Integer value.
      REAL             VALR  ! Real value.
      DOUBLE PRECISION VALD  ! Double precision value.
      LOGICAL          VALL  ! Logical value.
      CHARACTER        VALC*(CAT1__SZATS)   ! Character value.

*
*    The following variables hold a local of the returned value,
*    converted to data type D.  In the instantiation of the routine
*    of type D only the variable LVALD is used.  The purpose of
*    having the variables at all is to convert the local fixed length
*    string to the returned fixed length string.

      BYTE      LVALUB
      BYTE      LVALB
      INTEGER*2 LVALUW
      INTEGER*2 LVALW
      INTEGER   LVALI
      REAL      LVALR
      DOUBLE PRECISION LVALD
      LOGICAL   LVALL
      CHARACTER LVALC*(CAT1__SZATS)
*.

      IF (STATUS .EQ. CAT__OK) THEN

C        write(17, 1000) gi, attrib, status
C1000    format(1x, 'TIQAD: gi, attrib, status: ',
C    :     i5, 1x, a, 1x, i10 )

*
*       Check that the identifier is valid.

         IF (GI .GT. 0  .AND.  GI .LE. NIDS__CAT1) THEN

*
*          Attempt to locate the attribute.

            FOUND = .FALSE.
            MORE = .TRUE.
            LOOP = IDATT__CAT1(GI) - 1

            DO WHILE (MORE)
               LOOP = LOOP + 1

               IF (GI .EQ. ATTID__CAT1(LOOP)  .AND.
     :             ATTRIB .EQ. ATTNM__CAT1(LOOP) ) THEN
                  ATELM = LOOP
                  FOUND = .TRUE.
                  MORE = .FALSE.
               END IF

               IF (LOOP .GE. NATT__CAT1) THEN
                  MORE = .FALSE.
               END IF
            END DO

*
*          Proceed if the attribute has been found.

            IF (FOUND) THEN

*
*             Determine the type of the attribute and obtain its value.

               ATYPE = ATTYP__CAT1(ATELM)

               IF (ATYPE .EQ. CAT__TYPEUB) THEN
                  VALUB = ATTVV__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEB) THEN
                  VALB = ATTVB__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEUW) THEN
                  VALUW = ATTVU__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEW) THEN
                  VALW = ATTVW__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEI) THEN
                  VALI = ATTVI__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPER) THEN
                  VALR = ATTVR__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPED) THEN
                  VALD = ATTVD__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEL) THEN
                  VALL = ATTVL__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEC) THEN
                  VALC = ATTVC__CAT1(ATELM)
                  CALL CHR_CLEAN(VALC)

               END IF

*
*             Perform the type conversion.

               CALL CAT1_TCNVT (ATYPE, VALUB, VALB, VALUW, VALW, VALI,
     :           VALR, VALD, VALL, VALC,
     :           CAT__TYPED, LVALUB, LVALB, LVALUW, LVALW, LVALI, LVALR,
     :           LVALD, LVALL, LVALC, CONVOK, STATUS)

               VALUE = LVALD

               IF (STATUS .NE. CAT__OK) THEN
                  STATUS = CAT__TYPCV
               END IF

            ELSE
               STATUS = CAT__NOATT
            END IF

         ELSE
            STATUS = CAT__INVID
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_TIQAD_ERR', 'CAT_TIQAD: error '/
     :        /'inquiring the value of an attribute.', STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_TIQAI (GI, ATTRIB, VALUE, STATUS)
*+
*  Name:
*     CAT_TIQAI
*  Purpose:
*     Inquire the value of a single attribute for a component.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TIQAI (GI, ATTRIB; VALUEI; STATUS)
*  Description:
*     Inquire the value of a single attribute for a component.  If the
*     value of an array is inquired, the value of the first element is
*     returned.
*  Arguments:
*     GI  =  INTEGER (Given)
*        Generic component identifier.
*     ATTRIB  =  CHARACTER*(*) (Given)
*        Name of the attribute of the component.
*     VALUE  =  INTEGER (Returned)
*        Value of the named attribute.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the identifier is valid then
*       Attempt to locate the attribute.
*       If it is found then
*         Determine its type.
*         Obtain its value.
*         Perform any necessary type conversion.
*         If an error occurred then
*           Set the status.
*         end if
*       else
*         Set the status.
*       end if
*     else
*       Set the status.
*     end if
*     Report any error.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93   (ACD): Prologue only.
*     24/6/93  (ACD): First implementation.
*     11/10/93 (ACD): First stable version.
*     24/1/94  (ACD): Modified error reporting.
*     8/2/94   (ACD): Changed parameters for data type codes.
*     14/8/96  (ACD): Remove non-printable characters from returned
*        CHARACTER attributes.
*     27/5/98  (ACD): Speeded up search for the attribute.
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
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
      INCLUDE 'CAT1_ATTRB_CMN'    ! Attributes common block.
*  Arguments Given:
      INTEGER
     :  GI
      CHARACTER*(*)
     :  ATTRIB
*  Arguments Returned:
      INTEGER
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  LOOP,   ! Loop index.
     :  ATELM,  ! Element in attribute arrays.
     :  ATYPE   ! Data type for attribute.
      LOGICAL
     :  FOUND,  ! Flag: has the attribute been found?
     :  MORE,   ! Flag; continue hunting for attribute?
     :  CONVOK  ! Flag: error in the type conversion?

*
*    The following variables hold the value obtained for the named
*    attribute in the data type of the attribute.  In a given invocation
*    only the variable corresponding to the data type of the attribute
*    is used.

      BYTE             VALUB ! Unsigned byte value.
      BYTE             VALB  ! Byte value.
      INTEGER*2        VALUW ! Unsigned word value.
      INTEGER*2        VALW  ! Word value.
      INTEGER          VALI  ! Integer value.
      REAL             VALR  ! Real value.
      DOUBLE PRECISION VALD  ! Double precision value.
      LOGICAL          VALL  ! Logical value.
      CHARACTER        VALC*(CAT1__SZATS)   ! Character value.

*
*    The following variables hold a local of the returned value,
*    converted to data type I.  In the instantiation of the routine
*    of type I only the variable LVALI is used.  The purpose of
*    having the variables at all is to convert the local fixed length
*    string to the returned fixed length string.

      BYTE      LVALUB
      BYTE      LVALB
      INTEGER*2 LVALUW
      INTEGER*2 LVALW
      INTEGER   LVALI
      REAL      LVALR
      DOUBLE PRECISION LVALD
      LOGICAL   LVALL
      CHARACTER LVALC*(CAT1__SZATS)
*.

      IF (STATUS .EQ. CAT__OK) THEN

C        write(17, 1000) gi, attrib, status
C1000    format(1x, 'TIQAI: gi, attrib, status: ',
C    :     i5, 1x, a, 1x, i10 )

*
*       Check that the identifier is valid.

         IF (GI .GT. 0  .AND.  GI .LE. NIDS__CAT1) THEN

*
*          Attempt to locate the attribute.

            FOUND = .FALSE.
            MORE = .TRUE.
            LOOP = IDATT__CAT1(GI) - 1

            DO WHILE (MORE)
               LOOP = LOOP + 1

               IF (GI .EQ. ATTID__CAT1(LOOP)  .AND.
     :             ATTRIB .EQ. ATTNM__CAT1(LOOP) ) THEN
                  ATELM = LOOP
                  FOUND = .TRUE.
                  MORE = .FALSE.
               END IF

               IF (LOOP .GE. NATT__CAT1) THEN
                  MORE = .FALSE.
               END IF
            END DO

*
*          Proceed if the attribute has been found.

            IF (FOUND) THEN

*
*             Determine the type of the attribute and obtain its value.

               ATYPE = ATTYP__CAT1(ATELM)

               IF (ATYPE .EQ. CAT__TYPEUB) THEN
                  VALUB = ATTVV__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEB) THEN
                  VALB = ATTVB__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEUW) THEN
                  VALUW = ATTVU__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEW) THEN
                  VALW = ATTVW__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEI) THEN
                  VALI = ATTVI__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPER) THEN
                  VALR = ATTVR__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPED) THEN
                  VALD = ATTVD__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEL) THEN
                  VALL = ATTVL__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEC) THEN
                  VALC = ATTVC__CAT1(ATELM)
                  CALL CHR_CLEAN(VALC)

               END IF

*
*             Perform the type conversion.

               CALL CAT1_TCNVT (ATYPE, VALUB, VALB, VALUW, VALW, VALI,
     :           VALR, VALD, VALL, VALC,
     :           CAT__TYPEI, LVALUB, LVALB, LVALUW, LVALW, LVALI, LVALR,
     :           LVALD, LVALL, LVALC, CONVOK, STATUS)

               VALUE = LVALI

               IF (STATUS .NE. CAT__OK) THEN
                  STATUS = CAT__TYPCV
               END IF

            ELSE
               STATUS = CAT__NOATT
            END IF

         ELSE
            STATUS = CAT__INVID
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_TIQAI_ERR', 'CAT_TIQAI: error '/
     :        /'inquiring the value of an attribute.', STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_TIQAL (GI, ATTRIB, VALUE, STATUS)
*+
*  Name:
*     CAT_TIQAL
*  Purpose:
*     Inquire the value of a single attribute for a component.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TIQAL (GI, ATTRIB; VALUEL; STATUS)
*  Description:
*     Inquire the value of a single attribute for a component.  If the
*     value of an array is inquired, the value of the first element is
*     returned.
*  Arguments:
*     GI  =  INTEGER (Given)
*        Generic component identifier.
*     ATTRIB  =  CHARACTER*(*) (Given)
*        Name of the attribute of the component.
*     VALUE  =  LOGICAL (Returned)
*        Value of the named attribute.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the identifier is valid then
*       Attempt to locate the attribute.
*       If it is found then
*         Determine its type.
*         Obtain its value.
*         Perform any necessary type conversion.
*         If an error occurred then
*           Set the status.
*         end if
*       else
*         Set the status.
*       end if
*     else
*       Set the status.
*     end if
*     Report any error.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93   (ACD): Prologue only.
*     24/6/93  (ACD): First implementation.
*     11/10/93 (ACD): First stable version.
*     24/1/94  (ACD): Modified error reporting.
*     8/2/94   (ACD): Changed parameters for data type codes.
*     14/8/96  (ACD): Remove non-printable characters from returned
*        CHARACTER attributes.
*     27/5/98  (ACD): Speeded up search for the attribute.
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
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
      INCLUDE 'CAT1_ATTRB_CMN'    ! Attributes common block.
*  Arguments Given:
      INTEGER
     :  GI
      CHARACTER*(*)
     :  ATTRIB
*  Arguments Returned:
      LOGICAL
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  LOOP,   ! Loop index.
     :  ATELM,  ! Element in attribute arrays.
     :  ATYPE   ! Data type for attribute.
      LOGICAL
     :  FOUND,  ! Flag: has the attribute been found?
     :  MORE,   ! Flag; continue hunting for attribute?
     :  CONVOK  ! Flag: error in the type conversion?

*
*    The following variables hold the value obtained for the named
*    attribute in the data type of the attribute.  In a given invocation
*    only the variable corresponding to the data type of the attribute
*    is used.

      BYTE             VALUB ! Unsigned byte value.
      BYTE             VALB  ! Byte value.
      INTEGER*2        VALUW ! Unsigned word value.
      INTEGER*2        VALW  ! Word value.
      INTEGER          VALI  ! Integer value.
      REAL             VALR  ! Real value.
      DOUBLE PRECISION VALD  ! Double precision value.
      LOGICAL          VALL  ! Logical value.
      CHARACTER        VALC*(CAT1__SZATS)   ! Character value.

*
*    The following variables hold a local of the returned value,
*    converted to data type L.  In the instantiation of the routine
*    of type L only the variable LVALL is used.  The purpose of
*    having the variables at all is to convert the local fixed length
*    string to the returned fixed length string.

      BYTE      LVALUB
      BYTE      LVALB
      INTEGER*2 LVALUW
      INTEGER*2 LVALW
      INTEGER   LVALI
      REAL      LVALR
      DOUBLE PRECISION LVALD
      LOGICAL   LVALL
      CHARACTER LVALC*(CAT1__SZATS)
*.

      IF (STATUS .EQ. CAT__OK) THEN

C        write(17, 1000) gi, attrib, status
C1000    format(1x, 'TIQAL: gi, attrib, status: ',
C    :     i5, 1x, a, 1x, i10 )

*
*       Check that the identifier is valid.

         IF (GI .GT. 0  .AND.  GI .LE. NIDS__CAT1) THEN

*
*          Attempt to locate the attribute.

            FOUND = .FALSE.
            MORE = .TRUE.
            LOOP = IDATT__CAT1(GI) - 1

            DO WHILE (MORE)
               LOOP = LOOP + 1

               IF (GI .EQ. ATTID__CAT1(LOOP)  .AND.
     :             ATTRIB .EQ. ATTNM__CAT1(LOOP) ) THEN
                  ATELM = LOOP
                  FOUND = .TRUE.
                  MORE = .FALSE.
               END IF

               IF (LOOP .GE. NATT__CAT1) THEN
                  MORE = .FALSE.
               END IF
            END DO

*
*          Proceed if the attribute has been found.

            IF (FOUND) THEN

*
*             Determine the type of the attribute and obtain its value.

               ATYPE = ATTYP__CAT1(ATELM)

               IF (ATYPE .EQ. CAT__TYPEUB) THEN
                  VALUB = ATTVV__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEB) THEN
                  VALB = ATTVB__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEUW) THEN
                  VALUW = ATTVU__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEW) THEN
                  VALW = ATTVW__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEI) THEN
                  VALI = ATTVI__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPER) THEN
                  VALR = ATTVR__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPED) THEN
                  VALD = ATTVD__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEL) THEN
                  VALL = ATTVL__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEC) THEN
                  VALC = ATTVC__CAT1(ATELM)
                  CALL CHR_CLEAN(VALC)

               END IF

*
*             Perform the type conversion.

               CALL CAT1_TCNVT (ATYPE, VALUB, VALB, VALUW, VALW, VALI,
     :           VALR, VALD, VALL, VALC,
     :           CAT__TYPEL, LVALUB, LVALB, LVALUW, LVALW, LVALI, LVALR,
     :           LVALD, LVALL, LVALC, CONVOK, STATUS)

               VALUE = LVALL

               IF (STATUS .NE. CAT__OK) THEN
                  STATUS = CAT__TYPCV
               END IF

            ELSE
               STATUS = CAT__NOATT
            END IF

         ELSE
            STATUS = CAT__INVID
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_TIQAL_ERR', 'CAT_TIQAL: error '/
     :        /'inquiring the value of an attribute.', STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_TIQAR (GI, ATTRIB, VALUE, STATUS)
*+
*  Name:
*     CAT_TIQAR
*  Purpose:
*     Inquire the value of a single attribute for a component.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TIQAR (GI, ATTRIB; VALUER; STATUS)
*  Description:
*     Inquire the value of a single attribute for a component.  If the
*     value of an array is inquired, the value of the first element is
*     returned.
*  Arguments:
*     GI  =  INTEGER (Given)
*        Generic component identifier.
*     ATTRIB  =  CHARACTER*(*) (Given)
*        Name of the attribute of the component.
*     VALUE  =  REAL (Returned)
*        Value of the named attribute.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the identifier is valid then
*       Attempt to locate the attribute.
*       If it is found then
*         Determine its type.
*         Obtain its value.
*         Perform any necessary type conversion.
*         If an error occurred then
*           Set the status.
*         end if
*       else
*         Set the status.
*       end if
*     else
*       Set the status.
*     end if
*     Report any error.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93   (ACD): Prologue only.
*     24/6/93  (ACD): First implementation.
*     11/10/93 (ACD): First stable version.
*     24/1/94  (ACD): Modified error reporting.
*     8/2/94   (ACD): Changed parameters for data type codes.
*     14/8/96  (ACD): Remove non-printable characters from returned
*        CHARACTER attributes.
*     27/5/98  (ACD): Speeded up search for the attribute.
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
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
      INCLUDE 'CAT1_ATTRB_CMN'    ! Attributes common block.
*  Arguments Given:
      INTEGER
     :  GI
      CHARACTER*(*)
     :  ATTRIB
*  Arguments Returned:
      REAL
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  LOOP,   ! Loop index.
     :  ATELM,  ! Element in attribute arrays.
     :  ATYPE   ! Data type for attribute.
      LOGICAL
     :  FOUND,  ! Flag: has the attribute been found?
     :  MORE,   ! Flag; continue hunting for attribute?
     :  CONVOK  ! Flag: error in the type conversion?

*
*    The following variables hold the value obtained for the named
*    attribute in the data type of the attribute.  In a given invocation
*    only the variable corresponding to the data type of the attribute
*    is used.

      BYTE             VALUB ! Unsigned byte value.
      BYTE             VALB  ! Byte value.
      INTEGER*2        VALUW ! Unsigned word value.
      INTEGER*2        VALW  ! Word value.
      INTEGER          VALI  ! Integer value.
      REAL             VALR  ! Real value.
      DOUBLE PRECISION VALD  ! Double precision value.
      LOGICAL          VALL  ! Logical value.
      CHARACTER        VALC*(CAT1__SZATS)   ! Character value.

*
*    The following variables hold a local of the returned value,
*    converted to data type R.  In the instantiation of the routine
*    of type R only the variable LVALR is used.  The purpose of
*    having the variables at all is to convert the local fixed length
*    string to the returned fixed length string.

      BYTE      LVALUB
      BYTE      LVALB
      INTEGER*2 LVALUW
      INTEGER*2 LVALW
      INTEGER   LVALI
      REAL      LVALR
      DOUBLE PRECISION LVALD
      LOGICAL   LVALL
      CHARACTER LVALC*(CAT1__SZATS)
*.

      IF (STATUS .EQ. CAT__OK) THEN

C        write(17, 1000) gi, attrib, status
C1000    format(1x, 'TIQAR: gi, attrib, status: ',
C    :     i5, 1x, a, 1x, i10 )

*
*       Check that the identifier is valid.

         IF (GI .GT. 0  .AND.  GI .LE. NIDS__CAT1) THEN

*
*          Attempt to locate the attribute.

            FOUND = .FALSE.
            MORE = .TRUE.
            LOOP = IDATT__CAT1(GI) - 1

            DO WHILE (MORE)
               LOOP = LOOP + 1

               IF (GI .EQ. ATTID__CAT1(LOOP)  .AND.
     :             ATTRIB .EQ. ATTNM__CAT1(LOOP) ) THEN
                  ATELM = LOOP
                  FOUND = .TRUE.
                  MORE = .FALSE.
               END IF

               IF (LOOP .GE. NATT__CAT1) THEN
                  MORE = .FALSE.
               END IF
            END DO

*
*          Proceed if the attribute has been found.

            IF (FOUND) THEN

*
*             Determine the type of the attribute and obtain its value.

               ATYPE = ATTYP__CAT1(ATELM)

               IF (ATYPE .EQ. CAT__TYPEUB) THEN
                  VALUB = ATTVV__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEB) THEN
                  VALB = ATTVB__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEUW) THEN
                  VALUW = ATTVU__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEW) THEN
                  VALW = ATTVW__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEI) THEN
                  VALI = ATTVI__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPER) THEN
                  VALR = ATTVR__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPED) THEN
                  VALD = ATTVD__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEL) THEN
                  VALL = ATTVL__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEC) THEN
                  VALC = ATTVC__CAT1(ATELM)
                  CALL CHR_CLEAN(VALC)

               END IF

*
*             Perform the type conversion.

               CALL CAT1_TCNVT (ATYPE, VALUB, VALB, VALUW, VALW, VALI,
     :           VALR, VALD, VALL, VALC,
     :           CAT__TYPER, LVALUB, LVALB, LVALUW, LVALW, LVALI, LVALR,
     :           LVALD, LVALL, LVALC, CONVOK, STATUS)

               VALUE = LVALR

               IF (STATUS .NE. CAT__OK) THEN
                  STATUS = CAT__TYPCV
               END IF

            ELSE
               STATUS = CAT__NOATT
            END IF

         ELSE
            STATUS = CAT__INVID
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_TIQAR_ERR', 'CAT_TIQAR: error '/
     :        /'inquiring the value of an attribute.', STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT_TIQAW (GI, ATTRIB, VALUE, STATUS)
*+
*  Name:
*     CAT_TIQAW
*  Purpose:
*     Inquire the value of a single attribute for a component.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TIQAW (GI, ATTRIB; VALUEW; STATUS)
*  Description:
*     Inquire the value of a single attribute for a component.  If the
*     value of an array is inquired, the value of the first element is
*     returned.
*  Arguments:
*     GI  =  INTEGER (Given)
*        Generic component identifier.
*     ATTRIB  =  CHARACTER*(*) (Given)
*        Name of the attribute of the component.
*     VALUE  =  INTEGER*2 (Returned)
*        Value of the named attribute.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the identifier is valid then
*       Attempt to locate the attribute.
*       If it is found then
*         Determine its type.
*         Obtain its value.
*         Perform any necessary type conversion.
*         If an error occurred then
*           Set the status.
*         end if
*       else
*         Set the status.
*       end if
*     else
*       Set the status.
*     end if
*     Report any error.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93   (ACD): Prologue only.
*     24/6/93  (ACD): First implementation.
*     11/10/93 (ACD): First stable version.
*     24/1/94  (ACD): Modified error reporting.
*     8/2/94   (ACD): Changed parameters for data type codes.
*     14/8/96  (ACD): Remove non-printable characters from returned
*        CHARACTER attributes.
*     27/5/98  (ACD): Speeded up search for the attribute.
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
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
      INCLUDE 'CAT1_ATTRB_CMN'    ! Attributes common block.
*  Arguments Given:
      INTEGER
     :  GI
      CHARACTER*(*)
     :  ATTRIB
*  Arguments Returned:
      INTEGER*2
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  LOOP,   ! Loop index.
     :  ATELM,  ! Element in attribute arrays.
     :  ATYPE   ! Data type for attribute.
      LOGICAL
     :  FOUND,  ! Flag: has the attribute been found?
     :  MORE,   ! Flag; continue hunting for attribute?
     :  CONVOK  ! Flag: error in the type conversion?

*
*    The following variables hold the value obtained for the named
*    attribute in the data type of the attribute.  In a given invocation
*    only the variable corresponding to the data type of the attribute
*    is used.

      BYTE             VALUB ! Unsigned byte value.
      BYTE             VALB  ! Byte value.
      INTEGER*2        VALUW ! Unsigned word value.
      INTEGER*2        VALW  ! Word value.
      INTEGER          VALI  ! Integer value.
      REAL             VALR  ! Real value.
      DOUBLE PRECISION VALD  ! Double precision value.
      LOGICAL          VALL  ! Logical value.
      CHARACTER        VALC*(CAT1__SZATS)   ! Character value.

*
*    The following variables hold a local of the returned value,
*    converted to data type W.  In the instantiation of the routine
*    of type W only the variable LVALW is used.  The purpose of
*    having the variables at all is to convert the local fixed length
*    string to the returned fixed length string.

      BYTE      LVALUB
      BYTE      LVALB
      INTEGER*2 LVALUW
      INTEGER*2 LVALW
      INTEGER   LVALI
      REAL      LVALR
      DOUBLE PRECISION LVALD
      LOGICAL   LVALL
      CHARACTER LVALC*(CAT1__SZATS)
*.

      IF (STATUS .EQ. CAT__OK) THEN

C        write(17, 1000) gi, attrib, status
C1000    format(1x, 'TIQAW: gi, attrib, status: ',
C    :     i5, 1x, a, 1x, i10 )

*
*       Check that the identifier is valid.

         IF (GI .GT. 0  .AND.  GI .LE. NIDS__CAT1) THEN

*
*          Attempt to locate the attribute.

            FOUND = .FALSE.
            MORE = .TRUE.
            LOOP = IDATT__CAT1(GI) - 1

            DO WHILE (MORE)
               LOOP = LOOP + 1

               IF (GI .EQ. ATTID__CAT1(LOOP)  .AND.
     :             ATTRIB .EQ. ATTNM__CAT1(LOOP) ) THEN
                  ATELM = LOOP
                  FOUND = .TRUE.
                  MORE = .FALSE.
               END IF

               IF (LOOP .GE. NATT__CAT1) THEN
                  MORE = .FALSE.
               END IF
            END DO

*
*          Proceed if the attribute has been found.

            IF (FOUND) THEN

*
*             Determine the type of the attribute and obtain its value.

               ATYPE = ATTYP__CAT1(ATELM)

               IF (ATYPE .EQ. CAT__TYPEUB) THEN
                  VALUB = ATTVV__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEB) THEN
                  VALB = ATTVB__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEUW) THEN
                  VALUW = ATTVU__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEW) THEN
                  VALW = ATTVW__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEI) THEN
                  VALI = ATTVI__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPER) THEN
                  VALR = ATTVR__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPED) THEN
                  VALD = ATTVD__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEL) THEN
                  VALL = ATTVL__CAT1(ATELM)

               ELSE IF (ATYPE .EQ. CAT__TYPEC) THEN
                  VALC = ATTVC__CAT1(ATELM)
                  CALL CHR_CLEAN(VALC)

               END IF

*
*             Perform the type conversion.

               CALL CAT1_TCNVT (ATYPE, VALUB, VALB, VALUW, VALW, VALI,
     :           VALR, VALD, VALL, VALC,
     :           CAT__TYPEW, LVALUB, LVALB, LVALUW, LVALW, LVALI, LVALR,
     :           LVALD, LVALL, LVALC, CONVOK, STATUS)

               VALUE = LVALW

               IF (STATUS .NE. CAT__OK) THEN
                  STATUS = CAT__TYPCV
               END IF

            ELSE
               STATUS = CAT__NOATT
            END IF

         ELSE
            STATUS = CAT__INVID
         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_TIQAW_ERR', 'CAT_TIQAW: error '/
     :        /'inquiring the value of an attribute.', STATUS)
         END IF

      END IF

      END
