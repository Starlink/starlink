      SUBROUTINE CAT_TATTB (GI, ATTRIB, VALUE, STATUS)
*+
*  Name:
*     CAT_TATTB
*  Purpose:
*     Set an attribute of a component to a given value.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TATTB (GI, ATTRIB, VALUEB; STATUS)
*  Description:
*     Set an attribute of a component to a given value.  Type
*     conversions are performed if necessary.
*  Arguments:
*     GI  =  INTEGER (Given)
*        Generic component identifier.
*     ATTRIB  =  CHARACTER*(*) (Given)
*        Name of the attribute to be set.
*     VALUE  =  BYTE  (Given)
*        Value to which the attribute is to be set.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the identifier is valid then
*       Attempt to locate the attribute.
*       If it is found then
*         If it is mutable then
*           Determine its type.
*           Perform any necessary type conversion.
*           If ok then
*             Put the value to the attribute.
*           end if
*           Check whether the mutability flag needs to be reset and if
*           so then reset it.
*         else
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
*     8/3/94   (ACD): Changed parameters for data type codes.
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
      BYTE
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  LOOP,   ! Loop index.
     :  ATELM,  ! Element in attribute arrays.
     :  ATYPE,  ! Data type for attribute.
     :  GITYPE  ! Type of identifier GI (column, parameter etc.).
      LOGICAL
     :  FOUND,  ! Flag: has the attribute been found?
     :  MORE,   ! Flag; continue hunting for attribute?
     :  CONVOK, ! Flag: error in the type conversion?
     :  MUTBLE  ! Flag: is the attribute of a mutable type?

*
*    The following variables hold a local of the passed argument.
*    In the instantiation of the routine of type B only the variable
*    LVALB is used.  The purpose of having the variables at all is
*    to convert the passed length character to a fixed length value.

      BYTE      LVALUB
      BYTE      LVALB
      INTEGER*2 LVALUW
      INTEGER*2 LVALW
      INTEGER   LVALI
      REAL      LVALR
      DOUBLE PRECISION LVALD
      LOGICAL   LVALL
      CHARACTER LVALC*(CAT1__SZATS)

*
*    The following variables hold the passed value after conversion to
*    the data type of the attribute.  In a given invocation only the
*    variable corresponding to the data type of the attribute is used.

      BYTE             VALUB ! Unsigned byte value.
      BYTE             VALB  ! Byte value.
      INTEGER*2        VALUW ! Unsigned word value.
      INTEGER*2        VALW  ! Word value.
      INTEGER          VALI  ! Integer value.
      REAL             VALR  ! Real value.
      DOUBLE PRECISION VALD  ! Double precision value.
      LOGICAL          VALL  ! Logical value.
      CHARACTER        VALC*(CAT1__SZATS)   ! Character value.
*.

      IF (STATUS .EQ. CAT__OK) THEN

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
*             Check whether the attribute is mutable.

               IF (ATTMU__CAT1(ATELM) ) THEN

*
*                Determine the type of the attribute and perform any
*                necessary type conversion.

                  ATYPE = ATTYP__CAT1(ATELM)

                  LVALB = VALUE

                  CALL CAT1_TCNVT (CAT__TYPEB, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALR, LVALD, LVALL, LVALC,
     :              ATYPE, VALUB, VALB, VALUW, VALW, VALI, VALR, VALD,
     :              VALL, VALC, CONVOK, STATUS)

                  IF (STATUS .EQ. CAT__OK) THEN
                     IF (ATYPE .EQ. CAT__TYPEUB) THEN
                        ATTVV__CAT1(ATELM) = VALUB

                     ELSE IF (ATYPE .EQ. CAT__TYPEB) THEN
                        ATTVB__CAT1(ATELM) = VALB

                     ELSE IF (ATYPE .EQ. CAT__TYPEUW) THEN
                        ATTVU__CAT1(ATELM) = VALUW

                     ELSE IF (ATYPE .EQ. CAT__TYPEW) THEN
                        ATTVW__CAT1(ATELM) = VALW

                     ELSE IF (ATYPE .EQ. CAT__TYPEI) THEN
                        ATTVI__CAT1(ATELM) = VALI

                     ELSE IF (ATYPE .EQ. CAT__TYPER) THEN
                        ATTVR__CAT1(ATELM) = VALR

                     ELSE IF (ATYPE .EQ. CAT__TYPED) THEN
                        ATTVD__CAT1(ATELM) = VALD

                     ELSE IF (ATYPE .EQ. CAT__TYPEL) THEN
                        ATTVL__CAT1(ATELM) = VALL

                     ELSE IF (ATYPE .EQ. CAT__TYPEC) THEN
                        ATTVC__CAT1(ATELM) = VALC

                     END IF
                  ELSE
                     STATUS = CAT__TYPCV

                  END IF

*
*                Check whether the mutability flag needs to be reset, and
*                if so then reset it (immutable attributes may initially be
*                set once, but not thereafter).

                  CALL CAT_TIDTP (GI, GITYPE, STATUS)
                  CALL CAT1_CKMUT (GITYPE, ATTRIB, MUTBLE, STATUS)

                  IF (.NOT. MUTBLE) THEN
                     ATTMU__CAT1(ATELM) = .FALSE.
                  END IF

               ELSE
                  STATUS = CAT__IMATT

               END IF
            ELSE
               STATUS = CAT__NOATT

            END IF
         ELSE
            STATUS = CAT__INVID

         END IF

*
*          Report any error.

            IF (STATUS .NE. CAT__OK) THEN
               CALL CAT1_ERREP ('CAT_TATTB_ERR', 'CAT_TATTB: '/
     :           /'error setting a component attribute.', STATUS)
            END IF

      END IF

      END
      SUBROUTINE CAT_TATTC (GI, ATTRIB, VALUE, STATUS)
*+
*  Name:
*     CAT_TATTC
*  Purpose:
*     Set an attribute of a component to a given value.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TATTC (GI, ATTRIB, VALUEC; STATUS)
*  Description:
*     Set an attribute of a component to a given value.  Type
*     conversions are performed if necessary.
*  Arguments:
*     GI  =  INTEGER (Given)
*        Generic component identifier.
*     ATTRIB  =  CHARACTER*(*) (Given)
*        Name of the attribute to be set.
*     VALUE  =  CHARACTER*(*)  (Given)
*        Value to which the attribute is to be set.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the identifier is valid then
*       Attempt to locate the attribute.
*       If it is found then
*         If it is mutable then
*           Determine its type.
*           Perform any necessary type conversion.
*           If ok then
*             Put the value to the attribute.
*           end if
*           Check whether the mutability flag needs to be reset and if
*           so then reset it.
*         else
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
*     8/3/94   (ACD): Changed parameters for data type codes.
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
      CHARACTER*(*)
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  LOOP,   ! Loop index.
     :  ATELM,  ! Element in attribute arrays.
     :  ATYPE,  ! Data type for attribute.
     :  GITYPE  ! Type of identifier GI (column, parameter etc.).
      LOGICAL
     :  FOUND,  ! Flag: has the attribute been found?
     :  MORE,   ! Flag; continue hunting for attribute?
     :  CONVOK, ! Flag: error in the type conversion?
     :  MUTBLE  ! Flag: is the attribute of a mutable type?

*
*    The following variables hold a local of the passed argument.
*    In the instantiation of the routine of type C only the variable
*    LVALC is used.  The purpose of having the variables at all is
*    to convert the passed length character to a fixed length value.

      BYTE      LVALUB
      BYTE      LVALB
      INTEGER*2 LVALUW
      INTEGER*2 LVALW
      INTEGER   LVALI
      REAL      LVALR
      DOUBLE PRECISION LVALD
      LOGICAL   LVALL
      CHARACTER LVALC*(CAT1__SZATS)

*
*    The following variables hold the passed value after conversion to
*    the data type of the attribute.  In a given invocation only the
*    variable corresponding to the data type of the attribute is used.

      BYTE             VALUB ! Unsigned byte value.
      BYTE             VALB  ! Byte value.
      INTEGER*2        VALUW ! Unsigned word value.
      INTEGER*2        VALW  ! Word value.
      INTEGER          VALI  ! Integer value.
      REAL             VALR  ! Real value.
      DOUBLE PRECISION VALD  ! Double precision value.
      LOGICAL          VALL  ! Logical value.
      CHARACTER        VALC*(CAT1__SZATS)   ! Character value.
*.

      IF (STATUS .EQ. CAT__OK) THEN

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
*             Check whether the attribute is mutable.

               IF (ATTMU__CAT1(ATELM) ) THEN

*
*                Determine the type of the attribute and perform any
*                necessary type conversion.

                  ATYPE = ATTYP__CAT1(ATELM)

                  LVALC = VALUE

                  CALL CAT1_TCNVT (CAT__TYPEC, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALR, LVALD, LVALL, LVALC,
     :              ATYPE, VALUB, VALB, VALUW, VALW, VALI, VALR, VALD,
     :              VALL, VALC, CONVOK, STATUS)

                  IF (STATUS .EQ. CAT__OK) THEN
                     IF (ATYPE .EQ. CAT__TYPEUB) THEN
                        ATTVV__CAT1(ATELM) = VALUB

                     ELSE IF (ATYPE .EQ. CAT__TYPEB) THEN
                        ATTVB__CAT1(ATELM) = VALB

                     ELSE IF (ATYPE .EQ. CAT__TYPEUW) THEN
                        ATTVU__CAT1(ATELM) = VALUW

                     ELSE IF (ATYPE .EQ. CAT__TYPEW) THEN
                        ATTVW__CAT1(ATELM) = VALW

                     ELSE IF (ATYPE .EQ. CAT__TYPEI) THEN
                        ATTVI__CAT1(ATELM) = VALI

                     ELSE IF (ATYPE .EQ. CAT__TYPER) THEN
                        ATTVR__CAT1(ATELM) = VALR

                     ELSE IF (ATYPE .EQ. CAT__TYPED) THEN
                        ATTVD__CAT1(ATELM) = VALD

                     ELSE IF (ATYPE .EQ. CAT__TYPEL) THEN
                        ATTVL__CAT1(ATELM) = VALL

                     ELSE IF (ATYPE .EQ. CAT__TYPEC) THEN
                        ATTVC__CAT1(ATELM) = VALC

                     END IF
                  ELSE
                     STATUS = CAT__TYPCV

                  END IF

*
*                Check whether the mutability flag needs to be reset, and
*                if so then reset it (immutable attributes may initially be
*                set once, but not thereafter).

                  CALL CAT_TIDTP (GI, GITYPE, STATUS)
                  CALL CAT1_CKMUT (GITYPE, ATTRIB, MUTBLE, STATUS)

                  IF (.NOT. MUTBLE) THEN
                     ATTMU__CAT1(ATELM) = .FALSE.
                  END IF

               ELSE
                  STATUS = CAT__IMATT

               END IF
            ELSE
               STATUS = CAT__NOATT

            END IF
         ELSE
            STATUS = CAT__INVID

         END IF

*
*          Report any error.

            IF (STATUS .NE. CAT__OK) THEN
               CALL CAT1_ERREP ('CAT_TATTC_ERR', 'CAT_TATTC: '/
     :           /'error setting a component attribute.', STATUS)
            END IF

      END IF

      END
      SUBROUTINE CAT_TATTD (GI, ATTRIB, VALUE, STATUS)
*+
*  Name:
*     CAT_TATTD
*  Purpose:
*     Set an attribute of a component to a given value.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TATTD (GI, ATTRIB, VALUED; STATUS)
*  Description:
*     Set an attribute of a component to a given value.  Type
*     conversions are performed if necessary.
*  Arguments:
*     GI  =  INTEGER (Given)
*        Generic component identifier.
*     ATTRIB  =  CHARACTER*(*) (Given)
*        Name of the attribute to be set.
*     VALUE  =  DOUBLE PRECISION  (Given)
*        Value to which the attribute is to be set.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the identifier is valid then
*       Attempt to locate the attribute.
*       If it is found then
*         If it is mutable then
*           Determine its type.
*           Perform any necessary type conversion.
*           If ok then
*             Put the value to the attribute.
*           end if
*           Check whether the mutability flag needs to be reset and if
*           so then reset it.
*         else
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
*     8/3/94   (ACD): Changed parameters for data type codes.
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
      DOUBLE PRECISION
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  LOOP,   ! Loop index.
     :  ATELM,  ! Element in attribute arrays.
     :  ATYPE,  ! Data type for attribute.
     :  GITYPE  ! Type of identifier GI (column, parameter etc.).
      LOGICAL
     :  FOUND,  ! Flag: has the attribute been found?
     :  MORE,   ! Flag; continue hunting for attribute?
     :  CONVOK, ! Flag: error in the type conversion?
     :  MUTBLE  ! Flag: is the attribute of a mutable type?

*
*    The following variables hold a local of the passed argument.
*    In the instantiation of the routine of type D only the variable
*    LVALD is used.  The purpose of having the variables at all is
*    to convert the passed length character to a fixed length value.

      BYTE      LVALUB
      BYTE      LVALB
      INTEGER*2 LVALUW
      INTEGER*2 LVALW
      INTEGER   LVALI
      REAL      LVALR
      DOUBLE PRECISION LVALD
      LOGICAL   LVALL
      CHARACTER LVALC*(CAT1__SZATS)

*
*    The following variables hold the passed value after conversion to
*    the data type of the attribute.  In a given invocation only the
*    variable corresponding to the data type of the attribute is used.

      BYTE             VALUB ! Unsigned byte value.
      BYTE             VALB  ! Byte value.
      INTEGER*2        VALUW ! Unsigned word value.
      INTEGER*2        VALW  ! Word value.
      INTEGER          VALI  ! Integer value.
      REAL             VALR  ! Real value.
      DOUBLE PRECISION VALD  ! Double precision value.
      LOGICAL          VALL  ! Logical value.
      CHARACTER        VALC*(CAT1__SZATS)   ! Character value.
*.

      IF (STATUS .EQ. CAT__OK) THEN

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
*             Check whether the attribute is mutable.

               IF (ATTMU__CAT1(ATELM) ) THEN

*
*                Determine the type of the attribute and perform any
*                necessary type conversion.

                  ATYPE = ATTYP__CAT1(ATELM)

                  LVALD = VALUE

                  CALL CAT1_TCNVT (CAT__TYPED, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALR, LVALD, LVALL, LVALC,
     :              ATYPE, VALUB, VALB, VALUW, VALW, VALI, VALR, VALD,
     :              VALL, VALC, CONVOK, STATUS)

                  IF (STATUS .EQ. CAT__OK) THEN
                     IF (ATYPE .EQ. CAT__TYPEUB) THEN
                        ATTVV__CAT1(ATELM) = VALUB

                     ELSE IF (ATYPE .EQ. CAT__TYPEB) THEN
                        ATTVB__CAT1(ATELM) = VALB

                     ELSE IF (ATYPE .EQ. CAT__TYPEUW) THEN
                        ATTVU__CAT1(ATELM) = VALUW

                     ELSE IF (ATYPE .EQ. CAT__TYPEW) THEN
                        ATTVW__CAT1(ATELM) = VALW

                     ELSE IF (ATYPE .EQ. CAT__TYPEI) THEN
                        ATTVI__CAT1(ATELM) = VALI

                     ELSE IF (ATYPE .EQ. CAT__TYPER) THEN
                        ATTVR__CAT1(ATELM) = VALR

                     ELSE IF (ATYPE .EQ. CAT__TYPED) THEN
                        ATTVD__CAT1(ATELM) = VALD

                     ELSE IF (ATYPE .EQ. CAT__TYPEL) THEN
                        ATTVL__CAT1(ATELM) = VALL

                     ELSE IF (ATYPE .EQ. CAT__TYPEC) THEN
                        ATTVC__CAT1(ATELM) = VALC

                     END IF
                  ELSE
                     STATUS = CAT__TYPCV

                  END IF

*
*                Check whether the mutability flag needs to be reset, and
*                if so then reset it (immutable attributes may initially be
*                set once, but not thereafter).

                  CALL CAT_TIDTP (GI, GITYPE, STATUS)
                  CALL CAT1_CKMUT (GITYPE, ATTRIB, MUTBLE, STATUS)

                  IF (.NOT. MUTBLE) THEN
                     ATTMU__CAT1(ATELM) = .FALSE.
                  END IF

               ELSE
                  STATUS = CAT__IMATT

               END IF
            ELSE
               STATUS = CAT__NOATT

            END IF
         ELSE
            STATUS = CAT__INVID

         END IF

*
*          Report any error.

            IF (STATUS .NE. CAT__OK) THEN
               CALL CAT1_ERREP ('CAT_TATTD_ERR', 'CAT_TATTD: '/
     :           /'error setting a component attribute.', STATUS)
            END IF

      END IF

      END
      SUBROUTINE CAT_TATTI (GI, ATTRIB, VALUE, STATUS)
*+
*  Name:
*     CAT_TATTI
*  Purpose:
*     Set an attribute of a component to a given value.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TATTI (GI, ATTRIB, VALUEI; STATUS)
*  Description:
*     Set an attribute of a component to a given value.  Type
*     conversions are performed if necessary.
*  Arguments:
*     GI  =  INTEGER (Given)
*        Generic component identifier.
*     ATTRIB  =  CHARACTER*(*) (Given)
*        Name of the attribute to be set.
*     VALUE  =  INTEGER  (Given)
*        Value to which the attribute is to be set.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the identifier is valid then
*       Attempt to locate the attribute.
*       If it is found then
*         If it is mutable then
*           Determine its type.
*           Perform any necessary type conversion.
*           If ok then
*             Put the value to the attribute.
*           end if
*           Check whether the mutability flag needs to be reset and if
*           so then reset it.
*         else
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
*     8/3/94   (ACD): Changed parameters for data type codes.
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
      INTEGER
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  LOOP,   ! Loop index.
     :  ATELM,  ! Element in attribute arrays.
     :  ATYPE,  ! Data type for attribute.
     :  GITYPE  ! Type of identifier GI (column, parameter etc.).
      LOGICAL
     :  FOUND,  ! Flag: has the attribute been found?
     :  MORE,   ! Flag; continue hunting for attribute?
     :  CONVOK, ! Flag: error in the type conversion?
     :  MUTBLE  ! Flag: is the attribute of a mutable type?

*
*    The following variables hold a local of the passed argument.
*    In the instantiation of the routine of type I only the variable
*    LVALI is used.  The purpose of having the variables at all is
*    to convert the passed length character to a fixed length value.

      BYTE      LVALUB
      BYTE      LVALB
      INTEGER*2 LVALUW
      INTEGER*2 LVALW
      INTEGER   LVALI
      REAL      LVALR
      DOUBLE PRECISION LVALD
      LOGICAL   LVALL
      CHARACTER LVALC*(CAT1__SZATS)

*
*    The following variables hold the passed value after conversion to
*    the data type of the attribute.  In a given invocation only the
*    variable corresponding to the data type of the attribute is used.

      BYTE             VALUB ! Unsigned byte value.
      BYTE             VALB  ! Byte value.
      INTEGER*2        VALUW ! Unsigned word value.
      INTEGER*2        VALW  ! Word value.
      INTEGER          VALI  ! Integer value.
      REAL             VALR  ! Real value.
      DOUBLE PRECISION VALD  ! Double precision value.
      LOGICAL          VALL  ! Logical value.
      CHARACTER        VALC*(CAT1__SZATS)   ! Character value.
*.

      IF (STATUS .EQ. CAT__OK) THEN

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
*             Check whether the attribute is mutable.

               IF (ATTMU__CAT1(ATELM) ) THEN

*
*                Determine the type of the attribute and perform any
*                necessary type conversion.

                  ATYPE = ATTYP__CAT1(ATELM)

                  LVALI = VALUE

                  CALL CAT1_TCNVT (CAT__TYPEI, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALR, LVALD, LVALL, LVALC,
     :              ATYPE, VALUB, VALB, VALUW, VALW, VALI, VALR, VALD,
     :              VALL, VALC, CONVOK, STATUS)

                  IF (STATUS .EQ. CAT__OK) THEN
                     IF (ATYPE .EQ. CAT__TYPEUB) THEN
                        ATTVV__CAT1(ATELM) = VALUB

                     ELSE IF (ATYPE .EQ. CAT__TYPEB) THEN
                        ATTVB__CAT1(ATELM) = VALB

                     ELSE IF (ATYPE .EQ. CAT__TYPEUW) THEN
                        ATTVU__CAT1(ATELM) = VALUW

                     ELSE IF (ATYPE .EQ. CAT__TYPEW) THEN
                        ATTVW__CAT1(ATELM) = VALW

                     ELSE IF (ATYPE .EQ. CAT__TYPEI) THEN
                        ATTVI__CAT1(ATELM) = VALI

                     ELSE IF (ATYPE .EQ. CAT__TYPER) THEN
                        ATTVR__CAT1(ATELM) = VALR

                     ELSE IF (ATYPE .EQ. CAT__TYPED) THEN
                        ATTVD__CAT1(ATELM) = VALD

                     ELSE IF (ATYPE .EQ. CAT__TYPEL) THEN
                        ATTVL__CAT1(ATELM) = VALL

                     ELSE IF (ATYPE .EQ. CAT__TYPEC) THEN
                        ATTVC__CAT1(ATELM) = VALC

                     END IF
                  ELSE
                     STATUS = CAT__TYPCV

                  END IF

*
*                Check whether the mutability flag needs to be reset, and
*                if so then reset it (immutable attributes may initially be
*                set once, but not thereafter).

                  CALL CAT_TIDTP (GI, GITYPE, STATUS)
                  CALL CAT1_CKMUT (GITYPE, ATTRIB, MUTBLE, STATUS)

                  IF (.NOT. MUTBLE) THEN
                     ATTMU__CAT1(ATELM) = .FALSE.
                  END IF

               ELSE
                  STATUS = CAT__IMATT

               END IF
            ELSE
               STATUS = CAT__NOATT

            END IF
         ELSE
            STATUS = CAT__INVID

         END IF

*
*          Report any error.

            IF (STATUS .NE. CAT__OK) THEN
               CALL CAT1_ERREP ('CAT_TATTI_ERR', 'CAT_TATTI: '/
     :           /'error setting a component attribute.', STATUS)
            END IF

      END IF

      END
      SUBROUTINE CAT_TATTL (GI, ATTRIB, VALUE, STATUS)
*+
*  Name:
*     CAT_TATTL
*  Purpose:
*     Set an attribute of a component to a given value.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TATTL (GI, ATTRIB, VALUEL; STATUS)
*  Description:
*     Set an attribute of a component to a given value.  Type
*     conversions are performed if necessary.
*  Arguments:
*     GI  =  INTEGER (Given)
*        Generic component identifier.
*     ATTRIB  =  CHARACTER*(*) (Given)
*        Name of the attribute to be set.
*     VALUE  =  LOGICAL  (Given)
*        Value to which the attribute is to be set.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the identifier is valid then
*       Attempt to locate the attribute.
*       If it is found then
*         If it is mutable then
*           Determine its type.
*           Perform any necessary type conversion.
*           If ok then
*             Put the value to the attribute.
*           end if
*           Check whether the mutability flag needs to be reset and if
*           so then reset it.
*         else
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
*     8/3/94   (ACD): Changed parameters for data type codes.
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
      LOGICAL
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  LOOP,   ! Loop index.
     :  ATELM,  ! Element in attribute arrays.
     :  ATYPE,  ! Data type for attribute.
     :  GITYPE  ! Type of identifier GI (column, parameter etc.).
      LOGICAL
     :  FOUND,  ! Flag: has the attribute been found?
     :  MORE,   ! Flag; continue hunting for attribute?
     :  CONVOK, ! Flag: error in the type conversion?
     :  MUTBLE  ! Flag: is the attribute of a mutable type?

*
*    The following variables hold a local of the passed argument.
*    In the instantiation of the routine of type L only the variable
*    LVALL is used.  The purpose of having the variables at all is
*    to convert the passed length character to a fixed length value.

      BYTE      LVALUB
      BYTE      LVALB
      INTEGER*2 LVALUW
      INTEGER*2 LVALW
      INTEGER   LVALI
      REAL      LVALR
      DOUBLE PRECISION LVALD
      LOGICAL   LVALL
      CHARACTER LVALC*(CAT1__SZATS)

*
*    The following variables hold the passed value after conversion to
*    the data type of the attribute.  In a given invocation only the
*    variable corresponding to the data type of the attribute is used.

      BYTE             VALUB ! Unsigned byte value.
      BYTE             VALB  ! Byte value.
      INTEGER*2        VALUW ! Unsigned word value.
      INTEGER*2        VALW  ! Word value.
      INTEGER          VALI  ! Integer value.
      REAL             VALR  ! Real value.
      DOUBLE PRECISION VALD  ! Double precision value.
      LOGICAL          VALL  ! Logical value.
      CHARACTER        VALC*(CAT1__SZATS)   ! Character value.
*.

      IF (STATUS .EQ. CAT__OK) THEN

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
*             Check whether the attribute is mutable.

               IF (ATTMU__CAT1(ATELM) ) THEN

*
*                Determine the type of the attribute and perform any
*                necessary type conversion.

                  ATYPE = ATTYP__CAT1(ATELM)

                  LVALL = VALUE

                  CALL CAT1_TCNVT (CAT__TYPEL, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALR, LVALD, LVALL, LVALC,
     :              ATYPE, VALUB, VALB, VALUW, VALW, VALI, VALR, VALD,
     :              VALL, VALC, CONVOK, STATUS)

                  IF (STATUS .EQ. CAT__OK) THEN
                     IF (ATYPE .EQ. CAT__TYPEUB) THEN
                        ATTVV__CAT1(ATELM) = VALUB

                     ELSE IF (ATYPE .EQ. CAT__TYPEB) THEN
                        ATTVB__CAT1(ATELM) = VALB

                     ELSE IF (ATYPE .EQ. CAT__TYPEUW) THEN
                        ATTVU__CAT1(ATELM) = VALUW

                     ELSE IF (ATYPE .EQ. CAT__TYPEW) THEN
                        ATTVW__CAT1(ATELM) = VALW

                     ELSE IF (ATYPE .EQ. CAT__TYPEI) THEN
                        ATTVI__CAT1(ATELM) = VALI

                     ELSE IF (ATYPE .EQ. CAT__TYPER) THEN
                        ATTVR__CAT1(ATELM) = VALR

                     ELSE IF (ATYPE .EQ. CAT__TYPED) THEN
                        ATTVD__CAT1(ATELM) = VALD

                     ELSE IF (ATYPE .EQ. CAT__TYPEL) THEN
                        ATTVL__CAT1(ATELM) = VALL

                     ELSE IF (ATYPE .EQ. CAT__TYPEC) THEN
                        ATTVC__CAT1(ATELM) = VALC

                     END IF
                  ELSE
                     STATUS = CAT__TYPCV

                  END IF

*
*                Check whether the mutability flag needs to be reset, and
*                if so then reset it (immutable attributes may initially be
*                set once, but not thereafter).

                  CALL CAT_TIDTP (GI, GITYPE, STATUS)
                  CALL CAT1_CKMUT (GITYPE, ATTRIB, MUTBLE, STATUS)

                  IF (.NOT. MUTBLE) THEN
                     ATTMU__CAT1(ATELM) = .FALSE.
                  END IF

               ELSE
                  STATUS = CAT__IMATT

               END IF
            ELSE
               STATUS = CAT__NOATT

            END IF
         ELSE
            STATUS = CAT__INVID

         END IF

*
*          Report any error.

            IF (STATUS .NE. CAT__OK) THEN
               CALL CAT1_ERREP ('CAT_TATTL_ERR', 'CAT_TATTL: '/
     :           /'error setting a component attribute.', STATUS)
            END IF

      END IF

      END
      SUBROUTINE CAT_TATTR (GI, ATTRIB, VALUE, STATUS)
*+
*  Name:
*     CAT_TATTR
*  Purpose:
*     Set an attribute of a component to a given value.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TATTR (GI, ATTRIB, VALUER; STATUS)
*  Description:
*     Set an attribute of a component to a given value.  Type
*     conversions are performed if necessary.
*  Arguments:
*     GI  =  INTEGER (Given)
*        Generic component identifier.
*     ATTRIB  =  CHARACTER*(*) (Given)
*        Name of the attribute to be set.
*     VALUE  =  REAL  (Given)
*        Value to which the attribute is to be set.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the identifier is valid then
*       Attempt to locate the attribute.
*       If it is found then
*         If it is mutable then
*           Determine its type.
*           Perform any necessary type conversion.
*           If ok then
*             Put the value to the attribute.
*           end if
*           Check whether the mutability flag needs to be reset and if
*           so then reset it.
*         else
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
*     8/3/94   (ACD): Changed parameters for data type codes.
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
      REAL
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  LOOP,   ! Loop index.
     :  ATELM,  ! Element in attribute arrays.
     :  ATYPE,  ! Data type for attribute.
     :  GITYPE  ! Type of identifier GI (column, parameter etc.).
      LOGICAL
     :  FOUND,  ! Flag: has the attribute been found?
     :  MORE,   ! Flag; continue hunting for attribute?
     :  CONVOK, ! Flag: error in the type conversion?
     :  MUTBLE  ! Flag: is the attribute of a mutable type?

*
*    The following variables hold a local of the passed argument.
*    In the instantiation of the routine of type R only the variable
*    LVALR is used.  The purpose of having the variables at all is
*    to convert the passed length character to a fixed length value.

      BYTE      LVALUB
      BYTE      LVALB
      INTEGER*2 LVALUW
      INTEGER*2 LVALW
      INTEGER   LVALI
      REAL      LVALR
      DOUBLE PRECISION LVALD
      LOGICAL   LVALL
      CHARACTER LVALC*(CAT1__SZATS)

*
*    The following variables hold the passed value after conversion to
*    the data type of the attribute.  In a given invocation only the
*    variable corresponding to the data type of the attribute is used.

      BYTE             VALUB ! Unsigned byte value.
      BYTE             VALB  ! Byte value.
      INTEGER*2        VALUW ! Unsigned word value.
      INTEGER*2        VALW  ! Word value.
      INTEGER          VALI  ! Integer value.
      REAL             VALR  ! Real value.
      DOUBLE PRECISION VALD  ! Double precision value.
      LOGICAL          VALL  ! Logical value.
      CHARACTER        VALC*(CAT1__SZATS)   ! Character value.
*.

      IF (STATUS .EQ. CAT__OK) THEN

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
*             Check whether the attribute is mutable.

               IF (ATTMU__CAT1(ATELM) ) THEN

*
*                Determine the type of the attribute and perform any
*                necessary type conversion.

                  ATYPE = ATTYP__CAT1(ATELM)

                  LVALR = VALUE

                  CALL CAT1_TCNVT (CAT__TYPER, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALR, LVALD, LVALL, LVALC,
     :              ATYPE, VALUB, VALB, VALUW, VALW, VALI, VALR, VALD,
     :              VALL, VALC, CONVOK, STATUS)

                  IF (STATUS .EQ. CAT__OK) THEN
                     IF (ATYPE .EQ. CAT__TYPEUB) THEN
                        ATTVV__CAT1(ATELM) = VALUB

                     ELSE IF (ATYPE .EQ. CAT__TYPEB) THEN
                        ATTVB__CAT1(ATELM) = VALB

                     ELSE IF (ATYPE .EQ. CAT__TYPEUW) THEN
                        ATTVU__CAT1(ATELM) = VALUW

                     ELSE IF (ATYPE .EQ. CAT__TYPEW) THEN
                        ATTVW__CAT1(ATELM) = VALW

                     ELSE IF (ATYPE .EQ. CAT__TYPEI) THEN
                        ATTVI__CAT1(ATELM) = VALI

                     ELSE IF (ATYPE .EQ. CAT__TYPER) THEN
                        ATTVR__CAT1(ATELM) = VALR

                     ELSE IF (ATYPE .EQ. CAT__TYPED) THEN
                        ATTVD__CAT1(ATELM) = VALD

                     ELSE IF (ATYPE .EQ. CAT__TYPEL) THEN
                        ATTVL__CAT1(ATELM) = VALL

                     ELSE IF (ATYPE .EQ. CAT__TYPEC) THEN
                        ATTVC__CAT1(ATELM) = VALC

                     END IF
                  ELSE
                     STATUS = CAT__TYPCV

                  END IF

*
*                Check whether the mutability flag needs to be reset, and
*                if so then reset it (immutable attributes may initially be
*                set once, but not thereafter).

                  CALL CAT_TIDTP (GI, GITYPE, STATUS)
                  CALL CAT1_CKMUT (GITYPE, ATTRIB, MUTBLE, STATUS)

                  IF (.NOT. MUTBLE) THEN
                     ATTMU__CAT1(ATELM) = .FALSE.
                  END IF

               ELSE
                  STATUS = CAT__IMATT

               END IF
            ELSE
               STATUS = CAT__NOATT

            END IF
         ELSE
            STATUS = CAT__INVID

         END IF

*
*          Report any error.

            IF (STATUS .NE. CAT__OK) THEN
               CALL CAT1_ERREP ('CAT_TATTR_ERR', 'CAT_TATTR: '/
     :           /'error setting a component attribute.', STATUS)
            END IF

      END IF

      END
      SUBROUTINE CAT_TATTW (GI, ATTRIB, VALUE, STATUS)
*+
*  Name:
*     CAT_TATTW
*  Purpose:
*     Set an attribute of a component to a given value.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TATTW (GI, ATTRIB, VALUEW; STATUS)
*  Description:
*     Set an attribute of a component to a given value.  Type
*     conversions are performed if necessary.
*  Arguments:
*     GI  =  INTEGER (Given)
*        Generic component identifier.
*     ATTRIB  =  CHARACTER*(*) (Given)
*        Name of the attribute to be set.
*     VALUE  =  INTEGER*2  (Given)
*        Value to which the attribute is to be set.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the identifier is valid then
*       Attempt to locate the attribute.
*       If it is found then
*         If it is mutable then
*           Determine its type.
*           Perform any necessary type conversion.
*           If ok then
*             Put the value to the attribute.
*           end if
*           Check whether the mutability flag needs to be reset and if
*           so then reset it.
*         else
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
*     8/3/94   (ACD): Changed parameters for data type codes.
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
      INTEGER*2
     :  VALUE
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  LOOP,   ! Loop index.
     :  ATELM,  ! Element in attribute arrays.
     :  ATYPE,  ! Data type for attribute.
     :  GITYPE  ! Type of identifier GI (column, parameter etc.).
      LOGICAL
     :  FOUND,  ! Flag: has the attribute been found?
     :  MORE,   ! Flag; continue hunting for attribute?
     :  CONVOK, ! Flag: error in the type conversion?
     :  MUTBLE  ! Flag: is the attribute of a mutable type?

*
*    The following variables hold a local of the passed argument.
*    In the instantiation of the routine of type W only the variable
*    LVALW is used.  The purpose of having the variables at all is
*    to convert the passed length character to a fixed length value.

      BYTE      LVALUB
      BYTE      LVALB
      INTEGER*2 LVALUW
      INTEGER*2 LVALW
      INTEGER   LVALI
      REAL      LVALR
      DOUBLE PRECISION LVALD
      LOGICAL   LVALL
      CHARACTER LVALC*(CAT1__SZATS)

*
*    The following variables hold the passed value after conversion to
*    the data type of the attribute.  In a given invocation only the
*    variable corresponding to the data type of the attribute is used.

      BYTE             VALUB ! Unsigned byte value.
      BYTE             VALB  ! Byte value.
      INTEGER*2        VALUW ! Unsigned word value.
      INTEGER*2        VALW  ! Word value.
      INTEGER          VALI  ! Integer value.
      REAL             VALR  ! Real value.
      DOUBLE PRECISION VALD  ! Double precision value.
      LOGICAL          VALL  ! Logical value.
      CHARACTER        VALC*(CAT1__SZATS)   ! Character value.
*.

      IF (STATUS .EQ. CAT__OK) THEN

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
*             Check whether the attribute is mutable.

               IF (ATTMU__CAT1(ATELM) ) THEN

*
*                Determine the type of the attribute and perform any
*                necessary type conversion.

                  ATYPE = ATTYP__CAT1(ATELM)

                  LVALW = VALUE

                  CALL CAT1_TCNVT (CAT__TYPEW, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALR, LVALD, LVALL, LVALC,
     :              ATYPE, VALUB, VALB, VALUW, VALW, VALI, VALR, VALD,
     :              VALL, VALC, CONVOK, STATUS)

                  IF (STATUS .EQ. CAT__OK) THEN
                     IF (ATYPE .EQ. CAT__TYPEUB) THEN
                        ATTVV__CAT1(ATELM) = VALUB

                     ELSE IF (ATYPE .EQ. CAT__TYPEB) THEN
                        ATTVB__CAT1(ATELM) = VALB

                     ELSE IF (ATYPE .EQ. CAT__TYPEUW) THEN
                        ATTVU__CAT1(ATELM) = VALUW

                     ELSE IF (ATYPE .EQ. CAT__TYPEW) THEN
                        ATTVW__CAT1(ATELM) = VALW

                     ELSE IF (ATYPE .EQ. CAT__TYPEI) THEN
                        ATTVI__CAT1(ATELM) = VALI

                     ELSE IF (ATYPE .EQ. CAT__TYPER) THEN
                        ATTVR__CAT1(ATELM) = VALR

                     ELSE IF (ATYPE .EQ. CAT__TYPED) THEN
                        ATTVD__CAT1(ATELM) = VALD

                     ELSE IF (ATYPE .EQ. CAT__TYPEL) THEN
                        ATTVL__CAT1(ATELM) = VALL

                     ELSE IF (ATYPE .EQ. CAT__TYPEC) THEN
                        ATTVC__CAT1(ATELM) = VALC

                     END IF
                  ELSE
                     STATUS = CAT__TYPCV

                  END IF

*
*                Check whether the mutability flag needs to be reset, and
*                if so then reset it (immutable attributes may initially be
*                set once, but not thereafter).

                  CALL CAT_TIDTP (GI, GITYPE, STATUS)
                  CALL CAT1_CKMUT (GITYPE, ATTRIB, MUTBLE, STATUS)

                  IF (.NOT. MUTBLE) THEN
                     ATTMU__CAT1(ATELM) = .FALSE.
                  END IF

               ELSE
                  STATUS = CAT__IMATT

               END IF
            ELSE
               STATUS = CAT__NOATT

            END IF
         ELSE
            STATUS = CAT__INVID

         END IF

*
*          Report any error.

            IF (STATUS .NE. CAT__OK) THEN
               CALL CAT1_ERREP ('CAT_TATTW_ERR', 'CAT_TATTW: '/
     :           /'error setting a component attribute.', STATUS)
            END IF

      END IF

      END
