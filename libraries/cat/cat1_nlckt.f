      SUBROUTINE CAT1_NLCKB (FI, INVAL, NULFLG, OUTVAL, STATUS)
*+
*  Name:
*     CAT1_NLCKB
*  Purpose:
*     Check whether a field value is null or not.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_NLCKB (FI, INVAL; NULFLG, OUTVAL; STATUS)
*  Description:
*     Check whether a field value is null or not.  The check is made by
*     comparing the value for the field against the null value (if any)
*     for the column.  If they are equal then then the field value is
*     null.
*
*     A logical flag is returned indicating whether the value is null
*     or not.
*
*     The behaviour of this routine conforms to the prescription for
*     handling null values given in document StarBase/ACD/3.4.  See
*     this document for the rationale behind this behaviour.  Also
*     see the document for an explanation of the CAT technical
*     terms used in the comments for this routine, for example
*     'exception value' and 'locum'.
*  Arguments:
*     FI  =  INTEGER (Given)
*        Column identifier for the column from which the field value
*        was extracted.
*     INVAL  =  BYTE (Given)
*        Field value which is to be checked to determine whether it is
*        null or not.
*     NULFLG  =  LOGICAL (Returned)
*        Flag indicating whether or not the field value is null.  It is
*        coded as follows:
*        .TRUE.  -  the value is null.
*        .FALSE. -  the value is not null; a genuine datum is available.
*     OUTVAL  =  BYTE (Returned)
*        The output value.  If INVAL is a genuine datum then OUTVAL is a
*        copy of it.  If INVAL is null then OUTVAL contains the
*        StarBase null value of the appropriate data type.  Note that
*        in this case OUTVAL *always* contains the appropriate StarBase
*        null value, *not* any null value which may have been specified
*        for the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the type of exception value supprted by the column.
*     If the column supports default null values then
*       If the null value is equal to the actual field value then
*         Set the null flag to true.
*         Set the return value to the appropriate StarBase null.
*       else (not null)
*         Set the null flag to false.
*         Set the return value to the given field value.
*       end if
*     else if the column supports explicit null values then
*       Obtain the null value for the column.
*       If the explicit null value is equal to the actual field value
*       then
*         Set the null flag to true.
*         Set the return value to the appropriate StarBase null.
*       else (not null)
*         Set the null flag to false.
*         Set the return value to the given field value.
*       end if
*     else if the column supports locum values (and hence the value
*     cannot be null)
*       Set the null flag to false.
*       Set the return value to the given field value.
*     else (invalid internal code for the type of exception value)
*       Set the null flag to true.
*       Set the return value to the appropriate StarBase null.
*       Set the status.
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
*     27/1/94  (ACD): Original version.
*     13/12/96 (ACD): Removed non-standard use of the relational
*       operators in the LOGICAL instantiation of the routine (which
*       had been revealed by the port to Linux).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Arguments Given:
      INTEGER
     :  FI
      BYTE
     :  INVAL
*  Arguments Returned:
      LOGICAL
     :  NULFLG
      BYTE
     :  OUTVAL
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  NULTYP,    ! Type of null: default, explicit or locum.
     :  ERRLEN     ! Length of ERRMSG (excl. trail. blanks).
      BYTE
     :  SNULL      ! Exception value for the column.
      CHARACTER
     :  ERRMSG*75, ! Error message.
     :  EXCEPT*(CAT__SZVAL) ! Exception value for column, coded as char.
      LOGICAL
     :  EQUAL      ! Flag; are value and null value equal?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the type of exception value supported by the column.

         CALL CAT_TIQAI (FI, 'NULL', NULTYP, STATUS)

*
*       Check for the various types of exception values.
*
*       First, standard, Starbase null values.

         IF (NULTYP .EQ. CAT__NULLD) THEN

*
*          Check whether the field value is null and set the return
*          values accordingly.

            CALL CAT1_EQCKB (INVAL, CAT1__DNULB, EQUAL, STATUS)
            IF (EQUAL) THEN
               NULFLG = .TRUE.
               OUTVAL = CAT1__DNULB

            ELSE
               NULFLG = .FALSE.
               OUTVAL = INVAL

            END IF

*
*       Second, a null value has been explictly specified for the
*       column.

         ELSE IF (NULTYP .EQ. CAT__NULLS) THEN

*
*          Obtain the null value for the column.

            CALL CAT_TIQAC (FI, 'EXCEPT', EXCEPT, STATUS)
            CALL CAT1_DCNLB (EXCEPT, SNULL, STATUS)

*
*          Check whether the field value is null and set the return
*          values accordingly.

            CALL CAT1_EQCKB (INVAL, SNULL, EQUAL, STATUS)
            IF (EQUAL) THEN
               NULFLG = .TRUE.
               OUTVAL = CAT1__DNULB

            ELSE
               NULFLG = .FALSE.
               OUTVAL = INVAL

            END IF

*
*       Third, a locum value has been specified for the column; it
*       cannot be checked for null values and all values are assumed
*       genuine.

         ELSE IF (NULTYP .EQ. CAT__LOCUM) THEN
            NULFLG = .FALSE.
            OUTVAL = INVAL

*
*       NULTYP has adopted an illegal value.  Set the return arguments,
*       Set the status and report an error.

         ELSE
            NULFLG = .TRUE.
            OUTVAL = CAT1__DNULB

            STATUS = CAT__INVNL

            ERRLEN = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Invalid code for type of null: ',
     :        ERRMSG, ERRLEN)
            CALL CHR_PUTI (NULTYP, ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT1_NLCKB_INL', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT1_NLCKC (FI, INVAL, NULFLG, OUTVAL, STATUS)
*+
*  Name:
*     CAT1_NLCKC
*  Purpose:
*     Check whether a field value is null or not.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_NLCKC (FI, INVAL; NULFLG, OUTVAL; STATUS)
*  Description:
*     Check whether a field value is null or not.  The check is made by
*     comparing the value for the field against the null value (if any)
*     for the column.  If they are equal then then the field value is
*     null.
*
*     A logical flag is returned indicating whether the value is null
*     or not.
*
*     The behaviour of this routine conforms to the prescription for
*     handling null values given in document StarBase/ACD/3.4.  See
*     this document for the rationale behind this behaviour.  Also
*     see the document for an explanation of the CAT technical
*     terms used in the comments for this routine, for example
*     'exception value' and 'locum'.
*  Arguments:
*     FI  =  INTEGER (Given)
*        Column identifier for the column from which the field value
*        was extracted.
*     INVAL  =  CHARACTER*(*) (Given)
*        Field value which is to be checked to determine whether it is
*        null or not.
*     NULFLG  =  LOGICAL (Returned)
*        Flag indicating whether or not the field value is null.  It is
*        coded as follows:
*        .TRUE.  -  the value is null.
*        .FALSE. -  the value is not null; a genuine datum is available.
*     OUTVAL  =  CHARACTER*(*) (Returned)
*        The output value.  If INVAL is a genuine datum then OUTVAL is a
*        copy of it.  If INVAL is null then OUTVAL contains the
*        StarBase null value of the appropriate data type.  Note that
*        in this case OUTVAL *always* contains the appropriate StarBase
*        null value, *not* any null value which may have been specified
*        for the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the type of exception value supprted by the column.
*     If the column supports default null values then
*       If the null value is equal to the actual field value then
*         Set the null flag to true.
*         Set the return value to the appropriate StarBase null.
*       else (not null)
*         Set the null flag to false.
*         Set the return value to the given field value.
*       end if
*     else if the column supports explicit null values then
*       Obtain the null value for the column.
*       If the explicit null value is equal to the actual field value
*       then
*         Set the null flag to true.
*         Set the return value to the appropriate StarBase null.
*       else (not null)
*         Set the null flag to false.
*         Set the return value to the given field value.
*       end if
*     else if the column supports locum values (and hence the value
*     cannot be null)
*       Set the null flag to false.
*       Set the return value to the given field value.
*     else (invalid internal code for the type of exception value)
*       Set the null flag to true.
*       Set the return value to the appropriate StarBase null.
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/1/94  (ACD): Original version.
*     13/12/96 (ACD): Removed non-standard use of the relational
*       operators in the LOGICAL instantiation of the routine (which
*       had been revealed by the port to Linux).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Arguments Given:
      INTEGER
     :  FI
      CHARACTER*(*)
     :  INVAL
*  Arguments Returned:
      LOGICAL
     :  NULFLG
      CHARACTER*(*)
     :  OUTVAL
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  NULTYP,    ! Type of null: default, explicit or locum.
     :  ERRLEN     ! Length of ERRMSG (excl. trail. blanks).
      CHARACTER*(CAT__SZVAL)
     :  SNULL      ! Exception value for the column.
      CHARACTER
     :  ERRMSG*75, ! Error message.
     :  EXCEPT*(CAT__SZVAL) ! Exception value for column, coded as char.
      LOGICAL
     :  EQUAL      ! Flag; are value and null value equal?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the type of exception value supported by the column.

         CALL CAT_TIQAI (FI, 'NULL', NULTYP, STATUS)

*
*       Check for the various types of exception values.
*
*       First, standard, Starbase null values.

         IF (NULTYP .EQ. CAT__NULLD) THEN

*
*          Check whether the field value is null and set the return
*          values accordingly.

            CALL CAT1_EQCKC (INVAL, CAT1__DNULC, EQUAL, STATUS)
            IF (EQUAL) THEN
               NULFLG = .TRUE.
               OUTVAL = CAT1__DNULC

            ELSE
               NULFLG = .FALSE.
               OUTVAL = INVAL

            END IF

*
*       Second, a null value has been explictly specified for the
*       column.

         ELSE IF (NULTYP .EQ. CAT__NULLS) THEN

*
*          Obtain the null value for the column.

            CALL CAT_TIQAC (FI, 'EXCEPT', EXCEPT, STATUS)
            CALL CAT1_DCNLC (EXCEPT, SNULL, STATUS)

*
*          Check whether the field value is null and set the return
*          values accordingly.

            CALL CAT1_EQCKC (INVAL, SNULL, EQUAL, STATUS)
            IF (EQUAL) THEN
               NULFLG = .TRUE.
               OUTVAL = CAT1__DNULC

            ELSE
               NULFLG = .FALSE.
               OUTVAL = INVAL

            END IF

*
*       Third, a locum value has been specified for the column; it
*       cannot be checked for null values and all values are assumed
*       genuine.

         ELSE IF (NULTYP .EQ. CAT__LOCUM) THEN
            NULFLG = .FALSE.
            OUTVAL = INVAL

*
*       NULTYP has adopted an illegal value.  Set the return arguments,
*       Set the status and report an error.

         ELSE
            NULFLG = .TRUE.
            OUTVAL = CAT1__DNULC

            STATUS = CAT__INVNL

            ERRLEN = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Invalid code for type of null: ',
     :        ERRMSG, ERRLEN)
            CALL CHR_PUTI (NULTYP, ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT1_NLCKC_INL', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT1_NLCKD (FI, INVAL, NULFLG, OUTVAL, STATUS)
*+
*  Name:
*     CAT1_NLCKD
*  Purpose:
*     Check whether a field value is null or not.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_NLCKD (FI, INVAL; NULFLG, OUTVAL; STATUS)
*  Description:
*     Check whether a field value is null or not.  The check is made by
*     comparing the value for the field against the null value (if any)
*     for the column.  If they are equal then then the field value is
*     null.
*
*     A logical flag is returned indicating whether the value is null
*     or not.
*
*     The behaviour of this routine conforms to the prescription for
*     handling null values given in document StarBase/ACD/3.4.  See
*     this document for the rationale behind this behaviour.  Also
*     see the document for an explanation of the CAT technical
*     terms used in the comments for this routine, for example
*     'exception value' and 'locum'.
*  Arguments:
*     FI  =  INTEGER (Given)
*        Column identifier for the column from which the field value
*        was extracted.
*     INVAL  =  DOUBLE PRECISION (Given)
*        Field value which is to be checked to determine whether it is
*        null or not.
*     NULFLG  =  LOGICAL (Returned)
*        Flag indicating whether or not the field value is null.  It is
*        coded as follows:
*        .TRUE.  -  the value is null.
*        .FALSE. -  the value is not null; a genuine datum is available.
*     OUTVAL  =  DOUBLE PRECISION (Returned)
*        The output value.  If INVAL is a genuine datum then OUTVAL is a
*        copy of it.  If INVAL is null then OUTVAL contains the
*        StarBase null value of the appropriate data type.  Note that
*        in this case OUTVAL *always* contains the appropriate StarBase
*        null value, *not* any null value which may have been specified
*        for the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the type of exception value supprted by the column.
*     If the column supports default null values then
*       If the null value is equal to the actual field value then
*         Set the null flag to true.
*         Set the return value to the appropriate StarBase null.
*       else (not null)
*         Set the null flag to false.
*         Set the return value to the given field value.
*       end if
*     else if the column supports explicit null values then
*       Obtain the null value for the column.
*       If the explicit null value is equal to the actual field value
*       then
*         Set the null flag to true.
*         Set the return value to the appropriate StarBase null.
*       else (not null)
*         Set the null flag to false.
*         Set the return value to the given field value.
*       end if
*     else if the column supports locum values (and hence the value
*     cannot be null)
*       Set the null flag to false.
*       Set the return value to the given field value.
*     else (invalid internal code for the type of exception value)
*       Set the null flag to true.
*       Set the return value to the appropriate StarBase null.
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/1/94  (ACD): Original version.
*     13/12/96 (ACD): Removed non-standard use of the relational
*       operators in the LOGICAL instantiation of the routine (which
*       had been revealed by the port to Linux).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Arguments Given:
      INTEGER
     :  FI
      DOUBLE PRECISION
     :  INVAL
*  Arguments Returned:
      LOGICAL
     :  NULFLG
      DOUBLE PRECISION
     :  OUTVAL
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  NULTYP,    ! Type of null: default, explicit or locum.
     :  ERRLEN     ! Length of ERRMSG (excl. trail. blanks).
      DOUBLE PRECISION
     :  SNULL      ! Exception value for the column.
      CHARACTER
     :  ERRMSG*75, ! Error message.
     :  EXCEPT*(CAT__SZVAL) ! Exception value for column, coded as char.
      LOGICAL
     :  EQUAL      ! Flag; are value and null value equal?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the type of exception value supported by the column.

         CALL CAT_TIQAI (FI, 'NULL', NULTYP, STATUS)

*
*       Check for the various types of exception values.
*
*       First, standard, Starbase null values.

         IF (NULTYP .EQ. CAT__NULLD) THEN

*
*          Check whether the field value is null and set the return
*          values accordingly.

            CALL CAT1_EQCKD (INVAL, CAT1__DNULD, EQUAL, STATUS)
            IF (EQUAL) THEN
               NULFLG = .TRUE.
               OUTVAL = CAT1__DNULD

            ELSE
               NULFLG = .FALSE.
               OUTVAL = INVAL

            END IF

*
*       Second, a null value has been explictly specified for the
*       column.

         ELSE IF (NULTYP .EQ. CAT__NULLS) THEN

*
*          Obtain the null value for the column.

            CALL CAT_TIQAC (FI, 'EXCEPT', EXCEPT, STATUS)
            CALL CAT1_DCNLD (EXCEPT, SNULL, STATUS)

*
*          Check whether the field value is null and set the return
*          values accordingly.

            CALL CAT1_EQCKD (INVAL, SNULL, EQUAL, STATUS)
            IF (EQUAL) THEN
               NULFLG = .TRUE.
               OUTVAL = CAT1__DNULD

            ELSE
               NULFLG = .FALSE.
               OUTVAL = INVAL

            END IF

*
*       Third, a locum value has been specified for the column; it
*       cannot be checked for null values and all values are assumed
*       genuine.

         ELSE IF (NULTYP .EQ. CAT__LOCUM) THEN
            NULFLG = .FALSE.
            OUTVAL = INVAL

*
*       NULTYP has adopted an illegal value.  Set the return arguments,
*       Set the status and report an error.

         ELSE
            NULFLG = .TRUE.
            OUTVAL = CAT1__DNULD

            STATUS = CAT__INVNL

            ERRLEN = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Invalid code for type of null: ',
     :        ERRMSG, ERRLEN)
            CALL CHR_PUTI (NULTYP, ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT1_NLCKD_INL', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT1_NLCKI (FI, INVAL, NULFLG, OUTVAL, STATUS)
*+
*  Name:
*     CAT1_NLCKI
*  Purpose:
*     Check whether a field value is null or not.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_NLCKI (FI, INVAL; NULFLG, OUTVAL; STATUS)
*  Description:
*     Check whether a field value is null or not.  The check is made by
*     comparing the value for the field against the null value (if any)
*     for the column.  If they are equal then then the field value is
*     null.
*
*     A logical flag is returned indicating whether the value is null
*     or not.
*
*     The behaviour of this routine conforms to the prescription for
*     handling null values given in document StarBase/ACD/3.4.  See
*     this document for the rationale behind this behaviour.  Also
*     see the document for an explanation of the CAT technical
*     terms used in the comments for this routine, for example
*     'exception value' and 'locum'.
*  Arguments:
*     FI  =  INTEGER (Given)
*        Column identifier for the column from which the field value
*        was extracted.
*     INVAL  =  INTEGER (Given)
*        Field value which is to be checked to determine whether it is
*        null or not.
*     NULFLG  =  LOGICAL (Returned)
*        Flag indicating whether or not the field value is null.  It is
*        coded as follows:
*        .TRUE.  -  the value is null.
*        .FALSE. -  the value is not null; a genuine datum is available.
*     OUTVAL  =  INTEGER (Returned)
*        The output value.  If INVAL is a genuine datum then OUTVAL is a
*        copy of it.  If INVAL is null then OUTVAL contains the
*        StarBase null value of the appropriate data type.  Note that
*        in this case OUTVAL *always* contains the appropriate StarBase
*        null value, *not* any null value which may have been specified
*        for the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the type of exception value supprted by the column.
*     If the column supports default null values then
*       If the null value is equal to the actual field value then
*         Set the null flag to true.
*         Set the return value to the appropriate StarBase null.
*       else (not null)
*         Set the null flag to false.
*         Set the return value to the given field value.
*       end if
*     else if the column supports explicit null values then
*       Obtain the null value for the column.
*       If the explicit null value is equal to the actual field value
*       then
*         Set the null flag to true.
*         Set the return value to the appropriate StarBase null.
*       else (not null)
*         Set the null flag to false.
*         Set the return value to the given field value.
*       end if
*     else if the column supports locum values (and hence the value
*     cannot be null)
*       Set the null flag to false.
*       Set the return value to the given field value.
*     else (invalid internal code for the type of exception value)
*       Set the null flag to true.
*       Set the return value to the appropriate StarBase null.
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/1/94  (ACD): Original version.
*     13/12/96 (ACD): Removed non-standard use of the relational
*       operators in the LOGICAL instantiation of the routine (which
*       had been revealed by the port to Linux).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Arguments Given:
      INTEGER
     :  FI
      INTEGER
     :  INVAL
*  Arguments Returned:
      LOGICAL
     :  NULFLG
      INTEGER
     :  OUTVAL
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  NULTYP,    ! Type of null: default, explicit or locum.
     :  ERRLEN     ! Length of ERRMSG (excl. trail. blanks).
      INTEGER
     :  SNULL      ! Exception value for the column.
      CHARACTER
     :  ERRMSG*75, ! Error message.
     :  EXCEPT*(CAT__SZVAL) ! Exception value for column, coded as char.
      LOGICAL
     :  EQUAL      ! Flag; are value and null value equal?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the type of exception value supported by the column.

         CALL CAT_TIQAI (FI, 'NULL', NULTYP, STATUS)

*
*       Check for the various types of exception values.
*
*       First, standard, Starbase null values.

         IF (NULTYP .EQ. CAT__NULLD) THEN

*
*          Check whether the field value is null and set the return
*          values accordingly.

            CALL CAT1_EQCKI (INVAL, CAT1__DNULI, EQUAL, STATUS)
            IF (EQUAL) THEN
               NULFLG = .TRUE.
               OUTVAL = CAT1__DNULI

            ELSE
               NULFLG = .FALSE.
               OUTVAL = INVAL

            END IF

*
*       Second, a null value has been explictly specified for the
*       column.

         ELSE IF (NULTYP .EQ. CAT__NULLS) THEN

*
*          Obtain the null value for the column.

            CALL CAT_TIQAC (FI, 'EXCEPT', EXCEPT, STATUS)
            CALL CAT1_DCNLI (EXCEPT, SNULL, STATUS)

*
*          Check whether the field value is null and set the return
*          values accordingly.

            CALL CAT1_EQCKI (INVAL, SNULL, EQUAL, STATUS)
            IF (EQUAL) THEN
               NULFLG = .TRUE.
               OUTVAL = CAT1__DNULI

            ELSE
               NULFLG = .FALSE.
               OUTVAL = INVAL

            END IF

*
*       Third, a locum value has been specified for the column; it
*       cannot be checked for null values and all values are assumed
*       genuine.

         ELSE IF (NULTYP .EQ. CAT__LOCUM) THEN
            NULFLG = .FALSE.
            OUTVAL = INVAL

*
*       NULTYP has adopted an illegal value.  Set the return arguments,
*       Set the status and report an error.

         ELSE
            NULFLG = .TRUE.
            OUTVAL = CAT1__DNULI

            STATUS = CAT__INVNL

            ERRLEN = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Invalid code for type of null: ',
     :        ERRMSG, ERRLEN)
            CALL CHR_PUTI (NULTYP, ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT1_NLCKI_INL', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT1_NLCKL (FI, INVAL, NULFLG, OUTVAL, STATUS)
*+
*  Name:
*     CAT1_NLCKL
*  Purpose:
*     Check whether a field value is null or not.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_NLCKL (FI, INVAL; NULFLG, OUTVAL; STATUS)
*  Description:
*     Check whether a field value is null or not.  The check is made by
*     comparing the value for the field against the null value (if any)
*     for the column.  If they are equal then then the field value is
*     null.
*
*     A logical flag is returned indicating whether the value is null
*     or not.
*
*     The behaviour of this routine conforms to the prescription for
*     handling null values given in document StarBase/ACD/3.4.  See
*     this document for the rationale behind this behaviour.  Also
*     see the document for an explanation of the CAT technical
*     terms used in the comments for this routine, for example
*     'exception value' and 'locum'.
*  Arguments:
*     FI  =  INTEGER (Given)
*        Column identifier for the column from which the field value
*        was extracted.
*     INVAL  =  LOGICAL (Given)
*        Field value which is to be checked to determine whether it is
*        null or not.
*     NULFLG  =  LOGICAL (Returned)
*        Flag indicating whether or not the field value is null.  It is
*        coded as follows:
*        .TRUE.  -  the value is null.
*        .FALSE. -  the value is not null; a genuine datum is available.
*     OUTVAL  =  LOGICAL (Returned)
*        The output value.  If INVAL is a genuine datum then OUTVAL is a
*        copy of it.  If INVAL is null then OUTVAL contains the
*        StarBase null value of the appropriate data type.  Note that
*        in this case OUTVAL *always* contains the appropriate StarBase
*        null value, *not* any null value which may have been specified
*        for the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the type of exception value supprted by the column.
*     If the column supports default null values then
*       If the null value is equal to the actual field value then
*         Set the null flag to true.
*         Set the return value to the appropriate StarBase null.
*       else (not null)
*         Set the null flag to false.
*         Set the return value to the given field value.
*       end if
*     else if the column supports explicit null values then
*       Obtain the null value for the column.
*       If the explicit null value is equal to the actual field value
*       then
*         Set the null flag to true.
*         Set the return value to the appropriate StarBase null.
*       else (not null)
*         Set the null flag to false.
*         Set the return value to the given field value.
*       end if
*     else if the column supports locum values (and hence the value
*     cannot be null)
*       Set the null flag to false.
*       Set the return value to the given field value.
*     else (invalid internal code for the type of exception value)
*       Set the null flag to true.
*       Set the return value to the appropriate StarBase null.
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/1/94  (ACD): Original version.
*     13/12/96 (ACD): Removed non-standard use of the relational
*       operators in the LOGICAL instantiation of the routine (which
*       had been revealed by the port to Linux).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Arguments Given:
      INTEGER
     :  FI
      LOGICAL
     :  INVAL
*  Arguments Returned:
      LOGICAL
     :  NULFLG
      LOGICAL
     :  OUTVAL
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  NULTYP,    ! Type of null: default, explicit or locum.
     :  ERRLEN     ! Length of ERRMSG (excl. trail. blanks).
      LOGICAL
     :  SNULL      ! Exception value for the column.
      CHARACTER
     :  ERRMSG*75, ! Error message.
     :  EXCEPT*(CAT__SZVAL) ! Exception value for column, coded as char.
      LOGICAL
     :  EQUAL      ! Flag; are value and null value equal?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the type of exception value supported by the column.

         CALL CAT_TIQAI (FI, 'NULL', NULTYP, STATUS)

*
*       Check for the various types of exception values.
*
*       First, standard, Starbase null values.

         IF (NULTYP .EQ. CAT__NULLD) THEN

*
*          Check whether the field value is null and set the return
*          values accordingly.

            CALL CAT1_EQCKL (INVAL, CAT1__DNULL, EQUAL, STATUS)
            IF (EQUAL) THEN
               NULFLG = .TRUE.
               OUTVAL = CAT1__DNULL

            ELSE
               NULFLG = .FALSE.
               OUTVAL = INVAL

            END IF

*
*       Second, a null value has been explictly specified for the
*       column.

         ELSE IF (NULTYP .EQ. CAT__NULLS) THEN

*
*          Obtain the null value for the column.

            CALL CAT_TIQAC (FI, 'EXCEPT', EXCEPT, STATUS)
            CALL CAT1_DCNLL (EXCEPT, SNULL, STATUS)

*
*          Check whether the field value is null and set the return
*          values accordingly.

            CALL CAT1_EQCKL (INVAL, SNULL, EQUAL, STATUS)
            IF (EQUAL) THEN
               NULFLG = .TRUE.
               OUTVAL = CAT1__DNULL

            ELSE
               NULFLG = .FALSE.
               OUTVAL = INVAL

            END IF

*
*       Third, a locum value has been specified for the column; it
*       cannot be checked for null values and all values are assumed
*       genuine.

         ELSE IF (NULTYP .EQ. CAT__LOCUM) THEN
            NULFLG = .FALSE.
            OUTVAL = INVAL

*
*       NULTYP has adopted an illegal value.  Set the return arguments,
*       Set the status and report an error.

         ELSE
            NULFLG = .TRUE.
            OUTVAL = CAT1__DNULL

            STATUS = CAT__INVNL

            ERRLEN = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Invalid code for type of null: ',
     :        ERRMSG, ERRLEN)
            CALL CHR_PUTI (NULTYP, ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT1_NLCKL_INL', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT1_NLCKR (FI, INVAL, NULFLG, OUTVAL, STATUS)
*+
*  Name:
*     CAT1_NLCKR
*  Purpose:
*     Check whether a field value is null or not.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_NLCKR (FI, INVAL; NULFLG, OUTVAL; STATUS)
*  Description:
*     Check whether a field value is null or not.  The check is made by
*     comparing the value for the field against the null value (if any)
*     for the column.  If they are equal then then the field value is
*     null.
*
*     A logical flag is returned indicating whether the value is null
*     or not.
*
*     The behaviour of this routine conforms to the prescription for
*     handling null values given in document StarBase/ACD/3.4.  See
*     this document for the rationale behind this behaviour.  Also
*     see the document for an explanation of the CAT technical
*     terms used in the comments for this routine, for example
*     'exception value' and 'locum'.
*  Arguments:
*     FI  =  INTEGER (Given)
*        Column identifier for the column from which the field value
*        was extracted.
*     INVAL  =  REAL (Given)
*        Field value which is to be checked to determine whether it is
*        null or not.
*     NULFLG  =  LOGICAL (Returned)
*        Flag indicating whether or not the field value is null.  It is
*        coded as follows:
*        .TRUE.  -  the value is null.
*        .FALSE. -  the value is not null; a genuine datum is available.
*     OUTVAL  =  REAL (Returned)
*        The output value.  If INVAL is a genuine datum then OUTVAL is a
*        copy of it.  If INVAL is null then OUTVAL contains the
*        StarBase null value of the appropriate data type.  Note that
*        in this case OUTVAL *always* contains the appropriate StarBase
*        null value, *not* any null value which may have been specified
*        for the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the type of exception value supprted by the column.
*     If the column supports default null values then
*       If the null value is equal to the actual field value then
*         Set the null flag to true.
*         Set the return value to the appropriate StarBase null.
*       else (not null)
*         Set the null flag to false.
*         Set the return value to the given field value.
*       end if
*     else if the column supports explicit null values then
*       Obtain the null value for the column.
*       If the explicit null value is equal to the actual field value
*       then
*         Set the null flag to true.
*         Set the return value to the appropriate StarBase null.
*       else (not null)
*         Set the null flag to false.
*         Set the return value to the given field value.
*       end if
*     else if the column supports locum values (and hence the value
*     cannot be null)
*       Set the null flag to false.
*       Set the return value to the given field value.
*     else (invalid internal code for the type of exception value)
*       Set the null flag to true.
*       Set the return value to the appropriate StarBase null.
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/1/94  (ACD): Original version.
*     13/12/96 (ACD): Removed non-standard use of the relational
*       operators in the LOGICAL instantiation of the routine (which
*       had been revealed by the port to Linux).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Arguments Given:
      INTEGER
     :  FI
      REAL
     :  INVAL
*  Arguments Returned:
      LOGICAL
     :  NULFLG
      REAL
     :  OUTVAL
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  NULTYP,    ! Type of null: default, explicit or locum.
     :  ERRLEN     ! Length of ERRMSG (excl. trail. blanks).
      REAL
     :  SNULL      ! Exception value for the column.
      CHARACTER
     :  ERRMSG*75, ! Error message.
     :  EXCEPT*(CAT__SZVAL) ! Exception value for column, coded as char.
      LOGICAL
     :  EQUAL      ! Flag; are value and null value equal?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the type of exception value supported by the column.

         CALL CAT_TIQAI (FI, 'NULL', NULTYP, STATUS)

*
*       Check for the various types of exception values.
*
*       First, standard, Starbase null values.

         IF (NULTYP .EQ. CAT__NULLD) THEN

*
*          Check whether the field value is null and set the return
*          values accordingly.

            CALL CAT1_EQCKR (INVAL, CAT1__DNULR, EQUAL, STATUS)
            IF (EQUAL) THEN
               NULFLG = .TRUE.
               OUTVAL = CAT1__DNULR

            ELSE
               NULFLG = .FALSE.
               OUTVAL = INVAL

            END IF

*
*       Second, a null value has been explictly specified for the
*       column.

         ELSE IF (NULTYP .EQ. CAT__NULLS) THEN

*
*          Obtain the null value for the column.

            CALL CAT_TIQAC (FI, 'EXCEPT', EXCEPT, STATUS)
            CALL CAT1_DCNLR (EXCEPT, SNULL, STATUS)

*
*          Check whether the field value is null and set the return
*          values accordingly.

            CALL CAT1_EQCKR (INVAL, SNULL, EQUAL, STATUS)
            IF (EQUAL) THEN
               NULFLG = .TRUE.
               OUTVAL = CAT1__DNULR

            ELSE
               NULFLG = .FALSE.
               OUTVAL = INVAL

            END IF

*
*       Third, a locum value has been specified for the column; it
*       cannot be checked for null values and all values are assumed
*       genuine.

         ELSE IF (NULTYP .EQ. CAT__LOCUM) THEN
            NULFLG = .FALSE.
            OUTVAL = INVAL

*
*       NULTYP has adopted an illegal value.  Set the return arguments,
*       Set the status and report an error.

         ELSE
            NULFLG = .TRUE.
            OUTVAL = CAT1__DNULR

            STATUS = CAT__INVNL

            ERRLEN = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Invalid code for type of null: ',
     :        ERRMSG, ERRLEN)
            CALL CHR_PUTI (NULTYP, ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT1_NLCKR_INL', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT1_NLCKW (FI, INVAL, NULFLG, OUTVAL, STATUS)
*+
*  Name:
*     CAT1_NLCKW
*  Purpose:
*     Check whether a field value is null or not.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_NLCKW (FI, INVAL; NULFLG, OUTVAL; STATUS)
*  Description:
*     Check whether a field value is null or not.  The check is made by
*     comparing the value for the field against the null value (if any)
*     for the column.  If they are equal then then the field value is
*     null.
*
*     A logical flag is returned indicating whether the value is null
*     or not.
*
*     The behaviour of this routine conforms to the prescription for
*     handling null values given in document StarBase/ACD/3.4.  See
*     this document for the rationale behind this behaviour.  Also
*     see the document for an explanation of the CAT technical
*     terms used in the comments for this routine, for example
*     'exception value' and 'locum'.
*  Arguments:
*     FI  =  INTEGER (Given)
*        Column identifier for the column from which the field value
*        was extracted.
*     INVAL  =  INTEGER*2 (Given)
*        Field value which is to be checked to determine whether it is
*        null or not.
*     NULFLG  =  LOGICAL (Returned)
*        Flag indicating whether or not the field value is null.  It is
*        coded as follows:
*        .TRUE.  -  the value is null.
*        .FALSE. -  the value is not null; a genuine datum is available.
*     OUTVAL  =  INTEGER*2 (Returned)
*        The output value.  If INVAL is a genuine datum then OUTVAL is a
*        copy of it.  If INVAL is null then OUTVAL contains the
*        StarBase null value of the appropriate data type.  Note that
*        in this case OUTVAL *always* contains the appropriate StarBase
*        null value, *not* any null value which may have been specified
*        for the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the type of exception value supprted by the column.
*     If the column supports default null values then
*       If the null value is equal to the actual field value then
*         Set the null flag to true.
*         Set the return value to the appropriate StarBase null.
*       else (not null)
*         Set the null flag to false.
*         Set the return value to the given field value.
*       end if
*     else if the column supports explicit null values then
*       Obtain the null value for the column.
*       If the explicit null value is equal to the actual field value
*       then
*         Set the null flag to true.
*         Set the return value to the appropriate StarBase null.
*       else (not null)
*         Set the null flag to false.
*         Set the return value to the given field value.
*       end if
*     else if the column supports locum values (and hence the value
*     cannot be null)
*       Set the null flag to false.
*       Set the return value to the given field value.
*     else (invalid internal code for the type of exception value)
*       Set the null flag to true.
*       Set the return value to the appropriate StarBase null.
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/1/94  (ACD): Original version.
*     13/12/96 (ACD): Removed non-standard use of the relational
*       operators in the LOGICAL instantiation of the routine (which
*       had been revealed by the port to Linux).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Arguments Given:
      INTEGER
     :  FI
      INTEGER*2
     :  INVAL
*  Arguments Returned:
      LOGICAL
     :  NULFLG
      INTEGER*2
     :  OUTVAL
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  NULTYP,    ! Type of null: default, explicit or locum.
     :  ERRLEN     ! Length of ERRMSG (excl. trail. blanks).
      INTEGER*2
     :  SNULL      ! Exception value for the column.
      CHARACTER
     :  ERRMSG*75, ! Error message.
     :  EXCEPT*(CAT__SZVAL) ! Exception value for column, coded as char.
      LOGICAL
     :  EQUAL      ! Flag; are value and null value equal?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the type of exception value supported by the column.

         CALL CAT_TIQAI (FI, 'NULL', NULTYP, STATUS)

*
*       Check for the various types of exception values.
*
*       First, standard, Starbase null values.

         IF (NULTYP .EQ. CAT__NULLD) THEN

*
*          Check whether the field value is null and set the return
*          values accordingly.

            CALL CAT1_EQCKW (INVAL, CAT1__DNULW, EQUAL, STATUS)
            IF (EQUAL) THEN
               NULFLG = .TRUE.
               OUTVAL = CAT1__DNULW

            ELSE
               NULFLG = .FALSE.
               OUTVAL = INVAL

            END IF

*
*       Second, a null value has been explictly specified for the
*       column.

         ELSE IF (NULTYP .EQ. CAT__NULLS) THEN

*
*          Obtain the null value for the column.

            CALL CAT_TIQAC (FI, 'EXCEPT', EXCEPT, STATUS)
            CALL CAT1_DCNLW (EXCEPT, SNULL, STATUS)

*
*          Check whether the field value is null and set the return
*          values accordingly.

            CALL CAT1_EQCKW (INVAL, SNULL, EQUAL, STATUS)
            IF (EQUAL) THEN
               NULFLG = .TRUE.
               OUTVAL = CAT1__DNULW

            ELSE
               NULFLG = .FALSE.
               OUTVAL = INVAL

            END IF

*
*       Third, a locum value has been specified for the column; it
*       cannot be checked for null values and all values are assumed
*       genuine.

         ELSE IF (NULTYP .EQ. CAT__LOCUM) THEN
            NULFLG = .FALSE.
            OUTVAL = INVAL

*
*       NULTYP has adopted an illegal value.  Set the return arguments,
*       Set the status and report an error.

         ELSE
            NULFLG = .TRUE.
            OUTVAL = CAT1__DNULW

            STATUS = CAT__INVNL

            ERRLEN = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Invalid code for type of null: ',
     :        ERRMSG, ERRLEN)
            CALL CHR_PUTI (NULTYP, ERRMSG, ERRLEN)

            CALL CAT1_ERREP ('CAT1_NLCKW_INL', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
