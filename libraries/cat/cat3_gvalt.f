      SUBROUTINE CAT3_GVALB (FITYPE, SFLAG, SCALE, ZERO, FITUNT,
     :  COLNO, ELEM, ROWNO, VALUEB, NULFLG, STATUS)
*+
*  Name:
*     CAT3_GVALB
*  Purpose:
*     Get a value of type B from the FITS table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_GVALB (FITYPE, SFLAG, SCALE, ZERO, FITUNT, COLNO,
*       ELEM, ROWNO; VALUEB, NULFLG; STATUS)
*  Description:
*     Get a value of type B from the FITS table.  Type conversions are
*     performed if necessary.
*  Arguments:
*     FITYPE  =  INTEGER (Given)
*        FITS data type of the column.
*     SFLAG  =  LOGICAL (Given)
*        Flag indicating whether the column is scaled or not.
*     SCALE  =  DOUBLE PRECISION (Given)
*        Scale factor for a scaled column.
*     ZERO  =  DOUBLE PRECISION (Given)
*        Zero point for a scaled column.
*     FITUNT  =  INTEGER (Given)
*        Fortran unit number for accessing the FITS table.
*     COLNO  =  INTEGER (Given)
*        Sequence number for the column in the FITS table.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be got.  For scalars it should
*        adopt the value 1.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUEB  =  BYTE (Returned)
*        Value obtained.  The value need not be of the same data type
*        as the field.  Type conversions are performed if necessary.
*     NULFLG  =  LOGICAL (ENTRY or EXIT)
*        Flag indicating whether or not the value is null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each FITS data type:
*       Attempt to get a value.
*     else (the FITS data type is invalid)
*       Report an error message.
*     end if
*     If the null flag has been set then
*       Set the returned value to null.
*     end if
*  Implementation Deficiencies:
*     Unsigned byte and unsigned word data types are not handled.
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
*     6/7/93  (ACD): Original version.
*     8/10/93 (ACD): First working version.
*     24/1/94 (ACD): Modified error reporting.
*     30/1/94 (ACD): Modified for proper treatment of null values.
*     2/2/94  (ACD): Added handling of vector column elements.
*     7/5/94  (ACD): Re-written to handle complex columns and properly
*       handle scaled columns.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
      INCLUDE 'CAT3_FIT_PAR'      ! FITS back-end symbolic constants.
*  Arguments Given:
      INTEGER
     :  FITYPE,
     :  FITUNT,
     :  COLNO,
     :  ELEM,
     :  ROWNO
      LOGICAL
     :  SFLAG
      DOUBLE PRECISION
     :  SCALE,
     :  ZERO
*  Arguments Returned:

*
*    For routine CAT3_GVALB variable VALUEB is a passed argument;
*    all the other VALUEx variables are local.

      BYTE      VALUEUB
      BYTE      VALUEB
      INTEGER*2 VALUEUW
      INTEGER*2 VALUEW
      INTEGER   VALUEI
      REAL      VALUER
      DOUBLE PRECISION VALUED
      LOGICAL   VALUEL
      CHARACTER VALUEC*(CAT__SZVAL)

      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      CHARACTER
     :  ERRMSG*75  ! Text for error message.
      INTEGER
     :  ERRPOS     ! Length of ERRMSG (excl. trail. blanks).
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Get a value of the appropriate CAT data type, using the
*       routine corresponding to the FITS data type of the column.

         IF (FITYPE .EQ. CAT3__FTYPX) THEN
            CALL CAT3_FGCVX (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEB, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPB) THEN
            CALL CAT3_FGCVB (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEB, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPI) THEN
            CALL CAT3_FGCVI (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEB, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN
            CALL CAT3_FGCVJ (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEB, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPE) THEN
            CALL CAT3_FGCVE (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEB, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPD) THEN
            CALL CAT3_FGCVD (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEB, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPL) THEN
            CALL CAT3_FGCVL (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEB, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPA) THEN
            CALL CAT3_FGCVA (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEB, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPC) THEN
            CALL CAT3_FGCVC (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEB, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPM) THEN
            CALL CAT3_FGCVM (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEB, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE

*
*          The FITS data type is invalid; set the status and report an
*          error message.

            STATUS = CAT__INVDT

            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure getting field from FITS table '/
     :        /'(column: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (COLNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (' element: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRPOS)
            CALL CHR_PUTC (' row: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (').', ERRMSG, ERRPOS)

            CALL CAT1_ERREP ('CAT3_GVALB_IDT', ERRMSG(1 : ERRPOS),
     :        STATUS)
         END IF

*
*       If the null flag has been set then set the returned value to
*       the standard Starlink null value.

         IF (NULFLG) THEN
            VALUEB = CAT1__DNULB
         END IF

      END IF

      END
      SUBROUTINE CAT3_GVALC (FITYPE, SFLAG, SCALE, ZERO, FITUNT,
     :  COLNO, ELEM, ROWNO, VALUEC, NULFLG, STATUS)
*+
*  Name:
*     CAT3_GVALC
*  Purpose:
*     Get a value of type C from the FITS table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_GVALC (FITYPE, SFLAG, SCALE, ZERO, FITUNT, COLNO,
*       ELEM, ROWNO; VALUEC, NULFLG; STATUS)
*  Description:
*     Get a value of type C from the FITS table.  Type conversions are
*     performed if necessary.
*  Arguments:
*     FITYPE  =  INTEGER (Given)
*        FITS data type of the column.
*     SFLAG  =  LOGICAL (Given)
*        Flag indicating whether the column is scaled or not.
*     SCALE  =  DOUBLE PRECISION (Given)
*        Scale factor for a scaled column.
*     ZERO  =  DOUBLE PRECISION (Given)
*        Zero point for a scaled column.
*     FITUNT  =  INTEGER (Given)
*        Fortran unit number for accessing the FITS table.
*     COLNO  =  INTEGER (Given)
*        Sequence number for the column in the FITS table.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be got.  For scalars it should
*        adopt the value 1.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUEC  =  CHARACTER*(*) (Returned)
*        Value obtained.  The value need not be of the same data type
*        as the field.  Type conversions are performed if necessary.
*     NULFLG  =  LOGICAL (ENTRY or EXIT)
*        Flag indicating whether or not the value is null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each FITS data type:
*       Attempt to get a value.
*     else (the FITS data type is invalid)
*       Report an error message.
*     end if
*     If the null flag has been set then
*       Set the returned value to null.
*     end if
*  Implementation Deficiencies:
*     Unsigned byte and unsigned word data types are not handled.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     6/7/93  (ACD): Original version.
*     8/10/93 (ACD): First working version.
*     24/1/94 (ACD): Modified error reporting.
*     30/1/94 (ACD): Modified for proper treatment of null values.
*     2/2/94  (ACD): Added handling of vector column elements.
*     7/5/94  (ACD): Re-written to handle complex columns and properly
*       handle scaled columns.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
      INCLUDE 'CAT3_FIT_PAR'      ! FITS back-end symbolic constants.
*  Arguments Given:
      INTEGER
     :  FITYPE,
     :  FITUNT,
     :  COLNO,
     :  ELEM,
     :  ROWNO
      LOGICAL
     :  SFLAG
      DOUBLE PRECISION
     :  SCALE,
     :  ZERO
*  Arguments Returned:

*
*    For routine CAT3_GVALC variable VALUEC is a passed argument;
*    all the other VALUEx variables are local.

      BYTE      VALUEUB
      BYTE      VALUEB
      INTEGER*2 VALUEUW
      INTEGER*2 VALUEW
      INTEGER   VALUEI
      REAL      VALUER
      DOUBLE PRECISION VALUED
      LOGICAL   VALUEL
      CHARACTER VALUEC*(CAT__SZVAL)

      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      CHARACTER
     :  ERRMSG*75  ! Text for error message.
      INTEGER
     :  ERRPOS     ! Length of ERRMSG (excl. trail. blanks).
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Get a value of the appropriate CAT data type, using the
*       routine corresponding to the FITS data type of the column.

         IF (FITYPE .EQ. CAT3__FTYPX) THEN
            CALL CAT3_FGCVX (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEC, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPB) THEN
            CALL CAT3_FGCVB (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEC, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPI) THEN
            CALL CAT3_FGCVI (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEC, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN
            CALL CAT3_FGCVJ (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEC, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPE) THEN
            CALL CAT3_FGCVE (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEC, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPD) THEN
            CALL CAT3_FGCVD (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEC, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPL) THEN
            CALL CAT3_FGCVL (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEC, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPA) THEN
            CALL CAT3_FGCVA (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEC, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPC) THEN
            CALL CAT3_FGCVC (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEC, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPM) THEN
            CALL CAT3_FGCVM (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEC, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE

*
*          The FITS data type is invalid; set the status and report an
*          error message.

            STATUS = CAT__INVDT

            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure getting field from FITS table '/
     :        /'(column: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (COLNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (' element: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRPOS)
            CALL CHR_PUTC (' row: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (').', ERRMSG, ERRPOS)

            CALL CAT1_ERREP ('CAT3_GVALC_IDT', ERRMSG(1 : ERRPOS),
     :        STATUS)
         END IF

*
*       If the null flag has been set then set the returned value to
*       the standard Starlink null value.

         IF (NULFLG) THEN
            VALUEC = CAT1__DNULC
         END IF

      END IF

      END
      SUBROUTINE CAT3_GVALD (FITYPE, SFLAG, SCALE, ZERO, FITUNT,
     :  COLNO, ELEM, ROWNO, VALUED, NULFLG, STATUS)
*+
*  Name:
*     CAT3_GVALD
*  Purpose:
*     Get a value of type D from the FITS table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_GVALD (FITYPE, SFLAG, SCALE, ZERO, FITUNT, COLNO,
*       ELEM, ROWNO; VALUED, NULFLG; STATUS)
*  Description:
*     Get a value of type D from the FITS table.  Type conversions are
*     performed if necessary.
*  Arguments:
*     FITYPE  =  INTEGER (Given)
*        FITS data type of the column.
*     SFLAG  =  LOGICAL (Given)
*        Flag indicating whether the column is scaled or not.
*     SCALE  =  DOUBLE PRECISION (Given)
*        Scale factor for a scaled column.
*     ZERO  =  DOUBLE PRECISION (Given)
*        Zero point for a scaled column.
*     FITUNT  =  INTEGER (Given)
*        Fortran unit number for accessing the FITS table.
*     COLNO  =  INTEGER (Given)
*        Sequence number for the column in the FITS table.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be got.  For scalars it should
*        adopt the value 1.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUED  =  DOUBLE PRECISION (Returned)
*        Value obtained.  The value need not be of the same data type
*        as the field.  Type conversions are performed if necessary.
*     NULFLG  =  LOGICAL (ENTRY or EXIT)
*        Flag indicating whether or not the value is null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each FITS data type:
*       Attempt to get a value.
*     else (the FITS data type is invalid)
*       Report an error message.
*     end if
*     If the null flag has been set then
*       Set the returned value to null.
*     end if
*  Implementation Deficiencies:
*     Unsigned byte and unsigned word data types are not handled.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     6/7/93  (ACD): Original version.
*     8/10/93 (ACD): First working version.
*     24/1/94 (ACD): Modified error reporting.
*     30/1/94 (ACD): Modified for proper treatment of null values.
*     2/2/94  (ACD): Added handling of vector column elements.
*     7/5/94  (ACD): Re-written to handle complex columns and properly
*       handle scaled columns.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
      INCLUDE 'CAT3_FIT_PAR'      ! FITS back-end symbolic constants.
*  Arguments Given:
      INTEGER
     :  FITYPE,
     :  FITUNT,
     :  COLNO,
     :  ELEM,
     :  ROWNO
      LOGICAL
     :  SFLAG
      DOUBLE PRECISION
     :  SCALE,
     :  ZERO
*  Arguments Returned:

*
*    For routine CAT3_GVALD variable VALUED is a passed argument;
*    all the other VALUEx variables are local.

      BYTE      VALUEUB
      BYTE      VALUEB
      INTEGER*2 VALUEUW
      INTEGER*2 VALUEW
      INTEGER   VALUEI
      REAL      VALUER
      DOUBLE PRECISION VALUED
      LOGICAL   VALUEL
      CHARACTER VALUEC*(CAT__SZVAL)

      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      CHARACTER
     :  ERRMSG*75  ! Text for error message.
      INTEGER
     :  ERRPOS     ! Length of ERRMSG (excl. trail. blanks).
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Get a value of the appropriate CAT data type, using the
*       routine corresponding to the FITS data type of the column.

         IF (FITYPE .EQ. CAT3__FTYPX) THEN
            CALL CAT3_FGCVX (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPED, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPB) THEN
            CALL CAT3_FGCVB (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPED, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPI) THEN
            CALL CAT3_FGCVI (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPED, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN
            CALL CAT3_FGCVJ (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPED, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPE) THEN
            CALL CAT3_FGCVE (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPED, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPD) THEN
            CALL CAT3_FGCVD (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPED, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPL) THEN
            CALL CAT3_FGCVL (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPED, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPA) THEN
            CALL CAT3_FGCVA (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPED, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPC) THEN
            CALL CAT3_FGCVC (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPED, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPM) THEN
            CALL CAT3_FGCVM (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPED, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE

*
*          The FITS data type is invalid; set the status and report an
*          error message.

            STATUS = CAT__INVDT

            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure getting field from FITS table '/
     :        /'(column: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (COLNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (' element: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRPOS)
            CALL CHR_PUTC (' row: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (').', ERRMSG, ERRPOS)

            CALL CAT1_ERREP ('CAT3_GVALD_IDT', ERRMSG(1 : ERRPOS),
     :        STATUS)
         END IF

*
*       If the null flag has been set then set the returned value to
*       the standard Starlink null value.

         IF (NULFLG) THEN
            VALUED = CAT1__DNULD
         END IF

      END IF

      END
      SUBROUTINE CAT3_GVALI (FITYPE, SFLAG, SCALE, ZERO, FITUNT,
     :  COLNO, ELEM, ROWNO, VALUEI, NULFLG, STATUS)
*+
*  Name:
*     CAT3_GVALI
*  Purpose:
*     Get a value of type I from the FITS table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_GVALI (FITYPE, SFLAG, SCALE, ZERO, FITUNT, COLNO,
*       ELEM, ROWNO; VALUEI, NULFLG; STATUS)
*  Description:
*     Get a value of type I from the FITS table.  Type conversions are
*     performed if necessary.
*  Arguments:
*     FITYPE  =  INTEGER (Given)
*        FITS data type of the column.
*     SFLAG  =  LOGICAL (Given)
*        Flag indicating whether the column is scaled or not.
*     SCALE  =  DOUBLE PRECISION (Given)
*        Scale factor for a scaled column.
*     ZERO  =  DOUBLE PRECISION (Given)
*        Zero point for a scaled column.
*     FITUNT  =  INTEGER (Given)
*        Fortran unit number for accessing the FITS table.
*     COLNO  =  INTEGER (Given)
*        Sequence number for the column in the FITS table.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be got.  For scalars it should
*        adopt the value 1.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUEI  =  INTEGER (Returned)
*        Value obtained.  The value need not be of the same data type
*        as the field.  Type conversions are performed if necessary.
*     NULFLG  =  LOGICAL (ENTRY or EXIT)
*        Flag indicating whether or not the value is null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each FITS data type:
*       Attempt to get a value.
*     else (the FITS data type is invalid)
*       Report an error message.
*     end if
*     If the null flag has been set then
*       Set the returned value to null.
*     end if
*  Implementation Deficiencies:
*     Unsigned byte and unsigned word data types are not handled.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     6/7/93  (ACD): Original version.
*     8/10/93 (ACD): First working version.
*     24/1/94 (ACD): Modified error reporting.
*     30/1/94 (ACD): Modified for proper treatment of null values.
*     2/2/94  (ACD): Added handling of vector column elements.
*     7/5/94  (ACD): Re-written to handle complex columns and properly
*       handle scaled columns.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
      INCLUDE 'CAT3_FIT_PAR'      ! FITS back-end symbolic constants.
*  Arguments Given:
      INTEGER
     :  FITYPE,
     :  FITUNT,
     :  COLNO,
     :  ELEM,
     :  ROWNO
      LOGICAL
     :  SFLAG
      DOUBLE PRECISION
     :  SCALE,
     :  ZERO
*  Arguments Returned:

*
*    For routine CAT3_GVALI variable VALUEI is a passed argument;
*    all the other VALUEx variables are local.

      BYTE      VALUEUB
      BYTE      VALUEB
      INTEGER*2 VALUEUW
      INTEGER*2 VALUEW
      INTEGER   VALUEI
      REAL      VALUER
      DOUBLE PRECISION VALUED
      LOGICAL   VALUEL
      CHARACTER VALUEC*(CAT__SZVAL)

      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      CHARACTER
     :  ERRMSG*75  ! Text for error message.
      INTEGER
     :  ERRPOS     ! Length of ERRMSG (excl. trail. blanks).
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Get a value of the appropriate CAT data type, using the
*       routine corresponding to the FITS data type of the column.

         IF (FITYPE .EQ. CAT3__FTYPX) THEN
            CALL CAT3_FGCVX (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEI, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPB) THEN
            CALL CAT3_FGCVB (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEI, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPI) THEN
            CALL CAT3_FGCVI (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEI, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN
            CALL CAT3_FGCVJ (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEI, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPE) THEN
            CALL CAT3_FGCVE (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEI, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPD) THEN
            CALL CAT3_FGCVD (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEI, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPL) THEN
            CALL CAT3_FGCVL (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEI, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPA) THEN
            CALL CAT3_FGCVA (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEI, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPC) THEN
            CALL CAT3_FGCVC (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEI, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPM) THEN
            CALL CAT3_FGCVM (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEI, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE

*
*          The FITS data type is invalid; set the status and report an
*          error message.

            STATUS = CAT__INVDT

            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure getting field from FITS table '/
     :        /'(column: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (COLNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (' element: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRPOS)
            CALL CHR_PUTC (' row: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (').', ERRMSG, ERRPOS)

            CALL CAT1_ERREP ('CAT3_GVALI_IDT', ERRMSG(1 : ERRPOS),
     :        STATUS)
         END IF

*
*       If the null flag has been set then set the returned value to
*       the standard Starlink null value.

         IF (NULFLG) THEN
            VALUEI = CAT1__DNULI
         END IF

      END IF

      END
      SUBROUTINE CAT3_GVALL (FITYPE, SFLAG, SCALE, ZERO, FITUNT,
     :  COLNO, ELEM, ROWNO, VALUEL, NULFLG, STATUS)
*+
*  Name:
*     CAT3_GVALL
*  Purpose:
*     Get a value of type L from the FITS table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_GVALL (FITYPE, SFLAG, SCALE, ZERO, FITUNT, COLNO,
*       ELEM, ROWNO; VALUEL, NULFLG; STATUS)
*  Description:
*     Get a value of type L from the FITS table.  Type conversions are
*     performed if necessary.
*  Arguments:
*     FITYPE  =  INTEGER (Given)
*        FITS data type of the column.
*     SFLAG  =  LOGICAL (Given)
*        Flag indicating whether the column is scaled or not.
*     SCALE  =  DOUBLE PRECISION (Given)
*        Scale factor for a scaled column.
*     ZERO  =  DOUBLE PRECISION (Given)
*        Zero point for a scaled column.
*     FITUNT  =  INTEGER (Given)
*        Fortran unit number for accessing the FITS table.
*     COLNO  =  INTEGER (Given)
*        Sequence number for the column in the FITS table.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be got.  For scalars it should
*        adopt the value 1.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUEL  =  LOGICAL (Returned)
*        Value obtained.  The value need not be of the same data type
*        as the field.  Type conversions are performed if necessary.
*     NULFLG  =  LOGICAL (ENTRY or EXIT)
*        Flag indicating whether or not the value is null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each FITS data type:
*       Attempt to get a value.
*     else (the FITS data type is invalid)
*       Report an error message.
*     end if
*     If the null flag has been set then
*       Set the returned value to null.
*     end if
*  Implementation Deficiencies:
*     Unsigned byte and unsigned word data types are not handled.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     6/7/93  (ACD): Original version.
*     8/10/93 (ACD): First working version.
*     24/1/94 (ACD): Modified error reporting.
*     30/1/94 (ACD): Modified for proper treatment of null values.
*     2/2/94  (ACD): Added handling of vector column elements.
*     7/5/94  (ACD): Re-written to handle complex columns and properly
*       handle scaled columns.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
      INCLUDE 'CAT3_FIT_PAR'      ! FITS back-end symbolic constants.
*  Arguments Given:
      INTEGER
     :  FITYPE,
     :  FITUNT,
     :  COLNO,
     :  ELEM,
     :  ROWNO
      LOGICAL
     :  SFLAG
      DOUBLE PRECISION
     :  SCALE,
     :  ZERO
*  Arguments Returned:

*
*    For routine CAT3_GVALL variable VALUEL is a passed argument;
*    all the other VALUEx variables are local.

      BYTE      VALUEUB
      BYTE      VALUEB
      INTEGER*2 VALUEUW
      INTEGER*2 VALUEW
      INTEGER   VALUEI
      REAL      VALUER
      DOUBLE PRECISION VALUED
      LOGICAL   VALUEL
      CHARACTER VALUEC*(CAT__SZVAL)

      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      CHARACTER
     :  ERRMSG*75  ! Text for error message.
      INTEGER
     :  ERRPOS     ! Length of ERRMSG (excl. trail. blanks).
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Get a value of the appropriate CAT data type, using the
*       routine corresponding to the FITS data type of the column.

         IF (FITYPE .EQ. CAT3__FTYPX) THEN
            CALL CAT3_FGCVX (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEL, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPB) THEN
            CALL CAT3_FGCVB (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEL, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPI) THEN
            CALL CAT3_FGCVI (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEL, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN
            CALL CAT3_FGCVJ (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEL, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPE) THEN
            CALL CAT3_FGCVE (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEL, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPD) THEN
            CALL CAT3_FGCVD (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEL, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPL) THEN
            CALL CAT3_FGCVL (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEL, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPA) THEN
            CALL CAT3_FGCVA (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEL, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPC) THEN
            CALL CAT3_FGCVC (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEL, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPM) THEN
            CALL CAT3_FGCVM (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEL, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE

*
*          The FITS data type is invalid; set the status and report an
*          error message.

            STATUS = CAT__INVDT

            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure getting field from FITS table '/
     :        /'(column: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (COLNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (' element: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRPOS)
            CALL CHR_PUTC (' row: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (').', ERRMSG, ERRPOS)

            CALL CAT1_ERREP ('CAT3_GVALL_IDT', ERRMSG(1 : ERRPOS),
     :        STATUS)
         END IF

*
*       If the null flag has been set then set the returned value to
*       the standard Starlink null value.

         IF (NULFLG) THEN
            VALUEL = CAT1__DNULL
         END IF

      END IF

      END
      SUBROUTINE CAT3_GVALR (FITYPE, SFLAG, SCALE, ZERO, FITUNT,
     :  COLNO, ELEM, ROWNO, VALUER, NULFLG, STATUS)
*+
*  Name:
*     CAT3_GVALR
*  Purpose:
*     Get a value of type R from the FITS table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_GVALR (FITYPE, SFLAG, SCALE, ZERO, FITUNT, COLNO,
*       ELEM, ROWNO; VALUER, NULFLG; STATUS)
*  Description:
*     Get a value of type R from the FITS table.  Type conversions are
*     performed if necessary.
*  Arguments:
*     FITYPE  =  INTEGER (Given)
*        FITS data type of the column.
*     SFLAG  =  LOGICAL (Given)
*        Flag indicating whether the column is scaled or not.
*     SCALE  =  DOUBLE PRECISION (Given)
*        Scale factor for a scaled column.
*     ZERO  =  DOUBLE PRECISION (Given)
*        Zero point for a scaled column.
*     FITUNT  =  INTEGER (Given)
*        Fortran unit number for accessing the FITS table.
*     COLNO  =  INTEGER (Given)
*        Sequence number for the column in the FITS table.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be got.  For scalars it should
*        adopt the value 1.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUER  =  REAL (Returned)
*        Value obtained.  The value need not be of the same data type
*        as the field.  Type conversions are performed if necessary.
*     NULFLG  =  LOGICAL (ENTRY or EXIT)
*        Flag indicating whether or not the value is null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each FITS data type:
*       Attempt to get a value.
*     else (the FITS data type is invalid)
*       Report an error message.
*     end if
*     If the null flag has been set then
*       Set the returned value to null.
*     end if
*  Implementation Deficiencies:
*     Unsigned byte and unsigned word data types are not handled.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     6/7/93  (ACD): Original version.
*     8/10/93 (ACD): First working version.
*     24/1/94 (ACD): Modified error reporting.
*     30/1/94 (ACD): Modified for proper treatment of null values.
*     2/2/94  (ACD): Added handling of vector column elements.
*     7/5/94  (ACD): Re-written to handle complex columns and properly
*       handle scaled columns.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
      INCLUDE 'CAT3_FIT_PAR'      ! FITS back-end symbolic constants.
*  Arguments Given:
      INTEGER
     :  FITYPE,
     :  FITUNT,
     :  COLNO,
     :  ELEM,
     :  ROWNO
      LOGICAL
     :  SFLAG
      DOUBLE PRECISION
     :  SCALE,
     :  ZERO
*  Arguments Returned:

*
*    For routine CAT3_GVALR variable VALUER is a passed argument;
*    all the other VALUEx variables are local.

      BYTE      VALUEUB
      BYTE      VALUEB
      INTEGER*2 VALUEUW
      INTEGER*2 VALUEW
      INTEGER   VALUEI
      REAL      VALUER
      DOUBLE PRECISION VALUED
      LOGICAL   VALUEL
      CHARACTER VALUEC*(CAT__SZVAL)

      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      CHARACTER
     :  ERRMSG*75  ! Text for error message.
      INTEGER
     :  ERRPOS     ! Length of ERRMSG (excl. trail. blanks).
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Get a value of the appropriate CAT data type, using the
*       routine corresponding to the FITS data type of the column.

         IF (FITYPE .EQ. CAT3__FTYPX) THEN
            CALL CAT3_FGCVX (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPER, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPB) THEN
            CALL CAT3_FGCVB (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPER, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPI) THEN
            CALL CAT3_FGCVI (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPER, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN
            CALL CAT3_FGCVJ (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPER, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPE) THEN
            CALL CAT3_FGCVE (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPER, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPD) THEN
            CALL CAT3_FGCVD (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPER, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPL) THEN
            CALL CAT3_FGCVL (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPER, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPA) THEN
            CALL CAT3_FGCVA (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPER, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPC) THEN
            CALL CAT3_FGCVC (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPER, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPM) THEN
            CALL CAT3_FGCVM (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPER, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE

*
*          The FITS data type is invalid; set the status and report an
*          error message.

            STATUS = CAT__INVDT

            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure getting field from FITS table '/
     :        /'(column: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (COLNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (' element: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRPOS)
            CALL CHR_PUTC (' row: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (').', ERRMSG, ERRPOS)

            CALL CAT1_ERREP ('CAT3_GVALR_IDT', ERRMSG(1 : ERRPOS),
     :        STATUS)
         END IF

*
*       If the null flag has been set then set the returned value to
*       the standard Starlink null value.

         IF (NULFLG) THEN
            VALUER = CAT1__DNULR
         END IF

      END IF

      END
      SUBROUTINE CAT3_GVALW (FITYPE, SFLAG, SCALE, ZERO, FITUNT,
     :  COLNO, ELEM, ROWNO, VALUEW, NULFLG, STATUS)
*+
*  Name:
*     CAT3_GVALW
*  Purpose:
*     Get a value of type W from the FITS table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_GVALW (FITYPE, SFLAG, SCALE, ZERO, FITUNT, COLNO,
*       ELEM, ROWNO; VALUEW, NULFLG; STATUS)
*  Description:
*     Get a value of type W from the FITS table.  Type conversions are
*     performed if necessary.
*  Arguments:
*     FITYPE  =  INTEGER (Given)
*        FITS data type of the column.
*     SFLAG  =  LOGICAL (Given)
*        Flag indicating whether the column is scaled or not.
*     SCALE  =  DOUBLE PRECISION (Given)
*        Scale factor for a scaled column.
*     ZERO  =  DOUBLE PRECISION (Given)
*        Zero point for a scaled column.
*     FITUNT  =  INTEGER (Given)
*        Fortran unit number for accessing the FITS table.
*     COLNO  =  INTEGER (Given)
*        Sequence number for the column in the FITS table.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be got.  For scalars it should
*        adopt the value 1.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUEW  =  INTEGER*2 (Returned)
*        Value obtained.  The value need not be of the same data type
*        as the field.  Type conversions are performed if necessary.
*     NULFLG  =  LOGICAL (ENTRY or EXIT)
*        Flag indicating whether or not the value is null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each FITS data type:
*       Attempt to get a value.
*     else (the FITS data type is invalid)
*       Report an error message.
*     end if
*     If the null flag has been set then
*       Set the returned value to null.
*     end if
*  Implementation Deficiencies:
*     Unsigned byte and unsigned word data types are not handled.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     6/7/93  (ACD): Original version.
*     8/10/93 (ACD): First working version.
*     24/1/94 (ACD): Modified error reporting.
*     30/1/94 (ACD): Modified for proper treatment of null values.
*     2/2/94  (ACD): Added handling of vector column elements.
*     7/5/94  (ACD): Re-written to handle complex columns and properly
*       handle scaled columns.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
      INCLUDE 'CAT3_FIT_PAR'      ! FITS back-end symbolic constants.
*  Arguments Given:
      INTEGER
     :  FITYPE,
     :  FITUNT,
     :  COLNO,
     :  ELEM,
     :  ROWNO
      LOGICAL
     :  SFLAG
      DOUBLE PRECISION
     :  SCALE,
     :  ZERO
*  Arguments Returned:

*
*    For routine CAT3_GVALW variable VALUEW is a passed argument;
*    all the other VALUEx variables are local.

      BYTE      VALUEUB
      BYTE      VALUEB
      INTEGER*2 VALUEUW
      INTEGER*2 VALUEW
      INTEGER   VALUEI
      REAL      VALUER
      DOUBLE PRECISION VALUED
      LOGICAL   VALUEL
      CHARACTER VALUEC*(CAT__SZVAL)

      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      CHARACTER
     :  ERRMSG*75  ! Text for error message.
      INTEGER
     :  ERRPOS     ! Length of ERRMSG (excl. trail. blanks).
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Get a value of the appropriate CAT data type, using the
*       routine corresponding to the FITS data type of the column.

         IF (FITYPE .EQ. CAT3__FTYPX) THEN
            CALL CAT3_FGCVX (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEW, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPB) THEN
            CALL CAT3_FGCVB (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEW, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPI) THEN
            CALL CAT3_FGCVI (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEW, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN
            CALL CAT3_FGCVJ (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEW, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPE) THEN
            CALL CAT3_FGCVE (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEW, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPD) THEN
            CALL CAT3_FGCVD (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEW, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPL) THEN
            CALL CAT3_FGCVL (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEW, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPA) THEN
            CALL CAT3_FGCVA (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEW, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPC) THEN
            CALL CAT3_FGCVC (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEW, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE IF (FITYPE .EQ. CAT3__FTYPM) THEN
            CALL CAT3_FGCVM (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :        ELEM, CAT__TYPEW, VALUEUB, VALUEB, VALUEUW, VALUEW,
     :        VALUEI, VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)

         ELSE

*
*          The FITS data type is invalid; set the status and report an
*          error message.

            STATUS = CAT__INVDT

            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure getting field from FITS table '/
     :        /'(column: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (COLNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (' element: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRPOS)
            CALL CHR_PUTC (' row: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (').', ERRMSG, ERRPOS)

            CALL CAT1_ERREP ('CAT3_GVALW_IDT', ERRMSG(1 : ERRPOS),
     :        STATUS)
         END IF

*
*       If the null flag has been set then set the returned value to
*       the standard Starlink null value.

         IF (NULFLG) THEN
            VALUEW = CAT1__DNULW
         END IF

      END IF

      END
