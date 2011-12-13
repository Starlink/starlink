      SUBROUTINE CAT3_PVALB (FITYPE, FITUNT, COLNO, ELEM, ROWNO,
     :  VALUEB, NULFLG, STATUS)
*+
*  Name:
*     CAT3_PVALB
*  Purpose:
*     Put a value of type B to the FITS table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_PVALB (FITYPE, FITUNT, COLNO, ELEM, ROWNO,
*        VALUEB, NULFLG; STATUS)
*  Description:
*     Put a value of type B to the FITS table.  Type conversions are
*     performed if necessary.
*  Arguments:
*     FITYPE  =  INTEGER (Given)
*        FITS data type of the column.
*     FITUNT  =  INTEGER (Given)
*        Fortran unit number for accessing the FITS table.
*     COLNO  =  INTEGER (Given)
*        Sequence number for the column in the FITS table.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be put.  For scalars it should
*        adopt the value 1.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUEB  =  BYTE (Given)
*        Value put.  The value need not be of the same data type
*        as the field.  Type conversions are performed if necessary.
*     NULFLG  =  LOGICAL (Given)
*        Flag indicating whether or not the alue is null.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each FITS data type:
*       If the null flag is not set then
*         Attempt to convert the value to the appropriate FITS data type.
*         If not ok then
*           Set the local null flag.
*         end if
*       else
*         Set the local null flag.
*       end if
*       If the local null flag is not set then
*         Attempt to put the value to the FITS table.
*       else
*         Attempt to put a null value to the FITS table.
*       end if
*     else (the FITS data type is invalid)
*       Set the return status: invalid data type.
*     end if
*     Report any FITS error.
*     Report any general error.
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
*     8/10/93 (ACD): Original version (based on GVALB).
*     24/1/94 (ACD): Modified error reporting.
*     30/1/94 (ACD): Implemented proper treatment of null values.
*     2/2/94  (ACD): Added handling of vector column elements.
*     6/7/94  (ACD): Re-written to handle FITS complex columns, and
*       handle scaled columns properly.
*     12/7/94 (ACD): Re-written.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT3_FIT_PAR'      ! FITS back-end symbolic constants.
*  Arguments Given:
      INTEGER
     :  FITYPE,
     :  FITUNT,
     :  COLNO,
     :  ELEM,
     :  ROWNO
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
*  Local Constants:
      INTEGER FITOK  ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:

*
*    Local copies of the value to be put to the FITS table, after
*    type conversion.

      BYTE             BUFFUB
      BYTE             BUFFB
      INTEGER*2        BUFFUW
      INTEGER*2        BUFFW
      INTEGER          BUFFI
      REAL             BUFFR
      DOUBLE PRECISION BUFFD
      LOGICAL          BUFFL
      CHARACTER        BUFFC*(CAT__SZVAL)

      LOGICAL
     :  LNULFG,       ! Local flag; is the value null?
     :  CONVOK        ! Flag; did the data type conversion succeed?
      INTEGER
     :  FITSTT,       ! FITSIO status.
     :  ERRPOS        ! Position in ERRMSG (excl.trail. blanks).
      CHARACTER
     :  ERRMSG*75     ! Error message text.
*.

      IF (STATUS .EQ. CAT__OK) THEN

         IF (FITYPE .EQ. CAT3__FTYPI) THEN

*
*          FITS data type WORD.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEB, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEW, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLI (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFW,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN

*
*          FITS data type INTEGER.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEB, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEI, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLJ (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFI,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPE) THEN

*
*          FITS data type REAL.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEB, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPER, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLE (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFR,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPD) THEN

*
*          FITS data type DOUBLE PRECISION.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEB, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPED, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLD (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFD,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPL) THEN

*
*          FITS data type LOGICAL.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEB, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEL, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLL (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFL,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPA) THEN

*
*          FITS data type CHARACTER.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEB, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEC, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLS (FITUNT, COLNO, ROWNO, ELEM, 1,
     :           BUFFC,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE

*
*          The FITS data type is invalid; set the status and report an
*          error message.

            STATUS = CAT__INVDT
         END IF

*
*       Report any FITS error.

         IF (FITSTT .NE. FITOK) THEN
            STATUS = CAT__ERROR

            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure putting field of FITS type ',
     :        ERRMSG, ERRPOS)

            IF (FITYPE .EQ. CAT3__FTYPI) THEN
               CALL CHR_PUTC ('WORD', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN
               CALL CHR_PUTC ('INTEGER', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPE) THEN
               CALL CHR_PUTC ('REAL', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPD) THEN
               CALL CHR_PUTC ('DOUBLE PRECISION', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPL) THEN
               CALL CHR_PUTC ('LOGICAL', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPA) THEN
               CALL CHR_PUTC ('CHARACTER', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN
               CALL CHR_PUTC ('<unknown code: ', ERRMSG, ERRPOS)
               CALL CHR_PUTI (FITYPE, ERRMSG, ERRPOS)
               CALL CHR_PUTC ('>', ERRMSG, ERRPOS)
            END IF

            CALL CHR_PUTC ('.', ERRMSG, ERRPOS)

            CALL CAT3_FITER ('CAT3_PVALB_IDT', ERRMSG(1 : ERRPOS),
     :        FITSTT, STATUS)
         END IF

*
*       Report any error.  Note that the following code is intended to
*       be executed if an FITS error has been reported.

         IF (STATUS .NE. CAT__OK) THEN
            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure putting field to FITS table '/
     :        /'(column: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (COLNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (', element: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRPOS)
            CALL CHR_PUTC (', row: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (').', ERRMSG, ERRPOS)

            CALL CAT1_ERREP ('CAT3_PVALB_ERR', ERRMSG(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT3_PVALC (FITYPE, FITUNT, COLNO, ELEM, ROWNO,
     :  VALUEC, NULFLG, STATUS)
*+
*  Name:
*     CAT3_PVALC
*  Purpose:
*     Put a value of type C to the FITS table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_PVALC (FITYPE, FITUNT, COLNO, ELEM, ROWNO,
*        VALUEC, NULFLG; STATUS)
*  Description:
*     Put a value of type C to the FITS table.  Type conversions are
*     performed if necessary.
*  Arguments:
*     FITYPE  =  INTEGER (Given)
*        FITS data type of the column.
*     FITUNT  =  INTEGER (Given)
*        Fortran unit number for accessing the FITS table.
*     COLNO  =  INTEGER (Given)
*        Sequence number for the column in the FITS table.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be put.  For scalars it should
*        adopt the value 1.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUEC  =  CHARACTER*(*) (Given)
*        Value put.  The value need not be of the same data type
*        as the field.  Type conversions are performed if necessary.
*     NULFLG  =  LOGICAL (Given)
*        Flag indicating whether or not the alue is null.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each FITS data type:
*       If the null flag is not set then
*         Attempt to convert the value to the appropriate FITS data type.
*         If not ok then
*           Set the local null flag.
*         end if
*       else
*         Set the local null flag.
*       end if
*       If the local null flag is not set then
*         Attempt to put the value to the FITS table.
*       else
*         Attempt to put a null value to the FITS table.
*       end if
*     else (the FITS data type is invalid)
*       Set the return status: invalid data type.
*     end if
*     Report any FITS error.
*     Report any general error.
*  Implementation Deficiencies:
*     Unsigned byte and unsigned word data types are not handled.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     8/10/93 (ACD): Original version (based on GVALC).
*     24/1/94 (ACD): Modified error reporting.
*     30/1/94 (ACD): Implemented proper treatment of null values.
*     2/2/94  (ACD): Added handling of vector column elements.
*     6/7/94  (ACD): Re-written to handle FITS complex columns, and
*       handle scaled columns properly.
*     12/7/94 (ACD): Re-written.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT3_FIT_PAR'      ! FITS back-end symbolic constants.
*  Arguments Given:
      INTEGER
     :  FITYPE,
     :  FITUNT,
     :  COLNO,
     :  ELEM,
     :  ROWNO
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
*  Local Constants:
      INTEGER FITOK  ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:

*
*    Local copies of the value to be put to the FITS table, after
*    type conversion.

      BYTE             BUFFUB
      BYTE             BUFFB
      INTEGER*2        BUFFUW
      INTEGER*2        BUFFW
      INTEGER          BUFFI
      REAL             BUFFR
      DOUBLE PRECISION BUFFD
      LOGICAL          BUFFL
      CHARACTER        BUFFC*(CAT__SZVAL)

      LOGICAL
     :  LNULFG,       ! Local flag; is the value null?
     :  CONVOK        ! Flag; did the data type conversion succeed?
      INTEGER
     :  FITSTT,       ! FITSIO status.
     :  ERRPOS        ! Position in ERRMSG (excl.trail. blanks).
      CHARACTER
     :  ERRMSG*75     ! Error message text.
*.

      IF (STATUS .EQ. CAT__OK) THEN

         IF (FITYPE .EQ. CAT3__FTYPI) THEN

*
*          FITS data type WORD.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEC, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEW, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLI (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFW,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN

*
*          FITS data type INTEGER.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEC, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEI, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLJ (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFI,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPE) THEN

*
*          FITS data type REAL.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEC, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPER, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLE (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFR,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPD) THEN

*
*          FITS data type DOUBLE PRECISION.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEC, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPED, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLD (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFD,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPL) THEN

*
*          FITS data type LOGICAL.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEC, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEL, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLL (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFL,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPA) THEN

*
*          FITS data type CHARACTER.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEC, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEC, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLS (FITUNT, COLNO, ROWNO, ELEM, 1,
     :           BUFFC,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE

*
*          The FITS data type is invalid; set the status and report an
*          error message.

            STATUS = CAT__INVDT
         END IF

*
*       Report any FITS error.

         IF (FITSTT .NE. FITOK) THEN
            STATUS = CAT__ERROR

            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure putting field of FITS type ',
     :        ERRMSG, ERRPOS)

            IF (FITYPE .EQ. CAT3__FTYPI) THEN
               CALL CHR_PUTC ('WORD', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN
               CALL CHR_PUTC ('INTEGER', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPE) THEN
               CALL CHR_PUTC ('REAL', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPD) THEN
               CALL CHR_PUTC ('DOUBLE PRECISION', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPL) THEN
               CALL CHR_PUTC ('LOGICAL', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPA) THEN
               CALL CHR_PUTC ('CHARACTER', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN
               CALL CHR_PUTC ('<unknown code: ', ERRMSG, ERRPOS)
               CALL CHR_PUTI (FITYPE, ERRMSG, ERRPOS)
               CALL CHR_PUTC ('>', ERRMSG, ERRPOS)
            END IF

            CALL CHR_PUTC ('.', ERRMSG, ERRPOS)

            CALL CAT3_FITER ('CAT3_PVALC_IDT', ERRMSG(1 : ERRPOS),
     :        FITSTT, STATUS)
         END IF

*
*       Report any error.  Note that the following code is intended to
*       be executed if an FITS error has been reported.

         IF (STATUS .NE. CAT__OK) THEN
            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure putting field to FITS table '/
     :        /'(column: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (COLNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (', element: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRPOS)
            CALL CHR_PUTC (', row: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (').', ERRMSG, ERRPOS)

            CALL CAT1_ERREP ('CAT3_PVALC_ERR', ERRMSG(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT3_PVALD (FITYPE, FITUNT, COLNO, ELEM, ROWNO,
     :  VALUED, NULFLG, STATUS)
*+
*  Name:
*     CAT3_PVALD
*  Purpose:
*     Put a value of type D to the FITS table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_PVALD (FITYPE, FITUNT, COLNO, ELEM, ROWNO,
*        VALUED, NULFLG; STATUS)
*  Description:
*     Put a value of type D to the FITS table.  Type conversions are
*     performed if necessary.
*  Arguments:
*     FITYPE  =  INTEGER (Given)
*        FITS data type of the column.
*     FITUNT  =  INTEGER (Given)
*        Fortran unit number for accessing the FITS table.
*     COLNO  =  INTEGER (Given)
*        Sequence number for the column in the FITS table.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be put.  For scalars it should
*        adopt the value 1.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUED  =  DOUBLE PRECISION (Given)
*        Value put.  The value need not be of the same data type
*        as the field.  Type conversions are performed if necessary.
*     NULFLG  =  LOGICAL (Given)
*        Flag indicating whether or not the alue is null.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each FITS data type:
*       If the null flag is not set then
*         Attempt to convert the value to the appropriate FITS data type.
*         If not ok then
*           Set the local null flag.
*         end if
*       else
*         Set the local null flag.
*       end if
*       If the local null flag is not set then
*         Attempt to put the value to the FITS table.
*       else
*         Attempt to put a null value to the FITS table.
*       end if
*     else (the FITS data type is invalid)
*       Set the return status: invalid data type.
*     end if
*     Report any FITS error.
*     Report any general error.
*  Implementation Deficiencies:
*     Unsigned byte and unsigned word data types are not handled.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     8/10/93 (ACD): Original version (based on GVALD).
*     24/1/94 (ACD): Modified error reporting.
*     30/1/94 (ACD): Implemented proper treatment of null values.
*     2/2/94  (ACD): Added handling of vector column elements.
*     6/7/94  (ACD): Re-written to handle FITS complex columns, and
*       handle scaled columns properly.
*     12/7/94 (ACD): Re-written.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT3_FIT_PAR'      ! FITS back-end symbolic constants.
*  Arguments Given:
      INTEGER
     :  FITYPE,
     :  FITUNT,
     :  COLNO,
     :  ELEM,
     :  ROWNO
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
*  Local Constants:
      INTEGER FITOK  ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:

*
*    Local copies of the value to be put to the FITS table, after
*    type conversion.

      BYTE             BUFFUB
      BYTE             BUFFB
      INTEGER*2        BUFFUW
      INTEGER*2        BUFFW
      INTEGER          BUFFI
      REAL             BUFFR
      DOUBLE PRECISION BUFFD
      LOGICAL          BUFFL
      CHARACTER        BUFFC*(CAT__SZVAL)

      LOGICAL
     :  LNULFG,       ! Local flag; is the value null?
     :  CONVOK        ! Flag; did the data type conversion succeed?
      INTEGER
     :  FITSTT,       ! FITSIO status.
     :  ERRPOS        ! Position in ERRMSG (excl.trail. blanks).
      CHARACTER
     :  ERRMSG*75     ! Error message text.
*.

      IF (STATUS .EQ. CAT__OK) THEN

         IF (FITYPE .EQ. CAT3__FTYPI) THEN

*
*          FITS data type WORD.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPED, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEW, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLI (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFW,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN

*
*          FITS data type INTEGER.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPED, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEI, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLJ (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFI,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPE) THEN

*
*          FITS data type REAL.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPED, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPER, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLE (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFR,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPD) THEN

*
*          FITS data type DOUBLE PRECISION.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPED, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPED, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLD (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFD,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPL) THEN

*
*          FITS data type LOGICAL.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPED, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEL, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLL (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFL,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPA) THEN

*
*          FITS data type CHARACTER.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPED, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEC, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLS (FITUNT, COLNO, ROWNO, ELEM, 1,
     :           BUFFC,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE

*
*          The FITS data type is invalid; set the status and report an
*          error message.

            STATUS = CAT__INVDT
         END IF

*
*       Report any FITS error.

         IF (FITSTT .NE. FITOK) THEN
            STATUS = CAT__ERROR

            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure putting field of FITS type ',
     :        ERRMSG, ERRPOS)

            IF (FITYPE .EQ. CAT3__FTYPI) THEN
               CALL CHR_PUTC ('WORD', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN
               CALL CHR_PUTC ('INTEGER', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPE) THEN
               CALL CHR_PUTC ('REAL', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPD) THEN
               CALL CHR_PUTC ('DOUBLE PRECISION', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPL) THEN
               CALL CHR_PUTC ('LOGICAL', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPA) THEN
               CALL CHR_PUTC ('CHARACTER', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN
               CALL CHR_PUTC ('<unknown code: ', ERRMSG, ERRPOS)
               CALL CHR_PUTI (FITYPE, ERRMSG, ERRPOS)
               CALL CHR_PUTC ('>', ERRMSG, ERRPOS)
            END IF

            CALL CHR_PUTC ('.', ERRMSG, ERRPOS)

            CALL CAT3_FITER ('CAT3_PVALD_IDT', ERRMSG(1 : ERRPOS),
     :        FITSTT, STATUS)
         END IF

*
*       Report any error.  Note that the following code is intended to
*       be executed if an FITS error has been reported.

         IF (STATUS .NE. CAT__OK) THEN
            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure putting field to FITS table '/
     :        /'(column: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (COLNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (', element: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRPOS)
            CALL CHR_PUTC (', row: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (').', ERRMSG, ERRPOS)

            CALL CAT1_ERREP ('CAT3_PVALD_ERR', ERRMSG(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT3_PVALI (FITYPE, FITUNT, COLNO, ELEM, ROWNO,
     :  VALUEI, NULFLG, STATUS)
*+
*  Name:
*     CAT3_PVALI
*  Purpose:
*     Put a value of type I to the FITS table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_PVALI (FITYPE, FITUNT, COLNO, ELEM, ROWNO,
*        VALUEI, NULFLG; STATUS)
*  Description:
*     Put a value of type I to the FITS table.  Type conversions are
*     performed if necessary.
*  Arguments:
*     FITYPE  =  INTEGER (Given)
*        FITS data type of the column.
*     FITUNT  =  INTEGER (Given)
*        Fortran unit number for accessing the FITS table.
*     COLNO  =  INTEGER (Given)
*        Sequence number for the column in the FITS table.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be put.  For scalars it should
*        adopt the value 1.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUEI  =  INTEGER (Given)
*        Value put.  The value need not be of the same data type
*        as the field.  Type conversions are performed if necessary.
*     NULFLG  =  LOGICAL (Given)
*        Flag indicating whether or not the alue is null.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each FITS data type:
*       If the null flag is not set then
*         Attempt to convert the value to the appropriate FITS data type.
*         If not ok then
*           Set the local null flag.
*         end if
*       else
*         Set the local null flag.
*       end if
*       If the local null flag is not set then
*         Attempt to put the value to the FITS table.
*       else
*         Attempt to put a null value to the FITS table.
*       end if
*     else (the FITS data type is invalid)
*       Set the return status: invalid data type.
*     end if
*     Report any FITS error.
*     Report any general error.
*  Implementation Deficiencies:
*     Unsigned byte and unsigned word data types are not handled.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     8/10/93 (ACD): Original version (based on GVALI).
*     24/1/94 (ACD): Modified error reporting.
*     30/1/94 (ACD): Implemented proper treatment of null values.
*     2/2/94  (ACD): Added handling of vector column elements.
*     6/7/94  (ACD): Re-written to handle FITS complex columns, and
*       handle scaled columns properly.
*     12/7/94 (ACD): Re-written.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT3_FIT_PAR'      ! FITS back-end symbolic constants.
*  Arguments Given:
      INTEGER
     :  FITYPE,
     :  FITUNT,
     :  COLNO,
     :  ELEM,
     :  ROWNO
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
*  Local Constants:
      INTEGER FITOK  ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:

*
*    Local copies of the value to be put to the FITS table, after
*    type conversion.

      BYTE             BUFFUB
      BYTE             BUFFB
      INTEGER*2        BUFFUW
      INTEGER*2        BUFFW
      INTEGER          BUFFI
      REAL             BUFFR
      DOUBLE PRECISION BUFFD
      LOGICAL          BUFFL
      CHARACTER        BUFFC*(CAT__SZVAL)

      LOGICAL
     :  LNULFG,       ! Local flag; is the value null?
     :  CONVOK        ! Flag; did the data type conversion succeed?
      INTEGER
     :  FITSTT,       ! FITSIO status.
     :  ERRPOS        ! Position in ERRMSG (excl.trail. blanks).
      CHARACTER
     :  ERRMSG*75     ! Error message text.
*.

      IF (STATUS .EQ. CAT__OK) THEN

         IF (FITYPE .EQ. CAT3__FTYPI) THEN

*
*          FITS data type WORD.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEI, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEW, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLI (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFW,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN

*
*          FITS data type INTEGER.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEI, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEI, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLJ (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFI,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPE) THEN

*
*          FITS data type REAL.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEI, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPER, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLE (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFR,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPD) THEN

*
*          FITS data type DOUBLE PRECISION.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEI, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPED, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLD (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFD,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPL) THEN

*
*          FITS data type LOGICAL.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEI, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEL, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLL (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFL,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPA) THEN

*
*          FITS data type CHARACTER.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEI, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEC, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLS (FITUNT, COLNO, ROWNO, ELEM, 1,
     :           BUFFC,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE

*
*          The FITS data type is invalid; set the status and report an
*          error message.

            STATUS = CAT__INVDT
         END IF

*
*       Report any FITS error.

         IF (FITSTT .NE. FITOK) THEN
            STATUS = CAT__ERROR

            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure putting field of FITS type ',
     :        ERRMSG, ERRPOS)

            IF (FITYPE .EQ. CAT3__FTYPI) THEN
               CALL CHR_PUTC ('WORD', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN
               CALL CHR_PUTC ('INTEGER', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPE) THEN
               CALL CHR_PUTC ('REAL', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPD) THEN
               CALL CHR_PUTC ('DOUBLE PRECISION', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPL) THEN
               CALL CHR_PUTC ('LOGICAL', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPA) THEN
               CALL CHR_PUTC ('CHARACTER', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN
               CALL CHR_PUTC ('<unknown code: ', ERRMSG, ERRPOS)
               CALL CHR_PUTI (FITYPE, ERRMSG, ERRPOS)
               CALL CHR_PUTC ('>', ERRMSG, ERRPOS)
            END IF

            CALL CHR_PUTC ('.', ERRMSG, ERRPOS)

            CALL CAT3_FITER ('CAT3_PVALI_IDT', ERRMSG(1 : ERRPOS),
     :        FITSTT, STATUS)
         END IF

*
*       Report any error.  Note that the following code is intended to
*       be executed if an FITS error has been reported.

         IF (STATUS .NE. CAT__OK) THEN
            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure putting field to FITS table '/
     :        /'(column: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (COLNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (', element: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRPOS)
            CALL CHR_PUTC (', row: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (').', ERRMSG, ERRPOS)

            CALL CAT1_ERREP ('CAT3_PVALI_ERR', ERRMSG(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT3_PVALL (FITYPE, FITUNT, COLNO, ELEM, ROWNO,
     :  VALUEL, NULFLG, STATUS)
*+
*  Name:
*     CAT3_PVALL
*  Purpose:
*     Put a value of type L to the FITS table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_PVALL (FITYPE, FITUNT, COLNO, ELEM, ROWNO,
*        VALUEL, NULFLG; STATUS)
*  Description:
*     Put a value of type L to the FITS table.  Type conversions are
*     performed if necessary.
*  Arguments:
*     FITYPE  =  INTEGER (Given)
*        FITS data type of the column.
*     FITUNT  =  INTEGER (Given)
*        Fortran unit number for accessing the FITS table.
*     COLNO  =  INTEGER (Given)
*        Sequence number for the column in the FITS table.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be put.  For scalars it should
*        adopt the value 1.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUEL  =  LOGICAL (Given)
*        Value put.  The value need not be of the same data type
*        as the field.  Type conversions are performed if necessary.
*     NULFLG  =  LOGICAL (Given)
*        Flag indicating whether or not the alue is null.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each FITS data type:
*       If the null flag is not set then
*         Attempt to convert the value to the appropriate FITS data type.
*         If not ok then
*           Set the local null flag.
*         end if
*       else
*         Set the local null flag.
*       end if
*       If the local null flag is not set then
*         Attempt to put the value to the FITS table.
*       else
*         Attempt to put a null value to the FITS table.
*       end if
*     else (the FITS data type is invalid)
*       Set the return status: invalid data type.
*     end if
*     Report any FITS error.
*     Report any general error.
*  Implementation Deficiencies:
*     Unsigned byte and unsigned word data types are not handled.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     8/10/93 (ACD): Original version (based on GVALL).
*     24/1/94 (ACD): Modified error reporting.
*     30/1/94 (ACD): Implemented proper treatment of null values.
*     2/2/94  (ACD): Added handling of vector column elements.
*     6/7/94  (ACD): Re-written to handle FITS complex columns, and
*       handle scaled columns properly.
*     12/7/94 (ACD): Re-written.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT3_FIT_PAR'      ! FITS back-end symbolic constants.
*  Arguments Given:
      INTEGER
     :  FITYPE,
     :  FITUNT,
     :  COLNO,
     :  ELEM,
     :  ROWNO
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
*  Local Constants:
      INTEGER FITOK  ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:

*
*    Local copies of the value to be put to the FITS table, after
*    type conversion.

      BYTE             BUFFUB
      BYTE             BUFFB
      INTEGER*2        BUFFUW
      INTEGER*2        BUFFW
      INTEGER          BUFFI
      REAL             BUFFR
      DOUBLE PRECISION BUFFD
      LOGICAL          BUFFL
      CHARACTER        BUFFC*(CAT__SZVAL)

      LOGICAL
     :  LNULFG,       ! Local flag; is the value null?
     :  CONVOK        ! Flag; did the data type conversion succeed?
      INTEGER
     :  FITSTT,       ! FITSIO status.
     :  ERRPOS        ! Position in ERRMSG (excl.trail. blanks).
      CHARACTER
     :  ERRMSG*75     ! Error message text.
*.

      IF (STATUS .EQ. CAT__OK) THEN

         IF (FITYPE .EQ. CAT3__FTYPI) THEN

*
*          FITS data type WORD.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEL, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEW, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLI (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFW,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN

*
*          FITS data type INTEGER.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEL, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEI, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLJ (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFI,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPE) THEN

*
*          FITS data type REAL.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEL, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPER, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLE (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFR,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPD) THEN

*
*          FITS data type DOUBLE PRECISION.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEL, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPED, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLD (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFD,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPL) THEN

*
*          FITS data type LOGICAL.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEL, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEL, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLL (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFL,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPA) THEN

*
*          FITS data type CHARACTER.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEL, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEC, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLS (FITUNT, COLNO, ROWNO, ELEM, 1,
     :           BUFFC,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE

*
*          The FITS data type is invalid; set the status and report an
*          error message.

            STATUS = CAT__INVDT
         END IF

*
*       Report any FITS error.

         IF (FITSTT .NE. FITOK) THEN
            STATUS = CAT__ERROR

            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure putting field of FITS type ',
     :        ERRMSG, ERRPOS)

            IF (FITYPE .EQ. CAT3__FTYPI) THEN
               CALL CHR_PUTC ('WORD', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN
               CALL CHR_PUTC ('INTEGER', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPE) THEN
               CALL CHR_PUTC ('REAL', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPD) THEN
               CALL CHR_PUTC ('DOUBLE PRECISION', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPL) THEN
               CALL CHR_PUTC ('LOGICAL', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPA) THEN
               CALL CHR_PUTC ('CHARACTER', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN
               CALL CHR_PUTC ('<unknown code: ', ERRMSG, ERRPOS)
               CALL CHR_PUTI (FITYPE, ERRMSG, ERRPOS)
               CALL CHR_PUTC ('>', ERRMSG, ERRPOS)
            END IF

            CALL CHR_PUTC ('.', ERRMSG, ERRPOS)

            CALL CAT3_FITER ('CAT3_PVALL_IDT', ERRMSG(1 : ERRPOS),
     :        FITSTT, STATUS)
         END IF

*
*       Report any error.  Note that the following code is intended to
*       be executed if an FITS error has been reported.

         IF (STATUS .NE. CAT__OK) THEN
            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure putting field to FITS table '/
     :        /'(column: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (COLNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (', element: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRPOS)
            CALL CHR_PUTC (', row: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (').', ERRMSG, ERRPOS)

            CALL CAT1_ERREP ('CAT3_PVALL_ERR', ERRMSG(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT3_PVALR (FITYPE, FITUNT, COLNO, ELEM, ROWNO,
     :  VALUER, NULFLG, STATUS)
*+
*  Name:
*     CAT3_PVALR
*  Purpose:
*     Put a value of type R to the FITS table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_PVALR (FITYPE, FITUNT, COLNO, ELEM, ROWNO,
*        VALUER, NULFLG; STATUS)
*  Description:
*     Put a value of type R to the FITS table.  Type conversions are
*     performed if necessary.
*  Arguments:
*     FITYPE  =  INTEGER (Given)
*        FITS data type of the column.
*     FITUNT  =  INTEGER (Given)
*        Fortran unit number for accessing the FITS table.
*     COLNO  =  INTEGER (Given)
*        Sequence number for the column in the FITS table.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be put.  For scalars it should
*        adopt the value 1.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUER  =  REAL (Given)
*        Value put.  The value need not be of the same data type
*        as the field.  Type conversions are performed if necessary.
*     NULFLG  =  LOGICAL (Given)
*        Flag indicating whether or not the alue is null.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each FITS data type:
*       If the null flag is not set then
*         Attempt to convert the value to the appropriate FITS data type.
*         If not ok then
*           Set the local null flag.
*         end if
*       else
*         Set the local null flag.
*       end if
*       If the local null flag is not set then
*         Attempt to put the value to the FITS table.
*       else
*         Attempt to put a null value to the FITS table.
*       end if
*     else (the FITS data type is invalid)
*       Set the return status: invalid data type.
*     end if
*     Report any FITS error.
*     Report any general error.
*  Implementation Deficiencies:
*     Unsigned byte and unsigned word data types are not handled.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     8/10/93 (ACD): Original version (based on GVALR).
*     24/1/94 (ACD): Modified error reporting.
*     30/1/94 (ACD): Implemented proper treatment of null values.
*     2/2/94  (ACD): Added handling of vector column elements.
*     6/7/94  (ACD): Re-written to handle FITS complex columns, and
*       handle scaled columns properly.
*     12/7/94 (ACD): Re-written.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT3_FIT_PAR'      ! FITS back-end symbolic constants.
*  Arguments Given:
      INTEGER
     :  FITYPE,
     :  FITUNT,
     :  COLNO,
     :  ELEM,
     :  ROWNO
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
*  Local Constants:
      INTEGER FITOK  ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:

*
*    Local copies of the value to be put to the FITS table, after
*    type conversion.

      BYTE             BUFFUB
      BYTE             BUFFB
      INTEGER*2        BUFFUW
      INTEGER*2        BUFFW
      INTEGER          BUFFI
      REAL             BUFFR
      DOUBLE PRECISION BUFFD
      LOGICAL          BUFFL
      CHARACTER        BUFFC*(CAT__SZVAL)

      LOGICAL
     :  LNULFG,       ! Local flag; is the value null?
     :  CONVOK        ! Flag; did the data type conversion succeed?
      INTEGER
     :  FITSTT,       ! FITSIO status.
     :  ERRPOS        ! Position in ERRMSG (excl.trail. blanks).
      CHARACTER
     :  ERRMSG*75     ! Error message text.
*.

      IF (STATUS .EQ. CAT__OK) THEN

         IF (FITYPE .EQ. CAT3__FTYPI) THEN

*
*          FITS data type WORD.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPER, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEW, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLI (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFW,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN

*
*          FITS data type INTEGER.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPER, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEI, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLJ (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFI,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPE) THEN

*
*          FITS data type REAL.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPER, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPER, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLE (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFR,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPD) THEN

*
*          FITS data type DOUBLE PRECISION.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPER, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPED, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLD (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFD,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPL) THEN

*
*          FITS data type LOGICAL.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPER, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEL, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLL (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFL,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPA) THEN

*
*          FITS data type CHARACTER.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPER, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEC, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLS (FITUNT, COLNO, ROWNO, ELEM, 1,
     :           BUFFC,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE

*
*          The FITS data type is invalid; set the status and report an
*          error message.

            STATUS = CAT__INVDT
         END IF

*
*       Report any FITS error.

         IF (FITSTT .NE. FITOK) THEN
            STATUS = CAT__ERROR

            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure putting field of FITS type ',
     :        ERRMSG, ERRPOS)

            IF (FITYPE .EQ. CAT3__FTYPI) THEN
               CALL CHR_PUTC ('WORD', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN
               CALL CHR_PUTC ('INTEGER', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPE) THEN
               CALL CHR_PUTC ('REAL', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPD) THEN
               CALL CHR_PUTC ('DOUBLE PRECISION', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPL) THEN
               CALL CHR_PUTC ('LOGICAL', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPA) THEN
               CALL CHR_PUTC ('CHARACTER', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN
               CALL CHR_PUTC ('<unknown code: ', ERRMSG, ERRPOS)
               CALL CHR_PUTI (FITYPE, ERRMSG, ERRPOS)
               CALL CHR_PUTC ('>', ERRMSG, ERRPOS)
            END IF

            CALL CHR_PUTC ('.', ERRMSG, ERRPOS)

            CALL CAT3_FITER ('CAT3_PVALR_IDT', ERRMSG(1 : ERRPOS),
     :        FITSTT, STATUS)
         END IF

*
*       Report any error.  Note that the following code is intended to
*       be executed if an FITS error has been reported.

         IF (STATUS .NE. CAT__OK) THEN
            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure putting field to FITS table '/
     :        /'(column: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (COLNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (', element: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRPOS)
            CALL CHR_PUTC (', row: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (').', ERRMSG, ERRPOS)

            CALL CAT1_ERREP ('CAT3_PVALR_ERR', ERRMSG(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT3_PVALW (FITYPE, FITUNT, COLNO, ELEM, ROWNO,
     :  VALUEW, NULFLG, STATUS)
*+
*  Name:
*     CAT3_PVALW
*  Purpose:
*     Put a value of type W to the FITS table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_PVALW (FITYPE, FITUNT, COLNO, ELEM, ROWNO,
*        VALUEW, NULFLG; STATUS)
*  Description:
*     Put a value of type W to the FITS table.  Type conversions are
*     performed if necessary.
*  Arguments:
*     FITYPE  =  INTEGER (Given)
*        FITS data type of the column.
*     FITUNT  =  INTEGER (Given)
*        Fortran unit number for accessing the FITS table.
*     COLNO  =  INTEGER (Given)
*        Sequence number for the column in the FITS table.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be put.  For scalars it should
*        adopt the value 1.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUEW  =  INTEGER*2 (Given)
*        Value put.  The value need not be of the same data type
*        as the field.  Type conversions are performed if necessary.
*     NULFLG  =  LOGICAL (Given)
*        Flag indicating whether or not the alue is null.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each FITS data type:
*       If the null flag is not set then
*         Attempt to convert the value to the appropriate FITS data type.
*         If not ok then
*           Set the local null flag.
*         end if
*       else
*         Set the local null flag.
*       end if
*       If the local null flag is not set then
*         Attempt to put the value to the FITS table.
*       else
*         Attempt to put a null value to the FITS table.
*       end if
*     else (the FITS data type is invalid)
*       Set the return status: invalid data type.
*     end if
*     Report any FITS error.
*     Report any general error.
*  Implementation Deficiencies:
*     Unsigned byte and unsigned word data types are not handled.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     8/10/93 (ACD): Original version (based on GVALW).
*     24/1/94 (ACD): Modified error reporting.
*     30/1/94 (ACD): Implemented proper treatment of null values.
*     2/2/94  (ACD): Added handling of vector column elements.
*     6/7/94  (ACD): Re-written to handle FITS complex columns, and
*       handle scaled columns properly.
*     12/7/94 (ACD): Re-written.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT3_FIT_PAR'      ! FITS back-end symbolic constants.
*  Arguments Given:
      INTEGER
     :  FITYPE,
     :  FITUNT,
     :  COLNO,
     :  ELEM,
     :  ROWNO
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
*  Local Constants:
      INTEGER FITOK  ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:

*
*    Local copies of the value to be put to the FITS table, after
*    type conversion.

      BYTE             BUFFUB
      BYTE             BUFFB
      INTEGER*2        BUFFUW
      INTEGER*2        BUFFW
      INTEGER          BUFFI
      REAL             BUFFR
      DOUBLE PRECISION BUFFD
      LOGICAL          BUFFL
      CHARACTER        BUFFC*(CAT__SZVAL)

      LOGICAL
     :  LNULFG,       ! Local flag; is the value null?
     :  CONVOK        ! Flag; did the data type conversion succeed?
      INTEGER
     :  FITSTT,       ! FITSIO status.
     :  ERRPOS        ! Position in ERRMSG (excl.trail. blanks).
      CHARACTER
     :  ERRMSG*75     ! Error message text.
*.

      IF (STATUS .EQ. CAT__OK) THEN

         IF (FITYPE .EQ. CAT3__FTYPI) THEN

*
*          FITS data type WORD.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEW, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEW, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLI (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFW,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN

*
*          FITS data type INTEGER.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEW, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEI, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLJ (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFI,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPE) THEN

*
*          FITS data type REAL.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEW, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPER, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLE (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFR,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPD) THEN

*
*          FITS data type DOUBLE PRECISION.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEW, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPED, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLD (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFD,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPL) THEN

*
*          FITS data type LOGICAL.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEW, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEL, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLL (FITUNT, COLNO, ROWNO, ELEM, 1, BUFFL,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE IF (FITYPE .EQ. CAT3__FTYPA) THEN

*
*          FITS data type CHARACTER.
*
*          If the null flag is not set then attempt to convert the value
*          to the appropriate FITSIO data type (the local null value is
*          set if the attempt fails).  Otherwise set the local null flag.

            IF (.NOT. NULFLG) THEN
               CALL CAT1_TCNVT (CAT__TYPEW, VALUEUB, VALUEB, VALUEUW,
     :           VALUEW, VALUEI, VALUER, VALUED, VALUEL, VALUEC,
     :           CAT__TYPEC, BUFFUB, BUFFB, BUFFUW, BUFFW, BUFFI, BUFFR,
     :
     :           BUFFD, BUFFL, BUFFC, CONVOK, STATUS)

               IF (CONVOK) THEN
                  LNULFG = .FALSE.
               ELSE
                  LNULFG = .TRUE.
               END IF

            ELSE
               LNULFG = .TRUE.

            END IF

*
*          Attempt to write either an actual value or a null value to the
*          FITS table.

            FITSTT = FITOK

            IF (.NOT. LNULFG) THEN
               CALL FTPCLS (FITUNT, COLNO, ROWNO, ELEM, 1,
     :           BUFFC,
     :           FITSTT)
            ELSE
               CALL FTPCLU (FITUNT, COLNO, ROWNO, ELEM, 1, FITSTT)
            END IF

         ELSE

*
*          The FITS data type is invalid; set the status and report an
*          error message.

            STATUS = CAT__INVDT
         END IF

*
*       Report any FITS error.

         IF (FITSTT .NE. FITOK) THEN
            STATUS = CAT__ERROR

            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure putting field of FITS type ',
     :        ERRMSG, ERRPOS)

            IF (FITYPE .EQ. CAT3__FTYPI) THEN
               CALL CHR_PUTC ('WORD', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN
               CALL CHR_PUTC ('INTEGER', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPE) THEN
               CALL CHR_PUTC ('REAL', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPD) THEN
               CALL CHR_PUTC ('DOUBLE PRECISION', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPL) THEN
               CALL CHR_PUTC ('LOGICAL', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPA) THEN
               CALL CHR_PUTC ('CHARACTER', ERRMSG, ERRPOS)

            ELSE IF (FITYPE .EQ. CAT3__FTYPJ) THEN
               CALL CHR_PUTC ('<unknown code: ', ERRMSG, ERRPOS)
               CALL CHR_PUTI (FITYPE, ERRMSG, ERRPOS)
               CALL CHR_PUTC ('>', ERRMSG, ERRPOS)
            END IF

            CALL CHR_PUTC ('.', ERRMSG, ERRPOS)

            CALL CAT3_FITER ('CAT3_PVALW_IDT', ERRMSG(1 : ERRPOS),
     :        FITSTT, STATUS)
         END IF

*
*       Report any error.  Note that the following code is intended to
*       be executed if an FITS error has been reported.

         IF (STATUS .NE. CAT__OK) THEN
            ERRPOS = 0
            ERRMSG = ' '

            CALL CHR_PUTC ('Failure putting field to FITS table '/
     :        /'(column: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (COLNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (', element: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRPOS)
            CALL CHR_PUTC (', row: ', ERRMSG, ERRPOS)
            CALL CHR_PUTI (ROWNO, ERRMSG, ERRPOS)
            CALL CHR_PUTC (').', ERRMSG, ERRPOS)

            CALL CAT1_ERREP ('CAT3_PVALW_ERR', ERRMSG(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
