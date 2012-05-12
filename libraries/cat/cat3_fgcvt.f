*     SUBROUTINE CAT3_FGCV<t> (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
*    :   ELEM, DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI, VALUEK, VALUER,
*    :   VALUED, VALUEL, VALUEC, NULFLG, STATUS)
*+
*  Name:
*     CAT3_FGCV<t>
*  Purpose:
*     Get a value from a FITS table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_FGCV<t> (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
*       ELEM, DTYPE; VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI, VALUEK, VALUER,
*       VALUED, VALUEL, VALUEC, NULFLG; STATUS)
*  Description:
*     Get a value from a FITS table using the FITSIO routines.
*
*     This routine is generic, in the sense that that there are multiple
*     versions, each corresponding to one of the FITS data types.  However
*     individual routines are maintained for the different FITS data types
*     because these data types, and the letters used to distinguish them,
*     are different from the Starlink ones used by the GENERIC compiler.
*
*     The FITSIO data types, and the corresponding CAT data types are
*     as follows:
*
*       Description      FITSIO tables   CAT data type
*                       binary    ASCII
*       variable array    P         -         -
*       bit mask          X         -         B
*       BYTE              B         -         B
*       WORD              I         -         W
*       INTEGER           J         I         I
*       INTEGER*8         K         -         K
*       REAL              E        F,E        R
*       DOUBLE PRECISION  D         D         D
*       LOGICAL           L         -         L
*       CHARACTER         A         A         C
*       complex REAL      C         -         R (converted)
*       complex DOUBLE    M         -         D (    "    )
*
*     This prologue serves for all the CAT3_FGCV<t> routines.  The
*     code for all these routines is included in this file.
*  Arguments:
*     SFLAG  =  LOGICAL (Given)
*        Flag indicating whether or not the column is scaled.
*     SCALE  =  DOUBLE PRECISION (Given)
*        Scale factor if the column is scaled.
*     ZERO  =  DOUBLE PRECISION (Given)
*        Zero point if the column is scaled.
*     FITUNT   =  INTEGER (Given)
*        Fortran unit number for accessing the FITS table.
*     COLNO   =  INTEGER (Given)
*        Number of the column in the FITS table which is to be accessed.
*     ROWNO   =  INTEGER (Given)
*        Number of the row in the FITS table which is to be accessed.
*     ELEM   =  INTEGER (Given)
*        For binary tables the first pixel of the element vector to be
*        accessed; ignored for ASCII tables.
*     DTYPE  =  INTEGR (Given)
*        CAT code for the data type to be returned.
*     VALUEUB  =  BYTE (Returned)
*        Value returned if an unsigned BYTE value is requested.
*     VALUEB  =  BYTE (Returned)
*        Value returned if a BYTE value is requested.
*     VALUEUW  =  INTEGER*2 (Returned)
*        Value returned if an unsigned WORD value is requested.
*     VALUEW  =  INTEGER*2 (Returned)
*        Value returned if a WORD value is requested.
*     VALUEI  =  INTEGER (Returned)
*        Value returned if an INTEGER value is requested.
*     VALUEI  =  INTEGER*8 (Returned)
*        Value returned if an INTEGER*8 value is requested.
*     VALUER  =  REAL (Returned)
*        Value returned if a REAL value is requested.
*     VALUED  =  DOUBLE PRECISION (Returned)
*        Value returned if a DOUBLE PRECISION value is requested.
*     VALUEL  =  LOGICAL (Returned)
*        Value returned if a LOGICAL value is requested.
*     VALUEC  =  CHARACTER*(*) (Returned)
*        Value returned if a CHARACTER value is requested.
*     NULFLG   =  LOGICAL (Returned)
*        Flag indicating whether or not the value is null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise the null value flag to indicate a non-null value.
*     Work out the array element.
*     Set the FITS status.
*     Attempt to get a value.
*     If the FITS status is ok then
*       If no null values have been obtained then
*         If the value is not scaled then
*           Attempt to convert the value to the required CAT data type.
*           If the conversion fails then
*             Set the null flag.
*           end if
*         else (the value is scaled)
*           Attempt to convert the value to DOUBLE PRECISION.
*           If ok then
*             Apply the scaling.
*             Attempt to convert the DOUBLE PRECISION value to the
*             required data type.
*             If the conversion fails then
*               Set the null flag.
*             end if
*           else
*             Set the null flag.
*           end if
*         end if
*       else (null values have been obtained)
*         Set the null flag.
*       end if
*     else
*       Set the null flag.
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
*     30/1/94 (ACD): Original version.
*     10/2/94 (ACD): Fixed bug with setting the null value flag in the
*        CHARACTER version when a valid datum is available.
*     16/6/94 (ACD): Added proper support for FITS LOGICAL columns.
*     7/7/94  (ACD): Re-written to handle complex columns and handle
*        scaled columns properly.
*     4/6/98  (ACD): Changed all occurences of 'StarBase' to 'CAT'.
*  Bugs:
*     None known
*-

* -- Bit mask version ----------------------------------------------------
      SUBROUTINE CAT3_FGCVX (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :   ELEM, DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI, VALUEK,
     :   VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Arguments Given:
      LOGICAL
     :  SFLAG
      DOUBLE PRECISION
     :  SCALE,
     :  ZERO
      INTEGER
     :  FITUNT,
     :  COLNO,
     :  ROWNO,
     :  ELEM,
     :  DTYPE
*  Arguments Returned:
      BYTE             VALUEUB
      BYTE             VALUEB
      INTEGER*2        VALUEUW
      INTEGER*2        VALUEW
      INTEGER          VALUEI
      INTEGER*8        VALUEK
      REAL             VALUER
      DOUBLE PRECISION VALUED
      LOGICAL          VALUEL
      CHARACTER        VALUEC*(*)

      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Constants:
      INTEGER FITOK  ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:
      INTEGER
     :  FELEM,    ! Element of the FITS array to be accessed.
     :  FITSTT,   ! FITSIO status.
     :  ERRPOS    ! Current position in ERRMSG (excl. trail. blanks).
      LOGICAL
     :  FLGVAL,   ! Null value flag from FITSIO.
     :  ANYF,     !  "     "    "    "     "   .
     :  CONVOK    ! Flag; did data type conversion complete ok?
      CHARACTER
     :  ERRMSG*75 ! Text of error message.

*
*    These variables hold the local copy of the value obtained from the
*    FITS column.  There is one variable per CAT data type.

      BYTE             LVALUB
      BYTE             LVALB
      INTEGER*2        LVALUW
      INTEGER*2        LVALW
      INTEGER          LVALI
      INTEGER*8        LVALK
      REAL             LVALR
      DOUBLE PRECISION LVALD
      LOGICAL          LVALL
      CHARACTER        LVALC*(CAT__SZVAL)

*
*    These variables hold an intermediate copy of the value obtained
*    from the FITS column.  They are used in the extra conversion
*    to data type DOUBLE PRECISION necessary for scaled columns.
*    There is one variable per CAT data type.

      BYTE             IVALUB
      BYTE             IVALB
      INTEGER*2        IVALUW
      INTEGER*2        IVALW
      INTEGER          IVALI
      INTEGER*8        IVALK
      REAL             IVALR
      DOUBLE PRECISION IVALD
      LOGICAL          IVALL
      CHARACTER        IVALC*(CAT__SZVAL)
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the null value flag to indicate a non-null value.

         NULFLG = .FALSE.

*
*       Work out the FITS element from the CAT element.

         FELEM = ELEM

*
*       Set the FITS status, attempt to get a value and proceed if ok.

         FITSTT = FITOK
         CALL FTGCFB (FITUNT, COLNO, ROWNO, FELEM, 1, LVALB,
     :     FLGVAL, ANYF, FITSTT)

         IF (FITSTT .EQ. FITOK) THEN

*
*          Check that no null values have been obtained.

            IF (.NOT. ANYF) THEN

*
*             Check if the column is scaled.

               IF (.NOT. SFLAG) THEN

*
*                The column is not scaled; simply convert the value from
*                the FITS column to the appropriate data type.

                  CALL CAT1_TCNVT (CAT__TYPEB, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :              VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :              STATUS)

                  IF (.NOT. CONVOK) THEN
                     NULFLG = .TRUE.
                  END IF

               ELSE

*
*                The column is scaled.  First convert the value obtained
*                from the FITS table to DOUBLE PRECISION, then apply the
*                scaling and finally convert the value to the required
*                data type.

                  CALL CAT1_TCNVT (CAT__TYPEB, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              CAT__TYPED, IVALUB, IVALB, IVALUW, IVALW, IVALI,
     :              IVALK, IVALR, IVALD, IVALL, IVALC, CONVOK, STATUS)

                  IF (CONVOK) THEN
                     IVALD = (IVALD * SCALE) + ZERO

                     CALL CAT1_TCNVT (CAT__TYPED, IVALUB, IVALB, IVALUW,
     :                 IVALW, IVALI, IVALK, IVALR, IVALD, IVALL, IVALC,
     :                 DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :                 VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :                 STATUS)

                     IF (.NOT. CONVOK) THEN
                        NULFLG = .TRUE.
                     END IF

                  ELSE
                     NULFLG = .TRUE.
                  END IF
               END IF
            ELSE

*
*             A null value was obtained from FITSIO.

               NULFLG = .TRUE.

            END IF

         ELSE

*
*          FITSIO returned an error status.  Set the null flag, set
*          the return status and report an error.

            NULFLG = .TRUE.

            STATUS = CAT__ERROR

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

            CALL CAT3_FITER ('CAT3_FGCVX_ERR', ERRMSG(1 : ERRPOS),
     :        FITSTT, STATUS)
         END IF

      END IF

      END

* -- Byte version -------------------------------------------------------
      SUBROUTINE CAT3_FGCVB (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :   ELEM, DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI, VALUEK,
     :   VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Arguments Given:
      LOGICAL
     :  SFLAG
      DOUBLE PRECISION
     :  SCALE,
     :  ZERO
      INTEGER
     :  FITUNT,
     :  COLNO,
     :  ROWNO,
     :  ELEM,
     :  DTYPE
*  Arguments Returned:
      BYTE             VALUEUB
      BYTE             VALUEB
      INTEGER*2        VALUEUW
      INTEGER*2        VALUEW
      INTEGER          VALUEI
      INTEGER*8        VALUEK
      REAL             VALUER
      DOUBLE PRECISION VALUED
      LOGICAL          VALUEL
      CHARACTER        VALUEC*(*)

      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Constants:
      INTEGER FITOK  ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:
      INTEGER
     :  FELEM,    ! Element of the FITS array to be accessed.
     :  FITSTT,   ! FITSIO status.
     :  ERRPOS    ! Current position in ERRMSG (excl. trail. blanks).
      LOGICAL
     :  FLGVAL,   ! Null value flag from FITSIO.
     :  ANYF,     !  "     "    "    "     "   .
     :  CONVOK    ! Flag; did data type conversion complete ok?
      CHARACTER
     :  ERRMSG*75 ! Text of error message.

*
*    These variables hold the local copy of the value obtained from the
*    FITS column.  There is one variable per CAT data type.

      BYTE             LVALUB
      BYTE             LVALB
      INTEGER*2        LVALUW
      INTEGER*2        LVALW
      INTEGER          LVALI
      INTEGER*8        LVALK
      REAL             LVALR
      DOUBLE PRECISION LVALD
      LOGICAL          LVALL
      CHARACTER        LVALC*(CAT__SZVAL)

*
*    These variables hold an intermediate copy of the value obtained
*    from the FITS column.  They are used in the extra conversion
*    to data type DOUBLE PRECISION necessary for scaled columns.
*    There is one variable per CAT data type.

      BYTE             IVALUB
      BYTE             IVALB
      INTEGER*2        IVALUW
      INTEGER*2        IVALW
      INTEGER          IVALI
      INTEGER*8        IVALK
      REAL             IVALR
      DOUBLE PRECISION IVALD
      LOGICAL          IVALL
      CHARACTER        IVALC*(CAT__SZVAL)
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the null value flag to indicate a non-null value.

         NULFLG = .FALSE.

*
*       Work out the FITS element from the CAT element.

         FELEM = ELEM

*
*       Set the FITS status, attempt to get a value and proceed if ok.

         FITSTT = FITOK
         CALL FTGCFB (FITUNT, COLNO, ROWNO, FELEM, 1, LVALB,
     :     FLGVAL, ANYF, FITSTT)

         IF (FITSTT .EQ. FITOK) THEN

*
*          Check that no null values have been obtained.

            IF (.NOT. ANYF) THEN

*
*             Check if the column is scaled.

               IF (.NOT. SFLAG) THEN

*
*                The column is not scaled; simply convert the value from
*                the FITS column to the appropriate data type.

                  CALL CAT1_TCNVT (CAT__TYPEB, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :              VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :              STATUS)

                  IF (.NOT. CONVOK) THEN
                     NULFLG = .TRUE.
                  END IF

               ELSE

*
*                The column is scaled.  First convert the value obtained
*                from the FITS table to DOUBLE PRECISION, then apply the
*                scaling and finally convert the value to the required
*                data type.

                  CALL CAT1_TCNVT (CAT__TYPEB, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              CAT__TYPED, IVALUB, IVALB, IVALUW, IVALW, IVALI,
     :              IVALK, IVALR, IVALD, IVALL, IVALC, CONVOK, STATUS)

                  IF (CONVOK) THEN
                     IVALD = (IVALD * SCALE) + ZERO

                     CALL CAT1_TCNVT (CAT__TYPED, IVALUB, IVALB, IVALUW,
     :                 IVALW, IVALI, IVALK, IVALR, IVALD, IVALL, IVALC,
     :                 DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :                 VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :                 STATUS)

                     IF (.NOT. CONVOK) THEN
                        NULFLG = .TRUE.
                     END IF

                  ELSE
                     NULFLG = .TRUE.
                  END IF
               END IF
            ELSE

*
*             A null value was obtained from FITSIO.

               NULFLG = .TRUE.

            END IF

         ELSE

*
*          FITSIO returned an error status.  Set the null flag, set
*          the return status and report an error.

            NULFLG = .TRUE.

            STATUS = CAT__ERROR

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

            CALL CAT3_FITER ('CAT3_FGCVB_ERR', ERRMSG(1 : ERRPOS),
     :        FITSTT, STATUS)
         END IF

      END IF

      END

* -- Word version -------------------------------------------------------
      SUBROUTINE CAT3_FGCVI (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :   ELEM, DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI, VALUEK,
     :   VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Arguments Given:
      LOGICAL
     :  SFLAG
      DOUBLE PRECISION
     :  SCALE,
     :  ZERO
      INTEGER
     :  FITUNT,
     :  COLNO,
     :  ROWNO,
     :  ELEM,
     :  DTYPE
*  Arguments Returned:
      BYTE             VALUEUB
      BYTE             VALUEB
      INTEGER*2        VALUEUW
      INTEGER*2        VALUEW
      INTEGER          VALUEI
      INTEGER*8        VALUEK
      REAL             VALUER
      DOUBLE PRECISION VALUED
      LOGICAL          VALUEL
      CHARACTER        VALUEC*(*)

      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Constants:
      INTEGER FITOK  ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:
      INTEGER
     :  FELEM,    ! Element of the FITS array to be accessed.
     :  FITSTT,   ! FITSIO status.
     :  ERRPOS    ! Current position in ERRMSG (excl. trail. blanks).
      LOGICAL
     :  FLGVAL,   ! Null value flag from FITSIO.
     :  ANYF,     !  "     "    "    "     "   .
     :  CONVOK    ! Flag; did data type conversion complete ok?
      CHARACTER
     :  ERRMSG*75 ! Text of error message.

*
*    These variables hold the local copy of the value obtained from the
*    FITS column.  There is one variable per CAT data type.

      BYTE             LVALUB
      BYTE             LVALB
      INTEGER*2        LVALUW
      INTEGER*2        LVALW
      INTEGER          LVALI
      INTEGER*8        LVALK
      REAL             LVALR
      DOUBLE PRECISION LVALD
      LOGICAL          LVALL
      CHARACTER        LVALC*(CAT__SZVAL)

*
*    These variables hold an intermediate copy of the value obtained
*    from the FITS column.  They are used in the extra conversion
*    to data type DOUBLE PRECISION necessary for scaled columns.
*    There is one variable per CAT data type.

      BYTE             IVALUB
      BYTE             IVALB
      INTEGER*2        IVALUW
      INTEGER*2        IVALW
      INTEGER          IVALI
      INTEGER*8        IVALK
      REAL             IVALR
      DOUBLE PRECISION IVALD
      LOGICAL          IVALL
      CHARACTER        IVALC*(CAT__SZVAL)
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the null value flag to indicate a non-null value.

         NULFLG = .FALSE.

*
*       Work out the FITS element from the CAT element.

         FELEM = ELEM

*
*       Set the FITS status, attempt to get a value and proceed if ok.

         FITSTT = FITOK
         CALL FTGCFI (FITUNT, COLNO, ROWNO, FELEM, 1, LVALW,
     :     FLGVAL, ANYF, FITSTT)

         IF (FITSTT .EQ. FITOK) THEN

*
*          Check that no null values have been obtained.

            IF (.NOT. ANYF) THEN

*
*             Check if the column is scaled.

               IF (.NOT. SFLAG) THEN

*
*                The column is not scaled; simply convert the value from
*                the FITS column to the appropriate data type.

                  CALL CAT1_TCNVT (CAT__TYPEW, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :              VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :              STATUS)

                  IF (.NOT. CONVOK) THEN
                     NULFLG = .TRUE.
                  END IF

               ELSE

*
*                The column is scaled.  First convert the value obtained
*                from the FITS table to DOUBLE PRECISION, then apply the
*                scaling and finally convert the value to the required
*                data type.

                  CALL CAT1_TCNVT (CAT__TYPEW, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              CAT__TYPED, IVALUB, IVALB, IVALUW, IVALW, IVALI,
     :              IVALK, IVALR, IVALD, IVALL, IVALC, CONVOK, STATUS)

                  IF (CONVOK) THEN
                     IVALD = (IVALD * SCALE) + ZERO

                     CALL CAT1_TCNVT (CAT__TYPED, IVALUB, IVALB, IVALUW,
     :                 IVALW, IVALI, IVALK, IVALR, IVALD, IVALL, IVALC,
     :                 DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :                 VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :                 STATUS)

                     IF (.NOT. CONVOK) THEN
                        NULFLG = .TRUE.
                     END IF

                  ELSE
                     NULFLG = .TRUE.
                  END IF
               END IF
            ELSE

*
*             A null value was obtained from FITSIO.

               NULFLG = .TRUE.

            END IF

         ELSE

*
*          FITSIO returned an error status.  Set the null flag, set
*          the return status and report an error.

            NULFLG = .TRUE.

            STATUS = CAT__ERROR

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

            CALL CAT3_FITER ('CAT3_FGCVW_ERR', ERRMSG(1 : ERRPOS),
     :        FITSTT, STATUS)
         END IF

      END IF

      END

* -- Integer version ----------------------------------------------------
      SUBROUTINE CAT3_FGCVJ (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :   ELEM, DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI, VALUEK,
     :   VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Arguments Given:
      LOGICAL
     :  SFLAG
      DOUBLE PRECISION
     :  SCALE,
     :  ZERO
      INTEGER
     :  FITUNT,
     :  COLNO,
     :  ROWNO,
     :  ELEM,
     :  DTYPE
*  Arguments Returned:
      BYTE             VALUEUB
      BYTE             VALUEB
      INTEGER*2        VALUEUW
      INTEGER*2        VALUEW
      INTEGER          VALUEI
      INTEGER*8        VALUEK
      REAL             VALUER
      DOUBLE PRECISION VALUED
      LOGICAL          VALUEL
      CHARACTER        VALUEC*(*)

      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Constants:
      INTEGER FITOK  ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:
      INTEGER
     :  FELEM,    ! Element of the FITS array to be accessed.
     :  FITSTT,   ! FITSIO status.
     :  ERRPOS    ! Current position in ERRMSG (excl. trail. blanks).
      LOGICAL
     :  FLGVAL,   ! Null value flag from FITSIO.
     :  ANYF,     !  "     "    "    "     "   .
     :  CONVOK    ! Flag; did data type conversion complete ok?
      CHARACTER
     :  ERRMSG*75 ! Text of error message.

*
*    These variables hold the local copy of the value obtained from the
*    FITS column.  There is one variable per CAT data type.

      BYTE             LVALUB
      BYTE             LVALB
      INTEGER*2        LVALUW
      INTEGER*2        LVALW
      INTEGER          LVALI
      INTEGER*8        LVALK
      REAL             LVALR
      DOUBLE PRECISION LVALD
      LOGICAL          LVALL
      CHARACTER        LVALC*(CAT__SZVAL)

*
*    These variables hold an intermediate copy of the value obtained
*    from the FITS column.  They are used in the extra conversion
*    to data type DOUBLE PRECISION necessary for scaled columns.
*    There is one variable per CAT data type.

      BYTE             IVALUB
      BYTE             IVALB
      INTEGER*2        IVALUW
      INTEGER*2        IVALW
      INTEGER          IVALI
      INTEGER*8        IVALK
      REAL             IVALR
      DOUBLE PRECISION IVALD
      LOGICAL          IVALL
      CHARACTER        IVALC*(CAT__SZVAL)
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the null value flag to indicate a non-null value.

         NULFLG = .FALSE.

*
*       Work out the FITS element from the CAT element.

         FELEM = ELEM

*
*       Set the FITS status, attempt to get a value and proceed if ok.

         FITSTT = FITOK
         CALL FTGCFJ (FITUNT, COLNO, ROWNO, FELEM, 1, LVALI,
     :     FLGVAL, ANYF, FITSTT)

         IF (FITSTT .EQ. FITOK) THEN

*
*          Check that no null values have been obtained.

            IF (.NOT. ANYF) THEN

*
*             Check if the column is scaled.

               IF (.NOT. SFLAG) THEN

*
*                The column is not scaled; simply convert the value from
*                the FITS column to the appropriate data type.

                  CALL CAT1_TCNVT (CAT__TYPEI, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :              VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :              STATUS)

                  IF (.NOT. CONVOK) THEN
                     NULFLG = .TRUE.
                  END IF

               ELSE

*
*                The column is scaled.  First convert the value obtained
*                from the FITS table to DOUBLE PRECISION, then apply the
*                scaling and finally convert the value to the required
*                data type.

                  CALL CAT1_TCNVT (CAT__TYPEI, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              CAT__TYPED, IVALUB, IVALB, IVALUW, IVALW, IVALI,
     :              IVALK, IVALR, IVALD, IVALL, IVALC, CONVOK, STATUS)

                  IF (CONVOK) THEN
                     IVALD = (IVALD * SCALE) + ZERO

                     CALL CAT1_TCNVT (CAT__TYPED, IVALUB, IVALB, IVALUW,
     :                 IVALW, IVALI, IVALK, IVALR, IVALD, IVALL, IVALC,
     :                 DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :                 VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :                 STATUS)

                     IF (.NOT. CONVOK) THEN
                        NULFLG = .TRUE.
                     END IF

                  ELSE
                     NULFLG = .TRUE.
                  END IF
               END IF
            ELSE

*
*             A null value was obtained from FITSIO.

               NULFLG = .TRUE.

            END IF

         ELSE

*
*          FITSIO returned an error status.  Set the null flag, set
*          the return status and report an error.

            NULFLG = .TRUE.

            STATUS = CAT__ERROR

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

            CALL CAT3_FITER ('CAT3_FGCVI_ERR', ERRMSG(1 : ERRPOS),
     :        FITSTT, STATUS)
         END IF

      END IF

      END

* -- 64-bit Integer version ----------------------------------------------------
      SUBROUTINE CAT3_FGCVK (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :   ELEM, DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI, VALUEK,
     :   VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Arguments Given:
      LOGICAL
     :  SFLAG
      DOUBLE PRECISION
     :  SCALE,
     :  ZERO
      INTEGER
     :  FITUNT,
     :  COLNO,
     :  ROWNO,
     :  ELEM,
     :  DTYPE
*  Arguments Returned:
      BYTE             VALUEUB
      BYTE             VALUEB
      INTEGER*2        VALUEUW
      INTEGER*2        VALUEW
      INTEGER          VALUEI
      INTEGER*8        VALUEK
      REAL             VALUER
      DOUBLE PRECISION VALUED
      LOGICAL          VALUEL
      CHARACTER        VALUEC*(*)

      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Constants:
      INTEGER FITOK  ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:
      INTEGER
     :  FELEM,    ! Element of the FITS array to be accessed.
     :  FITSTT,   ! FITSIO status.
     :  ERRPOS    ! Current position in ERRMSG (excl. trail. blanks).
      LOGICAL
     :  FLGVAL,   ! Null value flag from FITSIO.
     :  ANYF,     !  "     "    "    "     "   .
     :  CONVOK    ! Flag; did data type conversion complete ok?
      CHARACTER
     :  ERRMSG*75 ! Text of error message.

*
*    These variables hold the local copy of the value obtained from the
*    FITS column.  There is one variable per CAT data type.

      BYTE             LVALUB
      BYTE             LVALB
      INTEGER*2        LVALUW
      INTEGER*2        LVALW
      INTEGER          LVALI
      INTEGER*8        LVALK
      REAL             LVALR
      DOUBLE PRECISION LVALD
      LOGICAL          LVALL
      CHARACTER        LVALC*(CAT__SZVAL)

*
*    These variables hold an intermediate copy of the value obtained
*    from the FITS column.  They are used in the extra conversion
*    to data type DOUBLE PRECISION necessary for scaled columns.
*    There is one variable per CAT data type.

      BYTE             IVALUB
      BYTE             IVALB
      INTEGER*2        IVALUW
      INTEGER*2        IVALW
      INTEGER          IVALI
      INTEGER*8        IVALK
      REAL             IVALR
      DOUBLE PRECISION IVALD
      LOGICAL          IVALL
      CHARACTER        IVALC*(CAT__SZVAL)
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the null value flag to indicate a non-null value.

         NULFLG = .FALSE.

*
*       Work out the FITS element from the CAT element.

         FELEM = ELEM

*
*       Set the FITS status, attempt to get a value and proceed if ok.

         FITSTT = FITOK
         CALL FTGCFK (FITUNT, COLNO, ROWNO, FELEM, 1, LVALI,
     :     FLGVAL, ANYF, FITSTT)

         IF (FITSTT .EQ. FITOK) THEN

*
*          Check that no null values have been obtained.

            IF (.NOT. ANYF) THEN

*
*             Check if the column is scaled.

               IF (.NOT. SFLAG) THEN

*
*                The column is not scaled; simply convert the value from
*                the FITS column to the appropriate data type.

                  CALL CAT1_TCNVT (CAT__TYPEK, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :              VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :              STATUS)

                  IF (.NOT. CONVOK) THEN
                     NULFLG = .TRUE.
                  END IF

               ELSE

*
*                The column is scaled.  First convert the value obtained
*                from the FITS table to DOUBLE PRECISION, then apply the
*                scaling and finally convert the value to the required
*                data type.

                  CALL CAT1_TCNVT (CAT__TYPEL, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              CAT__TYPED, IVALUB, IVALB, IVALUW, IVALW, IVALI,
     :              IVALK, IVALR, IVALD, IVALL, IVALC, CONVOK, STATUS)

                  IF (CONVOK) THEN
                     IVALD = (IVALD * SCALE) + ZERO

                     CALL CAT1_TCNVT (CAT__TYPED, IVALUB, IVALB, IVALUW,
     :                 IVALW, IVALI, IVALK,  IVALR, IVALD, IVALL, IVALC,
     :                 DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :                 VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :                 STATUS)

                     IF (.NOT. CONVOK) THEN
                        NULFLG = .TRUE.
                     END IF

                  ELSE
                     NULFLG = .TRUE.
                  END IF
               END IF
            ELSE

*
*             A null value was obtained from FITSIO.

               NULFLG = .TRUE.

            END IF

         ELSE

*
*          FITSIO returned an error status.  Set the null flag, set
*          the return status and report an error.

            NULFLG = .TRUE.

            STATUS = CAT__ERROR

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

            CALL CAT3_FITER ('CAT3_FGCVK_ERR', ERRMSG(1 : ERRPOS),
     :        FITSTT, STATUS)
         END IF

      END IF

      END

* -- REAL version ------------------------------------------------------
      SUBROUTINE CAT3_FGCVE (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :   ELEM, DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI, VALUEK,
     :   VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Arguments Given:
      LOGICAL
     :  SFLAG
      DOUBLE PRECISION
     :  SCALE,
     :  ZERO
      INTEGER
     :  FITUNT,
     :  COLNO,
     :  ROWNO,
     :  ELEM,
     :  DTYPE
*  Arguments Returned:
      BYTE             VALUEUB
      BYTE             VALUEB
      INTEGER*2        VALUEUW
      INTEGER*2        VALUEW
      INTEGER          VALUEI
      INTEGER*8        VALUEK
      REAL             VALUER
      DOUBLE PRECISION VALUED
      LOGICAL          VALUEL
      CHARACTER        VALUEC*(*)

      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Constants:
      INTEGER FITOK  ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:
      INTEGER
     :  FELEM,    ! Element of the FITS array to be accessed.
     :  FITSTT,   ! FITSIO status.
     :  ERRPOS    ! Current position in ERRMSG (excl. trail. blanks).
      LOGICAL
     :  FLGVAL,   ! Null value flag from FITSIO.
     :  ANYF,     !  "     "    "    "     "   .
     :  CONVOK    ! Flag; did data type conversion complete ok?
      CHARACTER
     :  ERRMSG*75 ! Text of error message.

*
*    These variables hold the local copy of the value obtained from the
*    FITS column.  There is one variable per CAT data type.

      BYTE             LVALUB
      BYTE             LVALB
      INTEGER*2        LVALUW
      INTEGER*2        LVALW
      INTEGER          LVALI
      INTEGER*8        LVALK
      REAL             LVALR
      DOUBLE PRECISION LVALD
      LOGICAL          LVALL
      CHARACTER        LVALC*(CAT__SZVAL)

*
*    These variables hold an intermediate copy of the value obtained
*    from the FITS column.  They are used in the extra conversion
*    to data type DOUBLE PRECISION necessary for scaled columns.
*    There is one variable per CAT data type.

      BYTE             IVALUB
      BYTE             IVALB
      INTEGER*2        IVALUW
      INTEGER*2        IVALW
      INTEGER          IVALI
      INTEGER*8        IVALK
      REAL             IVALR
      DOUBLE PRECISION IVALD
      LOGICAL          IVALL
      CHARACTER        IVALC*(CAT__SZVAL)
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the null value flag to indicate a non-null value.

         NULFLG = .FALSE.

*
*       Work out the FITS element from the CAT element.

         FELEM = ELEM

*
*       Set the FITS status, attempt to get a value and proceed if ok.

         FITSTT = FITOK
         CALL FTGCFE (FITUNT, COLNO, ROWNO, FELEM, 1, LVALR,
     :     FLGVAL, ANYF, FITSTT)

         IF (FITSTT .EQ. FITOK) THEN

*
*          Check that no null values have been obtained.

            IF (.NOT. ANYF) THEN

*
*             Check if the column is scaled.

               IF (.NOT. SFLAG) THEN

*
*                The column is not scaled; simply convert the value from
*                the FITS column to the appropriate data type.

                  CALL CAT1_TCNVT (CAT__TYPER, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :              VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :              STATUS)

                  IF (.NOT. CONVOK) THEN
                     NULFLG = .TRUE.
                  END IF

               ELSE

*
*                The column is scaled.  First convert the value obtained
*                from the FITS table to DOUBLE PRECISION, then apply the
*                scaling and finally convert the value to the required
*                data type.

                  CALL CAT1_TCNVT (CAT__TYPER, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              CAT__TYPED, IVALUB, IVALB, IVALUW, IVALW, IVALI,
     :              IVALK, IVALR, IVALD, IVALL, IVALC, CONVOK, STATUS)

                  IF (CONVOK) THEN
                     IVALD = (IVALD * SCALE) + ZERO

                     CALL CAT1_TCNVT (CAT__TYPED, IVALUB, IVALB, IVALUW,
     :                 IVALW, IVALI, IVALK, IVALR, IVALD, IVALL, IVALC,
     :                 DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :                 VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :                 STATUS)

                     IF (.NOT. CONVOK) THEN
                        NULFLG = .TRUE.
                     END IF

                  ELSE
                     NULFLG = .TRUE.
                  END IF
               END IF
            ELSE

*
*             A null value was obtained from FITSIO.

               NULFLG = .TRUE.

            END IF

         ELSE

*
*          FITSIO returned an error status.  Set the null flag, set
*          the return status and report an error.

            NULFLG = .TRUE.

            STATUS = CAT__ERROR

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

            CALL CAT3_FITER ('CAT3_FGCVR_ERR', ERRMSG(1 : ERRPOS),
     :        FITSTT, STATUS)
         END IF

      END IF

      END

* -- DOUBLE PRECISION version ------------------------------------------------
      SUBROUTINE CAT3_FGCVD (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :   ELEM, DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI, VALUEK,
     :   VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Arguments Given:
      LOGICAL
     :  SFLAG
      DOUBLE PRECISION
     :  SCALE,
     :  ZERO
      INTEGER
     :  FITUNT,
     :  COLNO,
     :  ROWNO,
     :  ELEM,
     :  DTYPE
*  Arguments Returned:
      BYTE             VALUEUB
      BYTE             VALUEB
      INTEGER*2        VALUEUW
      INTEGER*2        VALUEW
      INTEGER          VALUEI
      INTEGER*8        VALUEK
      REAL             VALUER
      DOUBLE PRECISION VALUED
      LOGICAL          VALUEL
      CHARACTER        VALUEC*(*)

      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Constants:
      INTEGER FITOK  ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:
      INTEGER
     :  FELEM,    ! Element of the FITS array to be accessed.
     :  FITSTT,   ! FITSIO status.
     :  ERRPOS    ! Current position in ERRMSG (excl. trail. blanks).
      LOGICAL
     :  FLGVAL,   ! Null value flag from FITSIO.
     :  ANYF,     !  "     "    "    "     "   .
     :  CONVOK    ! Flag; did data type conversion complete ok?
      CHARACTER
     :  ERRMSG*75 ! Text of error message.

*
*    These variables hold the local copy of the value obtained from the
*    FITS column.  There is one variable per CAT data type.

      BYTE             LVALUB
      BYTE             LVALB
      INTEGER*2        LVALUW
      INTEGER*2        LVALW
      INTEGER          LVALI
      INTEGER*8        LVALK
      REAL             LVALR
      DOUBLE PRECISION LVALD
      LOGICAL          LVALL
      CHARACTER        LVALC*(CAT__SZVAL)

*
*    These variables hold an intermediate copy of the value obtained
*    from the FITS column.  They are used in the extra conversion
*    to data type DOUBLE PRECISION necessary for scaled columns.
*    There is one variable per CAT data type.

      BYTE             IVALUB
      BYTE             IVALB
      INTEGER*2        IVALUW
      INTEGER*2        IVALW
      INTEGER          IVALI
      INTEGER*8        IVALK
      REAL             IVALR
      DOUBLE PRECISION IVALD
      LOGICAL          IVALL
      CHARACTER        IVALC*(CAT__SZVAL)
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the null value flag to indicate a non-null value.

         NULFLG = .FALSE.

*
*       Work out the FITS element from the CAT element.

         FELEM = ELEM

*
*       Set the FITS status, attempt to get a value and proceed if ok.

         FITSTT = FITOK
         CALL FTGCFD (FITUNT, COLNO, ROWNO, FELEM, 1, LVALD,
     :     FLGVAL, ANYF, FITSTT)
C        print3000, colno, rowno, felem, lvald, flgval, anyf, fitstt
C3000    format(1x, 'colno, rowno, felem, lvald, flgval, anyf, fitstt: '
C    :     / i6, i6, i4, 1pd18.8, l5, l5, i6)

         IF (FITSTT .EQ. FITOK) THEN

*
*          Check that no null values have been obtained.

            IF (.NOT. ANYF) THEN

*
*             Check if the column is scaled.

               IF (.NOT. SFLAG) THEN

*
*                The column is not scaled; simply convert the value from
*                the FITS column to the appropriate data type.

                  CALL CAT1_TCNVT (CAT__TYPED, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :              VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :              STATUS)

                  IF (.NOT. CONVOK) THEN
                     NULFLG = .TRUE.
                  END IF

               ELSE

*
*                The column is scaled.  First convert the value obtained
*                from the FITS table to DOUBLE PRECISION, then apply the
*                scaling and finally convert the value to the required
*                data type.

                  CALL CAT1_TCNVT (CAT__TYPED, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              CAT__TYPED, IVALUB, IVALB, IVALUW, IVALW, IVALI,
     :              IVALK, IVALR, IVALD, IVALL, IVALC, CONVOK, STATUS)

                  IF (CONVOK) THEN
                     IVALD = (IVALD * SCALE) + ZERO

                     CALL CAT1_TCNVT (CAT__TYPED, IVALUB, IVALB, IVALUW,
     :                 IVALW, IVALI, IVALK, IVALR, IVALD, IVALL, IVALC,
     :                 DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :                 VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :                 STATUS)

                     IF (.NOT. CONVOK) THEN
                        NULFLG = .TRUE.
                     END IF

                  ELSE
                     NULFLG = .TRUE.
                  END IF
               END IF
            ELSE

*
*             A null value was obtained from FITSIO.

               NULFLG = .TRUE.

            END IF

         ELSE

*
*          FITSIO returned an error status.  Set the null flag, set
*          the return status and report an error.

            NULFLG = .TRUE.

            STATUS = CAT__ERROR

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

            CALL CAT3_FITER ('CAT3_FGCVD_ERR', ERRMSG(1 : ERRPOS),
     :        FITSTT, STATUS)
         END IF

      END IF

      END

* -- LOGICAL version ------------------------------------------------------
      SUBROUTINE CAT3_FGCVL (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :   ELEM, DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI, VALUEK,
     :   VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Arguments Given:
      LOGICAL
     :  SFLAG
      DOUBLE PRECISION
     :  SCALE,
     :  ZERO
      INTEGER
     :  FITUNT,
     :  COLNO,
     :  ROWNO,
     :  ELEM,
     :  DTYPE
*  Arguments Returned:
      BYTE             VALUEUB
      BYTE             VALUEB
      INTEGER*2        VALUEUW
      INTEGER*2        VALUEW
      INTEGER          VALUEI
      INTEGER*8        VALUEK
      REAL             VALUER
      DOUBLE PRECISION VALUED
      LOGICAL          VALUEL
      CHARACTER        VALUEC*(*)

      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Constants:
      INTEGER FITOK  ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:
      INTEGER
     :  FELEM,    ! Element of the FITS array to be accessed.
     :  FITSTT,   ! FITSIO status.
     :  ERRPOS    ! Current position in ERRMSG (excl. trail. blanks).
      LOGICAL
     :  FLGVAL,   ! Null value flag from FITSIO.
     :  ANYF,     !  "     "    "    "     "   .
     :  CONVOK    ! Flag; did data type conversion complete ok?
      CHARACTER
     :  ERRMSG*75 ! Text of error message.

*
*    These variables hold the local copy of the value obtained from the
*    FITS column.  There is one variable per CAT data type.

      BYTE             LVALUB
      BYTE             LVALB
      INTEGER*2        LVALUW
      INTEGER*2        LVALW
      INTEGER          LVALI
      INTEGER*8        LVALK
      REAL             LVALR
      DOUBLE PRECISION LVALD
      LOGICAL          LVALL
      CHARACTER        LVALC*(CAT__SZVAL)

*
*    These variables hold an intermediate copy of the value obtained
*    from the FITS column.  They are used in the extra conversion
*    to data type DOUBLE PRECISION necessary for scaled columns.
*    There is one variable per CAT data type.

      BYTE             IVALUB
      BYTE             IVALB
      INTEGER*2        IVALUW
      INTEGER*2        IVALW
      INTEGER          IVALI
      INTEGER*8        IVALK
      REAL             IVALR
      DOUBLE PRECISION IVALD
      LOGICAL          IVALL
      CHARACTER        IVALC*(CAT__SZVAL)
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the null value flag to indicate a non-null value.

         NULFLG = .FALSE.

*
*       Work out the FITS element from the CAT element.

         FELEM = ELEM

*
*       Set the FITS status, attempt to get a value and proceed if ok.

         FITSTT = FITOK
         CALL FTGCFL (FITUNT, COLNO, ROWNO, FELEM, 1, LVALL,
     :     FLGVAL, ANYF, FITSTT)

         IF (FITSTT .EQ. FITOK) THEN

*
*          Check that no null values have been obtained.

            IF (.NOT. ANYF) THEN

*
*             Check if the column is scaled.

               IF (.NOT. SFLAG) THEN

*
*                The column is not scaled; simply convert the value from
*                the FITS column to the appropriate data type.

                  CALL CAT1_TCNVT (CAT__TYPEL, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :              VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :              STATUS)

                  IF (.NOT. CONVOK) THEN
                     NULFLG = .TRUE.
                  END IF

               ELSE

*
*                The column is scaled.  First convert the value obtained
*                from the FITS table to DOUBLE PRECISION, then apply the
*                scaling and finally convert the value to the required
*                data type.

                  CALL CAT1_TCNVT (CAT__TYPEL, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              CAT__TYPED, IVALUB, IVALB, IVALUW, IVALW, IVALI,
     :              IVALK, IVALR, IVALD, IVALL, IVALC, CONVOK, STATUS)

                  IF (CONVOK) THEN
                     IVALD = (IVALD * SCALE) + ZERO

                     CALL CAT1_TCNVT (CAT__TYPED, IVALUB, IVALB, IVALUW,
     :                 IVALW, IVALI, IVALK, IVALR, IVALD, IVALL, IVALC,
     :                 DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :                 VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :                 STATUS)

                     IF (.NOT. CONVOK) THEN
                        NULFLG = .TRUE.
                     END IF

                  ELSE
                     NULFLG = .TRUE.
                  END IF
               END IF
            ELSE

*
*             A null value was obtained from FITSIO.

               NULFLG = .TRUE.

            END IF

         ELSE

*
*          FITSIO returned an error status.  Set the null flag, set
*          the return status and report an error.

            NULFLG = .TRUE.

            STATUS = CAT__ERROR

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

            CALL CAT3_FITER ('CAT3_FGCVL_ERR', ERRMSG(1 : ERRPOS),
     :        FITSTT, STATUS)
         END IF

      END IF

      END

* -- CHARACTER version ----------------------------------------------------
      SUBROUTINE CAT3_FGCVA (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :   ELEM, DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI, VALUEK,
     :   VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Arguments Given:
      LOGICAL
     :  SFLAG
      DOUBLE PRECISION
     :  SCALE,
     :  ZERO
      INTEGER
     :  FITUNT,
     :  COLNO,
     :  ROWNO,
     :  ELEM,
     :  DTYPE
*  Arguments Returned:
      BYTE             VALUEUB
      BYTE             VALUEB
      INTEGER*2        VALUEUW
      INTEGER*2        VALUEW
      INTEGER          VALUEI
      INTEGER*8        VALUEK
      REAL             VALUER
      DOUBLE PRECISION VALUED
      LOGICAL          VALUEL
      CHARACTER        VALUEC*(*)

      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Constants:
      INTEGER FITOK  ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:
      INTEGER
     :  FELEM,    ! Element of the FITS array to be accessed.
     :  FITSTT,   ! FITSIO status.
     :  ERRPOS    ! Current position in ERRMSG (excl. trail. blanks).
      LOGICAL
     :  FLGVAL,   ! Null value flag from FITSIO.
     :  ANYF,     !  "     "    "    "     "   .
     :  CONVOK    ! Flag; did data type conversion complete ok?
      CHARACTER
     :  ERRMSG*75 ! Text of error message.

*
*    These variables hold the local copy of the value obtained from the
*    FITS column.  There is one variable per CAT data type.

      BYTE             LVALUB
      BYTE             LVALB
      INTEGER*2        LVALUW
      INTEGER*2        LVALW
      INTEGER          LVALI
      INTEGER*8        LVALK
      REAL             LVALR
      DOUBLE PRECISION LVALD
      LOGICAL          LVALL
      CHARACTER        LVALC*(CAT__SZVAL)

*
*    These variables hold an intermediate copy of the value obtained
*    from the FITS column.  They are used in the extra conversion
*    to data type DOUBLE PRECISION necessary for scaled columns.
*    There is one variable per CAT data type.

      BYTE             IVALUB
      BYTE             IVALB
      INTEGER*2        IVALUW
      INTEGER*2        IVALW
      INTEGER          IVALI
      INTEGER*8        IVALK
      REAL             IVALR
      DOUBLE PRECISION IVALD
      LOGICAL          IVALL
      CHARACTER        IVALC*(CAT__SZVAL)
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the null value flag to indicate a non-null value.

         NULFLG = .FALSE.

*
*       Work out the FITS element from the CAT element.

         FELEM = ELEM

*
*       Set the FITS status, attempt to get a value and proceed if ok.

         FITSTT = FITOK
         CALL FTGCFS (FITUNT, COLNO, ROWNO, FELEM, 1, LVALC,
     :     FLGVAL, ANYF, FITSTT)

         IF (FITSTT .EQ. FITOK) THEN

*
*          Check that no null values have been obtained.

            IF (.NOT. ANYF) THEN

*
*             Check if the column is scaled.

               IF (.NOT. SFLAG) THEN

*
*                The column is not scaled; simply convert the value from
*                the FITS column to the appropriate data type.

                  CALL CAT1_TCNVT (CAT__TYPEC, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :              VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :              STATUS)

                  IF (.NOT. CONVOK) THEN
                     NULFLG = .TRUE.
                  END IF

               ELSE

*
*                The column is scaled.  First convert the value obtained
*                from the FITS table to DOUBLE PRECISION, then apply the
*                scaling and finally convert the value to the required
*                data type.

                  CALL CAT1_TCNVT (CAT__TYPEC, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              CAT__TYPED, IVALUB, IVALB, IVALUW, IVALW, IVALI,
     :              IVALK, IVALR, IVALD, IVALL, IVALC, CONVOK, STATUS)

                  IF (CONVOK) THEN
                     IVALD = (IVALD * SCALE) + ZERO

                     CALL CAT1_TCNVT (CAT__TYPED, IVALUB, IVALB, IVALUW,
     :                 IVALW, IVALI, IVALK, IVALR, IVALD, IVALL, IVALC,
     :                 DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :                 VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :                 STATUS)

                     IF (.NOT. CONVOK) THEN
                        NULFLG = .TRUE.
                     END IF

                  ELSE
                     NULFLG = .TRUE.
                  END IF
               END IF
            ELSE

*
*             A null value was obtained from FITSIO.

               NULFLG = .TRUE.

            END IF

         ELSE

*
*          FITSIO returned an error status.  Set the null flag, set
*          the return status and report an error.

            NULFLG = .TRUE.

            STATUS = CAT__ERROR

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

            CALL CAT3_FITER ('CAT3_FGCVC_ERR', ERRMSG(1 : ERRPOS),
     :        FITSTT, STATUS)
         END IF

      END IF

      END

* -- Complex REAL version --------------------------------------------------
      SUBROUTINE CAT3_FGCVC (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :   ELEM, DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI, VALUEK,
     :   VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Arguments Given:
      LOGICAL
     :  SFLAG
      DOUBLE PRECISION
     :  SCALE,
     :  ZERO
      INTEGER
     :  FITUNT,
     :  COLNO,
     :  ROWNO,
     :  ELEM,
     :  DTYPE
*  Arguments Returned:
      BYTE             VALUEUB
      BYTE             VALUEB
      INTEGER*2        VALUEUW
      INTEGER*2        VALUEW
      INTEGER          VALUEI
      INTEGER*8        VALUEK
      REAL             VALUER
      DOUBLE PRECISION VALUED
      LOGICAL          VALUEL
      CHARACTER        VALUEC*(*)

      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Constants:
      INTEGER FITOK  ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:
      INTEGER
     :  FELEM,    ! Element of the FITS array to be accessed.
     :  FITSTT,   ! FITSIO status.
     :  ERRPOS    ! Current position in ERRMSG (excl. trail. blanks).
      LOGICAL
     :  FLGVAL,   ! Null value flag from FITSIO.
     :  ANYF,     !  "     "    "    "     "   .
     :  CONVOK    ! Flag; did data type conversion complete ok?
      CHARACTER
     :  ERRMSG*75 ! Text of error message.

*
*    These variables hold the local copy of the value obtained from the
*    FITS column.  There is one variable per CAT data type.

      BYTE             LVALUB
      BYTE             LVALB
      INTEGER*2        LVALUW
      INTEGER*2        LVALW
      INTEGER          LVALI
      INTEGER*8        LVALK
      REAL             LVALR
      DOUBLE PRECISION LVALD
      LOGICAL          LVALL
      CHARACTER        LVALC*(CAT__SZVAL)

      COMPLEX          LVALCR
*
*    These variables hold an intermediate copy of the value obtained
*    from the FITS column.  They are used in the extra conversion
*    to data type DOUBLE PRECISION necessary for scaled columns.
*    There is one variable per CAT data type.

      BYTE             IVALUB
      BYTE             IVALB
      INTEGER*2        IVALUW
      INTEGER*2        IVALW
      INTEGER          IVALI
      INTEGER*8        IVALK
      REAL             IVALR
      DOUBLE PRECISION IVALD
      LOGICAL          IVALL
      CHARACTER        IVALC*(CAT__SZVAL)
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the null value flag to indicate a non-null value.

         NULFLG = .FALSE.

*
*       Work out the FITS element from the CAT element.  Remember that
*       a FITS complex single precision vector corresponds to a CAT
*       REAL vector with twice the number of elements.

         FELEM = (ELEM + 1) / 2

*
*       Set the FITS status, attempt to get a value and proceed if ok.

         FITSTT = FITOK
         CALL FTGCFC (FITUNT, COLNO, ROWNO, FELEM, 1, LVALCR,
     :     FLGVAL, ANYF, FITSTT)

         IF (FITSTT .EQ. FITOK) THEN
            IF (MOD(ELEM, 2) .EQ. 0) THEN
               LVALR = REAL(LVALCR)
            ELSE
               LVALR = AIMAG(LVALCR)
            END IF

*
*          Check that no null values have been obtained.

            IF (.NOT. ANYF) THEN

*
*             Check if the column is scaled.

               IF (.NOT. SFLAG) THEN

*
*                The column is not scaled; simply convert the value from
*                the FITS column to the appropriate data type.

                  CALL CAT1_TCNVT (CAT__TYPER, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :              VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :              STATUS)

                  IF (.NOT. CONVOK) THEN
                     NULFLG = .TRUE.
                  END IF

               ELSE

*
*                The column is scaled.  First convert the value obtained
*                from the FITS table to DOUBLE PRECISION, then apply the
*                scaling and finally convert the value to the required
*                data type.

                  CALL CAT1_TCNVT (CAT__TYPER, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              CAT__TYPED, IVALUB, IVALB, IVALUW, IVALW, IVALI,
     :              IVALK, IVALR, IVALD, IVALL, IVALC, CONVOK, STATUS)

                  IF (CONVOK) THEN
                     IVALD = (IVALD * SCALE) + ZERO

                     CALL CAT1_TCNVT (CAT__TYPED, IVALUB, IVALB, IVALUW,
     :                 IVALW, IVALI, IVALK, IVALR, IVALD, IVALL, IVALC,
     :                 DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :                 VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :                 STATUS)

                     IF (.NOT. CONVOK) THEN
                        NULFLG = .TRUE.
                     END IF

                  ELSE
                     NULFLG = .TRUE.
                  END IF
               END IF
            ELSE

*
*             A null value was obtained from FITSIO.

               NULFLG = .TRUE.

            END IF

         ELSE

*
*          FITSIO returned an error status.  Set the null flag, set
*          the return status and report an error.

            NULFLG = .TRUE.

            STATUS = CAT__ERROR

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

            CALL CAT3_FITER ('CAT3_FGCVC_ERR', ERRMSG(1 : ERRPOS),
     :        FITSTT, STATUS)
         END IF

      END IF

      END

* -- Complex DOUBLE PRECISION version ----------------------------------------
      SUBROUTINE CAT3_FGCVM (SFLAG, SCALE, ZERO, FITUNT, COLNO, ROWNO,
     :   ELEM, DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI, VALUEK,
     :   VALUER, VALUED, VALUEL, VALUEC, NULFLG, STATUS)
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT1_NUL'          ! CAT null values.
*  Arguments Given:
      LOGICAL
     :  SFLAG
      DOUBLE PRECISION
     :  SCALE,
     :  ZERO
      INTEGER
     :  FITUNT,
     :  COLNO,
     :  ROWNO,
     :  ELEM,
     :  DTYPE
*  Arguments Returned:
      BYTE             VALUEUB
      BYTE             VALUEB
      INTEGER*2        VALUEUW
      INTEGER*2        VALUEW
      INTEGER          VALUEI
      INTEGER*8        VALUEK
      REAL             VALUER
      DOUBLE PRECISION VALUED
      LOGICAL          VALUEL
      CHARACTER        VALUEC*(*)

      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Constants:
      INTEGER FITOK  ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:
      INTEGER
     :  FELEM,    ! Element of the FITS array to be accessed.
     :  FITSTT,   ! FITSIO status.
     :  ERRPOS    ! Current position in ERRMSG (excl. trail. blanks).
      LOGICAL
     :  FLGVAL,   ! Null value flag from FITSIO.
     :  ANYF,     !  "     "    "    "     "   .
     :  CONVOK    ! Flag; did data type conversion complete ok?
      CHARACTER
     :  ERRMSG*75 ! Text of error message.

*
*    These variables hold the local copy of the value obtained from the
*    FITS column.  There is one variable per CAT data type.

      BYTE             LVALUB
      BYTE             LVALB
      INTEGER*2        LVALUW
      INTEGER*2        LVALW
      INTEGER          LVALI
      INTEGER*8        LVALK
      REAL             LVALR
      DOUBLE PRECISION LVALD
      LOGICAL          LVALL
      CHARACTER        LVALC*(CAT__SZVAL)

      DOUBLE COMPLEX   LVALMD

*
*    These variables hold an intermediate copy of the value obtained
*    from the FITS column.  They are used in the extra conversion
*    to data type DOUBLE PRECISION necessary for scaled columns.
*    There is one variable per CAT data type.

      BYTE             IVALUB
      BYTE             IVALB
      INTEGER*2        IVALUW
      INTEGER*2        IVALW
      INTEGER          IVALI
      INTEGER*8        IVALK
      REAL             IVALR
      DOUBLE PRECISION IVALD
      LOGICAL          IVALL
      CHARACTER        IVALC*(CAT__SZVAL)
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the null value flag to indicate a non-null value.

         NULFLG = .FALSE.

*
*       Work out the FITS element from the CAT element.  Remember that
*       a FITS complex single precision vector corresponds to a CAT
*       REAL vector with twice the number of elements.

         FELEM = (ELEM + 1) / 2

*
*       Set the FITS status, attempt to get a value and proceed if ok.

         FITSTT = FITOK
         CALL FTGCFM (FITUNT, COLNO, ROWNO, FELEM, 1, LVALMD,
     :     FLGVAL, ANYF, FITSTT)

         IF (FITSTT .EQ. FITOK) THEN
            IF (MOD(ELEM, 2) .EQ. 0) THEN
               LVALD = DBLE(LVALMD)
            ELSE
               LVALD = DIMAG(LVALMD)
            END IF

*
*          Check that no null values have been obtained.

            IF (.NOT. ANYF) THEN

*
*             Check if the column is scaled.

               IF (.NOT. SFLAG) THEN

*
*                The column is not scaled; simply convert the value from
*                the FITS column to the appropriate data type.

                  CALL CAT1_TCNVT (CAT__TYPED, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :              VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :              STATUS)

                  IF (.NOT. CONVOK) THEN
                     NULFLG = .TRUE.
                  END IF

               ELSE

*
*                The column is scaled.  First convert the value obtained
*                from the FITS table to DOUBLE PRECISION, then apply the
*                scaling and finally convert the value to the required
*                data type.

                  CALL CAT1_TCNVT (CAT__TYPED, LVALUB, LVALB, LVALUW,
     :              LVALW, LVALI, LVALK, LVALR, LVALD, LVALL, LVALC,
     :              CAT__TYPED, IVALUB, IVALB, IVALUW, IVALW, IVALI,
     :              IVALK, IVALR, IVALD, IVALL, IVALC, CONVOK, STATUS)

                  IF (CONVOK) THEN
                     IVALD = (IVALD * SCALE) + ZERO

                     CALL CAT1_TCNVT (CAT__TYPED, IVALUB, IVALB, IVALUW,
     :                 IVALW, IVALI, IVALK, IVALR, IVALD, IVALL, IVALC,
     :                 DTYPE, VALUEUB, VALUEB, VALUEUW, VALUEW, VALUEI,
     :                 VALUEK, VALUER, VALUED, VALUEL, VALUEC, CONVOK,
     :                 STATUS)

                     IF (.NOT. CONVOK) THEN
                        NULFLG = .TRUE.
                     END IF

                  ELSE
                     NULFLG = .TRUE.
                  END IF
               END IF
            ELSE

*
*             A null value was obtained from FITSIO.

               NULFLG = .TRUE.

            END IF

         ELSE

*
*          FITSIO returned an error status.  Set the null flag, set
*          the return status and report an error.

            NULFLG = .TRUE.

            STATUS = CAT__ERROR

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

            CALL CAT3_FITER ('CAT3_FGCVM_ERR', ERRMSG(1 : ERRPOS),
     :        FITSTT, STATUS)
         END IF

      END IF

      END
