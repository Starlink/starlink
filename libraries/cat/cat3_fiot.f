      SUBROUTINE CAT3_FIOB (IOFLG, CIELM, FI, ELEM, ROWNO, VALUE,
     :  NULFLG, STATUS)
*+
*  Name:
*     CAT3_FIOB
*  Purpose:
*     Get or put a value for a single field.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_FIOB (IOFLG, CIELM, FI, ELEM, ROWNO; VALUE, NULFLG;
*       STATUS)
*  Description:
*     Get or put a value for a single field.
*  Arguments:
*     IOFLG  =  LOGICAL (Given)
*        Flag indicating whether the routine is to get or put a value,
*        coded as follows:
*        .TRUE.  - get a value,
*        .FALSE. - put a value.
*     CIELM  =  INTEGER (Given)
*        Array element in the catalogue common blocks corresponding to
*        the catalogue to be accessed.
*     FI  =  INTEGER (Given)
*        Identifier to the field to be put or got.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be put or got.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUE  =  BYTE (ENTRY or EXIT)
*        Value to be put or obtained.  The value need not be of the
*        same data type as the field.  Type conversions are performed if
*        necessary.
*     NULFLG  =  LOGICAL (ENTRY or EXIT)
*        Flag indicating whether or not the value is null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine: the unit number for accessing the FITS table, the
*     FITS data type of the column, the scaled flag and the scale
*     factor and zero point.
*     Attempt to determine the column number.
*     If ok then
*       If a value is to be got then
*         Get a value from the FITS table.
*       else
*         Put a value to the FITS table.
*       end if
*     end if
*  Implementation Deficiencies:
*     This version kludges the handling of null values.  The appropriate
*     Null value should be obtained from the column attributes.
*
*     Also, unsigned byte and unsigned word data types are not
*     supported.
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
*     29/7/93 (ACD): Original version.
*     8/10/93 (ACD): First stable version.
*     24/1/94 (ACD): Modified error reporting.
*     30/1/94 (ACD): Implemented null values in putting values.
*     2/2/94  (ACD): Added handling of vector column elements.
*     5/7/94  (ACD): Added proper handling of scaled FITS columns.
*     4/6/98  (ACD): Speeded up determining column details.
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
      INCLUDE 'CAT3_FIT_CMN'      ! FITS back-end common block.
*  Arguments Given:
      LOGICAL
     :  IOFLG
      INTEGER
     :  CIELM,
     :  FI,
     :  ELEM,
     :  ROWNO
*  Arguments Given and Returned:
      BYTE
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS        ! Global status
*  Local Variables:
      INTEGER
     :  FITUNT,   ! Fortran unit no. for accessing FITS file.
     :  COLNO,    ! Number of column to be accessed.
     :  FITYPE    ! FITS data type of the current column.
      LOGICAL
     :  SFLAG     ! Flag: is the current column scaled?
      DOUBLE PRECISION
     :  SCALE,    ! Scale factor for a scaled column.
     :  ZERO      ! Zero point for a scaled column.

*
*    The following variables hold a local copy of the value being got or
*    put.  They are needed because FIOB uses a passed length
*    character argument, whereas GVALB and PVALB necessarily use
*    a fixed length character argument.

      BYTE      LVALB
      INTEGER*2 LVALW
      INTEGER   LVALI
      REAL      LVALR
      DOUBLE PRECISION LVALD
      LOGICAL   LVALL
      CHARACTER LVALC*(CAT__SZVAL)

*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the unit number for the FITS file.

         FITUNT = FUNT__CAT3(CIELM)

*
*       Attempt to determine: the number of the column, the FITS data type
*       of the column, the scaled flag and the scale factor and zero point.

         IF (FI .GT. 0  .AND.  FI .LE. NIDS__CAT1) THEN
            COLNO = COLNO__CAT3(FI)
            FITYPE = FTYPE__CAT3(FI)
            SFLAG = SFLAG__CAT3(FI)
            SCALE = SCALE__CAT3(FI)
            ZERO = ZERO__CAT3(FI)
         ELSE
            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT3_FIOB_NCP', 'Error getting '/
     :        /'or putting field value.', STATUS)

         END IF

         IF (STATUS .EQ. CAT__OK) THEN
            IF (IOFLG) THEN

*
*             Get a value from the FITS table.

               CALL CAT3_GVALB (FITYPE, SFLAG, SCALE, ZERO, FITUNT,
     :           COLNO, ELEM, ROWNO, LVALB, NULFLG, STATUS)

               VALUE = LVALB

            ELSE

*
*             Put a value to the FITS table.

               LVALB = VALUE

               CALL CAT3_PVALB (FITYPE, FITUNT, COLNO, ELEM, ROWNO,
     :           LVALB, NULFLG, STATUS)
            END IF

         END IF

      END IF

      END
      SUBROUTINE CAT3_FIOC (IOFLG, CIELM, FI, ELEM, ROWNO, VALUE,
     :  NULFLG, STATUS)
*+
*  Name:
*     CAT3_FIOC
*  Purpose:
*     Get or put a value for a single field.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_FIOC (IOFLG, CIELM, FI, ELEM, ROWNO; VALUE, NULFLG;
*       STATUS)
*  Description:
*     Get or put a value for a single field.
*  Arguments:
*     IOFLG  =  LOGICAL (Given)
*        Flag indicating whether the routine is to get or put a value,
*        coded as follows:
*        .TRUE.  - get a value,
*        .FALSE. - put a value.
*     CIELM  =  INTEGER (Given)
*        Array element in the catalogue common blocks corresponding to
*        the catalogue to be accessed.
*     FI  =  INTEGER (Given)
*        Identifier to the field to be put or got.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be put or got.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUE  =  CHARACTER*(*) (ENTRY or EXIT)
*        Value to be put or obtained.  The value need not be of the
*        same data type as the field.  Type conversions are performed if
*        necessary.
*     NULFLG  =  LOGICAL (ENTRY or EXIT)
*        Flag indicating whether or not the value is null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine: the unit number for accessing the FITS table, the
*     FITS data type of the column, the scaled flag and the scale
*     factor and zero point.
*     Attempt to determine the column number.
*     If ok then
*       If a value is to be got then
*         Get a value from the FITS table.
*       else
*         Put a value to the FITS table.
*       end if
*     end if
*  Implementation Deficiencies:
*     This version kludges the handling of null values.  The appropriate
*     Null value should be obtained from the column attributes.
*
*     Also, unsigned byte and unsigned word data types are not
*     supported.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     29/7/93 (ACD): Original version.
*     8/10/93 (ACD): First stable version.
*     24/1/94 (ACD): Modified error reporting.
*     30/1/94 (ACD): Implemented null values in putting values.
*     2/2/94  (ACD): Added handling of vector column elements.
*     5/7/94  (ACD): Added proper handling of scaled FITS columns.
*     4/6/98  (ACD): Speeded up determining column details.
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
      INCLUDE 'CAT3_FIT_CMN'      ! FITS back-end common block.
*  Arguments Given:
      LOGICAL
     :  IOFLG
      INTEGER
     :  CIELM,
     :  FI,
     :  ELEM,
     :  ROWNO
*  Arguments Given and Returned:
      CHARACTER*(*)
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS        ! Global status
*  Local Variables:
      INTEGER
     :  FITUNT,   ! Fortran unit no. for accessing FITS file.
     :  COLNO,    ! Number of column to be accessed.
     :  FITYPE    ! FITS data type of the current column.
      LOGICAL
     :  SFLAG     ! Flag: is the current column scaled?
      DOUBLE PRECISION
     :  SCALE,    ! Scale factor for a scaled column.
     :  ZERO      ! Zero point for a scaled column.

*
*    The following variables hold a local copy of the value being got or
*    put.  They are needed because FIOC uses a passed length
*    character argument, whereas GVALC and PVALC necessarily use
*    a fixed length character argument.

      BYTE      LVALB
      INTEGER*2 LVALW
      INTEGER   LVALI
      REAL      LVALR
      DOUBLE PRECISION LVALD
      LOGICAL   LVALL
      CHARACTER LVALC*(CAT__SZVAL)

*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the unit number for the FITS file.

         FITUNT = FUNT__CAT3(CIELM)

*
*       Attempt to determine: the number of the column, the FITS data type
*       of the column, the scaled flag and the scale factor and zero point.

         IF (FI .GT. 0  .AND.  FI .LE. NIDS__CAT1) THEN
            COLNO = COLNO__CAT3(FI)
            FITYPE = FTYPE__CAT3(FI)
            SFLAG = SFLAG__CAT3(FI)
            SCALE = SCALE__CAT3(FI)
            ZERO = ZERO__CAT3(FI)
         ELSE
            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT3_FIOC_NCP', 'Error getting '/
     :        /'or putting field value.', STATUS)

         END IF

         IF (STATUS .EQ. CAT__OK) THEN
            IF (IOFLG) THEN

*
*             Get a value from the FITS table.

               CALL CAT3_GVALC (FITYPE, SFLAG, SCALE, ZERO, FITUNT,
     :           COLNO, ELEM, ROWNO, LVALC, NULFLG, STATUS)

               VALUE = LVALC

            ELSE

*
*             Put a value to the FITS table.

               LVALC = VALUE

               CALL CAT3_PVALC (FITYPE, FITUNT, COLNO, ELEM, ROWNO,
     :           LVALC, NULFLG, STATUS)
            END IF

         END IF

      END IF

      END
      SUBROUTINE CAT3_FIOD (IOFLG, CIELM, FI, ELEM, ROWNO, VALUE,
     :  NULFLG, STATUS)
*+
*  Name:
*     CAT3_FIOD
*  Purpose:
*     Get or put a value for a single field.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_FIOD (IOFLG, CIELM, FI, ELEM, ROWNO; VALUE, NULFLG;
*       STATUS)
*  Description:
*     Get or put a value for a single field.
*  Arguments:
*     IOFLG  =  LOGICAL (Given)
*        Flag indicating whether the routine is to get or put a value,
*        coded as follows:
*        .TRUE.  - get a value,
*        .FALSE. - put a value.
*     CIELM  =  INTEGER (Given)
*        Array element in the catalogue common blocks corresponding to
*        the catalogue to be accessed.
*     FI  =  INTEGER (Given)
*        Identifier to the field to be put or got.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be put or got.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUE  =  DOUBLE PRECISION (ENTRY or EXIT)
*        Value to be put or obtained.  The value need not be of the
*        same data type as the field.  Type conversions are performed if
*        necessary.
*     NULFLG  =  LOGICAL (ENTRY or EXIT)
*        Flag indicating whether or not the value is null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine: the unit number for accessing the FITS table, the
*     FITS data type of the column, the scaled flag and the scale
*     factor and zero point.
*     Attempt to determine the column number.
*     If ok then
*       If a value is to be got then
*         Get a value from the FITS table.
*       else
*         Put a value to the FITS table.
*       end if
*     end if
*  Implementation Deficiencies:
*     This version kludges the handling of null values.  The appropriate
*     Null value should be obtained from the column attributes.
*
*     Also, unsigned byte and unsigned word data types are not
*     supported.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     29/7/93 (ACD): Original version.
*     8/10/93 (ACD): First stable version.
*     24/1/94 (ACD): Modified error reporting.
*     30/1/94 (ACD): Implemented null values in putting values.
*     2/2/94  (ACD): Added handling of vector column elements.
*     5/7/94  (ACD): Added proper handling of scaled FITS columns.
*     4/6/98  (ACD): Speeded up determining column details.
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
      INCLUDE 'CAT3_FIT_CMN'      ! FITS back-end common block.
*  Arguments Given:
      LOGICAL
     :  IOFLG
      INTEGER
     :  CIELM,
     :  FI,
     :  ELEM,
     :  ROWNO
*  Arguments Given and Returned:
      DOUBLE PRECISION
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS        ! Global status
*  Local Variables:
      INTEGER
     :  FITUNT,   ! Fortran unit no. for accessing FITS file.
     :  COLNO,    ! Number of column to be accessed.
     :  FITYPE    ! FITS data type of the current column.
      LOGICAL
     :  SFLAG     ! Flag: is the current column scaled?
      DOUBLE PRECISION
     :  SCALE,    ! Scale factor for a scaled column.
     :  ZERO      ! Zero point for a scaled column.

*
*    The following variables hold a local copy of the value being got or
*    put.  They are needed because FIOD uses a passed length
*    character argument, whereas GVALD and PVALD necessarily use
*    a fixed length character argument.

      BYTE      LVALB
      INTEGER*2 LVALW
      INTEGER   LVALI
      REAL      LVALR
      DOUBLE PRECISION LVALD
      LOGICAL   LVALL
      CHARACTER LVALC*(CAT__SZVAL)

*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the unit number for the FITS file.

         FITUNT = FUNT__CAT3(CIELM)

*
*       Attempt to determine: the number of the column, the FITS data type
*       of the column, the scaled flag and the scale factor and zero point.

         IF (FI .GT. 0  .AND.  FI .LE. NIDS__CAT1) THEN
            COLNO = COLNO__CAT3(FI)
            FITYPE = FTYPE__CAT3(FI)
            SFLAG = SFLAG__CAT3(FI)
            SCALE = SCALE__CAT3(FI)
            ZERO = ZERO__CAT3(FI)
         ELSE
            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT3_FIOD_NCP', 'Error getting '/
     :        /'or putting field value.', STATUS)

         END IF

         IF (STATUS .EQ. CAT__OK) THEN
            IF (IOFLG) THEN

*
*             Get a value from the FITS table.

               CALL CAT3_GVALD (FITYPE, SFLAG, SCALE, ZERO, FITUNT,
     :           COLNO, ELEM, ROWNO, LVALD, NULFLG, STATUS)

               VALUE = LVALD

            ELSE

*
*             Put a value to the FITS table.

               LVALD = VALUE

               CALL CAT3_PVALD (FITYPE, FITUNT, COLNO, ELEM, ROWNO,
     :           LVALD, NULFLG, STATUS)
            END IF

         END IF

      END IF

      END
      SUBROUTINE CAT3_FIOI (IOFLG, CIELM, FI, ELEM, ROWNO, VALUE,
     :  NULFLG, STATUS)
*+
*  Name:
*     CAT3_FIOI
*  Purpose:
*     Get or put a value for a single field.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_FIOI (IOFLG, CIELM, FI, ELEM, ROWNO; VALUE, NULFLG;
*       STATUS)
*  Description:
*     Get or put a value for a single field.
*  Arguments:
*     IOFLG  =  LOGICAL (Given)
*        Flag indicating whether the routine is to get or put a value,
*        coded as follows:
*        .TRUE.  - get a value,
*        .FALSE. - put a value.
*     CIELM  =  INTEGER (Given)
*        Array element in the catalogue common blocks corresponding to
*        the catalogue to be accessed.
*     FI  =  INTEGER (Given)
*        Identifier to the field to be put or got.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be put or got.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUE  =  INTEGER (ENTRY or EXIT)
*        Value to be put or obtained.  The value need not be of the
*        same data type as the field.  Type conversions are performed if
*        necessary.
*     NULFLG  =  LOGICAL (ENTRY or EXIT)
*        Flag indicating whether or not the value is null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine: the unit number for accessing the FITS table, the
*     FITS data type of the column, the scaled flag and the scale
*     factor and zero point.
*     Attempt to determine the column number.
*     If ok then
*       If a value is to be got then
*         Get a value from the FITS table.
*       else
*         Put a value to the FITS table.
*       end if
*     end if
*  Implementation Deficiencies:
*     This version kludges the handling of null values.  The appropriate
*     Null value should be obtained from the column attributes.
*
*     Also, unsigned byte and unsigned word data types are not
*     supported.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     29/7/93 (ACD): Original version.
*     8/10/93 (ACD): First stable version.
*     24/1/94 (ACD): Modified error reporting.
*     30/1/94 (ACD): Implemented null values in putting values.
*     2/2/94  (ACD): Added handling of vector column elements.
*     5/7/94  (ACD): Added proper handling of scaled FITS columns.
*     4/6/98  (ACD): Speeded up determining column details.
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
      INCLUDE 'CAT3_FIT_CMN'      ! FITS back-end common block.
*  Arguments Given:
      LOGICAL
     :  IOFLG
      INTEGER
     :  CIELM,
     :  FI,
     :  ELEM,
     :  ROWNO
*  Arguments Given and Returned:
      INTEGER
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS        ! Global status
*  Local Variables:
      INTEGER
     :  FITUNT,   ! Fortran unit no. for accessing FITS file.
     :  COLNO,    ! Number of column to be accessed.
     :  FITYPE    ! FITS data type of the current column.
      LOGICAL
     :  SFLAG     ! Flag: is the current column scaled?
      DOUBLE PRECISION
     :  SCALE,    ! Scale factor for a scaled column.
     :  ZERO      ! Zero point for a scaled column.

*
*    The following variables hold a local copy of the value being got or
*    put.  They are needed because FIOI uses a passed length
*    character argument, whereas GVALI and PVALI necessarily use
*    a fixed length character argument.

      BYTE      LVALB
      INTEGER*2 LVALW
      INTEGER   LVALI
      REAL      LVALR
      DOUBLE PRECISION LVALD
      LOGICAL   LVALL
      CHARACTER LVALC*(CAT__SZVAL)

*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the unit number for the FITS file.

         FITUNT = FUNT__CAT3(CIELM)

*
*       Attempt to determine: the number of the column, the FITS data type
*       of the column, the scaled flag and the scale factor and zero point.

         IF (FI .GT. 0  .AND.  FI .LE. NIDS__CAT1) THEN
            COLNO = COLNO__CAT3(FI)
            FITYPE = FTYPE__CAT3(FI)
            SFLAG = SFLAG__CAT3(FI)
            SCALE = SCALE__CAT3(FI)
            ZERO = ZERO__CAT3(FI)
         ELSE
            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT3_FIOI_NCP', 'Error getting '/
     :        /'or putting field value.', STATUS)

         END IF

         IF (STATUS .EQ. CAT__OK) THEN
            IF (IOFLG) THEN

*
*             Get a value from the FITS table.

               CALL CAT3_GVALI (FITYPE, SFLAG, SCALE, ZERO, FITUNT,
     :           COLNO, ELEM, ROWNO, LVALI, NULFLG, STATUS)

               VALUE = LVALI

            ELSE

*
*             Put a value to the FITS table.

               LVALI = VALUE

               CALL CAT3_PVALI (FITYPE, FITUNT, COLNO, ELEM, ROWNO,
     :           LVALI, NULFLG, STATUS)
            END IF

         END IF

      END IF

      END
      SUBROUTINE CAT3_FIOL (IOFLG, CIELM, FI, ELEM, ROWNO, VALUE,
     :  NULFLG, STATUS)
*+
*  Name:
*     CAT3_FIOL
*  Purpose:
*     Get or put a value for a single field.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_FIOL (IOFLG, CIELM, FI, ELEM, ROWNO; VALUE, NULFLG;
*       STATUS)
*  Description:
*     Get or put a value for a single field.
*  Arguments:
*     IOFLG  =  LOGICAL (Given)
*        Flag indicating whether the routine is to get or put a value,
*        coded as follows:
*        .TRUE.  - get a value,
*        .FALSE. - put a value.
*     CIELM  =  INTEGER (Given)
*        Array element in the catalogue common blocks corresponding to
*        the catalogue to be accessed.
*     FI  =  INTEGER (Given)
*        Identifier to the field to be put or got.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be put or got.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUE  =  LOGICAL (ENTRY or EXIT)
*        Value to be put or obtained.  The value need not be of the
*        same data type as the field.  Type conversions are performed if
*        necessary.
*     NULFLG  =  LOGICAL (ENTRY or EXIT)
*        Flag indicating whether or not the value is null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine: the unit number for accessing the FITS table, the
*     FITS data type of the column, the scaled flag and the scale
*     factor and zero point.
*     Attempt to determine the column number.
*     If ok then
*       If a value is to be got then
*         Get a value from the FITS table.
*       else
*         Put a value to the FITS table.
*       end if
*     end if
*  Implementation Deficiencies:
*     This version kludges the handling of null values.  The appropriate
*     Null value should be obtained from the column attributes.
*
*     Also, unsigned byte and unsigned word data types are not
*     supported.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     29/7/93 (ACD): Original version.
*     8/10/93 (ACD): First stable version.
*     24/1/94 (ACD): Modified error reporting.
*     30/1/94 (ACD): Implemented null values in putting values.
*     2/2/94  (ACD): Added handling of vector column elements.
*     5/7/94  (ACD): Added proper handling of scaled FITS columns.
*     4/6/98  (ACD): Speeded up determining column details.
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
      INCLUDE 'CAT3_FIT_CMN'      ! FITS back-end common block.
*  Arguments Given:
      LOGICAL
     :  IOFLG
      INTEGER
     :  CIELM,
     :  FI,
     :  ELEM,
     :  ROWNO
*  Arguments Given and Returned:
      LOGICAL
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS        ! Global status
*  Local Variables:
      INTEGER
     :  FITUNT,   ! Fortran unit no. for accessing FITS file.
     :  COLNO,    ! Number of column to be accessed.
     :  FITYPE    ! FITS data type of the current column.
      LOGICAL
     :  SFLAG     ! Flag: is the current column scaled?
      DOUBLE PRECISION
     :  SCALE,    ! Scale factor for a scaled column.
     :  ZERO      ! Zero point for a scaled column.

*
*    The following variables hold a local copy of the value being got or
*    put.  They are needed because FIOL uses a passed length
*    character argument, whereas GVALL and PVALL necessarily use
*    a fixed length character argument.

      BYTE      LVALB
      INTEGER*2 LVALW
      INTEGER   LVALI
      REAL      LVALR
      DOUBLE PRECISION LVALD
      LOGICAL   LVALL
      CHARACTER LVALC*(CAT__SZVAL)

*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the unit number for the FITS file.

         FITUNT = FUNT__CAT3(CIELM)

*
*       Attempt to determine: the number of the column, the FITS data type
*       of the column, the scaled flag and the scale factor and zero point.

         IF (FI .GT. 0  .AND.  FI .LE. NIDS__CAT1) THEN
            COLNO = COLNO__CAT3(FI)
            FITYPE = FTYPE__CAT3(FI)
            SFLAG = SFLAG__CAT3(FI)
            SCALE = SCALE__CAT3(FI)
            ZERO = ZERO__CAT3(FI)
         ELSE
            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT3_FIOL_NCP', 'Error getting '/
     :        /'or putting field value.', STATUS)

         END IF

         IF (STATUS .EQ. CAT__OK) THEN
            IF (IOFLG) THEN

*
*             Get a value from the FITS table.

               CALL CAT3_GVALL (FITYPE, SFLAG, SCALE, ZERO, FITUNT,
     :           COLNO, ELEM, ROWNO, LVALL, NULFLG, STATUS)

               VALUE = LVALL

            ELSE

*
*             Put a value to the FITS table.

               LVALL = VALUE

               CALL CAT3_PVALL (FITYPE, FITUNT, COLNO, ELEM, ROWNO,
     :           LVALL, NULFLG, STATUS)
            END IF

         END IF

      END IF

      END
      SUBROUTINE CAT3_FIOR (IOFLG, CIELM, FI, ELEM, ROWNO, VALUE,
     :  NULFLG, STATUS)
*+
*  Name:
*     CAT3_FIOR
*  Purpose:
*     Get or put a value for a single field.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_FIOR (IOFLG, CIELM, FI, ELEM, ROWNO; VALUE, NULFLG;
*       STATUS)
*  Description:
*     Get or put a value for a single field.
*  Arguments:
*     IOFLG  =  LOGICAL (Given)
*        Flag indicating whether the routine is to get or put a value,
*        coded as follows:
*        .TRUE.  - get a value,
*        .FALSE. - put a value.
*     CIELM  =  INTEGER (Given)
*        Array element in the catalogue common blocks corresponding to
*        the catalogue to be accessed.
*     FI  =  INTEGER (Given)
*        Identifier to the field to be put or got.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be put or got.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUE  =  REAL (ENTRY or EXIT)
*        Value to be put or obtained.  The value need not be of the
*        same data type as the field.  Type conversions are performed if
*        necessary.
*     NULFLG  =  LOGICAL (ENTRY or EXIT)
*        Flag indicating whether or not the value is null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine: the unit number for accessing the FITS table, the
*     FITS data type of the column, the scaled flag and the scale
*     factor and zero point.
*     Attempt to determine the column number.
*     If ok then
*       If a value is to be got then
*         Get a value from the FITS table.
*       else
*         Put a value to the FITS table.
*       end if
*     end if
*  Implementation Deficiencies:
*     This version kludges the handling of null values.  The appropriate
*     Null value should be obtained from the column attributes.
*
*     Also, unsigned byte and unsigned word data types are not
*     supported.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     29/7/93 (ACD): Original version.
*     8/10/93 (ACD): First stable version.
*     24/1/94 (ACD): Modified error reporting.
*     30/1/94 (ACD): Implemented null values in putting values.
*     2/2/94  (ACD): Added handling of vector column elements.
*     5/7/94  (ACD): Added proper handling of scaled FITS columns.
*     4/6/98  (ACD): Speeded up determining column details.
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
      INCLUDE 'CAT3_FIT_CMN'      ! FITS back-end common block.
*  Arguments Given:
      LOGICAL
     :  IOFLG
      INTEGER
     :  CIELM,
     :  FI,
     :  ELEM,
     :  ROWNO
*  Arguments Given and Returned:
      REAL
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS        ! Global status
*  Local Variables:
      INTEGER
     :  FITUNT,   ! Fortran unit no. for accessing FITS file.
     :  COLNO,    ! Number of column to be accessed.
     :  FITYPE    ! FITS data type of the current column.
      LOGICAL
     :  SFLAG     ! Flag: is the current column scaled?
      DOUBLE PRECISION
     :  SCALE,    ! Scale factor for a scaled column.
     :  ZERO      ! Zero point for a scaled column.

*
*    The following variables hold a local copy of the value being got or
*    put.  They are needed because FIOR uses a passed length
*    character argument, whereas GVALR and PVALR necessarily use
*    a fixed length character argument.

      BYTE      LVALB
      INTEGER*2 LVALW
      INTEGER   LVALI
      REAL      LVALR
      DOUBLE PRECISION LVALD
      LOGICAL   LVALL
      CHARACTER LVALC*(CAT__SZVAL)

*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the unit number for the FITS file.

         FITUNT = FUNT__CAT3(CIELM)

*
*       Attempt to determine: the number of the column, the FITS data type
*       of the column, the scaled flag and the scale factor and zero point.

         IF (FI .GT. 0  .AND.  FI .LE. NIDS__CAT1) THEN
            COLNO = COLNO__CAT3(FI)
            FITYPE = FTYPE__CAT3(FI)
            SFLAG = SFLAG__CAT3(FI)
            SCALE = SCALE__CAT3(FI)
            ZERO = ZERO__CAT3(FI)
         ELSE
            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT3_FIOR_NCP', 'Error getting '/
     :        /'or putting field value.', STATUS)

         END IF

         IF (STATUS .EQ. CAT__OK) THEN
            IF (IOFLG) THEN

*
*             Get a value from the FITS table.

               CALL CAT3_GVALR (FITYPE, SFLAG, SCALE, ZERO, FITUNT,
     :           COLNO, ELEM, ROWNO, LVALR, NULFLG, STATUS)

               VALUE = LVALR

            ELSE

*
*             Put a value to the FITS table.

               LVALR = VALUE

               CALL CAT3_PVALR (FITYPE, FITUNT, COLNO, ELEM, ROWNO,
     :           LVALR, NULFLG, STATUS)
            END IF

         END IF

      END IF

      END
      SUBROUTINE CAT3_FIOW (IOFLG, CIELM, FI, ELEM, ROWNO, VALUE,
     :  NULFLG, STATUS)
*+
*  Name:
*     CAT3_FIOW
*  Purpose:
*     Get or put a value for a single field.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_FIOW (IOFLG, CIELM, FI, ELEM, ROWNO; VALUE, NULFLG;
*       STATUS)
*  Description:
*     Get or put a value for a single field.
*  Arguments:
*     IOFLG  =  LOGICAL (Given)
*        Flag indicating whether the routine is to get or put a value,
*        coded as follows:
*        .TRUE.  - get a value,
*        .FALSE. - put a value.
*     CIELM  =  INTEGER (Given)
*        Array element in the catalogue common blocks corresponding to
*        the catalogue to be accessed.
*     FI  =  INTEGER (Given)
*        Identifier to the field to be put or got.
*     ELEM  =  INTEGER (Given)
*        Element of the array which is to be put or got.
*     ROWNO  =  INTEGER (Given)
*        Number of the row in which a field is to be accessed.
*     VALUE  =  INTEGER*2 (ENTRY or EXIT)
*        Value to be put or obtained.  The value need not be of the
*        same data type as the field.  Type conversions are performed if
*        necessary.
*     NULFLG  =  LOGICAL (ENTRY or EXIT)
*        Flag indicating whether or not the value is null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine: the unit number for accessing the FITS table, the
*     FITS data type of the column, the scaled flag and the scale
*     factor and zero point.
*     Attempt to determine the column number.
*     If ok then
*       If a value is to be got then
*         Get a value from the FITS table.
*       else
*         Put a value to the FITS table.
*       end if
*     end if
*  Implementation Deficiencies:
*     This version kludges the handling of null values.  The appropriate
*     Null value should be obtained from the column attributes.
*
*     Also, unsigned byte and unsigned word data types are not
*     supported.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     29/7/93 (ACD): Original version.
*     8/10/93 (ACD): First stable version.
*     24/1/94 (ACD): Modified error reporting.
*     30/1/94 (ACD): Implemented null values in putting values.
*     2/2/94  (ACD): Added handling of vector column elements.
*     5/7/94  (ACD): Added proper handling of scaled FITS columns.
*     4/6/98  (ACD): Speeded up determining column details.
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
      INCLUDE 'CAT3_FIT_CMN'      ! FITS back-end common block.
*  Arguments Given:
      LOGICAL
     :  IOFLG
      INTEGER
     :  CIELM,
     :  FI,
     :  ELEM,
     :  ROWNO
*  Arguments Given and Returned:
      INTEGER*2
     :  VALUE
      LOGICAL
     :  NULFLG
*  Status:
      INTEGER STATUS        ! Global status
*  Local Variables:
      INTEGER
     :  FITUNT,   ! Fortran unit no. for accessing FITS file.
     :  COLNO,    ! Number of column to be accessed.
     :  FITYPE    ! FITS data type of the current column.
      LOGICAL
     :  SFLAG     ! Flag: is the current column scaled?
      DOUBLE PRECISION
     :  SCALE,    ! Scale factor for a scaled column.
     :  ZERO      ! Zero point for a scaled column.

*
*    The following variables hold a local copy of the value being got or
*    put.  They are needed because FIOW uses a passed length
*    character argument, whereas GVALW and PVALW necessarily use
*    a fixed length character argument.

      BYTE      LVALB
      INTEGER*2 LVALW
      INTEGER   LVALI
      REAL      LVALR
      DOUBLE PRECISION LVALD
      LOGICAL   LVALL
      CHARACTER LVALC*(CAT__SZVAL)

*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the unit number for the FITS file.

         FITUNT = FUNT__CAT3(CIELM)

*
*       Attempt to determine: the number of the column, the FITS data type
*       of the column, the scaled flag and the scale factor and zero point.

         IF (FI .GT. 0  .AND.  FI .LE. NIDS__CAT1) THEN
            COLNO = COLNO__CAT3(FI)
            FITYPE = FTYPE__CAT3(FI)
            SFLAG = SFLAG__CAT3(FI)
            SCALE = SCALE__CAT3(FI)
            ZERO = ZERO__CAT3(FI)
         ELSE
            STATUS = CAT__INVID

            CALL CAT1_ERREP ('CAT3_FIOW_NCP', 'Error getting '/
     :        /'or putting field value.', STATUS)

         END IF

         IF (STATUS .EQ. CAT__OK) THEN
            IF (IOFLG) THEN

*
*             Get a value from the FITS table.

               CALL CAT3_GVALW (FITYPE, SFLAG, SCALE, ZERO, FITUNT,
     :           COLNO, ELEM, ROWNO, LVALW, NULFLG, STATUS)

               VALUE = LVALW

            ELSE

*
*             Put a value to the FITS table.

               LVALW = VALUE

               CALL CAT3_PVALW (FITYPE, FITUNT, COLNO, ELEM, ROWNO,
     :           LVALW, NULFLG, STATUS)
            END IF

         END IF

      END IF

      END
