      SUBROUTINE CAP_GSCTB (CIIN, II, CIOUT, NUMCOL, FIOUT, STATUS)
*+
*  Name:
*     CAP_GSCTB
*  Purpose:
*     Copy the columns for a HST GSC region.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GSCTB (CIIN, II, CIOUT, NUMCOL, FIOUT; STATUS)
*  Description:
*     Copy the columns for a HST GSC region.  The Right Ascension and
*     Declination are converted from degrees to radians.  All the other
*     columns are simply copied.
*  Arguments:
*     CIIN  =  INTEGER (Given)
*        Identifier to the input catalogue.
*     II  =  INTEGER (Given)
*        Identifier to the index used to write the catalogue.
*     CIOUT  =  INTEGER (Given)
*        Identifier to the output catalogue.
*     NUMCOL  =  INTEGER (Given)
*        Number of columns in the input (and hence output) catalogue.
*     FIOUT(NUMCOL)  =  INTEGER (Given)
*        Identifiers for the columns in the output catalogue.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Get identifiers for the columns in the input catalogue.
*     Determine the number of rows in the catalogue.
*     For every row
*       Read the required values from the input catalogue.
*       Convert the Right Ascension and Declination to Radians.
*       Write the row to the output catalogue.
*     end for
*     Report any error.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     10/5/95 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants.
      INCLUDE 'CAT_PAR'     ! CAT symbolic constants.
      INCLUDE 'CAT_ERR'     ! Symbolic constants for CAT error codes.
*  Arguments Given:
      INTEGER
     :  CIIN,
     :  II,
     :  CIOUT,
     :  NUMCOL
*  Arguments Returned:
      INTEGER
     :  FIOUT(NUMCOL)
*  Status:
      INTEGER STATUS        ! Global status.
*  Local Constants:
      DOUBLE PRECISION PI   ! Pi.
      PARAMETER (PI = 3.14159265359D0)
*  Local Variables:
      INTEGER
     :  FIIN(CAT__MXCOL),   ! Identifiers for cols. in input cat.
     :  ROWS,               ! Number of rows in the input catalogue.
     :  ROW                 ! Current row.

*
*    Variables holding values for the individual fields.

      INTEGER          GSCID    ! GSC ID. within the region.
      DOUBLE PRECISION RAI      ! Input  Right Ascension (degrees).
      DOUBLE PRECISION RAO      ! Output   "      "      (radians).
      DOUBLE PRECISION DECI     ! Input  Declination (degrees).
      DOUBLE PRECISION DECO     ! Output      "      (radians).
      DOUBLE PRECISION POSERR   ! Positional error (seconds of arc).
      DOUBLE PRECISION MAG      ! Magnitude.
      DOUBLE PRECISION MAGERR   ! Magnitude error.
      INTEGER          MAGBND   ! Magnitude band.
      INTEGER          CLASS    ! Classification.
      CHARACTER        PLATE*4  ! GSSS Internal plate number.
      CHARACTER        MULT*1   ! Multiple component flag.

*
*    Null value flags.

      LOGICAL
     :  NGSCID,  ! GSC ID. within the region.
     :  NRAI,    ! Input  Right Ascension (degrees).
     :  NDECI,   ! Input  Declination (degrees).
     :  NPOSER,  ! Positional error (seconds of arc).
     :  NMAG,    ! Magnitude.
     :  NMAGER,  ! Magnitude error.
     :  NMAGBD,  ! Magnitude band.
     :  NCLASS,  ! Classification.
     :  NPLATE,  ! GSSS Internal plate number.
     :  NMULT    ! Multiple component flag.

*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Get identifiers for the columns in the input catalogue.

         CALL CAT_TIDNT (CIIN, 'GSC_ID',   FIIN(1), STATUS)
         CALL CAT_TIDNT (CIIN, 'RA_DEG',   FIIN(2), STATUS)
         CALL CAT_TIDNT (CIIN, 'DEC_DEG',  FIIN(3), STATUS)
         CALL CAT_TIDNT (CIIN, 'POS_ERR',  FIIN(4), STATUS)
         CALL CAT_TIDNT (CIIN, 'MAG',      FIIN(5), STATUS)
         CALL CAT_TIDNT (CIIN, 'MAG_ERR',  FIIN(6), STATUS)
         CALL CAT_TIDNT (CIIN, 'MAG_BAND', FIIN(7), STATUS)
         CALL CAT_TIDNT (CIIN, 'CLASS',    FIIN(8), STATUS)
         CALL CAT_TIDNT (CIIN, 'PLATE_ID', FIIN(9), STATUS)
         CALL CAT_TIDNT (CIIN, 'MULTIPLE', FIIN(10), STATUS)

*
*       Determine the number of rows in the catalogue.

         CALL CAT_TROWS (CIIN, ROWS, STATUS)

*
*       Copy every row from the input catalogue to the output catalogue,
*       chosing the required fields.

         ROW = 0

         DO WHILE (ROW .LT. ROWS  .AND.  STATUS .EQ. SAI__OK)
            ROW = ROW + 1

*
*          Read the next row from the input catalogue.

            CALL CAT_RGET (II, ROW, STATUS)

*
*          Read the required values from the input catalogue.

            CALL CAT_EGT0I (FIIN(1), GSCID, NGSCID, STATUS)
            CALL CAT_EGT0D (FIIN(2), RAI, NRAI, STATUS)
            CALL CAT_EGT0D (FIIN(3), DECI, NDECI, STATUS)
            CALL CAT_EGT0D (FIIN(4), POSERR, NPOSER, STATUS)
            CALL CAT_EGT0D (FIIN(5), MAG, NMAG, STATUS)
            CALL CAT_EGT0D (FIIN(6), MAGERR, NMAGER, STATUS)
            CALL CAT_EGT0I (FIIN(7), MAGBND, NMAGBD, STATUS)
            CALL CAT_EGT0I (FIIN(8), CLASS, NCLASS, STATUS)
            CALL CAT_EGT0C (FIIN(9), PLATE, NPLATE, STATUS)
            CALL CAT_EGT0C (FIIN(10), MULT, NMULT, STATUS)

*
*          Convert the Right Ascension and Declination to Radians.

            RAO = RAI * PI / 1.8D2
            DECO = DECI * PI / 1.8D2

*
*          Write the row to the output catalogue.

            CALL CAT_PUT0I (FIOUT(1), GSCID, NGSCID, STATUS)
            CALL CAT_PUT0D (FIOUT(2), RAO, NRAI, STATUS)
            CALL CAT_PUT0D (FIOUT(3), DECO, NDECI, STATUS)
            CALL CAT_PUT0D (FIOUT(4), POSERR, NPOSER, STATUS)
            CALL CAT_PUT0D (FIOUT(5), MAG, NMAG, STATUS)
            CALL CAT_PUT0D (FIOUT(6), MAGERR, NMAGER, STATUS)
            CALL CAT_PUT0I (FIOUT(7), MAGBND, NMAGBD, STATUS)
            CALL CAT_PUT0I (FIOUT(8), CLASS, NCLASS, STATUS)
            CALL CAT_PUT0C (FIOUT(9), PLATE, NPLATE, STATUS)
            CALL CAT_PUT0C (FIOUT(10), MULT, NMULT, STATUS)

*
*          Append the current row to the output catalogue.

            CALL CAT_RAPND (CIOUT, STATUS)
         END DO

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CAP_CPTAB_ERR', 'Error copying catalogue '/
     :        /'fields.', STATUS)
            CALL MSG_SETI ('ROW', ROW)
            CALL MSG_SETI ('ROWS', ROWS)
            CALL ERR_REP ('CAP_CPTAB_ERR', 'Failure in row ^ROW '/
     :        /'(the number of rows in the catalogue is ^ROWS).',
     :        STATUS)
         END IF

      END IF

      END
