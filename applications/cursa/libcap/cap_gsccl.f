      SUBROUTINE CAP_GSCCL (CIOUT, MXCOL, NUMCOL, FIOUT, STATUS)
*+
*  Name:
*     CAP_GSCCL
*  Purpose:
*     Create a set of columns for a HST GSC region.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GSCCL (CIOUT, MXCOL; NUMCOL, FIOUT; STATUS)
*  Description:
*     Create a set of columns for a HST GSC region converted to the
*     preferred CURSA format.  Note that the catalogue will be sorted
*     on Declination.
*  Arguments:
*     CIOUT  =  INTEGER (Given)
*        Identifier for the output catalogue.
*     MXCOL  =  INTEGER (Given)
*        Maximum permitted number of catalogues.
*     NUMCOL  =  INTEGER (Returned)
*        Number of columns in the output catalogue.
*     FIOUT(MXCOL)  =  INTEGER (Returned)
*        identifiers for the columns in the output catalogue.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Create the columns.
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
*  Arguments Given:
      INTEGER
     :  CIOUT,
     :  MXCOL
*  Arguments Returned:
      INTEGER
     :  NUMCOL,
     :  FIOUT(MXCOL)
*  Status:
      INTEGER STATUS        ! Global status.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       ID within Region.

         CALL CAT_CNEWS (CIOUT, 'GSC_ID', CAT__TYPEI, 0, ' ',
     :     'I5', 'ID within Region.', FIOUT(1), STATUS)

*
*       Right Ascension.

         CALL CAT_CNEWS (CIOUT, 'RA', CAT__TYPED, 0, 'RADIANS{HOURS}',
     :     'D15.7', 'Right Ascension', FIOUT(2), STATUS)

*
*       Declination.  Note that the 'ORDER' attribute is set.

         CALL CAT_CNEWS (CIOUT, 'DEC', CAT__TYPED, 0,
     :     'RADIANS{DEGREES}', 'D15.7', 'Declination', FIOUT(3),
     :     STATUS)
         CALL CAT_TATTI (FIOUT(3), 'ORDER', CAT__ASCND, STATUS)

*
*       Position Error in Arc Seconds.

         CALL CAT_CNEWS (CIOUT, 'POS_ERR', CAT__TYPED, 0, 'Arc Seconds',
     :     'F5.1', 'Position Error.', FIOUT(4), STATUS)

*
*       Magnitude.

         CALL CAT_CNEWS (CIOUT, 'MAG', CAT__TYPED, 0, 'Magnitude',
     :     'F5.2', 'Magnitude.', FIOUT(5), STATUS)

*
*       Magnitude error.

         CALL CAT_CNEWS (CIOUT, 'MAG_ERR', CAT__TYPED, 0, 'Magnitude',
     :     'F4.2', 'Magnitude error.', FIOUT(6), STATUS)

*
*       Magnitude Band.

         CALL CAT_CNEWS (CIOUT, 'MAG_BAND', CAT__TYPEI, 0, ' ',
     :     'I2', 'Magnitude Band.', FIOUT(7), STATUS)

*
*       Classification.

         CALL CAT_CNEWS (CIOUT, 'CLASS', CAT__TYPEI, 0, ' ',
     :     'I1', 'Classification.', FIOUT(8), STATUS)

*
*       GSSS Internal Plate Number.

         CALL CAT_CNEWS (CIOUT, 'PLATE_ID', CAT__TYPEC, 4, ' ',
     :     'A4', 'GSSS Internal Plate Number.', FIOUT(9), STATUS)

*
*       (T/F) Flag for additional entries.

         CALL CAT_CNEWS (CIOUT, 'MULTIPLE', CAT__TYPEC, 1, ' ',
     :     'A1', '(T/F) Flag for additional entries.', FIOUT(10),
     :     STATUS)

         NUMCOL = 10

      END IF

      END
