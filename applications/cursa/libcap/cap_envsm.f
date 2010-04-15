      SUBROUTINE CAP_ENVSM (CI, STATUS)
*+
*  Name:
*     CAP_ENVSM
*  Purpose:
*     Write summary catalogue details as environment parameters.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_ENVSM (CI; STATUS)
*  Description:
*     Write summary catalogue details as environment parameters.  The
*     details written are: the number of rows, the number of columns,
*     the number of parameters and a list of column names.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Obtain the details for the catalogue.
*     Obtain a list of column names.
*     Write the details as environment parameters.
*     Write the column names as an environment vector parameter.
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     2/11/01 (ACD): Original version.
*     5/11/01 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants.
      INCLUDE 'CAT_PAR'     ! Standard CAT symbolic constants.
*  Arguments Given:
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS        ! Global status.
*  Local Variables:
      INTEGER
     :  NUMROW,   ! Number of rows       in the catalogue.
     :  NUMCOL,   !   "    "  columns    "   "     "     .
     :  NUMPAR,   !   "    "  parameters "   "     "     .
     :  NUMIND,   !   "    "  indices    "   "     "     .
     :  FI,       ! Column (or field) identifier.
     :  FCOUNT    ! Sequential number of the current column.
      LOGICAL
     :  MORE      ! Flag: more columns to access?
      DOUBLE PRECISION
     :  DATE      ! Modification date of the catalogue.
      CHARACTER
     :  FNAME*(CAT__SZCMP),             ! Name of the current column.
     :  NAMES(CAT__MXCOL)*(CAT__SZCMP)  ! List of column names.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Obtain the details for the catalogue.

         CALL CAT_TDETL (CI, CAT__GALL, NUMROW, NUMCOL, NUMIND, NUMPAR,
     :     DATE, STATUS)

*
*       Obtain a list of column names.

         FCOUNT = 0
         MORE = .TRUE.

         DO WHILE (MORE)
            FCOUNT = FCOUNT + 1

            CALL CAT_TNDNT (CI, CAT__FITYP, FCOUNT, FI, STATUS)

            IF (STATUS .EQ. SAI__OK  .AND.  FI .NE. CAT__NOID) THEN
               CALL CAT_TIQAC (FI, 'NAME', FNAME, STATUS)
               NAMES(FCOUNT) = FNAME

            ELSE
               MORE = .FALSE.

            END IF
         END DO

*
*       Double-check that the number of columns is consistent.  Note that
*       the warning should only every be issued if CAT is in an internally
*       inconsistent state.

         IF (STATUS .EQ. SAI__OK  .AND.  FCOUNT-1 .NE. NUMCOL) THEN
            CALL CAP_WARN (.TRUE., ' ', 'The catalogue description '/
     :        /'is internally inconsistent.', STATUS)
         END IF

*
*       Write the details as environment parameters.

         CALL PAR_PUT0I ('ROWS', NUMROW, STATUS)
         CALL PAR_PUT0I ('COLS', NUMCOL, STATUS)
         CALL PAR_PUT0I ('PARS', NUMPAR, STATUS)

*
*       Write the column names as an environment vector parameter.

         CALL PAR_PUT1C ('NAMES', NUMCOL, NAMES, STATUS)

      END IF

      END
