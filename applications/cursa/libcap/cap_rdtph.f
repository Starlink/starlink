      SUBROUTINE CAP_RDTPH (CI, ROWS, EINCL, ECATMG, EINSMG,
     :  EAIRMS, CINCL, CCATMG, CINSMG, CAIRMS, STATUS)
*+
*  Name:
*     CAP_RDTPH
*  Purpose:
*     Read in catalogue for fitting instrumental to standard magnitudes.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_RDTPH (CI, ROWS, EINCL, ECATMG, EINSMG, EAIRMS,
*       CINCL, CCATMG, CINSMG, CAIRMS, STATUS)
*  Description:
*     Read in catalogue for fitting instrumental to standard magnitudes.
*     The columns read are: 'include in fit' flag, catalogue magnitude,
*     instrumental magnitude and airmass.  The 'include in fit' flag is
*     optional.  Any star with a null value in one of the fields read is
*     set so that it will not be fitted.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the catalogue.
*     EINCL  =  INTEGER (Given)
*        Expression identifier for the 'include in fit' flag.
*     ECATMG  =  INTEGER (Given)
*        Expression identifier for the catalogue magnitude.
*     EINSMG  =  INTEGER (Given)
*        Expression identifier for the instrumental magnitude.
*     EAIRMS  =  INTEGER (Given)
*        Expression identifier for the airmass.
*     CINCL(ROWS)  =  LOGICAL (Returned)
*        Column of 'include in fit' flags for the standard stars.
*     CCATMG(ROWS)  =  DOUBLE PRECISION (Returned)
*        Column of catalogue magnitudes for the standard stars.
*     CINSMG(ROWS)  =  DOUBLE PRECISION (Returned)
*        Column of instrumental magnitudes for the standard stars.
*     CAIRMS(ROWS)  =  DOUBLE PRECISION (Returned)
*        Column of airmasses for the standard stars.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For every row
*       Read the row.
*       Attempt to read the fields as appropriate.
*       If there were any null values then
*         Set the 'include in fit' flag for the row to false.
*         Report a message.
*       end if
*     end for
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     8/5/97  (ACD): Original version.
*     3/10/97 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parameteric constants.
*  Arguments Given:
      INTEGER
     :  CI,
     :  ROWS,
     :  EINCL,
     :  ECATMG,
     :  EINSMG,
     :  EAIRMS
*  Arguments Returned:
      LOGICAL
     :  CINCL(ROWS)
      DOUBLE PRECISION
     :  CCATMG(ROWS),
     :  CINSMG(ROWS),
     :  CAIRMS(ROWS)
*  Status:
      INTEGER STATUS  ! Global status.
*  Local Variables:
      INTEGER
     :  CURROW        ! Current row.
      LOGICAL     ! Values for the current field.s
     :  FINCL         ! 'Include in fit' flag.
      DOUBLE PRECISION
     :  FCATMG,       ! Catalogue magnitude.
     :  FINSMG,       ! Instrumental magnitude.
     :  FAIRMS        ! Air mass.
      LOGICAL     ! Null value flags for the current field:
     :  NINCL,        ! 'Include in fit' flag.
     :  NCATMG,       ! Catalogue magnitude.
     :  NINSMG,       ! Instrumental magnitude.
     :  NAIRMS        ! Air mass.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Read in all the rows.

         DO CURROW = 1, ROWS

*
*          Read the next row.

            CALL CAT_RGET (CI, CURROW, STATUS)

*
*          Read the fields and check for null values.  Note that
*          the 'include in fit' flag is optional (its absence being
*          indicated by the appropriate identifier being null).

*          ... 'include in fit' flag,

            IF (EINCL .NE. CAT__NOID) THEN
               CALL CAT_EGT0L (EINCL, FINCL, NINCL, STATUS)

               IF (.NOT. NINCL) THEN
                  CINCL(CURROW) = FINCL
               ELSE
                  CINCL(CURROW) = .FALSE.
               END IF
            ELSE
               CINCL(CURROW) = .TRUE.
            END IF

*          ... catalogue magnitude,

            CALL CAT_EGT0D (ECATMG, FCATMG, NCATMG, STATUS)
            IF (.NOT. NCATMG) THEN
               CCATMG(CURROW) = FCATMG
            ELSE
               CCATMG(CURROW) = 0.0D0
            END IF

*          ... instrumental magnitude,

            CALL CAT_EGT0D (EINSMG, FINSMG, NINSMG, STATUS)
            IF (.NOT. NINSMG) THEN
               CINSMG(CURROW) = FINSMG
            ELSE
               CINSMG(CURROW) = 0.0D0
            END IF

*          ... air mass.

            CALL CAT_EGT0D (EAIRMS, FAIRMS, NAIRMS, STATUS)
            IF (.NOT. NAIRMS) THEN
               CAIRMS(CURROW) = FAIRMS
            ELSE
               CAIRMS(CURROW) = 0.0D0
            END IF

*
*          If there is a null value in any column name then set the
*          'include in fit' flag for the row to false and report a
*          message.

            IF (NINCL .OR. NCATMG .OR. NINSMG .OR. NAIRMS) THEN
               CINCL(CURROW) = .FALSE.

               CALL MSG_SETI ('CURROW', CURROW)
               CALL MSG_OUT (' ', 'Standard star ^CURROW omitted '/
     :           /'because of null values.', STATUS)
            END IF

         END DO

      END IF

      END
