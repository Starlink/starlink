      SUBROUTINE CAP_FITPH (ROWS, FZEROP, FATMOS, CINCL, CCATMG,
     :  CINSMG, CAIRMS, INSCON, ZEROP, ATMOS, WORK, RNORM, NUMFIT,
     :  STATUS)
*+
*  Name:
*     CAP_FITPH
*  Purpose:
*     Fit a set of instrumental star magnitudes to catalogue magnitudes.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_FITPH (ROWS, FZEROP, FATMOS, CINCL, CCATMG, CINSMG,
*       CAIRMS, INSCON; ZEROP, ATMOS; WORK; RNORM, NUMFIT; STATUS)
*  Description:
*     Fit a set of instrumental star magnitudes to catalogue magnitudes.
*  Arguments:
*     ROWS  =  INTEGER (Given)
*        The number of rows (ie. standard stars) in the list.
*     FZEROP  =  LOGICAL (Given)
*        Flag indicating whether the zero point is to be fitted or is
*        already determined and fixed.  It is coded as follows:
*        .TRUE.  -  fit the zero point,
*        .FALSE. -  determine the zero point.
*     FATMOS  =  LOGICAL (Given)
*        Flag indicating whether the atmospheric extinction is to be
*        fitted or is already determined and fixed.  It is coded as
*        follows:
*        .TRUE.  -  fit the atmospheric extinction,
*        .FALSE. -  determine the atmospheric extinction.
*     CINCL(ROWS)  =  LOGICAL (Given)
*        Flags indicating for each row (ie. star) whether they are to be
*        included in the fit.  The coding is as follows:
*        .TRUE.  - include in fit,
*        .FALSE. - exclude from fit.
*     CCATMG(ROWS)  =  DOUBLE PRECISION (Given)
*        Catalogue magnitudes for the standard stars.
*     CINSMG(ROWS)  =  DOUBLE PRECISION (Given)
*        Instrumental magnitudes for the standard stars.
*     CAIRMS(ROWS)  =  DOUBLE PRECISION (Given)
*        Air mass for the standard stars.
*     INSCON  =  DOUBLE PRECISION (Given)
*        Arbitrary constant applied to the instrumental magnitudes.
*     ZEROP  =  DOUBLE PRECISION (Given and Returned)
*        Zero point of the transformation.
*     ATMOS  =  DOUBLE PRECISION (Given and Returned)
*        Atmospheric extinction of the transformation.
*     WORK(ROWS, 3)  =  DOUBLE PRECISION (Work)
*        Work array.
*     RNORM  =  DOUBLE PRECISION (Returned)
*        Minimum residual vector length (a measure of the goodness of
*        the fit).
*     NUMFIT  =  DOUBLE PRECISION (Returned)
*        Number of standard stars included in the fit.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise the control arrays to indicate which elements are to
*     vary.
*     Copy the data to the work array, excluding stars which are not
*     to be fitted.
*     If there are sufficient stars then
*       Perform the fit.
*       If ok then
*         Copy the results to the return variables.
*       else
*         Set the status.
*         Report an error: failed to make a fit.
*       end if
*     else
*       Set the status.
*       Report an error: insufficient stars to fit.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     3/10/97 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      INTEGER
     :  ROWS
      LOGICAL
     :  FZEROP,
     :  FATMOS,
     :  CINCL(ROWS)
      DOUBLE PRECISION
     :  CCATMG(ROWS),
     :  CINSMG(ROWS),
     :  CAIRMS(ROWS),
     :  INSCON
*  Arguments Given and Returned:
      DOUBLE PRECISION
     :  ZEROP,
     :  ATMOS,
     :  WORK(ROWS, 3)
*  Arguments Returned:
      DOUBLE PRECISION
     :  RNORM
      INTEGER
     :  NUMFIT
*  Status:
      INTEGER STATUS        ! Global status.
*  External References:
*     <...>
*  Local Constants:
      INTEGER NFIT          ! Number of parameters to fit.
      PARAMETER (NFIT = 2)
*  Local Variables:
      DOUBLE PRECISION
     :  BL(NFIT),   ! Lower bound for constrained coeffcients.
     :  BU(NFIT),   ! Upper   "    "       "          "      .
     :  X(NFIT+3),  ! Work array.
     :  RW(NFIT*5)  !  "     "  .
      INTEGER
     :  IND(NFIT),  ! Type code for coefficient bounds.
     :  IOPT(10),   ! Array for specifying non-standard fitting options.
     :  IW(NFIT*2), ! Work array.
     :  MODE,       ! Error and status codes returned by fitting routine.
     :  LOOP,       ! Lopp index.
     :  CURROW      ! Current row.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Initialise the control arrays to indicate which elements are
*       to vary.

         IOPT(1) = 99

         DO LOOP = 1, NFIT
            BU(LOOP) = 0.0D0
            BL(LOOP) = 0.0D0
            IND(LOOP) = 4
         END DO

         IF (FZEROP) THEN
            IND(1) = 3
            BU(1) = ZEROP
            BL(1) = ZEROP
         END IF

         IF (FATMOS) THEN
            IND(2) = 3
            BU(2) = ATMOS
            BL(2) = ATMOS
         END IF

*
*       Copy the data to the work array, excluding stars which are
*       not to be fitted.

         NUMFIT = 0

         DO CURROW = 1, ROWS

*
*          Check whether the current row (that is, standard star) is
*          to be fitted.

            IF (CINCL(CURROW)) THEN
               NUMFIT = NUMFIT + 1

*
*             Copy the values to the work array, rearranging ready for
*             fitting.

               WORK(NUMFIT, 1) = 1.0D0
               WORK(NUMFIT, 2) = -1.0D0 * CAIRMS(CURROW)
               WORK(NUMFIT, 3) =
     :           INSCON - CINSMG(CURROW) + CCATMG(CURROW)
            END IF
         END DO

*
*       Check that sufficient stars have been selected to perform a fit.

         IF (NUMFIT .GT. 3) THEN

*
*          Perform the fit.

            CALL PDA_DBOLS (WORK, ROWS, NUMFIT, NFIT, BL, BU, IND,
     :        IOPT, X, RNORM, MODE, RW, IW, STATUS)

*
*          If ok then copy the results to the return variables.
*          Otherwise set the status and report an error.

            IF (MODE .GE. 0) THEN
               ZEROP = X(1)
               ATMOS = X(2)

            ELSE
               STATUS = SAI__ERROR

               CALL MSG_SETI ('MODE', MODE)
               CALL ERR_REP ('CATPHOTOMFIT_MDE', 'CAP_FITPH: error '/
     :           /'code from PDS_DBOLS: ^MODE (see SUN/194).',
     :           STATUS)

            END IF

         ELSE
            STATUS = SAI__ERROR

            CALL ERR_REP ('CATPHOTOMFIT_FEW', 'CAP_FITPH: too few '/
     :        /'stars to perform a fit.', STATUS)

         END IF

      END IF

      END
