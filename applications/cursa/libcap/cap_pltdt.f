      SUBROUTINE CAP_PLTDT (GAI, QRA, QDEC, QRAC, QDECC, STATUS)
*+
*  Name:
*     CAP_PLTDT
*  Purpose:
*     Get the find chart plotting details: query centre and radius.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PLTDT (GAI, QRA, QDEC, QRAC, QDECC; STATUS)
*  Description:
*     Get the find chart plotting details: query centre and radius.
*
*     An attempt is made to read the details as catalogue parameters.
*     If they are not present as parameters they are computed from the
*     table of values.
*  Arguments:
*     GAI  =  INTEGER (Given)
*        Catalogue identifier for the input graphics attributes list.
*     QRA  =  DOUBLE PRECISION (Returned)
*        Query central Right Ascension (radians).
*     QDEC  =  DOUBLE PRECISION (Returned)
*        Query central Declination (radians).
*     QRAC  =  CHARACTER*(*) (Returned)
*        Query central Right Ascension as a string formatted for display
*        (sexagesimal hours).
*     QDECC  =  CHARACTER*(*) (Returned)
*        Query central Declination as a string formatted for display
*        (sexagesimal degrees).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to get an identifier for the Query RA parameter.
*     If ok then
*       Set the 'got RA parameter' flag.
*       Get the value.
*     else
*       If the parameter was absent then
*         Annul the error.
*       end if
*     end if
*     Attempt to get an identifier for the Query Dec. parameter.
*     If ok then
*       Set the 'got Dec. parameter' flag.
*       Get the value.
*     else
*       If the parameter was absent then
*         Annul the error.
*       end if
*     end if
*     If ok then
*       If all the values were not obtained then
*         Get the number of rows in the catalogue.
*         If there is more than zero rows then
*           Get the value for the first row.
*           If there is more than one row then
*             For all subsequent rows
*               Check for the maximum value.
*               Check for the minimum value.
*             end for
*             Compute the centre.
*           else
*             Adopt values for the centre.
*           end if
*           Set the values which have not been read from catalogue
*           parameters.
*           Report the centre and radius adopted.
*         else
*           Set the status.
*           report an error.
*         end if
*       end if
*     end if
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     25/7/00 (ACD): Original version.
*     18/4/01 (ACD): Corrected bug in deciding when to compute the
*       plot radius.  Also made computing the central Right Ascension
*       work correctly across the 0/24 hour boundary.
*     19/4/01 (ACD): Corrected calculation of radius.
*     20/4/01 (ACD): Removed calculation of radius entirely.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAP_PAR'           ! CAP parametric constants.
*  Arguments Given:
      INTEGER
     :  GAI
*  Arguments Returned:
      DOUBLE PRECISION
     :  QRA,
     :  QDEC
      CHARACTER
     :  QRAC*(*),
     :  QDECC*(*)
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Constants:
      DOUBLE PRECISION TWOPI     ! 2.Pi.
      PARAMETER (TWOPI = 2.0D0 * CAP__PI)
*  Local Variables:
      INTEGER
     :  QRAI,     ! Identifier for the query central R.A. parameter.
     :  QDECI,    !      "      "   "    "      "    Dec.     "    .
     :  RAI,      !      "      "   "  R.A. column.
     :  DECI,     !      "      "   "  Dec.   "   .
     :  ROWS,     ! Number of rows in the catalogue.
     :  ROW       ! Number of the current row.
      LOGICAL
     :  GQRA,     ! Flag; got the query central R.A. parameter?
     :  GQDEC,    !  "  ;  "   "    "      "    Dec.     "    ?
     :  NULFLG    ! Null value flag for the current field.
      DOUBLE PRECISION
     :  RAVAL,    ! R.A. for the current row (radians).
     :  DECVAL,   ! Dec.  "   "     "     "  (   "   ).
     :  MINRA,    ! Minimum R.A. (radians).
     :  MAXRA,    ! Maximum R.A. (   "   ).
     :  MINDEC,   ! Minimum Dec. (   "   ).
     :  MAXDEC,   ! Maximum Dec. (   "   ).
     :  LQRA,     ! Local QRA  (excl. trail. blanks).
     :  LQDEC     !   "   QDEC ( "  .   "  .   "   ).
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Intialise the 'got RA, and Dec.' flags.

         GQRA = .FALSE.
         GQDEC = .FALSE.

*
*       Attempt to get an identifier for the Query R.A. parameter.
*       If ok then get the value and set the appropriate flag.
*       If not then annul the error if the parameter was not found.

         CALL CAT_TIDNT (GAI, 'QUERYRA', QRAI, STATUS)
         IF (STATUS .EQ. SAI__OK) THEN
            CALL CAT_TIQAC (QRAI, 'VALUE', QRAC, STATUS)
            CALL CAP_ANGDC ('HOURS', QRAC, QRA, STATUS)
            GQRA = .TRUE.
         ELSE
            IF (STATUS .EQ. CAT__NOCMP) THEN
               CALL ERR_ANNUL (STATUS)
            END IF
         END IF

*
*       Attempt to get an identifier for the Query Dec. parameter.
*       If ok then get the value and set the appropriate flag.
*       If not then annul the error if the parameter was not found.

         CALL CAT_TIDNT (GAI, 'QUERYDEC', QDECI, STATUS)
         IF (STATUS .EQ. SAI__OK) THEN
            CALL CAT_TIQAC (QDECI, 'VALUE', QDECC, STATUS)
            CALL CAP_ANGDC ('DEGREES', QDECC, QDEC, STATUS)
            GQDEC = .TRUE.
         ELSE
            IF (STATUS .EQ. CAT__NOCMP) THEN
               CALL ERR_ANNUL (STATUS)
            END IF
         END IF

*
*       Proceed if all is ok.

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Check whether all the values were obtained and if not then
*          attempt to compute the missing values from the catalogue.

            IF (.NOT. GQRA  .OR.  .NOT. GQDEC) THEN

*
*             Get the number of rows in the catalogue.

               CALL CAT_TROWS (GAI, ROWS, STATUS)

*
*             Check that there are more than zero rows.

               IF (ROWS .GT. 0) THEN

*
*                Get identifiers for the R.A. and Dec. columns and then
*                the values for the first row.

                  CALL CAT_TIDNT (GAI, 'RA', RAI, STATUS)
                  CALL CAT_TIDNT (GAI, 'DEC', DECI, STATUS)

                  CALL CAT_RGET (GAI, 1, STATUS)
                  CALL CAT_EGT0D (RAI, RAVAL, NULFLG, STATUS)
                  CALL CAT_EGT0D (DECI, DECVAL, NULFLG, STATUS)

*
*                Check whether there is more than one row.

                  IF (ROWS .GT. 1) THEN

*
*                   Read through the catalogue to determine the
*                   minimum and maximum R.A. and Dec.

                     MINRA = RAVAL
                     MAXRA = RAVAL

                     MINDEC = DECVAL
                     MAXDEC = DECVAL

                     DO ROW = 2, ROWS
                        CALL CAT_RGET (GAI, ROW, STATUS)
                        CALL CAT_EGT0D (RAI, RAVAL, NULFLG, STATUS)
                        CALL CAT_EGT0D (DECI, DECVAL, NULFLG, STATUS)

                        MINRA = MIN(MINRA, RAVAL)
                        MAXRA = MAX(MAXRA, RAVAL)

                        MINDEC = MIN(MINDEC, DECVAL)
                        MAXDEC = MAX(MAXDEC, DECVAL)
                     END DO

*
*                   Compute the centre.
*
*                   A check is made that the region does not cross the
*                   0/24 hour boundary; if the minimum and maximum
*                   Right Ascension differ by more than Pi radians then
*                   it is assumed that the region really crosses the
*                   boundary.

                     IF ((MAXRA - MINRA) .LT. CAP__PI) THEN
                        LQRA = (MAXRA + MINRA) / 2.0D0
                     ELSE
                        MINRA = MINRA + TWOPI
                        LQRA = (MAXRA + MINRA) / 2.0D0

                        IF (LQRA .GT. TWOPI) THEN
                           LQRA = LQRA + TWOPI
                        END IF
                     END IF

                     LQDEC  = (MAXDEC  + MINDEC) / 2.0D0

                  ELSE

*
*                   The catalogue contained only a single row;
*                   adopt the values for the central R.A. and Dec.

                     LQRA = RAVAL
                     LQDEC = DECVAL

                  END IF

*
*                Set the values which have not been read from catalogue
*                parameters.

                  IF (.NOT. GQRA) THEN
                     QRA = LQRA
                     CALL CAP_R2SGF (QRA, 'HOURS', 1, QRAC, STATUS)
                  END IF

                  IF (.NOT. GQDEC) THEN
                     QDEC = LQDEC
                     CALL CAP_R2SGF (QDEC, 'DEGREES', 0, QDECC, STATUS)
                  END IF

*
*                Report the centre adopted.

                  CALL MSG_OUT (' ', 'Chart details recomputed as '/
     :              /'follows:-', STATUS)

                  CALL MSG_SETC ('QRAC', QRAC)
                  CALL MSG_OUT (' ', '  Central Right Ascension '/
     :              /'(hours): ^QRAC', STATUS)

                  CALL MSG_SETC ('QDECC', QDECC)
                  CALL MSG_OUT (' ', '  Central Declination '/
     :              /'(degrees): ^QDECC', STATUS)
               ELSE

*
*                The catalogue contained no rows.

                  STATUS = SAI__ERROR

                  CALL ERR_REP ('CAP_PLTDT_NRW', 'Empty catalogue '/
     :              /'(zero rows).', STATUS)

               END IF
            END IF
         END IF

      END IF

      END
