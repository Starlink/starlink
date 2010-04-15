      SUBROUTINE EXAMPLE_ANGLES (STATUS)
*+
*  Name:
*     EXAMPLE_ANGLES
*  Purpose:
*     Example of writing a catalogue with angular celestial coordinates.
*  Language:
*     Fortran 77
*  Type of Module:
*     ADAM A-task
*  Invocation:
*     CALL EXAMPLE_ANGLES (STATUS)
*  Arguments:
*     STATUS = INTEGER (UPDATE)
*        The global status.
*  Description:
*     Example of writing a catalogue with angular celestial coordinates.
*     The catalogue contains fake equatorial coordinates and B
*     magnitudes for a set of stars.
*  Algorithm:
*     Create the catalogue.
*     Create the columns.
*     Finish the creation of the catalogue.
*     for several rows
*       Write values for each field.
*     end for
*     Release the identifier for the catalogue.
*     Report success or failure creating the catalogue, as appropriate.
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     19/4/95  (ACD): Original version.
*  Bugs:
*     None known.
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'CAT_PAR'
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  CI,       ! Catalogue identifier.
     :  RAI,      ! Identifier for column of Right Ascension.
     :  DECI,     !     "       "    "    "  Declination.
     :  BI,       !     "       "    "    "  B magnitudes.
     :  LOOP      ! Loop index.
      DOUBLE PRECISION
     :  RA,       ! Right Ascension for current star.
     :  DEC       ! Declination      "     "     "  .
      REAL
     :  B         ! B magnitude      "     "     "  .
      LOGICAL
     :  NULRA,    ! Null flag corresponding to RA.
     :  NULDEC,   !  "    "         "       "  DEC.
     :  NULB      !  "    "         "       "  B.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Create the new catalogue.

         CALL CAT_CREAT ('CNAME', CI, STATUS)

*
*       Create the columns.

         CALL CAT_PNEW0 (CI, CAT__FITYP, 'RA', CAT__TYPED, RAI,
     :     STATUS)
         CALL CAT_TATTC (RAI, 'UNITS', 'RADIANS{HOURS}', STATUS)
         CALL CAT_TATTC (RAI, 'COMM', 'Right Ascension', STATUS)

         CALL CAT_PNEW0 (CI, CAT__FITYP, 'DEC', CAT__TYPED, DECI,
     :     STATUS)
         CALL CAT_TATTC (DECI, 'UNITS', 'RADIANS{DEGREES}', STATUS)
         CALL CAT_TATTC (DECI, 'COMM', 'Declination', STATUS)

         CALL CAT_PNEW0 (CI, CAT__FITYP, 'B', CAT__TYPER, BI,
     :     STATUS)
         CALL CAT_TATTC (BI, 'UNITS', 'Magnitudes', STATUS)
         CALL CAT_TATTC (BI, 'COMM', 'B magnitude', STATUS)

*
*       Invent and write some fake values for the celestial coordinates
*       and magnitudes.  Note that the values for the Right Ascension
*       and Declination are specified in radians.

         DO LOOP = 1, 25
            RA = 1.0D0 + (DBLE(LOOP) / 1.0D2)
            DEC = -1.0D0 + (DBLE(LOOP) / 1.0D2)

            B = 1.0E1 + (REAL(LOOP) / 1.0E1)

            NULRA = .FALSE.
            NULDEC = .FALSE.
            NULB = .FALSE.

            CALL CAT_PUT0D (RAI, RA, NULRA, STATUS)
            CALL CAT_PUT0D (DECI, DEC, NULDEC, STATUS)
            CALL CAT_PUT0R (BI, B, NULB, STATUS)

            CALL CAT_RAPND (CI, STATUS)
         END DO

*
*       Release the identifier for the catalogue.

         CALL CAT_TRLSE (CI, STATUS)

*
*       Report success or failure creating the catalogue, as appropriate.

         IF (STATUS .EQ. SAI__OK) THEN
            CALL MSG_OUT (' ', 'Catalogue created successfully.',
     :        STATUS)
         ELSE
            CALL ERR_REP ('EXAMPLE_ANGLES_ERR', 'Failed to create '/
     :        /'catalogue.', STATUS)
         END IF

      END IF

      END
