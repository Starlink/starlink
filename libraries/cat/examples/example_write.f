      SUBROUTINE EXAMPLE_WRITE (STATUS)
*+
*  Name:
*     EXAMPLE_WRITE
*  Purpose:
*     Example program to demonstrate writing a catalogue.
*  Language:
*     Fortran 77
*  Type of Module:
*     ADAM A-task
*  Invocation:
*     CALL EXAMPLE_WRITE (STATUS)
*  Arguments:
*     STATUS = INTEGER (UPDATE)
*        The global status.
*  Description:
*     Example program to demonstrate writing a catalogue.
*  Algorithm:
*     Create the catalogue.
*     Create some columns.
*     Create some parameters.
*     Finish the creation of the catalogue.
*     for several rows
*       Write values for each field.
*     end for
*     Release the identifier for the catalogue.
*     Report success or failure creating the catalogue, as appropriate.
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     13/8/93  (ACD): Original version.
*     28/1/94  (ACD): Re-written as a proper ADAM application.
*     10/3/94  (ACD): Repaced CAT_TOPEN with CAT_CREAT.
*     16/3/94  (ACD): Removed un-used variable.
*     23/11/94 (ACD): Fixed incorrect comment for parameter PARI.
*     19/4/95  (ACD): Modified the error and message reporting.
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
     :  QII,      ! Identifier for a real parameter.
     :  QIR,      ! Identifier for a real parameter.
     :  QIC,      ! Identifier for a character parameter .
     :  FII,      ! Identifier for an integer column (or field).
     :  FIR,      ! Identifier for a real column (or field).
     :  FIC,      ! Identifier for a character column (or field).
     :  LOOP      ! Loop index.
      INTEGER
     :  VALI      ! Integer value.
      REAL
     :  VALR      ! Real value.
      CHARACTER
     :  VALC*10   ! Character value.
      LOGICAL
     :  NULI,     ! Null flag corresponding to VALI.
     :  NULR,     !  "    "         "       "  VALR.
     :  NULC      !  "    "         "       "  VALC.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Create the new catalogue.

         CALL CAT_CREAT ('CNAME', CI, STATUS)

*
*       Create some columns.

         CALL CAT_PNEW0 (CI, CAT__FITYP, 'COLI', CAT__TYPEI, FII,
     :     STATUS)
         CALL CAT_TATTC (FII, 'COMM', 'Integer column', STATUS)

         CALL CAT_PNEW0 (CI, CAT__FITYP, 'COLR', CAT__TYPER, FIR,
     :     STATUS)
         CALL CAT_TATTC (FIR, 'COMM', 'Real column', STATUS)

         CALL CAT_PNEW0 (CI, CAT__FITYP, 'COLC', CAT__TYPEC, FIC,
     :     STATUS)
         CALL CAT_TATTC (FIC, 'COMM', 'Character column', STATUS)
         CALL CAT_TATTI (FIC, 'CSIZE', 10, STATUS)

*
*       Create some parameters.

         CALL CAT_PNEW0 (CI, CAT__QITYP, 'PARI', CAT__TYPEI, QII,
     :     STATUS)
         CALL CAT_TATTC (QII, 'COMM', 'Integer parameter', STATUS)
         CALL CAT_TATTI (QII, 'VALUE', 23, STATUS)

         CALL CAT_PNEW0 (CI, CAT__QITYP, 'PARR', CAT__TYPER, QIR,
     :     STATUS)
         CALL CAT_TATTC (QIR, 'COMM', 'Real parameter', STATUS)
         CALL CAT_TATTR (QIR, 'VALUE', 42.0, STATUS)

         CALL CAT_PNEW0 (CI, CAT__QITYP, 'PARC', CAT__TYPEC, QIC,
     :     STATUS)
         CALL CAT_TATTC (QIC, 'COMM', 'Character parameter', STATUS)
         CALL CAT_TATTI (QIC, 'CSIZE', 20, STATUS)
         CALL CAT_TATTC (QIC, 'VALUE', 'Example string', STATUS)

*
*       Write some values to the catalogue.

         DO LOOP = 1, 25

            VALI = LOOP
            VALR = 2.3E1 + REAL(LOOP)
            VALC = ' '
            WRITE(VALC, 4000) LOOP
 4000       FORMAT(' Loop ',I3, '%')

            NULI = .FALSE.
            NULR = .FALSE.
            NULC = .FALSE.

*
*          Make all the columns contain null values for row 10.

            IF (LOOP .EQ. 10) THEN
               NULI = .TRUE.
               NULR = .TRUE.
               NULC = .TRUE.
            END IF

            CALL CAT_PUT0I (FII, VALI, NULI, STATUS)
            CALL CAT_PUT0R (FIR, VALR, NULR, STATUS)
            CALL CAT_PUT0C (FIC, VALC, NULC, STATUS)

            CALL CAT_RAPND (CI, STATUS)
         END DO

*
*       Release the identifier for the catalogue.

         CALL CAT_TRLSE (CI, STATUS)

*
*       Report success or failure writing the catalogue, as appropriate.

         IF (STATUS .EQ. SAI__OK) THEN
            CALL MSG_OUT (' ', 'Catalogue created successfully.',
     :        STATUS)
         ELSE
            CALL ERR_REP ('EXAMPLE_WRITE_ERR', 'Failed to create '/
     :        /'catalogue.', STATUS)
         END IF

      END IF

      END
