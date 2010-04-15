      SUBROUTINE POL1_CTPRP( PARAM, CI1, CI2, STATUS )
*+
*  Name:
*     POL1_CTPRP

*  Purpose:
*     Create an output catalogue, based on a supplied template catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CTPRP( PARAM, CI1, CI2, STATUS )

*  Description:
*     This routine creates a new catalogue using the supplied environment
*     parameter, and initializes its columns and parameters to match
*     those of a supplied existing catalogue.
*
*     At the moment, the CAT library reports errors if you try to store
*     textual information in a FITS file before any rows have been written
*     to the catalogue. For this reason, the textual information should
*     be stored after all rows have been written, just before the catalogue
*     is closed. See POL1_CLCAT.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the environment parameter to use.
*     CI1 = INTEGER (Given)
*        A CAT identifier for an existing catalogue.
*     CI2 = INTEGER (Returned)
*        A CAT identifier for the newly created catalogue.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-DEC-2000 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CAT_PAR'          ! CAT constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER CI1

*  Arguments Returned:
      INTEGER CI2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXCSIZ             ! Max length of character value
      PARAMETER ( MXCSIZ = 255 )

*  Local Variables:
      BYTE BVAL                  ! Component value
      CHARACTER COMM*250         ! Component comment
      CHARACTER CVAL*(MXCSIZ)    ! Component value
      CHARACTER EXFMT*50        ! Column external format string
      CHARACTER NAME*50          ! Component name
      CHARACTER UNITS*50         ! Column units string
      DOUBLE PRECISION DVAL      ! Component value
      INTEGER CSIZE              ! Character size
      INTEGER DTYPE              ! Component data type
      INTEGER GI1                ! Component identifier in input catalogue
      INTEGER GI2                ! Component identifier in output catalogue
      INTEGER J                  ! Component index
      INTEGER IVAL               ! Component value
      INTEGER*2 WVAL             ! Component value
      LOGICAL DONE               ! Finish looping?
      LOGICAL LVAL               ! Component value
      REAL RVAL                  ! Component value
*.

*  Initialise.
      CI2 = CAT__NOID

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create the output catalogue.
      CALL CTG_CREA1( PARAM, CI2, NAME, STATUS )

*  Loop round until all parameters in the input catalogue have been
*  copied to the output catalogue.
      J = 0
      DONE = .FALSE.
      DO WHILE( .NOT. DONE .AND. STATUS .EQ. SAI__OK )

*  Get an identifier for the next parameter
         J = J + 1
         CALL CAT_TNDNT( CI1, CAT__QITYP, J, GI1, STATUS )

*  Abort the loop if not found.
         IF( GI1 .EQ. CAT__NOID ) THEN
            DONE = .TRUE.
         ELSE

*  Create a  parameter with the same name, comment and value in the output
*  catalogue.
            CALL CAT_TIQAC( GI1, 'NAME', NAME, STATUS )
            CALL CAT_TIQAI( GI1, 'DTYPE', DTYPE, STATUS )
            CALL CAT_TIQAC( GI1, 'COMM', COMM, STATUS )

            IF( DTYPE .EQ. CAT__TYPEB ) THEN
               CALL CAT_TIQAB( GI1, 'VALUE', BVAL, STATUS )
               CALL CAT_PPTSB( CI2, NAME, BVAL, COMM, GI2, STATUS)

            ELSE IF( DTYPE .EQ. CAT__TYPEW ) THEN
               CALL CAT_TIQAW( GI1, 'VALUE', WVAL, STATUS )
               CALL CAT_PPTSW( CI2, NAME, WVAL, COMM, GI2, STATUS)

            ELSE IF( DTYPE .EQ. CAT__TYPEI ) THEN
               CALL CAT_TIQAI( GI1, 'VALUE', IVAL, STATUS )
               CALL CAT_PPTSI( CI2, NAME, IVAL, COMM, GI2, STATUS)

            ELSE IF( DTYPE .EQ. CAT__TYPER ) THEN
               CALL CAT_TIQAR( GI1, 'VALUE', RVAL, STATUS )
               CALL CAT_PPTSR( CI2, NAME, RVAL, COMM, GI2, STATUS)

            ELSE IF( DTYPE .EQ. CAT__TYPED ) THEN
               CALL CAT_TIQAD( GI1, 'VALUE', DVAL, STATUS )
               CALL CAT_PPTSD( CI2, NAME, DVAL, COMM, GI2, STATUS)

            ELSE IF( DTYPE .EQ. CAT__TYPEL ) THEN
               CALL CAT_TIQAL( GI1, 'VALUE', LVAL, STATUS )
               CALL CAT_PPTSL( CI2, NAME, LVAL, COMM, GI2, STATUS)

            ELSE IF( DTYPE .EQ. CAT__TYPEC ) THEN
               CALL CAT_TIQAC( GI1, 'VALUE', CVAL, STATUS )
               CALL CAT_PPTSC( CI2, NAME, CVAL, COMM, GI2, STATUS)
               CALL CAT_TIQAI( GI2, 'CSIZE', CSIZE, STATUS )
               CALL CAT_TATTI( GI2, 'CSIZE', MIN( CSIZE, MXCSIZ ),
     :                         STATUS )
            END IF

*  Set the external format string for the parameter, if it is defined in
*  the input.
            IF( STATUS .EQ. SAI__OK ) THEN
               CALL CAT_TIQAC( GI1, 'EXFMT', EXFMT, STATUS )
               IF( STATUS. EQ. SAI__OK ) THEN
                  CALL CAT_TATTC( GI2, 'EXFMT', EXFMT, STATUS )
               ELSE
                  CALL ERR_ANNUL( STATUS )
               END IF
            END IF

*  Release the identifiers.
            CALL CAT_TRLSE( GI1, STATUS )
            CALL CAT_TRLSE( GI2, STATUS )

         END IF
      END DO

*  Loop round until all columns in the input catalogue have been
*  copied to the output catalogue.
      J = 0
      DONE = .FALSE.
      DO WHILE( .NOT. DONE .AND. STATUS .EQ. SAI__OK )

*  Get an identifier for the next column.
         J = J + 1
         CALL CAT_TNDNT( CI1, CAT__FITYP, J, GI1, STATUS )

*  Abort the loop if not found.
         IF( GI1 .EQ. CAT__NOID ) THEN
            DONE = .TRUE.
         ELSE

*  Create a column in the output catalogue with the same attributes.
*  Take care to check for the possibility of attributes not existing.
            CALL CAT_TIQAC( GI1, 'NAME', NAME, STATUS )
            CALL CAT_TIQAI( GI1, 'DTYPE', DTYPE, STATUS )

            IF( STATUS .NE. SAI__OK ) GO TO 999

            CALL CAT_TIQAI( GI1, 'CSIZE', CSIZE, STATUS )
            IF( STATUS .NE. SAI__OK ) THEN
               CSIZE = 0
               CALL ERR_ANNUL( STATUS )
            END IF

            CALL CAT_TIQAC( GI1, 'UNITS', UNITS, STATUS )
            IF( STATUS .NE. SAI__OK ) THEN
               UNITS = ' '
               CALL ERR_ANNUL( STATUS )
            END IF

            CALL CAT_TIQAC( GI1, 'COMM', COMM, STATUS )
            IF( STATUS .NE. SAI__OK ) THEN
               COMM = ' '
               CALL ERR_ANNUL( STATUS )
            END IF

            CALL CAT_TIQAC( GI1, 'EXFMT', EXFMT, STATUS )
            IF( STATUS .NE. SAI__OK ) THEN
               EXFMT = ' '
               CALL ERR_ANNUL( STATUS )
            END IF

*  Create the output column.
            CALL CAT_CNEWS( CI2, NAME, DTYPE, CSIZE, UNITS, EXFMT,
     :                      COMM, GI2, STATUS )

*  Release the identifiers.
            CALL CAT_TRLSE( GI1, STATUS )
            CALL CAT_TRLSE( GI2, STATUS )

         END IF
      END DO

*  Arrive here if an error occurs.
 999  CONTINUE

*  Release the returned catalogue identifier if an eror occurred.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL CAT_TRLSE( CI2, STATUS )
         CI2 = CAT__NOID
      END IF

      END
