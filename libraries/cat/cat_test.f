      PROGRAM CAT_TEST
*+
*  Name:
*     CAT_TEST
*  Purpose:
*     Test program to check installation of the CAT library.
*  Language:
*     Fortran 77
*  Type of Module:
*     Fortran main program.
*  Description:
*     Test program to check installation of the CAT library.
*
*     The program writes a small, simple catalogue in the STL format.
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

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     25/11/98 (ACD): Original version (from EXAMPLE_WRITE).
*     28/11/98 (ACD): First stable version.
*  Bugs:
*     None known.
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'
*  Status:
      INTEGER STATUS             ! Local running status.
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

*
*       Initialise the sdtatus.

         STATUS = CAT__OK

*
*       Create the new catalogue.

         CALL CAT_TOPEN ('testcat.TXT', 'NEW', 'WRITE', CI, STATUS)

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

         IF (STATUS .EQ. CAT__OK) THEN
            PRINT2000
 2000       FORMAT(1X, 'Catalogue created successfully.')
         ELSE
            PRINT2000, STATUS
 2001       FORMAT(1X, 'Failed to create catalogue; CAT status: ',
     :        I10 )
         END IF

      END
