      SUBROUTINE NDF1_CRFOR( FILE, IFMT, EXPFIL, LEXP, FID, STATUS )
*+
*  Name:
*     NDF1_CRFOR

*  Purpose:
*     Create a placeholder foreign output file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CRFOR( FILE, IFMT, EXPFIL, LEXP, FID, STATUS )

*  Description:
*     The routine creates an empty placeholder foreign format output
*     file, over-writing any pre-existing file with the same name if
*     necessary. This operation is intended to validate the name of a
*     new foreign format file and also to reserve a place for it in the
*     filing system, subject to any external constraints (e.g. file
*     access restrictions). The dummy file will normally be deleted and
*     replaced with the real file at a later stage, but it meanwhile
*     serves as a placeholder, permitting the detection of file access
*     problems associated with possible multiple use of the same output
*     file name.
*
*     The fully expanded name of the output file is returned, along with
*     an associated file identification code.

*  Arguments:
*     FILE = CHARACTER * ( * ) (Given)
*        Name of the file to be created.
*     IFMT = INTEGER (Given)
*        FCB format code identifying the foreign file format (must be
*        non-zero).
*     EXPFIL = CHARACTER * ( * ) (Returned)
*        Fully expanded file name.
*     LEXP = INTEGER (Returned)
*        The number of significant characters in the expanded file name.
*     FID = CHARACTER * ( * ) (Returned)
*        File identification code which uniquely identifies the file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     An empty file with the specified name should always exist after a
*     successful invocation of this routine.

*  Copyright:
*     Copyright (C) 1994 Particle Physics & Astronomy Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     11-MAR-1994 (RFWS):
*        Original version.
*     20-MAY-1994 (RFWS):
*        Changed to leave the file in existence and to return the file
*        name and identification code.
*     25-MAY-1994 (RFWS):
*        Check the PCB as well as the DCB to ensure the file is not
*        already in use.
*     25-MAY-1994 (RFWS):
*        Allow deletion of files which already exist, where they are to
*        be over-written.
*     14-NOV-1994 (RFWS):
*        Ensure existing files are always deleted if at all possible.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_FORID( NDF__MXDCB ) = CHARACTER * ( NDF__SZFID ) (Read)
*           Foreign format file identification codes.

            INCLUDE 'NDF_PCB'          ! NDF_ Placeholder Control Block
*        PCB_FORID( NDF__MXPCB ) = CHARACTER * ( NDF__SZFID ) (Read)
*           Foreign format file identification codes.

*  Arguments Given:
      CHARACTER * ( * ) FILE
      INTEGER IFMT

*  Arguments Returned:
      CHARACTER * ( * ) EXPFIL
      INTEGER LEXP
      CHARACTER * ( * ) FID

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER D1                 ! First character of directory field
      INTEGER D2                 ! Last character of directory field
      INTEGER IDCB               ! Index to DCB entry
      INTEGER IOSTAT             ! I/O error code
      INTEGER IPCB               ! Index to PCB entry
      INTEGER N1                 ! First character of name field
      INTEGER N2                 ! Last character of name field
      INTEGER NEXT               ! Next DCB entry to consider
      INTEGER T1                 ! First character of type field
      INTEGER T2                 ! Last character of type field
      INTEGER UNIT               ! I/O unit number
      INTEGER V1                 ! First character of version field
      INTEGER V2                 ! Last character of version field
      LOGICAL ACTIVE             ! File already in use?
      LOGICAL EXIST              ! File/unit exists?
      LOGICAL OK                 ! File access OK?
      LOGICAL OPENED             ! File open on unit?
      LOGICAL VERS               ! Version number field present?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      VERS = .FALSE.

*  Enquire if the foreign format output file already exists.
      INQUIRE ( FILE = FILE, EXIST = EXIST, IOSTAT = IOSTAT )

*  If an error occurred while enquiring about the file's existence, then
*  there is probably something wrong with the file name syntax. Report
*  the error.
      IF ( IOSTAT .NE. 0 ) THEN
         STATUS = NDF__INQER
         CALL MSG_SETC( 'FILE', FILE )
         CALL ERR_FIOER( 'MESSAGE', IOSTAT )
         CALL ERR_REP( 'NDF1_CRFOR_INQ',
     :        'Error enquiring about the existence of the ' //
     :        'file ''^FILE'' - ^MESSAGE', STATUS )

*  If the file already exists, then obtain its fully expanded name and
*  associated file identification code.
      ELSE IF ( EXIST ) THEN
         CALL NDF1_EXPFN( FILE, .TRUE., EXPFIL, LEXP, FID, STATUS )

*  Split the file name into fields to check whether a version field is
*  present (if so, a new version of the file can be created without
*  deleting the original).
         CALL NDF1_FSPLT( EXPFIL( : MAX( LEXP, 1 ) ), D1, D2, N1, N2,
     :                    T1, T2, V1, V2, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            VERS = ( V2 .GE. V1 )

*  If no new version can be created, then we may have to over-write the
*  existing file, so check that the necessary file access is available.
            IF ( .NOT. VERS ) THEN
               CALL NDF1_FILAC( EXPFIL( : MAX( LEXP, 1 ) ), 'WRITE',
     :                          .TRUE., OK, STATUS )

*  We can only use a file name for output if it is not already in use by
*  the NDF library (this is to prevent possible access conflicts), so
*  loop to compare the file identification code with all foreign format
*  file identification codes already stored in the DCB.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  ACTIVE = .FALSE.
                  NEXT = 0
                  IDCB = 0
 1                CONTINUE       ! Start of 'DO WHILE' loop
                  CALL NDF1_NXTSL( NDF__DCB, IDCB, NEXT, STATUS )
                  IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :                 ( NEXT .NE. 0 ) ) THEN
                     IDCB = NEXT

*  Search for DCB entries with the same foreign file identification code
*  (ignore blank identification codes, which indicate that
*  identification information could not be obtained for the file).
                     IF ( ( DCB_FORID( IDCB ) .EQ. FID ) .AND.
     :                    ( FID .NE. ' ' ) ) THEN
                        ACTIVE = .TRUE.
                        GO TO 2
                     END IF
                     GO TO 1
                  END IF
 2                CONTINUE
               END IF

*  If necessary, also check the PCB in the same way, as it may also
*  contain references to the same file.
               IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :              ( .NOT. ACTIVE ) ) THEN
                  NEXT = 0
                  IPCB = 0
 3                CONTINUE       ! Start of 'DO WHILE' loop
                  CALL NDF1_NXTSL( NDF__PCB, IPCB, NEXT, STATUS )
                  IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :                 ( NEXT .NE. 0 ) ) THEN
                     IPCB = NEXT

*  As before, search for non-blank PCB entries with the same foreign
*  file identification code.
                     IF ( ( PCB_FORID( IPCB ) .EQ. FID ) .AND.
     :                    ( FID .NE. ' ' ) ) THEN
                        ACTIVE = .TRUE.
                        GO TO 4
                     END IF
                     GO TO 3
                  END IF
 4                CONTINUE
               END IF

*  If the file is already in use, then report an error.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( ACTIVE ) THEN
                     STATUS = NDF__FILIN
                     CALL MSG_SETC( 'FILE', EXPFIL( : MAX( LEXP, 1 ) ) )
                     CALL ERR_REP( 'NDF1_CRFOR_USED',
     :                    'The foreign format file ''^FILE'' is ' //
     :                    'already in use by the NDF data access ' //
     :                    'library; this name cannot be used to ' //
     :                    'create a new output file.', STATUS )

*  Otherwise, the output file name is valid but conflicts with an
*  existing file, so delete it (using an external deletion command if
*  available).
                  ELSE
                     CALL NDF1_DLFOR( EXPFIL( : MAX( LEXP, 1 ) ), IFMT,
     :                                STATUS )
                  END IF
               END IF
            END IF
         END IF
      END IF

*  We must now create a new placeholder file. Loop to search for an I/O
*  unit which exists and is not currently connected to a file. Check for
*  errors during this process.
      IF ( STATUS .EQ. SAI__OK ) THEN
         DO 5 UNIT = NDF__UNIT1, NDF__UNIT2
            INQUIRE ( UNIT = UNIT, EXIST = EXIST, OPENED = OPENED,
     :                IOSTAT = IOSTAT )
            IF ( IOSTAT .NE. 0 ) THEN
               STATUS = NDF__INQER
               CALL MSG_SETI( 'UNIT', UNIT )
               CALL ERR_FIOER( 'MESSAGE', IOSTAT )
               CALL ERR_REP( 'NDF1_CRFOR_UNIT',
     :              'Error enquiring whether Fortran I/O unit ^UNIT ' //
     :              'exists and is connected to a file - ^MESSAGE',
     :              STATUS )
               GO TO 6

*  Quit searching when a suitable I/O unit has been found.
            ELSE IF ( EXIST .AND. ( .NOT. OPENED ) ) THEN
               GO TO 6
            END IF
 5       CONTINUE

*  Report an error if no suitable I/O unit could be found.
         STATUS = NDF__NOFIO
         CALL MSG_SETI( 'UNIT1', NDF__UNIT1 )
         CALL MSG_SETI( 'UNIT2', NDF__UNIT2 )
         CALL MSG_SETC( 'FILE', FILE )
         CALL ERR_REP( 'NDF1_CRFOR_NOU',
     :        'Unable to find a free Fortran I/O unit in the range ' //
     :        '^UNIT1 to ^UNIT2 on which to open the new ' //
     :        'file ''^FILE''.', STATUS )
 6       CONTINUE

*  If a suitable I/O unit was found, then attempt to open a new file on
*  it, using the name supplied.
         IF ( STATUS .EQ. SAI__OK ) THEN
            OPEN ( UNIT = UNIT, FILE = FILE, STATUS = 'NEW',
     :             IOSTAT = IOSTAT )

*  If a new file could not be created, then the file name is invalid
*  (for instance there may be a file name syntax error, or the required
*  directory may not exist, or there may be an access permission
*  problem). Report a suitable error message.
            IF ( IOSTAT .NE. 0 ) THEN
               STATUS = NDF__FILIN
               CALL MSG_SETC( 'FILE', FILE )
               CALL ERR_FIOER( 'MESSAGE', IOSTAT )
               CALL ERR_REP( 'NDF1_CRFOR_OPN',
     :              'The new file ''^FILE'' cannot be created - ' //
     :              '^MESSAGE', STATUS )

*  If file creation succeeds, then close the file.
            ELSE
               CLOSE ( UNIT = UNIT )

*  Obtain the fully expanded file name and associated file
*  identification code.
               CALL NDF1_EXPFN( FILE, .TRUE., EXPFIL, LEXP, FID,
     :                          STATUS )
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CRFOR', STATUS )

      END
