      SUBROUTINE NDF1_CLFOR( DISPOS, IDCB, STATUS )
*+
*  Name:
*     NDF1_CLFOR

*  Purpose:
*     Close a DCB entry, possibly associated with a foreign file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CLFOR( DISPOS, IDCB, STATUS )

*  Description:
*     The routine closes a DCB entry, releasing the NDF data object and
*     updating any associated foreign file. This routine should only be
*     called once all other active DCB locators associated with the
*     object have been annulled. It will then annul the main data
*     object locator and clear its file and path names and any foreign
*     file and format information from the DCB, leaving the DCB entry
*     empty (but still allocated).

*  Arguments:
*     DISPOS = LOGICAL (Given)
*        Whether the data object is to be released completely from the
*        NDF system. If a .FALSE. value is given, it indicates that it
*        will remain in use (via another DCB entry).
*     IDCB = INTEGER (Given)
*        Index to the DCB entry to be closed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine will attempt to execute even if STATUS is set on
*     entry, although no further error report will be made if it should
*     subsequently fail under these circumstances.

*  Copyright:
*     Copyright (C) 2000 Central Laboratories of the Research Councils

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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-OCT-1993 (RFWS):
*        Original version.
*     11-NOV-1993 (RFWS):
*        Only display deletion of objects associated with foreign
*        files.
*     11-NOV-1993 (RFWS):
*        Use the DCB copy of the keep NDF objects flag.
*     11-NOV-1993 (RFWS):
*        Only attempt to delete a foreign file if it is known to exist.
*     15-APR-1994 (RFWS):
*        Split up error message which was too long.
*     25-APR-1994 (RFWS):
*        Clear DCB_FORID value.
*     25-MAY-1994 (RFWS):
*        Delete dummy placeholder files when appropriate.
*     1-JUN-1994 (RFWS):
*        Decide which foreign format to use for file deletion at start
*        of routine.
*     23-DEC-1994 (RFWS):
*        Added post processing capability.
*     30-JAN-1995 (RFWS):
*        Improved error handling.
*     17-JUL-2000 (DSB):
*        Allow for foreign extension specifiers in DCB_FORFL.
*     9-FEB-2005 (DSB):
*        Check that conversion command exists before deleting the foreign
*        format file.
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
*        DCB_DSP( NDF__MXDCB ) = CHARACTER * ( NDF__SZDSP ) (Read)
*           Data object disposal mode.
*        DCB_FILE( NDF__MXDCB ) = CHARACTER * ( NDF__SZFIL ) (Write)
*           Data object container file name.
*        DCB_FOREX( NDF__MXDCB ) = LOGICAL (Read)
*           Whether the associated foreign file (if any) existed before
*           the NDF library accessed it.
*        DCB_FORFL( NDF__MXDCB ) = CHARACTER * ( NDF__SZFXS ) (Read and
*        Write)
*           Name of foreign format file associated with NDF. This may
*           optionally include a foreign extension specification.
*        DCB_FORID( NDF__MXDCB ) = CHARACTER * ( NDF__SZFID ) (Write)
*           Foreign file identification code.
*        DCB_FORKP( NDF__MXDCB ) = LOGICAL (Read)
*           Whether the NDF copy of the foreign file is to be kept.
*        DCB_IFMT( NDF__MXDCB ) = INTEGER (Read and Write)
*           Code to identify the format of any associated foreign file
*           (zero if no such file exists).
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Data object locator.
*        DCB_MOD( NDF__MXDCB ) = CHARACTER * ( NDF__SZMOD ) (Read)
*           The NDF's access mode.
*        DCB_PATH( NDF__MXDCB ) = CHARACTER * ( NDF__SZPTH ) (Write)
*           Data object HDS path name.

      INCLUDE 'NDF_TCB'          ! NDF_ Tuning Control Block
*        TCB_SHCVT = LOGICAL (Read)
*           Whether to display information about format conversion
*           operations.

      INCLUDE 'NDF_FCB'          ! NDF_ Format Conversion Block
*        FCB_FMT = CHARACTER * ( 2 * NDF__SZFMT ) (Read)
*           Foreign format list string.
*        FCB_FMT1( 2 * NDF__MXFMT ) = INTEGER (Read)
*           Character positions of start of each foreign format name.
*        FCB_FMT2( 2 * NDF__MXFMT ) = INTEGER (Read)
*           Character positions of end of each foreign format name.

*  Arguments Given:
      LOGICAL DISPOS
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZCVT ) CMD ! Buffer for raw command text
      INTEGER DELFMT             ! Format code for deletion operations
      INTEGER F1                 ! First format name character position
      INTEGER F2                 ! Last format name character position
      INTEGER IFMT               ! FCB foreign format code
      INTEGER LCMD               ! Length of blank command text
      LOGICAL CVT                ! Convert data format?
      LOGICAL DEF                ! Environment variable defined?
      LOGICAL DELFOR             ! Delete associated foreign file?
      LOGICAL POST               ! Post-processing required?
      LOGICAL SAVE               ! Save NDF object (else delete it)?
      LOGICAL THERE              ! Foreign file exists?

*.

*  Begin a new error reporting environment.
      CALL ERR_BEGIN( STATUS )

*  Ensure that the TCB is initialised.
      CALL NDF1_INTCB( STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain the FCB format code for the NDF.
         IFMT = DCB_IFMT( IDCB )

*  Determine whether to convert the NDF object back into a foreign
*  format before releasing it. This will be necessary if (a) it is
*  being released completely from the NDF system, and (b) it has a
*  foreign format associated with it, and (c) it was accessed with an
*  access mode which permitted modification, and (d) it is not due to be
*  deleted. Note, use of foreign extension specifiers is only allowed for
*  read-only access, and so we can assume there is no foreign extension
*  specifier in DCB_FORFL if conversion is found to be necessary.
         CVT = DISPOS .AND.
     :         ( IFMT .NE. 0 ) .AND.
     :         ( DCB_MOD( IDCB ) .NE. 'READ' ) .AND.
     :         ( DCB_DSP( IDCB ) .EQ. 'KEEP' )

*  Determine whether to retain the NDF object. It should be retained if
*  (a) it is not being completely released from the NDF system, or (b)
*  it is not due to be deleted and it has no foreign format file
*  associated with it (or the "keep NDF objects" flag is set).
         SAVE = ( .NOT. DISPOS ) .OR.
     :          ( ( DCB_DSP( IDCB ) .EQ. 'KEEP' ) .AND.
     :            ( ( IFMT .EQ. 0 ) .OR. DCB_FORKP( IDCB ) ) )

*  Determine which foreign format code should be used when deleting
*  foreign files. If the file did not exist prior to being accessed by
*  the NDF_ library, then only a dummy (empty) placeholder file will
*  exist, which can be deleted without knowing its format. However, if
*  the file existed before being accessed, then it will contain data
*  (and may have other files associated with it). In this case, an
*  external deletion command specific to the foreign format may be
*  required, and the user may also need to be informed of its deletion.
         DELFMT = 0
         IF ( DCB_FOREX( IDCB ) ) DELFMT = IFMT

*  Determine whether to delete a foreign format file. This will only be
*  required if (a) the NDF is being completely released, and (b) the NDF
*  is due to be deleted, and (c) there is a foreign format associated
*  with it.
         DELFOR = DISPOS .AND.
     :            ( DCB_DSP( IDCB ) .NE. 'KEEP' ) .AND.
     :            ( IFMT .NE. 0 )

*  Determine whether to perform post-processing on the dataset. This
*  will be necessary only if it is being completely released from the
*  NDF system.
         POST = DISPOS

*  If required, define standard message tokens for the post-processing
*  command which will be executed once the NDF has been released.
         IF ( POST ) THEN
            CALL NDF1_CVTOK( DCB_FORFL( IDCB ), IFMT, DCB_LOC( IDCB ),
     :                       ' ', STATUS )

*  Define additional message tokens for use by the post-processing
*  command.
            IF ( ( IFMT .EQ. 0 ) .OR. ( DCB_FORKP( IDCB ) ) ) THEN
               CALL MSG_SETC( 'KEEP', '1' )
            ELSE
               CALL MSG_SETC( 'KEEP', '0' )
            END IF
            IF ( DCB_MOD( IDCB ) .NE. 'READ' ) THEN
               CALL MSG_SETC( 'MOD', '1' )
            ELSE
               CALL MSG_SETC( 'MOD', '0' )
            END IF
            IF ( DCB_DSP( IDCB ) .NE. 'KEEP' ) THEN
               CALL MSG_SETC( 'DEL', '1' )
            ELSE
               CALL MSG_SETC( 'DEL', '0' )
            END IF

*  Mark the error stack to prevent subsequent operations from using
*  these token definitions, which will be recovered for use later.
            CALL ERR_MARK
         END IF

*  If conversion to a foreign file format is required...
         IF ( CVT ) THEN

*  Check that a conversion command is available. An error is reported if not.
            CALL NDF1_CVCMD( DCB_FORFL( IDCB ), IFMT, DCB_LOC( IDCB ),
     :                       ' ', .FALSE., .TRUE., DEF, CMD, LCMD,
     :                       STATUS )

*  If conversion to a foreign file format is required, and a conversion
*  command is available, then first delete any existing version of the file.
            IF( STATUS .EQ. SAI__OK .AND. LCMD .GT. 0 ) THEN
               CALL NDF1_DLFOR( DCB_FORFL( IDCB ), DELFMT, STATUS )

*  Convert the NDF to the foreign file format.
               CALL ERR_BEGIN( STATUS )
               CALL NDF1_CVFOR( DCB_FORFL( IDCB ), IFMT,
     :                          DCB_LOC( IDCB ), ' ', .FALSE.,
     :                          STATUS )

*  If this appears to have succeeded, then check that the foreign file
*  now exists. If not, then something has gone wrong with the
*  conversion process, so report an error.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL NDF1_FILEX( DCB_FORFL( IDCB ), ' ', .FALSE.,
     :                             THERE, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( .NOT. THERE ) THEN
                        STATUS = NDF__CVTER
                        CALL DAT_MSG( 'NDF', DCB_LOC( IDCB ) )
                        F1 = FCB_FMT1( IFMT )
                        F2 = FCB_FMT2( IFMT )
                        CALL MSG_SETC( 'FMT', FCB_FMT( F1 : F2 ) )
                        CALL MSG_SETC( 'FOR', DCB_FORFL( IDCB ) )
                        CALL ERR_REP( 'NDF1_CLFOR_CVT1',
     :                       'Error converting the NDF object ^NDF ' //
     :                       'to ^FMT format in the file ''^FOR''.',
     :                       STATUS )
                        CALL MSG_RENEW
                        CALL ERR_REP( 'NDF_CLFOR_CVT2',
     :                       'The ^FMT file was not created.', STATUS )
                     END IF
                  END IF
               END IF
               CALL ERR_END( STATUS )
            END IF
         END IF

*  If the NDF is being retained, then simply annul its DCB locator.
         IF ( SAVE ) THEN
            CALL DAT_ANNUL( DCB_LOC( IDCB ), STATUS )

*  Otherwise, if required, report that it is being deleted. Use a new
*  error reporting environment in case of earlier errors.
         ELSE
            CALL ERR_BEGIN( STATUS )
            IF ( ( IFMT .NE. 0 ) .AND. TCB_SHCVT ) THEN
               CALL DAT_MSG( 'NDF', DCB_LOC( IDCB ) )
               CALL MSG_OUT( ' ',
     :         '-->  Deleting: NDF object ^NDF', STATUS )
            END IF

*  Delete the NDF data object.
            CALL NDF1_DELOB( DCB_LOC( IDCB ), STATUS )
            CALL ERR_END( STATUS )
         END IF

*  Delete the associated foreign format file if necessary.
         IF ( DELFOR ) CALL NDF1_DLFOR( DCB_FORFL( IDCB ), DELFMT,
     :                                  STATUS )

*  If required, release the error stack, making the message tokens
*  defined earlier visible again. Execute any post-processing command
*  for the released dataset.
         IF ( POST ) THEN
            CALL ERR_RLSE

*  N.B. This is an experimental and undocumented feature and may be
*  ----------------------------------------------------------------
*  removed or changed in future.
*  ----------------------------
            CALL NDF1_AFFOR( IFMT, STATUS )
         END IF

*  Clear the DCB entries relating to the foreign file and NDF data
*  object just released.
         DCB_FILE( IDCB ) = ' '
         DCB_PATH( IDCB ) = ' '
         DCB_IFMT( IDCB ) = 0
         DCB_FORFL( IDCB ) = ' '
         DCB_FORID( IDCB ) = ' '
      END IF

*  Call error tracing routine.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CLFOR', STATUS )

*  End the error reporting environment.
      CALL ERR_END( STATUS )

      END
