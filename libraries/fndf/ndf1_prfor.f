      SUBROUTINE NDF1_PRFOR( IACB, IPCB, STATUS )
*+
*  Name:
*     NDF1_PRFOR

*  Purpose:
*     Propagate foreign format information to a PCB entry.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_PRFOR( IACB, IPCB, STATUS )

*  Description:
*     The routine modifies an existing PCB entry to take account of
*     information about a foreign file format propagated from an
*     existing NDF. If wild-carding of foreign file formats is enabled
*     for the PCB entry and an existing NDF is provided as a template,
*     then the PCB entry will be altered to use the same foreign file
*     format as the template (if no template is provided, the existing
*     PCB default foreign format will be confirmed). The routine will
*     then disable further wild-carding for the PCB entry.
*
*     This routine should be invoked on all new placeholder entries
*     before they are used to create new NDFs. If wild-carding of the
*     PCB entry is not enabled, it will return without action.

*  Arguments:
*     IACB = INTEGER (Given)
*        An ACB index identifying an optional template NDF from which
*        foreign file format information should be propagated. If no
*        template is required, then this argument should be set to
*        zero.
*     IPCB = INTEGER (Given)
*        A valid PCB index identifying the Placeholder Control Block
*        entry to be modified.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine operates in conjunction with NDF1_PLFOR, which should
*     be kept in step with any changes.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     3-NOV-1993 (RFWS):
*        Original version.
*     11-NOV-1993 (RFWS):
*        Use the PCB copy of the keep NDF objects flag.
*     9-MAR-1994 (RFWS):
*        Add support for PCB_NEW flag.
*     10-MAR-1994 (RFWS):
*        Added full expansion of foreign file names.
*     11-MAR-1994 (RFWS):
*        Added validation of foreign file names.
*     16-MAR-1994 (RFWS):
*        Revised NDF1_EXPFN argument list.
*     18-APR-1994 (RFWS):
*        Cater for anonymous format converters.
*     20-MAY-1994 (RFWS):
*        Cater for file version numbers if present.
*     25-MAY-1994 (RFWS):
*        Initialise PCB_FORID.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_IFMT( NDF__MXDCB ) = INTEGER (Read)
*           FCB format code for associated foreign file (zero if no
*           foreign file exists).

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

      INCLUDE 'NDF_PCB'          ! NDF_ Placeholder Control Block
*        PCB_FORFL( NDF__MXPCB ) = CHARACTER * ( NDF__SZFIL ) (Read and
*        Write)
*           Name of the foreign format file associated with the NDF.
*        PCB_FORID( NDF__MXPCB ) = CHARACTER * ( NDF__SZFID ) (Write)
*           Foreign format file identification code.
*        PCB_FORKP( NDF__MXPCB ) = LOGICAL (Read)
*           Whether the NDF copy of the foreign file is to be kept.
*        PCB_IFMT( NDF__MXPCB ) = INTEGER (Read and Write)
*           FCB format code identifying the format of the foreign file
*           associated with the NDF (zero if there is no foreign file).
*        PCB_LOC( NDF__MXPCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Locator to placeholder object.
*        PCB_NEW( NDF__MXPCB ) = LOGICAL (Write)
*           Whether a new placeholder object was created.
*        PCB_PRFMT( NDF__MXPCB ) = LOGICAL (Read and Write)
*           Whether foreign format information is to be propagated to
*           the PCB entry.

      INCLUDE 'NDF_FCB'          ! NDF_ Format Conversion Block
*        FCB_FEX1( 2 * NDF__MXFMT ) = INTEGER (Read)
*           Character positions of start of each foreign format file
*           extension.
*        FCB_FEX2( 2 * NDF__MXFMT ) = INTEGER (Read)
*           Character positions of end of each foreign format file
*           extension.
*        FCB_FMT = CHARACTER * ( 2 * NDF__SZFMT ) (Read)
*           Foreign format list string.

*  Arguments Given:
      INTEGER IACB
      INTEGER IPCB

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      CHARACTER * ( NDF__SZFID ) FORID ! Foreign file ID
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator for placeholder object
      CHARACTER * ( DAT__SZLOC ) NDFLOC ! Locator for native format NDF
      CHARACTER * ( NDF__SZFIL ) EXPFIL ! Expanded file name string
      CHARACTER * ( NDF__SZFIL ) FORFIL ! Foreign filename
      CHARACTER * ( NDF__SZREF ) NDFNAM ! Name for native format NDF
      INTEGER D1                 ! First character of directory field
      INTEGER D2                 ! Last character of directory field
      INTEGER F1                 ! First character of file extension
      INTEGER F2                 ! Last character of file extension
      INTEGER LEXP               ! Length of expanded file name
      INTEGER LFOR               ! Length of foreign file name
      INTEGER LNAM               ! Length of native format NDF name
      INTEGER N1                 ! First character of name field
      INTEGER N2                 ! Last character of name field
      INTEGER T1                 ! First character of type field
      INTEGER T2                 ! Last character of type field
      INTEGER V1                 ! First character of version field
      INTEGER V2                 ! Last character of version field
      LOGICAL NEW                ! New placeholder object created?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check to see if foreign format information can be propagted to the
*  placeholder entry (i.e. whether wild-carding of output formats is
*  enabled). If not, then there is nothing more to do.
      IF ( PCB_PRFMT( IPCB ) ) THEN

*  If an input NDF has been supplied, then propagate its foreign format
*  code (stored in the DCB) to the PCB entry.
         IF ( IACB .GT. 0 ) PCB_IFMT( IPCB ) =
     :                      DCB_IFMT( ACB_IDCB( IACB ) )

*  No foreign file.
*  ===============
*  If there is now no foreign format file associated with the NDF, then
*  create a new native format NDF placeholder object for direct use
*  using the original object name stored in the PCB.
         IF ( PCB_IFMT( IPCB ) .EQ. 0 ) THEN
            CALL NDF1_PLCRE( DAT__ROOT, PCB_FORFL( IPCB ), LOC, NEW,
     :                       STATUS )

*  Delete the (temporary) placeholder object previously associated with
*  the PCB entry and substitute the new one. Clear the foreign format
*  file name.
            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL NDF1_DELOB( PCB_LOC( IPCB ), STATUS )
               PCB_LOC( IPCB ) = LOC
               PCB_NEW( IPCB ) = NEW
               PCB_FORFL( IPCB ) = ' '
            END IF

*  Foreign file.
*  ============
*  If a foreign format file is associated with the NDF, then expand the
*  original object name stored in the PCB as a normal file name. If this
*  doesn't succeed, then annul the error and use the original name as
*  supplied.
         ELSE
            CALL ERR_MARK
            CALL NDF1_EXPFN( PCB_FORFL( IPCB ), .FALSE., EXPFIL, LEXP,
     :                       FORID, STATUS )
            LEXP = MAX( 1, LEXP )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )
               LEXP = MIN( MAX( 1, CHR_LEN( PCB_FORFL( IPCB ) ) ),
     :                     LEN( EXPFIL ) )
               EXPFIL( : LEXP ) = PCB_FORFL( IPCB )( : LEXP )
            END IF
            CALL ERR_RLSE

*  Split the resulting name into directory, name, type and version
*  fields.
            CALL NDF1_FSPLT( EXPFIL( : LEXP ), D1, D2, N1, N2, T1, T2,
     :                       V1, V2, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Build the foreign file name by concatenating the directory and name
*  fields, if they exist, and appending the appropriate file type
*  extension (note there should never be a file extension present on the
*  file name prior to this, as foreign format information is not
*  propagated to files with extensions). Append the file version number
*  field, if necessary.
               LFOR = 0
               IF ( D1 .LE. D2 )
     :            CALL CHR_PUTC( EXPFIL( D1 : D2 ), FORFIL, LFOR )
               IF ( N1 .LE. N2 )
     :            CALL CHR_PUTC( EXPFIL( N1 : N2 ), FORFIL, LFOR )
               F1 = FCB_FEX1( PCB_IFMT( IPCB ) )
               F2 = FCB_FEX2( PCB_IFMT( IPCB ) )
               CALL CHR_PUTC( FCB_FMT( F1 : F2 ), FORFIL, LFOR )
               IF ( V1 .LE. V2 )
     :            CALL CHR_PUTC( EXPFIL( V1 : V2 ), FORFIL, LFOR )

*  Re-expand the original name supplied, this time allowing any errors
*  to stand (this provides a measure of syntax checking). Then create a
*  dummy placeholder file. If an error is detected, report context
*  information.
               CALL NDF1_EXPFN( PCB_FORFL( IPCB ), .FALSE., EXPFIL,
     :                          LEXP, FORID, STATUS )
               CALL NDF1_CRFOR( FORFIL( : LFOR ), PCB_IFMT( IPCB ),
     :                          EXPFIL, LEXP, FORID, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  F1 = FCB_FMT1( PCB_IFMT( IPCB ) )
                  F2 = FCB_FMT2( PCB_IFMT( IPCB ) )
                  CALL MSG_SETC( 'FMT', FCB_FMT( F1 : F2 ) )
                  CALL MSG_SETC( 'FILE', PCB_FORFL( IPCB ) )
                  CALL ERR_REP( 'NDF1_PRFOR_FNM',
     :                 'Error in ^FMT format output file ' //
     :                 'specification ''^FILE''.', STATUS )
               END IF

*  Identify a native format NDF to be used and create it as a new
*  placeholder object.
               CALL NDF1_NTFOR( FORFIL( : LFOR ), PCB_IFMT( IPCB ),
     :                          PCB_FORKP( IPCB ), NDFLOC, NDFNAM, LNAM,
     :                          STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL NDF1_PLCRE( NDFLOC, NDFNAM( : LNAM ), LOC, NEW,
     :                             STATUS )
               END IF

*  Delete the (temporary) placeholder object previously associated with
*  the PCB entry and substitute the new one.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL NDF1_DELOB( PCB_LOC( IPCB ), STATUS )
                  PCB_LOC( IPCB ) = LOC
                  PCB_NEW( IPCB ) = NEW
               END IF

*  Store the foreign file name and its identification code in the PCB
*  entry.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  PCB_FORFL( IPCB ) = FORFIL( : LFOR )
                  PCB_FORID( IPCB ) = FORID
               END IF
            END IF
         END IF

*  If OK, note that foreign format information can no longer be
*  propagated to the PCB entry.
         IF ( STATUS .EQ. SAI__OK ) PCB_PRFMT( IPCB ) = .FALSE.
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_PRFOR', STATUS )

      END
