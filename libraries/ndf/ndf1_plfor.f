      SUBROUTINE NDF1_PLFOR( LOC, NAME, IPCB, STATUS )
*+
*  Name:
*     NDF1_PLFOR

*  Purpose:
*     Create a PCB placeholder entry for an NDF possibly associated with
*     a foreign file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_PLFOR( LOC, NAME, IPCB, STATUS )

*  Description:
*     The routine creates a PCB placeholder entry for a new NDF which
*     may be associated with a foreign format file, and returns its PCB
*     index.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        Locator which, in conjunction with the NAME value, identifies
*        the new NDF. A value of DAT__ROOT may be given to indicate
*        that NAME contains the absolute name of an NDF object or of a
*        foreign format file.
*     NAME = CHARACTER * ( * ) (Given)
*        Name of the new NDF. If LOC is set to DAT__ROOT, this should
*        be an absolute NDF name, or the name of a foreign format file.
*        Otherwise it should be a relative NDF name.
*     IPCB = INTEGER (Returned)
*        Index to the new PCB entry.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If this routine is called with STATUS set, then a value of zero
*     will be returned for the IPCB argument. The same value will also
*     be returned if the routine should fail for any reason.
*     -  This routine operates in conjunction with NDF1_PRFOR, which
*     should be kept in step with any changes.

*  Copyright:
*     Copyright (C) 2000 Science & Engineering Research Council

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
*     25-OCT-1993 (RFWS):
*        Original version.
*     12-NOV-1993 (RFWS):
*        Changed to handle '.' as an output format specification
*        (meaning native NDF format).
*     10-MAR-1994 (RFWS):
*        Added full expansion of foreign file names.
*     11-MAR-1994 (RFWS):
*        Added validation of foreign format file names.
*     16-MAR-1994 (RFWS):
*        Revised NDF1_EXPFN argument list.
*     18-APR-1994 (RFWS):
*        Cater for file type extensions containing '.'.
*     20-MAY-1994 (RFWS):
*        Cater for file version numbers if present.
*     15-NOV-1994 (RFWS):
*        Made test for file extension containing '.' character more
*        secure.
*     11-MAR-1997 (RFWS):
*        Add use of TCB_DOCVT flag to control access to foreign data files.
*     17-JUL-2000 (DSB):
*        Check that no foreign extension specifier is included in the
*        supplied object name.
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
      INCLUDE 'NDF_ERR'          ! NDF_ error constants

*  Global Variables:
      INCLUDE 'NDF_PCB'          ! NDF_ Placeholder Control Block
*        PCB_FORFL( NDF__MXPCB ) = CHARACTER * ( NDF__SZFIL ) (Write)
*           Name of the foreign format file associated with the NDF.
*        PCB_FORKP( NDF__MXPCB ) = LOGICAL (Write)
*           Whether the NDF copy of the foreign file is to be kept.
*        PCB_IFMT( NDF__MXPCB ) = INTEGER (Write)
*           FCB format code identifying the format of the foreign file
*           associated with the NDF (zero if there is no foreign file).
*        PCB_PRFMT( NDF__MXPCB ) = LOGICAL (Write)
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
*        FCB_FMT1( 2 * NDF__MXFMT ) = INTEGER (Read)
*           Character positions of start of each foreign format name.
*        FCB_FMT2( 2 * NDF__MXFMT ) = INTEGER (Read)
*           Character positions of end of each foreign format name.
*        FCB_NOUT = INTEGER (Read)
*           Number of foreign formats to recognise on output.

      INCLUDE 'NDF_TCB'          ! NDF_ Tuning Control Block
*        TCB_DOCVT = LOGICAL (Read)
*           Do format conversions flag.
*        TCB_KEEP = LOGICAL (Read)
*           Keep NDF data objects flag.

*  Arguments Given:
      CHARACTER * ( * ) LOC
      CHARACTER * ( * ) NAME

*  Arguments Returned:
      INTEGER IPCB

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) NDFLOC ! Locator for associated NDF
      CHARACTER * ( NDF__SZFID ) FORID ! Foreign file ID
      CHARACTER * ( NDF__SZFIL ) EXPFIL ! Expanded file name string
      CHARACTER * ( NDF__SZFIL ) FORFIL ! Foreign file name
      CHARACTER * ( NDF__SZREF ) NDFNAM ! Name of associated native NDF
      INTEGER D1                 ! First character of directory field
      INTEGER D2                 ! Last character of directory field
      INTEGER F                  ! First character position in name
      INTEGER F1                 ! First character of file extension
      INTEGER F2                 ! Last character of file extension
      INTEGER IFMT               ! Loop counter for foreign formats
      INTEGER L                  ! Last character position in name
      INTEGER LEXP               ! Length of expanded file name string
      INTEGER LFOR               ! Length of foreign file name
      INTEGER LNAM               ! Number of characters in NDF name
      INTEGER N1                 ! First character of name field
      INTEGER N2                 ! Last character of name field
      INTEGER T1                 ! First character of type field
      INTEGER T2                 ! Last character of type field
      INTEGER TMIN               ! Anticipated start of type field
      INTEGER V1                 ! First character of version field
      INTEGER V2                 ! Last character of version field
      INTEGER X1                 ! First character of foreign extension field
      INTEGER X2                 ! Last character of foreign extension field
      LOGICAL CVT                ! Conversion required?
      LOGICAL FOUND              ! Output file identified?
      LOGICAL WILD               ! Output format wild-carded?

*.

*  Set an initial null value for the IPCB argument.
      IPCB = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that the TCB and FCB are initialised.
      CALL NDF1_INTCB( STATUS )
      CALL NDF1_INFCB( STATUS )

*  Initialise flags.
      CVT = .FALSE.
      WILD = .FALSE.

*  No foreign formats.
*  ==================
*  If an active input locator has been supplied (indicating that we
*  need not consider a foreign format file), or there are no foreign
*  data formats to be recognised on output, or the TCB_DOCVT flag is
*  .FALSE. indicating that foreign format conversions are not required,
*  then simply create the required new placeholder entry in the PCB.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( ( LOC .NE. DAT__ROOT ) .OR.
     :        ( FCB_NOUT .EQ. 0 ) .OR.
     :        ( .NOT. TCB_DOCVT ) ) THEN
            CALL NDF1_NPLAC( LOC, NAME, IPCB, STATUS )

*  Foreign formats.
*  ===============
*  If there are foreign formats to be recognised, then check that no
*  foreign extension specifier is included in the object name.
*  Foreign extension specifiers may only be used when specifying existing
*  NDFs, not new ones.
         ELSE
            CALL NDF1_FORXT( NAME, X1, X2, STATUS )
            IF( X1 .LE. X2 .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = NDF__ACDEN
               CALL MSG_SETC( 'FILE', NAME )
               CALL ERR_REP( 'NDF1_PLFOR_FXS1',
     :                 'Error in foreign format output file ' //
     :                 'specification ''^FILE''.', STATUS )
               CALL MSG_SETC( 'EX', NAME( X1 : X2 ) )
               CALL ERR_REP( 'NDF1_PLFOR_FXS2', 'Extension '//
     :            'specifiers such as ''^EX'' may only be used when'//
     :            ' accessing existing foreign format NDFs.', STATUS )
            END IF

*  Mark the error stack and attempt to expand the object name as if it was
*  a normal file name. If this doesn't succeed (we may actually have an
*  NDF name whose syntax is not valid for the host operating system, for
*  instance), then annul the error and use the original name as supplied.
            CALL ERR_MARK
            CALL NDF1_EXPFN( NAME, .FALSE., EXPFIL, LEXP, FORID,
     :                       STATUS )
            LEXP = MAX( 1, LEXP )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )
               LEXP = MIN( MAX( 1, CHR_LEN( NAME ) ), LEN( EXPFIL ) )
               EXPFIL( : LEXP ) = NAME( : LEXP )
            END IF
            CALL ERR_RLSE

*  Interpret the resulting name as a foreign file name and split it into
*  directory, name, type and version fields (any of which may be
*  absent).
            CALL NDF1_FSPLT( EXPFIL( : LEXP ), D1, D2, N1, N2, T1, T2,
     :                       V1, V2, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  File type present.
*  =================
*  If a file type field appears to be present, then we must determine
*  whether it identifies a foreign format file. Loop to test against
*  each recognised foreign output format.
               IF ( T1 .LE. T2 ) THEN
                  FOUND = .FALSE.
                  DO 1 IFMT = NDF__MXFMT + 1, NDF__MXFMT + FCB_NOUT

*  Obtain the character string limits of the file extension in the FCB
*  foreign formats list string.
                     F1 = FCB_FEX1( IFMT )
                     F2 = FCB_FEX2( IFMT )

*  Since the file extension may contain a '.' character, it may actually
*  be longer than identified above (i.e. the end of the name field may
*  still contain the first part of the file extension). Find the first
*  character position at which the full file extension field could
*  start (allowing it to extend into the name field, if present, but not
*  into the directory field).
                     TMIN = T1
                     IF ( N2 .GE. N1 ) TMIN = N1

*  Adjust the anticipated starting position for the expected file
*  extension.
                     TMIN = MIN( MAX( TMIN, T2 - ( F2 - F1 ) ), T1 )

*  Test if the file extension field matches (be case sensitive if
*  necessary).
                     IF ( ( FCB_FMT( F1 : F2 ) .NE. '*' ) .AND.
     :                    ( FCB_FMT( F1 : F2 ) .NE. '.' ) ) THEN
                        CALL NDF1_CMPFL( EXPFIL( TMIN : T2 ),
     :                                   FCB_FMT( F1 : F2 ), FOUND,
     :                                   STATUS )

*  Quit searching if a match is found or an error occurs.
                        IF ( FOUND .OR.
     :                       ( STATUS .NE. SAI__OK ) ) GO TO 2
                     END IF
 1                CONTINUE
 2                CONTINUE

*  If the file name extension was not recognised, then interpret the
*  name supplied as the name of a native NDF object and create the
*  required new placeholder entry directly in the PCB, using the
*  original name as supplied.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( .NOT. FOUND ) THEN
                        CALL NDF1_NPLAC( DAT__ROOT, NAME, IPCB, STATUS )

*  Otherwise, note that conversion to a foreign format file must be
*  handled and save the foreign file name.
                     ELSE
                        CVT = .TRUE.
                        LFOR = LEXP
                        FORFIL( : LFOR ) = EXPFIL( : LEXP )
                     END IF
                  END IF

*  No file type present.
*  ====================
*  If no file type field appears to be present (but at least one foreign
*  format is to be recognised on output), then we must search the output
*  foreign format list for the first output format which can be used.
               ELSE
                  FOUND = .FALSE.
                  DO 3 IFMT = NDF__MXFMT + 1, NDF__MXFMT + FCB_NOUT

*  Obtain the character string limits of the format name in the FCB
*  foreign formats list string.
                     F1 = FCB_FMT1( IFMT )
                     F2 = FCB_FMT2( IFMT )

*  Note if wild-carding of output formats is enabled and quit searching
*  at the first output format which is not wild-carded. Set FOUND to
*  indicate if this was a foreign format specification or not.
                     IF ( FCB_FMT( F1 : F2 ) .EQ. '*' ) THEN
                        WILD = .TRUE.
                     ELSE
                        FOUND = ( FCB_FMT( F1 : F2 ) .NE. '.' )
                        GO TO 4
                     END IF
 3                CONTINUE
 4                CONTINUE

*  If no foreign output format was found because a format specification
*  of '.' was found (indicating native NDF format) without any previous
*  wild-card specification, then simply create the required new
*  placeholder entry in the PCB.
                  IF ( .NOT. FOUND ) THEN
                     IF ( .NOT. WILD ) THEN
                        CALL NDF1_NPLAC( LOC, NAME, IPCB, STATUS )

*  If a wild-card specification was found first, then set IFMT to zero,
*  indicating that native NDF format should be used. Note that since
*  wild-carding may yet override this format, the possibility of format
*  conversion must still be catered for.
                     ELSE
                        IFMT = 0
                        CVT = .TRUE.
                     END IF

*  If a foreign output format was identified, then construct the full
*  foreign file name from its directory and name fields (if they
*  exist), with the appropriate file extension appended. Append the file
*  version number field, if necessary.
                  ELSE
                     CVT = .TRUE.
                     LFOR = 0
                     IF ( D1 .LE. D2 ) CALL CHR_PUTC( EXPFIL( D1 : D2 ),
     :                                                FORFIL, LFOR )
                     IF ( N1 .LE. N2 ) CALL CHR_PUTC( EXPFIL( N1 : N2 ),
     :                                                FORFIL, LFOR )
                     F1 = FCB_FEX1( IFMT )
                     F2 = FCB_FEX2( IFMT )
                     CALL CHR_PUTC( FCB_FMT( F1 : F2 ), FORFIL, LFOR )
                     IF ( V1 .LE. V2 ) CALL CHR_PUTC( EXPFIL( V1 : V2 ),
     :                                                FORFIL, LFOR )
                  END IF
               END IF
            END IF
         END IF
      END IF

*  Conversion required.
*  ===================
*  If conversion to a foreign file is required and wild-carding is not
*  enabled, then we can be sure of which foreign format is involved.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( CVT ) THEN
            IF ( .NOT. WILD ) THEN

*  Attempt to validate the foreign file name by re-expanding the
*  original name supplied, this time allowing any errors to stand. Then
*  create a dummy placeholder file. If an error is detected, report
*  context information.
               CALL NDF1_EXPFN( NAME, .FALSE., EXPFIL, LEXP, FORID,
     :                          STATUS )
               CALL NDF1_CRFOR( FORFIL( : LFOR ), IFMT, EXPFIL, LEXP,
     :                          FORID, STATUS )
               LEXP = MAX( LEXP, 1 )
               IF ( STATUS .NE. SAI__OK ) THEN
                  F1 = FCB_FMT1( IFMT )
                  F2 = FCB_FMT2( IFMT )
                  CALL MSG_SETC( 'FMT', FCB_FMT( F1 : F2 ) )
                  CALL MSG_SETC( 'FILE', NAME )
                  CALL ERR_REP( 'NDF1_PLFOR_FNM',
     :                 'Error in ^FMT format output file ' //
     :                 'specification ''^FILE''.', STATUS )
               END IF

*  Identify a native format NDF object to be associated with the
*  foreign file and create a PCB placeholder entry for it.
               CALL NDF1_NTFOR( EXPFIL( : LEXP ), IFMT, TCB_KEEP,
     :                          NDFLOC, NDFNAM, LNAM, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL NDF1_NPLAC( NDFLOC, NDFNAM( : LNAM ), IPCB,
     :                             STATUS )
               END IF

*  Store the foreign format code, expanded file name, and file
*  identification code in the PCB. Also note whether the NDF copy of the
*  foreign file is to be kept.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  PCB_IFMT( IPCB ) = IFMT
                  PCB_FORFL( IPCB ) = EXPFIL( : LEXP )
                  PCB_FORID( IPCB ) = FORID
                  PCB_FORKP( IPCB ) = TCB_KEEP

*  Disable subsequent propagation of foreign format information to the
*  PCB entry.
                  PCB_PRFMT( IPCB ) = .FALSE.
               END IF

*  If wild-carding was enabled, then we cannot be sure which foreign
*  format is involved (or indeed whether any foreign format is involved)
*  until the NDF is actually created. We therefore proceed by assuming
*  that conversion will be needed, identifying a default native format
*  NDF to be associated with a foreign file and creating a PCB
*  placeholder entry for it. This (possibly wrong) assumption will be
*  reversed by the NDF1_PRFOR routine when it propagates foreign format
*  information to the PCB entry. Assume that the native format NDF will
*  not be kept, so that a temporary object will be created and we need
*  not supply a foreign file name (this also avoids possible errors from
*  attempting to create a permanent file which may turn out not to be
*  needed).
            ELSE
               CALL NDF1_DNFOR( ' ', 0, .FALSE., NDFLOC, NDFNAM, LNAM,
     :                          STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL NDF1_NPLAC( NDFLOC, NDFNAM( : LNAM ), IPCB,
     :                             STATUS )
               END IF

*  Store the foreign format code (which will act as a default) and the
*  object name in the PCB and note whether the NDF copy of the foreign
*  file is to be kept. Note that we store the original (unprocessed and
*  unvalidated) name, since it may require re-interpretation once the
*  foreign file format is known.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  PCB_IFMT( IPCB ) = IFMT
                  PCB_FORFL( IPCB ) = NAME
                  PCB_FORKP( IPCB ) = TCB_KEEP

*  Enable subsequent propagation of foreign format information to the
*  PCB entry.
                  PCB_PRFMT( IPCB ) = .TRUE.
               END IF
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_PLFOR', STATUS )

      END
