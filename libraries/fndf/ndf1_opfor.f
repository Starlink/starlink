      SUBROUTINE NDF1_OPFOR( LOC, NAME, MODE, IACB, STATUS )
*+
*  Name:
*     NDF1_OPFOR

*  Purpose:
*     Open an NDF, possibly converting from a foreign format file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_OPFOR( LOC, NAME, MODE, IACB, STATUS )

*  Description:
*     The routine opens an existing NDF for a specified form of access,
*     importing it into the NDF_ system and returning an ACB index for
*     it. If the object specified does not exist in native NDF format,
*     then a search may be made (according to the contents of the
*     Format Conversion Block and the Tuning Control Block) for a
*     suitable foreign format file which may be converted into NDF
*     format and then accessed. If a foreign format file is accessed,
*     then the NDF name supplied is interpreted as the name of that
*     file, although in all cases a subscript expression may also be
*     appended.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        Locator which, in conjunction with the NAME value, identifies
*        the NDF to be opened. A value of DAT__ROOT may be given to
*        indicate that NAME contains the absolute name of an NDF object
*        or of a foreign format file.
*     NAME = CHARACTER * ( * ) (Given)
*        Name of the NDF to be opened. If LOC is set to DAT__ROOT, this
*        should be an absolute NDF name, or the name of a foreign
*        format file (with an optional subscript expression appended).
*        Otherwise it should be a relative NDF name.
*     MODE = CHARACTER * ( * ) (Given)
*        The required mode of access to the NDF ('READ', 'UPDATE' or
*        'WRITE').
*     IACB = INTEGER (Returned)
*        Index of the new NDF entry in the ACB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     If this routine is called with STATUS set, then a value of zero
*     will be returned for the IACB argument, although no further
*     processing will occur.  The same value will also be returned if
*     the routine should fail for any reason.

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
*     13-OCT-1993 (RFWS):
*        Original version.
*     11-NOV-1993 (RFWS):
*        Store the TCB_KEEP value in the DCB for later use.
*     10-MAR-1994 (RFWS):
*        Added full expansion of foreign file names.
*     16-MAR-1994 (RFWS):
*        Added support for file identification codes.
*     15-APR-1994 (RFWS):
*        Allow recognition of file extensions with '.' characters in.
*     25-APR-1994 (RFWS):
*        Search for a native format NDF if we fail to find a foreign
*        file with an explicit file type extension.
*     27-APR-1994 (RFWS):
*        Mark converted NDF container files as scratch files if
*        necessary.
*     29-APR-1994 (RFWS):
*        Improved handling of "file not found" type errors following
*        format conversion failures.
*     25-MAY-1994 (RFWS):
*        Initialise the DCB_FOREX value for pre-existing foreign files.
*     15-NOV-1994 (RFWS):
*        Made test for file extension containing '.' character more
*        secure.
*     11-MAR-1997 (RFWS):
*        Add use of TCB_DOCVT flag to control access to foreign data files.
*     17-JUL-2000 (DSB):
*        Added support for foreign extension specifiers.
*     10-OCT-2002 (AJC):
*        Special stuff for foreign FTP extension
*     4-JUN-2003 (AJC):
*        Further mods to accept any URL (:// present) for NDF name. Invokes
*        the 'conversion' command in NDF_FROM_URL
*     7-OCT-2004 (AJC):
*        Stylistic corrections to Alan's changes prior to inclusion in
*        CVS repository.
*     21-FEB-2006 (DSB):
*        Changed length of FEXT from 6 to 20 since 6 is not enough for
*        extensions such as ".sdf.gz" (which is 7 characters long).
*     31-OCT-2007 (DSB):
*        Add call to NDF1_EVENT.
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
*        DCB_FOREX( NDF__MXDCB ) = LOGICAL (Write)
*           Whether the associated foreign file (if any) existed before
*           the NDF library accessed it.
*        DCB_FORFL( NDF__MXDCB ) = CHARACTER * ( NDF__SZFXS ) (Write)
*           Name of foreign format file associated with NDF. This may
*           optionally include a foreign extension specification.
*        DCB_FORID( NDF__MXDCB ) = CHARACTER * ( NDF__SZFID ) (Write)
*           Unique ID for foreign format file associated with NDF.
*        DCB_FORKP( NDF__MXDCB ) = LOGICAL (Write)
*           Whether the NDF copy of the foreign file is to be kept.
*        DCB_IFMT( NDF__MXDCB ) = INTEGER (Write)
*           FCB index identifying the format of the associated foreign
*           file.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.
*        DCB_MOD( NDF__MXDCB ) = CHARACTER * ( NDF__SZMOD ) (Read and
*        Write)
*           The NDF's access mode.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

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
*        FCB_NIN = INTEGER (Read)
*           Number of foreign formats to recognise on input.

      INCLUDE 'NDF_TCB'          ! NDF_ Tuning Control Block
*        TCB_DOCVT = LOGICAL (Read)
*           Do format conversions flag.
*        TCB_KEEP = LOGICAL (Read)
*           Keep NDF data objects flag.

*  Arguments Given:
      CHARACTER * ( * ) LOC
      CHARACTER * ( * ) NAME
      CHARACTER * ( * ) MODE

*  Arguments Returned:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of string
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison
      LOGICAL NDF1_ABSNT         ! Test for absent NDF or component

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) NDFLOC ! Locator for associated NDF
      CHARACTER * ( NDF__SZFID ) FORID  ! Foreign format file ID
      CHARACTER * ( NDF__SZFIL ) EXPFIL ! Expanded file name string
      CHARACTER * ( NDF__SZFIL ) FORFIL ! Foreign file name
      CHARACTER * ( NDF__SZMOD ) VMODE  ! Validated access mode string
      CHARACTER * ( NDF__SZREF ) NDFNAM ! Name of associated native NDF
      CHARACTER * 20 FEXT        ! Foreign filename extension
      INTEGER D1                 ! First character of directory field
      INTEGER D2                 ! Last character of directory field
      INTEGER F1                 ! First character of file extension
      INTEGER F2                 ! Last character of file extension
      INTEGER IACBT              ! Temporary ACB index
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER IFMT               ! Loop counter for foreign formats
      INTEGER LEXP               ! Length of expanded file name string
      INTEGER LFOR               ! Number of characters in file name
      INTEGER LNAM               ! Number of characters in NDF name
      INTEGER N1                 ! First character of name field
      INTEGER N2                 ! Last character of name field
      INTEGER NEXT               ! Next DCB entry to consider
      INTEGER O1                 ! First character of object name
      INTEGER O2                 ! Last character of object name
      INTEGER S1                 ! First character of subscripts
      INTEGER S2                 ! Last character of subscripts
      INTEGER T1                 ! First character of type field
      INTEGER T2                 ! Last character of type field
      INTEGER TMIN               ! Anticipated start of type field
      INTEGER V1                 ! First character of version field
      INTEGER V2                 ! Last character of version field
      INTEGER X1                 ! First character of foreign extension field
      INTEGER X2                 ! Last character of foreign extension field
      INTEGER XX1                ! First character of foreign extension field
      INTEGER XX2                ! Last character of foreign extension field
      LOGICAL URL                ! Name is URL
      LOGICAL ACTIVE             ! NDF is already in use?
      LOGICAL CVT                ! Conversion required?
      LOGICAL FOUND              ! Input file identified?
      LOGICAL REPORT             ! Report error if file does not exist?

*.
*  Set an initial null value for the IACB argument.
      IACB = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that the TCB and FCB are initialised.
      CALL NDF1_INTCB( STATUS )
      CALL NDF1_INFCB( STATUS )

*  Validate the access mode string.
      CALL NDF1_VMOD( MODE, VMODE, STATUS )

*  Initialise.
      CVT = .FALSE.
      URL = .FALSE.

*  No foreign formats.
*  ==================
*  If an active input locator has been supplied (indicating that we
*  need not consider a foreign format file), or there are no foreign
*  data formats to be recognised on input, or the TCB_DOCVT flag is
*  .FALSE. indicating that foreign format conversions are not required,
*  then open the NDF directly as a native format object.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( ( LOC .NE. DAT__ROOT ) .OR.
     :        ( FCB_NIN .EQ. 0 ) .OR.
     :        ( .NOT. TCB_DOCVT ) ) THEN
            CALL NDF1_NFIND( LOC, NAME, VMODE, IACB, STATUS )

*  Foreign formats.
*  ===============
*  If there are foreign formats to be recognised, then first check for URI's
*  and construct an appropriate standard foreign file format, then split the
*  NDF name into an object name and an (optional) subscript expression.
         ELSE
            IF( INDEX( NAME, '://' ) .GT. 0 ) URL = .TRUE.
            CALL NDF1_NSPLT( NAME, .FALSE., O1, O2, S1, S2, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  The object name found above may include a foreign extension specifier
*  (e.g. a FITS extension). Locate the start and and of any such string.
               CALL NDF1_FORXT( NAME( : O2 ), X1, X2, STATUS )

*  At the moment, foreign extension specifiers can only be used when
*  converting from a foreign format to NDF, not the other way round.
*  Therefore, report an error if the object name includes a foreign
*  extension specifier and the access mode is not READ.
               IF ( VMODE .NE. 'READ' .AND. X1 .LE. X2 .AND.
     :              STATUS .EQ. SAI__OK ) THEN
                  STATUS = NDF__ACDEN
                  CALL MSG_SETC( 'MODE', VMODE )
                  CALL MSG_SETC( 'FILE', NAME )
                  CALL ERR_REP( 'NDF1_OPFOR_FXSA1', 'Unable to open '//
     :            'the foreign format file ''^FILE'' for ^MODE access.',
     :                           STATUS )
                  CALL MSG_SETC( 'EX', NAME( X1 : X2 ) )
                  CALL ERR_REP( 'NDF1_OPFOR_FXSA2', 'Extension '//
     :            'specifiers such as ''^EX'' may only be used if the'//
     :            ' foreign file is accessed read-only.', STATUS )
               END IF

*  Adjust the index of the end of the object name to exclude any foreign
*  extension specifier. If there is no foreign extension specifier, this
*  will leave O2 unchanged.
               O2 = X1 - 1

*  Mark the error stack and attempt to expand the object name as if it
*  was a normal file name. If this doesn't succeed (we may actually have
*  an NDF name whose syntax is not valid for the host operating system,
*  for instance), then annul the error and use the original name as
*  supplied.
               CALL ERR_MARK
               IF( URL ) THEN
                  LEXP = MIN( MAX( 1, O2 - 01 + 1 ), LEN( EXPFIL ) )
                  EXPFIL( :LEXP ) = NAME( O1 : O2 )
               ELSE
                  CALL NDF1_EXPFN( NAME( O1 : O2 ), .FALSE., EXPFIL,
     :                             LEXP, FORID, STATUS )
                  LEXP = MAX( 1, LEXP )
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_ANNUL( STATUS )
                     LEXP = MIN( MAX( 1, O2 - O1 + 1 ), LEN( EXPFIL ) )
                     EXPFIL( : LEXP ) = NAME( O1 : O2 )
                  END IF
               END IF
               CALL ERR_RLSE

*  Split the resulting name into directory, name, type and version
*  fields (any of which may be absent).
               CALL NDF1_FSPLT( EXPFIL( : LEXP ), D1, D2, N1, N2,
     :                          T1, T2, V1, V2, STATUS )
            END IF

*  File type present.
*  =================
*  If a file type field appears to be present, then we must determine
*  whether it identifies a foreign format file. Loop to test against
*  each recognised foreign input format.
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( URL .OR. ( T1 .LE. T2 ) ) THEN
                  FOUND = .FALSE.
                  DO 1 IFMT = 1, FCB_NIN

*  Obtain the character string limits for the file extension in the FCB
*  format list string.
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
*  necessary). We pretend that the a file type of ".URL" has been
*  supplied if the expanded file looks like a URL. This is just a trick
*  for picking up the correct conversion command.
                     IF( URL ) THEN
                        FEXT = '.URL'
                     ELSE
                        FEXT = EXPFIL( TMIN : T2 )
                     END IF
                     CALL NDF1_CMPFL( FEXT,
     :                                FCB_FMT( F1 : F2 ), FOUND,
     :                                STATUS )

*  Quit searching if a match is found or an error occurs.
                     IF ( FOUND .OR. ( STATUS .NE. SAI__OK ) ) GO TO 2
 1                CONTINUE
 2                CONTINUE

*  If the file name extension was not recognised, then open the NDF
*  directly as a native format object, using the original name string.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( .NOT. FOUND ) THEN
                        CALL NDF1_NFIND( DAT__ROOT, NAME, VMODE, IACB,
     :                                   STATUS )

*  Otherwise, a foreign format file has probably been specified. Flag
*  that conversion is required, mark the error stack and check whether
*  the foreign file exists and is accessible (request an error message
*  if it is not).
                     ELSE
                        CVT = .TRUE.
                        CALL ERR_MARK

                        IF( URL ) THEN
                           REPORT = .FALSE.
                           NDFLOC = DAT__ROOT
                        ELSE
                           REPORT = .TRUE.
                        END IF

                        CALL NDF1_FILEX( EXPFIL( : LEXP ), VMODE,
     :                                   REPORT, FOUND, STATUS )

*  If no foreign file was found, then report contextual information.
                        IF ( STATUS .NE. SAI__OK ) THEN
                           F1 = FCB_FMT1( IFMT )
                           F2 = FCB_FMT2( IFMT )
                           CALL MSG_SETC( 'FMT', FCB_FMT( F1 : F2 ) )
                           CALL ERR_REP( 'NDF1_OPFOR_FNF1',
     :                          'Error searching for ^FMT format file.',
     :                          STATUS )

*  Attempt to open the NDF as a native format object instead, using the
*  original name string. Begin a new error reporting environment, since
*  we are attempting to recover from an earlier error.
                           CALL ERR_BEGIN( STATUS )
                           CALL NDF1_NFIND( DAT__ROOT, NAME, VMODE,
     :                                      IACB, STATUS )

*  Note if a native NDF object was found, otherwise annul the error. End
*  the error reporting environment.
                           IF ( STATUS .EQ. SAI__OK ) THEN
                              CVT = .FALSE.
                           ELSE
                              CALL ERR_ANNUL( STATUS )
                           END IF
                           CALL ERR_END( STATUS )
                        END IF

*  If a native NDF was found, then annul the previous error (failure to
*  find a foreign file).
                        IF ( .NOT. CVT ) CALL ERR_ANNUL( STATUS )
                        CALL ERR_RLSE

*  Conversion required.
*  ===================
*  If conversion from a foreign file is required, then re-expand the
*  original file name, this time requesting that a file identification
*  code be returned (now that we know the file exists and is
*  accessible). Save the results for later use.
                        IF ( CVT ) THEN
                           IF( URL ) THEN
                              FORFIL = EXPFIL
                              LFOR = LEXP
                              FORID = ' '
                           ELSE
                              CALL NDF1_EXPFN( NAME( O1 : O2 ), .TRUE.,
     :                                         FORFIL, LFOR, FORID,
     :                                         STATUS )
                           END IF
                           LFOR = MAX( 1, LFOR )
                        END IF
                     END IF
                  END IF

*  No file type present.
*  ====================
*  If no file type appears to be present, then first mark the error
*  stack and try to open the NDF directly as a native format object.
*  Note if a native format NDF is found.
               ELSE
                  FOUND = .FALSE.
                  CALL ERR_MARK
                  CALL NDF1_NFIND( DAT__ROOT, NAME, VMODE, IACB,
     :                             STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     FOUND = .TRUE.

*  If there is no native format NDF with this name, then annul the
*  error. We must now search for a foreign format file.
                  ELSE IF ( NDF1_ABSNT( STATUS ) ) THEN
                     CALL ERR_ANNUL( STATUS )
                  END IF
                  CALL ERR_RLSE

*  If necessary, loop to look for a file with each of the recognised
*  foreign input formats in turn, until one is found.
                  IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :                 ( .NOT. FOUND ) ) THEN
                     DO 3 IFMT = 1, FCB_NIN

*  Obtain the character string limits for the file extension in the FCB
*  format list string.
                        F1 = FCB_FEX1( IFMT )
                        F2 = FCB_FEX2( IFMT )

*  Construct the name of the file by appending the foreign format file
*  type field to the rest of the file name. Also append the version
*  number field if present.
                        LFOR = 0
                        IF ( D1 .LE. D2 )
     :                       CALL CHR_PUTC( EXPFIL( D1 : D2 ),
     :                                      FORFIL, LFOR )
                        IF ( N1 .LE. N2 )
     :                       CALL CHR_PUTC( EXPFIL( N1 : N2 ),
     :                                      FORFIL, LFOR )
                        CALL CHR_PUTC( FCB_FMT( F1 : F2 ), FORFIL,
     :                                 LFOR )
                        IF ( V1 .LE. V2 )
     :                       CALL CHR_PUTC( EXPFIL( V1 : V2 ),
     :                                      FORFIL, LFOR )

*  Check whether the file exists (do not report an error if it does
*  not).
                        CALL NDF1_FILEX( FORFIL( : LFOR ), ' ',
     :                                   .FALSE., FOUND, STATUS )

*  If a suitable file has been found, check whether it is accessible
*  (this time, request an error report if it is not).
                        IF ( ( STATUS .EQ. SAI__OK ) .AND. FOUND ) THEN
                           CALL NDF1_FILEX( FORFIL( : LFOR ), VMODE,
     :                                      .TRUE., FOUND, STATUS )

*  If it is not accessible, then report contextual information.
                           IF ( STATUS .NE. SAI__OK ) THEN
                              F1 = FCB_FMT1( IFMT )
                              F2 = FCB_FMT2( IFMT )
                              CALL MSG_SETC( 'FMT', FCB_FMT( F1 : F2 ) )
                              CALL ERR_REP( 'NDF1_OPFOR_FNF2',
     :                             'Error searching for ^FMT format ' //
     :                             'file.', STATUS )
                           END IF
                        END IF

*  Quit searching when a suitable file is found, or if an error occurs.
                        IF ( ( STATUS .NE. SAI__OK ) .OR.
     :                       FOUND ) GO TO 4
 3                   CONTINUE
 4                   CONTINUE

*  If no suitable file was found, then report an error.
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        IF ( .NOT. FOUND ) THEN
                           STATUS = NDF__FILNF
                           CALL MSG_SETC( 'FILE', EXPFIL( : LEXP ) )
                           CALL MSG_SETC( 'MODE', VMODE )
                           CALL ERR_REP( 'NDF1_OPFOR_NO1',
     :                          'Unable to open the file ''^FILE'' ' //
     :                          'for ^MODE access.', STATUS )
                           CALL ERR_REP( 'NDF1_OPFOR_NO2',
     :                          'No file exists with this name and ' //
     :                          'a recognised file type extension.',
     :                          STATUS )

*  Otherwise, flag that conversion is required.
                        ELSE
                           CVT = .TRUE.

*  Expand the identified file name, requesting that a file
*  identification code be returned. Save the results for later use.
                           LEXP = LFOR
                           EXPFIL( : LEXP ) = FORFIL( : LFOR )
                           CALL NDF1_EXPFN( EXPFIL( : LEXP ), .TRUE.,
     :                                      FORFIL, LFOR, FORID,
     :                                      STATUS )
                           LFOR = MAX( 1, LFOR )
                        END IF
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF

*  Conversion required.
*  ===================
*  If conversion of a foreign format file appears to be required, then
*  search through the DCB to determine whether any NDF data object
*  which is currently in use already has this foreign format file
*  and extension associated with it.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( CVT ) THEN
            ACTIVE = .FALSE.
            NEXT = 0
            IDCB = 0
 5          CONTINUE             ! Start of 'DO WHILE' loop
            CALL NDF1_NXTSL( NDF__DCB, IDCB, NEXT, STATUS )
            IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NEXT .NE. 0 ) ) THEN
               IDCB = NEXT

*  Search for DCB entries with the same foreign file identification code
*  and format (ignore blank identification codes, which indicate that
*  identification information could not be obtained for the file).
               IF ( ( DCB_FORID( IDCB ) .EQ. FORID ) .AND.
     :              ( FORID .NE. ' ' ) .AND.
     :              ( DCB_IFMT( IDCB ) .EQ. IFMT ) ) THEN

*  Now check that the DCB entry refers to the same foreign extension.
*  First locate the bounds of the foreign extension specifier within the
*  existing DCB entry.
                  CALL NDF1_FORXT( DCB_FORFL( IDCB ), XX1, XX2, STATUS )

*  If neither the new entry, nor the existing entry refer to a specific
*  foreign extension, indicate that the require structure is already
*  active.
                  IF ( XX1 .GT. XX2 .AND. X1 .GT. X2 ) THEN
                     ACTIVE = .TRUE.

*  If one but not both entries refer to a specific foreign extension,
*  indicate that the require structure is not already active.
                  ELSE IF ( XX1 .GT. XX2 .OR. X1 .GT. X2 ) THEN
                     ACTIVE = .FALSE.

*  If both entries refer to the same foreign extension, indicate that the
*  require structure is already active.
                  ELSE
                     ACTIVE = ( DCB_FORFL( IDCB )( XX1 : XX2 ) .EQ.
     :                          NAME( X1 : X2 ) )
                  ENDIF

*  Leave the loop if we have found that the new entry is already active.
                  IF ( ACTIVE ) GO TO 6

               END IF
               GO TO 5
            END IF
 6          CONTINUE

*  If the foreign file is already associated with an NDF, then that NDF
*  will contain a converted copy, so there is no need to repeat the
*  conversion.
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( ACTIVE ) THEN

*  Check whether the required mode of access is compatible with the
*  existing access mode. If not, then the data object must be
*  re-imported (involving re-opening its container file).  Find the
*  name of the object from its DCB locator and use this name to open it
*  with the required access mode.
                  IF ( ( VMODE .NE. 'READ' ) .AND.
     :                 ( DCB_MOD( IDCB ) .NE. 'UPDATE' ) ) THEN
                     CALL DAT_REF( DCB_LOC( IDCB ), NDFNAM, LNAM,
     :                             STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        CALL NDF1_NFIND( DAT__ROOT, NDFNAM( : LNAM ),
     :                                   VMODE, IACB, STATUS )
                     END IF

*  If the access modes are compatible, then simply create a new base
*  NDF entry in the ACB to refer to the existing DCB entry.
                  ELSE
                     CALL NDF1_CRNBN( IDCB, IACB, STATUS )
                  END IF

*  If conversion of a foreign format file is definitely required...
               ELSE
*  Append any foreign extension specifier to the foreign format file spec.
                  IF( X1 .LE. X2 ) THEN
                     CALL CHR_APPND( NAME( X1 : X2 ), FORFIL, LFOR )
                  END IF

*  If conversion of a foreign format file is definitely required, then
*  identify the native format NDF object which is to be associated with
*  it and will hold the converted data.
                  CALL NDF1_NTFOR( FORFIL( : LFOR ), IFMT, TCB_KEEP,
     :                             NDFLOC, NDFNAM, LNAM, STATUS )

*  If we are getting an 'SDF' file (i.e. no file type) from a URL there will
*  be no true conversion step to produce the NDF specified by NDF1_NTFOR, so
*  we force NDFNAM to the name of the NDF being retrieved but preceded by
*  'URL'. This NDF will be affected by the KEEP tuning parameter.
                  IF ( URL ) THEN
                     IF( T2 .LE. T1 ) THEN
                        NDFLOC = DAT__ROOT
                        NDFNAM = 'URL' // EXPFIL( N1:N2 )
                        LNAM =  N2 - N1 + 4
                     END IF
                  END IF

*  Convert the foreign file.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     CALL NDF1_CVFOR( FORFIL( : LFOR ), IFMT, NDFLOC,
     :                                NDFNAM( : LNAM ), .TRUE., STATUS )

*  If conversion appears to have succeeded, then mark the error stack
*  and open the resulting native format NDF object, obtaining an ACB
*  index for the new base NDF. Specify UPDATE access unless the object
*  is to be kept, since we will later need to delete it.
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        CALL ERR_MARK
                        IF ( TCB_KEEP ) THEN
                           CALL NDF1_NFIND( NDFLOC, NDFNAM( : LNAM ),
     :                                      VMODE, IACB, STATUS )
                        ELSE
                           CALL NDF1_NFIND( NDFLOC, NDFNAM( : LNAM ),
     :                                      'UPDATE', IACB, STATUS )
                        END IF

*  If this fails, then format conversion has probably gone wrong. Annul
*  any "object not found" type errors, but let others remain (to help
*  diagnose any unanticipated problems).
                        IF ( STATUS .NE. SAI__OK ) THEN
                           IF ( NDF1_ABSNT( STATUS ) ) THEN
                              CALL ERR_ANNUL( STATUS )
                           END IF

*  Report (additional) context information, extracting the name of the
*  foreign format from the FCB format list string.
                           STATUS = NDF__CVTER
                           F1 = FCB_FMT1( IFMT )
                           F2 = FCB_FMT2( IFMT )
                           CALL MSG_SETC( 'FMT', FCB_FMT( F1 : F2 ) )

                           CALL MSG_SETC( 'FOR', FORFIL( : LFOR ) )
                           IF ( NDFLOC .NE. DAT__ROOT ) THEN
                              CALL DAT_MSG( 'NDF', NDFLOC )
                              CALL MSG_SETC( 'NDF', '.' )
                           END IF

                           CALL MSG_SETC( 'NDF', NDFNAM( : LNAM ) )
                           CALL ERR_REP( 'NDF1_OPFOR_CVT',
     :                          'Failed to convert the ^FMT format ' //
     :                          'file ''^FOR'' to NDF format in the ' //
     :                          'object ^NDF.', STATUS )
                        END IF

*  Release the error stack.
                        CALL ERR_RLSE

*  If OK, obtain an index to the data object entry in the DCB.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           IDCB = ACB_IDCB( IACB )

*  If we have the NDF object open for UPDATE access (either because it
*  must later be deleted or because it resides in a temporary file
*  which was previously opened with this access mode) then the DCB
*  access mode entry will reflect this. If UPDATE access to the
*  object's contents is not actually required, then modify this DCB
*  entry, since it will otherwise cause the NDF's contents to be
*  written back to the foreign file (with format conversion) when it is
*  released.
                           IF ( VMODE .EQ. 'READ' ) THEN
                              DCB_MOD( IDCB ) = 'READ'
                           END IF

*  Save the name and identification code of the associated foreign file
*  and the FCB index identifying its data format in the DCB. Also note
*  that the foreign file existed before we accessed it, and record
*  whether the NDF copy of the foreign file is to be kept.
                           DCB_FORFL( IDCB ) = FORFIL( : LFOR )
                           DCB_FORID( IDCB ) = FORID
                           DCB_IFMT( IDCB ) = IFMT
                           DCB_FOREX( IDCB ) = .TRUE.
                           DCB_FORKP( IDCB ) = TCB_KEEP

*  If the converted NDF object is not being kept, then mark its file as
*  a scratch file to ensure it will be deleted when finished with.
                           IF ( .NOT. TCB_KEEP ) THEN
                              CALL NDF1_HSCRT( DCB_LOC( IDCB ), STATUS )
                           END IF
                        END IF
                     END IF
                  END IF

*  Annul the locator used to identify the native format NDF object.
                  IF ( NDFLOC .NE. DAT__ROOT ) THEN
                     CALL DAT_ANNUL( NDFLOC, STATUS )
                  END IF
               END IF

*  If a subscript expression was supplied, then cut the appropriate
*  section from the converted NDF and annul the original ACB entry.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( S1 .LE. S2 ) THEN
                     CALL NDF1_NCUT( IACB, NAME( S1 : S2 ), IACBT,
     :                               STATUS )
                     CALL NDF1_ANL( IACB, STATUS )
                     IACB = IACBT
                     IACBT = 0
                  END IF
               END IF
            END IF
         END IF
      END IF

*  Assign the name of the data file to the MSG token "NDF_EVENT"
      CALL NDF1_EVMSG( 'NDF_EVENT', ACB_IDCB( IACB ) )

*  Raise an NDF event, describing the opening of an existing NDF.
      IF( VMODE .EQ. 'READ' ) THEN
         CALL NDF1_EVENT( 'READ_EXISTING_NDF', STATUS )

      ELSE IF( VMODE .EQ. 'WRITE' ) THEN
         CALL NDF1_EVENT( 'WRITE_EXISTING_NDF', STATUS )

      ELSE IF( VMODE .EQ. 'UPDATE' ) THEN
         CALL NDF1_EVENT( 'UPDATE_EXISTING_NDF', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_OPFOR', STATUS )

      END
