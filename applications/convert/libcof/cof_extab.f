      SUBROUTINE COF_EXTAB ( STATUS )
*+
*  Name:
*     COF_EXTAB

*  Purpose:
*     Set up any required EXTABLE

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_EXTAB( STATUS )

*  Description:
*     Obtains a value for the EXTABLE parameter and if set, opens the
*     specified file and reads it to set up the arrays in COMMON associated
*     with the EXTABLE system for handling multi-extension FITS files.
*     The arrays, defined in F2NDF1_CMN, are:
*     CHARACTER*(DAT__SZNAM) NDFNMS(MAXEXT)
*        An array of names, one for each of the NDFs to be created within the
*        specified container file.
*        (See the definition of the NDFNAMES record below.)
*     CHARACTER*(DAT__SZNAM*2) COMPS(MAXCMP)
*        An array of NDF component names.
*     CHARACTER*12 CODES( MAXCMP )
*        An optional code number specifying a transformation to be applied
*        to the FITS data to produce the NDF data.
*        Available codes are:
*         'NONE' - No transformation
*     CHARACTER*32 EXTNS( MAXCMP, MAXEXT )
*        An array of the extension specifiers given in the EXTABLE file.
*        EXTNS(I,J) is the extension to be used for the component
*        COMPS(I) of the Jth NDF created in the specified container file.
*        The set of extensions corresponding to the components of a single
*        NDF is known as an extension set.
*        The specifiers are left as strings here so we can decide how to
*        handle them elsewhere.
*     INTEGER NCMP is set to the number of components specified in the
*        EXTABLE. (NCMP=0 signifies that no table was defined.)
*     INTEGER NEXTS is set to the number of extension sets defined in the
*        EXTABLE. This is the maximum number of extensions given in any
*        component record.
*
*     An EXTABLE file contains records which may be:
*
*     'component specifier records', which associate FITS extensions with
*       NDF components;
*     'NDFNAMES records', which specify the names of the NDFs to be
*       created. Normally they will be created within a top-level HDS
*       container file specified by the OUT parameter;
*     'directive records', which inform the table file parser.
*
*     Spaces are allowed between elements within records and blank records
*     are ignored.
*
*     Component specifier records have the form:
*
*        component; extension_specifiers; transformation_code
*
*       Where:
*        'component' (case-insensitive) specifies the NDF component and
*           is DATA, VARIANCE, QUALITY or EXTNi.name. The EXTNi.name form
*           specifies the name 'name' of an NDF extension to be created.
*           'name' may be omitted in which case 'FITS_EXT_n' is assumed
*           where n is the FITS extension number. 'i' is any characters and
*           may be omitted - it serves to differentiate component specifiers
*           where the default name is to be used.
*        'extension_specifiers' is a list of FITS extension specifiers,
*           separated by commas. The nth extension specifier from each
*           component specifier record forms an 'extension set' and each
*           extension set will be used to create one NDF in the output file.
*
*           Each extension specifier may be:
*           1. An integer specifying the FITS Header and Data Unit (HDU)
*              number. The primary HDU number is 0.
*           2. keyword=value (case-insensitive), specifying a FITS HDU
*              where the specified keyword has the specified value. E.g.
*              EXTNAME=IM2.  The 'keyword=' may be omitted in which case
*              EXTNAME is assumed.
*              Multiple keyword=value pairs separated by commas and enclosed
*              in [] may be given as a single extension specifier. All the
*              given keywords must match the extension header values.
*           3. Omitted to indicate that the component is not required
*              for the corresponding NDF. (Commas may be needed to maintain
*              correct extension-set alignment for later extension specifiers.)
*              If the last character of 'extension_specifiers' is comma, it
*              indicates an omitted specifier at the end.
*              Note that if an extension is not specified for the DATA
*              component of an NDF, an error will be reported at closedown.
*        'transformation_code' (case-insensitive) is a character string
*           specifying a  transformation to be applied to the FITS data
*           before it is written into the NDF component. The code and
*           preceding ; may be omitted in which case 'NONE' (no
*           transformation) is assumed. Currently the only permitted code
*           is 'NONE'.
*       There may be more than one component specifier record for a given
*       component, the extension specifiers will be concatenated. An
*       extension specifier may not span records and only the transformation
*       code specified by the last record for the component will be effective.
*
*     An NDFNAMES record has the format:
*       NDFNAMES name_list
*        Where name_list is a list of names for the NDFs to be created, one
*        for each extension set specified by the component specifier lines.
*        The names are separated by commas. If any of the names are omitted,
*        the last name specified is assumed to be a root name to which an
*        integer counter is to be added until a new name is found. If no
*        names are specified, 'EXTN_SET' is used as the root name.
*        E.g.:  NDFNAMES NDF,,SET_
*        would result in NDFs named NDF1, NDF2, SET_1, SET_2 etc. up to the
*        given number of extension sets.
*
*        There may be multiple NDFNAMES records, the names will be
*        concatenated. A name may not span records and a comma as the last
*        non-blank character indicates an omitted name.
*
*        If there is only one extension set, the name_list may be '*', in
*        which case the NDF will be created at the top level of the output
*        file.
*
*     Directive records have # in column 1 and will generally be treated
*        as comments and ignored.  An exception is a record starting with
*        '#END', which may optionally be used to terminate the file.

*  Arguments:
*     STATUS = INTEGER ({status_access_mode})
*        The global status.

*  Notes:
*     -  extension specifiers are separated by comma unless in [] or quotes.
*     -  spaces are removed from the extension specifiers unless in quotes
*        (single or double).
*     -  Double quotes may appear in single quotes and vice versa.
*     [routine_notes]...

*  Implementation Deficiencies:
*     -  Doesn't check for the same number of extension specifiers on each
*        line.
*     [routine_deficiencies]...

*  External Routines Used:
*     FIO
*        FIO_ASSOC
*        FIO_ANNUL
*        FIO_READ
*     MERS
*        ERR_ANNUL
*        ERR_REP
*        MSG_SETC
*        MSG_SETI
*     CHR
*        CHR_DELIM
*        CHR_FIWS
*        CHR_ITOC
*        CHR_LEN
*        CHR_PUTC
*        CHR_SIMLR
*        CHR_UCASE

*  Keywords:
*     {routine_keywords}...

*  Pitfalls:
*     -  {pitfall}
*     [pitfall_description]...

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     AJC: Alan J. Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     29-MAR-2000 (AJC):
*        Original version.
*      9-JUN-2000 (AJC):
*        Initialise NDFNMS
*     12-JUN-2000 (AJC):
*        Use NDFNAMES not #NDFNAMES
*        Revise description
*        Allow multiple records for same component
*        Initialise SQUOT, DQUOT
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'         ! PAR Error codes
      INCLUDE 'FIO_ERR'         ! FIO Error codes

*  Arguments Given:
*     None

*  Arguments Returned:
*     None

*  Status:
      INTEGER STATUS            ! Global status

*  External Routines:
      LOGICAL CHR_SIMLR
      INTEGER CHR_LEN

*  Local Constants:
      INTEGER TABVAL              ! ASCII value of TAB char
      PARAMETER( TABVAL = 9 )

*  Global Variables:
      INCLUDE 'F2NDF1_CMN'        ! EXTABLE variables
*     Symbolic constants defined
*        MAXCMP = INTEGER
*           The maximum number of components allowed
*        MAXEXT = INTEGER
*           The maxtimum number of extension sets allowed
*     Global variables declared
*        NCMP = INTEGER
*           Number of component lines in EXTABLE
*        NEXTS = INTEGER
*           Number of extension sets in EXTABLE
*        EXTNS(MAXCMP,MAXEXT) = CHARACTER*32
*           Extension table from EXTABLE
*        NDFNMS(MAXEXT) = CHARACTER*(DAT__SZNAM)
*           NDF names from EXTABLE
*        COMPS(MAXCMP) = CHARACTER*(DAT__SZNAM*2)
*           Component names from EXTABLE
*        CODES(MAXCMP) = CHARACTER*12
*           Transformation codes from EXTABLE

*  Local Variables:
      CHARACTER*80 BUFFER         ! EXTABLE input buffer
      INTEGER FD                  ! FIO File descriptor
      INTEGER NCH                 ! Used length of BUFFER
      INTEGER IST                 ! Index of start of info on line
      INTEGER I1, I2              ! String index
      INTEGER I, J                ! String index
      INTEGER ICMP                ! Index this component
      INTEGER IEXT                ! Number of extension sets so far for this
                                  !  component
      INTEGER CMPNXT(MAXCMP)      ! Number of extension sets so far for each
                                  !  component
      INTEGER NNMS                ! Number of NDF names so far
      LOGICAL END                 ! If end of loop
      LOGICAL SQUOT, DQUOT        ! If in single, double quotes
      LOGICAL BRACK               ! If in []
      LOGICAL ISNAME              ! If there's a name on the NDFNAMES record
      LOGICAL LAST                ! If last specifier on record
      CHARACTER*(DAT__SZNAM + 5) COMP  ! This component name
      CHARACTER*1 CH              ! Current character in BUFFER
      CHARACTER*(DAT__SZNAM) ROOTNM  ! Root NDF name
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the EXTNS and NDFNMS arrays
      DO J = 1, MAXEXT
         DO I = 1, MAXCMP
            EXTNS(I,J) = ' '
         END DO
         NDFNMS(J) = ' '
      END DO

*  Initialise the number of extension sets so far for each component.
      DO I = 1, MAXCMP
         CMPNXT(I) = 0
      END DO

*  Initialise NCMP - also signals no valid EXTABLE
      NCMP = 0
*  and the max number of extensions on a line so far
      NEXTS = 0
*  and the number of NDFNMS so far
      NNMS = 0

*  Obtain the value of the EXTABLE parameter
      CALL FIO_ASSOC( 'EXTABLE', 'READ', 'LIST', 0, FD, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
*  If the parameter is null, there is no table
         CALL ERR_ANNUL( STATUS )

      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'COF_EXTAB_OPERR',
     :     'Failed opening the EXTABLE.', STATUS )

      ELSE
*  There was a table - process it         .
*  Read the file
         END = .FALSE.
         DOWHILE ( ( STATUS .EQ. SAI__OK ) .AND. .NOT. END )
            CALL FIO_READ( FD, BUFFER, NCH, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
*  Ignore comments
               IF ( BUFFER(1:1) .EQ. '#' ) THEN
*  Directive
*  Check for END
                  IF( BUFFER(1:NCH) .EQ. '#END' ) THEN
                     END = .TRUE.

                  ELSE ! it must be a comment
*  The line is a comment - ignore it
                     CONTINUE

                  END IF

               ELSE IF ( BUFFER(1:NCH) .EQ. ' ' ) THEN

*  Ignore blank records
                  CONTINUE

               ELSE
*  Component or NDFNAMES specifier - ignore leading blanks
                  IST = 1
                  CALL CHR_FIWS( BUFFER(1:NCH), IST, STATUS )

                  IF( CHR_SIMLR(BUFFER(IST:IST+7), 'NDFNAMES' ) )THEN
*  An NDF names record
                     I = NNMS + 1
                     I1 = IST + 9
                     I2 = 1
                     ISNAME = .FALSE.
                     DO WHILE ( ( STATUS .EQ. SAI__OK )
     :                    .AND. ( I1 .LE. NCH ) )
                        CH = BUFFER(I1:I1)
                        IF( ( CH .EQ. ' ' ) .OR.
     :                      ( CH .EQ. CHAR( TABVAL ) ) ) THEN
*                    Ignore spaces
                           CONTINUE
                        ELSE IF( CH .EQ. ',' ) THEN
*                    End of name - point to next
                           I = I + 1
                           IF ( I .GT. MAXEXT ) THEN
                              STATUS = SAI__ERROR
                              I1 = NCH + 1
                              CALL ERR_REP( 'COF_EXTAB_NMER',
     :                          'EXTABLE $EXTABLE: Too many NDF names.',
     :                          STATUS )
                           ELSE
                              ISNAME = .TRUE.
                              I2 = 1

                           END IF

                        ELSE
*                    Normal character
*                    Save it in NDFNMS if there is room
                           IF( I2 .GT. DAT__SZNAM ) THEN
                              STATUS = SAI__ERROR
                              I1 = NCH + 1
                              CALL MSG_SETC( 'NAME', NDFNMS )
                              CALL MSG_SETC( 'NAME', CH )
                              CALL ERR_REP( 'COF_EXTAB_NMER',
     :                           'EXTABLE $EXTABLE: '//
     :                           'NDF name ^NAME... is too long.',
     :                           STATUS )
                           ELSE
                              ISNAME = .TRUE.
                              NDFNMS(I)(I2:I2) = CH
                              I2 = I2 + 1
                           END IF
                        END IF
                        I1 = I1 + 1
                     END DO

*  End of NDFNAMES record - update the number of names so far
                     IF ( ISNAME ) NNMS = I

                  ELSE
*  It's a component specifier record - should have the form:
*   COMPONENT;extension_specifiers[;transformation_code]

*  Get the component name in upper case
                     CALL CHR_DELIM( BUFFER(1:NCH), ';', I1, I2 )
                     IF ( I1 .LE. 1 ) THEN
*  No component name
                        STATUS = SAI__ERROR
                        CALL ERR_REP( 'COF_EXTAB_CMPER2',
     :                       'EXTABLE $EXTABLE: '//
     :                       'Illegal format record.', STATUS )
                        CALL MSG_SETC( 'BUF', BUFFER )
                        CALL ERR_REP( 'COF_EXTAB_CMPER1', '^BUF',
     :                       STATUS )
                     ELSE
*  Component name present
                        COMP = BUFFER(IST:I1-1)
                        CALL CHR_UCASE( COMP )

*  See if it has already been used - if so get the next extension set number
                        IF ( NCMP .GT. 0 ) THEN
*  There are some already used
                           DO ICMP = 1, MAXCMP
                              IF ( COMP .EQ. COMPS(ICMP) ) GOTO 100
                           END DO
*  COMP not used yet
*  count total number of component names
                           NCMP = NCMP + 1
                           IF ( NCMP .GT. MAXCMP ) THEN
                              CALL MSG_SETI( 'MAXCMP', MAXCMP )
                              STATUS = SAI__ERROR
                              CALL ERR_REP( 'COF_EXTAB_MXREC',
     :                           'EXTABLE $EXTABLE: '//
     :                           'Max ^MAXCMP components exceeded.',
     :                           STATUS )
                           ELSE
                              ICMP = NCMP
                              COMPS( ICMP ) = COMP
                           END IF

                        ELSE
* No component names so far
                           NCMP = 1
                           ICMP = 1
                           COMPS( 1 ) = COMP
                        END IF ! component names

100                     CONTINUE

                     END IF  ! Component name present in record

* If OK so far, set the CODE for this component
* This process will be repeated for subsequent component specifier records
* for the same component - only thelast will be effective.
                     IF ( STATUS .EQ. SAI__OK ) THEN
*  Set the transformation code ( default 'NONE' )
                        CODES( ICMP ) = 'NONE'
                        IF ( I2 .LT. NCH ) THEN
*  There is a code
                           IST = 1
                           CALL CHR_FIWS( BUFFER(I2+1:), IST, STATUS )
                           CODES(ICMP) = BUFFER(I2+IST:)
                           CALL CHR_UCASE( CODES(ICMP) )

*  Check for legal transformation code
                           IF ( CODES(ICMP) .NE. 'NONE' ) THEN
                              STATUS = SAI__ERROR
                              CALL MSG_SETC( 'CODE', CODES(ICMP) )
                              CALL ERR_REP( 'COF_EXTAB_CODER',
     :                          'EXTABLE $EXTABLE: Illegal '//
     :                          'transformation code ''^CODE''',
     :                          STATUS )
                           END IF  ! Illegal code

                        END IF  ! Code present

                     END IF ! OK to get CODE

*  If OK so far
*  Set the extensions table
*  First remove all spaces not in quotes from the extension list section;
*  and change , not in [] to ; - this eases the parsing process.
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  I1,I2 index the first and last character of extension specifications
                        I1 = I1 + 1
                        IF ( BUFFER(I2:I2) .EQ. ';' ) I2 = I2 -1
*  J indexes the processed extension specifiers string
                        J = I1
                        BRACK = .FALSE.
                        SQUOT = .FALSE.
                        DQUOT = .FALSE.
                        DO I = I1, I2
                           CH = BUFFER(I:I)
                           IF( CH .NE. ' ' ) THEN
                              IF ( CH .EQ. ',' ) THEN
                                 IF ( .NOT. BRACK ) CH = ';'
                              ELSE IF( CH .EQ. '"' ) THEN
                                 IF ( .NOT. SQUOT ) DQUOT = .NOT. DQUOT
                              ELSE IF( CH .EQ. '''' ) THEN
                                 IF ( .NOT. DQUOT ) SQUOT = .NOT. SQUOT
                              ELSE IF( CH .EQ. '[' ) THEN
                                 BRACK = .TRUE.
                              ELSE IF( CH .EQ. ']' ) THEN
                                 BRACK = .FALSE.
                              END IF
                              BUFFER(J:J) = CH
                              J = J + 1
                           ELSE
                              IF ( SQUOT .OR. DQUOT ) THEN
                                 BUFFER(J:J) = CH
                                 J = J + 1
                              END IF
                           END IF  !  space

                        END DO  ! removing spaces

* Get the number of extension sets so far for this component
                        IEXT = CMPNXT( ICMP )

*  Now split the extension specifiers (separated by ;)
*  I1, J index the first and last characters of the processed string
*  LAST flags the last specifier (i.e. no ; terminating it).
                        J = J - 1
                        LAST = .FALSE.
                        DOWHILE ( ( .NOT. LAST )
     :                            .AND. (STATUS .EQ. SAI__OK ) )
                           IEXT = IEXT + 1
                           IF ( IEXT .GT. MAXEXT ) THEN
                              STATUS = SAI__ERROR
                              CALL MSG_SETI( 'MAXEXT', MAXEXT )
                              CALL ERR_REP( 'COF_EXTAB_MXEXT',
     :                          'EXTABLE $EXTABLE: '//
     :                          'Max ^MAXEXT extension sets exceeded.',
     :                          STATUS )


                           ELSE
                              IF ( I1 .LE. J ) THEN
                                 I = INDEX(BUFFER(I1:J),';')
*  If I is 1, we have an omitted specifier leave the blank EXTNS entry
                                 IF ( I .NE. 1 ) THEN
                                    IF ( I .EQ. 0 ) THEN
*  There are no more ; separators
                                       I = J - I1 + 2
                                       LAST = .TRUE.

                                    END IF
                                    EXTNS(ICMP,IEXT) = BUFFER(I1:I1+I-2)
                                 END IF
                                 I1 = I1 + I
                              ELSE
*  No more characters
                                 LAST = .TRUE.
                              END IF  ! more characters in specifiers
                           END IF  ! room for more extensions
                        END DO  ! processing extensions
*  Update NEXTS in case the number of extns on a line differs between
*  components.
                        NEXTS = MAX( NEXTS, IEXT )

                     END IF  ! OK to set extension specifiers

*  Done the component specifier record - update the number of extension
*  specifiers for this component
                     CMPNXT( ICMP ) = IEXT

                  END IF  ! Component specifier record

               END IF  ! Component specifier or NDFNAMES record

            ELSE IF ( STATUS .EQ. FIO__EOF ) THEN
*  Ran off end of file
               CALL ERR_ANNUL( STATUS )
               END = .TRUE.

            END IF  ! Read OK

         END DO  ! For each record

*  Check for compatible NDFNAME * with 1 extension sets
         IF ( ( NDFNMS(1) .EQ. '*' ) .AND. ( NEXTS .GT. 1 ) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'COF_EXTAB_SZNAM',
     :         'EXTABLE $EXTABLE: '//
     :         'Only one extension set is allowed with NDFNAME ''*''.',
     :         STATUS )
         END IF

*  Now fill in any empty NDF names
         IF ( NDFNMS(1) .EQ. ' ' ) NDFNMS(1) = 'EXTN_SET'
         ROOTNM = NDFNMS(1)
         IF ( NEXTS .GT. 1 ) THEN
            I = 1
            DOWHILE ( ( I .LE. NEXTS ) .AND. ( STATUS .EQ. SAI__OK ) )
               IF ( NDFNMS( I ) .EQ. ' ' ) THEN
                  IF ( NDFNMS( I-1 ) .EQ. ROOTNM ) THEN
* If the last name was the root name, add 1 suffix to it
                     I1 = 1
                     I2 = CHR_LEN( ROOTNM )
                     CALL CHR_ITOC( I1, BUFFER, NCH )
                     IF( (I2+NCH) .GT. DAT__SZNAM ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETC( 'NAME', ROOTNM )
                        CALL MSG_SETI( 'NAME', I1 )
                        CALL ERR_REP( 'COF_EXTAB_SZNAM',
     :                    'EXTABLE $EXTABLE: '//
     :                    'Generated NDF name ^NAME too long', STATUS )
                     ELSE
                        CALL CHR_PUTC( BUFFER(1:NCH), NDFNMS(I-1), I2 )
                     END IF
                  END IF
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     I1 = I1 + 1
                     NDFNMS( I ) = ROOTNM
                     I2 = CHR_LEN( ROOTNM )
                     CALL CHR_ITOC( I1, BUFFER, NCH )
                     IF( (I2+NCH) .GT. DAT__SZNAM ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETC( 'NAME', ROOTNM )
                        CALL MSG_SETI( 'NAME', I1 )
                        CALL ERR_REP( 'COF_EXTAB_SZNAM',
     :                    'EXTABLE $EXTABLE: '//
     :                    'Generated NDF name ^NAME too long', STATUS )
                     ELSE
                        CALL CHR_PUTC( BUFFER(1:NCH), NDFNMS(I), I2 )
                     END IF
                  END IF
               ELSE
*  An NDF name is set
                  ROOTNM = NDFNMS( I )
               END IF
               I = I + 1
            END DO
         END IF

*  Close the file
         CALL FIO_ANNUL( FD, STATUS )
      END IF

      END
