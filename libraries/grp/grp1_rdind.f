      SUBROUTINE GRP1_RDIND( UNIT, INFILE, SLOT, INDX, EDEP, EIFILE, 
     :                       EMODGP, EMODIN, NADDED, FLAG, STATUS )
*+
*  Name:
*     GRP1_RDIND

*  Purpose:
*     Read the contents of an indirection text file into a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_RDIND( UNIT, INFILE, SLOT, INDX, EDEP, EIFILE, EMODGP,
*                      EMODIN, NADDED, FLAG, STATUS )

*  Description:
*     Each record in the specified file is stored in the supplied group,
*     starting at the index given by argument INDX. Each record is 
*     stored as a single element (i.e. no expansion takes place). Each 
*     element is enclosed between opening and closing kernel dilimiters.
*     The number of records added to the group is returned in argument 
*     NADDED.

*  Arguments:
*     UNIT = INTEGER (Given)
*        The fortran unit number on which to open the file.
*     INFILE = CHARACTER * ( * ) (Given)
*        The name of the indirection file.
*     SLOT1 = INTEGER (Given)
*        The slot number for the group in which to store the records
*        read from the file.
*     INDX = INTEGER (Given)
*        The index within the group given by SLOT at which the first
*        new element is to be stored.
*     EDEP = INTEGER (Given)
*        The indirection depth at which the name was specified. Zero
*        should be given if the name was given directly, instead of by
*        an indirection element.
*     EIFILE = INTEGER (Given)
*        The index within the FILES array (see routine GRP1_PTIND) at
*        which the the name of the indirection file in which the name
*        was specified is given. A value of zero should be given if the
*        name was given directly, instead of by an indirection element.
*     EMODGP = INTEGER (Given)
*        The GRP identifier for the group used as a basis for the name
*        if it was created as a result of a modification element. A
*        value of GRP__NOID should be given if the name was not created
*        as a result of a modification element.
*     EMODIN = INTEGER (Given)
*        The index within the group specified by MODGP, of the name
*        used as a basis for the name given by argument NAME.  If MODGP
*        is given as GRP__NOID, then MODIN is ignored.
*     NADDED = INTEGER (Returned)
*        The number of elements added to the group given by SLOT.
*     FLAG = LOGICAL (Returned)
*        Returned true if the last element in the file is flagged. The 
*        flag character is removed in this case.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-JAN-1994 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants.
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_ERR'          ! GRP error values.

*  Arguments Given:
      INTEGER UNIT
      CHARACTER INFILE*(*)
      INTEGER SLOT
      INTEGER INDX
      INTEGER EDEP
      INTEGER EIFILE
      INTEGER EMODGP
      INTEGER EMODIN

*  Arguments Returned:
      INTEGER NADDED
      LOGICAL FLAG

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      INTEGER COM                ! Index of the comment character
      CHARACTER COMC*1           ! Groups current omment character.
      LOGICAL COMOK              ! .TRUE. if COMC is not NULL.
      LOGICAL EOF                ! Has end of file has been reached ?
      CHARACTER FLAGC*1          ! Current flag character
      LOGICAL FLAGOK             ! Is a flag character defined?
      CHARACTER GEXP*(GRP__SZNAM)! A group expression read from a file
      INTEGER GF                 ! First free character in GEXP.
      INTEGER INDIND             ! Index within the FILES array
      INTEGER IOERR              ! Fortran IO status value
      CHARACTER KCLCC*1          ! Closing kernel delimiter character
      CHARACTER KOPCC*1          ! Opening kernel delimiter character
      LOGICAL KCLOK              ! Is closing kernel delimiter defined?
      LOGICAL KOPOK              ! Is opening kernel delimiter defined?
      CHARACTER LGEXP*(GRP__SZNAM)! Last times group expression 
      INTEGER TLEN               ! Used length of the group expression
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open the text file specified after the first character.
      OPEN( UNIT = UNIT, FILE = INFILE, STATUS = 'OLD', IOSTAT = IOERR )

*  Check for errors, setting a suitable STATUS value and reporting the
*  error.
      IF ( IOERR .NE. 0 ) THEN
         STATUS = GRP__FIOER
         CALL MSG_SETC( 'FNAME', INFILE )
         CALL MSG_SETI( 'UNIT', UNIT )
         CALL ERR_FIOER( 'MESSAGE', IOERR )
         CALL ERR_REP( 'GRP1_EXPAN_ERR1', 'GRP1_EXPAN: Error opening '//
     :                 'text file ^FNAME on Fortran unit ^UNIT - '//
     :                 '"^MESSAGE".', STATUS )
         GO TO 999
      END IF

*  Get the group's current comment and flag characters.
      CALL GRP1_CONC( SLOT, GRP__PCOMC, COMC, COMOK, STATUS )
      CALL GRP1_CONC( SLOT, GRP__PFLGC, FLAGC, FLAGOK, STATUS )

*  Store the file name in the FILES array for this group, extending
*  the size of the array by one to make room for it. The index at which
*  te file name is stored within the FILES array is returned in INDIND.
      CALL GRP1_PTIND( SLOT, INFILE, INDIND, STATUS )

*  Initialise the number of elements added.
      NADDED = 0

*  Initialise the first character of the group expression to hold an
*  opening kernel delimiter (if one is defined).
      CALL GRP1_CONC( SLOT, GRP__POPKC, KOPCC, KOPOK, STATUS )
      CALL GRP1_CONC( SLOT, GRP__PCLKC, KCLCC, KCLOK, STATUS )
      IF( KOPOK .AND. KCLOK ) THEN
         GEXP( 1 : 1 ) = KOPCC
         GF = 2
      ELSE
         GF = 1
      END IF

*  Read the first record from the file.
      CALL GRP1_READF( UNIT, GEXP( GF : ), EOF, STATUS )

*  Indicate that no flag character has yet been found.
      FLAG = .FALSE.

*  Loop round until the end of file is reached, or an error occurs.
      DO WHILE( .NOT. EOF .AND. STATUS .EQ. SAI__OK )

*  If a comment character is defined...
         IF( COMOK ) THEN

*  Search for the first occurrence of the comment character in the 
*  group expression.
            COM = INDEX( GEXP, COMC )

*  If a comment character was found, set the rest of the group 
*  expression blank (including the comment character itself).
            IF( COM .GT. 0 ) GEXP( COM : ) = ' '

         ELSE
            COM = 0

         END IF

*  If a comment character was found in the first column, ignore this 
*  record.
         IF( COM .NE. GF ) THEN

*  Get the used length of the group expression.
            TLEN = CHR_LEN( GEXP )

*  See if the last character is a flag character.
            IF( TLEN .GT. 0 ) THEN
               FLAG = GEXP( TLEN : TLEN ) .EQ. FLAGC .AND. FLAGOK
            ELSE
               FLAG = .FALSE.
            END IF

*  If kernel delimiters are defined, append a closing kernel delimiter
*  to the group expression. Report an error if there is no room for the
*  closing kernel delimiter.
            IF( GF .EQ. 2 ) THEN
               IF( TLEN .EQ. LEN( GEXP ) ) THEN
                  STATUS = GRP__INVEL
                  CALL MSG_SETC( 'REC', GEXP( GF: ) )
                  CALL ERR_REP( 'GRP1_RDIND_ERR2', 'GRP1_RDIND: '//
     :                       ' Group expression too long - ''^REC''',
     :                       STATUS )
                  GO TO 999
               END IF
               CALL CHR_APPND( KCLCC, GEXP, TLEN )
            END IF

*  Store this record in the group, giving it an indirection depth of
*  one more than that of the supplied group expression, and storing the
*  index of the file given in the indirection element.
            CALL GRP1_PTELM( SLOT, INDX + NADDED, GEXP, EDEP + 1, 
     :                       INDIND, EMODGP, EMODIN, STATUS )

*  Save a copy of the record.
            LGEXP = GEXP            

*  Increment the number of elements added to the group.
            NADDED = NADDED + 1

         END IF

*  Read the next record from the file.
         CALL GRP1_READF( UNIT, GEXP( GF : ), EOF, STATUS )

      END DO

*  Close the text file.
      CLOSE( UNIT )

*  If the last record read from the file terminated with a flag 
*  character, remove it.
      IF( FLAG ) THEN
         IF( GF .EQ. 2 ) THEN
            LGEXP( TLEN - 1 : TLEN - 1 ) = ' '
         ELSE
            LGEXP( TLEN : TLEN ) = ' '
         END IF

*  Save the new value.
         CALL GRP1_PTELM( SLOT, INDX + NADDED - 1, LGEXP, EDEP + 1, 
     :                    INDIND, EMODGP, EMODIN, STATUS )

      END IF

 999  CONTINUE

      END
