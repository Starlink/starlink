      SUBROUTINE IRH1_GRAPP( IDH1, IDH2, GRPEXP, TERMC, TERM, STATUS )
*+
*  Name:
*     IRH1_GRAPP

*  Purpose:
*     Append a set of names specified by a group expression, to an
*     existing group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_GRAPP( IDH1, IDH2, GRPEXP, TERMC, TERM, STATUS )

*  Description:
*     The supplied group expression is split into its constituent
*     elements, each element being appended to the end of the group
*     identified by argument IDH2.  Each new element is then checked to
*     see if it is an "indirection element". If it is, all the elements
*     stored in the associated text file are appended to the end of the
*     group (the original indirection element is deleted from the
*     group).  If the element is not an indirection element, it is
*     checked to see if it is a "modification element". If it is, and
*     if IDH1 identifies a valid group, the modification element is
*     expanded into a list of names based on the names contained in the
*     group identified by IDH1, according to the rules described for
*     modification elements in ID/9, and the original modification
*     element is deleted.  This checking process is continued until the
*     end of the group is reached (including any elements added as a
*     result of expanding previous elements).  The final group is
*     purged of blank entries before being returned.
*
*     Each group entry consists of a name, plus extra descriptive
*     information and is stored in the associated GROUP structure.
*     This extra information consists of:
*
*     1) The indirection depth at which the name was specified.
*
*     2) The file in which the name was specified (blank if the name was
*     given in the original group expression).
*
*     3) The identifier for the group used as the basis for
*     modification (zero if the name was not derived from a modification
*     element).
*
*     4) The index within the basis group of the name which was modified
*     to give the new name (zero if the name was not derived from a
*     modification element).

*  Arguments:
*     IDH1 = INTEGER (Given)
*        The identifier for an existing group to be used as the basis
*        for an output group. If an invalid value such as IRH__NOID is
*        given for IDH1 then no check is made for modification elements.
*        If any are given, they will be treated as a single literal
*        name.
*     IDH2 = INTEGER (Given)
*        The identifier for the group to which the names are to be
*        appended. The group must previously have been created by a
*        call to IRH1_GTIDH.
*     GRPEXP = CHARACTER (Given)
*        The group expression specifying the names to be appended to
*        the group specified by IDH2 (see ID/9 for a description of the
*        format of group expressions).
*     TERMC = CHARACTER (Given)
*        The termination character. Only the first character is used.
*     TERM = LOGICAL (Returned)
*        True if the last name in the group expression is terminated by
*        the character given by argument TERMC.  "Last" in this context
*        refers to the order in which the names were included in the
*        supplied group expression. This will not in general be the
*        same as the order in which the names are stored in the NAMES
*        array. Note, all termination characters are removed from the
*        returned group.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-MAY-1991 (DSB):
*        Original version.
*     2-JUL-1991 (DSB):
*        Termination character made variable.
*     26-FEB-1992 (PDRAPER):
*        Removed I90_PAR reference, added DAT_PAR.
*     13-MAR-1992 (PDRAPER):
*        Changed TSET to logical from integer.
*     12-AUG-1993 (PDRAPER):
*        Added test for depreciated substitution element
*     16-MAR-1995 (PDRAPER):
*        Removed non-standard READONLY in open statement.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'IRH_PAR'          ! IRH constants.
      INCLUDE 'IRH_ERR'          ! IRH error values.

*  Global Variables:
      INCLUDE 'IRH_COM'          ! IRH common blocks.
*        HCM_GSIZE( IRH__MAXG ) = INTEGER (Read and Write)
*           The index of the last entry in each group.
*        HCM_VALID( IRH__MAXG ) = LOGICAL (Read)
*           True if the corresponding group identifier is valid (i.e. in
*           use).

*  Arguments Given:
      INTEGER IDH1
      INTEGER IDH2
      CHARACTER GRPEXP*(*)
      CHARACTER TERMC*(*)

*  Arguments Returned:
      LOGICAL TERM

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Function giving used length of a
                                 ! string.

*  Local Variables:
      INTEGER DEPEND             ! The index at which the last element
                                 ! obtained at the current depth of
                                 ! indirection is stored.
      INTEGER DEPTH              ! Current depth of indirection.
      CHARACTER ELEM*(IRH__SZNAM)! The current element.
      LOGICAL EOF                ! True if the end of file has been
                                 ! reached in the file specified within
                                 ! an indirection element.
      INTEGER EDEP               ! Depth at which the retrieved element
                                 ! was given.
      CHARACTER EFILE*(IRH__SZNAM)! File in which the retrieved element was
                                 ! given.
      INTEGER EMODGP             ! Modified group which gave rise to the
                                 ! retrieved element.
      INTEGER EMODIN             ! Modified index which gave rise to the
                                 ! retrieved element.
      LOGICAL ETERM              ! True if the current element contained
                                 ! a termination character.
      CHARACTER GEXP*(IRH__SZGEX)! The current group expression.
      LOGICAL INDELM             ! True if the current element is an
                                 ! indirection element.
      INTEGER GINDEX             ! The index of the current group element.
      INTEGER IAT                ! Position with string
      LOGICAL INGRP              ! True if IDH1 identifies a valid group.
      INTEGER IOERR              ! Fortran IO status value.
      INTEGER LASTC              ! Position of last non-blank character.
      INTEGER MODSIZ             ! Size of the group to be used as the
                                 ! basis for any modification elements.
      LOGICAL TSET               ! True if the TERM argument has been
                                 ! assigned a value.
      INTEGER UNIT               ! Fortran unit on which to open the
                                 ! file specified within an indirection
                                 ! element.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the group expression is blank, return immediately.
      IF( GRPEXP .EQ. ' ' ) RETURN

*  See if IDH1 identifies a valid group. If it is, save its current
*  size.
      IF( IDH1 .LT. 1 .OR. IDH1 .GT. IRH__MAXG ) THEN
         INGRP = .FALSE.

      ELSE IF( .NOT. HCM_VALID( IDH1 ) ) THEN
         INGRP = .FALSE.

      ELSE
         INGRP = .TRUE.
         MODSIZ = HCM_GSIZE( IDH1 )

      END IF

*  Copy the supplied group expression to a local variable.
      GEXP = GRPEXP

*  Initialise the unit number for accessing text files to an invalid
*  negative value.
      UNIT = -1

*  Save the index at which the next entry will be stored in the
*  group being extended.
      GINDEX = HCM_GSIZE( IDH2 ) + 1

*  DEPTH stores the number of levels of indirection at which the
*  elements currently being processed were stored. Elements contained
*  in the given group expression are at depth zero; elements given
*  within text files specified by indirection elements in the given
*  group expression are at depth 1; elements given within text files
*  specified by indirection elements at depth 1 are at depth 2, etc.
      DEPTH = 0

*  Split the given group expression up into elements (i.e. strings
*  delimited by commas), and append each element to the end of the
*  group. The elements are assigned an indirection depth of zero and
*  a blank indirection file name.
      CALL IRH1_ELEMS( IDH2, GEXP, DEPTH, ' ', STATUS )

*  The current group size gives the last element specified at the
*  current depth of indirection (i.e. in this case, no indirection at
*  all).  Save it for future use.
      DEPEND = HCM_GSIZE( IDH2 )

*  Indicate that the TERM argument has not yet been assigned a value.
      TSET = .FALSE.

*  Now loop through each new element added to the group. The group size
*  is updated within the loop if any elements are added to the group as
*  a result of indirection or modification elements being expanded.
      DO WHILE( GINDEX .LE. HCM_GSIZE( IDH2 ) )

*  Check that the maximum permisable depth of indirection has not been
*  exceeded. A limit is imposed just to safeguard against "run-away"
*  indirection, in which a text file references itself within an
*  indirection element!
         IF( DEPTH .GT. IRH__MAXDI ) THEN
            STATUS = IRH__DEEP
            CALL MSG_SETI( 'MAX', IRH__MAXDI )
            CALL ERR_REP( 'IRH1_GRAPP_ERR1',
     :       'IRH1_GRAPP: Maximum depth of indirection (^MAX) exceeded',
     :                    STATUS )
            GO TO 999
         END IF

*  Get the current element from the group.
         CALL IRH1_GTELM( IDH2, GINDEX, ELEM, EDEP, EFILE, EMODGP,
     :                    EMODIN, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  See if the name was terminated. If it is, removed the termination
*  character. If the name consisted of nothing more than the termination
*  character, then erase the name.
         LASTC = MAX( 1, CHR_LEN( ELEM ) )
         ETERM = .FALSE.

         IF( LASTC .GT. 0 ) THEN
            IF( ELEM( LASTC : LASTC ) .EQ. TERMC(1:1) ) THEN
               ETERM = .TRUE.
               IF( LASTC .GT. 1 ) THEN
                  ELEM( LASTC : LASTC ) = ' '
                  CALL IRH1_PTELM( IDH2, GINDEX, ELEM, EDEP, EFILE,
     :                             EMODGP, EMODIN, STATUS )
               ELSE
                  CALL IRH1_ERELM( IDH2, GINDEX, STATUS )

               END IF
            END IF
         END IF

*  If the first character is equal to the value of symbolic constant
*  IRH__INDC, the element is an "indirection element" (i.e. it
*  specifies a text file holding further group expressions to be
*  appended to the end of the current group expression).
         IF( ELEM(1:1) .EQ. IRH__INDC ) THEN
            INDELM = .TRUE.

*  If this is the first indirection element, get a free Fortran unit
*  number for reading the text file specified after the first character.
            IF( UNIT .EQ. -1) CALL FIO_GUNIT( UNIT, STATUS )

*  Open the text file specified after the first character. Succesive
*  input files are not closed as this is done automatically when a new
*  file is opened on the same unit.
            OPEN( UNIT = UNIT, FILE = ELEM(2:), STATUS = 'OLD',
     :            IOSTAT = IOERR )

*  Check for errors, setting a suitable STATUS value and reporting the
*  error.
            IF ( IOERR .NE. 0 ) THEN
               CALL FIO_SERR( IOERR, STATUS )
               CALL MSG_SETC( 'FNAME', ELEM(2:) )
               CALL MSG_SETI( 'UNIT', UNIT )
               CALL ERR_FIOER( 'MESSAGE', IOERR )
               CALL ERR_REP( 'IRH1_GRAPP_ERR2',
     :                       'Error opening input file ^FNAME on '//
     :                       'Fortran unit ^UNIT - ^MESSAGE.', STATUS )
               GO TO 999
            END IF

*  Erase the indirection element from the group.
            CALL IRH1_ERELM( IDH2, GINDEX, STATUS )

*  Loop round until the end of file is reached, or an error occurs.
            EOF = .FALSE.

            DO WHILE( .NOT. EOF .AND. STATUS .EQ. SAI__OK )

*  Read a record from the file. This is the next group expression.
               CALL IRH1_READF( UNIT, GEXP, EOF, STATUS )

*  Split the record up into its constituent elements, and add them to
*  the end of the group. If the end of file has been reached, GEXP will
*  be returned blank by IRH1_READF and so no elements will be added to
*  the group. The elements are assigned the next lower indirection depth
*  and the current indirection file name.
               CALL IRH1_ELEMS( IDH2, GEXP, EDEP + 1, ELEM( 2: ),
     :                          STATUS )

*  Read the next record from text file.
            END DO

*  If this element is not an indirection element...
         ELSE
            INDELM = .FALSE.


*  If a valid group was identified by IDH1, check for modification
*  elements.
            IF( INGRP ) THEN
*  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*  Modify this so that a "depreciated" substitution element is
*  allow. The rules for this are that the depreciated element
*  should be the last (non-blank) element in the string, if this
*  if the case then the last three occurrences of this will be
*  replaced by the current subsitution element.
*  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*  First locate position of lsat occurence of IRH__MODSD
               CALL IRH1_LASTO( ELEM, IRH__MODSD, IAT, STATUS )
               IF ( IAT .NE. 0 ) THEN

*  Ok we do have an occurence of the depreciated element is it at the
*  end of the string?
                  IF ( LASTC .EQ. IAT ) THEN

*  Yes it is. Replace this and the previous two by the official
*  substitution element.
                     ELEM( IAT: IAT ) = IRH__MODSP
                     CALL IRH1_LASTO( ELEM, IRH__MODSD, IAT, STATUS )
                     IF ( IAT .NE. 0 ) ELEM( IAT: IAT ) = IRH__MODSP
                     CALL IRH1_LASTO( ELEM, IRH__MODSD, IAT, STATUS )
                     IF ( IAT .NE. 0 ) ELEM( IAT: IAT ) = IRH__MODSP
                  END IF
               END IF

*  This is a modification element if it contains either of the two
*  modification element control characters given by symbolic constants
*  IRH__MODNM (the token representing the input name) and IRH__MODSP
*  (the character used to seperate the two substitution strings).
               IF( INDEX( ELEM, IRH__MODNM ) .NE. 0 .OR.
     :             INDEX( ELEM, IRH__MODSP ) .NE. 0 ) THEN

*  If it is, delete the original element from the output group.
                  CALL IRH1_ERELM( IDH2, GINDEX, STATUS )

*  Expand it to form a list of explicit names, and append them to the
*  end of the group.
                  CALL IRH1_MODIF( IDH1, MODSIZ, IDH2, ELEM, EDEP,
     :                             EFILE, STATUS )

               END IF

            END IF

         END IF

*  Decide if the group is "terminated".  This is determined by the
*  presence or absence of the "termination" character (defined by the
*  argument TERMC) within the last element at each level of
*  indirection.
         IF( GINDEX .EQ. DEPEND ) THEN

*  If the TERM argument has already been assigned a value, skip over
*  this section.
            IF( .NOT.TSET ) THEN

*  If the last element at this level is not an indirection element, the
*  TERM argument is set according to the presence or absence of a
*  termination character.
               IF( .NOT. INDELM ) THEN
                  TERM = ETERM
                  TSET = .TRUE.

*  If the last element at this level is an indirection element, the
*  group is considered terminated if a termination character was
*  contained in the indirection element, but no value is assigned to
*  TERM otherwise (this leaves open the option of it being set at a
*  greater depth of indirection).
               ELSE
                  IF( ETERM ) THEN
                     TERM = .TRUE.
                     TSET = .TRUE.

                  END IF

               END IF

            ENDIF

*  All the elements specified at the current depth of indirection have now
*  been processed. Save the index of the last element at the next
*  depth of indirection, and save the actual depth.
            DEPEND = HCM_GSIZE( IDH2 )
            DEPTH = DEPTH + 1

         END IF

*  Pass on to the next element in the group.
         GINDEX = GINDEX + 1

      END DO

*  Close the last text file opened (if any), and release the Fortran
*  unit used.
      IF( UNIT .NE. -1) THEN
         CLOSE( UNIT )
         CALL FIO_PUNIT( UNIT, STATUS )
      END IF

*  Remove any blank entries from the group, and all terminating
*  characters.
      CALL IRH1_REMBL( IDH2, STATUS )

 999  CONTINUE

      END


* $Id$
