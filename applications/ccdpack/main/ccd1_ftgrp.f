      SUBROUTINE CCD1_FTGRP( WRDGRP, LINGRP, MASK, FITGRP, DESGRP,
     :                       STATUS )
*+
*  Name:
*     CCD1_FTGRP

*  Purpose:
*     Interprets a "FITS control table" giving groups of FITS and
*     extension descriptions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_FTGRP( WRDGRP, LINGRP, MASK, FITGRP, DESGRP, STATUS )

*  Description:
*     This routine is intended for interpreting the contents of a "FITS
*     control table" which has been processed into words using
*     CCD1_GFGRP. The information in the input GRP groups is used to
*     construct new groups which specify the names (keyword) and types
*     of the FITS items to be used in constructing the values which will
*     be entered into an NDF extension. After all FITS keywords have
*     been identified and typed a second set of groups are created which
*     now contain the names of the destination extension items their
*     types and the function of the FITS values which are to be used to
*     define the value.

*  Arguments:
*     WRDGRP( 3 ) = INTEGER (Given and Returned)
*        The GRP identifiers of the groups which contain the FITS
*        control table information. This may be returned if HDS-type
*        information is deduced from the Extension-item. If this
*        case the second element will have the deduced type written to
*        it and the third element will have the value that the second
*        had on entry. If the Extension item type is incorrect then the
*        correct value will supersede.
*     LINGRP = INTEGER (Given)
*        A GRP identifier for a group which has a direct one-one
*        correspondence to the WRDGRP groups and which contains the
*        original line numbers from which the table information was
*        extracted.
*     MASK( * ) = LOGICAL (Given and Returned)
*        Workspace used to indicate which statements have been
*        processed. Should be at least as large as the number of
*        elements in WRDGRP and LINGRP.
*     FITGRP( 2 ) = INTEGER (Returned)
*        Two groups which on exit will contain the keyword and type of
*        any FITS items whose values will be used in contructing
*        extension item values.  The members of the first group will
*        be normal (though possibly hierarchical) FITS keyword names,
*        except that they may contain one of the strings "<X1>", "<X2>",
*        "<Y1>" or "<Y2>" to indicate indexing into a header value
*        of the form [X1:X2,Y1:Y2].
*     DESGRP( 3 ) = INTEGER (Returned)
*        Three groups which on exit will contain the extension item
*        name, its expected type (constrained by CCDPACK) and the
*        function of FITS items which will give the value to be used.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The format of FITS control tables is very flexible. But
*     basically this is
*
*          _HDS-type FITS-keyword
*          Extension-item  _HDS-type   function-of-FITS-items
*
*     Declarations of the FITS-keywords may occur anywhere, or
*     not at all if the function-of-FITS-items for an Extension-item
*     is just a reference to a single string (i.e. a FITS-keyword).
*
*     _HDS-type is required for FITS-keyword declarations but may be
*     left out of the Extension-item statements, if the Extension-item
*     is a recognisable CCDPACK item. In this way both complex and
*     minimal transformation tables are possible as well as support for
*     the current (9-12-93) KAPPA:FITSIMP format.
*
*     - The input groups should have their case sensitivity switched
*     off.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-DEC-1993 (PDRAPER):
*        Original version.
*     2-AUG-2000 (MBT):
*        Added support for FITS header values of the form [X1:X2,Y1:Y2],
*        and fixed some bugs with ERR_MARK/ERR_RLSE pairing.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP parameters
      INCLUDE 'TRN_ERR'          ! TRANSFORM error codes

*  Arguments Given:
      INTEGER LINGRP

*  Arguments Given and Returned:
      INTEGER WRDGRP( 3 )
      LOGICAL MASK( * )

*  Arguments Returned:
      INTEGER FITGRP( 2 )
      INTEGER DESGRP( 3 )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of string

*  Local Variables:
      CHARACTER * ( GRP__SZNAM ) NAME1 ! Buffer for "name" from group
      CHARACTER * ( GRP__SZNAM ) NAME2 ! Buffer for "name" from group
      CHARACTER * ( GRP__SZNAM ) NAME3 ! Buffer for "name" from group
      CHARACTER * ( GRP__SZNAM ) NAME4 ! Buffer for "name" from group
      CHARACTER * ( GRP__SZNAM ) NAME5 ! Buffer for "name" from group
      CHARACTER * ( GRP__SZNAM ) WORDS( 1 ) ! Word returned
      DOUBLE PRECISION DVAL      ! Dummy
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Position in string
      INTEGER INDEX              ! Index into group
      INTEGER IX                 ! Index of '[X1:X2,Y1:Y2]' substitution
      INTEGER J                  ! Loop variable
      INTEGER LSTAT              ! Local status
      INTEGER NFITS              ! Number of FITS items
      INTEGER NLINES             ! Number of entries in input groups
      INTEGER NWRD               ! Number of words returned
      INTEGER START( 1 )         ! Start of word
      INTEGER NSUB               ! Number of tokens substituted
      INTEGER STOP( 1 )          ! End of word
      LOGICAL LVAL               ! Dummy
      LOGICAL YES                ! Extension item is recognised
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of entries in the input groups.
      CALL GRP_GRPSZ( LINGRP, NLINES, STATUS )

*  Initialise the mask for statements processed.
      DO 1 I = 1, NLINES
         MASK( I ) = .FALSE.
 1    CONTINUE

*  Create new groups. Two for FITS-item information, three for
*  Extension-item information. Make them case insensitive.
      CALL GRP_NEW( 'FITS-keyword', FITGRP( 1 ), STATUS )
      CALL GRP_SETCS( FITGRP( 1 ), .FALSE., STATUS )
      CALL GRP_NEW( 'FITS_HDS-type', FITGRP( 2 ), STATUS )
      CALL GRP_SETCS( FITGRP( 2 ), .FALSE., STATUS )

      CALL GRP_NEW( 'Extension-item', DESGRP( 1 ), STATUS )
      CALL GRP_SETCS( DESGRP( 1 ), .FALSE., STATUS )
      CALL GRP_NEW( 'Extension_HDS-type', DESGRP( 2 ), STATUS )
      CALL GRP_SETCS( DESGRP( 2 ), .FALSE., STATUS )
      CALL GRP_NEW( 'Extension-function-of-FITS-items', DESGRP( 3 ),
     :               STATUS )
      CALL GRP_SETCS( DESGRP( 3 ), .FALSE., STATUS )

*  First pass looks for _HDS-type statements.
      DO 2 I = 1, NLINES

*  Get the first word of WRDGRP.
         CALL GRP_GET( WRDGRP( 1 ), I, 1, NAME1, STATUS )

*  Is it a recognisable HDS-type?
         IF ( NAME1( 1 : 8 ) .EQ. '_INTEGER' .OR.
     :        NAME1( 1 : 5 ) .EQ. '_REAL'    .OR.
     :        NAME1( 1 : 7 ) .EQ. '_DOUBLE'  .OR.
     :        NAME1( 1 : 8 ) .EQ. '_LOGICAL' .OR.
     :        NAME1( 1 : 5 ) .EQ. '_CHAR' ) THEN

*  Yes it is.  Check that the next word is a usable FITS keyword by
*  making it TRANSFORM-friendly and then attempting a dummy substitution
*  with it.
            CALL GRP_GET( WRDGRP( 2 ), I, 1, NAME2, STATUS )
            NAME3 = NAME2
            CALL CCD1_KTIDY( .TRUE., NAME3, IX, STATUS )
            CALL ERR_MARK
            CALL TRN_STOK( NAME3, ' ', ' ', NSUB, STATUS )

*  If TRANSFORM choked on it, we can't use this.
            IF ( STATUS .EQ. TRN__TOKIN ) THEN
               CALL ERR_ANNUL( STATUS )
               CALL GRP_GET( LINGRP, I, 1, NAME1, STATUS )
               CALL MSG_SETC( 'LINNUM', NAME1 )
               CALL MSG_SETC( 'KEY', NAME2 )
               STATUS = SAI__ERROR
               CALL ERR_REP( 'CCD1_FTGRP_ERR',
     :'  Invalid keyword name ^KEY at line ^LINNUM', STATUS )
               CALL ERR_RLSE
               GO TO 99
            END IF
            CALL ERR_RLSE

*  It's a usable keyword, so store it.
            MASK( I ) = .TRUE.
            CALL GRP_PUT( FITGRP( 1 ), 1, NAME2, 0, STATUS )
            CALL GRP_PUT( FITGRP( 2 ), 1, NAME1, 0, STATUS )
         END IF
 2    CONTINUE

*  May have not located all FITS-keywords yet but we cannot proceed
*  until all the type information is available (need to identify _CHAR
*  items). This may be in confusion as a knowledge of CCDPACK specific
*  extension items is allowed. So scan the lists and check for HDS-type
*  information. If none is available then guess it (could do this
*  quicker by folding this loop and the next together but will keep
*  separate for clarity).
      DO 3 I = 1, NLINES
         IF ( .NOT. MASK( I ) ) THEN

*  Have an unprocessed statement. Look at second element and see if it
*  is an HDS-type.
            CALL GRP_GET( WRDGRP( 2 ), I, 1, NAME2, STATUS )

*  Is it a recognisable HDS-type?
            IF ( .NOT. ( NAME2( 1 : 8 ) .EQ. '_INTEGER' .OR.
     :                   NAME2( 1 : 5 ) .EQ. '_REAL'    .OR.
     :                   NAME2( 1 : 7 ) .EQ. '_DOUBLE'  .OR.
     :                   NAME2( 1 : 8 ) .EQ. '_LOGICAL' .OR.
     :                   NAME2( 1 : 5 ) .EQ. '_CHAR' ) ) THEN

*  Not a FITS-keyword declaration and isn't a recognisable HDS-type.
*  Look at the first element and see if is possible to convert this
*  into a "known" CCDPACK extension item.
               CALL GRP_GET( WRDGRP( 1 ), I, 1, NAME1, STATUS )
               CALL CCD1_KNEXT( NAME1, YES, NAME4, STATUS )
               IF ( YES ) THEN

*  Is known and has a type. Reorganize the input group to reflect this
*  new state (last group may have trailing information so remember to
*  append this.
                  CALL GRP_GET( WRDGRP( 3 ), I, 1, NAME3, STATUS )
                  CALL GRP_PUT( WRDGRP( 2 ), 1, NAME4, I, STATUS )
                  IAT = CHR_LEN( NAME2 ) + 1
                  CALL CHR_APPND( NAME3, NAME2, IAT )
                  CALL GRP_PUT( WRDGRP( 3 ), 1, NAME2, I, STATUS )
               ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  Not recognised, hasn't been typed, isn't a FITS-keyword. Must be
*  a mistake. Set status, issue an error message and abort.
                  CALL GRP_GET( LINGRP, I, 1, NAME1, STATUS )
                  CALL MSG_SETC( 'LINNUM', NAME1 )
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'CCD1_FTGRP_ERR1', '  Unrecognisable'//
     :' statement or unknown extension item - line ^LINNUM', STATUS )
                  GO TO 99
               END IF
            END IF
         END IF
 3    CONTINUE

*  Now start looking at the other statements, we need to scan for any
*  "undeclared" FITS-keywords. These are generally single word
*  non-numeric strings which do not have any of the present
*  FITS-keywords referenced in them. _CHAR extension items are special
*  these need to be treat separately.
      DO 4 I = 1, NLINES
         IF ( .NOT. MASK( I ) ) THEN

*  Unprocessed statement. This now definitely has three elements. The
*  first is the name of a CCDPACK extension item, the second is the TYPE
*  of the extension item and the third is a functional statement which
*  describes how to create the value of the extension item. Now check
*  that the extension item is recognisable (know that some are from
*  previous loop, but not all have been checked) and has an appropriate
*  type.
            CALL GRP_GET( WRDGRP( 1 ), I, 1, NAME1, STATUS )
            CALL GRP_GET( WRDGRP( 2 ), I, 1, NAME2, STATUS )
            CALL CCD1_KNEXT( NAME1, YES, NAME3, STATUS )
            IF ( YES ) THEN

*  Ok item is known, is it typing correct?
C               IF ( NAME2 .NE. NAME3 ) THEN

*  No, better supersede this.
C                  CALL MSG_SETC( 'ITEM', NAME1 )
C                  CALL MSG_SETC( 'OLDTYPE', NAME2 )
C                  CALL MSG_SETC( 'NEWTYPE', NAME3 )
C                  CALL CCD1_MSG( ' ', '  Warning - data type of'//
C     :' extension item ^ITEM incorrect (^OLDTYPE) superseded by '//
C     :'(^NEWTYPE)', STATUS )

*  Overwrite the old type.
C                  CALL GRP_PUT( WRDGRP( 2 ), 1, NAME3, I, STATUS )
C                  NAME2 = NAME3
C               END IF
            ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  Unrecognised extension item. Give up.
               CALL GRP_GET( LINGRP, I, 1, NAME3, STATUS )
               CALL MSG_SETC( 'LINNUM', NAME3 )
               CALL MSG_SETC( 'ITEM', NAME1 )
               STATUS = SAI__ERROR
               CALL ERR_REP( 'CCD1_FTGRP_ERR1', '  Unrecognisable'//
     :' statement or unknown extension item (^ITEM) - line ^LINNUM',
     : STATUS )
            END IF
            IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Ok, now have a corroborated extension item, still havn't checked for
*  any further FITS-keywords (which may be implicit). See if third item
*  is singular and isn't numeric or logical, if this is so assume that
*  it might be FITS-keyword. If it's _CHAR then treat separetely.
            CALL GRP_GET( WRDGRP( 3 ), I, 1, NAME3, STATUS )
            CALL CHR_DCWRD( NAME3, 1, NWRD, START, STOP, WORDS, LSTAT )
            IF ( NAME2( 1 : 5 ) .NE. '_CHAR' ) THEN
               IF ( LSTAT .EQ. 0 ) THEN

*  Single word. Is it a numeric or logical value?
                  CALL ERR_MARK
                  CALL CHR_CTOD( NAME3, DVAL, STATUS )
                  IF ( STATUS .NE. SAI__OK ) THEN

*  Not numeric. Is it a possible logical?
                     CALL ERR_ANNUL( STATUS )
                     CALL CHR_CTOL( NAME3, LVAL, STATUS )
                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL ERR_ANNUL( STATUS )

*  Not a numeric not a logical and is a single word. The only
*  possibilities left are that this might already been declared or might
*  be a single word which is a function of FITS-keywords already known.

*  So check this name against those already in the group.
                        CALL GRP_INDEX( NAME3, FITGRP( 1 ), 1, INDEX,
     :                                  STATUS )
                        IF ( INDEX .EQ. 0 ) THEN

*  Isn't known, does it contain a reference to a known keyword? If so it
*  is a straight function, if not don't know what it is (probably
*  function of undeclared keyword).
                           CALL GRP_GRPSZ( FITGRP( 1 ), NFITS, STATUS )
                           LVAL = .FALSE.
                           DO 5 J = 1, NFITS
                              CALL GRP_GET( FITGRP( 1 ), J, 1, NAME4,
     :                                      STATUS )
                              CALL CCD1_KTIDY( .TRUE., NAME4, IX,
     :                                         STATUS )
                              NAME5 = NAME3
                              CALL CCD1_KTIDY( .TRUE., NAME5, IX,
     :                                         STATUS )
                              CALL TRN_STOK( NAME4, ' ', NAME5, NSUB,
     :                                       STATUS )
                              IF ( NSUB .NE. 0 ) THEN

*  Known keyword substitutes into value, must be a function.
                                 LVAL = .TRUE.
                                 GO TO 6
                              END IF
 5                         CONTINUE
 6                         CONTINUE
                           IF ( .NOT. LVAL ) THEN

*  Almost certainly is a new FITS-keyword, one final check. Is it a
*  valid token?  Test this by doing a dummy substitution.
                              WORDS( 1 ) = NAME3
                              CALL CCD1_KTIDY( .TRUE., WORDS( 1 ), IX,
     :                                         STATUS )
                              CALL ERR_MARK
                              CALL TRN_STOK( WORDS( 1 ), ' ', ' ', NSUB,
     :                                       STATUS )
                              IF ( STATUS .EQ. TRN__TOKIN ) THEN

*  It's an invalid token. Cannot process this regardless. Time to give
*  up on this.
                                 CALL ERR_ANNUL( STATUS )
                                 CALL GRP_GET( LINGRP, I, 1, NAME1,
     :                                         STATUS )
                                 CALL MSG_SETC( 'LINNUM', NAME1 )
                                 STATUS = SAI__ERROR
                                 CALL ERR_REP( 'CCD1_FTGRP_ERR2',
     :'  Unrecognisable statement - line ^LINNUM', STATUS )
                                 CALL MSG_SETC( 'NAME3', NAME3 )
                                 CALL ERR_REP( 'CCD1_FTGRP_ERR3',
     :'  Cannot interpret "^NAME3" as a FITS-keyword -- keywords in'//
     :' functions must be declared', STATUS )
                                 CALL ERR_RLSE
                                 CALL ERR_RLSE
                                 GO TO 99
                              ELSE

*  It's a FITS-keyword! Enter into group.
                                 CALL GRP_PUT( FITGRP( 1 ), 1, NAME3, 0,
     :                                         STATUS )
                                 CALL GRP_PUT( FITGRP( 2 ), 1, NAME2, 0,
     :                                         STATUS )
                              END IF
                              CALL ERR_RLSE
                           END IF
                        END IF
                     END IF
                  END IF
                  CALL ERR_RLSE
               END IF
            ELSE

*  _CHAR data type. This need special processing, should always have a
*  first word which is the name of a FITS-keyword. Just check if this
*  exists, if not add it to the list.
               CALL GRP_GRPSZ( FITGRP( 1 ), NFITS, STATUS )
               LVAL = .FALSE.
               DO 7 J = 1, NFITS
                  CALL GRP_GET( FITGRP( 1 ), J, 1, NAME4, STATUS )
                  CALL CCD1_KTIDY( .TRUE., NAME4, IX, STATUS )
                  NAME5 = NAME3( START( 1 ) : STOP( 1 ) )
                  CALL CCD1_KTIDY( .TRUE., NAME5, IX, STATUS )
                  CALL TRN_STOK( NAME4, ' ', NAME5, NSUB, STATUS )
                  IF ( NSUB .NE. 0 ) THEN

*  Known keyword substitutes into value, must be a function.
                     LVAL = .TRUE.
                     GO TO 8
                  END IF
 7             CONTINUE
 8             CONTINUE
               IF ( .NOT. LVAL ) THEN

*  Almost certainly is a new FITS-keyword, one final check. Is it a
*  valid token?  Test this by doing a dummy substitution.
                  WORDS( 1 ) = NAME3( START( 1 ) : STOP( 1 ) )
                  CALL CCD1_KTIDY( .TRUE., WORDS( 1 ), IX, STATUS )
                  CALL ERR_MARK
                  CALL TRN_STOK( WORDS( 1 ), ' ', ' ', NSUB, STATUS )
                  IF ( STATUS .EQ. TRN__TOKIN ) THEN

*  It's an invalid token. Cannot process this regardless. Time to give
*  up on this.
                     CALL ERR_ANNUL( STATUS )
                     CALL GRP_GET( LINGRP, I, 1, NAME1, STATUS )
                     CALL MSG_SETC( 'LINNUM', NAME1 )
                     STATUS = SAI__ERROR
                     CALL ERR_REP( 'CCD1_FTGRP_ERR2',
     :'  Unrecognisable statement - line ^LINNUM', STATUS )
                     CALL MSG_SETC( 'NAME3',
     :                              NAME3( START( 1 ): STOP( 1 ) ) )
                     CALL ERR_REP( 'CCD1_FTGRP_ERR3',
     :'  Cannot interpret "^NAME3" as a FITS-keyword', STATUS )
                     CALL ERR_RLSE
                     GO TO 99
                  ELSE

*  It's a FITS-keyword! Enter into group.
                     CALL GRP_PUT( FITGRP( 1 ), 1,
     :                             NAME3( START( 1 ) : STOP ( 1 ) ),
     :                             0, STATUS )
                     CALL GRP_PUT( FITGRP( 2 ), 1, NAME2, 0, STATUS )
                  END IF
                  CALL ERR_RLSE
               END IF
            END IF

*  Now record this extension-item together with its type and
*  fits function.
            CALL GRP_PUT( DESGRP( 1 ), 1, NAME1, 0, STATUS )
            CALL GRP_PUT( DESGRP( 2 ), 1, NAME2, 0, STATUS )
            CALL GRP_PUT( DESGRP( 3 ), 1, NAME3, 0, STATUS )
         END IF
 4    CONTINUE

*  Exit in a hurry label.
 99   CONTINUE
      END
* $Id$
