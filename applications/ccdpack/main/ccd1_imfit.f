      SUBROUTINE CCD1_IMFIT( FITGRP, DESGRP, NDF, FITBLK, FITLEN,
     :                       IVALS, RVALS, DVALS, LVALS, CVALS,
     :                       STATUS )
*+
*  Name:
*     CCD1_IMFIT

*  Purpose:
*     Imports FITS information into the CCDPACK extension of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_IMFIT( FITGRP, DESGRP, NDF, FITBLK, FITLEN,
*                      IVALS, RVALS, DVALS, LVALS, STATUS )

*  Description:
*     This routine interprets the information passed to it as instructions
*     about how to extract information from the FITS block of an NDF and
*     consequently how to use the values associated to derive a value for
*     named extension items in the NDF's CCDPACK extension. FITGRP and
*     DESGRP should have been produced the CCD1_FTGRP routine, these
*     contain the HDS types and names of the FITS-keywords to be used
*     (FITGRP) and the destination name and type of the CCDPACK extension
*     ITEM, together with a function which describes how to generate this
*     information from the values of the FITS-items (DESGRP).
*
*     The destination group function may be of several types. Most simply
*     it is a FITS-keyword in which case the value extracted from the
*     FITS block will be used (possibly after a type transformation of
*     some kind).  It can also be a TRANSFORM expression containing
*     references to FITS-keywords. Items whose destination type are
*     _LOGICAL must be translatable to a string which the routine CHR_CTOL
*     can process and should only contain a single FITS-keyword. Finally
*     if the destination item is of HDS type _CHAR then it is assumed to
*     be an expression which maps the values of string extracted from the
*     FITS block to strings which are valid in the CCDPACK extension,
*     unless it is a single word in which case the value is used
*     directly or unless it contains the string '//' in which case the
*     values are concatenated together.
*
*     The FITS items types need not be the same as the extension types as
*     translations between the various types are preformed. This may
*     occasionally result in an unwanted truncation, in this case all the
*     FITS-keywords should be extracted as _CHAR these will then be used
*     literally (i.e. as they appear).

*  Arguments:
*     FITGRP( 2 ) = INTEGER (Given)
*        GRP groups of FITS-keywords and their types. FITGRP( 1 )
*        contains the keywords, FITGRP( 2 ) their HDS types.
*     DESGRP( 3 ) = INTEGER (Given)
*        GRP groups of the destination extension item names, their
*        types and the function used to derive a value for the extension
*        item. DESGRP( 1 ) contains the extension names (which may be
*        hierarchical) DESGRP( 2 ) their HDS types and DESGRP( 3 ) the
*        functions of FITS-keywords which evaluate to the values to be
*        stored.
*     NDF = INTEGER (Given)
*        NDF identifier. This NDF will have a CCDPACK extension created
*        if one does not exist already.
*     FITBLK( FITLEN )= CHARACTER * ( * ) (Given)
*        Character array containing the FITS information stored in NDF
*        NDF (this is probably the mapped character array in the NDF
*        extension).
*     FITLEN = INTEGER (Given)
*        The number of FITS cards (array elements) in FITBLK.
*     IVALS( * ) = INTEGER (Given)
*        Storage space for any _INTEGER values extracted from the FITS
*        block. This should be at least as large as the number of FITS
*        items which will be extracted (size of FITGRP groups).
*     RVALS( * ) = REAL (Given)
*        Storage space for any _REAL values extracted from the FITS
*        block. This should be at least as large as the number of FITS
*        items which will be extracted (size of FITGRP groups).
*     DVALS( * ) = DOUBLE (Given)
*        Storage space for any _DOUBLE values extracted from the FITS
*        block. This should be at least as large as the number of FITS
*        items which will be extracted (size of FITGRP groups).
*     LVALS( * ) = INTEGER (Given)
*        Storage space for any _LOGICAL values extracted from the FITS
*        block. This should be at least as large as the number of FITS
*        items which will be extracted (size of FITGRP groups).
*     CVALS( * ) = INTEGER (Given)
*        Storage space for any pointers to _CHAR values extracted from
*        the FITS block. This should be at least as large as the number
*        of FITS items which will be extracted (size of FITGRP groups).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     8-DEC-1993 (PDRAPER):
*        Original version.
*     2-AUG-2000 (MBT):
*        Added support for FITS header values of the form '[X1:X2,Y1:Y2]'.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP parameters
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'MSG_PAR'          ! Message system parameters

*  Arguments Given:
      INTEGER FITGRP( 2 )
      INTEGER DESGRP( 3 )
      INTEGER NDF
      INTEGER FITLEN
      CHARACTER * ( * ) FITBLK( FITLEN )

*  Arguments Given and Returned:
      INTEGER IVALS( * )
      INTEGER CVALS( * )
      REAL RVALS( * )
      DOUBLE PRECISION DVALS( * )
      LOGICAL LVALS( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of string

*  Local Variables:
      CHARACTER * ( 12 ) INV    ! Inverse expression for FITS function
      CHARACTER * ( 12 ) TRNTYP ! Flexible type of TRANSFORM
      CHARACTER * ( MSG__SZMSG ) MESS ! Message buffer
      CHARACTER * ( DAT__SZLOC ) LOCTR ! Locator to transform structure
      CHARACTER * ( GRP__SZNAM ) EXTFUN ! Function to create extension value
      CHARACTER * ( GRP__SZNAM ) EXTNAM ! Name of extension item
      CHARACTER * ( GRP__SZNAM ) EXTTYP ! Extension item type
      CHARACTER * ( GRP__SZNAM ) FITVAL ! FITS keyword value
      CHARACTER * ( GRP__SZNAM ) KEYWRD ! FITS keyword name
      CHARACTER * ( GRP__SZNAM ) KEYBAS ! FITS keyword base name
      CHARACTER * ( GRP__SZNAM ) ORIFUN ! Unmodified form of EXTFUN
      CHARACTER * ( GRP__SZNAM + GRP__SZNAM ) FOR ! Forward expression for FITS function
      DOUBLE PRECISION DFIT     ! DBLE value of FITS item
      DOUBLE PRECISION DZERO( 1 ) ! 0.0D0
      DOUBLE PRECISION DVECT( 4 ) ! Numbers in "[X1:X2,Y1:Y2]" string
      INTEGER CHRGRP            ! Group for storing character values
      INTEGER FIRST             ! Position of first character in word
      INTEGER I                 ! Loop variable
      INTEGER IAT               ! Position in string
      INTEGER IX                ! Index of '[X1:X2,Y1:Y2]' substitution
      INTEGER NOWAT             ! Position in string
      INTEGER IFIT              ! Integer value of FITS item
      INTEGER IZERO( 1 )        ! 0
      INTEGER J                 ! Loop variable
      INTEGER LAST              ! Position of last character in word
      INTEGER NCHAR             ! Number of characters
      INTEGER NEXT              ! Number of extension items
      INTEGER NFITS             ! Number of FITS items
      INTEGER NSUBS             ! Number of substitutions
      INTEGER TRID              ! Transform identifier
      LOGICAL FOUND             ! Located item
      LOGICAL NOMORE            ! No more non-blank elements in string
      LOGICAL NOTFND            ! Failed to locate item
      LOGICAL NOTTRN            ! No translation possible
      REAL RFIT                 ! Real value of FITS item
      REAL RZERO( 1 )           ! 0.0
      INTEGER LENNAM            ! Length of extension item name
      INTEGER LENFUN            ! Length of function
      LOGICAL NOFUN             ! Whether a function exists or not
      INTEGER LENFOR            ! Length of forward expression
      INTEGER LENFIT            ! Length of FIT string
      INTEGER LENTYP            ! Length of item type
      LOGICAL STRICT            ! True if extension items and types must
                                ! be known and exist

*  Local Data:
      DATA DZERO / 0.0D0 /
      DATA RZERO / 0.0 /
      DATA IZERO / 0 /

*.

      STRICT = .FALSE.


*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of FITS-keywords we have to obtain.
      CALL GRP_GRPSZ( FITGRP( 1 ), NFITS, STATUS )

*  Create a GRP group to store any character values.
      CALL GRP_NEW( 'CCD1_IMFIT _CHAR keywords', CHRGRP, STATUS )

*  First job is to get the values of all the FITS-items which are
*  required. The names of the FITS-keywords are given in FITGRP( 1 ).
*  Their types are stored in FITGRP( 2 ).
      NCHAR = 0
      DO 1 I = 1, NFITS
         CVALS( I ) = 0
         IAT = 0
         CALL GRP_GET( FITGRP( 1 ), I, 1, KEYWRD, STATUS )
         CALL GRP_GET( FITGRP( 2 ), I, 1, FITVAL, STATUS )

*  Check whether the header keyword has an index into an [X1:X2,Y1:Y2]
*  type value.
         KEYBAS = KEYWRD
         CALL CCD1_KTIDY( .FALSE., KEYBAS, IX, STATUS )

*  The keyword has too many X1-type strings in it; this constitutes
*  a syntax error.
         IF ( IX .LT. 0 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'KEY', KEYWRD )
            CALL ERR_REP( 'CCD1_IMFIT_BADTEXT',
     : '  CCD1_IMFIT: Syntax error in keyword ^KEY', STATUS )

*  the value of the header is (asserted to be) a character string of
*  the form [x1:y1,x2:y2].
         ELSE IF ( IX .GT. 0 ) THEN

*  Get the character string.
            CALL FTS1_GKEYC( FITLEN, FITBLK, 1, KEYBAS, FOUND,
     :                       EXTFUN, IAT, STATUS )

*  Decode the string according to the value of IX to get a numeric value.
            CALL CCD1_DXY12( EXTFUN, DVECT, STATUS )

*  And store the value.
            IF ( FITVAL( 1 : 8 ) .EQ. '_INTEGER' ) THEN
               IVALS( I ) = INT( DVECT( IX ) )
            ELSE IF ( FITVAL( 1 : 5 ) .EQ. '_REAL' ) THEN
               RVALS( I ) = REAL( DVECT( IX ) )
            ELSE IF ( FITVAL( 1 : 7 ) .EQ. '_DOUBLE' ) THEN
               DVALS( I ) = DBLE( DVECT( IX ) )
            ELSE
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'KEY', KEYWRD )
               CALL ERR_REP( 'CCD1_IMFIT_BADTYP',
     : '  CCD1_IMFIT: Unrecognised numeric HDS type for FITS-keyword'//
     : ' ^KEY', STATUS )
               GO TO 99
            END IF

*  This is a normal header keyword, so we just need to pull the value
*  from the FITS header block and store it.
         ELSE
            IF ( FITVAL( 1 : 8 ) .EQ. '_INTEGER' ) THEN
               CALL FTS1_GKEYI( FITLEN, FITBLK, 1, KEYWRD, FOUND,
     :                          IVALS( I ), IAT, STATUS )
            ELSE IF ( FITVAL( 1 : 5 ) .EQ. '_REAL' ) THEN
               CALL FTS1_GKEYR( FITLEN, FITBLK, 1, KEYWRD, FOUND,
     :                          RVALS( I ), IAT, STATUS )
            ELSE IF ( FITVAL( 1 : 7 ) .EQ. '_DOUBLE' ) THEN
               CALL FTS1_GKEYD( FITLEN, FITBLK, 1, KEYWRD, FOUND,
     :                          DVALS( I ), IAT, STATUS )
            ELSE IF ( FITVAL( 1 : 8 ) .EQ. '_LOGICAL' ) THEN
               CALL FTS1_GKEYL( FITLEN, FITBLK, 1, KEYWRD, FOUND,
     :                          LVALS( I ), IAT, STATUS )
            ELSE IF ( FITVAL( 1 : 5 ) .EQ. '_CHAR' ) THEN

*  Characters are a special case, store these in a GRP group and retain
*  pointer information to extract them in order.
               CALL FTS1_GKEYC( FITLEN, FITBLK, 1, KEYWRD, FOUND,
     :                          EXTFUN, IAT, STATUS )
               NCHAR = NCHAR + 1
               CALL GRP_PUT( CHRGRP, 1, EXTFUN, NCHAR, STATUS )
               CVALS( I ) = NCHAR
            ELSE

*  Should never happen. Issue an error and abort.
               STATUS = SAI__ERROR
               CALL ERR_REP( 'CCD1_IMFIT_BADTYP',
     : '  CCD1_IMFIT: Unrecognised HDS type for FITS-keyword', STATUS )
               GO TO 99
            END IF
         END IF

*  If IAT is zero we failed to locate the FITS item.  Warn the user at
*  least, and either exit or carry on.
*  error and exit, or write a sensible value.
         IF ( IAT .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
            IF ( STRICT ) THEN

*  Consider this serious.
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'KEYWRD', KEYWRD )
               CALL NDF_MSG( 'NDF', NDF )
               CALL ERR_REP( 'CCD1_IMFIT_MISS',
     :'  Cannot locate the FITS keyword: ^KEYWRD, in the'//
     :' extension of NDF: ^NDF' , STATUS )
               GO TO 99
            ELSE
               CALL MSG_SETC( 'KEY', KEYWRD )
               CALL CCD1_MSG( ' ',
     :'  Warning: keyword ^KEY not found in FITS header.', STATUS )
               IF ( FITVAL( 1 : 8 ) .EQ. '_INTEGER' ) THEN
                  IVALS( I ) = 0
               ELSE IF ( FITVAL( 1 : 5 ) .EQ. '_REAL' ) THEN
                  RVALS( I ) = 0.0
               ELSE IF ( FITVAL( 1 : 7 ) .EQ. '_DOUBLE' ) THEN
                  DVALS( I ) = 0D0
               ELSE IF ( FITVAL( 1 : 8 ) .EQ. '_LOGICAL' ) THEN
                  LVALS( I ) = .FALSE.
               ELSE IF ( FITVAL( 1 : 5 ) .EQ. '_CHAR' ) THEN
                  NCHAR = NCHAR + 1
                  CALL GRP_PUT( CHRGRP, 1, ' ', NCHAR, STATUS )
                  CVALS( I ) = NCHAR
               END IF
            END IF
         END IF
 1    CONTINUE

*  Write out the header for the information messages about the items
*  imported.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      MESS = '      Item                Value'
      CALL CCD1_MSG( ' ', MESS, STATUS )
      MESS = '      ----                -----'
      CALL CCD1_MSG( ' ', MESS, STATUS )

*  Now have all keywords values. Now loop for all extension items. The
*  values of extension items are derived from the function information
*  given in DESGRP( 3 ).
      CALL GRP_GRPSZ( DESGRP( 1 ), NEXT, STATUS )
      DO 2 I = 1, NEXT

*  Get the values for this extension item, the destination name, its
*  type and the function of FITS-keywords that result in a value for it.
         CALL GRP_GET( DESGRP( 1 ), I, 1, EXTNAM, STATUS )
         LENNAM = CHR_LEN( EXTNAM( :80 ) )
         CALL GRP_GET( DESGRP( 2 ), I, 1, EXTTYP, STATUS )
         LENTYP = CHR_LEN( EXTTYP( :15 ) )
         CALL GRP_GET( DESGRP( 3 ), I, 1, EXTFUN, STATUS )
         ORIFUN = EXTFUN
         CALL CCD1_KTIDY( .TRUE., EXTFUN, IX, STATUS )
         LENFUN = CHR_LEN( EXTFUN )
         NOFUN = .FALSE.
         IF ( LENFUN .EQ. 0 ) NOFUN = .TRUE.
         IF ( EXTTYP( 1 : 5 ) .EQ. '_CHAR' ) THEN

*  Result is a character value. The Function for this should either be
*  a single keyword, a concatenation expression or a keyword followed
*  by string1=string2 ... statements. The values on the right-hand
*  side of the equations are the allowed names of the CCDPACK
*  extension items the values on the left should equate to those
*  picked up from the FITS extensions.
*  First get the name of the keyword.
            IF ( NOFUN ) THEN

*  Must be an empty field. Cannot do anything with this.
               CALL MSG_SETC( 'ITEM', EXTNAM )
               CALL CCD1_MSG( ' ', ' Warning - unable to form a '//
     :         'value for _CHAR extension item ^ITEM', STATUS )
            ELSE

*  Check if the "function" contains the concatenation operator, if so
*  then remove all the // strings and do the work.
               IF ( INDEX( EXTFUN( :LENFUN ), '//' ) .NE. 0 ) THEN
                  CALL CCD1_REPC( EXTFUN( :LENFUN ), '/', ' ', STATUS )

*  Loop over all keywords and look for their associated values.
                  IAT = 1
                  NOWAT = 0
                  FITVAL = ' '
 9                CONTINUE
                  CALL KPG_NXWRD( EXTFUN( :LENFUN ), IAT, FIRST,
     :                             LAST, NOTFND, STATUS )
                  IAT = LAST + 1
                  IF ( .NOT. NOTFND ) THEN
                     DO 8 J = 1, NFITS
                        CALL GRP_GET( FITGRP( 1 ), J, 1, KEYWRD, STATUS)
                        CALL CCD1_KTIDY( .TRUE., KEYWRD, IX, STATUS )
                        IF ( EXTFUN( FIRST : LAST ) .EQ.
     :                       KEYWRD( : LAST - FIRST + 1 ) ) THEN
                           FOUND = .TRUE.

*  Found keyword, now need its value. This could be store in any type so
*  need to translate.
                           CALL GRP_GET( FITGRP( 2 ), J, 1, FOR, STATUS)
                           IF ( FOR( 1 : 8 ) .EQ. '_INTEGER' ) THEN
                              CALL CHR_ITOC( IVALS( J ), FOR, NCHAR )
                           ELSE IF ( FOR( 1 : 5 ) .EQ. '_REAL' ) THEN
                              CALL CHR_RTOC( RVALS( J ), FOR, NCHAR )
                           ELSE IF ( FOR( 1 : 7 ) .EQ. '_DOUBLE' )
     :                     THEN
                              CALL CHR_DTOC( DVALS( J ), FOR, NCHAR )
                           ELSE IF ( FOR( 1 : 8 ) .EQ. '_LOGICAL' ) THEN
                              CALL CHR_LTOC( LVALS( J ), FOR, NCHAR )
                           ELSE
                              CALL GRP_GET( CHRGRP, CVALS( J ), 1,
     :                                      FOR, STATUS )
                           END IF
                           CALL CHR_APPND( FOR, FITVAL, NOWAT )

*  Skip more loops and try for next word.
                           GO TO 9
                        END IF
 8                   CONTINUE

*  Only arrive here if the FITS keyword wasn't matched. If this case
*  we must flag an error and abort.
                     FOUND = .FALSE.
                  END IF
                  FIRST = 1
               ELSE

*  Now look for the first word (must exist).
                  CALL KPG_NXWRD( EXTFUN( :LENFUN ), 1, FIRST, LAST,
     :                             NOTFND, STATUS )

*  If this is the only word and the string is not a concatenation
*  function then the associated value is that of the FITS-keyword.
                  IF ( LAST .EQ. LENFUN ) THEN
                     NOMORE = .TRUE.
                  ELSE
                     NOMORE = .FALSE.
                  END IF

*  Ok now look for the value associated with this keyword.
                  NCHAR = LAST - FIRST + 1
                  FOUND = .FALSE.
                  DO 3 J = 1, NFITS
                     CALL GRP_GET( FITGRP( 1 ), J, 1, KEYWRD, STATUS )
                     CALL CCD1_KTIDY( .TRUE., KEYWRD, IX, STATUS )
                     IF ( EXTFUN( 1: NCHAR ) .EQ. KEYWRD( 1: NCHAR ) )
     :               THEN
                        FOUND = .TRUE.

*  Found keyword, now need its value. This could be store in any type so
*  need to translate.
                        CALL GRP_GET( FITGRP( 2 ), J, 1, FOR, STATUS )
                        IF ( FOR( 1 : 8 ) .EQ. '_INTEGER' ) THEN
                           CALL CHR_ITOC( IVALS( J ), FITVAL, NCHAR )
                        ELSE IF ( FOR( 1 : 5 ) .EQ. '_REAL' ) THEN
                           CALL CHR_RTOC( RVALS( J ), FITVAL, NCHAR )
                        ELSE IF ( FOR( 1 : 7 ) .EQ. '_DOUBLE' ) THEN
                           CALL CHR_DTOC( DVALS( J ), FITVAL, NCHAR )
                        ELSE IF ( FOR( 1 : 8 ) .EQ. '_LOGICAL' ) THEN
                           CALL CHR_LTOC( LVALS( J ), FITVAL, NCHAR )
                        ELSE
                           CALL GRP_GET( CHRGRP, CVALS( J ), 1, FITVAL,
     :                                   STATUS )
                           NCHAR = CHR_LEN( FITVAL( :80 ) )
                        END IF

*  If necessary test this against the trailing description. Have
*  several possibilities for the form here;
*     string1=string2
*     string1 =string2
*     string1= string2
*     string1 = string2
*   This should cope with all these cases
                        IF ( .NOT. NOMORE ) THEN
                           NOTTRN = .FALSE.
                           IAT = INDEX( EXTFUN(LAST+1: ),
     :                                  FITVAL(:NCHAR ) )
                           IF ( IAT .NE. 0 ) THEN

*  Have found a match for this string in the trailing description, look
*  for '=', which should be next character or next word.
                              IAT = LAST + IAT + NCHAR
                              IF ( EXTFUN( IAT: IAT ) .NE. '=' ) THEN
                                 CALL KPG_NXWRD( EXTFUN, IAT, FIRST,
     :                                            LAST, NOTFND, STATUS )
                                 IF ( NOTFND ) THEN

*  Probably have a misleading syntax, just use the default value.
                                    NOTTRN = .TRUE.
                                 ELSE

*  This should be an equals sign
                                    IF ( EXTFUN( FIRST : FIRST ) .EQ.
     :                                   '=' ) THEN

*  Next word is the actual value.
                                       CALL KPG_NXWRD( EXTFUN,
     :                                                  FIRST + 1,FIRST,
     :                                                  LAST, NOTFND,
     :                                                  STATUS )
                                       IF ( NOTFND ) THEN

*  Probably have a misleading syntax, just use the default value.
                                          NOTTRN = .TRUE.
                                       ELSE

*  Got a value.
                                          FITVAL = EXTFUN( FIRST: LAST )
                                       END IF
                                    ELSE

*  Probably have a misleading syntax, just use the default value.
                                       NOTTRN = .TRUE.
                                    END IF
                                 END IF
                              ELSE

*  No space next word is translation.
                                 CALL KPG_NXWRD( EXTFUN, IAT + 1,
     :                                            FIRST, LAST, NOTFND,
     :                                            STATUS )
                                 IF ( .NOT. NOTFND ) THEN
                                    FITVAL = EXTFUN( FIRST : LAST )
                                 ELSE
                                    NOTTRN = .TRUE.
                                 END IF
                              END IF
                           ELSE
                              NOTTRN = .TRUE.
                           END IF
                           IF ( NOTTRN ) THEN

*  Failed to find a match for this. Just use the FITS-keyword value
*  directly.
                              CALL MSG_SETC( 'KEYWORD', KEYWRD )
                              CALL MSG_SETC( 'FITVAL', FITVAL )
                              CALL CCD1_MSG( ' ',
     :                        '  Warning - failed to locate a'//
     :                        ' translation for FITS-keyword'//
     :                        ' ^KEYWORD, using raw  value (^FITVAL)',
     :                        STATUS)
                           END IF
                        END IF

*  Skip any remaining loops.
                        GO TO 4
                     END IF
 3                CONTINUE
 4                CONTINUE
               END IF
               IF ( .NOT. FOUND ) THEN

*  Failed to locate a value for this keyword. Issue warning.
                  CALL MSG_SETC( 'KEYWORD', EXTFUN )
                  CALL CCD1_MSG( ' ', '  Warning - failed to obtain'//
     :            ' a value for ^KEYWORD', STATUS )
               ELSE

*  Put the value into the extension.
                  LENFIT = LAST - FIRST + 1
                  CALL CCG1_STOCC( NDF, EXTNAM, FITVAL( : LENFIT ),
     :                             STATUS )

*  Inform user of result.
                   MESS = ' '
                   MESS( 5 : )  = EXTNAM
                   MESS( 28 : ) = FITVAL( :LENFIT )
                   CALL CCD1_MSG( ' ', MESS, STATUS )
               END IF
            END IF

         ELSE IF ( EXTTYP( 1 : 8 ) .EQ. '_LOGICAL' ) THEN

*  Logicals are special too. Single value allowed must be translatable
*  by the CHR_CTOL routine. First get name of the keyword.
            IF ( NOFUN ) THEN

*  Must be an empty field. Cannot do anything with this.
               CALL MSG_SETC( 'ITEM', EXTNAM )
               CALL CCD1_MSG( ' ', '  Warning - unable to form a '//
     :         'value for _LOGICAL extension item ^ITEM', STATUS )
            ELSE

*  Look for word in function.
            CALL KPG_NXWRD( EXTFUN ( : LENFUN ), 1, FIRST, LAST,
     :                       NOTFND, STATUS )

*  Is this the only word or do other exist? This should be the only
*  word.
               IF ( LAST .EQ. LENFUN ) THEN
                  NOMORE = .TRUE.
               ELSE
                  NOMORE = .FALSE.
               END IF

*  If more exis, issue a warning at they will be ignored.
               IF ( .NOT. NOMORE ) THEN
                  CALL MSG_SETC( 'EXTRA', EXTFUN( :LENFUN ) )
                  CALL CCD1_MSG( ' ', '  Warning - extra information'//
     :' following _LOGICAL keyword will be ignored (^EXTRA)', STATUS )
               END IF

*  Ok now look for the value associated with this keyword.
               NCHAR = LAST - FIRST + 1
               FOUND = .FALSE.
               DO 5 J = 1, NFITS
                  CALL GRP_GET( FITGRP( 1 ), J, 1, KEYWRD, STATUS )
                  CALL CCD1_KTIDY( .TRUE., KEYWRD, IX, STATUS )
                  IF ( EXTFUN( 1: NCHAR ) .EQ. KEYWRD( 1: NCHAR ) )
     :            THEN
                     FOUND = .TRUE.

*  Found keyword, now need its value. This could be store in any type so
*  need to translate.
                     IF ( EXTTYP( 1 : 8 ) .EQ. '_INTEGER' ) THEN
                        CALL CHR_ITOC( IVALS( J ), FITVAL, NCHAR )
                     ELSE IF ( EXTTYP( 1 : 5 ) .EQ. '_REAL' ) THEN
                        CALL CHR_RTOC( RVALS( J ), FITVAL, NCHAR )
                     ELSE IF ( EXTTYP( 1 : 7 ) .EQ. '_DOUBLE' ) THEN
                        CALL CHR_DTOC( DVALS( J ), FITVAL, NCHAR )
                     ELSE IF ( EXTTYP( 1 : 8 ) .EQ. '_logiCAL' ) THEN
                        CALL CHR_LTOC( LVALS( J ), FITVAL, NCHAR )
                     ELSE
                        CALL GRP_GET( CHRGRP, CVALS( J ), 1, FITVAL,
     :                                STATUS )
                        NCHAR = CHR_LEN( FITVAL )
                     END IF

*  Convert the character string to logical. Just to check.
                     CALL ERR_MARK
                     CALL CHR_CTOL( FITVAL, NOTFND, STATUS )
                     IF ( STATUS .NE. SAI__OK ) THEN

*  Mustn't be a recognisable logical value.
                        NOTTRN = .FALSE.
                     END IF
                     CALL ERR_RLSE

*  Enter value into extension if ok.
                     IF ( .NOT. NOTTRN ) THEN
                        CALL CCG1_STOCL( NDF, EXTNAM( :LENNAM ),
     :                                   NOTFND, STATUS )
                        MESS = ' '
                        MESS( 5 : )  = EXTNAM
                        CALL CHR_LTOC( NOTFND, MESS( 28 : ), NCHAR )
                        CALL CCD1_MSG( ' ', MESS, STATUS )
                     ELSE

*  No value -- issue warning.
                        CALL MSG_SETC( 'FITVAL', FITVAL )
                        CALL MSG_SETC( 'KEYWRD', KEYWRD )
                        CALL CCD1_MSG( ' ', '  Unable to translate '//
     :' (^FITVAL) to a _LOGICAL value (FITS keyword - ^KEYWRD)', STATUS)
                     END IF

*  One value is enough. Skip any extra loops.
                     GO TO 6
                  END IF
 5             CONTINUE
 6             CONTINUE
            END IF
         ELSE

*  Assume that this is a recognisable HDS numeric type. The HDS type of
*  the extension item is the one we are aiming for, but the components
*  may be of differing types to this. Use TRANSFORM to work out these
*  values _and_ how to combine all the differing types (":" precision).
*  First job in this case is to replace all tokens with their values.
*  We will manafacture tokens for the actual expression evaluation.
            DO 7 J = 1, NFITS
               CALL GRP_GET( FITGRP( 1 ), J, 1, KEYWRD, STATUS )
               CALL CCD1_KTIDY( .TRUE., KEYWRD, IX, STATUS )
               NCHAR = CHR_LEN( KEYWRD )
               CALL GRP_GET( FITGRP( 2 ), J, 1, FITVAL, STATUS )
               IF ( FITVAL( 1 : 8 ) .EQ. '_INTEGER' ) THEN
                  CALL TRN_STOKI( KEYWRD( 1: NCHAR ), IVALS( J ),
     :                            EXTFUN, NSUBS, STATUS )
               ELSE IF ( FITVAL( 1 : 5 ) .EQ. '_REAL' ) THEN
                  CALL TRN_STOKR( KEYWRD( 1: NCHAR ), RVALS( J ),
     :                            EXTFUN, NSUBS, STATUS )
               ELSE IF ( FITVAL( 1 : 7 ) .EQ. '_DOUBLE' ) THEN
                  CALL TRN_STOKD( KEYWRD( 1: NCHAR ), DVALS( J ),
     :                            EXTFUN, NSUBS, STATUS )
               ELSE IF ( FITVAL( 1: 5 ) .EQ. '_CHAR' ) THEN

*  Character FITS item -- just trust the caller and use the raw
*  character data, no translation. A possible problem with this is
*  tokens which require parentheses (negative values in certain cases)
*  so add () to all values.
                  CALL GRP_GET( CHRGRP, CVALS( J ), 1, FITVAL, STATUS )
                  LENFIT = CHR_LEN( FITVAL )
                  FOR = '('//FITVAL( : LENFIT )//')'
                  CALL TRN_STOK( KEYWRD( 1: NCHAR ),
     :                           FOR( : LENFIT + 2 ),
     :                           EXTFUN( :LENFUN ), NSUBS, STATUS )
               ELSE

*  Must be _LOGICAL, cannot do anything with this. Complain.
                  CALL MSG_SETC( 'EXTNAM', EXTNAM( : LENNAM ) )
                  CALL CCD1_MSG( ' ', '  Warning - cannot handle'//
     :' _LOGICAL FITS-keywords in FITS-keyword functions (extension'//
     :' item ^EXTNAM). Not performing translation of this item',
     : STATUS )
                  GO TO 2
               END IF
 7          CONTINUE

*  Ok now need to convert the FITS-keyword function string into one
*  which we can translate using TRANSFORM. Add the dummy variables
*  CCDPACK_DUM1 and CCDPACK_DUM2 to convert into compilable expressions.
            LENFUN = CHR_LEN( EXTFUN )
            FOR = 'CCDPACK_DUM2 ='// EXTFUN( : LENFUN  )//
     :            '+CCDPACK_DUM1*0'
            LENFOR = LENFUN + 30
            INV = 'CCDPACK_DUM1'

*  Create a temporary transform structure. Add a colon to HDS type
*  to allow flexible type processing. Finally enter value into
*  extension. Encapsulate any messages from this part. They are more
*  likely to be the result of a input error, than problems TRANSFORM
*  should be worried about. Inform the user of the value derived and
*  entered into the extension.
            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL ERR_MARK
               TRNTYP = EXTTYP( :LENTYP  ) //':'
               CALL TRN_NEW( 1, 1, FOR( : LENFOR ), INV , TRNTYP,
     :                       ' ', ' ', ' ', LOCTR, STATUS )
               CALL TRN_COMP( LOCTR, .TRUE., TRID, STATUS )
               MESS = ' '
               MESS( 5 : )  = EXTNAM( : LENNAM )
               CALL MSG_SETC( 'ITEM', EXTNAM( :LENNAM ) )
               IF ( EXTTYP( 1 : 8 ) .EQ. '_INTEGER' ) THEN
                  CALL TRN_TR1I( .FALSE., 1, IZERO, TRID, IFIT, STATUS )
                  CALL CCG1_STOCI( NDF, EXTNAM (:LENNAM ), IFIT,
     :                             STATUS )
                  CALL CHR_ITOC( IFIT, MESS( 28 : ), NCHAR )
               ELSE IF ( EXTTYP( 1 : 5 ) .EQ. '_REAL' ) THEN
                  CALL TRN_TR1R( .FALSE., 1, RZERO, TRID, RFIT, STATUS )
                  CALL CCG1_STOCR( NDF, EXTNAM( : LENNAM ), RFIT,
     :                             STATUS )
                  CALL CHR_RTOC( RFIT, MESS( 28 : ), NCHAR )
               ELSE
                  CALL TRN_TR1D( .FALSE., 1, DZERO, TRID, DFIT, STATUS )
                  CALL CCG1_STOCD( NDF, EXTNAM( : LENNAM ), DFIT,
     :                             STATUS )
                  CALL CHR_DTOC( DFIT, MESS( 28 : ), NCHAR )
               END IF
               CALL CCD1_MSG( ' ', MESS, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_ANNUL( STATUS )
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'FUN', ORIFUN )
                  CALL MSG_SETC( 'EXTNAM', EXTNAM( : LENNAM ) )
                  CALL ERR_REP( ' ', '  Cannot interpret:"^FUN", '//
     :'as a valid transform for any known FIT-keywords '//
     : '(extension item ^EXTNAM).', STATUS )
                  CALL ERR_RLSE
                  GO TO 99
               END IF
               CALL ERR_RLSE
            END IF
         END IF
 2    CONTINUE

*  Exit in a hurry label.
 99   CONTINUE
      END

* $Id$
