      SUBROUTINE POL1_IMFIT( FITGRP, DESGRP, NDF, FITBLK, FITLEN,
     :                       IVALS, RVALS, DVALS, LVALS, CVALS,
     :                       GOTFV, STATUS )
*+
*  Name:
*     POL1_IMFIT

*  Purpose:
*     Imports FITS information into the POLPACK extension of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_IMFIT( FITGRP, DESGRP, NDF, FITBLK, FITLEN,
*                      IVALS, RVALS, DVALS, LVALS, CVALS, GOTFV, STATUS )

*  Description:
*   This routine interprets the information passed to it as instructions
*   about how to extract information from the FITS block of an NDF and
*   consequently how to use the values associated to derive a value for
*   named extension items in the NDF's POLPACK extension. FITGRP and
*   DESGRP should have been produced the CCD1_FTGRP routine, these
*   contain the HDS types and names of the FITS-keywords to be used
*   (FITGRP) and the destination name and type of the POLPACK extension
*   ITEM, together with a function which describes how to generate this
*   information from the values of the FITS-items (DESGRP).
*
*   The destination group function may be of several types. Most simply
*   it is a FITS-keyword in which case the value extracted from the
*   FITS block will be used (possibly after a type transformation of
*   some kind).  It can also be a TRANSFORM expression containing
*   references to FITS-keywords. Items whose destination type are
*   _LOGICAL must be translatable to a string which the routine CHR_CTOL
*   can process and should only contain a single FITS-keyword. Finally
*   if the destination item is of HDS type _CHAR then it is assumed to
*   be an expression which maps the values of string extracted from the
*   FITS block to strings which are valid in the POLPACK extension.
*
*   The FITS items types need not be the same as the extension types as
*   translations between the various types are preformed. This may
*   occasionally result in an unwanted truncation, in this case all the
*   FITS-keywords should be extracted as _CHAR these will then be used
*   literally (i.e. as they appear).

*  Arguments:
*     FITGRP( 2 ) = INTEGER (Given)
*        GRP groups of FITS-keywords and their types. FITGRP( 1 )
*        contains the keywords (terminated by a question mark if no error
*        is to be reported if the keyword does not exist), FITGRP( 2 ) 
*        their HDS types.
*     DESGRP( 3 ) = INTEGER (Given)
*        GRP groups of the destination extension item names, their
*        types and the function used to derive a value for the extension
*        item. DESGRP( 1 ) contains the extension names (which may be
*        hierarchical, and may be terminated by a question mark), 
*        DESGRP( 2 ) their HDS types and DESGRP( 3 ) the functions of 
*        FITS-keywords which evaluate to the values to be stored.
*     NDF = INTEGER (Given)
*        NDF identifier. This NDF will have a POLPACK extension created
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
*     LVALS( * ) = LOGICAL (Given)
*        Storage space for any _LOGICAL values extracted from the FITS
*        block. This should be at least as large as the number of FITS
*        items which will be extracted (size of FITGRP groups).
*     CVALS( * ) = INTEGER (Given)
*        Storage space for any pointers to _CHAR values extracted from
*        the FITS block. This should be at least as large as the number
*        of FITS items which will be extracted (size of FITGRP groups).
*     GOTFV( * ) = LOGICAL (Given)
*        Storage space for falgs indicating if a value for the corresponding 
*        keyword was found. This should be at least as large as the number 
*        of FITS items which will be extracted (size of FITGRP groups).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-DEC-1993 (PDRAPER):
*        Original version.
*     3-DEC-1997 (DSB):
*        CCDPACK version modified for use in POLPACK.
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
      INCLUDE 'TRN_ERR'          ! TRANSFORM error codes

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
      LOGICAL GOTFV( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of string

*  Local Variables:
      CHARACTER * ( 12 ) INV     ! Inverse expression for FITS function
      CHARACTER * ( 12 ) TRNTYP  ! Flexible type of TRANSFORM
      CHARACTER * ( 80 ) CMNT    ! FITS comment
      CHARACTER * ( DAT__SZLOC ) LOCTR ! Locator to transform structure
      CHARACTER * ( GRP__SZNAM ) EXTFUN ! Function to create extension value
      CHARACTER * ( GRP__SZNAM ) EXTNAM ! Name of extension item
      CHARACTER * ( GRP__SZNAM ) EXTTYP ! Extension item type
      CHARACTER * ( GRP__SZNAM ) FITVAL ! FITS keyword value
      CHARACTER * ( GRP__SZNAM ) KEYWRD ! FITS keyword name
      CHARACTER * ( GRP__SZNAM + GRP__SZNAM ) FOR ! Forward expression for FITS function
      DOUBLE PRECISION DFIT      ! DBLE value of FITS item
      DOUBLE PRECISION DZERO( 1 ) ! 0.0D0
      INTEGER CHRGRP             ! Group for storing character values
      INTEGER FIRST              ! Position of first character in word
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Position in string
      INTEGER IC                 ! Next index in literal string
      INTEGER IFIT               ! Integer value of FITS item
      INTEGER IZERO( 1 )         ! 0
      INTEGER J                  ! Loop variable
      INTEGER LAST               ! Position of last character in word
      INTEGER NCHAR              ! Number of characters
      INTEGER NEXT               ! Number of extension items
      INTEGER NFITS              ! Number of FITS items
      INTEGER NSUBS              ! Number of substitutions
      INTEGER TRID               ! Transform identifier
      LOGICAL FOUND              ! Located item
      LOGICAL NOMORE             ! No more non-blank elements in string
      LOGICAL NOTFND             ! Failed to locate item
      LOGICAL NOTTRN             ! No translation possible
      REAL RFIT                  ! Real value of FITS item
      REAL RZERO( 1 )            ! 0.0
      INTEGER LENNAM             ! Length of extension item name 
      INTEGER LENFUN             ! Length of function
      LOGICAL NOFUN              ! Whether a function exists or not
      INTEGER LENFOR             ! Length of forward expression
      INTEGER LENFIT             ! Length of FIT string
      INTEGER LENTYP             ! Length of item type
      LOGICAL QUEST              ! Did extension item name end with a "?" ?

*  Local Data:
      DATA DZERO / 0.0D0 / 
      DATA RZERO / 0.0 / 
      DATA IZERO / 0 / 

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of FITS-keywords we have to obtain.
      CALL GRP_GRPSZ( FITGRP( 1 ), NFITS, STATUS )

*  Create a GRP group to store any character values.
      CALL GRP_NEW( 'POL1_IMFIT _CHAR keywords', CHRGRP, STATUS )

*  First job is to get the values of all the FITS-items which are
*  required. The names of the FITS-keywords are given in FITGRP( 1 ).
*  Their types are stored in FITGRP( 2 ).
      NCHAR = 0 
      DO 1 I = 1, NFITS
         CALL GRP_GET( FITGRP( 1 ), I, 1, KEYWRD, STATUS )
         CALL GRP_GET( FITGRP( 2 ), I, 1, FITVAL, STATUS )
         CVALS( I ) = 0
         IAT = 0
         IF ( FITVAL( 1 : 8 ) .EQ. '_INTEGER' ) THEN
            IVALS( I ) = 0
            CALL FTS1_GKEYI( FITLEN, FITBLK, 1, KEYWRD, 0, FOUND,
     :                       IVALS( I ), CMNT, IAT, STATUS )
         ELSE IF ( FITVAL( 1 : 5 ) .EQ. '_REAL' ) THEN
            RVALS( I ) = 0.0
            CALL FTS1_GKEYR( FITLEN, FITBLK, 1, KEYWRD, 0, FOUND,
     :                       RVALS( I ), CMNT, IAT, STATUS )
         ELSE IF ( FITVAL( 1 : 7 ) .EQ. '_DOUBLE' ) THEN
            DVALS( I ) = 0.0
            CALL FTS1_GKEYD( FITLEN, FITBLK, 1, KEYWRD, 0, FOUND,
     :                       DVALS( I ), CMNT, IAT, STATUS )
         ELSE IF ( FITVAL( 1 : 8 ) .EQ. '_LOGICAL' ) THEN
            LVALS( I ) = .FALSE.
            CALL FTS1_GKEYL( FITLEN, FITBLK, 1, KEYWRD, 0, FOUND,
     :                       LVALS( I ), CMNT, IAT, STATUS )
         ELSE IF ( FITVAL( 1 : 5 ) .EQ. '_CHAR' ) THEN

*  Characters are a special case, store these in a GRP group and retain
*  pointer information to extract them in order.
            EXTFUN = ' '
            CALL FTS1_GKEYC( FITLEN, FITBLK, 1, KEYWRD, 0, FOUND,
     :                       EXTFUN, CMNT, IAT, STATUS )
            NCHAR = NCHAR + 1
            CALL GRP_PUT( CHRGRP, 1, EXTFUN, NCHAR, STATUS )
            CVALS( I ) = NCHAR
         ELSE

*  Should never happen. Issue an error and abort.
            IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'POL1_IMFIT_BADTYP',
     : '  POL1_IMFIT: Unrecognised HDS type for FITS-keyword', STATUS )
            END IF

            GO TO 99
         END IF

*  If IAT is zero we failed to locate the FITS item.
         GOTFV( I ) = ( IAT .NE. 0 )

 1    CONTINUE

*  Now have keywords values. Now loop for all extension items. The
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
         LENFUN = CHR_LEN( EXTFUN )
         NOFUN = .FALSE.
         IF ( LENFUN .EQ. 0 ) NOFUN = .TRUE.

*  If the extension item name ends with a question mark, then remove it
*  and indicate that no error should be reported if it cannot be assigned 
*  a value due to lack of the required FITS keywords.
         IF ( EXTNAM( LENNAM : LENNAM ) .EQ. '?' ) THEN
            EXTNAM( LENNAM : LENNAM ) = ' '
            LENNAM = LENNAM - 1
            QUEST = .TRUE.
         ELSE
            QUEST = .FALSE.
         END IF
                                                              
*  Check the data type of the extension item.
         IF ( EXTTYP( 1 : 5 ) .EQ. '_CHAR' ) THEN             

*  Result is a character value. If the first and last non-blank 
*  characters are double quotes, store the intervening string in 
*  the extension.
            IC = 0
            IF( LENFUN .GT. 0 ) THEN
               IF( EXTFUN( LENFUN : LENFUN ) .EQ. '"' ) THEN
                  IC = 1
                  DO WHILE( IC .LE. LENFUN .AND. 
     :                      EXTFUN( IC : IC ) .EQ. ' ' )
                     IC = IC + 1
                  END DO
                  IF( IC .LE. LENFUN ) THEN
                     IF( EXTFUN( IC : IC ) .EQ. '"' ) THEN                  
                        IF( IC + 1 .LE. LENFUN - 1 ) THEN
                           CALL POL1_STOCC( NDF, EXTNAM, 
     :                                    EXTFUN( IC + 1 : LENFUN - 1 ),
     :                                    STATUS )
                        ELSE
                           CALL POL1_STOCC( NDF, EXTNAM, ' ', STATUS )
                        END IF                        
                     END IF
                  END IF
               END IF
            END IF

*  Otherwise, the Function for this should either be
*  a single keyword or a keyword followed by string1=string2 ...
*  statements. The values on the right-hand side of the equations are
*  the allowed names of the POLPACK extension items the values on the
*  left should equate to those picked up from the FITS extensions.
*  First get the name of the keyword.
            IF ( NOFUN ) THEN                                

*  Must be an empty field. Cannot do anything with this.
               CALL MSG_SETC( 'ITEM', EXTNAM )
               CALL MSG_OUT( ' ', ' Warning - unable to form a '//
     :         'value for _CHAR extension item ^ITEM', STATUS )

            ELSE IF( IC .EQ. 0 ) THEN

*  Look for word (must exist) .
               CALL CCD1_NXWRD( EXTFUN( :LENFUN ), 1, FIRST, LAST,
     :                          NOTFND, STATUS )

*  Is this the only word or do other exist? If this is the only word
*  then the associated value is that of the FITS-keyword. Otherwise we
*  need to decode the equations.
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
                  IF ( EXTFUN( 1: NCHAR ) .EQ. KEYWRD( 1: NCHAR ) )
     :            THEN 
                     FOUND = .TRUE.                           

*  Found keyword. If it has no value, report an error unless the
*  extension item terminated in a question mark. Otherwise, jump
*  forward to do the next extension item.
                     IF ( .NOT. GOTFV( J ) ) THEN
                        IF ( .NOT. QUEST ) THEN
                           IF( STATUS .EQ. SAI__OK ) THEN
                              STATUS = SAI__ERROR
                              CALL MSG_SETC( 'KEYWRD', KEYWRD )
                              CALL NDF_MSG( 'NDF', NDF )
                              CALL ERR_REP( 'POL1_IMFIT_MISS',
     :           '  Cannot locate the FITS keyword: ^KEYWRD, in the'//
     :           ' FITS extension of NDF: ^NDF' , STATUS )
                           END IF

                           GO TO 99

                        ELSE
                           GO TO 2

                        END IF
                     END IF

*  Now need its value. This could be store in any type so need to translate.
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
     :                                STATUS )
                        NCHAR = CHR_LEN( FITVAL( :80 ) )
                     END IF

*  If necessary test this against the trailing description. Have
*  several possibilities for the form here;
*     string1=string2
*     string1 =string2
*     string1= string2
*     string1 = string2
*   This should cope with all these cases.
                     IF ( .NOT. NOMORE ) THEN
                        NOTTRN = .FALSE.
                        IAT = INDEX( EXTFUN(LAST+1: ), FITVAL(:NCHAR ) )
                        IF ( IAT .NE. 0 ) THEN            
                                                          
*  Have found a match for this string in the trailing description, look
*  for '=', which should be next character or next word.  
                           IAT = LAST + IAT + NCHAR       
                           IF ( EXTFUN( IAT: IAT ) .NE. '=' ) THEN
                              CALL CCD1_NXWRD( EXTFUN, IAT, FIRST,
     :                                         LAST, NOTFND, STATUS )
                              IF ( NOTFND ) THEN          
                                                          
*  Probably have a misleading syntax, just use the default value.
                                 NOTTRN = .TRUE.          
                              ELSE                        
                                                          
*  This should be an equals sign                          
                                 IF ( EXTFUN( FIRST : FIRST ) .EQ. '=' )
     :                           THEN                     
                                                              
*  Next word is the actual value.                             
                                    CALL CCD1_NXWRD( EXTFUN, FIRST + 1,
     :                                               FIRST, LAST,
     :                                               NOTFND, STATUS )
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
                              CALL CCD1_NXWRD( EXTFUN, IAT + 1, FIRST,
     :                                         LAST, NOTFND, STATUS )
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
                           CALL MSG_OUT( ' ', '  Warning - failed to'//
     :' locate a translation for FITS-keyword ^KEYWORD, using raw'//
     :' value (^FITVAL)',  STATUS )
                        END IF
                     END IF

*  Skip any remaining loops.
                     GO TO 4
                  END IF
 3             CONTINUE
 4             CONTINUE
               IF ( .NOT. FOUND ) THEN

*  Failed to locate a value for this keyword. Issue warning.
                  CALL MSG_SETC( 'KEYWORD', EXTFUN )
                  CALL MSG_OUT( ' ', '  Warning - failed to obtain'//
     :            ' a value for keyword ^KEYWORD', STATUS )
               ELSE

*  Put the value into the extension.
                  LENFIT = LAST - FIRST + 1
                  CALL POL1_STOCC( NDF, EXTNAM, FITVAL( : LENFIT ),
     :                             STATUS )

*  Inform user of result, unless the extension item is defered.
                  IF( EXTNAM .NE. 'FILTER' ) THEN
                     CALL MSG_SETC( 'NM', EXTNAM )
                     CALL MSG_SETC( 'VL', FITVAL )
                     CALL MSG_OUT( ' ', '     Setting ^NM to ''^VL''', 
     :                             STATUS )
                  END IF
               END IF
            END IF

         ELSE IF ( EXTTYP( 1 : 8 ) .EQ. '_LOGICAL' ) THEN

*  Logicals are special too. Single value allowed must be translatable
*  by the CHR_CTOL routine. First get name of the keyword.
            IF ( NOFUN ) THEN 

*  Must be an empty field. Cannot do anything with this.
               CALL MSG_SETC( 'ITEM', EXTNAM )
               CALL MSG_OUT( ' ', '  Warning - unable to form a '//
     :         'value for _LOGICAL extension item ^ITEM', STATUS )
            ELSE

*  Look for word in function.
            CALL CCD1_NXWRD( EXTFUN ( : LENFUN ), 1, FIRST, LAST,
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
                  CALL MSG_OUT( ' ', '  Warning - extra information'//
     :' following _LOGICAL keyword will be ignored (^EXTRA)', STATUS )
               END IF
            
*  Ok now look for the value associated with this keyword.
               NCHAR = LAST - FIRST + 1
               FOUND = .FALSE.
               DO 5 J = 1, NFITS
                  CALL GRP_GET( FITGRP( 1 ), J, 1, KEYWRD, STATUS )
                  IF ( EXTFUN( 1: NCHAR ) .EQ. KEYWRD( 1: NCHAR ) )
     :            THEN 
                     FOUND = .TRUE.
            
*  Found keyword. If it has no value, report an error unless the
*  extension item terminated in a question mark. Otherwise, jump
*  forward to do the next extension item.
                     IF ( .NOT. GOTFV( J ) ) THEN
                        IF ( .NOT. QUEST ) THEN
                           IF( STATUS .EQ. SAI__OK ) THEN
                              STATUS = SAI__ERROR
                              CALL MSG_SETC( 'KEYWRD', KEYWRD )
                              CALL NDF_MSG( 'NDF', NDF )
                              CALL ERR_REP( 'POL1_IMFIT_MISS',
     :           '  Cannot locate the FITS keyword: ^KEYWRD, in the'//
     :           ' FITS extension of NDF: ^NDF' , STATUS )
                           END IF
                           GO TO 99

                        ELSE
                           GO TO 2

                        END IF
                     END IF

*  Now need its value. This could be store in any type so
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
                        CALL POL1_STOCL( NDF, EXTNAM( :LENNAM ),
     :                                   NOTFND, STATUS )
                        CALL MSG_SETC( 'NM', EXTNAM )
                        CALL MSG_SETL( 'VL', NOTFND )
                        CALL MSG_OUT( ' ', '     Setting ^NM to ^VL', 
     :                                STATUS )
                     ELSE
            
*  No value -- issue warning.
                        CALL MSG_SETC( 'FITVAL', FITVAL )
                        CALL MSG_SETC( 'KEYWRD', KEYWRD )
                        CALL MSG_OUT( ' ', '  Unable to translate '//
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
                  CALL GRP_GET( CHRGRP, CVALS( J ), 1, FITVAL, 
     :                          STATUS )
                  LENFIT = CHR_LEN( FITVAL )
                  FOR = '('//FITVAL( : LENFIT )//')'
                  CALL TRN_STOK( KEYWRD( 1: NCHAR ),
     :                           FOR( : LENFIT + 2 ),
     :                           EXTFUN, NSUBS, STATUS )
                  LENFUN = CHR_LEN( EXTFUN )

               ELSE

*  Must be _LOGICAL, cannot do anything with this. Complain.
                  CALL MSG_SETC( 'EXTNAM', EXTNAM( : LENNAM ) )
                  CALL MSG_OUT( ' ', '  Warning - cannot handle'//
     :' _LOGICAL FITS-keywords in FITS-keyword functions (extension'//
     :' item ^EXTNAM). Not performing translation of this item',
     : STATUS )
                  GO TO 2
               END IF

*  If this FITS value was used in the function, but no value is available,
*  report an error unless the extensionname was terminated with a
*  question mark (in which case jump to the next extension item).
               IF( NSUBS .GT. 0 .AND. .NOT. GOTFV( J ) ) THEN
                  IF( .NOT. QUEST ) THEN
                     IF( STATUS .EQ. SAI__OK ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETC( 'KEYWRD', KEYWRD )
                        CALL NDF_MSG( 'NDF', NDF )
                        CALL ERR_REP( 'POL1_IMFIT_MISS',
     :           '  Cannot locate the FITS keyword: ^KEYWRD, in the'//
     :                       ' FITS extension of NDF: ^NDF' , STATUS )
                     END IF

                     GO TO 99

                  ELSE
                     GO TO 2
                  END IF
               END IF

 7          CONTINUE

*  Ok now need to convert the FITS-keyword function string into one
*  which we can translate using TRANSFORM. Add the dummy variables
*  POLPACK_DUM1 and POLPACK_DUM2 to convert into compilable expressions.
            LENFUN = CHR_LEN( EXTFUN )
            FOR = 'POLPACK_DUM2 ='// EXTFUN( : LENFUN  )//
     :            '+POLPACK_DUM1*0'
            LENFOR = LENFUN + 30
            INV = 'POLPACK_DUM1'

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

               CALL MSG_SETC( 'NM', EXTNAM )
               CALL MSG_SETC( 'ITEM', EXTNAM( :LENNAM ) )
               IF ( EXTTYP( 1 : 8 ) .EQ. '_INTEGER' ) THEN
                  CALL TRN_TR1I( .FALSE., 1, IZERO, TRID, IFIT, STATUS )
                  CALL POL1_STOCI( NDF, EXTNAM (:LENNAM ), IFIT,
     :                             STATUS )
                  CALL MSG_SETI( 'VL', IFIT )
                  CALL MSG_OUT( ' ', '     Setting ^NM to ^VL', STATUS )
               ELSE IF ( EXTTYP( 1 : 5 ) .EQ. '_REAL' ) THEN
                  CALL TRN_TR1R( .FALSE., 1, RZERO, TRID, RFIT, STATUS )
                  CALL POL1_STOCR( NDF, EXTNAM( : LENNAM ), RFIT,
     :                             STATUS )
                  CALL MSG_SETR( 'VL', RFIT )
                  CALL MSG_OUT( ' ', '     Setting ^NM to ^VL', STATUS )
               ELSE
                  CALL TRN_TR1D( .FALSE., 1, DZERO, TRID, DFIT, STATUS )
                  CALL POL1_STOCD( NDF, EXTNAM( : LENNAM ), DFIT,
     :                             STATUS )
                  CALL MSG_SETD( 'VL', DFIT )
                  CALL MSG_OUT( ' ', '     Setting ^NM to ^VL', STATUS )
               END IF

*  Has an error occurred while evaluating the function?
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_ANNUL( STATUS )
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'EXTFUN', EXTFUN( : LENFUN ) )
                  CALL MSG_SETC( 'EXTNAM', EXTNAM( : LENNAM ) )
                  CALL ERR_REP( ' ', '  Cannot interpret:"^EXTFUN"'//
     :', as a valid transform for any known FITS-keywords '//
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
