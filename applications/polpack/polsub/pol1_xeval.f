      SUBROUTINE POL1_XEVAL( ITEM, EXPR, TYPE, XLOC, FCHAN,
     :                       IGRP1, IGRP2, STATUS )
*+
*  Name:
*     POL1_XEVAL

*  Purpose:
*     Evaluate an import control table expression and store the value
*     in the POLPACK extension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_XEVAL( ITEM, EXPR, TYPE, XLOC, FCHAN,
*                      IGRP1, IGRP2, STATUS )

*  Description:
*   This routine evaluates an import control table expression and stores
*   the value in the supplied HDS structure.

*  Arguments:
*     ITEM = CHARACTER * ( * ) (Given)
*        The name of the POLPACK extension item.
*     EXPR = CHARACTER * ( * ) (Given)
*        The expression giving the value to store for the POLPACK
*        extension item.
*     TYPE = CHARACTER * ( * ) (Given)
*        The HDS data type of the POLPACK extension item.
*     XLOC = CHARACTER * ( * ) (Given)
*        The locator for the POLPACK extension.
*     FCHAN = INTEGER (Given)
*        An AST FitsChan containing the FITS header cards to be used when
*        resolving references to FITS keywords contained in EXPR.
*     IGRP1 = INTEGER (Given)
*        A GRP identifier for a group holding HDS data types. Ignored
*        if either IGRP1 or IGRP2 is GRP__NOID.
*     IGRP2 = INTEGER (Given)
*        A GRP identifier for a group holding FITS keyword names. Ignored
*        if either IGRP1 or IGRP2 is GRP__NOID.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - The FITS keyword values supplied in FCHAN are converted to an
*     appropriate data type before being used. The choice of data type
*     is made as follows:
*        o  If the keyword is included in group IGRP2, then the
*        corresponding data type in IGRP1 is used. An error is reported
*        if this data type is incompatible with the use of the keyword
*        within the expression (e.g. if a keyword is declared as _REAL
*        and is then used as an operand for a character operator such
*        as the concatenation operator "//").
*        o  If the keyword is not included in IGRP2, then the keyword
*        will be converted to _CHAR if it is used as an operand of a
*        character operator within the expression.
*        o  If it is not used with a character operator, then it will
*        be used in the form implied by its encoding in the FITS header:
*          -  values enclosed in quotes are treated as _CHAR
*          -  values including a dot treated as _DOUBLE
*          -  values not including a dot are treated as _INTEGER
*     - The resulting value is converted to the data type of the extenson
*     item before being stored.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     23-APR-1999 (DSB):
*        Original version.
*     31-JUL-2009 (TIMJ):
*        QUIET handling is done via MSG_IFGET now.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'GRP_PAR'          ! GRP parameters
      INCLUDE 'TRN_ERR'          ! TRN error parameters

*  Arguments Given:
      CHARACTER ITEM*(*)
      CHARACTER EXPR*(*)
      CHARACTER TYPE*(*)
      CHARACTER XLOC*(*)
      INTEGER FCHAN
      INTEGER IGRP1
      INTEGER IGRP2

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER CVAL*80          ! Buffer for character expression values
      CHARACTER FNAME*10         ! Declared keyword name
      CHARACTER FOR*255          ! Forward transformation function
      CHARACTER FTYPE*(DAT__SZTYP)! Declared data type
      CHARACTER FVALUE*80        ! Keyword value
      CHARACTER INV*1            ! Inverse transformation function
      CHARACTER LOCTR*(DAT__SZLOC)! Locator for temp. Transform structure
      CHARACTER NTYPE*(DAT__SZTYP)! Natural data type
      DOUBLE PRECISION DVAL      ! Double precision item value
      INTEGER I                  ! Loop count
      INTEGER IAT                ! Used length of a string
      INTEGER IVAL               ! Integer item value
      INTEGER LITEM              ! Length of item name
      INTEGER NDEC               ! No. of declarations supplied
      INTEGER NSUBS              ! No. of substitutions made
      INTEGER SIZE1              ! Size of group IGRP1
      INTEGER SIZE2              ! Size of group IGRP2
      INTEGER TRID               ! Identifier for compiled transformation
      LOGICAL LVAL               ! Logical item value
      LOGICAL OK                 ! Was expression evaluated OK?
      LOGICAL OPT                ! Is item optional?
      REAL RVAL                  ! Real item value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the extension item ends with a question mark, set a flag to indicate
*  the item is optional. Also store the index of the last character in the
*  item name.
      CALL CHR_RMBLK( ITEM )
      CALL CHR_UCASE( ITEM )
      LITEM = CHR_LEN( ITEM )
      IF( ITEM( LITEM : LITEM ) .EQ. '?' ) THEN
         OPT = .TRUE.
         LITEM = LITEM - 1
      ELSE
         OPT = .FALSE.
      END IF

*  See if the expression can be evaluated as a character function.
      CALL POL1_CEVAL( EXPR, FCHAN, IGRP1, IGRP2, CVAL, OK, STATUS )

*  If the expression is a character function...
      IF( OK ) THEN

*  If the function was evaluated succesfully...
         IF( STATUS .EQ. SAI__OK ) THEN

*  Convert it to the data type of the POLPACK extension item, and store
*  in the extension.
            IF( TYPE .EQ. '_DOUBLE' ) THEN
               CALL CHR_CTOD( CVAL, DVAL, STATUS )
               OK = ( STATUS .EQ. SAI__OK )
               CALL DAT_NEW0D( XLOC, ITEM( : LITEM ), STATUS )
               CALL CMP_PUT0D( XLOC, ITEM( : LITEM ), DVAL, STATUS )
               CALL MSG_SETD( 'VALUE', DVAL )

            ELSE IF( TYPE .EQ. '_REAL' ) THEN
               CALL CHR_CTOR( CVAL, RVAL, STATUS )
               OK = ( STATUS .EQ. SAI__OK )
               CALL DAT_NEW0R( XLOC, ITEM( : LITEM ), STATUS )
               CALL CMP_PUT0R( XLOC, ITEM( : LITEM ), RVAL, STATUS )
               CALL MSG_SETR( 'VALUE', RVAL )

            ELSE IF( TYPE .EQ. '_INTEGER' ) THEN
               CALL CHR_CTOI( CVAL, IVAL, STATUS )
               OK = ( STATUS .EQ. SAI__OK )
               CALL DAT_NEW0I( XLOC, ITEM( : LITEM ), STATUS )
               CALL CMP_PUT0I( XLOC, ITEM( : LITEM ), IVAL, STATUS )
               CALL MSG_SETI( 'VALUE', IVAL )

            ELSE IF( TYPE .EQ. '_CHAR' ) THEN
               CALL DAT_NEW0C( XLOC, ITEM( : LITEM ), CHR_LEN( CVAL ),
     :                         STATUS )
               CALL CMP_PUT0C( XLOC, ITEM( : LITEM ), CVAL, STATUS )
               IF( ITEM( : LITEM ) .NE. 'FILTER' ) THEN
                  CALL MSG_SETC( 'VALUE', '''' )
                  CALL MSG_SETC( 'VALUE', CVAL )
                  CALL MSG_SETC( 'VALUE', '''' )
               END IF

            ELSE IF( TYPE .EQ. '_LOGICAL' ) THEN
               CALL CHR_CTOL( CVAL, LVAL, STATUS )
               OK = ( STATUS .EQ. SAI__OK )
               CALL DAT_NEW0L( XLOC, ITEM( : LITEM ), STATUS )
               CALL CMP_PUT0L( XLOC, ITEM( : LITEM ), LVAL, STATUS )
               CALL MSG_SETL( 'VALUE', LVAL )

            ELSE IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'TYPE', TYPE )
               CALL ERR_REP( 'POL1_XEVAL_ERR1', 'POL1_XEVAL: '//
     :                       'Unsupported or unknown HDS data '//
     :                       'type ''^TYPE''.', STATUS )
            END IF

*  Add a context message if the value could not be converted.
            IF( .NOT. OK ) THEN
               CALL MSG_SETC( 'TYPE', TYPE )
               CALL MSG_SETC( 'CVAL', CVAL )
               CALL ERR_REP( 'POL1_XEVAL_ERR2', 'Could not convert '//
     :                    '''^CVAL'' to type ^TYPE.', STATUS )
            END IF

*  Tell the user it is being stored in the extension (if requested).
*  The FILTER item is reported later when the final contents of the
*  extension are known (in POL1_CHKEX).
            IF( ITEM( : LITEM ) .NE. 'FILTER' ) THEN
               CALL MSG_SETC( 'ITEM', ITEM( : LITEM ) )
               CALL MSG_OUT( 'POL1_XEVAL_MSG1','     Setting ^ITEM to'//
     :                       ' ^VALUE', STATUS )
            END IF

*  If the character function could not be evaluated because one or more
*  FITS keywords were undefined, then annul the error if the item is
*  optional.
         ELSE IF( STATUS .EQ. SAI__WARN .AND. OPT ) THEN
            CALL ERR_ANNUL( STATUS )
         END IF

*  If the expression did not look like a character expression, try
*  evaluating it as a numerical expression. The expression must be a function
*  involving one or more numerical values. FITS keywords involved in such
*  expressions must be declared before the expression.
      ELSE

*  Annul any error reported when attempting to interpret teh expression
*  as a character function.
         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

*  The forward transformation generates a constant Y value given by the
*  supplied control table expression.
         FOR = 'Y='
         IAT = 6
         CALL CHR_APPND( EXPR, FOR, IAT )

*  Get the number of declared FITS keywords.
         IF( IGRP1 .NE. GRP__NOID .AND. IGRP2 .NE. GRP__NOID ) THEN
            CALL GRP_GRPSZ( IGRP1, SIZE1, STATUS )
            CALL GRP_GRPSZ( IGRP2, SIZE2, STATUS )
            NDEC = MIN( SIZE1, SIZE2 )
         ELSE
            NDEC = 0
         END IF

*  Substitute the numerical value of each declared FITS keyword into the
*  forward transformation expression.
         DO I = 1, NDEC

*  Get the keyword name and its required data type.
            CALL GRP_GET( IGRP1, I, 1, FTYPE, STATUS )
            CALL GRP_GET( IGRP2, I, 1, FNAME, STATUS )

*  Remove spaces and convert to upper case.
            CALL CHR_RMBLK( FNAME )
            CALL CHR_RMBLK( FTYPE )
            CALL CHR_UCASE( FNAME )
            CALL CHR_UCASE( FTYPE )

*  Get the formatted keyword value.
            CALL POL1_GTFIT( FCHAN, FNAME, FVALUE, NTYPE, OK,
     :                       STATUS )
            IF( .NOT. OK ) THEN
               IF( .NOT. OPT .AND. STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'FNAME', FNAME )
                  CALL ERR_REP( 'POL1_XEVAL_ERR3', 'No value found '//
     :                          'for explicitly declared FITS '//
     :                          'keyword ''^FNAME''.', STATUS )
               END IF
               GO TO 999
            END IF

*  Convert the keyword value to the data type specified in the declaration,
*  and perform the substitution.
            IF( FTYPE .EQ. '_DOUBLE' ) THEN
               CALL CHR_CTOD( FVALUE, DVAL, STATUS )
               OK = ( STATUS .EQ. SAI__OK )
               CALL TRN_STOKD( FNAME, DVAL, FOR, NSUBS, STATUS )

            ELSE IF( FTYPE .EQ. '_REAL' ) THEN
               CALL CHR_CTOR( FVALUE, RVAL, STATUS )
               OK = ( STATUS .EQ. SAI__OK )
               CALL TRN_STOKR( FNAME, RVAL, FOR, NSUBS, STATUS )

            ELSE IF( FTYPE .EQ. '_INTEGER' ) THEN
               CALL CHR_CTOI( FVALUE, IVAL, STATUS )
               OK = ( STATUS .EQ. SAI__OK )
               CALL TRN_STOKI( FNAME, IVAL, FOR, NSUBS, STATUS )

            ELSE IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'TYPE', FTYPE )
               CALL MSG_SETC( 'NAME', FNAME )
               CALL ERR_REP( 'POL1_XEVAL_ERR4', 'FITS keyword ^NAME '//
     :                       'declared with non-numeric or '//
     :                       'unsupported HDS data type ''^TYPE''.',
     :                       STATUS )
               GO TO 999
            END IF

* Add a context message if the value could not be converted.
            IF( .NOT. OK ) THEN
               CALL MSG_SETC( 'NAME', FNAME )
               CALL MSG_SETC( 'TYPE', FTYPE )
               CALL MSG_SETC( 'CVAL', FVALUE )
               CALL ERR_REP( 'POL1_XEVAL_ERR5', 'FITS keyword ^NAME '//
     :                       'has value ''^CVAL'' which cannot be '//
     :                       'converted to type ^TYPE.', STATUS )
            END IF

         END DO

*  The inverse transformation is only used to define the un-used input
*  variable (X). */
         INV = 'X'

*  Create a temporary new transformation.
         CALL TRN_NEW( 1, 1, FOR, INV, '_DOUBLE', ' ', ' ', ' ',
     :                 LOCTR, STATUS )

*  Compile the forward transformation.
         CALL TRN_COMP( LOCTR, .TRUE., TRID, STATUS )

*  If the expression referred to any undeclared keywords, annul the error.
         IF( STATUS .EQ. TRN__VARUD ) THEN
            CALL ERR_ANNUL( STATUS )

*  Unless the item is optional, re-report the TRN error with a more
*  friendly message.
            IF( .NOT. OPT ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'POL1_XEVAL_ERR6', 'References to '//
     :                       'undeclared FITS keywords found.',
     :                    STATUS )
            END IF

*  If the compilation was succesfull..
         ELSE IF( STATUS .EQ. SAI__OK ) THEN

*  Transform an arbitrary X value to get the expression value, then store
*  it in the supplied HDS structure.
            IF( TYPE .EQ. '_REAL' ) THEN
               CALL TRN_TR1R( .FALSE., 1, 0.0, TRID, RVAL, STATUS )
               CALL DAT_NEW0R( XLOC, ITEM( : LITEM ), STATUS )
               CALL CMP_PUT0R( XLOC, ITEM( : LITEM ), RVAL, STATUS )
               CALL MSG_SETR( 'VALUE', RVAL )

            ELSE IF( TYPE .EQ. '_DOUBLE' ) THEN
               CALL TRN_TR1D( .FALSE., 1, 0.0D0, TRID, DVAL, STATUS )
               CALL DAT_NEW0D( XLOC, ITEM( : LITEM ), STATUS )
               CALL CMP_PUT0D( XLOC, ITEM( : LITEM ), DVAL, STATUS )
               CALL MSG_SETD( 'VALUE', DVAL )

            ELSE IF( TYPE .EQ. '_INT' ) THEN
               CALL TRN_TR1I( .FALSE., 1, 0, TRID, IVAL, STATUS )
               CALL DAT_NEW0I( XLOC, ITEM( : LITEM ), STATUS )
               CALL CMP_PUT0I( XLOC, ITEM( : LITEM ), IVAL, STATUS )
               CALL MSG_SETI( 'VALUE', IVAL )

            ELSE IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'TYPE', TYPE )
               CALL ERR_REP( 'POL1_XEVAL_ERR7', 'POL1_XEVAL: '//
     :                       'Unsupported or unknown HDS data '//
     :                       'type ''^TYPE''.', STATUS )
            END IF

*  Tell the user it is being stored in the extension (if requested).
            CALL MSG_SETC( 'ITEM', ITEM( : LITEM ) )
            CALL MSG_OUT( 'POL1_XEVAL_MSG2','     Setting ^ITEM to'//
     :           ' ^VALUE', STATUS )

         END IF

*  Annul the Transform structure
         CALL DAT_ANNUL( LOCTR,STATUS )

      END IF

*  Tidy up.
 999  CONTINUE

      END
