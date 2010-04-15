       SUBROUTINE TBL_NEWCOL( TBDSCR, CNAME, CUNITS,
     +                         CFMT, CCOMMENT, DTYPE, STATUS )
*+
*  Name:
*     TBL_NEWCOL


*  Purpose:
*     Define a new column.


*  Language:
*     Starlink Fortran 77


*  Invocation:
*     CALL TBL_NEWCOL( TBDSCR, CNAME, CUNITS, CFMT, CCOMMENT,
*                      DTYPE, STATUS )


*  Description:
*     This routine is used to create the actual
*     column structure.After defining the column
*     values can then be put into it.


*  Arguments:
*     TBDSCR = CHARACTER (Given)
*        Similar to a pointer ,Only this is
*        a character which identifies the table
*        i.e. used to immediately locate the
*        position of the table.
*     CNAME = CHARACTER (Given)
*        Column name.Used to identify a column.
*     CUNITS = CHARACTER (Given)
*        Units the numbers are in. In the given
*        column.
*     CFMT = CHARACTER (Given)
*        The format the given column is in.
*     DTYPE = CHARACTER (Given)
*        The data type for the column.
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*  Notes:
*     -  This routine will return without action
*        if on entry the STATUS is not SAI__OK.
*     -  The CNAME,CUNITS,CFMT  all
*        hold information
*        relating to the data column.

*  Algorithm:
*     -  Check STATUS , exit routine if not SAI__OK
*     -  Create new column.
*     -  Report an error if status not SAI__OK.
*     -  Get the number of
*        rows that need to be created.
*     -  Report an error if status not SAI__OK.
*     -  Get locator for the
*        newly defined column.
*     -  Report an error if status not SAI__OK.
*     -  Create CUNITS,CFMT,NROWSW for
*        the new column.
*     -  Report an error if status not SAI__OK.


*  Authors:
*     Z: Z. Iqbal (SA3500)
*     {enter_new_authors_here}


*  History:
*     {enter_further_changes_here}


*  Bugs:
*     {note_any_bugs_here}


*-


*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
       INCLUDE 'DAT_ERR'
      INCLUDE 'CMP_ERR'
      INCLUDE 'TBL_PAR'


*  Arguments Given and Returned:
      CHARACTER*(*) TBDSCR
      CHARACTER*(*) CNAME, CUNITS, DTYPE
      CHARACTER*(*) CFMT
      CHARACTER*(*) CCOMMENT

*  Status:
      INTEGER STATUS


*  Local Variables:
      CHARACTER*(DAT__SZLOC) CLOC, COLIDN, LOC, SLOC
      INTEGER   NROWS
      LOGICAL   REPLY
      INTEGER   DIMARR(1)
      INTEGER   PTR, CLENGTH
*.

        IF (STATUS .NE. SAI__OK) RETURN

* Check that we have the number of entries in column

        CALL CMP_GET0I(TBDSCR,'NROWS',NROWS,STATUS)
        IF (STATUS .NE. SAI__OK) THEN
            STATUS = TBL__NOTTABLE
            GOTO 9999
        ENDIF

* Get COLUMNS structure

        CALL DAT_FIND( TBDSCR, 'COLUMNS', CLOC, STATUS)
        IF ( STATUS .EQ. DAT__OBJNF) THEN
             STATUS = TBL__NOTTABLE
             GOTO 9999
        ELSEIF ( STATUS .NE. SAI__OK) THEN
             GOTO 9999
        ENDIF

* Get into structure and create column structure

        CALL DAT_NEW(CLOC, CNAME, 'COLUMN', 0, 0, STATUS)

        CALL DAT_FIND(CLOC, CNAME, COLIDN, STATUS)

        CALL DAT_NEW(COLIDN,'DATA',DTYPE,1,NROWS,STATUS)

* Write a value into the array - otherwise HDS would not like an immediate close

        CALL DAT_FIND( COLIDN, 'DATA', LOC, STATUS)
        CALL DAT_SLICE( LOC, 1, 1, 1, SLOC, STATUS)
        DIMARR(1) = 1
        CALL DAT_MAP( SLOC, DTYPE, 'WRITE', 1, DIMARR, PTR, STATUS)

        IF (    DTYPE(2:2) .EQ. 'I') THEN
            CALL TBL_PUTCVI( 0,        1, 1, %val(PTR))
        ELSEIF( DTYPE(2:2) .EQ. 'R') THEN
            CALL TBL_PUTCVR( 0.0,      1, 1, %val(PTR))
        ELSEIF( DTYPE(2:2) .EQ. 'D') THEN
            CALL TBL_PUTCVD( 0.0D0,    1, 1, %val(PTR))
        ELSEIF( DTYPE(2:2) .EQ. 'L') THEN
            CALL  TBL_PUTCVL( .FALSE., 1, 1, %val(PTR))
        ELSEIF( DTYPE(2:2) .EQ. 'C') THEN
            READ (DTYPE(7:), '(I)') CLENGTH
            CALL TBL_PUTCVC( ' ',      1, 1, %val(PTR), CLENGTH)
        ENDIF

        CALL DAT_UNMAP( SLOC, STATUS)
        CALL DAT_ANNUL( SLOC, STATUS)
        CALL DAT_ANNUL(  LOC, STATUS)

* The same for the NULLFLAGS column

        CALL DAT_NEW(COLIDN,'NULLFLAGS','_LOGICAL',1,NROWS,STATUS)
        CALL DAT_FIND( COLIDN, 'NULLFLAGS', LOC, STATUS)
        CALL DAT_SLICE( LOC, 1, 1, 1, SLOC, STATUS)
        DIMARR(1) = 1
        CALL DAT_MAP( SLOC, DTYPE, 'WRITE', 1, DIMARR, PTR, STATUS)
        CALL  TBL_PUTCVL( .FALSE., 1, 1, %val(PTR))
        CALL DAT_UNMAP( SLOC, STATUS)
        CALL DAT_ANNUL( SLOC, STATUS)
        CALL DAT_ANNUL(  LOC, STATUS)


* Now create the rest of the column structure components

        CALL DAT_NEW0C( COLIDN, 'UNITS', TBL__SZFUNIT, STATUS)
        CALL CMP_PUT0C( COLIDN, 'UNITS',   CUNITS,   STATUS)
        CALL DAT_NEW0C( COLIDN, 'COMMENT', TBL__SZFCOMMENT, STATUS)
        CALL CMP_PUT0C( COLIDN, 'COMMENT', CCOMMENT, STATUS)
        CALL DAT_NEW0C( COLIDN, 'FORMAT', TBL__SZFFORMAT, STATUS)
        CALL CMP_PUT0C( COLIDN, 'FORMAT',  CFMT,     STATUS)

        CALL DAT_ANNUL( COLIDN, STATUS)
        CALL DAT_ANNUL( CLOC, STATUS)

9999  RETURN

      END
