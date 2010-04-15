      SUBROUTINE
     : CHI_XSORT( INPUT, SORTCOLS, SORTDIR, STATUS )
*+
*  Name:
*     CHI_XSORT

*  Purpose:
*     Create a new catalogue that is sorted on given columns.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_XSORT( INPUT, SORTCOLS, SORTDIR, STATUS )
*
*  Description:
*     Create a new catalogue that is sorted on given columns.
*     The order of column names in the SORTCOLS array is significant.
*     SORTCOLS(1) must contain the primary column of the sort.
*     SORTCOLS(2) and SORTCOLS(3) contain the secondary and tertiary columns.
*     Spaces in either the secondary or tertiary position simply indicates
*     that there should be no secondary or tertiary ordering.
*     The direction of the sort for each column is given in the corresponding
*     element of the SORTDIR array. TRUE for ascending.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue to be sorted.
*     OUTPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the new sorted catalogue.
*     SORTCOLS( 3 ) = CHARACTER * ( CHI__SZCNAME ) (Given)
*        Names of the sort columns.
*     SORTDIR( 3 ) = LOGICAL (Given)
*        Direction of sort for each column. (TRUE for ascending).
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Notes:
*     If a column does not exist in the catalogue an error will be reported.

*  Anticipated Errors:
*     CHI__CATNOTFND
*     CHI__COLNOTFND

*  Authors:
*     ARW: Alan R Wood (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     1-JUL-1993 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CHI_PAR'   ! Standard CHI constants
      INCLUDE 'CHI_ERR'   ! Standard CHI errors

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      CHARACTER * ( * ) SORTCOLS( 3 )
      LOGICAL SORTDIR( 3 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:

      CHARACTER * ( CHI__SZNAME ) NAME1
      CHARACTER * ( CHI__SZNAME ) NAME2
      CHARACTER * ( CHI__SZCNAME ) CATCNAMES( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCNAME ) CNAMES1( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCNAME ) CNAMES2( CHI__NUMCOLS )
      CHARACTER * ( 1 ) COLTYPE1( CHI__NUMCOLS )
      CHARACTER * ( 1 ) COLTYPE2( CHI__NUMCOLS )
      CHARACTER * ( 1 ) CATCOLTYPES( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCVAL ) CHARVALS1( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCVAL ) CHARVALS2( CHI__NUMCOLS )
      INTEGER NUMENTS ! Number of entries in the catalogue.
      DOUBLE PRECISION DOUBVALS1( CHI__NUMCOLS)
      DOUBLE PRECISION DOUBVALS2( CHI__NUMCOLS)
      LOGICAL NULLS1( CHI__NUMCOLS )
      LOGICAL NULLS2( CHI__NUMCOLS )
      INTEGER INTVALS1( CHI__NUMCOLS )
      INTEGER INTVALS2( CHI__NUMCOLS )
      LOGICAL LOGVALS1( CHI__NUMCOLS )
      LOGICAL LOGVALS2( CHI__NUMCOLS )
      REAL REALVALS1( CHI__NUMCOLS )
      REAL REALVALS2( CHI__NUMCOLS )
      INTEGER CATNUMCOLS
      INTEGER NUMCOLS1
      INTEGER NUMCOLS2
      CHARACTER * ( CHI__SZCFMT ) CATFFORMATS( CHI__NUMCOLS )
      CHARACTER * ( 1 ) CATFTYPES( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCUNIT ) CATFUNITS( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCCMT ) CATFCOMMENTS( CHI__NUMCOLS )
      LOGICAL CATMDATAACC( CHI__NUMCOLS )
      LOGICAL CATDATAACC( CHI__NUMCOLS )
      INTEGER POSINCAT1
      INTEGER POSINCAT2
      INTEGER POSINCAT3
      CHARACTER * ( 1 ) POSTYPE1
      CHARACTER * ( 1 ) POSTYPE2
      CHARACTER * ( 1 ) POSTYPE3
      INTEGER CCOUNT ! Field counter
      INTEGER ENTCOUNT ! Number of entries in the catalogue
      LOGICAL UNDECIDED !
      LOGICAL ONEACTIVE ! Flag true if entry 1 to be kept
      LOGICAL CLEARPASS ! Flag marking a pass without bubbling
      INTEGER PASSCOUNT ! Count the number of passes
      INTEGER NUMCOL ! The number of sort columns
      LOGICAL ONEISLOW ! Flag indicating that buffer one is low.

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   Get all the information about the fields.
*
      CALL CHI_GALLCD(INPUT, CATNUMCOLS, CATCNAMES, CATFFORMATS,
     :   CATFTYPES, CATFUNITS, CATFCOMMENTS, CATMDATAACC, CATDATAACC,
     :   STATUS)
*
*   Search through the column names in the catalogue looking for the
*   primary, secondary and tertiary columns.
*
      POSINCAT1 = 0
      POSINCAT2 = 0
      POSINCAT3 = 0
      NUMCOL = 3
      IF (SORTCOLS(3)(1:1) .EQ. ' ') NUMCOL = 2
      IF (SORTCOLS(2)(1:1) .EQ. ' ') NUMCOL = 1
      DO CCOUNT = 1, CATNUMCOLS
        IF (NUMCOL .GE. 1) THEN
          IF (SORTCOLS(1) .EQ. CATCNAMES(CCOUNT)) THEN
            POSINCAT1 = CCOUNT
            POSTYPE1 = CATFTYPES(CCOUNT)
          ENDIF
        ENDIF
        IF (NUMCOL .GE. 2) THEN
          IF (SORTCOLS(2) .EQ. CATCNAMES(CCOUNT)) THEN
            POSINCAT2 = CCOUNT
            POSTYPE2 = CATFTYPES(CCOUNT)
          ENDIF
        ENDIF
        IF (NUMCOL .GE. 3) THEN
          IF (SORTCOLS(3) .EQ. CATCNAMES(CCOUNT)) THEN
            POSINCAT3 = CCOUNT
            POSTYPE3 = CATFTYPES(CCOUNT)
          ENDIF
        ENDIF
      ENDDO

*
*   Use a simple bubble sort. On the first pass read from the input and
*   sort to a temporary file.
*
*
*   Get the number of entries in the catalogue.
*   Loop through the input catalogue getting the data for the required
*   columns and creating an entry in the new catalogue.
*
      CALL CHI_GNENTS(INPUT, NUMENTS, STATUS)
*
*   On the first pass read from the input catalogue and write to TEMP1
*
      NAME1 = INPUT
      NAME2 = 'HDSTEMP1'
      CLEARPASS = .FALSE.
      PASSCOUNT = 0
*
      DO WHILE ( .NOT. CLEARPASS )
        PASSCOUNT = PASSCOUNT + 1
        IF (PASSCOUNT .GT.1) THEN
          IF ( MOD(PASSCOUNT,2) .EQ. 0) THEN
            NAME1 = 'HDSTEMP1'
            NAME2 = 'HDSTEMP2'
          ELSE
            NAME1 = 'HDSTEMP2'
            NAME2 = 'HDSTEMP1'
          ENDIF
          IF (PASSCOUNT .GT.2) THEN
            CALL CHI_DELCAT( NAME2, STATUS )
          ENDIF
        ENDIF
*
*   Create an empty catalogue.
*
      CALL CHI_CREDUP( INPUT, 100, NAME2, STATUS)
*
        CLEARPASS = .TRUE.
        CALL CHI_GDNAC( NAME1,CNAMES1,NUMCOLS1,CHARVALS1,
     :   DOUBVALS1, INTVALS1, LOGVALS1, REALVALS1,
     :   COLTYPE1, NULLS1, STATUS )
*
        ONEISLOW = .TRUE.

        CALL CHI_GDNAC( NAME1,CNAMES2,NUMCOLS2,CHARVALS2,
     :   DOUBVALS2, INTVALS2, LOGVALS2, REALVALS2,
     :   COLTYPE2, NULLS2, STATUS )
*

        DO ENTCOUNT = 3, NUMENTS+1
*
*   Compare on the two current entries
*
          UNDECIDED = .TRUE.
          IF (NULLS1(POSINCAT1)) THEN
               IF ( SORTDIR(1) ) THEN
                 ONEACTIVE = .TRUE.
               ELSE
                 ONEACTIVE = .FALSE.
               ENDIF
               UNDECIDED = .FALSE.
          ELSEIF (NULLS2(POSINCAT1)) THEN
               IF ( SORTDIR(1) ) THEN
                 ONEACTIVE = .FALSE.
               ELSE
                 ONEACTIVE = .TRUE.
               ENDIF
               UNDECIDED = .FALSE.
*
          ELSEIF ( POSTYPE1 .EQ. 'R' ) THEN
            IF (REALVALS1(POSINCAT1) .GT. REALVALS2(POSINCAT1)) THEN
*
*   If sort direction is true ie ascending then write the entry 2 and keep
*   the entry 1.
*
               IF ( SORTDIR(1) ) THEN
                 ONEACTIVE = .TRUE.
               ELSE
                 ONEACTIVE = .FALSE.
               ENDIF
               UNDECIDED = .FALSE.

            ELSEIF (REALVALS1(POSINCAT1) .LT. REALVALS2(POSINCAT1)) THEN
*
*   If sort direction is true ie ascending then write the entry 1 and keep
*   the entry 2.
*
               IF ( SORTDIR(1) ) THEN
                 ONEACTIVE = .FALSE.
               ELSE
                 ONEACTIVE = .TRUE.
               ENDIF
               UNDECIDED = .FALSE.
*
*  If we cann't decide on the primary field set the flag to go on to look
*  at the secondary.
*
            ELSE
              UNDECIDED = .TRUE.
            ENDIF
*
          ELSEIF ( POSTYPE1 .EQ. 'I' ) THEN
            IF (INTVALS1(POSINCAT1) .GT. INTVALS2(POSINCAT1)) THEN
*
*   If sort direction is true ie ascending then write the entry 2 and keep
*   the entry 1.
*
               IF ( SORTDIR(1) ) THEN
                 ONEACTIVE = .TRUE.
               ELSE
                 ONEACTIVE = .FALSE.
               ENDIF
               UNDECIDED = .FALSE.

            ELSEIF (INTVALS1(POSINCAT1) .LT. INTVALS2(POSINCAT1)) THEN
*
*   If sort direction is true ie ascending then write the entry 1 and keep
*   the entry 2.
*
               IF ( SORTDIR(1) ) THEN
                 ONEACTIVE = .FALSE.
               ELSE
                 ONEACTIVE = .TRUE.
               ENDIF
               UNDECIDED = .FALSE.
*
*  If we cann't decide on the primary field set the flag to go on to look
*  at the secondary.
*
            ELSE
              UNDECIDED = .TRUE.
            ENDIF
*
          ELSEIF ( POSTYPE1 .EQ. 'C' ) THEN
            IF (CHARVALS1(POSINCAT1) .GT. CHARVALS2(POSINCAT1)) THEN
*
*   If sort direction is true ie ascending then write the entry 2 and keep
*   the entry 1.
*
               IF ( SORTDIR(1) ) THEN
                 ONEACTIVE = .TRUE.
               ELSE
                 ONEACTIVE = .FALSE.
               ENDIF
               UNDECIDED = .FALSE.

            ELSEIF (CHARVALS1(POSINCAT1) .LT. CHARVALS2(POSINCAT1)) THEN
*
*   If sort direction is true ie ascending then write the entry 1 and keep
*   the entry 2.
*
               IF ( SORTDIR(1) ) THEN
                 ONEACTIVE = .FALSE.
               ELSE
                 ONEACTIVE = .TRUE.
               ENDIF
               UNDECIDED = .FALSE.
*
*  If we cann't decide on the primary field set the flag to go on to look
*  at the secondary.
*
            ELSE
              UNDECIDED = .TRUE.
            ENDIF
*
          ELSEIF ( POSTYPE1 .EQ. 'D' ) THEN
            IF (DOUBVALS1(POSINCAT1) .GT. DOUBVALS2(POSINCAT1)) THEN
*
*   If sort direction is true ie ascending then write the entry 2 and keep
*   the entry 1.
*
               IF ( SORTDIR(1) ) THEN
                 ONEACTIVE = .TRUE.
               ELSE
                 ONEACTIVE = .FALSE.
               ENDIF
               UNDECIDED = .FALSE.

            ELSEIF (DOUBVALS1(POSINCAT1) .LT. DOUBVALS2(POSINCAT1)) THEN
*
*   If sort direction is true ie ascending then write the entry 1 and keep
*   the entry 2.
*
               IF ( SORTDIR(1) ) THEN
                 ONEACTIVE = .FALSE.
               ELSE
                 ONEACTIVE = .TRUE.
               ENDIF
               UNDECIDED = .FALSE.
*
*  If we cann't decide on the primary field set the flag to go on to look
*  at the secondary.
*
            ELSE
              UNDECIDED = .TRUE.
            ENDIF
*
          ELSEIF ( POSTYPE1 .EQ. 'L' ) THEN
            IF (LOGVALS1(POSINCAT1) .GT. LOGVALS2(POSINCAT1)) THEN
*
*   If sort direction is true ie ascending then write the entry 2 and keep
*   the entry 1.
*
               IF ( SORTDIR(1) ) THEN
                 ONEACTIVE = .TRUE.
               ELSE
                 ONEACTIVE = .FALSE.
               ENDIF
               UNDECIDED = .FALSE.

            ELSEIF (LOGVALS1(POSINCAT1) .LT. LOGVALS2(POSINCAT1)) THEN
*
*   If sort direction is true ie ascending then write the entry 1 and keep
*   the entry 2.
*
               IF ( SORTDIR(1) ) THEN
                 ONEACTIVE = .FALSE.
               ELSE
                 ONEACTIVE = .TRUE.
               ENDIF
               UNDECIDED = .FALSE.
*
*  If we cann't decide on the primary field set the flag to go on to look
*  at the secondary.
*
            ELSE
               UNDECIDED = .TRUE.
            ENDIF
*
          ENDIF
*
*  If the primary field was insufficient to sort these two entries go on
*  to look at the secondary field.
*

          IF ( UNDECIDED .AND. (NUMCOL.EQ.2)) THEN
          IF (NULLS1(POSINCAT2)) THEN
               IF ( SORTDIR(1) ) THEN
                 ONEACTIVE = .TRUE.
               ELSE
                 ONEACTIVE = .FALSE.
               ENDIF
               UNDECIDED = .FALSE.
          ELSEIF (NULLS2(POSINCAT2)) THEN
               IF ( SORTDIR(1) ) THEN
                 ONEACTIVE = .FALSE.
               ELSE
                 ONEACTIVE = .TRUE.
               ENDIF
               UNDECIDED = .FALSE.
*
          ELSEIF ( POSTYPE2 .EQ. 'R' ) THEN
            IF (REALVALS1(POSINCAT2) .GT. REALVALS2(POSINCAT2)) THEN
*
*   If sort direction is true ie ascending then write the entry 2 and keep
*   the entry 1.
*
               IF ( SORTDIR(2) ) THEN
                 ONEACTIVE = .TRUE.
               ELSE
                 ONEACTIVE = .FALSE.
               ENDIF
               UNDECIDED = .FALSE.

            ELSEIF (REALVALS1(POSINCAT2) .LT. REALVALS2(POSINCAT2)) THEN
*
*   If sort direction is true ie ascending then write the entry 1 and keep
*   the entry 2.
*
               IF ( SORTDIR(2) ) THEN
                 ONEACTIVE = .FALSE.
               ELSE
                 ONEACTIVE = .TRUE.
               ENDIF
               UNDECIDED = .FALSE.
*
*  If we cann't decide on the primary field set the flag to go on to look
*  at the secondary.
*
            ELSE
              UNDECIDED = .TRUE.
            ENDIF
*
          ELSEIF ( POSTYPE2 .EQ. 'I' ) THEN
            IF (INTVALS1(POSINCAT2) .GT. INTVALS2(POSINCAT2)) THEN
*
*   If sort direction is true ie ascending then write the entry 2 and keep
*   the entry 1.
*
               IF ( SORTDIR(2) ) THEN
                 ONEACTIVE = .TRUE.
               ELSE
                 ONEACTIVE = .FALSE.
               ENDIF
               UNDECIDED = .FALSE.

            ELSEIF (INTVALS1(POSINCAT2) .LT. INTVALS2(POSINCAT2)) THEN
*
*   If sort direction is true ie ascending then write the entry 1 and keep
*   the entry 2.
*
               IF ( SORTDIR(2) ) THEN
                 ONEACTIVE = .FALSE.
               ELSE
                 ONEACTIVE = .TRUE.
               ENDIF
               UNDECIDED = .FALSE.
*
*  If we cann't decide on the primary field set the flag to go on to look
*  at the secondary.
*
            ELSE
              UNDECIDED = .TRUE.
            ENDIF
*
          ELSEIF ( POSTYPE2 .EQ. 'C' ) THEN
            IF (CHARVALS1(POSINCAT2) .GT. CHARVALS2(POSINCAT2)) THEN
*
*   If sort direction is true ie ascending then write the entry 2 and keep
*   the entry 1.
*
               IF ( SORTDIR(2) ) THEN
                 ONEACTIVE = .TRUE.
               ELSE
                 ONEACTIVE = .FALSE.
               ENDIF
               UNDECIDED = .FALSE.

            ELSEIF (CHARVALS1(POSINCAT2) .LT. CHARVALS2(POSINCAT2)) THEN
*
*   If sort direction is true ie ascending then write the entry 1 and keep
*   the entry 2.
*
               IF ( SORTDIR(2) ) THEN
                 ONEACTIVE = .FALSE.
               ELSE
                 ONEACTIVE = .TRUE.
               ENDIF
               UNDECIDED = .FALSE.
*
*  If we cann't decide on the primary field set the flag to go on to look
*  at the secondary.
*
            ELSE
              UNDECIDED = .TRUE.
            ENDIF
*
          ELSEIF ( POSTYPE2 .EQ. 'D' ) THEN
            IF (DOUBVALS1(POSINCAT2) .GT. DOUBVALS2(POSINCAT2)) THEN
*
*   If sort direction is true ie ascending then write the entry 2 and keep
*   the entry 1.
*
               IF ( SORTDIR(2) ) THEN
                 ONEACTIVE = .TRUE.
               ELSE
                 ONEACTIVE = .FALSE.
               ENDIF
               UNDECIDED = .FALSE.

            ELSEIF (DOUBVALS1(POSINCAT2) .LT. DOUBVALS2(POSINCAT2)) THEN
*
*   If sort direction is true ie ascending then write the entry 1 and keep
*   the entry 2.
*
               IF ( SORTDIR(2) ) THEN
                 ONEACTIVE = .FALSE.
               ELSE
                 ONEACTIVE = .TRUE.
               ENDIF
               UNDECIDED = .FALSE.
*
*  If we cann't decide on the primary field set the flag to go on to look
*  at the secondary.
*
            ELSE
              UNDECIDED = .TRUE.
            ENDIF
*
          ELSEIF ( POSTYPE2 .EQ. 'L' ) THEN
            IF (LOGVALS1(POSINCAT2) .GT. LOGVALS2(POSINCAT2)) THEN
*
*   If sort direction is true ie ascending then write the entry 2 and keep
*   the entry 1.
*
               IF ( SORTDIR(2) ) THEN
                 ONEACTIVE = .TRUE.
               ELSE
                 ONEACTIVE = .FALSE.
               ENDIF
               UNDECIDED = .FALSE.

            ELSEIF (LOGVALS1(POSINCAT2) .LT. LOGVALS2(POSINCAT2)) THEN
*
*   If sort direction is true ie ascending then write the entry 1 and keep
*   the entry 2.
*
               IF ( SORTDIR(2) ) THEN
                 ONEACTIVE = .FALSE.
               ELSE
                 ONEACTIVE = .TRUE.
               ENDIF
               UNDECIDED = .FALSE.
*
*  If we cann't decide on the primary field set the flag to go on to look
*  at the secondary.
*
            ELSE
              UNDECIDED = .TRUE.
            ENDIF
*
          ENDIF
          ENDIF
*
*  If the secondary field was insufficient to sort these two entries go on
*  to look at the tertiary field.
*

        IF ( UNDECIDED .AND. (NUMCOL.EQ.3)) THEN
          IF (NULLS1(POSINCAT3)) THEN
               IF ( SORTDIR(1) ) THEN
                 ONEACTIVE = .TRUE.
               ELSE
                 ONEACTIVE = .FALSE.
               ENDIF
               UNDECIDED = .FALSE.
          ELSEIF (NULLS2(POSINCAT3)) THEN
               IF ( SORTDIR(1) ) THEN
                 ONEACTIVE = .FALSE.
               ELSE
                 ONEACTIVE = .TRUE.
               ENDIF
               UNDECIDED = .FALSE.
*
          ELSEIF ( POSTYPE3 .EQ. 'R' ) THEN
            IF (REALVALS1(POSINCAT3) .GT. REALVALS2(POSINCAT3)) THEN
*
*   If sort direction is true ie ascending then write the entry 2 and keep
*   the entry 1.
*
               IF ( SORTDIR(3) ) THEN
                 ONEACTIVE = .TRUE.
               ELSE
                 ONEACTIVE = .FALSE.
               ENDIF
               UNDECIDED = .FALSE.

            ELSEIF (REALVALS1(POSINCAT3) .LT. REALVALS2(POSINCAT3)) THEN
*
*   If sort direction is true ie ascending then write the entry 1 and keep
*   the entry 2.
*
               IF ( SORTDIR(3) ) THEN
                 ONEACTIVE = .FALSE.
               ELSE
                 ONEACTIVE = .TRUE.
               ENDIF
               UNDECIDED = .FALSE.
*
*  If we cann't decide on the primary field set the flag to go on to look
*  at the secondary.
*
            ELSE
              UNDECIDED = .TRUE.
            ENDIF
*
          ELSEIF ( POSTYPE3 .EQ. 'I' ) THEN
            IF (INTVALS1(POSINCAT3) .GT. INTVALS2(POSINCAT3)) THEN
*
*   If sort direction is true ie ascending then write the entry 2 and keep
*   the entry 1.
*
               IF ( SORTDIR(3) ) THEN
                 ONEACTIVE = .TRUE.
               ELSE
                 ONEACTIVE = .FALSE.
               ENDIF
               UNDECIDED = .FALSE.

            ELSEIF (INTVALS1(POSINCAT3) .LT. INTVALS2(POSINCAT3)) THEN
*
*   If sort direction is true ie ascending then write the entry 1 and keep
*   the entry 2.
*
               IF ( SORTDIR(3) ) THEN
                 ONEACTIVE = .FALSE.
               ELSE
                 ONEACTIVE = .TRUE.
               ENDIF
               UNDECIDED = .FALSE.
*
*  If we cann't decide on the primary field set the flag to go on to look
*  at the secondary.
*
            ELSE
              UNDECIDED = .TRUE.
            ENDIF
*
          ELSEIF ( POSTYPE3 .EQ. 'C' ) THEN
            IF (CHARVALS1(POSINCAT3) .GT. CHARVALS2(POSINCAT3)) THEN
*
*   If sort direction is true ie ascending then write the entry 2 and keep
*   the entry 1.
*
               IF ( SORTDIR(3) ) THEN
                 ONEACTIVE = .TRUE.
               ELSE
                 ONEACTIVE = .FALSE.
               ENDIF
               UNDECIDED = .FALSE.

            ELSEIF (CHARVALS1(POSINCAT3) .LT. CHARVALS2(POSINCAT3)) THEN
*
*   If sort direction is true ie ascending then write the entry 1 and keep
*   the entry 2.
*
               IF ( SORTDIR(3) ) THEN
                 ONEACTIVE = .FALSE.
               ELSE
                 ONEACTIVE = .TRUE.
               ENDIF
               UNDECIDED = .FALSE.
*
*  If we cann't decide on the primary field set the flag to go on to look
*  at the secondary.
*
            ELSE
               UNDECIDED = .TRUE.
            ENDIF
*
          ELSEIF ( POSTYPE3 .EQ. 'D' ) THEN
            IF (DOUBVALS1(POSINCAT3) .GT. DOUBVALS2(POSINCAT3)) THEN
*
*   If sort direction is true ie ascending then write the entry 2 and keep
*   the entry 1.
*
               IF ( SORTDIR(3) ) THEN
                 ONEACTIVE = .TRUE.
               ELSE
                 ONEACTIVE = .FALSE.
               ENDIF
               UNDECIDED = .FALSE.

            ELSEIF (DOUBVALS1(POSINCAT3) .LT. DOUBVALS2(POSINCAT3)) THEN
*
*   If sort direction is true ie ascending then write the entry 1 and keep
*   the entry 2.
*
               IF ( SORTDIR(3) ) THEN
                 ONEACTIVE = .FALSE.
               ELSE
                 ONEACTIVE = .TRUE.
               ENDIF
               UNDECIDED = .FALSE.
*
*  If we cann't decide on the primary field set the flag to go on to look
*  at the secondary.
*
            ELSE
              UNDECIDED = .TRUE.
            ENDIF
*
          ELSEIF ( POSTYPE3 .EQ. 'L' ) THEN
            IF (LOGVALS1(POSINCAT3) .GT. LOGVALS2(POSINCAT3)) THEN
*
*   If sort direction is true ie ascending then write the entry 2 and keep
*   the entry 1.
*
               IF ( SORTDIR(3) ) THEN
                 ONEACTIVE = .TRUE.
               ELSE
                 ONEACTIVE = .FALSE.
               ENDIF
               UNDECIDED = .FALSE.

            ELSEIF (LOGVALS1(POSINCAT3) .LT. LOGVALS2(POSINCAT3)) THEN
*
*   If sort direction is true ie ascending then write the entry 1 and keep
*   the entry 2.
*
               IF ( SORTDIR(3) ) THEN
                 ONEACTIVE = .FALSE.
               ELSE
                 ONEACTIVE = .TRUE.
               ENDIF
               UNDECIDED = .FALSE.
*
*  If we cann't decide on the primary field set the flag to go on to look
*  at the secondary.
*
            ELSE
              UNDECIDED = .TRUE.
            ENDIF
*
          ENDIF
        ENDIF
*
*  Write the non active entry to temporary catalogue and replace it with
*  the next entry in the input catalogue.
*
          IF ( UNDECIDED ) THEN
            CALL CHI_PUTENT(NAME2,CNAMES2,NUMCOLS2,1,CHARVALS2,
     :         DOUBVALS2,INTVALS2,LOGVALS2,REALVALS2,COLTYPE2,
     :         NULLS2,STATUS)
            IF ( ENTCOUNT .LE. NUMENTS ) THEN
              CALL CHI_GDNAC(NAME1,CNAMES2,NUMCOLS2,CHARVALS2,
     :      DOUBVALS2, INTVALS2, LOGVALS2, REALVALS2,
     :      COLTYPE2, NULLS2, STATUS )
            ENDIF
          ELSE
          IF ( ONEACTIVE ) THEN

            CALL CHI_PUTENT(NAME2,CNAMES2,NUMCOLS2,1,CHARVALS2,
     :         DOUBVALS2,INTVALS2,LOGVALS2,REALVALS2,COLTYPE2,
     :         NULLS2,STATUS)
            IF ( ONEISLOW ) THEN
              CLEARPASS = .FALSE.
            ENDIF
            IF ( ENTCOUNT .LE. NUMENTS ) THEN
              CALL CHI_GDNAC(NAME1,CNAMES2,NUMCOLS2,CHARVALS2,
     :      DOUBVALS2, INTVALS2, LOGVALS2, REALVALS2,
     :      COLTYPE2, NULLS2, STATUS )
              ONEISLOW = .TRUE.
            ENDIF
*
          ELSE

            CALL CHI_PUTENT(NAME2,CNAMES1,NUMCOLS1,1,CHARVALS1,
     :         DOUBVALS1,INTVALS1,LOGVALS1,REALVALS1,COLTYPE1,
     :         NULLS1, STATUS)
            IF ( .NOT. ONEISLOW ) THEN
              CLEARPASS = .FALSE.
            ENDIF

            IF ( ENTCOUNT .LE. NUMENTS ) THEN
              CALL CHI_GDNAC(NAME1,CNAMES1,NUMCOLS1,CHARVALS1,
     :      DOUBVALS1, INTVALS1, LOGVALS1, REALVALS1,
     :      COLTYPE1, NULLS1, STATUS )
              ONEISLOW = .FALSE.
            ENDIF
          ENDIF
          ENDIF

        ENDDO
*
*  Write the last entry.
*
        IF ( ONEACTIVE ) THEN
          CALL CHI_PUTENT(NAME2,CNAMES1,NUMCOLS1,1,CHARVALS1,
     :         DOUBVALS1,INTVALS1,LOGVALS1,REALVALS1,COLTYPE1,
     :         NULLS1, STATUS)
*
        ELSE
          CALL CHI_PUTENT(NAME2,CNAMES2,NUMCOLS2,1,CHARVALS2,
     :         DOUBVALS2,INTVALS2,LOGVALS2,REALVALS2,COLTYPE2,
     :         NULLS2, STATUS)
        ENDIF
        CALL CHI_CLOSE( STATUS)
        CALL CHI_OPEN( STATUS)
      ENDDO
*
*  Copy the sorted catalogue to the output.
*
      IF (PASSCOUNT .EQ. 1) THEN
         CALL CHI_DELCAT( NAME2, STATUS )
      ELSE
         CALL CHI_DELCAT( INPUT, STATUS )
         CALL CHI_COPCAT( NAME2, INPUT, STATUS )
         CALL CHI_DELCAT( NAME2, STATUS )
         CALL CHI_DELCAT( NAME1, STATUS )
      ENDIF
        CALL CHI_CLOSE( STATUS)
        CALL CHI_OPEN( STATUS)
      END
