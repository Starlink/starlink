      SUBROUTINE
     : CHI_XJOIN( INPUT1, INPUT2, OUTPUT, EXPRESS, STATUS )
*+
*  Name:
*     CHI_JOIN

*  Purpose:
*     Create a new catalogue by joining two catalogues.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_XJOIN( INPUT1, INPUT2, OUTPUT, EXPRESS, STATUS )
*
*  Description:
*     Create a new catalogue by joining two catalogues. The effect of the join
*     is as follows. Consider a large catalogue that contains all the fields
*     from the INPUT1 catalogue and all the fields from the INPUT2 catalogue.
*     Into this catalogue put an entry for each combination of entries in
*     catalogues INPUT1 and INPUT2. The resulting catalogue will have N*M
*     entries where N is the number of entries in the INPUT1 catalogue and
*     M the number in the INPUT2 catalogue. Now search this catalogue for
*     those entries that satisfy the given expression.
*
*     Another way of looking at join is to say. Take every entry in turn
*     from catalogue INPUT1. Match this entry against every entry in
*     catalogue INPUT2 and if the EXPRESSion in satisfied combine both entries
*     to write to a new catalogue.

*  Arguments:
*     INPUT1 = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the first join input catalogue.
*     INPUT2 = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the second join input catalogue.
*     OUTPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the new catalogue.
*     EXPESS = CHARACTER * ( CHI__SZEXP ) (Given)
*        Expression to be applied during the join.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Anticipated Errors:
*     CHI__CATNOTFND
*     CHI__IVLDEXP

*  Authors:
*     ARW: Alan R Wood (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-NOV-1991 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHI_PAR'          ! Standard CHI constants
      INCLUDE 'CHIPAR_PAR'          ! Standard CHI constants
      INCLUDE 'CHIPAR1_PAR'          ! Standard CHI constants
      INCLUDE 'CHI_ERR'          ! Standard CHI errors

*  Global Variables:
      INCLUDE 'CHIWRK_CMN'          ! Parser work commons

*  Arguments Given:
      CHARACTER * ( * ) INPUT1
      CHARACTER * ( * ) INPUT2
      CHARACTER * ( * ) OUTPUT
      CHARACTER * ( * ) EXPRESS

*  Status:
      INTEGER STATUS             ! Global status

*  External Variables:
      INTEGER CHR_LEN

*  Local Variables:
      integer i
      CHARACTER * ( 8 ) JUNK
      CHARACTER * ( CHI__SZNAME ) TEMPINPUT1
      CHARACTER * ( CHI__SZNAME ) TEMPINPUT2
      CHARACTER * ( CHI__SZCNAME ) F1NAMES( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCFMT ) F1FORMATS( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCUNIT ) F1UNITS( CHI__NUMCOLS )
      LOGICAL F1NULLS( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCCMT ) F1COMMENTS( CHI__NUMCOLS )
      CHARACTER * ( 1 ) F1TYPES( CHI__NUMCOLS )
      INTEGER NUM1FLDS
      CHARACTER * ( CHI__SZCNAME ) F2NAMES( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCFMT ) F2FORMATS( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCUNIT ) F2UNITS( CHI__NUMCOLS )
      LOGICAL F2NULLS( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCCMT ) F2COMMENTS( CHI__NUMCOLS )
      CHARACTER * ( 1 ) F2TYPES( CHI__NUMCOLS )
      INTEGER NUM2FLDS
      CHARACTER * ( CHI__SZCNAME ) F3NAMES( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCNAME ) F31NAMES( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCFMT ) F3FORMATS( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCUNIT ) F3UNITS( CHI__NUMCOLS )
      LOGICAL F3NULLS( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCCMT ) F3COMMENTS( CHI__NUMCOLS )
      CHARACTER * ( 1 ) F3TYPES( CHI__NUMCOLS )
      INTEGER NUM3FLDS
      INTEGER NUM31FLDS
*
      CHARACTER * ( CHI__SZCNAME ) FTEMPNAMES( CHI__NUMCOLS )
      CHARACTER * ( 1 ) FTEMPTYPES( CHI__NUMCOLS )
*
      INTEGER COUNT
      INTEGER CAT1COUNT
      INTEGER CAT2COUNT
      INTEGER CAT3COUNT

      CHARACTER * ( CHI__SZCVAL ) CHAR1VALS( CHI__NUMCOLS )
      DOUBLE PRECISION DOUB1VALS( CHI__NUMCOLS )
      INTEGER INT1VALS( CHI__NUMCOLS )
      LOGICAL LOG1VALS( CHI__NUMCOLS )
      REAL REAL1VALS( CHI__NUMCOLS )

      CHARACTER * ( CHI__SZCVAL ) CHAR2VALS( CHI__NUMCOLS )
      DOUBLE PRECISION DOUB2VALS( CHI__NUMCOLS )
      INTEGER INT2VALS( CHI__NUMCOLS )
      LOGICAL LOG2VALS( CHI__NUMCOLS )
      REAL REAL2VALS( CHI__NUMCOLS )

      CHARACTER * ( CHI__SZCVAL ) CHAR3VALS( CHI__NUMCOLS )
      DOUBLE PRECISION DOUB3VALS( CHI__NUMCOLS )
      INTEGER INT3VALS( CHI__NUMCOLS )
      LOGICAL LOG3VALS( CHI__NUMCOLS )
      REAL REAL3VALS( CHI__NUMCOLS )
*
      CHARACTER * ( CHI__SZCVAL ) CHARVAL
      DOUBLE PRECISION DOUBVAL
      INTEGER INTVAL
      LOGICAL LOGVAL
      REAL REALVAL
      CHARACTER * ( 1 ) RESTYPE
*
      INTEGER NUM1ENTS
      INTEGER NUM2ENTS
      INTEGER LENFNAME
      INTEGER LENDIFF
      INTEGER LENTOT
      INTEGER LENINPUT

      LOGICAL F1MDATAACC( chi__numcols )
      LOGICAL F1DATAACC( CHI__NUMCOLS )
      LOGICAL F2MDATAACC( CHI__NUMCOLS )
      LOGICAL F2DATAACC( CHI__NUMCOLS )

*.
*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Parse the expression.
*
      CALL CHI_2PAR( INPUT1, INPUT2, EXPRESS, FTEMPNAMES,
     : FTEMPTYPES, STATUS )
*
*    If the criteria parsed without errors create the output catalogue
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   Get all the information about the fields in the input catalogue 1.
*
      CALL CHI_GALLCD(INPUT1, NUM1FLDS,
     :   F1NAMES, F1FORMATS, F1TYPES,
     :   F1UNITS, F1COMMENTS, F1MDATAACC, F1DATAACC, STATUS)
*
*   Get all the information about the fields in the input catalogue 2.
*
      CALL CHI_GALLCD(INPUT2, NUM2FLDS,
     :   F2NAMES, F2FORMATS, F2TYPES,
     :   F2UNITS, F2COMMENTS, F2MDATAACC, F2DATAACC, STATUS)
*
*  Form the fields in the new catalogue.
*
      DO COUNT = 1, NUM1FLDS
        CALL CHI_ACNAME( INPUT1, F1NAMES(COUNT), F3NAMES(COUNT),
     :    STATUS)
        F3FORMATS(COUNT) = F1FORMATS(COUNT)
        F3UNITS(COUNT) = F1UNITS(COUNT)
        F3NULLS(COUNT) = F1NULLS(COUNT)
        F3COMMENTS(COUNT) = F1COMMENTS(COUNT)
        F3TYPES(COUNT) = F1TYPES(COUNT)
      ENDDO
*
*
*
      NUM3FLDS = NUM1FLDS + NUM2FLDS
*
      DO COUNT = 1, NUM2FLDS
        CAT3COUNT = NUM1FLDS + COUNT
        CALL CHI_ACNAME( INPUT2, F2NAMES(COUNT), F3NAMES(CAT3COUNT),
     :    STATUS)
        F3FORMATS(CAT3COUNT) = F2FORMATS(COUNT)
        F3UNITS(CAT3COUNT) = F2UNITS(COUNT)
        F3NULLS(CAT3COUNT) = F2NULLS(COUNT)
        F3COMMENTS(CAT3COUNT) = F2COMMENTS(COUNT)
        F3TYPES(CAT3COUNT) = F2TYPES(COUNT)
      ENDDO
*
*  Create a catalogue with no entries.
*

      CALL CHI_CRECAT(OUTPUT, 100, NUM3FLDS, F3NAMES, F3FORMATS,
     :    F3UNITS, F3COMMENTS, STATUS)
*
*  Now perform the join trying ever entry in catalogue 1 against every entry
*  in catalogue 2.
*
      CALL CHI_GNENTS(INPUT1, NUM1ENTS, STATUS)
      CALL CHI_GNENTS(INPUT2, NUM2ENTS, STATUS)
*
      DO CAT1COUNT = 1, NUM1ENTS
*
*    Get the values from the next entry.
*
         CALL CHI_GDNAC(INPUT1,F31NAMES,NUM31FLDS,CHAR3VALS,
     :    DOUB3VALS,INT3VALS,LOG3VALS,REAL3VALS,F3TYPES,F3NULLS,STATUS )
*        do i = 1, num31flds
*        print *,'i = ',i
*        print *,'ftempnames = ',ftempnames(i)
*        print *,'ftemptypes = ',ftemptypes(i)
*        print *,'charvals = ',char3vals(i)
*        print *,'doubvals = ',doub3vals(i)
*        print *,'intvals = ',int3vals(i)
*        print *,'logrvals = ',log3vals(i)
*        print *,'realvals = ',real3vals(i)
*       enddo
*
*  Match this with every entry in the second catalogue.
*

         DO CAT2COUNT = 1, NUM2ENTS
*
*    Get the values from the next entry in catalogue 2.
*
          CALL CHI_GDNAC(INPUT2,F2NAMES,NUM2FLDS,CHAR2VALS,
     :    DOUB2VALS,INT2VALS,LOG2VALS,REAL2VALS,F2TYPES,F2NULLS,STATUS )
*
*  Copy into the array for evaluation.
*
          DO COUNT = 1, NUM2FLDS
            CAT3COUNT = NUM1FLDS + COUNT
            CHAR3VALS(CAT3COUNT) = CHAR2VALS(COUNT)
            DOUB3VALS(CAT3COUNT) = DOUB2VALS(COUNT)
            INT3VALS(CAT3COUNT) = INT2VALS(COUNT)
            LOG3VALS(CAT3COUNT) = LOG2VALS(COUNT)
            REAL3VALS(CAT3COUNT) = REAL2VALS(COUNT)
            F3NULLS(CAT3COUNT) = F2NULLS(COUNT)
            F3TYPES(CAT3COUNT) = F2TYPES(COUNT)
          ENDDO
*
*   Apply the expression to a logical result
*
          LOGVAL = .FALSE.
*        do i = 1, num3flds
*        print *,'i = ',i
*        print *,'ftempnames = ',ftempnames(i)
*        print *,'ftemptypes = ',ftemptypes(i)
*        print *,'charvals = ',char3vals(i)
*        print *,'doubvals = ',doub3vals(i)
*        print *,'intvals = ',int3vals(i)
*        print *,'logrvals = ',log3vals(i)
*        print *,'realvals = ',real3vals(i)
*       enddo

          CALL CHI_APPLY( CHAR3VALS, DOUB3VALS, INT3VALS, LOG3VALS,
     : REAL3VALS, NUM3FLDS, CHARVAL, DOUBVAL, INTVAL, LOGVAL, REALVAL,
     : RESTYPE, STATUS )

          IF (LOGVAL) THEN
*
*   Write the new entry.
*
            CALL CHI_PUTENT(OUTPUT,F3NAMES,NUM3FLDS,1,CHAR3VALS,
     :        DOUB3VALS, INT3VALS,LOG3VALS,REAL3VALS,F3TYPES,
     :        F3NULLS, STATUS)
          ENDIF
        ENDDO
        CALL CHI_RESET(INPUT2, STATUS)

      ENDDO
      END
