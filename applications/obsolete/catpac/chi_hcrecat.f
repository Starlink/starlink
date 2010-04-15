      SUBROUTINE
     : CHI_HCRECAT( INPUT, ESTNUMENTS,
     :             NUMFLDS, FNAMES, FFORMATS, FUNITS, FCOMMENTS,
     :             STATUS)

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard HDS constants
      INCLUDE 'CHI_PAR'          ! Standard CHI constants
      INCLUDE 'CHIH_PAR'        ! CHI_HDS constants
      INCLUDE 'DAT_ERR'
      INCLUDE 'CMP_ERR'
      INCLUDE 'CHI_ERR'          ! CHI error codes
      INCLUDE 'CHIH_ERR'         ! CHI_HDS error codes

*  Arguments Given:

      CHARACTER*(*)     INPUT
      INTEGER           ESTNUMENTS
      INTEGER           NUMFLDS
      CHARACTER * ( * ) FNAMES(*)
      CHARACTER * ( * ) FFORMATS(*)
      CHARACTER * ( * ) FUNITS(*)
      CHARACTER * ( * ) FCOMMENTS(*)

*  Status:
      INTEGER           CATNO
      INTEGER STATUS             ! Global status

*  Local Variables:

      INTEGER  I, NUMENTSCRE, LINENO, PT, CLENGTH
      CHARACTER*(DAT__SZTYP)  DTYPE
      CHARACTER*(1)           FLDTYPES(   CHI__NUMCOLS)

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL CHI_HGETLOC( INPUT, CHIH__WRITE, CATNO, STATUS)
      IF ( STATUS .NE. SAI__OK) THEN
          STATUS = CHI__CATNOTFND
          GOTO 9999
      ENDIF

* Set the value of NROWS in the file
      CHIH_NUMFLDS(CATNO) = 0

      NUMENTSCRE = MAX( ESTNUMENTS, CHIH__INITCOLSIZE)

      CALL TBL_SETNROWS( CHIH_LOC(1, CATNO), NUMENTSCRE, STATUS)
      CHIH_TOTSIZE( 1, CATNO) = NUMENTSCRE
      DO I = 1, NUMFLDS
           IF ( FFORMATS(I)(1:1) .EQ. 'I') THEN
               DTYPE = '_INTEGER'
               FLDTYPES(I) = 'I'
           ELSEIF ( FFORMATS(I)(1:1) .EQ. 'A') THEN
               DTYPE = '_CHAR*'//FFORMATS(I)(2:)
               FLDTYPES(I) = 'C'
           ELSEIF( FFORMATS(I)(1:1) .EQ. 'F' .OR.
     :             FFORMATS(I)(1:1) .EQ. 'E'     ) THEN
               DTYPE = '_REAL'
               FLDTYPES(I) = 'R'
           ELSEIF( FFORMATS(I)(1:1) .EQ. 'D') THEN
               DTYPE = '_DOUBLE'
               FLDTYPES(I) = 'D'
           ELSEIF( FFORMATS(I)(1:1) .EQ. 'L') THEN
               DTYPE = '_LOGICAL'
               FLDTYPES(I) = 'L'
           ELSE
               FNAMES(1)   = FNAMES(I)
               FFORMATS(1) = FFORMATS(I)
               STATUS      = CHI__IVLDFFMT
               CALL ERR_REP(' ', 'Column format not recognised', STATUS)
               GOTO 9999
           ENDIF

          CALL TBL_NEWCOL( CHIH_LOC( 1, CATNO), FNAMES(I), FUNITS(I),
     +                      FFORMATS(I), FCOMMENTS(I),
     +                      DTYPE, STATUS )

          CHIH_NUMFLDS(CATNO) = CHIH_NUMFLDS(CATNO) + 1
          CHIH_FNAME(  CHIH_NUMFLDS(CATNO), CATNO) = FNAMES(I)
* Record the size of the character field
          IF ( FFORMATS(I)(1:1) .EQ. 'A') THEN
              READ( FFORMATS(I)(2:), '(I)') CLENGTH
              CHIH_CHARSIZE(CHIH_NUMFLDS(CATNO), CATNO) = CLENGTH
          ELSE
              CHIH_CHARSIZE(CHIH_NUMFLDS(CATNO), CATNO) = 0
          ENDIF
          CHIH_COLMAPD(CHIH_NUMFLDS(CATNO), CATNO) = .FALSE.
          CALL TBL_GETNAM( CHIH_LOC( 1, CATNO), FNAMES(I),
     :      CHIH_COLLOC( CHIH_NUMFLDS(CATNO), CATNO), STATUS)

* Check for errors

          IF ( STATUS .NE. SAI__OK) THEN
              STATUS = CHIH__ERRCRCOL
              CALL ERR_REP(' ', 'Error creating column', STATUS)
              GOTO 9999
          ENDIF

      ENDDO

* Now close the catalogue

      CALL CHI_HCLOCAT( CATNO, STATUS)

9999  RETURN

      END
