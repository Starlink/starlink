*+  PRS_GETSLICE - Parse a string describing a slice of an array
      SUBROUTINE PRS_GETSLICE( NDIMS, DIMS, RANGESTR, RANGES, STATUS )
*
*    Description :
*
*     Parses a string of the form '(1:10,1:50)' and puts the ranges
*     specified into an integer array
*
*    Method :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER NDIMS                  ! dimensionality of data object
      INTEGER DIMS(NDIMS)            ! actual dimensions of data object
      CHARACTER*(*) RANGESTR         ! character description of range
*    Import-Export :
*    Export :
      INTEGER RANGES(2,DAT__MXDIM)   ! numeric description of ranges
*    Status :
      INTEGER STATUS
*    Local Constants :
      CHARACTER*1 BRA
      PARAMETER (BRA='(')
      CHARACTER*1 KET
      PARAMETER (KET=')')
      CHARACTER*1 COMMA
      PARAMETER (COMMA=',')
      CHARACTER*1 COLON
      PARAMETER (COLON=':')
      CHARACTER*1 SEMICOLON
      PARAMETER (SEMICOLON=';')
      CHARACTER*1 DOLLAR
      PARAMETER (DOLLAR='$')
      CHARACTER*1 ZERO
      PARAMETER (ZERO='0')
      CHARACTER*1 NINE
      PARAMETER (NINE='9')
*    Local variables :
      INTEGER NDIM                   ! number of dimensions (local copy)
      INTEGER IDIM                   ! index to dimensions
      INTEGER I                      ! pointer to character within string
      INTEGER L                      ! pointer to end of string
      INTEGER NCOLON                 ! number of colons in string
      INTEGER IFRST                  ! pointers to start and endpoint
      INTEGER ILAST                  ! of substring currently being scanned
      INTEGER LOHI                   ! lower or higher limit (1 or 2)
      INTEGER IVAL                   ! integer value extracted from string
      INTEGER DUMMY                  ! dummy variable to pass to subroutine
      LOGICAL RFMTOK                 ! whether format OK
*    Internal References :
      INTEGER CHR_LEN
*    Local data :
*-

      IF ( STATUS .EQ. SAI__OK ) THEN

        NDIM=NDIMS

*      Set defaults to full size of object
        DO IDIM = 1, NDIM
          RANGES(1,IDIM) = 1
          RANGES(2,IDIM) = DIMS(IDIM)
        END DO
        DO IDIM = NDIM+1, DAT__MXDIM
          RANGES(1,IDIM) = 1
          RANGES(2,IDIM) = 1
        END DO

        L=CHR_LEN(RANGESTR)

*  remove any brackets and terminate with '$'
        IF (RANGESTR(L:L).EQ.KET) THEN
          RANGESTR(L:L)=' '
          L=L-1
        END IF
        IF (RANGESTR(1:1).EQ.BRA) THEN
          RANGESTR=RANGESTR(2:L-1)
          L=L-1
        END IF
        L=L+1
        RANGESTR(L:L)=DOLLAR

        I=1
        NCOLON=0

*      Check format for each dimension in turn
        IDIM=1

*      Scan to end of string
        RFMTOK=.TRUE.
        DO WHILE (RFMTOK.AND.RANGESTR(I:I).NE.DOLLAR)

*        One colon per dimension
          IF ((RANGESTR(I:I).EQ.COLON)      .OR.
     &        (RANGESTR(I:I).EQ.SEMICOLON)) THEN
            NCOLON=NCOLON+1
            RFMTOK=(NCOLON.LE.NDIMS)

*          comma separating dimensions must have been preceded by
*          correct number of colons
          ELSEIF (RANGESTR(I:I).EQ.COMMA) THEN
            RFMTOK=(NCOLON.EQ.IDIM)
            IDIM=IDIM+1

*          other characters in string must be numeric
          ELSE
            RFMTOK=(RANGESTR(I:I).GE.ZERO.AND.RANGESTR(I:I).LE.NINE)
          END IF
          I=I+1
        END DO
        RFMTOK=(RFMTOK.AND.NCOLON.EQ.NDIMS)
*      if format OK then extract numbers
        IF (RFMTOK) THEN
          IFRST=1
          ILAST=1
          IDIM=1
          LOHI=1

*        scan to end of string
          DO WHILE (RANGESTR(ILAST:ILAST).NE.DOLLAR)
            ILAST=IFRST

*          extract numeric parts between delimiters
            DO WHILE (.NOT.(RANGESTR(ILAST:ILAST).EQ.COLON.OR.
     &                      RANGESTR(ILAST:ILAST).EQ.SEMICOLON.OR.
     &                      RANGESTR(ILAST:ILAST).EQ.COMMA.OR.
     &                      RANGESTR(ILAST:ILAST).EQ.DOLLAR))
              ILAST=ILAST+1
            END DO

*          If no numeric part then use defaults
            IF (ILAST.NE.IFRST) THEN

*            Otherwise convert to integer
              CALL CHR_CTOI(RANGESTR(IFRST:ILAST-1),IVAL,DUMMY)

*            Check it falls within size of object
              IF (IVAL.GE.RANGES(1,IDIM).
     &            AND.IVAL.LE.RANGES(2,IDIM)) THEN
                RANGES(LOHI,IDIM)=IVAL
              END IF
            END IF
            IFRST=ILAST+1

*          Next dimension if finished with this one
            IDIM=IDIM+LOHI-1

*          or other limit for current dimension
*          this piece of trickery toggles between 1 & 2
            LOHI=(.NOT.LOHI.AND.3)
          END DO
        END IF
      END IF

      END
