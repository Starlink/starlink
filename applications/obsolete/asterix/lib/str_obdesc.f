*+  STR_OBDESC- writes description of object into string
      SUBROUTINE STR_OBDESC(LOC,STR,STATUS)
*    Description :
*     returns a character string containing formatted information
*     about the object located by LOC - blank if locator invalid
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(DAT__SZLOC) LOC
      CHARACTER*(*) STR
*    Status :
      INTEGER STATUS
*    Local Constants :
      CHARACTER*1 BLANK
      PARAMETER (BLANK=' ')
*    Local variables :
      CHARACTER*80 STRING            ! string buffer
      CHARACTER*(DAT__SZNAM) NAME    ! object name
      CHARACTER*(DAT__SZTYP) TYPE    ! object type
      CHARACTER*30 DSTR
      CHARACTER*20 VSTR
      INTEGER NDIMS,DIMS(DAT__MXDIM) ! dimensionality and dimensions
      INTEGER I                      ! index to pos. in string
      LOGICAL VALID                  ! whether locator valid
      LOGICAL PRIM                   ! whether object primitive
*    Functions :
*    Local data :
*-

*    Check status :
      IF (STATUS.NE.SAI__OK ) RETURN

*  blank out string buffer
      STRING=BLANK

*  check if locator valid
      CALL DAT_VALID(LOC,VALID,STATUS)
      IF (VALID) THEN

*  get basic properties of data object
        CALL DAT_NAME(LOC,NAME,STATUS)
        CALL DAT_TYPE(LOC,TYPE,STATUS)
        CALL DAT_PRIM(LOC,PRIM,STATUS)
        CALL DAT_SHAPE(LOC,DAT__MXDIM,DIMS,NDIMS,STATUS)

*  deal with primitive object
        IF (PRIM) THEN
          STRING=NAME//'is prim  type '//TYPE
          I=DAT__SZNAM+14+DAT__SZTYP

*  for non scalar objects write out dimensions
          IF (NDIMS.GT.0) THEN
            CALL STR_DIMTOC(NDIMS,DIMS,DSTR)
            STRING=STRING(:I)//'dimensions: '//DSTR


*  for scalars give the actual value
          ELSE
            CALL STR_OBVAL(LOC,VSTR,STATUS)
            STRING=STRING(:I)//'value:'//VSTR

          ENDIF

*  deal with structured object
        ELSE
          STRING=NAME//'is struc type '//TYPE
          I=DAT__SZNAM+14+DAT__SZTYP

*  if its an array give the dimensions
          IF (NDIMS.GT.0) THEN
            CALL STR_DIMTOC(NDIMS,DIMS,DSTR)
            STRING=STRING(:I)//'dimensions: '//DSTR

          ENDIF
        ENDIF
      ENDIF
      STR=STRING
      END
