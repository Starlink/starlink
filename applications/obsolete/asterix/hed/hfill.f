*+  HFILL - fills an HDS data object with the specified value
      SUBROUTINE HFILL( STATUS )
*
*    Description :
*
*     This application allows primitive data object of any
*     shape and size to be filled with a single specified value
*
*    Parameters :
*
*     INP = UNIV  -  name of data object to be modified
*     VALUE = UNIV - values to be assigned to object
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*
*      ? ??? ?? : V1.0-0  Original (RJV)
*      6 Jan 93 : V1.7-0  Use ARR_ routines to fill arrays (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Status :
      INTEGER STATUS
*    Functions :
      LOGICAL HDX_SAMETYPE,HDX_NUMERIC
*    Local variables :
      CHARACTER*(DAT__SZLOC) VALOC       ! locator to values
      CHARACTER*(DAT__SZLOC) OBJLOC      ! locator to object to be modified
      CHARACTER*(DAT__SZTYP) TYPE        ! type of object
      CHARACTER*80 CHARVAL               ! character value
      DOUBLE PRECISION NUMVAL            ! numeric value
      INTEGER INTVAL                     ! integer values in object
      INTEGER NVAL                       ! number of values in object
      INTEGER OPTR                       ! pointer to mapped object
      INTEGER DIMS(DAT__MXDIM)           ! dimensions of value entered
      INTEGER NDIM                       ! dimensionality of value entered
      LOGICAL PRIM                       ! whether object primitive
      LOGICAL LOGVAL                     ! logical value
      LOGICAL SCALAR                     ! whether value given is scalar
*
*    Version :
*
      CHARACTER*30 VERSION
        PARAMETER (VERSION='HFILL Version 1.8-0')
*-

      CALL MSG_PRNT(VERSION)

      CALL AST_INIT()

*    get locator to object if it exists
      CALL USI_ASSOCI('INP','UPDATE',OBJLOC,PRIM,STATUS)
      IF (PRIM) THEN

*        get object type
        CALL DAT_TYPE(OBJLOC,TYPE,STATUS)

*        get value from console or other data object
        CALL USI_DASSOC('VALUE','READ',VALOC,STATUS)
*        check value is simple scalar
        CALL DAT_SHAPE(VALOC,DAT__MXDIM,DIMS,NDIM,STATUS)
        SCALAR=(NDIM.EQ.0)

*        value given must be scalar and both objects numeric or otherwise
*        of same type
        IF (SCALAR.AND.(HDX_SAMETYPE(OBJLOC,VALOC).OR.
     &        (HDX_NUMERIC(OBJLOC).AND.HDX_NUMERIC(VALOC)))) THEN

*          for character types of any length
          IF (TYPE(:5).EQ.'_CHAR') THEN

*            map object and values and make modification
            CALL DAT_MAPV(OBJLOC,TYPE,'WRITE',OPTR,NVAL,STATUS)
            CALL DAT_GET0C(VALOC,CHARVAL,STATUS)
            CALL ARR_INIT1C( CHARVAL, NVAL, %VAL(OPTR), STATUS )

*          treat all numeric types as double precision
          ELSEIF (HDX_NUMERIC(OBJLOC)) THEN
            IF ((TYPE(:5).EQ.'_REAL'  )  .OR.
     &            (TYPE(:7).EQ.'_DOUBLE')) THEN
               CALL DAT_MAPV(OBJLOC,'_DOUBLE',
     &                     'WRITE',OPTR,NVAL,STATUS)
               CALL DAT_GET0D(VALOC,NUMVAL,STATUS)
               CALL ARR_INIT1D( NUMVAL, NVAL, %VAL(OPTR), STATUS )
            ELSE
               CALL DAT_MAPV(OBJLOC,'_INTEGER',
     &                     'WRITE',OPTR,NVAL,STATUS)
               CALL DAT_GET0I(VALOC,INTVAL,STATUS)
               CALL ARR_INIT1I( INTVAL, NVAL, %VAL(OPTR), STATUS )
            ENDIF
*          logicals
          ELSEIF (TYPE.EQ.'_LOGICAL') THEN
            CALL DAT_MAPV(OBJLOC,'_LOGICAL','WRITE',OPTR,NVAL,STATUS)
            CALL DAT_GET0L(VALOC,LOGVAL,STATUS)
            CALL ARR_INIT1L( LOGVAL, NVAL, %VAL(OPTR), STATUS )
          ENDIF

          CALL DAT_UNMAP(OBJLOC,STATUS)

        ELSE
          STATUS = SAI__ERROR
          CALL ERR_REP(' ', 'Value incompatible with object', STATUS )
        ENDIF

        CALL DAT_ANNUL(VALOC,STATUS)

      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP(' ','Object is not primitive', STATUS )
      ENDIF

      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
