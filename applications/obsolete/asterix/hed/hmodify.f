*+  HMODIFY - modifies the value of HDS data object
      SUBROUTINE HMODIFY( STATUS )
*
*    Description :
*
*     This application allows interactive modification of a
*     specified primitive data object
*
*    Parameters :
*
*     INP    = UNIV  -  name of data object to be modified
*     VALUES = UNIV  -  values to be assigned to object
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*
*     ?? ??? ?? : V1.0-0  Original (RJV)
*      7 Jan 93 : V1.7-0  Move data with MAP_ routines (DJA)
*      6 Aug 93 : V1.7-1  Change to ARR_COP routines. Nothing better to do
*                         after all (DJA)
*     14 Jan 94 : V1.7-2  Use COP1B for character strings as ARR_COP1C
*                         doesn't work this way on UNIX (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      CHARACTER*40 CVALUE
      CHARACTER*(DAT__SZLOC) VALOC       ! locator to values
      CHARACTER*(DAT__SZLOC) OBJLOC      ! locator to object to be modified
      CHARACTER*(DAT__SZTYP) TYPE        ! type of object
      CHARACTER*80 DSTR	                 ! description of item
      INTEGER NDIM,DIMS(DAT__MXDIM)      ! dimensions of object
      INTEGER NVAL                       ! number of values in object
      INTEGER OPTR                       ! pointer to mapped object
      INTEGER VPTR                       ! pointer to mapped values
      LOGICAL PRIM                       ! whether object primitive
*    Internal References :
      LOGICAL HDX_SAMESHAPE,HDX_SAMETYPE,HDX_NUMERIC
*
*    Version :
*
      CHARACTER*30 VERSION
        PARAMETER (VERSION='HMODIFY Version 1.8-0')
*-

*    write out version number to terminal
      CALL MSG_PRNT( VERSION )

*    Start ASTERIX
      CALL AST_INIT()

*    get locator to object if it exists
      CALL USI_DASSOC('INP','UPDATE',OBJLOC,STATUS)
      IF (STATUS.EQ.SAI__OK) THEN

*      check that object is primitive
        CALL DAT_PRIM(OBJLOC,PRIM,STATUS)
        IF (PRIM) THEN
          CALL DAT_SHAPE(OBJLOC,DAT__MXDIM,DIMS,NDIM,STATUS)
*        write out basic attributes of object
          CALL STR_OBDESC(OBJLOC,DSTR,STATUS)
          IF (NDIM.EQ.0) THEN
            DSTR(41:43)='Old'
          ENDIF
          CALL MSG_PRNT( DSTR )

*        get object type
          CALL DAT_TYPE(OBJLOC,TYPE,STATUS)

*        get values from terminal or other data object
          CALL USI_DASSOC('VALUES','READ',VALOC,STATUS)

          IF (STATUS.EQ.SAI__OK) THEN

*          objects must be same shape and both numeric or otherwise
*          of same type
            IF (HDX_SAMESHAPE(OBJLOC,VALOC).AND.
     &         (HDX_SAMETYPE(OBJLOC,VALOC).OR.
     &         (HDX_NUMERIC(OBJLOC).AND.HDX_NUMERIC(VALOC)))) THEN

*            for character types of any length
              IF (TYPE(:5).EQ.'_CHAR') THEN

*              map object and values and make modification
                CALL DAT_MAPV(OBJLOC,TYPE,'WRITE',OPTR,NVAL,STATUS)
                CALL DAT_MAPV(VALOC,TYPE,'READ',VPTR,NVAL,STATUS)
                CALL ARR_COP1B(NVAL,%VAL(VPTR),%VAL(OPTR),STATUS)

*            treat all numeric types as double precision
              ELSEIF (HDX_NUMERIC(OBJLOC)) THEN
                CALL DAT_MAPV(OBJLOC,'_DOUBLE','WRITE',OPTR,NVAL,STATUS)
                CALL DAT_MAPV(VALOC,'_DOUBLE','READ',VPTR,NVAL,STATUS)
                CALL ARR_COP1D(NVAL,%VAL(VPTR),%VAL(OPTR),STATUS)

*            logicals
              ELSEIF (TYPE.EQ.'_LOGICAL') THEN
                CALL DAT_MAPV(OBJLOC,'_LOGICAL','WRITE',OPTR,
     &                                             NVAL,STATUS)
                CALL DAT_MAPV(VALOC,'_LOGICAL','READ',VPTR,NVAL,STATUS)
                CALL ARR_COP1L(NVAL,%VAL(VPTR),%VAL(OPTR),STATUS)
              ENDIF

              CALL DAT_UNMAP(OBJLOC,STATUS)
              CALL DAT_UNMAP(VALOC,STATUS)

*  show new value for scalar items
              IF (NDIM.EQ.0) THEN
                DSTR(:40)=' '
                DSTR(41:43)='New'
                CALL STR_OBVAL(OBJLOC,DSTR(51:),STATUS)
                CALL MSG_PRNT( DSTR )
              ENDIF

*          Scalars
            ELSE IF ( NDIM .EQ. 0 ) THEN

              CALL DAT_GET0C( VALOC, CVALUE, STATUS )
              CALL DAT_PUT0C( OBJLOC, CVALUE, STATUS )

*    show new value for scalar items
              IF (NDIM.EQ.0) THEN
                DSTR(:40)=' '
                DSTR(41:43)='New'
                CALL STR_OBVAL(OBJLOC,DSTR(51:),STATUS)
                CALL MSG_PRNT( DSTR )
              ENDIF

            ELSE
              CALL MSG_PRNT( '!  Values incompatible with object' )
            ENDIF

            CALL DAT_ANNUL(VALOC,STATUS)

          ENDIF

        ELSE
          CALL MSG_PRNT( '!  Object is not primitive' )
        ENDIF

        CALL DAT_ANNUL(OBJLOC,STATUS)
      ENDIF

*    Close ASTERIX
      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
