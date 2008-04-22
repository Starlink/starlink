      SUBROUTINE HMODIFY( STATUS )
*+
* Name:
*    HMODIFY

* Purpose:
*    Modify the value of an HDS object.

* Language:
*    Fortran 77

* Type of Module:
*    ADAM A-task

* Usage:
*    hmodify inp value

* ADAM Parameters :
*    INP=UNIV (Read)
*       Name of object to be modified - must be primitive.
*       <GLOBAL.HDSOBJ>
*    VAL=UNIV (Read)
*       Value to be given to object - this can be an explicit value
*       (including an array) entered at at the terminal or the name of
*       another object. The value must be of the same size and shape as the
*       object to be modified but will be converted to the correct type
*       if possible. Character values must be quoted.

* Description :
*    Allows the value of an HDS primitive data object to be changed.
*    The new value(s) may be entered directly at the terminal or may
*    be taken from another HDS object. Information about the object is
*    displayed.
*
*    Every effort is made to convert the value(s) to the required type,
*    using HDS rules. Error DAT__CONER is reported if this is not possible.
*
*    For _CHAR values, the character length of the value object need not
*    match that of the object to be updated. If the used length of any
*    element of the given value exceeds that of the output object, it will
*    be truncated and a warning message displayed, trailing spaces will be
*    silently truncated.

* Examples:
*    % hmodify spectrum.data.temp 20000
*       Writes the value 20000 into the specified component
*
*    % hmodify 'ds.axis(1).units' '"Counts/s"'
*       Writes the value "Counts/s" into the UNITS component of first element
*       of the AXIS array of structures in container file ds.sdf.
*   
*    % hmodify 'ds.data_array(1:5)' '[1 2 3 4 5]'
*       Writes the given values into the specified slice of component
*       DATA_ARRAY in container file ds.sdf.
*   
*    % hmodify ds.data_array  ds2.data_array
*       Replace the values of the first DATA_ARRAY with those in the second.
*       They must be the same size.

* Authors :
*    RJV: R.J. Vallance (Birmingham University)
*    DJA: D.J. Allan (Birmingham University)
*    AJC: A.J. Chipperfield (Starlink, RAL)
*    TIMJ: Tim Jenness (JAC, Hawaii)

* History :
*    ??-???-19?? (RJV):
*       V1.0-0  Original
*     7-JAN-1993 (DJA):
*       V1.7-0  Move data with MAP_ routines 
*     6-AUG-1993 (DJA):
*       V1.7-1  Change to ARR_COP routines.
*    14-JAN-1994 (DJA):
*       V1.7-2  Use COP1B for character strings as ARR_COP1C
*       doesn't work this way on UNIX 
*    24-NOV-1994 (DJA):
*       V1.8-0 Now use USI for user interface 
*     6-SEP-2001 (AJC):
*       V3.0-0 Remove Asterix stuff 
*       Get it working for _CHAR values
*       Switch order of mapping to minimise chance of corrupting object
*        due to having mapped WRITE.
*       Allow HDS to decide if it can convert different types
*     18-JUL-2007 (TIMJ):
*       Add CNF_PVAL for 64-bit
*-

*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'MSG_PAR'
      INCLUDE 'CNF_PAR'

*    Status :
      INTEGER STATUS

*    Local variables :
      CHARACTER*(DAT__SZLOC) VALOC       ! locator to values
      CHARACTER*(DAT__SZLOC) OBJLOC      ! locator to object to be modified
      CHARACTER*(DAT__SZTYP) TYPE        ! type of object
      CHARACTER*80 DSTR	                 ! description of item
      INTEGER NDIM,DIMS(DAT__MXDIM)      ! dimensions of object
      INTEGER CLENO                      ! character length of object
      INTEGER CLENV                      ! character length of value
      INTEGER NVAL                       ! number of values in object
      INTEGER OPTR                       ! pointer to mapped object
      INTEGER VPTR                       ! pointer to mapped values
      LOGICAL PRIM                       ! whether object primitive
      LOGICAL TRUNC                      ! whether characters truncated

*    External References :
      LOGICAL HDX_SAMESHAPE,HDX_NUMERIC

*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='HMODIFY Version 3.0-0')
*.
*   Get MSG environment
      CALL MSG_TUNE( 'ENVIRONMENT', 0, STATUS )

*   Write out version number to terminal
      CALL MSG_OUTIF( MSG__NORM, ' ', VERSION, STATUS )

*   Get locator to object if it exists
      CALL DAT_ASSOC('INP','UPDATE',OBJLOC,STATUS)
      IF (STATUS.EQ.SAI__OK) THEN

*      Check that object is primitive
         CALL DAT_PRIM(OBJLOC,PRIM,STATUS)
         IF (PRIM) THEN
            CALL DAT_SHAPE(OBJLOC,DAT__MXDIM,DIMS,NDIM,STATUS)
*         Write out basic attributes of object
            CALL STR_OBDESC(OBJLOC,DSTR,STATUS)
            IF (NDIM.EQ.0) THEN
               DSTR(41:43)='Old'
            ENDIF
            CALL MSG_OUTIF( MSG__NORM, ' ', DSTR, STATUS )

*         Get object type
            CALL DAT_TYPE(OBJLOC,TYPE,STATUS)

*         Get values from terminal or other data object
            CALL DAT_ASSOC('VAL','READ',VALOC,STATUS)
            IF (STATUS.EQ.SAI__OK) THEN

*            Objects must be same shape and both numeric or otherwise
*            of same type
               IF (HDX_SAMESHAPE(OBJLOC,VALOC)) THEN
*               For character types of any length
                  IF (TYPE(:5).EQ.'_CHAR') THEN

*                  Get character sizes of object and value
                     CALL DAT_CLEN(OBJLOC,CLENO,STATUS)
                     CALL DAT_CLEN(VALOC,CLENV,STATUS)

*                  Map object and values and make modification
                     CALL DAT_MAPV(
     &                 VALOC,'_CHAR','READ',VPTR,NVAL,STATUS)
                     CALL DAT_MAPV(OBJLOC,TYPE,'WRITE',OPTR,NVAL,STATUS)
                     CALL ARR_COP1C(
     &                 NVAL,%VAL(CNF_PVAL(VPTR)),%VAL(CNF_PVAL(OPTR)),
     &                 TRUNC,STATUS,
     &                 %VAL(CNF_CVAL(CLENV)),%VAL(CNF_CVAL(CLENO)))

*                  Warn if truncation occurred
                     IF ( ( STATUS .EQ. SAI__OK ) .AND. TRUNC ) THEN
                        CALL MSG_BLANK( STATUS )
                        CALL MSG_OUTIF( MSG__NORM, ' ',
     &                  'WARNING: Used length of _CHAR value is '//
     &                  'greater than object size - truncation has '//
     &                  'occurred',STATUS)
                     ENDIF

*               Treat all numeric types as double precision
                  ELSEIF (HDX_NUMERIC(OBJLOC)) THEN
                     CALL DAT_MAPV(
     &                 VALOC,'_DOUBLE','READ',VPTR,NVAL,STATUS)
                     CALL DAT_MAPV(
     &                 OBJLOC,'_DOUBLE','WRITE',OPTR,NVAL,STATUS)
                     CALL ARR_COP1D(NVAL,%VAL(CNF_PVAL(VPTR)),
     &                              %VAL(CNF_PVAL(OPTR)),STATUS)

*               Logicals
                  ELSEIF (TYPE.EQ.'_LOGICAL') THEN
                     CALL DAT_MAPV(
     &                 VALOC,'_LOGICAL','READ',VPTR,NVAL,STATUS)
                     CALL DAT_MAPV(
     &                 OBJLOC,'_LOGICAL','WRITE',OPTR,NVAL,STATUS)
                     CALL ARR_COP1L(NVAL,%VAL(CNF_PVAL(VPTR)),
     &                              %VAL(CNF_PVAL(OPTR)),STATUS)
                  ENDIF

                  CALL DAT_UNMAP(OBJLOC,STATUS)
                  CALL DAT_UNMAP(VALOC,STATUS)

*               Show new value for scalar items
                  IF (NDIM.EQ.0) THEN
                     DSTR(:40)=' '
                     DSTR(41:43)='New'
                     CALL STR_OBVAL(OBJLOC,DSTR(51:),STATUS)
                     CALL MSG_OUTIF( MSG__NORM, ' ', DSTR, STATUS )
                  ENDIF

               ELSE
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ',
     :             'Value and object incompatible shape', STATUS )
               ENDIF

               CALL DAT_ANNUL(VALOC,STATUS)

            ENDIF

         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Object is not primitive', STATUS )
         ENDIF

      ENDIF

      END
