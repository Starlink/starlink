      SUBROUTINE HFILL( STATUS )
*+
* Name:
*    HFILL

* Purpose:
*    Fill an HDS data object with a specified value.

* Language:
*    Fortran 77

* Type of Module:
*    ADAM A-task

* Usage:
*    % hfill inp value

* ADAM Parameters:
*    INP = UNIV (Read)
*       Name of object. <GLOBAL.HDSOBJ>
*    VALUE = UNIV (Read)
*       The value to be used.

* Description :
*    This application allows primitive data object of any shape and size to
*    be filled with a single specified value.
*
*    Every effort is made to convert the value to the required type,
*    using HDS rules. Error DAT__CONER is reported if this is not possible.
*
*    The defined object may be a slice of a larger array. If the array was
*    previously undefined, it becomes defined - other elements are initialised
*    to zero (or blank for type _CHAR).

* Deficiencies:
*    There is a limit of 80 characters on the size of a character value which
*    may be given. A value larger than this will cause a DAT__TRUNC error.
*    A smaller value too large to fit in the specified object's elements will
*    be silently truncated.

* Examples:
*    % hfill cfile.real 0
*       Puts the value 0.0 into every element of component REAL of
*       container file cfile.sdf.
*
*    % hfill 'cfile.real(2,1:)' 5.5
*       Puts value 5.5 in each element of the specified slice of OBJECT
*       (assuming the slice specification is valid for OBJECT).
*
*    % hfill cfile.real '"a"'
*       Fails as the value 'a' cannot be converted to _REAL.
*
*    % hfill cfile.chars '" "'
*       Writes a blank string into every element of component CHARS
*       of container file cont.sdf

* Authors :
*    RJV: R.J. Vallance (Birmingham University)
*    DJA: D.J. Allan (Birmingham University)
*    AJC: A.J. Chipperfield (Starlink, RAL)
*    TIMJ: Tim Jenness (JAC, Hawaii)

* History :
*    ??-???-???? (RJV):
*       V1.0-0  Original Version
*     6-JAN-1993 (DJA):
*       V1.7-0  Use ARR_ routines to fill arrays
*    24-NOV-1994 (DJA)
*       V1.8-0 Now use USI for user interface
*    17-JAN-1996 V2.0-0 (DJA):
*       Updated use of USI
*     6-SEP-2001 (AJC):
*       V3.0-0 Remove Asterix stuff
*       Improve prologue
*       Get it working for CHAR values
*       Get value before mapping to avoid corrupting files on error
*     18-JUL-2007 (TIMJ):
*       - Add CNF_PVAL for 64-bit
*       - Cancel input parameter on exit to prevent Locator leak warning
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

*    Functions :
      LOGICAL HDX_NUMERIC

*    Local variables :
      CHARACTER*(DAT__SZLOC) VALOC       ! locator to values
      CHARACTER*(DAT__SZLOC) OBJLOC      ! locator to object to be modified
      CHARACTER*(DAT__SZTYP) TYPE        ! type of object

      CHARACTER*80 CHARVAL               ! character value

      DOUBLE PRECISION NUMVAL            ! numeric value

      INTEGER CLEN                       ! char size of object element
      INTEGER INTVAL                     ! integer values in object
      INTEGER NVAL                       ! number of values in object
      INTEGER OPTR                       ! pointer to mapped object
      INTEGER DIMS(DAT__MXDIM)           ! dimensions of value entered
      INTEGER NDIM                       ! dimensionality of value entered
      LOGICAL PRIM                       ! whether object primitive
      LOGICAL LOGVAL                     ! logical value
      LOGICAL TRUNC                      ! whether characters truncated

*    Version :
      CHARACTER*30 VERSION
        PARAMETER (VERSION='HFILL Version 3.0-0')
*.

*    Set MSG environment
      CALL MSG_TUNE( 'ENVIRONMENT', 0, STATUS )

*    Version id
      CALL MSG_OUTIF( MSG__NORM, ' ', VERSION, STATUS )

*    Get locator to object if it exists
      CALL DAT_ASSOC( 'INP', 'UPDATE', OBJLOC, STATUS )
      CALL DAT_PRIM( OBJLOC, PRIM, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( PRIM ) THEN

*          Get object type
            CALL DAT_TYPE(OBJLOC,TYPE,STATUS)

*          Get value from console or other data object
            CALL DAT_ASSOC( 'VALUE', 'READ', VALOC, STATUS )

*          Check value is simple scalar
            CALL DAT_SHAPE(VALOC,DAT__MXDIM,DIMS,NDIM,STATUS)

*          Value given must be scalar
            IF( NDIM.EQ.0 ) THEN

*             Character types of any length
               IF (TYPE(:5).EQ.'_CHAR') THEN
*                Get the character length
                  CALL DAT_CLEN(OBJLOC,CLEN,STATUS)
*                Map object and values and make modification
                  CALL DAT_GET0C(VALOC,CHARVAL,STATUS)
                  CALL DAT_MAPV(OBJLOC,TYPE,'WRITE',OPTR,NVAL,STATUS)
                  CALL ARR_INIT1C(
     &              NVAL, %VAL(CNF_PVAL(OPTR)), CHARVAL, TRUNC, STATUS,
     &              %VAL(CNF_CVAL(CLEN)) )
                  IF( ( STATUS.EQ.SAI__OK ) .AND. TRUNC ) THEN
                     CALL MSG_OUTIF( MSG__NORM, ' ',
     &                ' WARNING: Used length of _CHAR value is '//
     &                'greater than object size - truncation has '//
     &                'occurred',STATUS)
                  ENDIF

*             Treat all numeric types as double precision
               ELSEIF (HDX_NUMERIC(OBJLOC)) THEN

                  IF ((TYPE(:5).EQ.'_REAL'  )  .OR.
     &                (TYPE(:7).EQ.'_DOUBLE')) THEN
                     CALL DAT_GET0D(VALOC,NUMVAL,STATUS)
                     CALL DAT_MAPV(
     &                 OBJLOC,'_DOUBLE', 'WRITE',OPTR,NVAL,STATUS)
                     CALL ARR_INIT1D(
     &                 NUMVAL, NVAL, %VAL(CNF_PVAL(OPTR)), STATUS )

                  ELSE
                     CALL DAT_GET0I(VALOC,INTVAL,STATUS)
                     CALL DAT_MAPV(
     &                 OBJLOC,'_INTEGER', 'WRITE',OPTR,NVAL,STATUS)
                     CALL ARR_INIT1I( INTVAL, NVAL,
     :                                %VAL(CNF_PVAL(OPTR)), STATUS )
                  ENDIF

*             Logicals
               ELSEIF (TYPE.EQ.'_LOGICAL') THEN
                  CALL DAT_GET0L(VALOC,LOGVAL,STATUS)
                  CALL DAT_MAPV(
     &              OBJLOC,'_LOGICAL','WRITE',OPTR,NVAL,STATUS)
                  CALL ARR_INIT1L( LOGVAL, NVAL, %VAL(CNF_PVAL(OPTR)),
     :                             STATUS )
               ENDIF

               CALL DAT_UNMAP(OBJLOC,STATUS)

            ELSE
               STATUS = SAI__ERROR
               CALL ERR_REP(
     &           ' ', 'Value must be scalar', STATUS )
            ENDIF

            CALL DAT_ANNUL(VALOC,STATUS)

         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP(' ','Object is not primitive', STATUS )
         ENDIF

         CALL DAT_ANNUL(OBJLOC,STATUS)

      ENDIF

      CALL DAT_CANCL('INP', STATUS)

      END
