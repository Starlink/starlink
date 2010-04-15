      SUBROUTINE HCREATE( STATUS )
*+
* Name:
*    HCREATE

* Purpose:
*    Create an HDS data object of specified type and dimensions.

* Language:
*    Fortran 77

* Type of Module:
*    ADAM A-task

* Usage:
*    hcreate inp type [dims]

* ADAM Parameters:
*    INP  = UNIV (Read)
*       Name of object.  GLOBAL.HDSOBJ>
*    TYPE = CHAR (Read)
*       Type of object to be created.
*    DIMS*(*) = INTEGER (Read)
*       Dimensions of object, comma or space separated and enclosed in [ ]
*       if more than one ([ ] optional in response to a prompt). [0]

* Description:
*    Creates an HDS data object of specified type and dimensions.  It
*    will either create a completely new container file or a new object
*    within an existing structure. An existing container file will be
*    overwritten but an existing component within a file will not.
*
*    By default the object created will be a scalar (dimension 0). If
*    you want to create an object of different shape then either
*    supply the dimensions on the command line or force prompting with
*    the PROMPT keyword.
*
*    Primitives are not given values and this action must be
*    performed subsequently by HMODIFY, HFILL or HCOPY.

* Valid Types:
*    HDS divides objects into two classes, primitive and structured.
*    The former contain simple data such as numbers or characters,
*    whereas the latter contain collections of other objects, structured
*    or primitive.
*    The valid primitive types recognised by HDS are:
*
*    !bt3
*    Type         !- Equiv Fortran     !- Range !n
*    !n
*    _LOGICAL     !- LOGICAL*4         !- .TRUE., .FALSE. !n
*    _UBYTE       !- not supported     !- 0..255 !n
*    _BYTE        !- BYTE              !- -128..127 !n
*    _UWORD       !- not supported     !- 0..65535 !n
*    _WORD        !- INTEGER*2         !- -32768..32767 !n
*    _INTEGER     !- INTEGER*4 !n
*    _REAL        !- REAL*4 !n
*    _DOUBLE      !- DOUBLE PRECISION !n
*    _CHAR*n      !- CHARACTER*(n)
*    !et
*
*    Any type not in the above list will be assumed to be a structured
*    type.

* Examples:
*    % hcreate file1 _integer '[10,25]'
*       Creates an HDS container file, file1, containing a 10x25 _INTEGER
*       array. (Note that the square brackets have to be protected from the
*       shell.)
*
*    % hcreate file1 struc
*       Creates an HDS container file, file1, containing a structure of
*       type STRUC.
*
*    % hcreate file1.array _real '[10,25]'
*       Creates a 10x25 array named ARRAY of type _REAL in the HDS
*       structure created in the previous example

* Authors:
*    RJV: R.J. Vallance (BHVAD::RJV)
*    DJA: D.J. Allan (BHVAD::DJA)
*    AJC: A.J.Chipperfield (Starlink, RAL)

* History:
*    ??-???-???? (RJV):
*       Original Version
*    24-NOV-1994 (DJA):
*       V1.8-0 Now use USI for user interface
*    06-SEP-2001 (AJC):
*       V3.0-0 Remove Asterix stuff
*       Improve prologue
*-

*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'MSG_PAR'

*    Status :
      INTEGER STATUS

*    Local variables :
      CHARACTER*(DAT__SZTYP) TYPE    ! type of data object
      INTEGER NDIMS                  ! dimensionalty of object
      INTEGER NVAL                   ! number of values read
      INTEGER DIMS(DAT__MXDIM)      ! dimensions of object

*    Version :
      CHARACTER*30 VERSION
        PARAMETER (VERSION='HCREATE Version 3.0-0')
*.

*    Set MSG environment
      CALL MSG_TUNE( 'ENVIRONMENT', 0, STATUS )

*    Version number
      CALL MSG_OUTIF( MSG__NORM, ' ', VERSION, STATUS )

*    Get type of object to be created
      CALL PAR_GET0C('TYPE',TYPE,STATUS)

*    Get dimensionality
      CALL PAR_GET1I( 'DIMS', DAT__MXDIM, DIMS, NVAL, STATUS )
      IF ( DIMS(1) .EQ. 0 ) THEN
        NDIMS = 0
      ELSE
        NDIMS = NVAL
      ENDIF

*    Now create
      CALL DAT_CREAT( 'INP', TYPE, NDIMS, DIMS, STATUS )

      END
