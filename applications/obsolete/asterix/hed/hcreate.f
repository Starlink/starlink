*+  HCREATE - creates HDS data object

      SUBROUTINE HCREATE(STATUS)

*    Description :

*     Creates an HDS data object of specified type and dimensions.  It
*     will either create a completely new object and container file or
*     a new object within an existing structure

*    Parameters :

*     INP  = UNIV    - name of object
*     TYPE = CHAR    -  type of object to be created
*     DIMS = INTEGER - dimensions of object

*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZTYP) TYPE    ! type of data object
      INTEGER NDIMS                  ! dimensionalty of object
      INTEGER NVAL                   ! number of values read
      INTEGER DIMS(DAT__MXDIM)      ! dimensions of object
*
*    Version :
*
      CHARACTER*30 VERSION
      PARAMETER (VERSION='HCREATE Version 1.0-0')
*-

*    write out version number to console
      CALL MSG_PRNT( VERSION )

*    get type of object to be created
      CALL PAR_GET0C('TYPE',TYPE,STATUS)

*    get dimensionality
      CALL PAR_GET1I('DIMS',DAT__MXDIM,DIMS,NVAL,STATUS)
      IF (DIMS(1).EQ.0) THEN
        NDIMS=0
      ELSE
        NDIMS=NVAL
      ENDIF

*    now create
      CALL DAT_CREAT('INP',TYPE,NDIMS,DIMS,STATUS)

      END
