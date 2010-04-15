      SUBROUTINE HRESHAPE(STATUS)
*+
* Name:
*    HRESHAPE

* Purpose:
*    Reshape an HDS object.

* Language:
*    Fortran 77

* Type of Module:
*    ADAM A-task

* Usage:
*    hreshape inp dims

* ADAM Parameters:
*    INP = CHAR  (Read)
*       The name of the object to be re-shaped. <GLOBAL.HDSOBJ>
*    DIMS*(*) = INTEGER (Read)
*       New dimensions of object, comma or space separated and enclosed in [ ]
*       if more than one ([ ] optional in response to a prompt).

* Description:
*    Changes the dimensions of the specified object. The number of dimensions
*    may be decreased but the total number of elements must remain the same
*    unless only the length of the last (or only) dimension is changed. If
*    the size is decreased, elements will be discarded; if it is increased,
*    the value of additional elements is not defined. The operation will fail
*    if the object is a structure array and any truncated elements contain
*    components.

* Examples:
*    Assuming numarr is a 50x100 array of numbers:
*
*    % hreshape numarr 5000
*       Changes  numvarr to a 5000 element vector.
*
*    % hreshape numarr '[50,50]'
*       Produces a 50x50 array of numbers. Elements [:50-100] are discarded.
*
*    % hreshape numarr '[50,100]'
*       After the last example this would restore the original shape of
*       numarr but the previously discarded values may be lost.

* Method:
*    Uses HDS subroutines DAT_ALTER or DAT_MOULD as appropriate.

* Deficiencies:

* Bugs:

* Authors:
*    RJV: R.J. Vallance (Birmingham University)
*    DJA: D.J. Allan (Birmingham University)
*    AJC: A.J. Chipperfield (Starlink, RAL)

* History :
*    ??-???-???? (RJV):
*       V1.0-0  Original
*    24-NOV-1994 (DJA):
*       V1.8-0 Now use USI for user interface
*    14-SEP-2001 (AJC):
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

*    Local Constants :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='HRESHAPE Version 3.0-0')

*    Local variables :
      CHARACTER*(DAT__SZLOC) LOC	! locator to object
      INTEGER NDIM,DIMSO(DAT__MXDIM)	! old dimensions
      INTEGER NVAL			! number of values read
      INTEGER DIMSN(DAT__MXDIM)		! new dimensions
      INTEGER I                         ! counter
      LOGICAL SAME                      ! if dimensions match apart from last
*-

*    Get MSG environment
      CALL MSG_TUNE( 'ENVIRONMENT', 0, STATUS )

*    Print version id
      CALL MSG_OUTIF( MSG__NORM, ' ', VERSION, STATUS )

      CALL DAT_ASSOC('INP','UPDATE',LOC,STATUS)

*  Get existing shape of object
      CALL DAT_SHAPE(LOC,DAT__MXDIM,DIMSO,NDIM,STATUS)

      IF( STATUS.EQ.SAI__OK ) THEN
*  Check for scalar
         IF (NDIM.EQ.0) THEN
            STATUS=SAI__ERROR
            CALL ERR_REP(
     :        ' ','Cannot change shape of scalar object',STATUS)

         ELSE
*  Get new dimensions
            CALL PAR_GET1I('DIMS',DAT__MXDIM,DIMSN,NVAL,STATUS)
            IF (STATUS.EQ.SAI__OK) THEN

*  Check that the number of dimensions is not to increase.
               IF (NVAL.GT.NDIM) THEN
                  STATUS=SAI__ERROR
                  CALL ERR_REP(' ',
     :              'Number of dimensions cannot be increased', STATUS)

               ELSE
*       We may be looking at a size change
*       SAME indicates if only the last (or only) dimension is (or may be)
*       different.
                  IF ( (NVAL.EQ.1) .AND. (NDIM.EQ.1) ) THEN
                     SAME = .TRUE.

                  ELSEIF ( (NVAL.EQ.NDIM) ) THEN
                     SAME = .TRUE.
                     DO I=1,NDIM-1
                        IF (DIMSO(I) .NE. DIMSN(I)) SAME=.FALSE.
                     ENDDO

                  ELSE
                     SAME = .FALSE.
                  ENDIF

*  If OK then do change
!          IF (NDIM.EQ.1) THEN
                  IF( SAME ) THEN
                     CALL DAT_ALTER(LOC,NVAL,DIMSN,STATUS)
                  ELSE
                     CALL DAT_MOULD(LOC,NVAL,DIMSN,STATUS)
                  ENDIF

               ENDIF
            ENDIF
         ENDIF

         CALL DAT_ANNUL(LOC,STATUS)

      ENDIF

      END

