*+  P4_FINDVAL - routine to find the data value at a particular x, y position
      SUBROUTINE P4_FINDVAL (X, Y, AXIS1, AXIS2, DATA, ERRORS, QUALITY,
     :   NDIM, DIM1, DIM2, IF_ERRORS, IF_QUALITY, DATAVAL, DATAERR,
     :   DATAQUAL, ACT_X, ACT_Y, IPOS, JPOS, STATUS)
*    Description :
*    Invocation :
*     CALL P4_FINDVAL (X, Y, AXIS1, AXIS2, DATA, ERRORS, QUALITY, NDIM,
*    :   DIM1, DIM2, IF_ERRORS, IF_QUALITY, DATAVAL, DATAERR, DATAQUAL,
*    :   ACT_X, ACT_Y, IPOS, JPOS, STATUS)
*    Parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*    History :
*     29-Jan-1990: This code would not compile under the V5 compiler.
*                  Modified.               (SMB)
*      3-Aug-1994: Ported to Unix (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      LOGICAL IF_ERRORS                   ! T if error array is valid
      LOGICAL IF_QUALITY                  ! T if quality array is valid
      INTEGER NDIM                        ! the number of dimensions in t'data
      INTEGER DIM1, DIM2                  ! dimension(s) of data
      REAL X, Y                           ! the coords of the desired point
      REAL AXIS1 (DIM1)                ! 1st axis of data
      REAL AXIS2 (DIM2)                ! 2nd axis
      REAL DATA (DIM1,DIM2)         ! the data itself
      REAL ERRORS (DIM1,DIM2)       ! associated errors, if any
      BYTE QUALITY (DIM1,DIM2)      ! and quality
*    Import-Export :
*    Export :
      REAL DATAVAL                        ! the desired data value
      REAL DATAERR                        ! associated error, if any
      INTEGER DATAQUAL                    ! 0 if there was a data value at
*                                         !  x, y
      INTEGER IPOS, JPOS                  ! co-ords of datum in array
      REAL ACT_X, ACT_Y                   ! actual position of x,y point
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER GEN_BSEARCH                 ! search routine in Figaro GEN lib.
*-

      IF (STATUS .NE. SAI__OK) RETURN

*    this subroutine assumes that the data axes are sorted into
*    ascending or descending order

*    axis1 first.
      IPOS = GEN_BSEARCH (AXIS1, DIM1, X)

*    now axis2, if there is one
      IF (NDIM .GT. 1) THEN
         JPOS = GEN_BSEARCH (AXIS2, DIM2, Y)
      ELSE

*       1-dimensional in J
         JPOS = 1

      ENDIF


*    finally, get the data value
*    check if found first
      IF ((IPOS.EQ. 0) .OR. (JPOS.EQ.0)) THEN
*       no, not found
         DATAVAL = 0.0
         DATAQUAL = 1
      ELSE
*       yes, found OK
         DATAVAL = DATA(IPOS,JPOS)
         IF (IF_ERRORS) THEN
            DATAERR = ERRORS(IPOS,JPOS)
         ELSE
            DATAERR = 0.0
         ENDIF
         IF (IF_QUALITY) THEN
            DATAQUAL = QUALITY(IPOS,JPOS)
         ELSE
            DATAQUAL = 0
         ENDIF
         ACT_X = AXIS1 (IPOS)
         IF (NDIM .NE. 1) THEN
            ACT_Y = AXIS2 (JPOS)
         ELSE
            ACT_Y = DATA(IPOS,JPOS)
         ENDIF
      ENDIF

      END
