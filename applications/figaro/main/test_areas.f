      SUBROUTINE TEST_AREAS(NLIST,D1,LIST,X,Y,ID)
*+
* Name:
*   TEST_AREAS

* Purpose:
*   See if X,Y position is within one of a number of defined areas.

* Description:
*   See if X,Y position is within one of a number of defined areas.

* Invocation:
*   CALL TEST_AREAS(NLIST,D1,LIST,X,Y,ID)

* Arguments:
*  NLIST = INTEGER (Given)
*     Number of items to check against
*  D1 = INTEGER (Given)
*     1st dimension of list (>=4)
*  LIST(D1,NLIST) = REAL ARRAY (Given)
*     List of coordinates of areas, XMIN, XMAX, YMIN, YMAX
*  X = REAL (Given)
*     X position in normalised device coordinates
*  Y = REAL (Given)
*     Y position in normalised device coordinates
*  ID = INTEGER (Returned)
*     Integer code giving area (2nd dimension of LIST).

* Authors:
*   TNW: T.N.Wilkins (Durham)
*     {enter_new_authors_here}

* History:
*   24-JAN-1994 TNW:
*      Original version
*   9-FEB-1994 TNW:
*      1st Dimension of LIST made an argument
*     {enter_changes_here}
*-
      IMPLICIT NONE
      INTEGER NLIST,D1
      REAL LIST(D1,NLIST),X,Y
      INTEGER ID
      INTEGER I
      DO I = 1, NLIST
         IF((X.GT.LIST(1,I)).AND.(X.LT.LIST(2,I)).AND.(Y.GT.LIST(3,I))
     :        .AND.(Y.LT.LIST(4,I))) THEN
            ID = I
            RETURN
         ENDIF
      ENDDO
      END
