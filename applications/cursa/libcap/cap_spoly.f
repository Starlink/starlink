      SUBROUTINE CAP_SPOLY (CI, XCOL, YCOL, CIPOLY, XPLCOL, YPLCOL,
     :  INSIDE, TRNFRM, REJCAT, SI, NUMSEL, SIR, NUMREJ, STATUS)
*+
*  Name:
*     CAP_SPOLY
*  Purpose:
*     Generate a selection of points inside or outside a polygon.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_SPOLY (CI, XCOL, YCOL, CIPOLY, XPLCOL, YPLCOL,
*       INSIDE, TRNFRM, REJCAT; SI, NUMSEL, SIR, NUMREJ; STATUS)
*  Description:
*     Generate a selection of points inside or outside a polygon.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Identifier to the input catalogue from which the selection is
*        to be generated.
*     XCOL  =  CHARACTER(*) (Given)
*        Name of the X coordinate column in the input catalogue.
*     YCOL  =  CHARACTER(*) (Given)
*        Name of the X coordinate column in the input catalogue.
*     CIPOLY  =  INTEGER (Given)
*        Identifier to the catalogue of polygon corners (or vertices).
*     XPLCOL  =  CHARACTER(*) (Given)
*        Name of the X coordinate column in the polygon catalogue.
*     YPLCOL  =  CHARACTER(*) (Given)
*        Name of the Y coordinate column in the polygon catalogue.
*     INSIDE  =  LOGICAL (Given)
*        Flag indicating whether points inside or outside the polygon
*        are to be selected, coded as follows:
*        .TRUE.  -  select points inside the polygon,
*        .FALSE. -    "      "    outside "     "   .
*     TRNFRM  =  LOGICAL (Given)
*        Flag indicating whether the coordinates of the polygon corners
*        need to be transformed to the catalogue system prior to making
*        the selection.  This option only applies if the coordinates are
*        celestial coordinates.  It is coded as follows:
*        .TRUE.  -  transform coordinates,
*        .FALSE. -  do not transform coordinates.
*     REJCAT  =  LOGICAL (Given)
*        Flag indicating whether a second optional selection of
*        rejected entries is also to be created.  It is coded as
*        follows:
*        .TRUE.  -  create selection of rejected objects,
*        .FALSE. -  do not create selection of rejected objects,
*     SI  =  INTEGER (Returned)
*        Identifier to list of selected objects.
*     NUMSEL  =  INTEGER (Returned)
*        Number of selected objects.
*     SIR  =  INTEGER (Returned)
*        Identifier to list of rejected objects.  If no list of rejected
*        objects has been specified it is returned set to null
*        (CAT__NOID).
*     NUMREJ  =  INTEGER (Returned)
*        Number of rejected objects.  If no list of rejected objects
*        has been specified it is returned set to zero.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Get identifiers for the input catalogue columns.
*     Get identifiers for the polygon catalogue columns.
*     Determine the number of corners in the polygon catalogue.
*     If the polygon catalogue contains less than three corners then
*       Set the status.
*       Report an errror.
*     end if
*     Map workspace for arrays to contain the polygon corners.
*     Read in the polygon corners.
*     If required then
*       Transform the coordinates of the polygon corners to the
*       catalogue system.
*     end if
*     Determine the number of rows in the input catalogue.
*     Map workspace for the list of selected rows.
*     Generate a list of points inside the polygon.
*     If points outside the polygon are required then
*       map workspace for the list of points outside the polygon.
*       Create the list of points outside the polygon.
*     end if
*     Attempt to create the selection.
*     Release the various workspace.
*     Report any error.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     14/6/96 (ACD): Original version.
*     19/6/96 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CNF_PAR'           ! CNF functions
*  Arguments Given:
      INTEGER
     :  CI,
     :  CIPOLY
      CHARACTER
     :  XCOL*(*),
     :  YCOL*(*),
     :  XPLCOL*(*),
     :  YPLCOL*(*)
      LOGICAL
     :  INSIDE,
     :  TRNFRM,
     :  REJCAT
*  Arguments Returned:
      INTEGER
     :  SI,
     :  NUMSEL,
     :  SIR,
     :  NUMREJ
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  XCOLI,   ! Identifier for X column in input catalogue.
     :  YCOLI,   !     "       "  Y   "    "    "       "    .
     :  XCRNRI,  !     "       "  X   "    "  polygon   "    .
     :  YCRNRI,  !     "       "  Y   "    "     "      "    .
     :  XCRNPT,  ! Pointer to array of X coordinates in polygon catalogue.
     :  YCRNPT,  !    "    "    "   "  Y      "      "     "        "    .
     :  INPTR,   ! Pointer to list of points inside the polygon.
     :  OUTPTR,  !    "    "   "   "    "    outside "     "   .
     :  WRKPTR   ! Pointer to work space.
      INTEGER
     :  ROWS,    ! Number of rows in the input catalogue.
     :  CORNER,  ! Number of corners in the polygon catalogue.
     :  NUMIN,   ! Number of points inside the polygon.
     :  NUMOUT   !   "    "    "    outside "     "   .
      CHARACTER
     :  CRIT*80  ! Description of the selection criterion.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Get identifiers for the input catalogue columns.

         CALL CAT_TIDNT (CI, XCOL, XCOLI, STATUS)
         CALL CAT_TIDNT (CI, YCOL, YCOLI, STATUS)

*
*       Get identifiers for the polygon catalogue columns.

         CALL CAT_TIDNT (CIPOLY, XPLCOL, XCRNRI, STATUS)
         CALL CAT_TIDNT (CIPOLY, YPLCOL, YCRNRI, STATUS)

*
*       Determine the number of corners in the polygon catalogue.

         CALL CAT_TROWS (CIPOLY, CORNER, STATUS)

*
*       If the polygon catalogue contains less than three corners then
*       set the status and report an error (at least three corners are
*       required to define an area).

         IF (CORNER .LT. 3  .AND.  STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR

            CALL MSG_SETI ('CORNER', CORNER)
            CALL ERR_REP ('CAP_SPOLY_POL', 'Failure because polygon '/
     :        /'catalogue contains ^CORNER rows (at least 3 required).',
     :        STATUS)
         END IF

*
*       Map workspace for arrays to contain the polygon corners.

         CALL CAP_CRTAR (CORNER, '_REAL', XCRNPT, STATUS)
         CALL CAP_CRTAR (CORNER, '_REAL', YCRNPT, STATUS)

*
*       Read in the polygon corners.

         CALL CAP_RDPLY (CIPOLY, XCRNRI, YCRNRI, CORNER,
     :     %VAL(CNF_PVAL(XCRNPT)), %VAL(CNF_PVAL(YCRNPT)), STATUS)

*
*       If a coordinate transformation is required then transform the
*       coordinates of the polygon corners to the input catalogue
*       system.

         IF (TRNFRM) THEN
C           CALL CAP_????
            CONTINUE
         END IF

*
*       Determine the number of rows in the input catalogue.

         CALL CAT_TROWS (CI, ROWS, STATUS)

*
*       Map workspace for the list of selected rows.

         CALL CAP_CRTAR (ROWS, '_INTEGER', INPTR, STATUS)

*
*       Generate a list of the points which fall inside the polygon.

         CALL CAP_PLYSL (CI, XCOLI, YCOLI, ROWS, CORNER,
     :     %VAL(CNF_PVAL(XCRNPT)), %VAL(CNF_PVAL(YCRNPT)),
     :     NUMIN, %VAL(CNF_PVAL(INPTR)), STATUS)

*
*       If points outside the polygon are required then map the required
*       workspace and generate the appropriate list of points.

         IF (.NOT. INSIDE) THEN
            CALL CAP_CRTAR (ROWS, '_INTEGER', OUTPTR, STATUS)
            CALL CAP_CRTAR (ROWS, '_INTEGER', WRKPTR, STATUS)

            CALL CAP_RJLST (NUMIN, %VAL(CNF_PVAL(INPTR)), ROWS,
     :        %VAL(CNF_PVAL(WRKPTR)), NUMOUT, %VAL(CNF_PVAL(OUTPTR)),
     :        STATUS)
         END IF

*
*       Attempt to create the selection.

         IF (INSIDE) THEN
            NUMSEL = NUMIN
            CRIT = 'Points inside polygon.'

            CALL CAT_SLIST (NUMSEL, %VAL(CNF_PVAL(INPTR)), CRIT,
     :        REJCAT, CI, SI, SIR, NUMREJ, STATUS)
         ELSE
            NUMSEL = NUMOUT
            CRIT = 'Points outside polygon.'

            CALL CAT_SLIST (NUMSEL, %VAL(CNF_PVAL(OUTPTR)), CRIT,
     :        REJCAT, CI, SI, SIR, NUMREJ, STATUS)
         END IF

*
*       Release the workspace for the arrays of polygon corners and
*       the lists of selected objects.

         CALL CAP_FREAR (XCRNPT, STATUS)
         CALL CAP_FREAR (YCRNPT, STATUS)
         CALL CAP_FREAR (INPTR, STATUS)

         IF (.NOT. INSIDE) THEN
            CALL CAP_FREAR (OUTPTR, STATUS)
            CALL CAP_FREAR (WRKPTR, STATUS)
         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CAP_SPOLY_ERR', 'CAP_SPOLY: '/
     :        /'failed to generate polygonal selection.', STATUS)
         END IF

      END IF

      END
