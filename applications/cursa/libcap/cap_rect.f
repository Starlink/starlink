      SUBROUTINE CAP_RECT (CI, XCOL, YCOL, XMIN, XMAX, YMIN, YMAX,
     :  REJCAT, SI, NUMSEL, SIR, NUMREJ, CRIT, STATUS)
*+
*  Name:
*     CAP_RECT
*  Purpose:
*     Select objects inside a rectangle.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_RECT (CI, XCOL, YCOL, XMIN, XMAX, YMIN, YMAX, REJCAT;
*       SI, NUMSEL, SIR, NUMREJ, CRIT; STATUS)
*  Description:
*     Select objects inside a rectangle.
*
*     If the catalogue is sorted on either of the columns comprising
*     the axes of the rectangle then a range selection is used to
*     speed up generating the selection.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Identifier to the catalogue from which the selection is to be
*        created.  This identifier may correspond to either a genuine
*        catalogue, a selection or an index.
*     XCOL  =  CHARACTER(*) (Given)
*        Name of the column defining the X axis of the selection.
*     YCOL  =  CHARACTER(*) (Given)
*        Name of the column defining the Y axis of the selection.
*     XMIN  =  DOUBLE PRECISION (Given)
*        Minimum X value of the rectangle.
*     XMAX  =  DOUBLE PRECISION (Given)
*        Maximum X value of the rectangle.
*     YMIN  =  DOUBLE PRECISION (Given)
*        Minimum Y value of the rectangle.
*     YMAX  =  DOUBLE PRECISION (Given)
*        Maximum Y value of the rectangle.
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
*     CRIT  =  CHARACTER*(*) (Returned)
*        String summarising the selection.  This string is intended
*        to be read by a human, not interpretted by a machine.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to get identifiers for the columns.
*     If ok then
*       Determine if either column is sorted.
*       If there is a sorted column then
*         Generate a range selection on the sorted column.
*       else
*         Adopt the entire catalogue as the basis for the selection.
*       end if
*       If all is ok and the identifier is not null then
*         Assemble the expression corresponding to the required
*         rectangle.
*         Get an identifier for this expression.
*         Select the required objects.
*         If a catalogue of rejected objects is required and all is ok
*         then
*           Assemble the catalogue of rejected objects.
*         else
*           Set the identifier to the list of rejected objects to null.
*         end if
*       else
*         Set the return identifiers to null.
*       end if
*     else
*       Set the return identifiers to null.
*     end if
*     If an error occurred then
*       Report the error.
*     end if
*  Implementation Deficiencies:
*     The current version only works on sorted columns.  When indices
*     are added it should be made to work on indexed columns too.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     20/9/96  (ACD): Original version.
*     22/11/96 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CNF_PAR'           ! CNF functions
*  Arguments Given:
      INTEGER
     :  CI
      CHARACTER
     :  XCOL*(*),
     :  YCOL*(*)
      DOUBLE PRECISION
     :  XMIN,
     :  XMAX,
     :  YMIN,
     :  YMAX
      LOGICAL
     :  REJCAT
*  Arguments Returned:
      INTEGER
     :  SI,
     :  NUMSEL,
     :  SIR,
     :  NUMREJ
      CHARACTER
     :  CRIT*(*)
*  Status:
      INTEGER STATUS              ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  XCOLI,     ! Identifier for X column.
     :  YCOLI,     !     "       "  Y   "   .
     :  SORTI,     !     "       "  for sorted column.
     :  XORDER,    ! Order of the X column.
     :  YORDER,    !   "   "   "  Y   "   .
     :  CBI,       ! Base identifier for the rectangular selection.
     :  NUMBSE,    ! Number of rows in the base selection.
     :  DRJI,      ! Dummy identifier for unused rejects identifier.
     :  DNREJ      ! Dummy number of objects in rejects selection.
      INTEGER
     :  LCRIT,     ! Length of CRIT (excl. trail. blanks).
     :  LXCOL,     !   "    "  XCOL ( "  .   "  .   "   ).
     :  LYCOL,     !   "    "  YCOL ( "  .   "  .   "   ).
     :  EI,        ! Identifier for expression defining the rectangle.
     :  SELPTR     ! Pointer to the list of selected objects.
      LOGICAL
     :  SORT       ! Flag; is there a sorted column.
      DOUBLE PRECISION
     :  SRTMIN,    ! Minimum value of range selection on sorted column.
     :  SRTMAX     ! Maximum   "   "    "       "     "    "      "   .
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Attempt to get identifiers for the columns and proceed if ok.

         CALL CAT_TIDNT (CI, XCOL, XCOLI, STATUS)
         CALL CAT_TIDNT (CI, YCOL, YCOLI, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Determine if either column is sorted.

            CALL CAT_TIQAI (XCOLI, 'ORDER', XORDER, STATUS)
            IF (XORDER .EQ. CAT__ASCND  .OR.  XORDER .EQ. CAT__DSCND)
     :        THEN
               SORT = .TRUE.

               SORTI = XCOLI
               SRTMIN = XMIN
               SRTMAX = XMAX

            ELSE
               CALL CAT_TIQAI (YCOLI, 'ORDER', YORDER, STATUS)
               IF (YORDER .EQ. CAT__ASCND  .OR.  YORDER .EQ. CAT__DSCND)
     :           THEN
                  SORT = .TRUE.

                  SORTI = YCOLI
                  SRTMIN = YMIN
                  SRTMAX = YMAX

               ELSE
                  SORT = .FALSE.

               END IF
            END IF

*
*          If there is a sorted column then generate a range selection on
*          it; oterwise adopt the entire catalogue as the basis for the
*          rectangular selection.

            IF (SORT) THEN
               CALL CAT_SFNDD (CI, SORTI, SRTMIN, SRTMAX, .FALSE.,
     :           CBI, NUMBSE, DRJI, DNREJ, STATUS)
            ELSE
               CBI = CI
            END IF

*
*          Proceed if all is ok and the base identifier for the final
*          rectangular selection is not null.  Note that the latter case
*          can arise if the range selection find no objects.

            IF (STATUS .EQ. SAI__OK  .AND.  CBI .NE. CAT__NOID) THEN

*
*             Assemble the expression corresponding to the required
*             rectangle.

               CRIT = ' '
               LCRIT = 0

               LXCOL = CHR_LEN(XCOL)
               LYCOL = CHR_LEN(YCOL)

               CALL CHR_PUTC (XCOL(1 : LXCOL), CRIT, LCRIT)
               CALL CHR_PUTC (' > ', CRIT, LCRIT)
               CALL CHR_PUTD (XMIN, CRIT, LCRIT)

               CALL CHR_PUTC (' AND ', CRIT, LCRIT)

               CALL CHR_PUTC (XCOL(1 : LXCOL), CRIT, LCRIT)
               CALL CHR_PUTC (' < ', CRIT, LCRIT)
               CALL CHR_PUTD (XMAX, CRIT, LCRIT)

               CALL CHR_PUTC (' AND ', CRIT, LCRIT)

               CALL CHR_PUTC (YCOL(1 : LYCOL), CRIT, LCRIT)
               CALL CHR_PUTC (' > ', CRIT, LCRIT)
               CALL CHR_PUTD (YMIN, CRIT, LCRIT)

               CALL CHR_PUTC (' AND ', CRIT, LCRIT)

               CALL CHR_PUTC (YCOL(1 : LYCOL), CRIT, LCRIT)
               CALL CHR_PUTC (' < ', CRIT, LCRIT)
               CALL CHR_PUTD (YMAX, CRIT, LCRIT)

*
*             Get an identifier for this expression.

               CALL CAT_EIDNT (CI, CRIT(1 : LCRIT), EI, STATUS)

*
*             Select the objects which satisfy this expression.

               CALL CAT_SELCT (CBI, EI, .FALSE., SI, NUMSEL, DRJI,
     :           DNREJ, STATUS)


*
*             If a catalogue of rejected objects is required and all is
*             ok then create it.

               IF (REJCAT  .AND.  SI .NE. CAT__NOID  .AND.
     :             STATUS .EQ. SAI__OK) THEN
                  CALL CAT_TIQAI (SI, 'PTR', SELPTR, STATUS)
                  CALL CAT_SLIST (NUMSEL, %VAL(CNF_PVAL(SELPTR)),
     :              CRIT, .TRUE., CI, DRJI, SIR, NUMREJ, STATUS)
               ELSE
                  SIR = CAT__NOID
                  NUMREJ = 0
               END IF
            ELSE

*
*             No objects satisfied the criteria (or an error occurred);
*             set the return identifiers to null.

               SI = CAT__NOID
               NUMSEL = 0
               SIR = CAT__NOID
               NUMREJ = 0

            END IF
         ELSE
            SI = CAT__NOID
            NUMSEL = 0
            SIR = CAT__NOID
            NUMREJ = 0

         END IF

*
*       Report any error and ensure that the identifiers are set to
*       null.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CAP_RECT_ERR', 'Error selecting objects '/
     :        /'inside a rectangle.', STATUS)

            SI = CAT__NOID
            NUMSEL = 0
            SIR = CAT__NOID
            NUMREJ = 0
         END IF

      END IF

      END
