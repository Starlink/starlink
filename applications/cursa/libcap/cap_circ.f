      SUBROUTINE CAP_CIRC (CI, RACOL, DCCOL, RACEN, DCCEN, RADIUS,
     :  REJCAT, SI, NUMSEL, SIR, NUMREJ, CRIT, STATUS)
*+
*  Name:
*     CAP_CIRC
*  Purpose:
*     Select objects within a given great circle distance of a point.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CIRC (CI, RACOL, DCCOL, RACEN, DCCEN, RADIUS, REJCAT;
*       SI, NUMSEL, SIR, NUMREJ, CRIT; STATUS)
*  Description:
*     Select objects within a given great circle distance of a point.
*
*     If the catalogue is sorted on Declination then a range selection is
*     used to speed up generating the selection.
*
*     The routine is written in terms of Right Ascension and Declination
*     for clarity.  However it will work equally well with any
*     spherical-polar coordinate system.  A range selection will be used
*     for speed if the latitude coordinate is sorted.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Identifier to the catalogue from which the selection is to be
*        created.  This identifier may correspond to either a genuine
*        catalogue, a selection or an index.
*     RACOL  =  CHARACTER(*) (Given)
*        Name of the column holding the Right Ascension (radians).
*     DCCOL  =  CHARACTER(*) (Given)
*        Name of the column holding the Declination (radians).
*     RACEN  =  DOUBLE PRECISION (Given)
*        Right Ascension of the central point for the selection
*        (radians).
*     DCCEN  =  DOUBLE PRECISION (Given)
*        Declination of the central point for the selection (radians).
*     RADIUS  =  DOUBLE PRECISION (Given)
*        Great Circle distance from the central point within which
*        objects are to be selected (radians).
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
*     Note that for efficiency various crude selections are used to
*     reject objects that are far from the given point without computing
*     the great circle distance.  Particularly the bounding box
*     surrounding the required circle is computed and:
*
*     - if the Declination column is sorted then a range selection is
*     made, followed by a simple selection of Right Ascension range,
*
*     - if the Declination column is not sorted then a simple selection
*     on the coordinates of the bounding box is made.
*
*     In both cases the case where the Right Ascension range straddles
*     the 0/24hr boundary is handled.
*
*
*     Attempt to get identifiers for the columns.
*     If ok then
*       Generate the coordinates of the bounding box.
*       If the Declination column is sorted then
*         Generate a range selection on the Declination.
*         If all is ok and the identifier is not null then
*           Assemble the expression corresponding to the Right Ascension
*           range.
*           Get an identifier for this expression.
*           Select the objects which match the expression.
*         end if
*       else
*         Assemble the expression corresponding to the bounding box.
*         Get an identifier for this expression.
*         Select the objects which match the expression.
*       end if
*       If all is ok and the identifier is not null then
*         Assemble the expression corresponding to the Great Circle
*         distance.
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
*     23/9/96 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAP_PAR'           ! CAP symbolic constants.
      INCLUDE 'CNF_PAR'           ! CNF functions
*  Arguments Given:
      INTEGER
     :  CI
      CHARACTER
     :  RACOL*(*),
     :  DCCOL*(*)
      DOUBLE PRECISION
     :  RACEN,
     :  DCCEN,
     :  RADIUS
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
     :  RACOLI,    ! Identifier for Right Ascension column.
     :  DCCOLI,    !     "       "  Declination       "   .
     :  ORDER,     ! Order of the Declination column.
     :  CBI,       ! Base identifier for the rectangular selection.
     :  NUMBSE,    ! Number of rows in the base selection.
     :  DRJI,      ! Dummy identifier for unused rejects identifier.
     :  DNREJ      ! Dummy number of objects in rejects selection.
      CHARACTER
     :  EXPR*(CAT__SZEXP)  ! RA range of bounding box expression.
      INTEGER
     :  RSI,       ! Identifier for range selection.
     :  NUMRNG,    ! Number of objects in the Declination range.
     :  LEXPR,     ! Length of EXPR (excl. trail. blanks).
     :  EIR,       ! Identifier for RA range or bounding box exprn.
     :  LCRIT,     ! Length of CRIT (excl. trail. blanks).
     :  LRACOL,    !   "    "  RACOL ( "  .   "  .   "   ).
     :  LDCCOL,    !   "    "  DCCOL ( "  .   "  .   "   ).
     :  EI,        ! Id. for expression defining the great circle dist.
     :  SELPTR     ! Pointer to the list of selected objects.
      DOUBLE PRECISION
     :  RAMIN,     ! Minimum Right Ascension of the bounding box.
     :  RAMAX,     ! Maximum   "       "     "   "     "      " .
     :  DCMIN,     ! Minimum Declination     "   "     "      " .
     :  DCMAX      ! Maximu       "          "   "     "      " .
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Attempt to get identifiers for the columns and proceed if ok.

         CALL CAT_TIDNT (CI, RACOL, RACOLI, STATUS)
         CALL CAT_TIDNT (CI, DCCOL, DCCOLI, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Generate the coordinates of the bounding box; the minimum and
*          maximum Right Ascension and Declination bounding the circle
*          within which objects are to be selected.

            RAMIN = RACEN - (RADIUS / COS(DCCEN) )
            RAMAX = RACEN + (RADIUS / COS(DCCEN) )

            DCMIN = DCCEN - RADIUS
            DCMAX = DCCEN + RADIUS

*
*          The Declination must lie in the range -Pi to +Pi.

            DCMIN = MAX(DCMIN, -CAP__PI / 2.0D0)
            DCMAX = MIN(DCMAX, CAP__PI / 2.0D0)

*
*          The Right Ascension must lie in the range 0 to 2*Pi

            IF ( RAMIN .LT. 0.0D0 ) THEN
               RAMIN = RAMIN + 2.0D0 * CAP__PI
            ENDIF
            IF ( RAMAX .GT. 2.0D0 * CAP__PI ) THEN
               RAMAX = RAMAX - 2.0D0 * CAP__PI
            ENDIF

C           print3000, radius, ramin, ramax, dcmin, dcmax
C3000       format(1x, 'Coordinates of the bounding box:' /
C     :        3x, 'radius: ', 1pd20.8 /
C     :        3x, 'ramin: ', 1pd20.8 /
C     :        3x, 'ramax: ',  1pd20.8 /
C     :        3x, 'dcmin: ',  1pd20.8 /
C     :        3x, 'dcmax: ', 1pd20.8 )

*
*          Check if the Declination column is sorted.

            CALL CAT_TIQAI (DCCOLI, 'ORDER', ORDER, STATUS)
            IF (ORDER .EQ. CAT__ASCND  .OR.  ORDER .EQ. CAT__DSCND)
     :        THEN

*
*             Attempt to generate a range selection on the Declination
*             and proceed if all is ok and the identifier is not null.
*             Note that the latter case can arise if no objects are
*             found within the required range.

               CALL CAT_SFNDD (CI, DCCOLI, DCMIN, DCMAX, .FALSE.,
     :           RSI, NUMRNG, DRJI, DNREJ, STATUS)

               IF (STATUS .EQ. SAI__OK  .AND.  RSI .NE. CAT__NOID) THEN

*
*                Assemble the expression corresponding to the range in
*                Right Ascension.  Note that the case where the range
*                straddles the 0/24hr boundary is handled.

                  EXPR = ' '
                  LEXPR = 0

                  LRACOL = CHR_LEN(RACOL)

                  IF ( RAMIN .LE. RAMAX ) THEN
                     CALL CHR_PUTC (RACOL(1 : LRACOL), EXPR, LEXPR)
                     CALL CHR_PUTC (' > ', EXPR, LEXPR)
                     CALL CHR_PUTD (RAMIN, EXPR, LEXPR)

                     CALL CHR_PUTC (' AND ', EXPR, LEXPR)

                     CALL CHR_PUTC (RACOL(1 : LRACOL), EXPR, LEXPR)
                     CALL CHR_PUTC (' < ', EXPR, LEXPR)
                     CALL CHR_PUTD (RAMAX, EXPR, LEXPR)
                  ELSE

*
*                  The range crosses the 0/24hr boundary.

                     CALL CHR_PUTC ('( ', EXPR, LEXPR)
                     CALL CHR_PUTC (RACOL(1 : LRACOL), EXPR, LEXPR)
                     CALL CHR_PUTC (' > ', EXPR, LEXPR)
                     CALL CHR_PUTD (RAMIN, EXPR, LEXPR)

                     CALL CHR_PUTC (' AND ', EXPR, LEXPR)

                     CALL CHR_PUTC (RACOL(1 : LRACOL), EXPR, LEXPR)
                     CALL CHR_PUTC (' < ', EXPR, LEXPR)
                     CALL CHR_PUTD (2.0D0*CAP__PI, EXPR, LEXPR)
                     CALL CHR_PUTC (' ) ', EXPR, LEXPR)

                     CALL CHR_PUTC (' OR ', EXPR, LEXPR)

                     CALL CHR_PUTC ('( ', EXPR, LEXPR)
                     CALL CHR_PUTC (RACOL(1 : LRACOL), EXPR, LEXPR)
                     CALL CHR_PUTC (' > ', EXPR, LEXPR)
                     CALL CHR_PUTC ('0.0', EXPR, LEXPR)
                     CALL CHR_PUTC (' AND ', EXPR, LEXPR)

                     CALL CHR_PUTC (RACOL(1 : LRACOL), EXPR, LEXPR)
                     CALL CHR_PUTC (' < ', EXPR, LEXPR)
                     CALL CHR_PUTD (RAMAX, EXPR, LEXPR)
                     CALL CHR_PUTC (' )', EXPR, LEXPR)
                  END IF

C                 print3001, ramin, ramax, dcmin, dcmax, expr(1 : 35),
C    :              expr(36 : 70), expr(71 : 105), lexpr
C3001             format(1x, 'After assembling expression.' /
C    :              3x, 'ramin: ', 1pd20.10 /
C    :              3x, 'ramax: ', 1pd20.10 /
C    :              3x, 'dcmin: ', 1pd20.10 /
C    :              3x, 'dcmax: ', 1pd20.10 /
C    :              3x, 'expr(1 : 35): ', a35 /
C    :              3x, 'expr(36 : 70): ', a35 /
C    :              3x, 'expr(71 : 105): ', a35 /
C    :              3x, 'lexpr: ', i5 )

*
*                Attempt to obtain an identifier for this selection.

                  CALL CAT_EIDNT (CI, EXPR, EIR, STATUS)

*
*                Generate a selection comprising the rows which satisfy
*                this expression.

                  CALL CAT_SELCT (RSI, EIR, .FALSE., CBI, NUMBSE,
     :              DRJI, DNREJ, STATUS)

               END IF
            ELSE

*
*             Assemble the expression for the bounding box.  Again
*             note that the case where the range in Right Ascension
*             straddles the 0/24hr boundary is handled.

               EXPR = ' '
               LEXPR = 0

               LRACOL = CHR_LEN(RACOL)
               LDCCOL = CHR_LEN(DCCOL)

               CALL CHR_PUTC (DCCOL(1 : LDCCOL), EXPR, LEXPR)
               CALL CHR_PUTC (' > ', EXPR, LEXPR)
               CALL CHR_PUTD (DCMIN, EXPR, LEXPR)

               CALL CHR_PUTC (' AND ', EXPR, LEXPR)

               CALL CHR_PUTC (DCCOL(1 : LDCCOL), EXPR, LEXPR)
               CALL CHR_PUTC (' < ', EXPR, LEXPR)
               CALL CHR_PUTD (DCMAX, EXPR, LEXPR)

               CALL CHR_PUTC (' AND ', EXPR, LEXPR)

               IF ( RAMIN .LE. RAMAX ) THEN
                  CALL CHR_PUTC (RACOL(1 : LRACOL), EXPR, LEXPR)
                  CALL CHR_PUTC (' > ', EXPR, LEXPR)
                  CALL CHR_PUTD (RAMIN, EXPR, LEXPR)

                  CALL CHR_PUTC (' AND ', EXPR, LEXPR)

                  CALL CHR_PUTC (RACOL(1 : LRACOL), EXPR, LEXPR)
                  CALL CHR_PUTC (' < ', EXPR, LEXPR)
                  CALL CHR_PUTD (RAMAX, EXPR, LEXPR)
               ELSE

*
*               The range crosses the 0/24hr boundary.

                  CALL CHR_PUTC ('( ', EXPR, LEXPR)
                  CALL CHR_PUTC (RACOL(1 : LRACOL), EXPR, LEXPR)
                  CALL CHR_PUTC (' > ', EXPR, LEXPR)
                  CALL CHR_PUTD (RAMIN, EXPR, LEXPR)

                  CALL CHR_PUTC (' AND ', EXPR, LEXPR)

                  CALL CHR_PUTC (RACOL(1 : LRACOL), EXPR, LEXPR)
                  CALL CHR_PUTC (' < ', EXPR, LEXPR)
                  CALL CHR_PUTD (2.0D0*CAP__PI, EXPR, LEXPR)
                  CALL CHR_PUTC (' ) ', EXPR, LEXPR)

                  CALL CHR_PUTC (' OR ', EXPR, LEXPR)

                  CALL CHR_PUTC ('( ', EXPR, LEXPR)
                  CALL CHR_PUTC (RACOL(1 : LRACOL), EXPR, LEXPR)
                  CALL CHR_PUTC (' > ', EXPR, LEXPR)
                  CALL CHR_PUTC ('0.0', EXPR, LEXPR)

                  CALL CHR_PUTC (' AND ', EXPR, LEXPR)

                  CALL CHR_PUTC (RACOL(1 : LRACOL), EXPR, LEXPR)
                  CALL CHR_PUTC (' < ', EXPR, LEXPR)
                  CALL CHR_PUTD (RAMAX, EXPR, LEXPR)
                  CALL CHR_PUTC (' )', EXPR, LEXPR)
               END IF

C              print3001, ramin, ramax, dcmin, dcmax, expr(1 : 35),
C    :           expr(36 : 70), expr(71 : 105), lexpr

*
*             Attempt to obtain an identifier for this selection.

               CALL CAT_EIDNT (CI, EXPR, EIR, STATUS)

*
*             Generate a selection comprising the rows which satisfy
*             this expression.

               CALL CAT_SELCT (CI, EIR, .FALSE., CBI, NUMBSE,
     :           DRJI, DNREJ, STATUS)

            END IF

*
*          Proceed if all is ok and the base identifier for the final
*          great circle distance selection is not null.  Note that the
*          latter case can arise if no objects were found in the bounding
*          box enclosing the required great circle.

            IF (STATUS .EQ. SAI__OK  .AND.  CBI .NE. CAT__NOID) THEN

*
*             Assemble the expression corresponding to the required
*             great circle.

               CRIT = ' '
               LCRIT = 0

               LRACOL = CHR_LEN(RACOL)
               LDCCOL = CHR_LEN(DCCOL)

               CALL CHR_PUTC ('GREAT(', CRIT, LCRIT)

               CALL CHR_PUTC (RACOL(1 : LRACOL), CRIT, LCRIT)
               CALL CHR_PUTC (',', CRIT, LCRIT)

               CALL CHR_PUTC (DCCOL(1 : LDCCOL), CRIT, LCRIT)
               CALL CHR_PUTC (',', CRIT, LCRIT)

               CALL CHR_PUTD (RACEN, CRIT, LCRIT)
               CALL CHR_PUTC (',', CRIT, LCRIT)

               CALL CHR_PUTD (DCCEN, CRIT, LCRIT)

               CALL CHR_PUTC (') .LE. ', CRIT, LCRIT)
               CALL CHR_PUTD (RADIUS, CRIT, LCRIT)

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
            CALL ERR_REP ('CAP_CIRC_ERR', 'Error finding objects '/
     :        /'within a given angle of a point.', STATUS)

            SI = CAT__NOID
            NUMSEL = 0
            SIR = CAT__NOID
            NUMREJ = 0
         END IF

      END IF

      END
