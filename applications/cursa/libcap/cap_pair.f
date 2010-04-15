      SUBROUTINE CAP_PAIR (CIP, PRMROW, PCRD1I, PCRD2I, CIS, SECROW,
     :  SCRD1I, SCRD2I, PDISTI, CRDTYP, MULTP, MULTS, MAXPR, SECLST,
     :  NPAIR, PRMPR, SECPR, SEPN, PMULT, SMULT, NPRNUL, NPMULT, NSMULT,
     :  STATUS)
*+
*  Name:
*     CAP_PAIR
*  Purpose:
*     Pair two lists of two-dimensional coordinates.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PAIR (CIP, PRMROW, PCRD1I, PCRD2I, CIS, SECROW,
*       SCRD1I, SCRD2I, PDISTI, CRDTYP, MULTP, MULTS, MAXPR; SECLST;
*       NPAIR, PRMPR, SECPR, SEPN, PMULT, SMULT, NPRNUL, NPMULT, NSMULT;
*       STATUS)
*  Description:
*     Pair two lists of two-dimensional coordinates.  The pairing is
*     performed on the basis of similar positions.  The coordinates may
*     be either Cartesian or celestial spherical-polar (eg. Right
*     Ascension and Declination).
*
*     The list of paired objects returned corresponds to 'PRIMARY'
*     pairing.
*  Arguments:
*     CIP  =  INTEGER (Given)
*        Identifier for the primary input catalogue.
*     PRMROW  =  INTEGER (Given)
*        Number of rows in the primary catalogue.
*     PCRD1I  =  INTEGER (Given)
*        Identifier for the first coordinate for pairing in the primary
*        catalogue.
*     PCRD2I  =  INTEGER (Given)
*        Identifier for the second coordinate for pairing in the primary
*        catalogue.
*     CIS  =  INTEGER (Given)
*        Identifier for the secondary input catalogue.
*     SECROW  =  INTEGER (Given)
*        Number of rows in the secondary catalogue.
*     SCRD1I  =  INTEGER (Given)
*        Identifier for the first coordinate for pairing in the secondary
*        catalogue.
*     SCRD2I  =  INTEGER (Given)
*        Identifier for the second coordinate for pairing in the secondary
*        catalogue.  Note that the catalogue must be sorted on this
*        column.
*     PDISTI  =  INTEGER (Given)
*        Identifier for the expression for calculating the critical
*        distance for the current row in the primary catalogue.  Objects
*        pair if their separation is less than or equal to this critical
*        distance.
*     CRDTYP  =  CHARACTER*(*) (Given)
*        The type of coordinates being paired, coded as follows:
*        C  -   Cartesian,
*        S  -   spherical-polar (eg. celestial Right Ascension and
*               Declination).
*     MULTP  =  LOGICAL (Given)
*        Flag indicating whether or not multiple matches in the primary
*        are to be retained in the list of paired objects.  It is coded
*        as follows.
*        .TRUE.  -  all multiple matches in the primary will be retained
*           in the paired list.
*        .FALSE.  -  only the single closest (or 'best') match from a
*           set of multiple matches in the primary will be retained in
*           the paired list.  All the other multiple matches will be
*           considered to be unpaired.
*     MULTS  =  LOGICAL (Given)
*        Flag indicating whether or not multiple matches in the secondary
*        are to be retained in the list of paired objects.  It is coded
*        as follows.
*        .TRUE.  -  all multiple matches in the secondary will be retained
*           in the paired list.
*        .FALSE.  -  only the single closest (or 'best') match from a
*           set of multiple matches in the secondary will be retained in
*           the paired list.  All the other multiple matches will be
*           considered to be unpaired.
*     MAXPR  =  INTEGER (Given)
*        The maximum permitted number of paired objects.
*     SECLST(SECROW)  =  INTEGER (Work)
*        The secondary list.  The array index is the secondary row
*        number.  An array element contains the row in the primary
*        which the secondary pairs with.
*     NPAIR  =  INTEGER (Returned)
*        Number of paired objects.
*     PRMPR(MAXPR)  =  INTEGER (Returned)
*        Array of row numbers in the primary catalogue for the paired
*        objects.
*     SECPR(MAXPR)  =  INTEGER (Returned)
*        Array of row numbers in the secondary catalogue for the paired
*        objects.
*     SEPN(MAXPR)  =  DOUBLE PRECISION (Returned)
*        The separation between two paired objects (radians).
*     PMULT(MAXPR)  =  INTEGER (Returned)
*        The number of primary objects which the row matches.
*     SMULT(MAXPR)  =  INTEGER (Returned)
*        The number of secondary objects which the row matches.
*     NPRNUL  =  INTEGER (Returned)
*        The number of rows in the primary rejected because of null
*        values.
*     NPMULT  =  INTEGER (Returned)
*        The total number of multiple matches encountered in the primary
*        catalogue.
*     NSMULT  =  INTEGER (Returned)
*        The total number of multiple matches encountered in the secondary
*        catalogue.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Note that the algorithm is an implementation of the 'index join'
*     method.
*
*     Set the number of pairs to zero.
*     Set the number of multiple matches to zero.
*     Set the number of primary rows rejected because of null values
*     to zero.
*     For every row in the primary (and while the status is ok)
*       Calculate the critical distance.
*       Get the coordinates for the current primary row.
*       If neither the primary coordinates nor the expression for
*       the critical distance are null then
*         Calculate the maximum and minimum possible values in the
*         secondary for this critical distance.
*         Determine the range of rows corresponding to this range of
*         values.
*         If ok then
*           Set the number of matches to zero.
*           For every row in the range of rows in the secondary (and while
*           the status is ok)
*             Get the coordinates for the current secondary row.
*             If the secondary coordinates are not null then
*               Compute the distance between the position of the current
*               secondary row and the current primary row.
*               If this distance is less than or equal to the critical
*               distance then
*                 Add the object to the list of matches and save its
*                 details.
*               end if
*             end if
*           end for
*           If there is a single match then
*             Accept it as the pair.
*           else if there is more than one match then
*             If a single match is required then
*               Find the closest match and accept it as the pair.
*             else
*               Accept all the matches as pairs.
*             end if
*             Increment the number of multiple matches.
*           else
*             Accept the primary object as unpaired.
*           end if
*         end if
*       else
*         Increment the number of primary rows rejected because of
*         nulls.
*         Accept the primary object as unpaired.
*       end if
*     end for
*     Check for and optionally remove pairs in the primary here, as
*     follows:-
*     Generate a secondary list with every element set to unpaired.
*     Initialise the list of multiple matches for the secondary.
*     For every row in the paired list.
*       If the secondary entry is not paired then
*         If the corresponding row in the secondary list is unpaired
*         then
*           Set the row in the secondary list to contain the
*           corresponding paired list row number.
*         else (the corresponding row in the secondary list is paired)
*           Increment the number of primary multiple matches.
*           If primary multiple matches are not permitted then
*             If the new multiple match is closer than the old one
*               Remove the old match.
*             else (the old match is closer)
*               Remove the new match.
*             end if
*           end if
*         end if
*       end if
*     end for
*     Report any error.
*  Note:
*     The change of 18/8/99 fixed the following inconsistency in counting
*     and reporting multiple matches:
*
*     'In the CURSA application CATPAIR, the reporting of multiple
*     matches in  the primary and secondary catalogues is slightly
*     confusing. When multiple matches are found in the secondary for one
*     object in the primary, the number of multiple matches by which the
*     total NSMULT is incremented is the total number of matches found in
*     the secondary for that object, whereas when multiple matches are
*     found in the primary for one object in the secondary, the number of
*     multiple matches by which the total NPMULT is incremented is the
*     excess of matches above one found in the primary for that object.'
*     (Sally Hales, 11/1/96).
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     9/2/95   (ACD): Original version.
*     17/2/95  (ACD): First stable version.
*     19/11/95 (ACD): Fixed a bug in removing the primary multiple
*        matches.
*     22/1/96  (ACD): Removed an incorrect comment from the
*        'Implementation Deficiencies' section.
*     12/8/99  (ACD): Fixed bug in computing MINVAL and MAXVAL (they
*        were computed the wrong way round).
*     18/8/99  (ACD): Added return arrays containing the separation
*        of paired objects and the number of primary and secondary
*        matches for each paired row.  Also removed an inconsistency
*        in counting the primary and secondary multiple matches.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'      ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'      ! CAT symbolic constants.
*  Arguments Given:
      INTEGER
     :  CIP,
     :  PRMROW,
     :  PCRD1I,
     :  PCRD2I,
     :  CIS,
     :  SECROW,
     :  SCRD1I,
     :  SCRD2I,
     :  PDISTI,
     :  MAXPR
      CHARACTER
     :  CRDTYP*(*)
      LOGICAL
     :  MULTP,
     :  MULTS
*  Arguments for work space:
      INTEGER
     :  SECLST(SECROW)
*  Arguments Returned:
      INTEGER
     :  NPAIR,
     :  PRMPR(MAXPR),
     :  SECPR(MAXPR),
     :  PMULT(MAXPR),
     :  SMULT(MAXPR),
     :  NPRNUL,
     :  NPMULT,
     :  NSMULT
      DOUBLE PRECISION
     :  SEPN(MAXPR)
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:

      DOUBLE PRECISION SLA_DSEP
*  Local Constants:
      INTEGER MMATCH  ! Maximum permitted number of mutiple matches.
      PARAMETER (MMATCH = 1000)

*
*    The value stored for the separation or distance in the case of
*    rows which do not pair.  A nonesense value is used deliberately;
*    genuine separations cannot be negative.

      DOUBLE PRECISION UDIST
      PARAMETER (UDIST = -1.0D0)
*  Local Variables:
      INTEGER
     :  CURPRM,  ! Current object in the primary.
     :  FIRSTR,  ! First object in the current range in the secondary.
     :  LASTR,   ! Last object in the current range in the secondary.
     :  CURSEC,  ! Current row in the secondary.
     :  NMATCH,  ! Number of matches found in the current range.
     :  AMATCH,  ! Actual number of available for the current range.
     :  LOOP,    ! Loop index.
     :  CLSMAT,  ! Row number of the closest match.
     :  SECMAT(MMATCH),  ! Row numbers for all the matches.
     :  PRVMAT   ! Row number of the previous match.
      DOUBLE PRECISION
     :  CRITD,   ! Critical distance for the current primary object.
     :  PCRD1V,  ! First  coord. for current primary object.
     :  PCRD2V,  ! Second   "  .  "     "       "      "   .
     :  MINVAL,  ! Minimum value for the current range.
     :  MAXVAL,  ! Maximum   "    "   "     "      "  .
     :  SCRD1V,  ! First  coord. for current secondary object.
     :  SCRD2V,  ! Second   "  .  "     "        "       "   .
     :  DIST,    ! Distance between current primary and secondary objects.
     :  CLSDIS,  ! Distance between the closest match.
     :  MATDIS(MMATCH) ! Distances between the matches.
      LOGICAL
     :  NULCRD,  ! Null value flag for critical distance expression.
     :  NULP1,   !  "     "    "    "  first  primary  coordinate.
     :  NULP2,   !  "     "    "    "  second    "         "     .
     :  NULS1,   !  "     "    "    "  first  secondary    "     .
     :  NULS2    !  "     "    "    "  second     "        "     .
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Set the number of pairs to zero.

         NPAIR = 0

*
*       Set the number of multiple matches to zero.

         NPMULT = 0
         NSMULT = 0

*
*       Set the number of primary rows rejected because of null values
*       to zero.

         NPRNUL = 0

*
*       Examine every row in the primary and check if it pairs.

         CURPRM = 0

         DO WHILE (CURPRM .LT. PRMROW  .AND.  STATUS .EQ. SAI__OK)
            CURPRM = CURPRM + 1
C           print3035, curprm
C3035       format(1x, 'curprm: ', i10, 2x, '------------')

*
*          Calculate the critical distance for the current row.

            CALL CAT_RGET (CIP, CURPRM, STATUS)
            CALL CAT_EGT0D (PDISTI, CRITD, NULCRD, STATUS)

*
*          Get the coordinate values for the primary.

            CALL CAT_EGT0D (PCRD1I, PCRD1V, NULP1, STATUS)
            CALL CAT_EGT0D (PCRD2I, PCRD2V, NULP2, STATUS)

C           print2000, curprm, pcrd1v, nulp1, pcrd2v, nulp2, status
C2000       format(3x, 'primary: ', i10, 5x, 1pd12.3, l5,
C    :        5x, 1pd12.3, l5, 2x, i10)

*
*          Proceed if none of the critical distance and the two coordinates
*          are null.

            IF (.NOT. NULCRD  .AND.  .NOT. NULP1  .AND.  .NOT. NULP2)
     :        THEN

*
*             Calculate the maximum and minimum possible values in the
*             secondary for this critical distance.

               MINVAL = PCRD2V - CRITD
               MAXVAL = PCRD2V + CRITD

*
*             Determine the range of rows corresponding to this range of
*             values and proceed if ok.

               CALL CAT_SRNGD (CIS, SCRD2I, MINVAL, MAXVAL, FIRSTR,
     :            LASTR, STATUS)

C              print4000, curprm, minval, maxval, firstr, lastr, status
C4000          format(3x,
C    :           'curprm, minval, maxval, firstr, lastr, status: ' /
C    :           3x, i6, 1pd16.7, 1pd16.7, i6, i6, i10)

               IF (STATUS .EQ. SAI__OK) THEN

*
*                Set the number of matches to zero.

                  NMATCH = 0

*
*                Examine every row in the range and check if it pairs.

                  CURSEC = FIRSTR - 1

                  DO WHILE (CURSEC .LT. LASTR  .AND.
     :              STATUS .EQ. SAI__OK)
                     CURSEC = CURSEC + 1

*
*                   Get the values for the current row in the secondary.

                     CALL CAT_RGET (CIS, CURSEC, STATUS)

                     CALL CAT_EGT0D (SCRD1I, SCRD1V, NULS1, STATUS)
                     CALL CAT_EGT0D (SCRD2I, SCRD2V, NULS2, STATUS)

*
*                   Proceed if neither of the coordinates is null.
*                   (Note that the value for the second coordinate
*                   should, in principle, never be null because it is
*                   read from a range selected on a sorted column.
*                   However, it is still checked for null values for
*                   consistency with the treatment of the other columns
*                   and 'just in case'.)

                     IF (.NOT. NULS1  .AND.  .NOT. NULS2) THEN

*
*                      Compute the distance between the position of the
*                      current secondary row and the current primary row,
*                      using either the Cartesian or great circle distance
*                      formula, as appropriate.

                        IF (CRDTYP .EQ. 'C') THEN
                           DIST = SQRT( ((PCRD1V - SCRD1V)**2) +
     :                       ((PCRD2V - SCRD2V)**2) )
                        ELSE
                           DIST =
     :                       SLA_DSEP(PCRD1V, PCRD2V, SCRD1V, SCRD2V)
                        END IF

*
*                      If this actual distance is less than or equal to the
*                      critical distance then add the secondary object to the
*                      list of matches for the current primary object.

                        IF (DIST .LE. CRITD) THEN
                           NMATCH = NMATCH + 1

                           IF (NMATCH .LE. MMATCH) THEN
                              SECMAT(NMATCH) = CURSEC
                              MATDIS(NMATCH) = DIST
                           ELSE
                              IF (NMATCH .EQ. MMATCH) THEN
                                 CALL MSG_SETI ('MMATCH', MMATCH)
                                 CALL MSG_SETI ('CURPRM', CURPRM)

                                 CALL MSG_OUT (' ',
     :                             'Warning: maximum permitted '/
     :                             /'^MMATCH matches exceeded for '/
     :                             /'primary row ^CURPRM.', STATUS)
                              END IF
                           END IF
                        END IF
                     END IF
                  END DO

*
*                Check for the cases of single and multiple matches in
*                the secondary.

                  IF (NMATCH .EQ. 1) THEN

*
*                   There is a single match; accept it as a pair.

                     NPAIR = NPAIR + 1

                     PRMPR(NPAIR) = CURPRM
                     SECPR(NPAIR) = SECMAT(1)
                     SEPN(NPAIR) = MATDIS(1)
                     SMULT(NPAIR) = 1

                  ELSE IF (NMATCH .GT. 1) THEN

*
*                   There are multiple matches; either find the closest or
*                   accept them all, as directed by the appropriate input
*                   argument.

                     IF (.NOT. MULTS) THEN

*
*                      Find the closest match.

                        CLSMAT = 1
                        CLSDIS = MATDIS(1)

                        AMATCH = MIN(NMATCH, MMATCH)

                        DO LOOP = 2, AMATCH
                           IF (MATDIS(LOOP) .LT. CLSDIS) THEN
                              CLSMAT = LOOP
                              CLSDIS = MATDIS(LOOP)
                           END IF
                        END DO

                        NPAIR = NPAIR + 1

                        PRMPR(NPAIR) = CURPRM
                        SECPR(NPAIR) = SECMAT(CLSMAT)
                        SEPN(NPAIR) = MATDIS(CLSMAT)
                        SMULT(NPAIR) = NMATCH

                     ELSE

*
*                      Accept all matches.

                        AMATCH = MIN(NMATCH, MMATCH)

                        DO LOOP = 1, AMATCH
                           PRMPR(NPAIR+LOOP) = CURPRM
                           SECPR(NPAIR+LOOP) = SECMAT(LOOP)
                           SEPN(NPAIR+LOOP) = MATDIS(LOOP)
                           SMULT(NPAIR+LOOP) = NMATCH
                        END DO

                        NPAIR = NPAIR + NMATCH

                     END IF

*
*                   Increment the number of multiple matches.

                     NSMULT = NSMULT + NMATCH

                  ELSE

*
*                   Accept the primary object as unpaired.

                     NPAIR = NPAIR + 1

                     PRMPR(NPAIR) = CURPRM
                     SECPR(NPAIR) = CAT__UPAIR
                     SEPN(NPAIR) = UDIST
                     SMULT(NPAIR) = 0

                  END IF
               END IF

            ELSE

*
*             Either the critical distance for the current primary row
*             evalutated to null or one of the two coordinates for the
*             current primary row was null.  Increment the number of
*             rows in the primary rejected because of nulls.

               NPRNUL = NPRNUL + 1

*
*             Accept the primary object as unpaired.

               NPAIR = NPAIR + 1

               PRMPR(NPAIR) = CURPRM
               SECPR(NPAIR) = CAT__UPAIR
               SEPN(NPAIR) = UDIST
               SMULT(NPAIR) = 0

            END IF

C           print2001, npair, prmpr(npair), secpr(npair)
C2001       format(4x, 'pair: ', i10, 5x, i10, i10)

         END DO

C        print7000, npair
C7000    format(1x, 'npair: ', i10 / )

C        do loop = 1, npair
C           print7001, loop, prmpr(loop), secpr(loop)
C7001       format(1x, 'loop, prmpr, secpr: ', i8, i8, i8)
C        end do

*
*       Check for and optionally remove multiple pairs in the primary
*       here.  First generate a list of secondary rows and set all the
*       entries to unpaired.

         DO LOOP = 1, SECROW
            SECLST(LOOP) = CAT__UPAIR
         END DO

*
*       Initialise the list of multiple matches for the primary.

         DO CURPRM = 1, NPAIR
            PMULT(CURPRM) = 0
         END DO

*
*       Examine entries in the paired list, saving the index into the
*       paired list in the secondary list and checking for multiple
*       matches.

         DO LOOP = 1, NPAIR

*
*          Check if the current row in the paired list is paired with
*          a row in the secondary.

            CURSEC = SECPR(LOOP)

            IF (CURSEC .NE. CAT__UPAIR) THEN
               PMULT(LOOP) = 1

*
*             If the corresponding entry in the secondary list is
*             empty then copy the current index into the paired list into
*             it.  Otherwise a multiple match has been encountered.

               IF (SECLST(CURSEC) .EQ. CAT__UPAIR) THEN
                  SECLST(CURSEC) = LOOP

               ELSE

*
*                Increment the number of primary multiple matches.

                  IF (PMULT(LOOP) .EQ. 1) THEN
                     NPMULT = NPMULT + 2
                  ELSE
                     NPMULT = NPMULT + 1
                  END IF

                  PMULT(LOOP) = PMULT(LOOP) + 1
                  PMULT(SECLST(CURSEC)) = PMULT(SECLST(CURSEC)) + 1

*
*                If primary multiple matches are not permitted then
*                retain only the closest of the two matches.

                  IF (.NOT. MULTP) THEN
                     PRVMAT = SECLST(CURSEC)

                     IF (SEPN(LOOP) .LT. SEPN(PRVMAT) ) THEN
                        SECPR(PRVMAT) = CAT__UPAIR
                        SEPN(PRVMAT) = UDIST

                        SECLST(CURSEC) = LOOP

                     ELSE
                        SECPR(LOOP) = CAT__UPAIR
                        SEPN(LOOP) = UDIST

                     END IF
                  END IF
               END IF
            END IF
         END DO

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CAP_PAIR_ERR', 'Failed to generate list '/
     :        /'of paired objects.', STATUS)
         END IF

      END IF

      END
