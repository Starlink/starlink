      SUBROUTINE CATPAIR (STATUS)
*+
*  Name:
*     CATPAIR
*  Purpose:
*     Pair two catalogues.
*  Language:
*     Fortran 77.
*  Type of Module:
*     ADAM A-task
*  Invocation:
*     CALL CATPAIR (STATUS)
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Description:
*     Pair two catalogues to create a new output catalogue.  The input
*     catalogues are paired on the basis of similar two-dimensional
*     coordinates.  The coordinates may be either celestial spherical-
*     polar or Cartesian.  An index join method is used.
*
*     catpair is a powerful and flexible application.  See SUN/190 for
*     a full description.
*  Usage:
*     catpair
*  ADAM Parameters:
*     PRIMARY  =  CHARACTER (read)
*        The name of the primary input catalogue.
*     SECOND  =  CHARACTER (read)
*        The name of the secondary input catalogue.  This catalogue
*        must be sorted on the second column to be used in the pairing.
*        Usually this column will be the Declination or Y coordinate.
*     OUTPUT  =  CHARACTER (read)
*        The name of the output paired catalogue.  A catalogue with
*        this name must not already exist.
*     CRDTYP  =  CHARACTER (read)
*        The type of coordinates to be paired.  The possibilities are
*        either Cartesian coordinates (`C') or celestial spherical-polar
*        coordinates (`S') such as Right Ascension and Declination.
*     PCRD1  =  CHARACTER (read)
*        The name of the column in the primary catalogue containing the
*        first column to be used in the pairing. This column will usually
*        be an X coordinate or a Right Ascension.
*     PCRD2  =  CHARACTER (read)
*        The name of the column in the primary catalogue containing the
*        second column to be used in the pairing. This column will usually
*        be a Y coordinate or a Declination.
*     SCRD1  =  CHARACTER (read)
*        The name of the column in the secondary catalogue containing the
*        first column to be used in the pairing. This column will usually
*        be an X coordinate or a Right Ascension.
*     SCRD2  =  CHARACTER (read)
*        The name of the column in the secondary catalogue containing the
*        second column to be used in the pairing. This column will usually
*        be a Y coordinate or a Declination. The secondary catalogue must
*        be sorted on this column.
*     PDIST  =  CHARACTER (read)
*        The 'critical distance'; the maximum separation for two objects
*        to be considered pairs.  It may be either a constant, the
*        name of a column in the primary catalogue or an expression
*        involving columns in the primary catalogue.
*     PRTYP  =  CHARACTER (read)
*        The `type of pairing' required, that is the set of rows from the
*        two input catalogues are to be retained in the output catalogue.
*        Briefly, the options are: C - common, P - primary, M - mosaic,
*        R - primrej or A - allrej.  See SUN/190 for more details.
*     MULTP  =  LOGICAL (read)
*        Specify how multiple matches in the primary are to be handled.
*        The options are either to retain the single closest match or to
*        retain all the matches.
*     MULTS  =  LOGICAL (read)
*        Specify how multiple matches in the secondary are to be handled.
*        The options are either to retain the single closest match or to
*        retain all the matches.
*     ALLCOL  =  LOGICAL (read)
*        Specify the set of columns to be retained in the output catalogue.
*        The options are to either retain all the columns from both input
*        catalogues or to retain specified columns from either input
*        catalogue.  If you are in doubt you should retain all the columns.
*     SPCOL  =  LOGICAL (read)
*        Flag indicating whether special columns giving details of the
*        paired objects are to be included in the output catalogue.
*        If SPCOL is set to TRUE the following columns are included:
*        SEPN, the separation of the paired primary and secondary objects;
*        PMULT, the number of matches in the primary;
*        SMULT, the number of matches in the seconary.
*     PRMPAR  =  LOGICAL (read)
*        Specify whether the parameters of the primary are to be copied to
*        the output catalogue.
*     SECPAR  =  LOGICAL (read)
*        Specify whether the parameters of the secondary are to be copied
*        to the output catalogue.
*     PTEXT  =  CHARACTER (read)
*        Specify whether any textual information associated with the
*        primary is to be copied to the output catalogue.  The options
*        are: A - all (create a duplicate of the primary header as
*        comments), C - just copy comments (and history) or N - none.
*     STEXT  =  CHARACTER (read)
*        Specify whether any textual information associated with the
*        secondary is to be copied to the output catalogue.  The options
*        are: A - all (create a duplicate of the secondary header as
*        comments), C - just copy comments (and history) or N - none.
*     TEXT  =  CHARACTER (read)
*        Specify whether a set of comments describing the specification
*        of the pairing pairing is written to the output catalogue.  The
*        options are: Y - write comments (default), N - do not write
*        comments.
*     COLBUF  =  CHARACTER (read)
*        Name for the individual columns to be included in the output
*        catalogue.  Enter 'END' to finish.
*     QUIET  =  LOGICAL (read)
*        Operate in quiet mode where warnings are suppressed.  The
*        permitted values are:
*        TRUE  - quiet mode,
*        FALSE - verbose mode.
*  Examples:
*     catpair
*        Answer the numerous prompts and pair two catalogues.
*     catpair spcol=true
*        Answer the numerous prompts and pair two catalogues.  The output
*        catalogue of paired objects will contain three additional
*        columns containing details for the paired objects.
*     catpair text=n
*        Answer the numerous prompts and pair two catalogues, but
*        specify that a summary of the pairing specification is not to
*        be written as comments to the output catalogue.
*  Pitfalls:
*     Ensure that the secondary catalogue is sorted on the second
*     pairing column.  Usually this column will be the Declination or Y
*     coordinate.  If the secondary is not suitably sorted then use
*     application catsort to sort it.
*  Notes:
*     catpair is intended for the case where the primary catalogue is
*     a relatively small list of target objects which is being paired
*     with a larger secondary catalogue.  It will still work if the
*     primary is a large catalogue, but it is not optimised for this
*     case and will take some time.  Conversely, the size of the
*     secondary catalogue is largely immaterial.
*  Prior Requirements:
*     The secondary catalogue must be sorted on the second pairing
*     coordinate.  Usually this coordinate will be the Declination or
*     Y coordinate.  If the secondary is not suitably sorted then use
*     application catsort to sort it.
*  Algorithm:
*     Obtain and set the quiet mode.
*     Attempt to open the primary catalogue.
*     Attempt to open the secondary catalogue.
*     If ok then
*       Attempt to open the output catalogue.
*       If ok then
*         Get the type of coordinates: celestial or Cartesian.
*         Get identifiers for the columns to be used for pairing the primary.
*         Get identifiers for the columns to be used for pairing the
*         secondary.
*         Get an identifier for the expression defining the critical
*         distance for pairing.
*         Get the type of pairing required (COMMON, MOSAIC etc.)
*         Get the flag indicating whether multiple matches in the
*         primary are to be retained.
*         Get the flag indicating whether multiple matches in the
*         secondary are to be retained.
*         Get the treatment of multiple matches in the secondary.
*         Determine if some or all of the columns are to be copied.
*         If all the columns are to be copied then
*           Generate identifiers for the input and output catalogues.
*         else
*           Get the names of the required columns and generate
*           identifiers for them.
*         end if
*         Determine whether special columns (giving details of the
*         pairing) are to be included in the output catalogue.
*         If the special columns are required then
*           Create the special columns.
*         else
*           Set the identifiers for the special columns to the null
*           identifier.
*         end if
*         Determine if parameters from the primary are to be copied.
*         Determine if parameters from the secondary are to be copied.
*         Determine if textual information from the primary is to be
*         copied.
*         Determine if textual information from the secondary is to be
*         copied.
*         Determine if comments describing the pairing are to be added
*         to the output catalogue.
*         If all is ok then
*           Generate a list of paired (common) objects.
*           Generate a list of objects corresponding to the type of
*           pairing required.
*           If all is ok then
*             Report some details of the join.
*             If required then
*               Copy the parameters from the primary.
*             end if
*             If required then
*               Copy the parameters from the secondary.
*             end if
*             Write the table for the output catalogue.
*             If all is ok then
*               If required then
*                 Write comments describing the pairing.
*               end if
*               If required then
*                 Copy the textual information from the primary.
*               end if
*               If required then
*                 Copy the textual information from the secondary.
*               end if
*             else
*               Report error writing the table for the output catalogue.
*             end if
*           else
*             Report error generating the paired objects.
*           end if
*           Free the various work arrays.
*         else
*           Report error getting input parameters.
*         end if
*         Close the output catalogue.
*       else
*         Report error opening the output catalogue.
*       end if
*       Close the input catalogues.
*     else
*       Report error opening the input catalogues.
*     end if
*     Report either success or an error, as appropriate.
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     8/2/95   (ACD): Original version.
*     20/4/95  (ACD): First stable version.
*     27/8/96  (ACD): Added additional options for saving textual
*        information and added a standard A-task prologue.
*     10/12/96 (ACD): Removed unused argument in call to CAP_PAIRT.
*     2/2/97   (ACD): Changed to use only the first character of
*        variable TEXT, for consistency.
*     26/5/97  (ACD): Moved writing the parameters for the output
*        catalogue to before writing its table of values.  This is
*        necessary if the parameters are to be actually written for
*        a FITS table.
*     27/5/97  (ACD): Added checks to remove name clashes between
*        parameters copied from the input and output catalogues.
*     3/6/97   (ACD): Added check for bad status when looping for
*        parameters PTEXT, STEXT and TEXT.
*     17/8/99  (ACD): Added option to include special columns giving
*        details of the pairing in the output catalogue.
*     26/11/99 (ACD): Added an item showing the use of the SPCOL option
*        in the 'Examples' section.
*     5/4/01   (ACD): Added the quiet mode.
*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CAT_PAR'          ! CAT constants.
      INCLUDE 'CAT_ERR'          ! CAT error codes.
      INCLUDE 'CNF_PAR'          ! CNF functions
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  CIP,     ! Identifier for primary   catalogue.
     :  CIS,     !     "       "  secondary     "    .
     :  CIOUT,   !     "       "  output        "    .
     :  FIPRIM(CAT__MXCOL),  ! Identifiers for columns in primary.
     :  FISEC(CAT__MXCOL),   !      "       "     "    "  secondary.
     :  FIPOUT(CAT__MXCOL),  ! Ids. for primary   cols. in output cat.
     :  FISOUT(CAT__MXCOL),  !  " .  "  secondary  "  . "    "     " .
     :  COLIDS(CAT__MXCOL),  ! Ids. for all columns in the output cat.
     :  PCOLS,   ! Number of columns to be copied from the primary.
     :  SCOLS,   !   "    "     "    "  "    "     "    "  secondary.
     :  TCOLS    ! Total number of columns in the output catalogue.
      INTEGER
     :  PCRD1I,  ! Id. for first  pairing coord. in primary.
     :  PCRD2I,  ! " .  "  second    "      "  . "     "   .
     :  SCRD1I,  ! " .  "  first     "      "  . "  secondary.
     :  SCRD2I,  ! " .  "  second    "      "  . "      "    .
     :  PDISTI,  ! Identifier for expression for the critical distance.
     :  PRMPTR,  ! Pointer for column of row numbers in the primary.
     :  SECPTR,  !    "     "    "    "   "     "    "   "  secondary.
     :  SPNPTR,  ! Pointer to list of pair separations.
     :  PMMPTR,  ! Pointer to list of numbers of primary   multiple matches.
     :  SMMPTR   !    "    "   "   "     "    "  secondary    "        "   .
      INTEGER
     :  SEPNI,   ! Identifier for column to hold separations.
     :  PMLTI,   !     "       "    "    "   "   primary   multiple matches.
     :  SMLTI,   !     "       "    "    "   "   secondary    "        "   .
     :  MAXROW,  ! Maximum permitted number of paired rows.
     :  NPAIR,   ! Actual number of paired rows ('PRIMARY' pairing).
     :  NPRNUL,  ! Number of primary rows rejected because of nulls.
     :  NPMULT,  ! Number of multiple matches in the primary.
     :  NSMULT,  !   "    "     "        "    "   "  secondary.
     :  PRMROW,  ! Number of rows in the primary   catalogue.
     :  SECROW   !   "    "   "   "   "  secondary     "    .
      INTEGER
     :  OPAIR,   ! Number of rows in the paired catalogue.
     :  OPRPTR,  ! Pointer to list of paired rows in the primary   catalogue.
     :  OSCPTR,  !    "    "   "   "    "     "   "   "  secondary     "    .
     :  OSPPTR,  !    "    "   "   "  separations.
     :  OPMPTR,  !    "    "   "   "  numbers of primary   multiple matches.
     :  OSMPTR,  !    "    "   "   "     "    "  secondary    "        "   .
     :  WRKPTR   ! Pointer to work array.
      CHARACTER
     :  CRDTYP*1, ! Type of coordinates: Cartesian or spherical-polar.
     :  PCRD1*(CAT__SZCMP), ! Name of first  pairing coord. in primary.
     :  PCRD2*(CAT__SZCMP), !  "   "  second    "      "  . "     "   .
     :  SCRD1*(CAT__SZCMP), !  "   "  first     "      "  . "  secondary.
     :  SCRD2*(CAT__SZCMP), !  "   "  second    "      "  . "      "    .
     :  PDIST*(CAT__SZEXP), ! Expression for the critical distance.
     :  COLNAM(CAT__MXCOL)*(CAT__SZCMP), ! Names all cols. in output cat.
     :  PRTYP*1   ! Type of pairing: common etc.
      LOGICAL
     :  QUIET,    ! Flag; operate in quiet or verbose (normal) mode?
     :  MULTP,    ! Flag; permit multiple matches in primary?
     :  MULTS,    !  "  ;   "       "        "    "  secondary?
     :  ALLCOL,   !  "  ; copy all columns?
     :  SPCOL,    !  "  ; create special columns with pairing details?
     :  PRMPAR,   !  "  ; copy primary   parameters?
     :  SECPAR    !  "  ;  "   secondary     "     ?
      CHARACTER
     :  PTEXT*10, ! Flag; copy textual information from the primary?
     :  STEXT*10, !  "  ;  "      "         "       "    "  secondary?
     :  TEXT*10   ! Flag; add comments describing the pairing?
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Obtain and set the quiet mode.

         CALL PAR_GET0L ('QUIET', QUIET, STATUS)

         IF (QUIET) THEN
            CALL CAT_TUNES ('QUIET', 'YES', STATUS)
         ELSE
            CALL CAT_TUNES ('QUIET', 'NO', STATUS)
         END IF

*
*       Attempt to open the primary and secondary catalogues and proceed
*       if ok.

         CALL CAT_ASSOC ('PRIMARY', 'READ', CIP, STATUS)
         CALL CAT_ASSOC ('SECOND', 'READ', CIS, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Attempt to open the output catalogue and proceed if ok.

            CALL CAT_CREAT ('OUTPUT', CIOUT, STATUS)

            IF (STATUS .EQ. SAI__OK) THEN

*
*             Get the type of coordinates: celestial or Cartesian.

               CRDTYP = ' '

               DO WHILE (CRDTYP .NE. 'C'  .AND.  CRDTYP .NE. 'S'
     :           .AND.  STATUS .EQ. SAI__OK)
                  CALL PAR_GET0C ('CRDTYP', CRDTYP, STATUS)
                  CALL PAR_CANCL ('CRDTYP', STATUS)

                  CALL CHR_UCASE (CRDTYP(1 : 1) )
               END DO

*
*             Get names and identifiers for the columns to be used for
*             pairing the primary.  Note that the default column names
*             depend on whether spherical-polar or Cartesian coordinates
*             are being used.

               IF (CRDTYP .EQ. 'S') THEN
                  CALL PAR_DEF0C ('PCRD1', 'RA', STATUS)
                  CALL PAR_DEF0C ('PCRD2', 'DEC', STATUS)
               ELSE
                  CALL PAR_DEF0C ('PCRD1', 'X', STATUS)
                  CALL PAR_DEF0C ('PCRD2', 'Y', STATUS)
               END IF

               CALL PAR_GET0C ('PCRD1', PCRD1, STATUS)
               CALL PAR_CANCL ('PCRD1', STATUS)
               CALL CAT_TIDNT (CIP, PCRD1, PCRD1I, STATUS)

               CALL PAR_GET0C ('PCRD2', PCRD2, STATUS)
               CALL PAR_CANCL ('PCRD2', STATUS)
               CALL CAT_TIDNT (CIP, PCRD2, PCRD2I, STATUS)

*
*             Get names and identifiers for the columns to be used for
*             pairing the secondary.

               CALL PAR_DEF0C ('SCRD1', PCRD1, STATUS)
               CALL PAR_GET0C ('SCRD1', SCRD1, STATUS)
               CALL PAR_CANCL ('SCRD1', STATUS)
               CALL CAT_TIDNT (CIS, SCRD1, SCRD1I, STATUS)

               CALL PAR_DEF0C ('SCRD2', PCRD2, STATUS)
               CALL PAR_GET0C ('SCRD2', SCRD2, STATUS)
               CALL PAR_CANCL ('SCRD2', STATUS)
               CALL CAT_TIDNT (CIS, SCRD2, SCRD2I, STATUS)

*
*             Get the expression for the critical distance and get an
*             identifier for it.  Note that the case where the critical
*             distance is entered in the form 'floating point number
*             followed by "arcsec"' is explicitly handled in routine
*             CAP_ARCDC rather than in the parser.

               CALL PAR_GET0C ('PDIST', PDIST, STATUS)
               CALL PAR_CANCL ('PDIST', STATUS)

               CALL CAP_ARCDC (PDIST, STATUS)

               CALL CAT_EIDNT (CIP, PDIST, PDISTI, STATUS)

*
*             Get the type of pairing required (COMMON, MOSAIC etc.)

               PRTYP = ' '

               DO WHILE (PRTYP .NE. 'C'  .AND.  PRTYP .NE. 'P'  .AND.
     :           PRTYP .NE. 'M'  .AND.  PRTYP .NE. 'R'  .AND.
     :           PRTYP .NE. 'A'  .AND.  STATUS .EQ. SAI__OK)
                  CALL MSG_OUT (' ', 'Select the required set of '/
     :              /'output rows:', STATUS)

                  CALL MSG_OUT (' ', 'COMMON:  '/
     :              /'common rows  ...   ...   ...   ...   ...   '/
     :              /'...  ... C', STATUS)
                  CALL MSG_OUT (' ', 'PRIMARY: '/
     :              /'all rows in the primary catalogue    ...   '/
     :              /'...  ... P', STATUS)
                  CALL MSG_OUT (' ', 'MOSAIC:  '/
     :              /'common rows plus unpaired in both catalogues '/
     :              /'.  ... M', STATUS)
                  CALL MSG_OUT (' ', 'PRIMREJ: '/
     :              /'rejected rows in the primary catalogue .   '/
     :              /'...  ... R', STATUS)
                  CALL MSG_OUT (' ', 'ALLREJ:  '/
     :              /'rejected rows in both catalogues .   ...   '/
     :              /'...  ... A', STATUS)

                  CALL PAR_GET0C ('PRTYP', PRTYP, STATUS)
                  CALL PAR_CANCL ('PRTYP', STATUS)
                  CALL CHR_UCASE (PRTYP)
               END DO

*
*             Get the flag indicating whether multiple matches in the
*             primary are to be retained.

               CALL PAR_GET0L ('MULTP', MULTP, STATUS)
               CALL PAR_CANCL ('MULTP', STATUS)

*
*             Get the flag indicating whether multiple matches in the
*             secondary are to be retained.

               CALL PAR_GET0L ('MULTS', MULTS, STATUS)
               CALL PAR_CANCL ('MULTS', STATUS)

*
*             Determine whether some or all of the columns are to be copied.

               CALL PAR_GET0L ('ALLCOL', ALLCOL, STATUS)
               CALL PAR_CANCL ('ALLCOL', STATUS)

*
*             Generate input and output identifiers for some or all of
*             the columns, as required.

               IF (ALLCOL) THEN
                  CALL CAP_PACOL (CIP, CIS, CIOUT, CAT__MXCOL,
     :              PCOLS, FIPRIM, FIPOUT, SCOLS, FISEC, FISOUT,
     :              STATUS)
               ELSE
                  TCOLS = 0

                  CALL MSG_OUT (' ', 'Enter columns to be copied '/
     :              /'from the primary:', STATUS)
                  CALL CAP_GCOLS (CIP, CIOUT, CAT__MXCOL,
     :              TCOLS, COLNAM, COLIDS, PCOLS, FIPRIM, FIPOUT,
     :              STATUS)

                  CALL MSG_OUT (' ', 'Enter columns to be copied '/
     :              /'from the secondary:', STATUS)
                  CALL CAP_GCOLS (CIS, CIOUT, CAT__MXCOL,
     :              TCOLS, COLNAM, COLIDS, SCOLS, FISEC, FISOUT,
     :              STATUS)
               END IF

*
*             Determine whether special columns (giving details of the
*             pairing) are to be included in the output catalogue.
*             If these columns are required then create them; otherwise
*             set the corresponbding identifiers to the null identifier.

               CALL PAR_GET0L ('SPCOL', SPCOL, STATUS)
               CALL PAR_CANCL ('SPCOL', STATUS)

               IF (SPCOL) THEN
                  CALL CAT_CNEWS (CIOUT, 'SEPN', CAT__TYPED, 0,
     :              'RADIANS{MS.2}', 'D16.8', 'Separation', SEPNI,
     :              STATUS)

                  CALL CAT_CNEWS (CIOUT, 'PMULT', CAT__TYPEI, 0,
     :              ' ', 'I4', 'Multiple matches in the primary',
     :              PMLTI, STATUS)

                  CALL CAT_CNEWS (CIOUT, 'SMULT', CAT__TYPEI, 0,
     :              ' ', 'I4', 'Multiple matches in the secondary',
     :              SMLTI, STATUS)
               ELSE
                  SEPNI = CAT__NOID
                  PMLTI = CAT__NOID
                  SMLTI = CAT__NOID
               END IF

*
*             Determine whether parameters and textual information in
*             the primary and secondary are to be copied.  For textual
*             information the options are A - all,   C - comments only,
*             N - none.

               CALL PAR_GET0L ('PRMPAR', PRMPAR, STATUS)
               CALL PAR_CANCL ('PRMPAR', STATUS)

               CALL PAR_GET0L ('SECPAR', SECPAR, STATUS)
               CALL PAR_CANCL ('SECPAR', STATUS)

               PTEXT = ' '

               DO WHILE (PTEXT(1 : 1) .NE. 'A'  .AND.
     :                   PTEXT(1 : 1) .NE. 'C'  .AND.
     :                   PTEXT(1 : 1) .NE. 'N'  .AND.
     :                   STATUS .EQ. SAI__OK)
                  CALL PAR_GET0C ('PTEXT', PTEXT, STATUS)
                  CALL PAR_CANCL ('PTEXT', STATUS)

                  CALL CHR_UCASE (PTEXT)
               END DO

               STEXT = ' '

               DO WHILE (STEXT(1 : 1) .NE. 'A'  .AND.
     :                   STEXT(1 : 1) .NE. 'C'  .AND.
     :                   STEXT(1 : 1) .NE. 'N'  .AND.
     :                   STATUS .EQ. SAI__OK)
                  CALL PAR_GET0C ('STEXT', STEXT, STATUS)
                  CALL PAR_CANCL ('STEXT', STATUS)

                  CALL CHR_UCASE (STEXT)
               END DO

*
*             Determine if comments describing the pairing are to be
*             added to the output catalogue.

               TEXT = ' '

               DO WHILE (TEXT(1 : 1) .NE. 'Y'  .AND.
     :                   TEXT(1 : 1) .NE. 'N'  .AND.
     :                   STATUS .EQ. SAI__OK)
                  CALL PAR_GET0C ('TEXT', TEXT, STATUS)
                  CALL PAR_CANCL ('TEXT', STATUS)

                  CALL CHR_UCASE (TEXT)
               END DO

*
*             Proceed if all is ok.

               IF (STATUS .EQ. SAI__OK) THEN

*
*                Obtain pointers to the arrays to hold lists of paired
*                rows.  The total number of rows which will pair is
*                indeterminate; it can be more than the total number of
*                rows in either catalogue (because of multiple matches).
*                It is assumed the total number of pairs will be no more
*                than three times the number of rows in the largest
*                catalogue.

                  CALL CAT_TROWS (CIP, PRMROW, STATUS)
                  CALL CAT_TROWS (CIS, SECROW, STATUS)

                  MAXROW = MAX(PRMROW, SECROW)
                  MAXROW = MAXROW * 3

                  CALL CAP_CRTAR (MAXROW, '_INTEGER',  PRMPTR, STATUS)
                  CALL CAP_CRTAR (MAXROW, '_INTEGER',  SECPTR, STATUS)

                  CALL CAP_CRTAR (MAXROW, '_DOUBLE',  SPNPTR, STATUS)
                  CALL CAP_CRTAR (MAXROW, '_INTEGER', PMMPTR, STATUS)
                  CALL CAP_CRTAR (MAXROW, '_INTEGER', SMMPTR, STATUS)
                  CALL CAP_CRTAR (SECROW, '_INTEGER', WRKPTR, STATUS)

                  CALL CAP_CRTAR (MAXROW, '_INTEGER', OPRPTR, STATUS)
                  CALL CAP_CRTAR (MAXROW, '_INTEGER', OSCPTR, STATUS)

                  CALL CAP_CRTAR (MAXROW, '_DOUBLE',  OSPPTR, STATUS)
                  CALL CAP_CRTAR (MAXROW, '_INTEGER', OPMPTR, STATUS)
                  CALL CAP_CRTAR (MAXROW, '_INTEGER', OSMPTR, STATUS)

*
*                Generate a list of paired (common) objects.

                  CALL CAP_PAIR (CIP, PRMROW, PCRD1I, PCRD2I,
     :              CIS, SECROW, SCRD1I, SCRD2I, PDISTI, CRDTYP,
     :              MULTP, MULTS, MAXROW, %VAL(CNF_PVAL(WRKPTR)),
     :              NPAIR, %VAL(CNF_PVAL(PRMPTR)),
     :              %VAL(CNF_PVAL(SECPTR)), %VAL(CNF_PVAL(SPNPTR)),
     :              %VAL(CNF_PVAL(PMMPTR)), %VAL(CNF_PVAL(SMMPTR)),
     :              NPRNUL, NPMULT, NSMULT, STATUS)

*
*                Generate a list of objects corresponding to the type of
*                pairing required.

                  CALL CAP_PAIRT (SECROW, NPAIR,
     :              %VAL(CNF_PVAL(PRMPTR)), %VAL(CNF_PVAL(SECPTR)),
     :              %VAL(CNF_PVAL(SPNPTR)), %VAL(CNF_PVAL(PMMPTR)),
     :              %VAL(CNF_PVAL(SMMPTR)), PRTYP, MAXROW,
     :              %VAL(CNF_PVAL(WRKPTR)), OPAIR,
     :              %VAL(CNF_PVAL(OPRPTR)), %VAL(CNF_PVAL(OSCPTR)),
     :              %VAL(CNF_PVAL(OSPPTR)), %VAL(CNF_PVAL(OPMPTR)),
     :              %VAL(CNF_PVAL(OSMPTR)), STATUS)

*
*                Proceed if all ok.

                  IF (STATUS .EQ. SAI__OK) THEN

*
*                   Report some details of the join.

                     CALL MSG_SETI ('PRMROW', PRMROW)
                     CALL MSG_OUT (' ', 'Number of rows in the '/
     :                 /'primary input catalogue: ^PRMROW', STATUS)

                     CALL MSG_SETI ('SECROW', SECROW)
                     CALL MSG_OUT (' ', 'Number of rows in the '/
     :                 /'secondary input catalogue: ^SECROW', STATUS)

                     CALL MSG_OUT (' ', ' ', STATUS)

                     CALL MSG_SETI ('OPAIR', OPAIR)
                     CALL MSG_OUT (' ', 'Number of rows in the output '/
     :                 /'catalogue: ^OPAIR', STATUS)

                     CALL MSG_OUT (' ', ' ', STATUS)

                     CALL MSG_SETI ('NPAIR', NPAIR)
                     CALL MSG_OUT (' ', 'Number of rows in the'/
     :                 /' ''PRIMARY'' pairing list: ^NPAIR', STATUS)

                     CALL MSG_SETI ('NPRNUL', NPRNUL)
                     CALL MSG_OUT (' ', 'Number of rows in the primary'/
     :                 /' unpaired because of null values: ^NPRNUL',
     :                 STATUS)

                     CALL MSG_SETI ('NPMULT', NPMULT)
                     CALL MSG_OUT (' ', 'Number of multiple matches '/
     :                 /'found in the primary: ^NPMULT', STATUS)

                     CALL MSG_SETI ('NSMULT', NSMULT)
                     CALL MSG_OUT (' ', 'Number of multiple matches '/
     :                 /'found in the secondary: ^NSMULT', STATUS)


*
*                   If required copy the parameters from the primary.

                     IF (PRMPAR) THEN
                        CALL CAP_CPPAR (CIP, CIOUT, STATUS)
                     END IF

*
*                   If required copy the parameters from the secondary.

                     IF (SECPAR) THEN
                        CALL CAP_CPPRS (CIS, CIOUT, STATUS)
                     END IF

*
*                   Write the table for the output catalogue and proceed
*                   if ok.

                     CALL CAP_JTWRT (CIP, CIS, CIOUT, PCOLS, FIPRIM,
     :                 FIPOUT, SCOLS, FISEC, FISOUT, SEPNI, PMLTI,
     :                 SMLTI, OPAIR, %VAL(CNF_PVAL(OPRPTR)),
     :                 %VAL(CNF_PVAL(OSCPTR)), %VAL(CNF_PVAL(OSPPTR)),
     :                 %VAL(CNF_PVAL(OPMPTR)), %VAL(CNF_PVAL(OSMPTR)),
     :                 STATUS)

                     IF (STATUS .EQ. SAI__OK) THEN

*
*                      If required write comments describing the pairing.

                        IF (TEXT(1 : 1) .NE. 'N') THEN
                           CALL CAP_PRDET (CIOUT, CIP, CIS,
     :                       PCRD1, PCRD2, SCRD1, SCRD2,
     :                       CRDTYP, PDIST, PRTYP,
     :                       MULTP, MULTS, ALLCOL, PRMPAR, SECPAR,
     :                       STATUS)
                        END IF

*
*                      If required copy the textual information from
*                      the primary.

                        IF (PTEXT(1 : 1) .NE. 'N') THEN
                           CALL CAT_PUTXT (CIOUT, 'COMMENT',
     :                       'Comments copied from the primary:',
     :                       STATUS)
                           CALL CAP_CPTXT (CIP, CIOUT, PTEXT(1 : 1),
     :                       STATUS)
                        END IF

*
*                      If required copy the textual information from
*                      the secondary.

                        IF (STEXT(1 : 1) .NE. 'N') THEN
                           CALL CAT_PUTXT (CIOUT, 'COMMENT',
     :                       'Comments copied from the secondary:',
     :                       STATUS)
                           CALL CAP_CPTXT (CIS, CIOUT, STEXT(1 : 1),
     :                       STATUS)
                        END IF

                     ELSE
                        CALL ERR_REP ('CUPAIR_JTB',
     :                    'CUPAIR: Failure writing the table for the '/
     :                    /'output catalogue.', STATUS)

                     END IF

                  ELSE
                     CALL ERR_REP ('CUPAIR_PLI',
     :                 'CUPAIR: Failure generating the list of paired '/
     :                 /'rows.', STATUS)

                  END IF

*
*                Free the various work arrays.

                  CALL CAP_FREAR (PRMPTR, STATUS)
                  CALL CAP_FREAR (SECPTR, STATUS)

                  CALL CAP_FREAR (SPNPTR, STATUS)
                  CALL CAP_FREAR (PMMPTR, STATUS)
                  CALL CAP_FREAR (SMMPTR, STATUS)
                  CALL CAP_FREAR (WRKPTR, STATUS)

                  CALL CAP_FREAR (OPRPTR, STATUS)
                  CALL CAP_FREAR (OSCPTR, STATUS)

                  CALL CAP_FREAR (OSPPTR, STATUS)
                  CALL CAP_FREAR (OPMPTR, STATUS)
                  CALL CAP_FREAR (OSMPTR, STATUS)

               ELSE
                  CALL ERR_REP ('CUPAIR_INP',
     :              'CUPAIR: Failure obtaining the input to define the'/
     :              /' pairing.', STATUS)

               END IF

*
*             Close the output catalogue.

               CALL CAT_TRLSE (CIOUT, STATUS)

            ELSE
               CALL ERR_REP ('CUPAIR_OUT',
     :           'CUPAIR: Failed to open the output catalogue.',
     :           STATUS)

            END IF

*
*          Close the input catalogues.

            CALL CAT_TRLSE (CIP, STATUS)
            CALL CAT_TRLSE (CIS, STATUS)

         ELSE
            CALL ERR_REP ('CUPAIR_IN',
     :        'CUPAIR: Failed to open the input catalogues.',
     :        STATUS)

         END IF

*
*       Report either success or an error, as appropriate.

         IF (STATUS .EQ. SAI__OK) THEN
            CALL MSG_OUT (' ', ' ', STATUS)
            CALL MSG_OUT (' ', 'Pairing completed successfully.',
     :        STATUS)

         ELSE
            CALL ERR_REP ('CUPAIR_ERR',
     :        'Failed to pair the catalogues.', STATUS)

            IF (STATUS .EQ. CAT__INVSR) THEN
               CALL MSG_SETC ('SCRD2', SCRD2)
               CALL ERR_REP ('CUPAIR_IVS', 'The secondary '/
     :           /'catalogue is not sorted on column ^SCRD2.',
     :           STATUS)
            END IF

         END IF

      END IF

      END
