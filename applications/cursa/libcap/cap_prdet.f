      SUBROUTINE CAP_PRDET (CIOUT, CIP, CIS, PCRD1, PCRD2, SCRD1, SCRD2,
     :   CRDTYP, PDIST, PRTYP, MULTP, MULTS, ALLCOL, PRMPAR, SECPAR,
     :   STATUS)
*+
*  Name:
*     CAP_PRDET
*  Purpose:
*     Add details of pairing to a catalogue as comments.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PRDET (CIOUT, CIP, CIS, PCRD1, PCRD2, SCRD1, SCRD2,
*        CRDTYP, PDIST, PRTYP, MULTP, MULTS, ALLCOL, PRMPAR, SECPAR;
*        STATUS)
*  Description:
*     Add details of pairing to a catalogue as comments.
*  Arguments:
*     CIOUT  =  INTEGER (Given)
*        Identifier for the output catalogue.
*     CIP  =  INTEGER (Given)
*        Identifier for the primary input catalogue.
*     CIS  =  INTEGER (Given)
*        Identifier for the secondary input catalogue.
*     PCRD1  =  CHARACTER*(*) (Given)
*        Name of the first coordinate for pairing in the primary.
*     PCRD2  =  CHARACTER*(*) (Given)
*        Name of the second coordinate for pairing in the primary.
*     SCRD1  =  CHARACTER*(*) (Given)
*        Name of the first coordinate for pairing in the seconadry.
*     SCRD2  =  CHARACTER*(*) (Given)
*        Name of the second coordinate for pairing in the seconadry.
*     CRDTYP  =  CHARACTER*(*) (Given)
*        Type of coordinates: Cartesian or spherical-polar.
*     PDIST  =  CHARACTER*(*) (Given)
*        Expression for the critical distance.
*     PRTYP  =  CHARACTER*(*) (Given)
*        Code for the type of pairing: common etc.
*     MULTP  =  LOGICAL (Given)
*        Flag; permit multiple matches in primary?
*     MULTS  =  LOGICAL (Given)
*        Flag; permit multiple matches in secondary?
*     ALLCOL  =  LOGICAL (Given)
*        Flag; copy all columns?
*     PRMPAR  =  LOGICAL (Given)
*        Flag; copy all the parameters from the primary?
*     SECPAR  =  LOGICAL (Given)
*        Flag; copy all the parameters from the secondary?
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Format comments corresponding to the various items and write them
*     to the output catalogue.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     27/8/96 (ACD): Original version.
*     26/5/97 (ACD): Changed wording slightly.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
*  Arguments Given:
      INTEGER
     :  CIOUT,
     :  CIP,
     :  CIS
      CHARACTER
     :  PCRD1*(*),
     :  PCRD2*(*),
     :  SCRD1*(*),
     :  SCRD2*(*),
     :  CRDTYP*(*),
     :  PDIST*(*),
     :  PRTYP*(*)
      LOGICAL
     :  MULTP,
     :  MULTS,
     :  ALLCOL,
     :  PRMPAR,
     :  SECPAR
*  Status:
      INTEGER STATUS          ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
*     <...>
*  Local Variables:
      CHARACTER
     :  PRNAME*(CAT__SZCNM),  ! Name of the primary   catalogue.
     :  SCNAME*(CAT__SZCNM),  !  "   "   "  secondary     "    .
     :  BUFFER*75             ! Output buffer for the current comment.
      INTEGER
     :  LENGTH,   ! Length of a string (excl. trail. blanks).
     :  BUFLEN    !   "    "  BUFFER   ( "  .   "  .   "   ).

*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Introductory text.

         CALL CAT_PUTXT (CIOUT, 'COMMENT',
     :     'This catalogue comprises objects paired using program '/
     :     /'catpair.', STATUS)
         CALL CAT_PUTXT (CIOUT, 'COMMENT',
     :     'The details are as follows.', STATUS)
         CALL CAT_PUTXT (CIOUT, 'COMMENT', ' ', STATUS)

*
*       Names of the primary and secondary catalogues.

         CALL CAT_TIQAC (CIP, 'NAME', PRNAME, STATUS)
         CALL CAT_TIQAC (CIS, 'NAME', SCNAME, STATUS)

         BUFFER = ' '
         BUFLEN = 0

         CALL CHR_PUTC ('Primary catalogue: ', BUFFER, BUFLEN)

         IF (PRNAME .NE. ' ') THEN
            LENGTH = CHR_LEN(PRNAME)
            CALL CHR_PUTC (PRNAME(1 : LENGTH), BUFFER, BUFLEN)
         END IF

         CALL CHR_PUTC ('  Secondary catalogue: ', BUFFER, BUFLEN)

         IF (SCNAME .NE. ' ') THEN
            LENGTH = CHR_LEN(SCNAME)
            CALL CHR_PUTC (SCNAME(1 : LENGTH), BUFFER, BUFLEN)
         END IF

         CALL CAT_PUTXT (CIOUT, 'COMMENT', BUFFER(1 : BUFLEN), STATUS)

*
*       Columns used in the pairing.

         BUFFER = ' '
         BUFLEN = 0

         CALL CHR_PUTC ('Primary columns for pairing: ', BUFFER, BUFLEN)

         IF (PCRD1 .NE. ' ') THEN
            LENGTH = CHR_LEN(PCRD1)
            CALL CHR_PUTC (PCRD1(1 : LENGTH), BUFFER, BUFLEN)
         END IF

         CALL CHR_PUTC (', ', BUFFER, BUFLEN)

         IF (PCRD2 .NE. ' ') THEN
            LENGTH = CHR_LEN(PCRD2)
            CALL CHR_PUTC (PCRD2(1 : LENGTH), BUFFER, BUFLEN)
         END IF

         CALL CAT_PUTXT (CIOUT, 'COMMENT', BUFFER(1 : BUFLEN), STATUS)

         BUFFER = ' '
         BUFLEN = 0

         CALL CHR_PUTC ('Secondary columns for pairing: ', BUFFER,
     :     BUFLEN)

         IF (SCRD1 .NE. ' ') THEN
            LENGTH = CHR_LEN(SCRD1)
            CALL CHR_PUTC (SCRD1(1 : LENGTH), BUFFER, BUFLEN)
         END IF

         CALL CHR_PUTC (', ', BUFFER, BUFLEN)

         IF (SCRD2 .NE. ' ') THEN
            LENGTH = CHR_LEN(SCRD2)
            CALL CHR_PUTC (SCRD2(1 : LENGTH), BUFFER, BUFLEN)
         END IF

         CALL CAT_PUTXT (CIOUT, 'COMMENT', BUFFER(1 : BUFLEN), STATUS)

*
*       Type of columns.

         BUFFER = ' '
         BUFLEN = 0

         CALL CHR_PUTC ('These columns contain ', BUFFER, BUFLEN)

         IF (CRDTYP .EQ. 'S') THEN
            CALL CHR_PUTC ('spherical-polar ', BUFFER, BUFLEN)
         ELSE IF (CRDTYP .EQ. 'C') THEN
            CALL CHR_PUTC ('Cartesian ', BUFFER, BUFLEN)
         ELSE
            CALL CHR_PUTC ('<unknown type> ', BUFFER, BUFLEN)
         END IF

         CALL CHR_PUTC ('coordinates.', BUFFER, BUFLEN)

         CALL CAT_PUTXT (CIOUT, 'COMMENT', BUFFER(1 : BUFLEN), STATUS)

*
*       Expression for the critical distance whcih determines whether
*       objects pair.

         BUFFER = ' '
         BUFLEN = 0

         CALL CHR_PUTC ('Critical distance expression: ', BUFFER,
     :     BUFLEN)

         IF (PDIST .NE. ' ') THEN
            LENGTH = CHR_LEN(PDIST)
            CALL CHR_PUTC (PDIST(1 : LENGTH), BUFFER, BUFLEN)
         END IF

         CALL CAT_PUTXT (CIOUT, 'COMMENT', BUFFER(1 : BUFLEN), STATUS)

*
*       Type of pairing.

         BUFFER = ' '
         BUFLEN = 0

         CALL CHR_PUTC ('Type of pairing: ', BUFFER, BUFLEN)

         IF (PRTYP .EQ. 'C') THEN
            CALL CHR_PUTC ('common', BUFFER, BUFLEN)
         ELSE IF (PRTYP .EQ. 'P') THEN
            CALL CHR_PUTC ('primary', BUFFER, BUFLEN)
         ELSE IF (PRTYP .EQ. 'M') THEN
            CALL CHR_PUTC ('mosaic', BUFFER, BUFLEN)
         ELSE IF (PRTYP .EQ. 'P') THEN
            CALL CHR_PUTC ('primrej', BUFFER, BUFLEN)
         ELSE IF (PRTYP .EQ. 'A') THEN
            CALL CHR_PUTC ('allrej', BUFFER, BUFLEN)
         ELSE
            CALL CHR_PUTC ('unkown', BUFFER, BUFLEN)
         END IF

         CALL CAT_PUTXT (CIOUT, 'COMMENT', BUFFER(1 : BUFLEN), STATUS)

*
*       Multiple matches.

         IF (MULTP) THEN
            CALL CAT_PUTXT (CIOUT, 'COMMENT',
     :        'All multiple matches in primary retained.', STATUS)
         ELSE
            CALL CAT_PUTXT (CIOUT, 'COMMENT',
     :        'Only closest multiple matches in primary retained.',
     :        STATUS)
         END IF

         IF (MULTS) THEN
            CALL CAT_PUTXT (CIOUT, 'COMMENT',
     :        'All multiple matches in secondary retained.', STATUS)
         ELSE
            CALL CAT_PUTXT (CIOUT, 'COMMENT',
     :        'Only closest multiple matches in secondary retained.',
     :        STATUS)
         END IF

*
*       Columns retained.

         IF (ALLCOL) THEN
            CALL CAT_PUTXT (CIOUT, 'COMMENT',
     :        'All columns from both catalogues retained.', STATUS)
         ELSE
            CALL CAT_PUTXT (CIOUT, 'COMMENT',
     :        'Only some columns retained.', STATUS)
         END IF

*
*       Parameters retained.

         IF (PRMPAR) THEN
            CALL CAT_PUTXT (CIOUT, 'COMMENT',
     :        'Parameters from the primary catalogue retained.', STATUS)
         ELSE
            CALL CAT_PUTXT (CIOUT, 'COMMENT',
     :        'Parameters from the primary catalogue discarded.',
     :        STATUS)
         END IF

         IF (SECPAR) THEN
            CALL CAT_PUTXT (CIOUT, 'COMMENT',
     :        'Parameters from the secondary catalogue retained.',
     :        STATUS)
         ELSE
            CALL CAT_PUTXT (CIOUT, 'COMMENT',
     :        'Parameters from the secondary catalogue discarded.',
     :        STATUS)
         END IF

         CALL CAT_PUTXT (CIOUT, 'COMMENT', ' ', STATUS)

      END IF

      END
