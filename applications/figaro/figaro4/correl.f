      SUBROUTINE CORREL( STATUS )
*+
*  Name:
*     CORREL

*  Purpose:
*     Correlate two or three data sets.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CORREL( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine correlates two or three data sets. Either pair is
*     subjected to a linear fit and the third data set is subjected to a
*     two-parameter linear fit (i.e. regarded as a linear function of
*     the first and second data sets). Each data set may be an NDF
*     section. All must have the same dimensions.

*  Usage:
*     correl inlist out logfil

*  ADAM Parameters:
*     INFO = _LOGICAL (Read)
*        If false, the routine will issue only error messages and no
*        informational messages. [YES]
*     VARUSE = _LOGICAL (Read)
*        If false, input variances are ignored. [YES]
*     INLIST = LITERAL (Read)
*        The group of input NDFs. Two or three NDFs must be specified.
*        A complicated INLIST could look something like
*
*        M_51(25:35,-23.0,-24.0),M101,^LISTFILE.LIS
*
*        This example NDF group specification consists of
*        -  one identified NDF from which a subset is to be taken,
*        -  one identified NDF,
*        -  an indirection to an ASCII file containing more NDF group
*           specifications. That file may have comment lines and in-line
*           comments, which are recognised as beginning with a hash (#).
*     OUT = FILENAME (Read)
*        The ASCII output file where the data points are written into a
*        table. A new file will be opened. No file will be opened, if
*        "!" is entered.
*        The table in OUT is without any information else than the
*        values from the 1st, 2nd, 3rd data array and errors from the
*        1st, 2nd, 3rd variance array in that order. [!]
*     LOGFIL = FILENAME (Read)
*        The ASCII log file where fit results are written to. This will
*        be opened for append, if such a file exists.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13 Jul 1991 (hme):
*        Original version.
*     20 Sep 1991 (hme):
*        Avoid NDF.
*     27 Nov 1991 (hme):
*        Use of null value for OUT. Change reporting.
*     15 Dec 1991 (hme):
*        Suppress Starlink error messages arising from DSA-calls.
*     17 Jun 1992 (hme):
*        Port to NDF and Unix.
*     17 Nov 1995 (hme):
*        Switch from IRG/IRG to GRP to handle INLIST parameter.
*     2005 June 1 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'PAR_ERR'          ! Stati returned by PAR_
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'GRP_PAR'          ! Standard GRP constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL INFO
      LOGICAL VARUSE
      LOGICAL TERM               ! Temporary logical
      LOGICAL VXIST( 3 )         ! True if input have variance
      LOGICAL THREEF             ! True/false if two/three data sets
      LOGICAL OUTFIL             ! True if OUT parameter not null
      INTEGER I, J               ! Temporary integers
      INTEGER NRET               ! Number of input NDFs
      INTEGER GID                ! IRG group identifier
      INTEGER FD1, FD2           ! FIO File descriptors
      INTEGER FILNO1, FILNO2     ! Fortran unit numbers
      INTEGER PLACE              ! NDF placeholder
      INTEGER NDF( 3 )           ! NDF identifiers
      INTEGER NELM               ! NDF size
      INTEGER NDIM( 3 )          ! NDF dimensionalities
      INTEGER DIM( NDF__MXDIM, 3 ) ! NDF dimensions
      INTEGER DPTR( 3 )          ! Data pointers
      INTEGER VPTR( 3 )          ! Variance pointers
      CHARACTER * ( 64 ) IN( 3 ) ! Names of the input NDF sections
      CHARACTER * ( 255 ) NDFNAM( 1 ) ! Name of current NDF
      INTEGER NS
      REAL A0, A1, A2            ! Fit parameters
      REAL DA0, DA1, DA2         ! Errors of fit parameters
      REAL R                     ! Regression coefficient
      REAL SIGMA                 ! Estimated variance
      REAL CHISQR                ! Normalised chi-squared

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup.
      CALL NDF_BEGIN
      CALL PAR_GET0L(   'INFO',   INFO, STATUS )
      CALL PAR_GET0L( 'VARUSE', VARUSE, STATUS )

*  Get the INLIST of NDFs.
      CALL GRP_NEW( 'InputNDFs', GID, STATUS )
      CALL GRP_GROUP( 'INLIST', GRP__NOID, GID,
     :   NRET, I, TERM, STATUS )

*  Work out if the third file name is missing.
      IF ( NRET .LT. 2 .OR. NRET .GT. 3 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CORREL_E01',
     :      'CORREL: Invalid number of input files.', STATUS )
         GO TO 500
      ELSE IF ( NRET .EQ. 3 ) THEN
         THREEF = .TRUE.
      ELSE
         THREEF   = .FALSE.
         VXIST(3) = .FALSE.
      END IF

*  Open the ASCII table output file.
      CALL FIO_ASSOC( 'OUT', 'WRITE', 'LIST', 0, FD1, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         OUTFIL = .FALSE.
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         OUTFIL = .FALSE.
         GO TO 500
      ELSE
         OUTFIL = .TRUE.
         CALL FIO_UNIT( FD1, FILNO1, STATUS )
      END IF

*  Open the result logfile.
      CALL FIO_ASSOC( 'LOGFIL', 'APPEND', 'LIST', 0, FD2, STATUS )
      CALL FIO_UNIT( FD2, FILNO2, STATUS )

*  Access the input NDFs. Find out shapes. Map data and variances.
      DO 1 I = 1, NRET
         CALL GRP_GET( GID, I, 1, NDFNAM, STATUS )
         CALL NDF_OPEN( DAT__ROOT, NDFNAM, 'READ', 'OLD',
     :      NDF(I), PLACE, STATUS )
         CALL NDF_MSG(  'CORREL_T01', NDF(I) )
         CALL MSG_LOAD( 'CORREL_M01', '^CORREL_T01', IN(I), J, STATUS )
         CALL NDF_DIM( NDF(I), NDF__MXDIM, DIM(1,I), NDIM(I), STATUS )
         CALL NDF_MAP( NDF(I), 'DATA', '_REAL', 'READ',
     :      DPTR(I), NELM, STATUS )
         VXIST(I) = VARUSE
         IF ( VARUSE )
     :      CALL NDF_STATE( NDF(I), 'VARIANCE', VXIST(I), STATUS )
         VXIST(I) = VXIST(I) .AND. VARUSE
         IF ( VXIST(I) )
     :      CALL NDF_MAP( NDF(I), 'VARIANCE', '_REAL', 'READ',
     :      VPTR(I), NELM, STATUS )
 1    CONTINUE
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Do NDF shapes match?
      IF ( ( NDIM(2) .NE. NDIM(1) ) .OR.
     :     ( THREEF .AND. NDIM(3) .NE. NDIM(1) ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CORREL_E02',
     :      'CORREL: Input NDFs do not match in dimensionality.',
     :      STATUS )
         GO TO 500
      END IF
      DO 7 I = 1, NDIM(1)
         IF ( ( DIM(I,2) .NE. DIM(I,1) ) .OR.
     :        ( THREEF .AND. DIM(I,3) .NE. DIM(I,1) ) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CORREL_E03',
     :         'CORREL: Input NDFs do not match in size.', STATUS )
            GO TO 500
         END IF
 7    CONTINUE

*  Correlate 1->2, 2->1.
      CALL SPD_WAAF( VXIST(2), .TRUE., NELM, %VAL( CNF_PVAL(DPTR(1)) ),
     :               %VAL( CNF_PVAL(DPTR(2)) ),
     :               %VAL( CNF_PVAL(VPTR(2)) ), NS, A0, A1, R,
     :               DA0, DA1, SIGMA, CHISQR, STATUS )
      CALL SPD_WAAG( FILNO2, INFO, IN(1), IN(2), NS, A0, A1, R,
     :               DA0, DA1, SIGMA, CHISQR, STATUS )
      CALL SPD_WAAF( VXIST(1), .TRUE., NELM, %VAL( CNF_PVAL(DPTR(2)) ),
     :               %VAL( CNF_PVAL(DPTR(1)) ),
     :               %VAL( CNF_PVAL(VPTR(1)) ), NS, A0, A1, R, DA0,
     :               DA1, SIGMA, CHISQR, STATUS )
      CALL SPD_WAAG( FILNO2, INFO, IN(2), IN(1), NS, A0, A1, R,
     :               DA0, DA1, SIGMA, CHISQR, STATUS )

*  If there is a third subset.
      IF ( THREEF ) THEN

*     Correlate 1->3, 3->1.
         CALL SPD_WAAF( VXIST(3), .TRUE., NELM,
     :                  %VAL( CNF_PVAL(DPTR(1)) ),
     :                  %VAL( CNF_PVAL(DPTR(3)) ),
     :                  %VAL( CNF_PVAL(VPTR(3)) ), NS, A0, A1, R,
     :                  DA0, DA1, SIGMA, CHISQR, STATUS )
         CALL SPD_WAAG( FILNO2, INFO, IN(1), IN(3), NS, A0, A1, R,
     :                  DA0, DA1, SIGMA, CHISQR, STATUS )
         CALL SPD_WAAF( VXIST(1), .TRUE., NELM,
     :                  %VAL( CNF_PVAL(DPTR(3)) ),
     :                  %VAL( CNF_PVAL(DPTR(1)) ),
     :                  %VAL( CNF_PVAL(VPTR(1)) ), NS, A0, A1, R,
     :                  DA0, DA1, SIGMA, CHISQR, STATUS )
         CALL SPD_WAAG( FILNO2, INFO, IN(3), IN(1), NS, A0, A1, R,
     :                  DA0, DA1, SIGMA, CHISQR, STATUS )

*     Correlate 2->3, 3->2.
         CALL SPD_WAAF( VXIST(3), .TRUE., NELM,
     :                  %VAL( CNF_PVAL(DPTR(2)) ),
     :                  %VAL( CNF_PVAL(DPTR(3)) ),
     :                  %VAL( CNF_PVAL(VPTR(3)) ), NS, A0, A1, R,
     :                  DA0, DA1, SIGMA, CHISQR, STATUS )
         CALL SPD_WAAG( FILNO2, INFO, IN(2), IN(3), NS, A0, A1, R,
     :                  DA0, DA1, SIGMA, CHISQR, STATUS )
         CALL SPD_WAAF( VXIST(2), .TRUE., NELM,
     :                  %VAL( CNF_PVAL(DPTR(3)) ),
     :                  %VAL( CNF_PVAL(DPTR(2)) ),
     :                  %VAL( CNF_PVAL(VPTR(2)) ), NS, A0, A1, R,
     :                  DA0, DA1, SIGMA, CHISQR, STATUS )
         CALL SPD_WAAG( FILNO2, INFO, IN(3), IN(2), NS, A0, A1, R,
     :                  DA0, DA1, SIGMA, CHISQR, STATUS )

*     Two-parameter linear fit (1,2)->3.
         CALL SPD_WAAH( VXIST(3), .TRUE., NELM,
     :                  %VAL( CNF_PVAL(DPTR(1)) ),
     :                  %VAL( CNF_PVAL(DPTR(2)) ),
     :                  %VAL( CNF_PVAL(DPTR(3)) ),
     :                  %VAL( CNF_PVAL(VPTR(3)) ), NS, A0, A1, A2,
     :                  DA0, DA1, DA2, SIGMA, CHISQR, STATUS )
         CALL SPD_WAAJ( FILNO2, INFO, IN(1), IN(2), IN(3), NS, A0,
     :                  A1, A2, DA0, DA1, DA2, SIGMA, CHISQR, STATUS )
      END IF

*  Write values 1,2,3 to logfile.
      IF ( OUTFIL )
     :   CALL SPD_WZUA( FILNO1, THREEF, VXIST, NELM,
     :                  %VAL( CNF_PVAL(DPTR(1)) ),
     :                  %VAL( CNF_PVAL(DPTR(2)) ),
     :                  %VAL( CNF_PVAL(DPTR(3)) ),
     :                  %VAL( CNF_PVAL(VPTR(1)) ),
     :                  %VAL( CNF_PVAL(VPTR(2)) ),
     :                  %VAL( CNF_PVAL(VPTR(3)) ), STATUS )

*  Tidy up.
 500  CONTINUE
      DO 8 I = 1, NRET
         CALL NDF_ANNUL( NDF(I), STATUS )
 8    CONTINUE
      CALL NDF_END( STATUS )
      CALL GRP_DELET( GID, STATUS )
      IF ( OUTFIL ) CALL FIO_CANCL( 'OUT', STATUS )
      CALL FIO_CANCL( 'LOGFIL', STATUS )
      CALL FIO_DEACT( STATUS )

      END
