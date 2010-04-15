      SUBROUTINE
     : CHU_PM( INPUT, OUTPUT, RAEP0, DECEP0, RAPM, DECPM, PARLX,
     : RADVEL, EP0, EP1, RAEP1, DECEP1, STATUS )
*+
*  Name:
*     CHP_PM

*  Purpose:
*     Apply proper motion correction to a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHU_PM( INPUT, OUTPUT, RAEP0, DECEP0, RAPM, DECPM, PARLX, RADVEL,
*    : EP0, EP1, RAEP1, DECEP1, STATUS )

*  Description:
*     Create a catalogue containing new fields for the new Right Ascension and
*     Declination after the correction has been made for proper motion.
*     Calculated using SLA_PM. See SUN 67.
*
*     Proper motions should be given in radians per year of epoch.
*     Parallax should be given in arcseconds.
*     Radial velocity should be given in km/sec (+ve if receeding)
*

*  Arguments:
*     INPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the catalogue.
*     OUTPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the output catalogue.
*     RAEP0 = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the RA field at epoch 0.
*     DECEP0 = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the DEC field at epoch 0.
*     RAPM = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the RA proper motion field.
*     DECPM = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the DEC proper motion field.
*     PARLX = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the parallax field.
*     RADVEL = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the radial velocity field.
*     RAEP0 = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the RA field at epoch 0.
*     DECEP0 = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the DEC field at epoch 0.
*     EP0 = REAL (Given)
*        Start Epoch.
*     EP1 = REAL (Given)
*        End epoch.

*  Anticipated Errors:
*     CHP__CATNOTFND
*     CHP__FLDNOTFND

*  Authors:
*     ARW: Alan R Wood (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-NOV-1991 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHP_PAR'          ! Standard CHP constants
      INCLUDE 'CHP_ERR'          ! Standard CHP errors

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      CHARACTER * ( * ) OUTPUT
      CHARACTER * ( * ) RAEP0
      CHARACTER * ( * ) DECEP0
      CHARACTER * ( * ) RAPM
      CHARACTER * ( * ) DECPM
      CHARACTER * ( * ) PARLX
      CHARACTER * ( * ) RADVEL
      CHARACTER * ( * ) RAEP1
      CHARACTER * ( * ) DECEP1
      REAL EP0
      REAL EP1

*  Status:
      INTEGER STATUS             ! Global status


*    Local variables :
      INTEGER ICOUNT
      INTEGER ENTCOUNT
      INTEGER NUMENTS
      INTEGER NUMFLDS
      CHARACTER * ( CHP__SZNAME ) INCAT ! Catalogue name
      CHARACTER * ( CHP__SZCNAME ) NAMES(CHP__NUMCOLS) ! Field name
      CHARACTER * ( 9 ) FREQ ! Field information required.
      CHARACTER * ( CHP__SZCCMT ) INFO ! Information required.
      CHARACTER * ( CHP__SZCFMT ) FORMAT(CHP__NUMCOLS)
      CHARACTER * ( CHP__SZCUNIT ) UNITS(CHP__NUMCOLS)
      CHARACTER * ( CHP__SZCCMT ) COMMENTS(CHP__NUMCOLS)
      LOGICAL PREFDIS(CHP__NUMCOLS)
      LOGICAL NULLS(CHP__NUMCOLS)
      CHARACTER * ( 1 ) COLTYPES(CHP__NUMCOLS)
      INTEGER COLDES(CHP__NUMCOLS)
      INTEGER ARRSHP(CHP__NUMCOLS)
      INTEGER ARRDIM( CHP__NUMCOLS, 7 )
      LOGICAL ASSERT(CHP__NUMCOLS)
      CHARACTER * ( CHP__SZEXP ) ASSEXP(CHP__NUMCOLS)
      LOGICAL DOMCHK(CHP__NUMCOLS)
      LOGICAL MDATAACC(CHP__NUMCOLS)
      LOGICAL DATAACC(CHP__NUMCOLS)
      LOGICAL VCFLAG(CHP__NUMCOLS)
      INTEGER DATELM(CHP__NUMCOLS)
      CHARACTER * ( CHP__SZEXP ) VCEXP(CHP__NUMCOLS)
      LOGICAL DELIND(CHP__NUMCOLS)
      LOGICAL NSFLAG(CHP__NUMCOLS)
      character*(CHP__szcfmt) RAFMT
      character*(CHP__szcfmt) DECFMT
      character*(CHP__szcfmt) RAPMFMT
      character*(CHP__szcfmt) DECPMFMT
      character*(CHP__szcfmt) PFMT
      character*(CHP__szcfmt) VFMT




      integer i,j
      character*(CHP__szcname) fnames(CHP__numcols)
      character*(CHP__szcname) oldfnames(CHP__numcols)
      character*(CHP__szcfmt) fformats(CHP__numcols)
      character*(CHP__szcunit) funits(CHP__numcols)
*      character*(CHP__szfnval) fnulls(CHP__numcols)
      character*(CHP__szccmt) fcomments(CHP__numcols)
      character*(1) ftypes(CHP__numcols)
      character*(CHP__szpname) pnames(CHP__numpars)
      character*(CHP__szpfmt) pformats(CHP__numpars)
      character*(CHP__szpval) pvals(CHP__numpars)
      character*(CHP__szpcmt) pcomments(CHP__numpars)
      character*(CHP__szcval) charvals(CHP__numCOLs)
      character*(1) fldtypes(CHP__numCOLs)
      integer fldcount
      integer r1pos
      integer d1pos
      integer dr1pos
      integer dd1pos
      integer p1pos
      integer v1pos
      integer r2pos
      integer d2pos
      integer dr2pos
      integer dd2pos
      integer p2pos
      integer v2pos
      integer inputnflds
      integer numparams
      integer intvals(CHP__numcols)
      integer PTRvals(CHP__numcols)
      logical raflg
      logical decflg
      logical rapmflg
      logical decpmflg
      logical pflg
      logical vflg
      logical firstpass
      logical logvals(CHP__numcols)
      real realvals(CHP__numcols)
      double precision doubvals(CHP__numcols)
      double precision dbr1
      double precision dbd1
      double precision dbdr1
      double precision dbdd1
      double precision dbp1
      double precision dbv1
      double precision dbr2
      double precision dbd2
      double precision dbdr2
      double precision dbdd2
      double precision dbp2
      double precision ddres
      double precision drres

*  Local Constants:
*
*  RADIANS to SECONDS
      REAL*8 DR2S
      PARAMETER (DR2S=0.1375098708313975703D+05)
*
*  RADIANS to ARC SECONDS
      REAL*8 DR2AS
      PARAMETER (DR2AS=0.2062648062470963560D+06)
*
*  SECONDS to RADIANS
      REAL*8 DS2R
      PARAMETER (DS2R=0.7272205216643039849D-04)
*
*  ARC SECONDS to RADIANS
      REAL*8 DAS2R
      PARAMETER (DAS2R=0.4848136811095359949D-05)
*-

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   Get all the information about the fields.
*
       call chp_gallcd(input,numflds,names,format,units,comments,
     : prefdis, coltypes, coldes,arrshp,arrdim,assert, assexp,
     : domchk,mdataacc,
     : dataacc,datelm,vcflag,vcexp,delind,nsflag,status)
*
*
      IF (STATUS .EQ. SAI__OK) THEN
*
*   Check that the fields for the calculation are present and remember
*   their position and formats.
*
        RAFLG = .FALSE.
        DECFLG = .FALSE.
        RAPMFLG = .FALSE.
        DECPMFLG = .FALSE.
        PFLG = .FALSE.
        VFLG = .FALSE.

        DO FLDCOUNT = 1, NUMFLDS
          IF (NAMES(FLDCOUNT) .EQ. RAEP0) THEN
            RAFLG = .TRUE.
            RAFMT = FORMAT(FLDCOUNT)
          ELSEIF (NAMES(FLDCOUNT) .EQ. DECEP0) THEN
            DECFLG = .TRUE.
            DECFMT = FORMAT(FLDCOUNT)
          ELSEIF (NAMES(FLDCOUNT) .EQ. RAPM) THEN
            RAPMFLG = .TRUE.
            RAPMFMT = FORMAT(FLDCOUNT)
          ELSEIF (NAMES(FLDCOUNT) .EQ. DECPM) THEN
            DECPMFLG = .TRUE.
            DECPMFMT = FORMAT(FLDCOUNT)
          ELSEIF (NAMES(FLDCOUNT) .EQ. PARLX) THEN
            PFLG = .TRUE.
            PFMT = FORMAT(FLDCOUNT)
          ELSEIF (NAMES(FLDCOUNT) .EQ. RADVEL) THEN
            VFLG = .TRUE.
            VFMT = FORMAT(FLDCOUNT)
          ENDIF
        ENDDO
*
*   Continue only if the fields are in the catalogue.
*
        IF (RAFLG .AND. DECFLG .AND. RAPMFLG .AND. DECPMFLG .AND.
     :      PFLG .AND. VFLG) THEN
*
*   Add the new fields and create an empty catalogue.
*
          NUMFLDS = NUMFLDS + 1
          R2POS = NUMFLDS
          NAMES(NUMFLDS) = RAEP1
          FORMAT(NUMFLDS) = 'HH MM SS'
          UNITS(NUMFLDS) = 'Sexag'
          COMMENTS(NUMFLDS) = 'RA at new epoch'
*
          NUMFLDS = NUMFLDS + 1
          D2POS = NUMFLDS
          NAMES(NUMFLDS) = DECEP1
          FORMAT(NUMFLDS) = 'SDD MM SS'
          UNITS(NUMFLDS) = 'Sexag'
          COMMENTS(NUMFLDS) = 'DEC at new epoch'
*
*
*    Get the information about the parameters.
*
*          CALL CHI_GETALLP(INPUT, NUMPARAMS, PNAMES, PFORMATS, PVALS,
*     :         PCOMMENTS,STATUS)
*
*    Create a catalogue with no entries.
*
          call chp_crecat(output,100, numflds, names, format,
     : units, comments, prefdis, coldes, arrshp, arrdim, assert,
     : assexp, domchk, status)
*
*   Loop reading an entry applying SLA_FK425 routine to calculate the new
*   fields and write the new entry to the output catalogue.
*
          FIRSTPASS = .TRUE.
          CALL CHP_GNENTS(INPUT, NUMENTS, STATUS)
          DO ENTCOUNT = 1, NUMENTS
            CALL CHP_GDNAC(INPUT, INPUTNFLDS, OLDFNAMES, FTYPES,
     :  COLDES, CHARVALS, DOUBVALS, INTVALS, LOGVALS, REALVALS,
     :  PTRVALS, NULLS, STATUS)
*
*   Calculate the new fields.
*
           IF (FIRSTPASS) THEN
             FIRSTPASS = .FALSE.
             DO FLDCOUNT = 1, INPUTNFLDS
              IF (OLDFNAMES(FLDCOUNT) .EQ. RAEP0) THEN
                R1POS = FLDCOUNT
              ELSEIF (OLDFNAMES(FLDCOUNT) .EQ. DECEP0) THEN
                D1POS = FLDCOUNT
              ELSEIF (OLDFNAMES(FLDCOUNT) .EQ. RAPM) THEN
                DR1POS = FLDCOUNT
              ELSEIF (OLDFNAMES(FLDCOUNT) .EQ. DECPM) THEN
                DD1POS = FLDCOUNT
              ELSEIF (OLDFNAMES(FLDCOUNT) .EQ. PARLX) THEN
                P1POS = FLDCOUNT
              ELSEIF (OLDFNAMES(FLDCOUNT) .EQ. RADVEL) THEN
                V1POS = FLDCOUNT
              ENDIF
             ENDDO
           ENDIF
*
*  Check for nulls in all the fields to be used in the calculation
*
           IF (NULLS(R1POS) .OR.
     :         NULLS(D1POS) .OR.
     :         NULLS(DR1POS) .OR.
     :         NULLS(DD1POS) .OR.
     :         NULLS(P1POS) .OR.
     :         NULLS(V1POS) ) THEN
*
              NULLS(R2POS) = .TRUE.
              NULLS(D2POS) = .TRUE.
*
           ELSE
*
*    For all passes calculate new fields
*
              NULLS(R2POS) = .FALSE.
              NULLS(D2POS) = .FALSE.
             CALL CHP_CONNDS(CHARVALS(R1POS), RAFMT, DBR1, STATUS)
             CALL CHP_CONNDS(CHARVALS(D1POS), DECFMT, DBD1, STATUS)

             IF (FTYPES(DR1POS) .EQ. 'D') THEN
               DBDR1 = DOUBVALS(DR1POS)
             ELSE
               DBDR1 = REALVALS(DR1POS)
             ENDIF
             IF (FTYPES(DD1POS) .EQ. 'D') THEN
               DBDD1 = DOUBVALS(DD1POS)
             ELSE
               DBDD1 = REALVALS(DD1POS)
             ENDIF
             IF (FTYPES(P1POS) .EQ. 'D') THEN
               DBP1 = DOUBVALS(P1POS)
             ELSE
               DBP1 = REALVALS(P1POS)
             ENDIF
             IF (FTYPES(V1POS) .EQ. 'D') THEN
               DBV1 = DOUBVALS(V1POS)
             ELSE
               DBV1 = REALVALS(V1POS)
             ENDIF
*
*
             DBDR1 = DS2R * DBDR1
             DBDD1 = DAS2R * DBDD1
             CALL SLA_FK425(DBR1, DBD1, DBDR1, DBDD1, DBP1, DBV1,
     :       EP0, EP1, DRRES, DDRES)
*
             REALVALS(R2POS) = DRRES
             REALVALS(D2POS) = DDRES
           ENDIF
*
*    Put the complete row into the table
*
          call chp_putent(output,1, numflds,names,ftypes, charvals,
     :  doubvals, intvals, logvals, realvals, ptrvals, nulls,
     :  status)
          ENDDO
*
        ELSE
          STATUS = CHP__COLNOTFND
        ENDIF
      ENDIF
*
      END
