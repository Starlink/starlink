      SUBROUTINE
     : CHU_FK425( INPUT, OUTPUT, RAFK4, DECFK4, RAPMFK4, DECPMFK4,
     : PARLXFK4, RADVELFK4, RAFK5, DECFK5, RAPMFK5, DECPMFK5, PARLXFK5,
     : RADVELFK5, STATUS )
*+
*  Name:
*     CHU_FK425

*  Purpose:
*     Convert FK4 coordinates to FK5

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHU_FK425( INPUT, OUTPUT, RAFK4, DECFK4, RAPMFK4, DECPMFK4,
*     PARLXFK4, RADVELFK4, RAFK5, DECFK5, RAPMFK5, DECPMFK5, PARLXFK5,
*     RADVELFK5, STATUS )

*  Description:
*     Create a catalogue containing new fields for the Right Ascension,
*     Declination, Parallax, Radial velocity and proper motions after a
*     conversion has been made from the FK4 system coordinates. The
*     new fields are calculated using SLA_FK425. See SUN 67
*
*     Conversion from Besselian epoch 1950.0 to Julian epoch 2000.0 only
*     is provided.
*
*     Proper motions should be given in sec/yr and arcsecs/yr
*     Parallax should be given in arcseconds.
*     Radial velocity should be given in km/sec (+ve if receeding)

*  Arguments:
*     INPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the catalogue.
*     OUTPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the output catalogue.
*     RAFK4 = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the RA field in FK4 system.
*     DECFK4 = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the DEC field in FK4 system.
*     RAPMFK4 = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the RA proper motion field in FK4 system.
*     DECPMFK4 = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the DEC proper motion field in FK4 system.
*     PARLXFK4 = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the parallax field in FK4 system.
*     RADVELFK4 = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the radial velocity field in FK4 system.
*     RAFK5 = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the RA field in FK5 system.
*     DECFK5 = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the DEC field in FK5 system.
*     RAPMFK5 = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the RA proper motion field in FK5 system.
*     DECPMFK5 = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the DEC proper motion field in FK5 system.
*     PARLXFK5 = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the parallax field in FK5 system.
*     RADVELFK5 = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the radial velocity field in FK5 system.

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
*     None

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHP_PAR'          ! Standard CHP constants
      INCLUDE 'CHP_ERR'          ! Standard CHP errors

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      CHARACTER * ( * ) OUTPUT
      CHARACTER * ( * ) RAFK4
      CHARACTER * ( * ) DECFK4
      CHARACTER * ( * ) RAPMFK4
      CHARACTER * ( * ) DECPMFK4
      CHARACTER * ( * ) PARLXFK4
      CHARACTER * ( * ) RADVELFK4
      CHARACTER * ( * ) RAFK5
      CHARACTER * ( * ) DECFK5
      CHARACTER * ( * ) RAPMFK5
      CHARACTER * ( * ) DECPMFK5
      CHARACTER * ( * ) PARLXFK5
      CHARACTER * ( * ) RADVELFK5

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
      character*(CHP__szcfmt) R1FMT
      character*(CHP__szcfmt) D1FMT
      character*(CHP__szcfmt) DR1FMT
      character*(CHP__szcfmt) DD1FMT
      character*(CHP__szcfmt) P1FMT
      character*(CHP__szcfmt) V1FMT




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
      logical r1flg
      logical d1flg
      logical dr1flg
      logical dd1flg
      logical p1flg
      logical v1flg
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
      double precision dbv2

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
        R1FLG = .FALSE.
        D1FLG = .FALSE.
        DR1FLG = .FALSE.
        DD1FLG = .FALSE.
        P1FLG = .FALSE.
        V1FLG = .FALSE.

        DO FLDCOUNT = 1, NUMFLDS
          IF (NAMES(FLDCOUNT) .EQ. RAFK4) THEN
            R1FLG = .TRUE.
            R1FMT = FORMAT(FLDCOUNT)
          ELSEIF (NAMES(FLDCOUNT) .EQ. DECFK4) THEN
            D1FLG = .TRUE.
            D1FMT = FORMAT(FLDCOUNT)
          ELSEIF (NAMES(FLDCOUNT) .EQ. RAPMFK4) THEN
            DR1FLG = .TRUE.
            DR1FMT = FORMAT(FLDCOUNT)
          ELSEIF (NAMES(FLDCOUNT) .EQ. DECPMFK4) THEN
            DD1FLG = .TRUE.
            DD1FMT = FORMAT(FLDCOUNT)
          ELSEIF (NAMES(FLDCOUNT) .EQ. PARLXFK4) THEN
            P1FLG = .TRUE.
            P1FMT = FORMAT(FLDCOUNT)
          ELSEIF (NAMES(FLDCOUNT) .EQ. RADVELFK4) THEN
            V1FLG = .TRUE.
            V1FMT = FORMAT(FLDCOUNT)
          ENDIF
        ENDDO
*
*   Continue only if the fields are in the catalogue.
*
        IF (R1FLG .AND. D1FLG .AND. DR1FLG .AND. DD1FLG .AND.
     :      P1FLG .AND. V1FLG) THEN
*
*   Add the new fields and create an empty catalogue.
*
          NUMFLDS = NUMFLDS + 1
          R2POS = NUMFLDS
          NAMES(NUMFLDS) = RAFK5
          FORMAT(NUMFLDS) = 'HH MM SS'
          UNITS(NUMFLDS) = 'RADIAN'
          COMMENTS(NUMFLDS) = 'HH MM SS!       RA J2000'
*
          NUMFLDS = NUMFLDS + 1
          D2POS = NUMFLDS
          NAMES(NUMFLDS) = DECFK5
          FORMAT(NUMFLDS) = 'SDD MM SS'
          UNITS(NUMFLDS) = 'RADIAN'
          COMMENTS(NUMFLDS) = 'SDD MM SS!      DEC J2000'
*
          NUMFLDS = NUMFLDS + 1
          DR2POS = NUMFLDS
          NAMES(NUMFLDS) = RAPMFK5
          FORMAT(NUMFLDS) = 'F10.7'
          UNITS(NUMFLDS) = 'SEC/YR'
          COMMENTS(NUMFLDS) = '               RA proper motion J2000'
*
          NUMFLDS = NUMFLDS + 1
          DD2POS = NUMFLDS
          NAMES(NUMFLDS) = DECPMFK5
          FORMAT(NUMFLDS) = 'F10.7'
          UNITS(NUMFLDS) = 'ARCSEC/YR'
          COMMENTS(NUMFLDS) = '                DEC proper motion J2000'
*
          NUMFLDS = NUMFLDS + 1
          P2POS = NUMFLDS
          NAMES(NUMFLDS) = PARLXFK5
          FORMAT(NUMFLDS) = 'F10.7'
          UNITS(NUMFLDS) = 'ARCSEC'
          COMMENTS(NUMFLDS) = '                Parallax'
*
          NUMFLDS = NUMFLDS + 1
          V2POS = NUMFLDS
          NAMES(NUMFLDS) = RADVELFK5
          FORMAT(NUMFLDS) = 'F10.7'
          UNITS(NUMFLDS) = 'KM/S'
          COMMENTS(NUMFLDS) = '                 Radial velocity'
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
              IF (OLDFNAMES(FLDCOUNT) .EQ. RAFK4) THEN
                R1POS = FLDCOUNT
              ELSEIF (OLDFNAMES(FLDCOUNT) .EQ. DECFK4) THEN
                D1POS = FLDCOUNT
              ELSEIF (OLDFNAMES(FLDCOUNT) .EQ. RAPMFK4) THEN
                DR1POS = FLDCOUNT
              ELSEIF (OLDFNAMES(FLDCOUNT) .EQ. DECPMFK4) THEN
                DD1POS = FLDCOUNT
              ELSEIF (OLDFNAMES(FLDCOUNT) .EQ. PARLXFK4) THEN
                P1POS = FLDCOUNT
              ELSEIF (OLDFNAMES(FLDCOUNT) .EQ. RADVELFK4) THEN
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
              NULLS(DR2POS) = .TRUE.
              NULLS(DD2POS) = .TRUE.
              NULLS(P2POS) = .TRUE.
              NULLS(V2POS) = .TRUE.
*
           ELSE
*
*    For all passes calculate new fields
*
              NULLS(R2POS) = .FALSE.
              NULLS(D2POS) = .FALSE.
              NULLS(DR2POS) = .FALSE.
              NULLS(DD2POS) = .FALSE.
              NULLS(P2POS) = .FALSE.
              NULLS(V2POS) = .FALSE.
             CALL CHP_CONNDS(CHARVALS(R1POS), R1FMT, DBR1, STATUS)
             CALL CHP_CONNDS(CHARVALS(D1POS), D1FMT, DBD1, STATUS)

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
     :       DBR2, DBD2, DBDR2, DBDD2, DBP2, DBV2)
*
*    Assume that proper motions arcsec/yr and sec/yr
*    Convert to radians
*
             CALL CHP_CONDSN(DBR2, 'HH MM SS', CHARVALS(R2POS), STATUS)
             CALL CHP_CONDSN(DBD2, 'SDD MM SS', CHARVALS(D2POS), STATUS)
             REALVALS(DR2POS) = DBDR2 * DR2S
             REALVALS(DD2POS) = DBDD2 * DR2AS
             REALVALS(P2POS) = DBP2
             REALVALS(V2POS) = DBV2
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
