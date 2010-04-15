      SUBROUTINE
     : CHU_FK45Z( INPUT, OUTPUT, R1950, D1950, BEPOCH, R2000, D2000,
     : STATUS )
*+
*  Name:
*     CHU_FK45Z

*  Purpose:
*     Convert FK4 coordinates to FK5 (SLALIB FK45Z)

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHU_FK45Z( INPUT, OUTPUT, R1950, D1950, BEPOCH, R2000, D2000,
*    : STATUS )

*  Description:
*     Create a catalogue containing new fields for the Right Ascension
*     and Declination after a conversion has been made from the FK4 system
*     coordinates. The new fields are calculated using SLA_FK45Z. See SUN 67
*

*  Arguments:
*     INPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the catalogue.
*     OUTPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the output catalogue.
*     R1950 = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the RA field in FK4 system.
*     D1950 = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the DEC field in FK4 system.
*     BEPOCH = REAL (Given)
*        Besselian epoch.
*     R2000 = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the RA field in FK5 system.
*     D2000 = CHARACTER * ( CHP__SZCNAME ) (Given)
*        Name of the DEC field in FK5 system.

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
      CHARACTER * ( * ) R1950
      CHARACTER * ( * ) D1950
      CHARACTER * ( * ) R2000
      CHARACTER * ( * ) D2000
      REAL BEPOCH

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
      double precision dbepoch

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

        DO FLDCOUNT = 1, NUMFLDS
          IF (NAMES(FLDCOUNT) .EQ. R1950) THEN
            R1FLG = .TRUE.
            R1FMT = FORMAT(FLDCOUNT)
          ELSEIF (NAMES(FLDCOUNT) .EQ. D1950) THEN
            D1FLG = .TRUE.
            D1FMT = FORMAT(FLDCOUNT)
          ENDIF
        ENDDO
*
*   Continue only if the fields are in the catalogue.
*
        IF (R1FLG .AND. D1FLG) THEN
*
*   Add the new fields and create an empty catalogue.
*
          NUMFLDS = NUMFLDS + 1
          R2POS = NUMFLDS
          NAMES(NUMFLDS) = R2000
          FORMAT(NUMFLDS) = 'HH MM SS'
          UNITS(NUMFLDS) = 'RADIAN'
          COMMENTS(NUMFLDS) = 'HH MM SS!       RA J2000'
*
          NUMFLDS = NUMFLDS + 1
          D2POS = NUMFLDS
          NAMES(NUMFLDS) = D2000
          FORMAT(NUMFLDS) = 'SDD MM SS'
          UNITS(NUMFLDS) = 'RADIAN'
          COMMENTS(NUMFLDS) = 'SDD MM SS!      DEC J2000'
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
              IF (OLDFNAMES(FLDCOUNT) .EQ. R1950) THEN
                R1POS = FLDCOUNT
              ELSEIF (OLDFNAMES(FLDCOUNT) .EQ. D1950) THEN
                D1POS = FLDCOUNT
              ENDIF
             ENDDO
           ENDIF
*
*  Check for nulls in all the fields to be used in the calculation
*
           IF (NULLS(R1POS) .OR.
     :         NULLS(D1POS) ) THEN
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
             CALL CHP_CONNDS(CHARVALS(R1POS), R1FMT, DBR1, STATUS)
             CALL CHP_CONNDS(CHARVALS(D1POS), D1FMT, DBD1, STATUS)
*
             DBEPOCH = BEPOCH
*
             CALL SLA_FK45Z(DBR1, DBD1, DBEPOCH, DBR2, DBD2, )
*
*    Assume that proper motions arcsec/yr and sec/yr
*    Convert to radians
*
             CALL CHP_CONDSN(DBR2, 'HH MM SS', CHARVALS(R2POS), STATUS)
             CALL CHP_CONDSN(DBD2, 'SDD MM SS', CHARVALS(D2POS), STATUS)
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
