      SUBROUTINE OUTPUT( STATUS )
*
*   Arguments:
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  History:
*     Sometime (UNK):
*        Original version.
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CONV calls
*     3-MAR-1993 (AJJB):
*        STATUS argument added to GETDEFLT call
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     11-MAR-1993 (AJJB):
*        Changed I, and JSIGN (used as 4th argument in calls to
*        CONV) to type Character, as CONV has been changed.


      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INCLUDE 'MAIN'

      PARAMETER(NBITS=8*IDBYTE)
      DOUBLE PRECISION TWOPI,HALFPI,RDSA,RDST,RDDG,RAO,DECO
*  Status:
      INTEGER STATUS             ! Global status

      INTEGER DMNO,DMZ,DMN,ARR(NBITS),PAGEL
      LOGICAL*1 IDLIST(IDBYTE),PRIVATE
      LOGICAL SET,NOSCAL
      CHARACTER*1 ISIGN,SUPPCH(20),DMSIGN,DMSUPP,HDSUPP, I, JSIGN
      CHARACTER*2 SPEC
      CHARACTER*3 DMC(4),DM
      CHARACTER*5 CATNAM
      CHARACTER*6 HDCHAR,MVCHAR,MPCHAR,MGCHAR,DIAMCH
      CHARACTER*4 IDNAM(80), CHTEMP
      CHARACTER*70 PARAMS(25),VALUE*50

      COMMON/CONVF/TWOPI,HALFPI,RDSA,RDST,RDDG

      DATA IDNAM/'HD','AGK','HZ','CPC','YZ','CCFS','Boss',' ','SAO'
     : ,'ADS','IDS','GCRV','YBS','N30','FK4','JSK','A+B','KDY',
     : 'uvby','Bay','USNP','GCTP','GCVS','UBV','NGC','IC','CLA','CLB'
     : ,'LS','IRC','Cel','GEN','U+F','SB','BE*','GL','UM','FE'
     : ,'RY','JP11','HGAM','AMAS','MSS','S+J',12*' ',
     : 'DM2','YZO','MMAG',21*' '/
      DATA DMC/' BD','CoD','CPD',' **'/
      DATA SUPPCH/' ','a','b','c','d','e','f','g','h','i','j',
     : 'k','l','m','n','o','p','q','r','s'/

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*+
*   Now Print the Co-ords
*   They are Derived from the Projected
*   X,Y of the Stars
*   for Position and Field Centre Precessed Date
*   Co-ordinates are in Mins. of Arc
*
*
*   Write out Column Headings
*
*   Initialise Line Count
*-
*
*   Get the type of catalogue being used. If it is a PRIvate astrometric
*   catalogue then we set PRIVATE to .TRUE. and deal with the catalogue
*   numbers differently
*
      CALL GETPARAMS(PARAMS,NVAL, STATUS )
      CALL GETDEFLT(PARAMS,NVAL,'MODE',VALUE,NPOS, STATUS )
      PRIVATE = .FALSE.
      IF(VALUE(1:3).EQ.'PRI') PRIVATE = .TRUE.
*
*   Find out the length of a page
*
      PAGEL = 38

      LNUM = 24
      IF (SUPP) LNUM = LNUM+9+NUMSUPP
      IF (CATRUN) THEN
         WRITE (7,906) NUM
      ELSE IF (.NOT.NONS) THEN
         WRITE (7,905) NUM
         IF (CHOOSE) THEN
            IF (YESCAT(1).NE.0) THEN
               WRITE (7,902)
     :         (IDNAM(YESCAT(K))(:LENG(IDNAM(YESCAT(K)))),K=1,NCH)
            ELSE
               WRITE (7,903)
     :         (IDNAM(NOCAT(K))(:LENG(IDNAM(NOCAT(K)))),K=1,NCH)
            ENDIF
         ENDIF
      ELSE
         WRITE (7,907) NUM
      ENDIF
902   FORMAT(8X,'Stars Selected are ONLY those in ',40(A:' and '))
903   FORMAT(8X,'Stars Selected are ONLY those not in ',40(A:' or '))
905   FORMAT(//8X,'Number of CSI Catalogue stars found =',I4)
906   FORMAT(//8X,'Number of Astrometric catalogue stars found =',I4//)
907   FORMAT(//8X,'Number of Nonstellar Objects Found =',I4//)
      CALL PAGE(0,CATRUN,NONS,IDENTS,EQUOUT, STATUS )
      NOSCAL = SCALE.LT.1E-6
      LPAGE = 1
      JBLOCK = 0
      DO 200 J = 1,NUM
      LNUM=LNUM+1
      JBLOCK = JBLOCK + 1
      IF (LNUM.GT.PAGEL) THEN
         LPAGE=LPAGE + 1
         LNUM = 1
         JBLOCK = 1
         CALL PAGE(LPAGE,CATRUN,NONS,IDENTS,EQUOUT, STATUS )
      ENDIF
      IF (JBLOCK.GT.5) THEN
         WRITE (7,900)
900   FORMAT(' ')
         JBLOCK = 1
      ENDIF
      RAO = STAR(1,IP(J))
      DECO = STAR(2,IP(J))
      SPEC = CSTAR(IP(J))
*
*   Compute Projected X,Y
*
      CALL PROJ(1,RAO,DECO,X,Y, STATUS )
      XC = REAL( X/RDSA )
      YC = REAL( Y/RDSA )
      IXC = NINT(XC)
      IYC = NINT(YC)
      IF (.NOT.NOSCAL) THEN
         XM = -(XC/SCALE)
         YM = YC/SCALE
      ENDIF
*
*   'CATRUN' Type Output
*    Print the Catalogue
*    Number and Magnitude of a Star
*    and its' RA & Dec.
*
      IF (CATRUN) THEN

*   Convert Precessed Positions

         CALL CONV(2,RAO,4,I,MHAO,MINSAO,N,SECSAO, STATUS )
         CALL CONV(1,DECO,3,JSIGN,MDEGD,MINSD,N,SECSD, STATUS )
         RMAG = FLOAT(NSTAR(1,IP(J)))/10.0
*
*   Pick up the proper motion for this star
*
         PMRA=PM(2,IP(J))
         PMDE=PM(1,IP(J))
         NCAT = NSTAR(3,IP(J))
*
*   Check if SAO or AGK3 CAT. No.
*
         IF (PRIVATE) THEN
               CATNAM = 'PRI  '
         ELSEIF (NCAT.LT.2000000) THEN
*
*   Is AGK3, so Separate Band No. and No. Within
*   that Band
             NCAT = NCAT - 1000000
             NS = MOD(IABS(NCAT),10000)
             NBAND = ABS(NCAT/10000)
             IF (NCAT.LT.0) THEN
                ISIGN = '-'
             ELSE
                ISIGN = '+'
             ENDIF
             IF (NOSCAL) THEN
                WRITE (7,921) J,ISIGN,NBAND,NS,RMAG,SPEC,MHAO,MINSAO,
     :       SECSAO,JSIGN,MDEGD,MINSD,SECSD,XC,YC,PMRA,PMDE
             ELSE
                WRITE (7,920) J,ISIGN,NBAND,NS,RMAG,SPEC,MHAO,MINSAO,
     :       SECSAO,JSIGN,MDEGD,MINSD,SECSD,XC,YC,XM,YM,PMRA,PMDE
             ENDIF
920      FORMAT(' ',I4,5H AGK3,1X,A1,I2,1X,I4,F8.1,3X,A2,3X,I5,I3,F7.3,
     :    2X,A1,I2,I3,F6.2,3X,2F9.1,2F8.1,f10.4,F9.3)
921      FORMAT(' ',I4,5H AGK3,1X,A1,I2,1X,I4,F8.1,3X,A2,3X,I5,I3,F7.3,
     :    2X,A1,I2,I3,F6.2,3X,2F9.1,f10.4,F9.3)
         ELSEIF (NCAT.GE.3000000) THEN
            CATNAM = 'PERTH'
            NCAT = MOD(NCAT,1000000)
            IF (NOSCAL) THEN
            WRITE(7,901) J,CATNAM,NCAT,RMAG,SPEC,MHAO,MINSAO,SECSAO,
     :       JSIGN,MDEGD,MINSD,SECSD,XC,YC,PMRA,PMDE
            ELSE
            WRITE(7,922) J,CATNAM,NCAT,RMAG,SPEC,MHAO,MINSAO,SECSAO,
     :       JSIGN,MDEGD,MINSD,SECSD,XC,YC,XM,YM,PMRA,PMDE
            ENDIF
         ELSE
            CATNAM = 'SAO  '
*
*    Modified KFH 12/5/82 to correct no-magnitude in SAO catalogue
*
            IF (ABS(RMAG-15.7).LE.0.05) RMAG=999.9
            NCAT = MOD(NCAT,1000000)
            IF (NOSCAL) THEN
            WRITE(7,901) J,CATNAM,NCAT,RMAG,SPEC,MHAO,MINSAO,SECSAO,
     :       JSIGN,MDEGD,MINSD,SECSD,XC,YC,PMRA,PMDE
            ELSE
            WRITE(7,922) J,CATNAM,NCAT,RMAG,SPEC,MHAO,MINSAO,SECSAO,
     :       JSIGN,MDEGD,MINSD,SECSD,XC,YC,XM,YM,PMRA,PMDE
            ENDIF
         ENDIF
901      FORMAT(' ',I4,1X,A5,I7,2X,F7.1,3X,A2,3X,I5,I3,F7.3,
     :    2X,A1,I2,I3,F6.2,3X,2F9.1,2F8.1,F10.4,F9.3)
922      FORMAT(' ',I4,1X,A5,I7,2X,F7.1,3X,A2,3X,I5,I3,F7.3,
     :    2X,A1,I2,I3,F6.2,3X,2F9.1,2F8.1,F10.4,f9.3)
      ELSE IF (.NOT.NONS) THEN
*
*   Normal Type Output
*
*   Convert Precessed Positions
*
         CALL CONV(2,RAO,2,I,MHAO,MINSAO,MRS,RSECS, STATUS )
         CALL CONV(1,DECO,0,JSIGN,MDEGD,MINSD,MDS,X, STATUS )
         MAGV = NSTAR(1,IP(J))
         MAGP = NSTAR(2,IP(J))
         NHD = NSTAR(3,IP(J))
         DMNO = NSTAR(4,IP(J))
         HDSUPP = SUPPCH(MOD(NHD,10) + 1)
         NHD = NHD/10
*
*   Decode the Durchmusterung Zone,Number etc.
*
         IDMNO = ABS(DMNO)
         K = IDMNO/100000000
         DM = DMC(K)
         DMZ = (IDMNO - (K*100000000))/1000000
         DMN = (IDMNO - (K*100000000) - (DMZ*1000000))/20
         DMSUPP = SUPPCH(MOD(IDMNO,20) + 1)
         IF (DMNO.LT.0) THEN
            DMSIGN = '-'
         ELSE
            DMSIGN = '+'
         ENDIF
*
*   Convert to Chars.,Blank if Zero
*   Bracket to Indicate approximate magnitude (B.61-62 SET)
*
         WRITE (HDCHAR,930) NHD
930      FORMAT(I6)
         IF (NHD.EQ.0) HDCHAR = ' '
         RMAGV=MAGV/10.0
         RMAGP=MAGP/10.0
         IF (MAGV.EQ.9999)  THEN
            MVCHAR = ' '
         ELSE
            WRITE (MVCHAR,932) RMAGV
            CHTEMP = MVCHAR(2:5)
            IF (SET(ID(IDBYTE,IP(J)),5)) MVCHAR='(' // CHTEMP // ')'
         ENDIF
         IF (MAGP.EQ.9999) THEN
             MPCHAR = ' '
         ELSE
            WRITE (MPCHAR,932) RMAGP
            CHTEMP = MPCHAR(2:5)
            IF (SET(ID(IDBYTE,IP(J)),4)) MPCHAR='('// CHTEMP //')'
         ENDIF
932      FORMAT(1X,F4.1,1X)
*
*   Get the Star IDS
*
         IF (IDENTS) THEN
            DO K=1,IDBYTE
               IDLIST(K)=ID(K,IP(J))
            ENDDO
            CALL IDENT(IDLIST,ARR,NUMID, STATUS )
            LNUM = LNUM + (NUMID-9)/9
         ELSE
            NUMID = 0
         ENDIF
*
*   Fix for Probable Catalogue Bug : ASCII 0'S Occuring
*
         N1SPEC=ICHAR(SPEC(1:1))
         N2SPEC=ICHAR(SPEC(2:2))
         IF (N1SPEC.EQ.0.OR.N2SPEC.EQ.0) SPEC = '  '
*   Now Write out the Record
         WRITE (7,990) J,HDCHAR,HDSUPP,DM,DMSIGN,DMZ,DMN,DMSUPP,SPEC,
     :    MVCHAR,MPCHAR,MHAO,MINSAO,RSECS,JSIGN,MDEGD,MINSD,MDS,IXC,
     :    IYC,(IDNAM(ARR(K))(1:LENG(IDNAM(ARR(K)))),K=1,NUMID)
990      FORMAT(1X,I4,1X,A6,A1,1X,A3,1X,A1,I2,I6,A1,2X,A2,1X,2A6,
     :    4X,I2,I3,1X,F4.1,3X,A1,I2,2I3,2X,2I7,4X,9(A:',')/(
     :    91X,9(A:',')))
       ELSE
*
*    Nonstellar Objects
*
         CALL CONV(2,RAO,2,I,MHAO,MINSAO,MRS,RSECS, STATUS )
         CALL CONV(1,DECO,0,JSIGN,MDEGD,MINSD,MDS,X, STATUS )
         RMAG = NSTAR(1,IP(J))/10.0
         IF (NSTAR(1,IP(J)).EQ.0) THEN
            MGCHAR ='      '
         ELSE
            WRITE (MGCHAR,934) RMAG
         ENDIF
         IF (DIAM(IP(J)).EQ.0) THEN
            DIAMCH = '      '
         ELSE
            WRITE (DIAMCH,936) DIAM(IP(J))
         ENDIF
934      FORMAT(F6.1)
936      FORMAT(I6)
         WRITE (7,995) J,NAME(IP(J)),MHAO,MINSAO,RSECS,JSIGN,
     :    MDEGD,MINSD,MDS,MGCHAR,DIAMCH,DESCR(IP(J)),IXC,IYC
995      FORMAT(1X,I4,1X,A15,2I3,F5.1,2X,A1,I2,2I3,2A6,2X,A24,
     :    2(2X,I6))
      ENDIF
200   CONTINUE
      END

