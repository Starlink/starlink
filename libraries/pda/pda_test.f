      PROGRAM TEST
C
C  f77 Test.f libpda.a -L/star/lib `ems_link`
C
C  Enter a call to any down-loaded item - application-callble routine -
C  into this main module. Don't bother with the argument list. The above
C  link command should indicate whether the library is complete, or
C  whether there are modules missing.
C
      IMPLICIT NONE
      REAL RIGNOR
      REAL PDA_SPLINT
      DOUBLE PRECISION DIGNOR
      DOUBLE PRECISION PDA_DBESJ1, PDA_DERF, PDA_PPND16, PDA_V11
C

      PRINT *,'If this message is printed the PDA library links ',
     +     'correctly.'
      GO TO 999

      CALL PDA_C2NAG()
      CALL PDA_CFFTB()
      CALL PDA_CFFTF()
      CALL PDA_CFFTI()
      CALL PDA_DC2NAG()
      CALL PDA_DCFFTB()
      CALL PDA_DCFFTF()
      CALL PDA_DCFFTI()
      CALL PDA_DNAG2C()
      CALL PDA_DNAG2R()
      CALL PDA_DNFFTB()
      CALL PDA_DNFFTF()
      CALL PDA_DR2NAG()
      CALL PDA_DRFFTB()
      CALL PDA_DRFFTF()
      CALL PDA_DRFFTI()
      CALL PDA_NAG2C()
      CALL PDA_NAG2R()
      CALL PDA_NFFTB()
      CALL PDA_NFFTF()
      CALL PDA_R2NAG()
      CALL PDA_RFFTB()
      CALL PDA_RFFTF()
      CALL PDA_RFFTI()
C
      CALL PDA_BSPDOC()
      CALL PDA_CURFIT()
      CALL PDA_DBINTK()
      CALL PDA_DBOLS()
      CALL PDA_DBVALU()
      CALL PDA_DBSQAD()
      CALL PDA_DEFC()
      CALL PDA_DGEDI()
      CALL PDA_DGEFA()
      CALL PDA_DGEFS()
      CALL PDA_DP1VLU()
      CALL PDA_DPCOEF()
      CALL PDA_DPLINT()
      CALL PDA_DPOLCF()
      CALL PDA_DPOLFT()
      CALL PDA_DPOLVL()
      CALL PDA_LMDIF()
      CALL PDA_LMDIF1()
      CALL PDA_UNCMND()
      CALL PDA_SA()
      CALL PDA_SPLDER()
      CALL PDA_SPLEV()
      CALL PDA_SUBPLX()
C
      DIGNOR = PDA_DBESJ1()
      DIGNOR = PDA_DERF()
      RIGNOR = PDA_SPLINT()
C
      CALL PDA_DB2INK()
      CALL PDA_DB2VAL()
      CALL PDA_DBKNOT()
      CALL PDA_DBTPCF()
      CALL PDA_DBVAL2()
      CALL PDA_DSQF()
      CALL PDA_IDBVIP()
      CALL PDA_IDCLDP()
      CALL PDA_IDGRID()
      CALL PDA_IDLCTN()
      CALL PDA_IDPDRV()
      CALL PDA_IDPTIP()
      CALL PDA_IDSFFT()
      CALL PDA_IDTANG()
      CALL PDA_IDXCHG()
      CALL PDA_SIDE()
      CALL PDA_SIDE2()
      CALL PDA_SPDT()
C
      DIGNOR = PDA_PPND16()
      DIGNOR = PDA_V11()
      CALL PDA_DCOV()
      CALL PDA_COVMAT()
      CALL PDA_DNLS1()
      CALL PDA_DNLS1E()
      CALL PDA_DQED()
      CALL PDA_IPERM()
      CALL PDA_LSQR()
      CALL PDA_NSCOR()
      CALL PDA_QSAD()
      CALL PDA_QSAI()
      CALL PDA_QSAR()
      CALL PDA_QSDD()
      CALL PDA_QSDI()
      CALL PDA_QSDR()
      CALL PDA_QSIAD()
      CALL PDA_QSIAI()
      CALL PDA_QSIAR()
      CALL PDA_QSIDD()
      CALL PDA_QSIDI()
      CALL PDA_QSIDR()
      CALL PDA_RINPD()
      CALL PDA_RINPI()
      CALL PDA_RINPR()
      CALL PDA_SAACD()
      CALL PDA_SAACI()
      CALL PDA_SAACR()
      CALL PDA_SAARD()
      CALL PDA_SAARI()
      CALL PDA_SAARR()
C
      CALL PDA_RAND()
      CALL PDA_RNEXP()
      CALL PDA_RNGAM()
      CALL PDA_RNNOR()
      CALL PDA_RNPOI()
      CALL PDA_RNSED()
C
      CALL PDA_BISPEV()
      CALL PDA_FPORDE()
      CALL PDA_FPRANK()
      CALL PDA_FPSURF()
      CALL PDA_FPBISP()
      CALL PDA_SURFIT()
      CALL PDA_CHE2R()
      CALL PDA_CHE2D()

 999  CONTINUE

      END
