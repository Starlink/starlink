      BLOCKDATA SRFABD
      COMMON /SRFBLK/ LIMU(1024) ,LIML(1024) ,CL(41)     ,NCL        ,
     1                LL         ,FACT       ,IROT       ,NDRZ       ,
     2                NUPPER     ,NRSWT      ,BIGD       ,UMIN       ,
     3                UMAX       ,VMIN       ,VMAX       ,RZERO      ,
     4                IOFFP      ,NSPVAL     ,SPVAL      ,BIGEST
      COMMON /SRFIP1/ IFR        ,ISTP       ,IROTS      ,IDRX       ,
     1                IDRY       ,IDRZ       ,IUPPER     ,ISKIRT     ,
     2                NCLA       ,THETA      ,HSKIRT     ,CHI        ,
     3                CLO        ,CINC       ,ISPVAL
      COMMON /SRFINT/ ISRFMJ     ,ISRFMN     ,ISRFTX
C
C  INITIALIZATION OF INTERNAL PARAMETERS
C
      DATA ISPVAL/-999/
      DATA IFR,ISTP,IROTS,IDRX,IDRY,IDRZ,IUPPER,ISKIRT,NCLA/
     1       1,   0,    0,   1,   1,   0,     0,     0,   6/
      DATA THETA,HSKIRT,CHI,CLO,CINC/
     1       .02,    0., 0., 0.,  0./
      DATA NRSWT/0/
      DATA IOFFP,SPVAL/0,0.0/
C LINE COLOR INDEX
      DATA ISRFMJ/1/
C
C REVISION HISTORY----------
C FEBURARY 1979    ADDED REVISION HISTORY
C                  MODIFIED CODE TO CONFORM TO FORTRAN 66 STANDARD
C
C DECEMBER 1979    CHANGED LATEST REVISION DATE AND CHANGED LIBRARY
C                  STATISTICS CALL TO NSSL INSTEAD OF CRAYLIB
C
C MAY 1980         INSERTED MODS TO CORRECT THE ERROR THAT ENOUGH
C                  SPACE WAS NOT BEING ALLOWED FOR SKIRTS IN
C                  CERTAIN CASES.
C                  CHANGED THE NAMES OF ENTRY POINTS DRAW AND
C                  TRN32 TO DRAWS AND TRN32S TO BE CONSISTENT
C                  WITH USAGE IN CONJUNCTION WITH PWRZS.
C
C MARCH 1984       CONVERTED CODE TO FORTRAN 77 AND GKS.
C
C-----------------------------------------------------------------------
C
      END
