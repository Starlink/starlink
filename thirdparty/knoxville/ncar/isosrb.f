      BLOCKDATA ISOSRB
C
C     BLOCK DATA
C
      SAVE
      COMMON /ISOSR2/ LX         ,NX         ,NY         ,ISCR(8,128),
     1                ISCA(8,128)
      COMMON /ISOSR4/ RX         ,RY
      COMMON /ISOSR5/ NBPW       ,MASK(16)   ,GENDON
      LOGICAL         GENDON
      COMMON /ISOSR6/ IX         ,IY         ,IDX        ,IDY        ,
     1                IS         ,ISS        ,NP         ,CV         ,
     2                INX(8)     ,INY(8)     ,IR(500)    ,NR
      COMMON /ISOSR7/ IENTRY     ,IONES
      COMMON /ISOSR8/ NMASK(16)  ,IXOLD      ,IYOLD      ,IBTOLD     ,
     1                HBFLAG     ,IOSLSN     ,LRLX       ,IFSX       ,
     2                IFSY       ,FIRST      ,IYDIR      ,IHX        ,
     3                IHB        ,IHS        ,IHV        ,IVOLD      ,
     4                IVAL       ,IHRX       ,YCHANG     ,ITPD       ,
     5                IHF
      COMMON /ISOSR9/ BIG        ,IXBIT
      COMMON /TEMPRD/ RZERO
      LOGICAL         YCHANG     ,HBFLAG     ,FIRST      ,IHF
C
      DATA LX,NX,NY/8,128,128/
      DATA INX(1),INX(2),INX(3),INX(4),INX(5),INX(6),INX(7),INX(8)/
     1        -1 ,   -1 ,    0 ,    1 ,    1 ,    1 ,    0 ,   -1 /
      DATA INY(1),INY(2),INY(3),INY(4),INY(5),INY(6),INY(7),INY(8)/
     1         0 ,    1 ,    1 ,    1 ,    0 ,   -1 ,   -1 ,   -1 /
      DATA NR/500/
      DATA NBPW/16/
      DATA IHF/.FALSE./
C
      DATA GENDON /.FALSE./
      DATA RZERO/0./
C
C
C RX = (NX-1)/SCREEN WIDTH FROM TRN32I
C RY = (NY-1)/SCREEN HEIGHT FROM TRN32I
C
      DATA RX,RY/.00389,.00389/
C
      END
