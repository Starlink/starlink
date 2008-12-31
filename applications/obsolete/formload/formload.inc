CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FORMLOAD.INC --  Include file for FORMLOAD
C
C  alan penny           RAL              1992 Nov

C  MAXPAG (=100) Maximum no of pages that can be processed
C  MAXQUE (=1000) Maximum no of questions that can be processed
C  MAXQSP (=500) Maximum no of special questions that can be processed
C  MAXCSP (=20)  Maximum no of codes per special question
C  MAXFOR (=20) Maximum no of types of forms that can be processed
C  NUMFOR   Actual possible number of forms
C  KTYPE    Code for form being processed
C  NPAGE    Number of pages to be processed
C  NQUEST   Number of questions to be processed
C  NSPEC    Number of special questions to be processed
C  NQSPEC   Number of codes for each special question
C  QSPEC    Names of special questions
C  CSPEC    Codes for special questions
C  ASPEC    Answers for a codes for special questions
C  JPAGE    Is this page to be processed?
C  PXSC     X scale in LaTeX page for answers (in cm/cm)
C  PYSC     Y scale in LaTeX page for answers (in cm/cm)
C  PXZE     X offset in LaTeX page in cm for answers
C  PYZE     Y offset in LaTeX page in cm for answers
C  FAILED   Program failed?
C  OPEN1    File No 1 open?
C  OPEN2    File No 2 open?
C  OPEN3    File No 3 open?
C  OPEN3    File No 4 open?
C  DOANY    Any pages to do?
C  TQUEST   Name of this question
C  OPTDIR   Directory options list etc are kept
C  TTFILE   Ful name of template file
C  TIFILE   Full name of input file
C  TPFILE   Prompt for name of input file
C  INFILE   Name of input file
C  OUTFILE  Name of output file
C  TPAGE    Name of this page
C  ISVMS    Is this computer running VMS?
C  TEXTX    '\' character
C  TEXTXA   '/' character
C  USEOFF   Add on printing offsets?
C  TOPOFF   Top offset
C  ODDOFF   Odd side offset
C  EVEOFF   Even side offset
C  ETOPOF   Extra Top offset
C  EODDOF   Extra Odd side offset
C  EEVEOF   Extra Even side offset
C  PEROUT   Previous 'percentage message' sent out?
C  DUM1     Dummy variables to make 4*n logicals
C  DUM2
C  DUM3



      integer MAXPAG
      parameter ( MAXPAG=100 )
      integer MAXQUE
      parameter ( MAXQUE=1000 )
      integer MAXQSP
      parameter ( MAXQSP=500 )
      integer MAXCSP
      parameter ( MAXCSP=20 )
      integer MAXFOR
      parameter ( MAXFOR=20 )

      character*1  TEXTX
      parameter ( TEXTX=char(92) )
      character*1  TEXTXA
      parameter ( TEXTXA=char(47) )

      integer NUMFOR
      integer NPAGE
      integer NQUEST
      integer NSPEC
      integer NQSPEC(MAXQSP)

      real    PXSC
      real    PYSC
      real    PXZE
      real    PYZE

      real    TOPOFF
      real    ODDOFF
      real    EVEOFF
      real    ETOPOF
      real    EODDOF
      real    EEVEOF

      logical  JPAGE(MAXPAG)

      logical  FAILED
      logical  OPEN1
      logical  OPEN2
      logical  OPEN3

      logical  OPEN4
      logical  DOANY
      logical  ISVMS
      logical  USEOFF

      logical  PEROUT

      logical  DUM1
      logical  DUM2
      logical  DUM3

      character*50      KTYPE
      character*7       TQUEST(MAXQUE)
      character*7       QSPEC(MAXQSP)
      character*20      CSPEC(MAXQSP,MAXCSP)
      character*20      ASPEC(MAXQSP,MAXCSP)
      character*132     TTFILE
      character*132     TIFILE
      character*132     TPFILE
      character*132     INFILE
      character*132     OUTFILE
      character*132     OPTDIR
      character*2       TPAGE(MAXPAG)

      common / pcom /  NPAGE,   NQUEST,  NUMFOR,  NSPEC,
     +                 NQSPEC,  PXSC,    PYSC,    PXZE, 
     +                 PYZE,    TOPOFF,  EVEOFF,  ODDOFF,
     +                 ETOPOF,  EODDOF,  EEVEOF

      common / pcomb / JPAGE,
     +                 FAILED,  OPEN1,  OPEN2,  OPEN3,
     +                 OPEN4,   DOANY,  ISVMS,  USEOFF,
     +                 PEROUT,  DUM1,   DUM2,   DUM3

      common / pcomc / TQUEST,  TTFILE,  TIFILE,  TPAGE,
     +                 INFILE,  TPFILE,  KTYPE,   QSPEC,
     +                 CSPEC,   ASPEC,   OPTDIR,  OUTFILE
