CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  IMTYPE.INC


      integer   ipo			!Output image pointer
      real      bso			!Image output pixel value scale
      real      bzo			!Image output pixel value zero
      real      rmaxi			!Input image maximum
      real      rmini			!Input image minimum
      integer   invaloi			!Output integer image invalid flag
      integer*2 invalos			!Output integer*2 image invalid flag
      real      invalor			!Output real image invalid flag

      character*6  outtype		!Output type ('REAL','INT','SHORT','USHORT')

      logical   range			!Calculate range of data?
      logical   auto			!Autoscale data if to smaller data type?
      logical   allbad			!All input image pixels are invalid

      logical imt_dum1			!Make logicals up to times 4
CX      logical imt_dum2			!Make logicals up to times 4
CX      logical imt_dum3			!Make logicals up to times 4



      common / imta / ipo,  bso,  bzo, rmini, rmaxi,
     +                invaloi, invalor

      common / imts / invalos

      common / imtb / range,  auto, allbad, imt_dum1

      common / imtc / outtype

