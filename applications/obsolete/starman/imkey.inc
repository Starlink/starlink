CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  IMKEY.INC


      integer   ipo			!Output image pointer
      integer   ipima			!Work image pointer
      real      rlev			!Background level

      logical   inim			!Is there an input image?

      logical imke_dum1			!Make logicals up to times 4
      logical imke_dum2			!Make logicals up to times 4
      logical imke_dum3			!Make logicals up to times 4



      common / imkea / ipo,  ipima, rlev

      common / imkeb / inim, imke_dum1, imke_dum2, imke_dum3

