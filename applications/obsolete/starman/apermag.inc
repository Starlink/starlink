CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C APERMAG.INC


      real    noise		!Extra noise per pixel
      real    poisv		!Poisson converter
      real    apdia		!Star aperture diameter
      real    skydiain		!Sky aperture inner diameter
      real    skydiaout		!Sky aperture outer diameter

      logical typing		!Type out results as proceed?
      logical centre		!Centre star before photometry?

      logical amp_dum1
      logical amp_dum2

      common / ampa / noise, poisv, apdia, skydiain, skydiaout

      common / ampb / typing, centre, amp_dum1, amp_dum2
