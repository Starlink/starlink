!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! VIRT.INC
!
      integer   maxvirt
      parameter ( maxvirt=1536*1536 )
      byte      vt_im(maxvirt)		! Display virtual image

      common /virt/ vt_im
