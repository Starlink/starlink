      program fig3D
      implicit none
      real w(6),eye(3),em,mat(3,3),r0,d0,s(3),v(3),o(3),asize,dash,
     :     opt,u(4),c(3),e(3),txh,size(2),radius,font
      integer istat,izone,len
      logical menu,cart
      character file*40,device*30,text*80,txj*2,cs*1
      common /cart/ cart

      istat = 0
      menu = .true.

*  Get input data file.

      write(6,'(A)') '$Enter name of data file : '
      read(5,'(A)') file
      open( 10, file=file, status='OLD')

*  See if Cartesian or Spherical coordinates are to be used.
      call charin('Enter C or S to select Cartesian or '//
     :            'Speherical coords',menu,cs,len,istat)
      if( cs .eq. 'C' .or. cs .eq. 'c' ) then
         cart = .true.
         write(6,*) 'OK. Cartesian (X,Y,Z) coordinate will be used'
      else
         cart = .false.
         write(6,*) 'OK. Spherical (LONG,LAT,RADIUS) coordinate '//
     :                 'will be used'
      end if

*  Set up EYE parameters.

      call readin('Enter eye vector',menu,3,eye,istat)
      if(istat.ne.0) goto 30
      call sla_cc2s( eye, r0, d0 )
      call sla_euler( 'ZY', r0, -d0, 0.0, mat )
      call sla_vn( eye, eye, em )


*  Set up graphics device.

      call charin('Enter graphics device name',menu,device,len,istat)
      call sgs_open( device(:len), izone, istat )
      if( istat .ne. 0 ) then
         write(*,*) '*** Bad graphics device: '//device(:len)
         stop ' '
      endif

      call readin('Enter picture X and Y dimensions (cms)',menu,2,size,
     :             istat)
      call readin('Enter 3D world coordinate limits X1,X2,Y1,Y2,Z1,Z2',
     :             menu,6,w,istat)
      if(istat.ne.0) goto 30
      call sw3d( size, w, em, mat, u )


*  Get option, and do it.

  10  if( istat .ne. 0 ) goto 30
      call sgs_flush

      if( menu ) then
         write(6,*) ' '
         write(6,*) ' 1 - Arc'
         write(6,*) ' 2 - Line'
         write(6,*) ' 3 - Arrow'
         write(6,*) ' 4 - Dashed arc'
         write(6,*) ' 5 - Dashed line'
         write(6,*) ' 6 - Dashed arrow'
         write(6,*) ' 7 - Supress menu display'
         write(6,*) ' 8 - 3D viewport'
         write(6,*) ' 9 - 2D viewport'
         write(6,*) ' 10 - Arc with arrow head'
         write(6,*) ' 11 - Dashed arc with arrow head'
         write(6,*) ' 12 - Text'
         write(6,*) ' 13 - Text at cursor position'
         write(6,*) ' 14 - Circle'
         write(6,*) ' 15 - Dashed circle'
         write(6,*) ' 16 - Set text justification'
         write(6,*) ' 17 - Set text font'
         write(6,*) ' 18 - Toggle Cartesian/Spherical coordinates'
         write(6,*) ' 19 - Quit'
         write(6,*) ' '
      end if

      call readin('Enter option',menu,1,opt,istat)
      if( istat.ne.0) goto 30

      if( opt .eq. 1 ) then

         call readin('Start',menu,3,s,istat)
         call readin('End',menu,3,e,istat)
         call readin('Centre of circle',menu,3,c,istat)
         call darc( s, e, 1.0E5, c, em, mat, v, istat)


      else if( opt .eq. 2 ) then

         call readin('Start',menu,3,s,istat)
         call readin('End',menu,3,e,istat)
         call dline( s, e, 1.0E5, em, mat ,istat)


      else if( opt. eq. 3 ) then

         call readin('Vector origin',menu,3,o,istat)
         call readin('Vector',menu,3,v,istat)
         call readin('Arrow head size',menu,1,asize,istat)
         call darrow( o, v, 1.0E5, asize, eye, em, mat, istat )

      else if( opt .eq. 4 ) then

         call readin('Start',menu,3,s,istat)
         call readin('End',menu,3,e,istat)
         call readin('Centre of circle',menu,3,c,istat)
         call readin('Dash size',menu,1,dash,istat)
         call darc( s, e, dash, c, em, mat, v, istat )


      else if( opt .eq. 5 ) then

         call readin('Starting (X,Y,Z)',menu,3,s,istat)
         call readin('Ending (X,Y,Z)',menu,3,e,istat)
         call readin('Dash size',menu,1,dash,istat)
         call dline( s, e, dash, em, mat, istat )


      else if( opt. eq. 6 ) then

         call readin('Enter vector origin',menu,3,o,istat)
         call readin('Enter vector',menu,3,v,istat)
         call readin('Enter arrow head size',menu,1,asize,
     :                istat)
         call readin('Dash size',menu,1,dash,istat)
         call darrow( o, v, dash, asize, eye, em, mat, istat)


      else if( opt. eq. 7 ) then
         menu = .not.menu

      else if( opt. eq. 8 ) then
         call view3d( w, em, mat )

      else if( opt. eq. 9 ) then
         call view2d( u )

      else if( opt. eq. 10) then
         call readin('Start',menu,3,s,istat)
         call readin('End',menu,3,e,istat)
         call readin('Centre of circle',menu,3,c,istat)
         call readin('Size of arrow head',menu,1,asize,istat)
         call darcah( s, e, 1.0E5, c, asize, eye, em, mat, istat)

      else if( opt. eq. 11) then
         call readin('Start',menu,3,s,istat)
         call readin('End',menu,3,e,istat)
         call readin('Centre of circle',menu,3,c,istat)
         call readin('Size of arrow head',menu,1,asize,istat)
         call readin('Dash size',menu,1,dash,istat)
         call darcah( s, e, dash, c, asize, eye, em, mat, istat)

      else if( opt. eq. 12) then
         call readin('3D coordinates',menu,3,v,istat)
         call charin('Text',menu,text,len,istat)
         call readin('Text height',menu,1,txh,istat)
         call ptext( text(:len), txh, v, em, mat, istat )

      else if( opt. eq. 13) then
         call charin('Text',menu,text,len,istat)
         call readin('Text height',menu,1,txh,istat)
         call curtxt( text(:len), txh, em, mat, istat )

      else if( opt. eq. 14) then
         call readin('Enter radius of circle',menu,1,radius,
     :                istat)
         call readin('Enter centre of circle',menu,3,o,istat)
         call readin('Enter normal vector to circle',menu,3,v,istat)
         call dcirc(radius,o,v,1.0E5,em,mat,istat)

      else if( opt. eq. 15) then
         call readin('Enter radius of circle',menu,1,radius,
     :                istat)
         call readin('Enter centre of circle',menu,3,o,istat)
         call readin('Enter normal vector to circle',menu,3,v,istat)
         call readin('Dash size',menu,1,dash,istat)
         call dcirc(radius,o,v,dash,em,mat,istat)

      else if( opt. eq. 16) then
         call charin('Text justification string (eg TC)',menu,txj,len,
     :                istat)
         if( istat.eq. 0) call sgs_stxj(txj(:len))

      else if( opt. eq. 17) then
         call readin('GKS font number',menu,1,font,istat)
         call sgs_sfont( int(font) )

      else if( opt. eq. 18) then
         cart = .not. cart
         if( cart ) then
            write(6,*) 'OK. Cartesian (X,Y,Z) coordinate will be used'
         else
            write(6,*) 'OK. Spherical (LONG,LAT,RADIUS) coordinate '//
     :                 'will be used'
         end if

      else if( opt. eq. 19) then
         go to 30

      else
         write(6,*) '*** Bad option: ',opt

      end if


*  Go round for next option.

      goto 10


*  Close.

 30   close(10)

      if( istat .ne. 0 ) then
         open( 10, file='sys$input', status='OLD' )
         istat = 0
         menu = .true.
         goto 10
      end if

      call sgs_close

      end


C------------------------------------------------------------------------
      subroutine darcah( s, e, dash, c, asize, eye, em, mat, istat)
      implicit none

      real eye(3),em,mat(3,3),s(3),e(3),v(3),c(3),asize,dash
      integer istat

      if( istat.ne.0) return

      call darc( s, e, dash, c, em, mat, v, istat )
      call ah( v, e, asize, eye, em, mat, istat )

      end

C------------------------------------------------------------------------
      subroutine darc( ss, ee, dsize, c, em, mat, v, istat )
      implicit none

      real em,mat(3,3),s(3),ss(3),ee(3),e(3),v(3),sep,step,dist,ms,me,
     :     dsize,efac,sfac,t(3),sla_vdv,c(3),cossep,sinsep
      integer i,istat

      if( istat.ne.0) return

      s(1) = ss(1) - c(1)
      s(2) = ss(2) - c(2)
      s(3) = ss(3) - c(3)
      e(1) = ee(1) - c(1)
      e(2) = ee(2) - c(2)
      e(3) = ee(3) - c(3)

      call sla_vn( s, s, ms )
      call sla_vn( e, e, me )
      call sla_vxv( s, e, t )
      call sla_vn( t, t, sinsep )
      cossep = sla_vdv( s, e )
      sep = atan2( sinsep, cossep )

      step = sep/real(400 - 1)
      v(1) = ms*s(1) + c(1)
      v(2) = ms*s(2) + c(2)
      v(3) = ms*s(3) + c(3)
      call bcurve( v, 1, em, mat)

      do i = 1, 400
         dist = (i-1)*step
         sfac = ms*sin(sep-dist)/sinsep
         efac = me*sin(dist)/sinsep
         v(1) = s(1)*sfac + e(1)*efac + c(1)
         v(2) = s(2)*sfac + e(2)*efac + c(2)
         v(3) = s(3)*sfac + e(3)*efac + c(3)
         call acurve( v, i, dsize, em, mat )

      end do

      call sgs_opoly

      sfac = -ms/sinsep
      efac = me*cossep/sinsep
      v(1) = s(1)*sfac + e(1)*efac
      v(2) = s(2)*sfac + e(2)*efac
      v(3) = s(3)*sfac + e(3)*efac

      end


C------------------------------------------------------------------------
      subroutine dline( s, e, dsize, em, mat,istat )
      implicit none

      real em,mat(3,3),s(3),e(3),v(3),sep,step,dist,dsize,t(3)
      integer i,istat

      if( istat.ne.0) return

      sep = sqrt( (s(1)-e(1))**2 + (s(2)-e(2))**2 + (s(3)-e(3))**2 )
      t(1)= (e(1)-s(1))/sep
      t(2)= (e(2)-s(2))/sep
      t(3)= (e(3)-s(3))/sep

      step = sep/real(400 - 1)
      call bcurve( s, 1, em, mat )

      do i = 1, 400
         dist = (i-1)*step
         v(1) = s(1) + dist*t(1)
         v(2) = s(2) + dist*t(2)
         v(3) = s(3) + dist*t(3)

         call acurve( v, i, dsize, em, mat )

      end do

      call sgs_opoly

      end


C--------------------------------------------------------------------------
      subroutine darrow( o, v, dsize, asize, eye, em, mat, istat )
      implicit none
      real em,mat(3,3),v(3),o(3),e(3),asize,dsize,eye(3)
      integer istat

      if( istat.ne.0) return

      e(1) = v(1) + o(1)
      e(2) = v(2) + o(2)
      e(3) = v(3) + o(3)
      call dline( o, e, dsize, em, mat, istat )

      call ah( v, e, asize, eye, em, mat, istat )

      end

C------------------------------------------------------------------------
      subroutine apoly( v, em, mat )
      implicit none
      real v(3), em, mat(3,3), x, y
      logical out
      integer istat
      common /poly/ out

      call proj( v, em, mat, x, y, istat )

      if( istat.eq.0 ) then
         if( out ) then
            out = .false.
            call sgs_bpoly( x, y )
         else
            call sgs_apoly( x, y )
         end if

      else
         call sgs_opoly
         out = .true.
      endif

      end

C--------------------------------------------------------------------------
      subroutine bpoly( v, em, mat )
      implicit none
      integer istat
      logical out
      real v(3), em, mat(3,3), x, y
      common /poly/ out

      call proj( v, em, mat, x, y, istat )
      if( istat .eq. 0 ) then
         call sgs_bpoly( x, y )
         out = .false.
      else
         out = .true.
      end if

      end


C--------------------------------------------------------------------------
      subroutine proj( v, em, mat, x, y, istat )
      implicit none
      real v(3), em, mat(3,3), t(3), x, y, face
      integer istat

      call sla_mxv( mat, v, t )
      t(1) = em - t(1)
      face = sqrt( t(2)**2+t(3)**2 )
      if( t(1) .gt. face*0.58 ) then
         x = em*( t(2)/t(1) )
         y = em*( t(3)/t(1) )
         istat = 0
      else
         istat = 1
         x = t(2)*t(1)*1.732/face
         y = t(3)*t(1)*1.732/face
      end if

      end


C--------------------------------------------------------------------------
      subroutine sw3d( size, w, em, mat, u )
      implicit none
      real w(6), v(3), em, mat(3,3), xmax,xmin,ymax,ymin,xx,yy,
     :     x(2),y(2),z(2),u(4),aspect,size(2)
      integer iz,iy,ix,istat,izone1,izone2

      call sgs_zsize( size(1)*0.01, size(2)*0.01, 'TL', izone1, istat)


      xmax=-1.0E32
      xmin=1.0E32
      ymax=-1.0E32
      ymin=1.0E32

      z(1)=min( w(5), w(6) )
      y(1)=min( w(3), w(4) )
      x(1)=min( w(1), w(2) )

      z(2)=max( w(5), w(6) )
      y(2)=max( w(3), w(4) )
      x(2)=max( w(1), w(2) )

      do iz = 1, 2
         v(3)=z(iz)

         do iy = 1, 2
            v(2)=y(iy)

            do ix = 1, 2
               v(1)=x(ix)

               call proj( v, em, mat, xx, yy, istat )

               xmax = max( xmax, xx )
               xmin = min( xmin, xx )
               ymax = max( ymax, yy )
               ymin = min( ymin, yy )

            end do
         end do
      end do

      aspect = (xmax-xmin)/(ymax-ymin)
      call sgs_zshap( aspect, 'TL', izone2, istat )
      call sgs_sw( xmin, xmax, ymin, ymax, istat )

      u(1) = xmin
      u(2) = xmax
      u(3) = ymin
      u(4) = ymax

      end


C------------------------------------------------------------------------
      subroutine view2d( u )
      implicit none
      real u(4)

      call sgs_box( u(1), u(2), u(3), u(4) )
      call sgs_flush

      end


C------------------------------------------------------------------------
      subroutine view3d( w, em, mat )
      implicit none
      real w(6), v(3,8), em, mat(3,3), xmax,xmin,ymax,ymin,zmax,zmin

      zmin=min( w(5), w(6) )
      ymin=min( w(3), w(4) )
      xmin=min( w(1), w(2) )

      zmax=max( w(5), w(6) )
      ymax=max( w(3), w(4) )
      xmax=max( w(1), w(2) )

      v(3,1) = zmax
      v(2,1) = ymax
      v(1,1) = xmax
      v(3,2) = zmax
      v(2,2) = ymax
      v(1,2) = xmin
      v(3,3) = zmax
      v(2,3) = ymin
      v(1,3) = xmin
      v(3,4) = zmax
      v(2,4) = ymin
      v(1,4) = xmax
      v(3,5) = zmin
      v(2,5) = ymax
      v(1,5) = xmax
      v(3,6) = zmin
      v(2,6) = ymax
      v(1,6) = xmin
      v(3,7) = zmin
      v(2,7) = ymin
      v(1,7) = xmin
      v(3,8) = zmin
      v(2,8) = ymin
      v(1,8) = xmax

      call bpoly( v(1,1), em, mat )
      call apoly( v(1,2), em, mat )
      call apoly( v(1,3), em, mat )
      call apoly( v(1,4), em, mat )
      call apoly( v(1,1), em, mat )
      call apoly( v(1,5), em, mat )
      call apoly( v(1,6), em, mat )
      call apoly( v(1,7), em, mat )
      call apoly( v(1,8), em, mat )
      call apoly( v(1,5), em, mat )

      call bpoly( v(1,6), em, mat )
      call apoly( v(1,2), em, mat )

      call bpoly( v(1,7), em, mat )
      call apoly( v(1,3), em, mat )

      call bpoly( v(1,8), em, mat )
      call apoly( v(1,4), em, mat )

      call sgs_opoly

      end

C------------------------------------------------------------------------
      subroutine readin(prompt,menu,nval,vals,istat)
      implicit none
      integer nval,i,excl,istat,chr_len
      real vals(nval),long,lat,radius
C   prompt buffer is declared in size here to keep g77 happy
C   when concatenating a string of unknown length
      character prompt*(80),buffer*80
      logical menu,cart
      real pi
      parameter ( pi = 3.141592654 )
      common /cart/ cart

      if( istat .ne. 0 ) return

      if( prompt .ne. ' ' .and. menu) write(6,'(A)') '$'//prompt//' : '
 10   read(10,'(A)',iostat=istat) buffer

      if( istat .eq. 0 ) then
         call chr_ldblk(buffer)
         excl = index( buffer,'!')
         if( excl .ne. 0 ) buffer( excl: ) = ' '

         if( buffer .ne. ' ' ) then
            read(buffer,*,iostat=istat) (vals(i),i=1,nval)
            if( istat .ne. 0 ) then
               write(6,*) '*** Error getting numeric values for '//
     :                    ' prompt "',prompt,'"'
               write(6,*) '    Value obtained was : ',
     :                    buffer(:chr_len(buffer))
            endif
         else
            goto 10
         end if
      endif

*  Convert to Cartesian coordinates if spherical coordinates were
*  entered.
      if( nval .eq. 3 .and. .not. cart ) then
         long = vals(1) * pi / 180.0
         lat = vals(2) * pi / 180.0
         radius = vals(3)
         vals(1) = radius*cos( lat )*cos( long )
         vals(2) = radius*cos( lat )*sin( long )
         vals(3) = radius*sin( lat )
         write(6,*) 'Equivalent Cartesian coordinates: ',vals(1),
     :               vals(2),vals(3)
      end if

      end
C------------------------------------------------------------------------
      subroutine charin(prompt,menu,text,len,istat)
      implicit none
      integer excl,istat,len,chr_len
C   prompt buffer is declared in size here to keep g77 happy
C   when concatenating a string of unknown length
      character text*(*),prompt*(80)
      logical menu

      if( istat .ne. 0 ) return

      if( prompt .ne. ' ' .and. menu) write(6,'(A)') '$'//prompt//' : '
 10   read(10,'(A)',iostat=istat) text

      if( istat .eq. 0 ) then
         excl = index( text,'!')
         if( excl .ne. 0 ) text( excl: ) = ' '

         if( text .eq. ' ' ) goto 10

      endif

      len = chr_len( text )

      end

C-------------------------------------------------------------------------
      subroutine ah( v, e, asize, eye, em, mat, istat )
      implicit none

      integer istat
      real    v(3),e(3),asize,eye(3),em,mat(3,3),vcomp,pcomp,c(3),t(3),
     :        mc,mv

      if( istat .ne. 0 ) return

      vcomp = 0.866*asize
      pcomp = 0.5*asize

      call sla_vxv( eye, v, c )

      call sla_vn( c, c, mc )
      call sla_vn( v, v, mv )
      t(1) = e(1)-vcomp*v(1)+pcomp*c(1)
      t(2) = e(2)-vcomp*v(2)+pcomp*c(2)
      t(3) = e(3)-vcomp*v(3)+pcomp*c(3)
      call bpoly( t, em, mat )
      call apoly( e, em, mat )
      t(1) = e(1)-vcomp*v(1)-pcomp*c(1)
      t(2) = e(2)-vcomp*v(2)-pcomp*c(2)
      t(3) = e(3)-vcomp*v(3)-pcomp*c(3)
      call apoly( t, em, mat )

      call sgs_opoly

      end
C--------------------------------------------------------------------------
      subroutine iproj( vin, vout, em, mat, istat )
      implicit none
      real vin(3), vout(3), em, mat(3,3), t(3)
      integer istat

      if( istat.ne.0 ) return

      t(1) = em - vin(3)
      t(2) = vin(1)*vin(3)/em
      t(3) = vin(2)*vin(3)/em
      call sla_imxv( mat, t, vout )

      end
C------------------------------------------------------------------------
      subroutine ptext( text, txh, v, em, mat, istat )
      implicit none
      real txh,v(3),em,mat(3,3),x,y
      integer istat
      character text*(*)

      call sgs_shtx( txh )
      call proj( v, em, mat, x, y, istat )
      if( istat .eq. 0 ) then
         call sgs_tx( x, y, text )
         call sgs_otext
      else
         write(6,*) '*** Text would lie behind the observer'
      end if

      end
C------------------------------------------------------------------------
      subroutine dcirc(radius,o,v,dash,em,mat,istat)
      implicit none
      real radius,v(3),dash,em,mat(3,3),t1(3),t2(3),r(3),et1,et2,dist,
     :     cosd,sind,o(3)
      integer istat,i

      if(istat.ne.0) return

      if( v(3) .ne. 0.0 ) then
         t1(1) = 0
         t1(2) = 1
         t1(3) = -v(2)/v(3)
      else if( v(2) .ne. 0.0 ) then
         t1(1) = 0
         t1(3) = 1
         t1(2) = -v(3)/v(2)
      else if( v(1) .ne. 0.0 ) then
         t1(2) = 0
         t1(3) = 1
         t1(1) = -v(3)/v(1)
      else
         write(6,*) '*** Circle normal vector is null'
         return
      endif

      call sla_vxv( v, t1, t2 )
      call sla_vn( t1, t1, et1 )
      call sla_vn( t2, t2, et2 )

      r(1) = radius*t1(1)+o(1)
      r(2) = radius*t1(2)+o(2)
      r(3) = radius*t1(3)+o(3)
      call bcurve( r, 1, em, mat)

      do i = 1, 400
         dist = i*1.5708E-2
         cosd = cos(dist)*radius
         sind = sin(dist)*radius
         r(1) = t1(1)*cosd+t2(1)*sind+o(1)
         r(2) = t1(2)*cosd+t2(2)*sind+o(2)
         r(3) = t1(3)*cosd+t2(3)*sind+o(3)
         call acurve( r, i, dash, em, mat )

      end do

      call sgs_opoly

      end
C------------------------------------------------------------------------
      subroutine bcurve( v, index, em, mat )
      implicit none
      real v(3), em, mat(3,3), l(3),arclen
      integer li,index
      logical penup
      common /curve/ l,arclen,li,penup

      call bpoly( v, em, mat )
      l(1) = v(1)
      l(2) = v(2)
      l(3) = v(3)
      arclen=0.0
      li=index
      penup = .false.

      end
C------------------------------------------------------------------------
      subroutine acurve( v, index, dash, em, mat )
      implicit none
      real v(3), dash, em, mat(3,3), l(3),arclen,inclen
      integer li,index
      logical penup
      common /curve/ l,arclen,li,penup

      inclen = sqrt( (v(1)-l(1))**2 + (v(2)-l(2))**2
     :                          +(v(3)-l(3))**2 )
      arclen = arclen + inclen
      l(1) = v(1)
      l(2) = v(2)
      l(3) = v(3)

      if( arclen .gt. dash ) then
         arclen = 0.0
         if( index - li .gt. 2 ) then
            penup = .not. penup
            if( .not. penup ) call bpoly( v, em, mat )
         else
            if( penup ) then
               penup = .false.
               call bpoly( v, em, mat )
            endif
         endif
         li = index
      end if
      if( .not. penup ) call apoly( v, em, mat )

      end
C-------------------------------------------------------------------
      subroutine curtxt( text, txh, em, mat, istat )
      implicit none
      real em,mat(3,3),txh,x,y,t(3),alpha,beta,sinapb,apb,f,org,minorg
      integer istat,n
      character text*(*)
      logical cursor

      if( istat .ne. 0 ) return

      call sgs_icuav(cursor)
      if(cursor) then
         write(6,*) 'Position cursor at text and press any button'
         call sgs_reqcu( x, y, n )
 10      call readin('Distance from origin (+ve on eye-side)',.true.,1,
     :                org, istat )

         f = sqrt(x**2+y**2)
         if( f .ne. 0.0 ) then
            alpha = atan2( f, em )
            minorg=em*sin(alpha)
            if( abs(org) .gt. minorg ) then
               sinapb = abs( minorg/org )
               apb = asin( sinapb )
               if( org. lt. 0 ) apb = 3.1415927 - apb
               beta = apb - alpha
               t(1) = abs(org)*cos(beta)
               t(2) = x*abs(org)*sin(beta)/f
               t(3) = y*abs(org)*sin(beta)/f
            else
               write(6,*)
     :         '*** The 3D line never gets that close to the origin'
               write(6,*) '    The closest approach to the origin is ',
     :                    minorg
               goto 10
            endif
         else
            t(1) = org
            t(2) = 0.0
            t(3) = 0.0
         end if

         call sla_imxv( mat, t, t )
         write(6,*) '  (X,Y,Z): ',t(1),t(2),t(3)
         call ptext( text, txh, t, em, mat, istat )

      else
         write(6,*) '*** Device has no cursor; unable to put text '//
     :                 'at cursor position'
      end if

      end
