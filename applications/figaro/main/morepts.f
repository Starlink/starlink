      subroutine morepts(results,resvar,ix,iy,line,lu)
*+
* Name:
*    MOREPTS

* Invocation:
*    CALL MOREPTS(RESULTS,RESVAR,IX,IY,LINE,LU)

* Purpose:
*    To locate further points with the same fit.

* Description:
*    To locate further points with the same fit (i.e. coadded to current
*    point to give stored fit). If the data was blocked then we will use
*    that information, otherwise we will have to search through the
*    whole array!
*
* Arguments:
*      RESULTS(MXPARS,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Results array
*      IX = INTEGER (Given)
*        X (spatial) position of data
*      IY = INTEGER (Given)
*        Y (spatial) position of data
*      LINE = INTEGER (Given)
*        Line being considered
*      LU = INTEGER (Given)
*        Logical unit of output file
* Global variables:
*      MXPARS = INTEGER (Workspace)
*        1st dimension of RESULTS (in arc_dims)
*      NYP = INTEGER (Workspace)
*        2nd dimension of RESULTS (in arc_dims)
*      SPDIM1 = INTEGER (Workspace)
*        1st spatial dimension of data (in arc_dims)
*      SPDIM2 = INTEGER (Workspace)
*        2nd spatial dimension of data (in arc_dims)
* History:
*   T.N.Wilkins, Cambridge, 16-JAN-1990
*        "          "        3-JAN-1991 Minor changes
*-
      implicit none
      include 'arc_dims'
      real results(mxpars,nyp,spdim1,spdim2)
      real resvar(mxpars,nyp,spdim1,spdim2)
      integer ix
      integer iy
      integer line
      integer lu
      character*80 chars

*

      include 'SAE_PAR'
      include 'PRM_PAR'
      real value,rxpos,rypos
      integer xpos,ypos,xend,yend,npts,pts2go,xppos,yppos
      integer get_parnum,i,j,len1,status

      status = SAI__OK

* How many points in fit?

      npts = nint(results(get_parnum('Pts_in_Fit'),line,ix,iy))

*  We need to list any further points in the fit

      if(npts.gt.1) then
        xppos = get_parnum('Space1_pos')
        rxpos = results(xppos,line,ix,iy)
        xpos = nint(rxpos)
        value = resvar(xppos,line,ix,iy)
        yppos = get_parnum('Space2_pos')
        rypos = nint(results(yppos,line,ix,iy))
        ypos = nint(rypos)

*  Was blocking used?
*   Allow for the blocking to be to be val__badr

        if(value.eq.VAL__BADR) then
          if(lu.gt.0) write(lu,*)
          len1 = 0
          call chr_fill(' ',chars)
          call chr_putc('Array positions: ',chars,len1)
          call chr_putc('(',chars,len1)
          call chr_puti(ix,chars,len1)
          call chr_putc(',',chars,len1)
          call chr_puti(iy,chars,len1)
          call chr_putc(')',chars,len1)

*       Loop until we've found all the remaining points, writing their
*       co-ordinates to the file (as many as will fit to a line of our
*       chosen length).

          j = iy
          i = ix + 1
          pts2go = npts - 1
          do while(pts2go.gt.0)
            if(i.gt.spdim1) then
              i = 1
              j = j + 1
              if(j.gt.spdim2) pts2go = 0
            end if
            if((pts2go.gt.0).and.(nint(results(xppos,line,i,j)).eq.xpos)
     :               .and.(nint(results(yppos,line,i,j)).eq.ypos)) then

*        Write into output string

              pts2go = pts2go - 1
              call chr_putc('(',chars,len1)
              call chr_puti(i,chars,len1)
              call chr_putc(',',chars,len1)
              call chr_puti(j,chars,len1)
              call chr_putc(')',chars,len1)
              if(len1.gt.65) then
                call wft(chars(:len1),lu,status)
                call chr_fill(' ',chars)
                len1 = 0
              end if
            end if
            i = i + 1
          end do
          if(len1.gt.0) call wft(chars(:len1),lu,status)
        else

*     Data was blocked, so can quickly find points

          value = sqrt(value) - 0.5
          xend = nint(rxpos + value)
          xpos = nint(rxpos - value)
          value = resvar(yppos,line,ix,iy)
          value = sqrt(value) - 0.5
          yend = nint(rypos + value)
          ypos = nint(rypos - value)
          len1 = 0
          call chr_putc('Array indices ',chars,len1)
          call encode_range(' ',' ',xpos,xend,chars,len1)
          call chr_putc(' ,',chars,len1)
          call encode_range(' ',' ',ypos,yend,chars,len1)
          call wft(chars(:len1),lu,status)
        end if
      else
        write(chars,1)ix,iy
  1     format('Array position ',i4,',',i4)
        call wft(chars,lu,status)
      end if
      end
