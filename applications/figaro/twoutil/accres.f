      subroutine accres(refnam,name,mode,nelm,work,chwork,status)
*+
* Name:
*    ACCRES

* Invocation:
*    CALL ACCRES(REFNAM,NAME,MODE,NELM,WORK,CWORK,STATUS)

* Purpose:
*     To access part of the results structure.

* Description:
*     To access part of the results structure. The form of access is
*    determined by the argument MODE. This MUST first be called with
*    MODE = 'FI'. At present all DTA data types except byte can be used.
*    The method is to obtain the name of the structure using
*    DSA_SPECIFIC_STRUCTURE, and store this name for future use. Further
*    work is carried out using DTA routines.
*
*     The use of one argument (WORK) for data of any type makes use of the
*    fact that data is passed by address, except for character data for
*    which a second argument is needed.
*
* Arguments:
*      REFNAM = CHARACTER*(*) (Given)
*        Reference name of file (only used for MODE='FI')
*      NAME = CHARACTER*(*) (Given)
*        Name of element of structure (e.g. 'RES', or
*                        DATA).
*      MODE = CHARACTER*2 (Given)
*        Mode of access:
*                      -   'CR' : Map character data for read only
*                      -   'CO' : Copy one structure to another. For
*                                 this NAME and RENAM are the destination
*                                 structure and the previously set up
*                                 structure is the source.
*                      -   'CU' : Map character data for update
*                      -   'DE' : Delete whole results structure
*                      -   'DR' : Map double precision data for read only
*                      -   'DU' : Map double precision data for update
*                      -   'FI' : Find results structure
*                      -   'FR' : Map float data for read only
*                      -   'FU' : Map float data for update
*                      -   'IR' : Map integer data for read only
*                      -   'IU' : Map integer data for update
*                      -   'RC' : Read character data
*                      -   'RD' : Read double precision data
*                      -   'RE' : Return name of structure
*                      -   'RF' : Read float data
*                      -   'RI' : Read integer data
*                      -   'RS' : Read short data
*                      -   'SI' : Get size of array
*                      -   'SR' : Map short data for read only
*                      -   'SU' : Map short data for update
*                      -   'UM' : Unmap data
*                      -   'WC' : Write character data
*                      -   'WD' : Write double precision data
*                      -   'WF' : Write float data
*                      -   'WI' : Write integer data
*                      -   'WS' : Write short data
*     Given/returned (depending upon MODE)
*      NELM = INTEGER (Given and returned)
*        Number of elements to consider (if mode='SI' then this on entry
*        should be the maximum number of dimensions, on exit it is the
*        actual) If this is 1 for mode='FI', then the structure is found
*        read-only, if 0 it is for update, otherwise the routine will
*        check whether it exists, but will not output error messages.
*      WORK(*) = ARRAY (Given and returned)
*        Work space, array to read/write, or returned pointer
*        (descriptor for character data).  (WORK is ignored for
*        MODE='FI'
*      CWORK = CHARACTER*(*) (Given and returned)
*        Character version of WORK (although on the VAX this doesn't
*        matter, for the SUN is does (it might matter at some time).
*      STATUS = INTEGER (Given and returned)
*        Error status, 0=ok. If non-zero on entry, this routine returns
*        without action.
*
* Subroutines/functions called:
*      DSA_SPECIFIC_STRUCTURE : Get DTA name of structure
*      DTA_DLVAR       : Delete struture
*      DTA_MRVARD      : Map double precision data read only
*      DTA_MUVARD      : Map double precision data for update
*      DTA_MRVARF      : Map floating point data read only
*      DTA_MUVARF      : Map floating point data for update
*      DTA_MRVARI      : Map integer data read only
*      DTA_MUVARI      : Map integer data for update
*      DTA_MRVARS      : Map short data read only
*      DTA_MUVARS      : Map short data for update
*      DTA_SZVAR       : Get size of data array
*      DTA_TYVAR       : Get type of object
*      DYN_ELEMENT = INTEGER
*        Convert address to array element of DYNAMIC_MEM
*      ICH_FOLD = INTEGER
*        Convert string to upper case
*      CHR_LEN = INTEGER
*        Find non-blank length of string
*
* Internal saved variables:
*      DTANAM          : DTA name of structure
*      DTALEN          : Length of DTA name of structure

* History:
*    T.N.Wilkins/Manchester 18-19/10/88
*         "     /Cambridge 6/6/89 ICH_FOLD replaced CHR_UCASE.
*         "          "     11/10/89 Change to call of message
*         "          "     30/11/89 No longer uses str$ routine.
*         "          "      4/1/90 DYN_ELEMENT added
*    A.J.Holloway/Manchester 5/98
*         "          "     Removal of non-FDA DTA & DSA calls.
*-
      implicit none
      include 'SAE_PAR'
      include 'DAT_PAR'
      include 'DAT_ERR'
      include 'CMP_ERR'
      character*(*) name,refnam
      character*2 mode,umode,fmode*1
      character*(*) chwork
      integer nelm,work,status,mxdim,ncomp,i
      character*70 fulnam,dtanam,type*50,tname
      character*(DAT__SZLOC) nloc, cloc, dloc, chars1p,tloc
      integer chr_len,dtalen,ilen,dyn_element,el
      logical state
      external par_wruser
      save dtanam,dtalen

*remove

      INTEGER IDOT1, IDOT2, IBRA ! Pointers into PATH
      INTEGER NDIM               ! Ignored
      INTEGER DIMS( DAT__MXDIM ) ! Ignored
      CHARACTER * ( DAT__SZLOC ) LOC ! HDS locator
      CHARACTER*70 PATH
      Logical defined

      if(status.ne.SAI__OK) return

*  convert mode to upper case

      umode = mode
      call chr_ucase(umode)

      if(umode.eq.'FI') then

* Get name of structure

        if(nelm.eq.1) then
          fmode = 'R'
        else if(nelm.eq.0) then
          fmode = 'U'
        else
          fmode = ' '
        end if

        call dsa_specific_structure(refnam,name,fmode,dtanam,status)

        if(fmode.eq.' ') then

* Old dta library
*          call dta_tyvar(dtanam,type,status)
*          call cmp_type(nloc, dtanam, type, status)

             call dta_loc(dtanam,nloc,status)
             call dat_type(nloc,type,status)
             call dta_annul(nloc,status)
        end if
        dtalen = chr_len(dtanam)

      else

*   Create name of element

        fulnam = dtanam(:dtalen)//'.'//name

*     Write data to file

        if(umode(1:1).eq.'W') then
          if(umode(2:2).eq.'C') then
* Old DSA library
*            call dta_wrvarc(fulnam,nelm,chwork,status)
*             call cmp_put0c(nloc,fulnam, chwork, status)

             call dta_loc(fulnam,nloc,status)

             call dat_put0c(nloc,chwork,status)


             call dta_annul(nloc,status)


          else if(umode(2:2).eq.'D') then
            call dta_wrvard(fulnam,nelm,work,status)
          else if(umode(2:2).eq.'F') then
            call dta_wrvarf(fulnam,nelm,work,status)
          else if(umode(2:2).eq.'I') then
            call dta_wrvari(fulnam,nelm,work,status)
          else if(umode(2:2).eq.'S') then
* Old DSA library
*            call dta_wrvars(fulnam,nelm,work,status)
*          call cmp_put1i(nloc,fulnam,nelm,work,status)

             call dta_loc(fulnam,nloc,status)
*             call dat_put(nloc,'_WORD',1,nelm,work,status)
             if (nelm.gt.1) then
                call dat_put(nloc,'_WORD',1,nelm,work,status)
             else
               call dat_put(nloc,'_WORD',0,0,work,status)
             end if

             call dta_annul(nloc,status)

          else
            goto 550
          end if
          if(status.ne.SAI__OK) then
            call tnw_dtaerr(status,'writing',fulnam)
          end if

*     Return name of structure

        else if(umode.eq.'RE') then
          ilen = dtalen
          if(ilen.gt.len(chwork)) then
            call par_wruser(
     :     'Error copying name, output string too short',status)
            status = SAI__ERROR
            ilen = len(chwork)
          end if
          chwork = dtanam(:ilen)

*     Read data from file

        else if(umode(1:1).eq.'R') then
          if(umode(2:2).eq.'C') then
* Old DTA library
*           call dta_rdvarc(fulnam,nelm,chwork,status)
*CALL CMP_GET0C(NLOC,fulnam, chwork, STATUS)

             call dta_loc(fulnam,nloc,status)

             call dat_state(nloc,state,status)

             if(state) then

                call dat_get0c(nloc,chwork,status)

             end if

             call dta_annul(nloc,status)

          else if(umode(2:2).eq.'D') then
            call dta_rdvard(fulnam,nelm,work,status)
          else if(umode(2:2).eq.'F') then
*            call dta_rdvarf(fulnam,nelm,work,status)

             call dta_loc(fulnam,nloc,status)
             call dat_state(nloc,state,status)
             if(state) then

               call dat_get(nloc,'_REAL',1,nelm,work,status)

             END IF


          else if(umode(2:2).eq.'I') then
            call dta_rdvari(fulnam,nelm,work,status)
          else if(umode(2:2).eq.'S') then
* Old DTA library
*            call dta_rdvars(fulnam,nelm,work,status)
*             call cmp_get1i(nloc,fulnam,nelm,work,status)

             call dta_loc(fulnam,nloc,status)

*             call dat_get(nloc,'_WORD',1,nelm,work,status)

             if (nelm.gt.1) then
                call dat_get(nloc,'_WORD',1,nelm,work,status)
             else
                 call dat_state(nloc,state,status)

                 if(state) then

                    call dat_get(nloc,'_WORD',0,0,work,status)

                 end if
             end if

             call dta_annul(nloc,status)


          else
            goto 550
          end if
          if(status.ne.SAI__OK) then
            call tnw_dtaerr(status,'reading',fulnam)
          end if

*     Get size of array

        else if(umode.eq.'SI') then
          mxdim = nelm
          call dta_szvar(fulnam,mxdim,nelm,work,status)
          if(status.ne.SAI__OK) then
            call tnw_dtaerr(status,'finding size of',fulnam)
          end if

*     Unmap data

        else if(umode.eq.'UM') then

* Old dsa library call
*          call dta_frvar(fulnam,status)

             call dta_loc(fulnam,nloc,status)
             call dat_unmap(nloc,status)
             call dta_annul(nloc,status)


          if(status.ne.SAI__OK) then
            call tnw_dtaerr(status,'unmapping',fulnam)
          end if

*     Copy one structure to another

        else if(umode.eq.'CO') then
          call dsa_specific_structure(refnam,name,'W',fulnam,status)

* Old DTA library
*          call dta_cyvar(dtanam,fulnam,status)

          call dta_loc(fulnam,dloc,status)
          call dta_loc(dtanam,cloc,status)


          call DAT_PAREN(dloc, chars1p, status)


          call dat_ncomp(cloc, ncomp, status)
          do 1 i=1, ncomp
             call dat_index(cloc, i, tloc, status)
             call dat_name(tloc, tname, status)
             call dat_copy(tloc, chars1p, name, status)
             call dat_annul(tloc, status)
 1        continue

          call dat_annul(chars1p, status)
          call dat_annul(cloc, status)
          call dat_annul(dloc, status)



*     Delete structure

        else if(umode.eq.'DE') then

*          call dta_dlvar(dtanam,status)


* SWIPE CODE FROM DTA_DLVAR TO TEST
           path=dtanam
          CALL DTA1_SPLIT( PATH, DAT__MXDIM,
     :         IDOT1, IDOT2, IBRA, NDIM, DIMS, STATUS )


      IF ( IDOT2 .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T016', PATH )
         CALL ERR_REP( 'FDA_E084', 'DTA1_DLVAR: Error deleting the ' //
     :        'object ^FDA_T016. The DTA object name cannot be ' //
     :        'split into parent and new component.', STATUS )

         GO TO 500
      END IF
*     Locate the parent.
      CALL DTA1_LOC( PATH(:IDOT2-1), LOC, STATUS )

*     Erase the component.
*     If there is a cell spec in square brackets, this will return moaning
*     about an invalid component name. That's just fine.
      CALL DAT_ERASE( LOC, PATH(IDOT2+1:), STATUS )

*     Tidy up.
 500  CONTINUE
      CALL DAT_ANNUL( LOC, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE


          if(status.ne.SAI__OK) then
            call tnw_dtaerr(status,'deleting',dtanam)
          end if

*     Map data

        else
          if(umode.eq.'CR') then
* Old DTA library
*            call dta_mrvarc(fulnam,nelm,work,status)
* mode not used
          else if(umode.eq.'CU') then
* Old DTA library
*            call dta_muvarc(fulnam,nelm,work,status)


             call dta_loc(fulnam,nloc,status)

* Check to see if the item is defined

             call dat_state(nloc,defined,status)

             if (defined) then

                call dat_mapv(nloc,'_CHAR','UPDATE',work,
     $               el,status)

             else
                call dat_mapv(nloc,'_CHAR','WRITE',work,
     $               el,status)

             endif

*             call dta_annul(nloc,status)


          else if(umode.eq.'DU') then
* Old DTA library
*            call dta_muvard(fulnam,nelm,work,status)

             call dta_loc(fulnam,nloc,status)

* Check to see if the item is defined

             call dat_state(nloc,defined,status)

             if (defined) then

                call dat_mapv(nloc,'_DOUBLE','UPDATE',work,
     $               el,status)

             else
                call dat_mapv(nloc,'_DOUBLE','WRITE',work,
     $               el,status)
             endif

             call dta_annul(nloc,status)



          else if(umode.eq.'DR') then
* Old DTA library
*            call dta_mrvard(fulnam,nelm,work,status)
*mode not used
          else if(umode.eq.'FU') then
* Old DTA library
*            call dta_muvarf(fulnam,nelm,work,status)

             call dta_loc(fulnam,nloc,status)

* Check to see if the item is defined

             call dat_state(nloc,defined,status)

             if (defined) then

             call dat_mapv(nloc,'_REAL','UPDATE',work,
     $            el,status)

          else

             call dat_mapv(nloc,'_REAL','WRITE',work,
     $            el,status)
          endif

* query regarding anulling file for update
*             call dta_annul(nloc,status)




          else if(umode.eq.'FR') then
* Old DTA library
*            call dta_mrvarf(fulnam,nelm,work,status)

             call dta_loc(fulnam,nloc,status)
             call dat_mapv(nloc,'_REAL','READ',work,
     $            el,status)
             call dta_annul(nloc,status)


          else if(umode.eq.'IU') then
* Old DTA library
*            call dta_muvari(fulnam,nelm,work,status)

             call dta_loc(fulnam,nloc,status)

* Check to see if the item is defined

             call dat_state(nloc,defined,status)

             if (defined) then


             call dat_mapv(nloc,'_INTEGER','UPDATE',work,
     $            el,status)

             else
             call dat_mapv(nloc,'_INTEGER','WRITE',work,
     $            el,status)

             endif

*             call dta_annul(nloc,status)



          else if(umode.eq.'IR') then
* Old DTA library
*            call dta_mrvari(fulnam,nelm,work,status)

             call dta_loc(fulnam,nloc,status)
             call dat_mapv(nloc,'_INTEGER','READ',work,
     $            el,status)
             call dta_annul(nloc,status)



          else if(umode.eq.'SU') then
* Old DTA library
*            call dta_muvars(fulnam,nelm,work,status)

             call dta_loc(fulnam,nloc,status)

* Check to see if the item is defined

             call dat_state(nloc,defined,status)

             if (defined) then

             call dat_mapv(nloc,'_WORD','UPDATE',work,
     $            el,status)

             else
             call dat_mapv(nloc,'_WORD','WRITE',work,
     $            el,status)

             endif

*             call dta_annul(nloc,status)


          else if(umode.eq.'SR') then
* Old DTA library
*            call dta_mrvars(fulnam,nelm,work,status)

             call dta_loc(fulnam,nloc,status)
             call dat_mapv(nloc,'_WORD','READ',work,
     $            el,status)
             call dta_annul(nloc,status)


          else
            goto 550
          end if

*      Convert to array element
* AJH 5/99 Only if mode is not SU,FU,CU,IU

	if (umode(2:2).ne.'U') then

		  work = dyn_element(work)
	  end if
          if(status.ne.SAI__OK) then
            call tnw_dtaerr(status,'mapping',fulnam)
          end if
        end if
      end if
      return
 550  continue
      call par_wruser('ACCRES: Code '//umode//' not recognised',status)
      status = SAI__ERROR
      end
