/*  This file, group.c, contains the grouping convention suport routines.  */

/*  The FITSIO software was written by William Pence at the High Energy    */
/*  Astrophysic Science Archive Research Center (HEASARC) at the NASA      */
/*  Goddard Space Flight Center.  Users shall not, without prior written   */
/*  permission of the U.S. Government,  establish a claim to statutory     */
/*  copyright.  The Government and others acting on its behalf, shall have */
/*  a royalty-free, non-exclusive, irrevocable,  worldwide license for     */
/*  Government purposes to publish, distribute, translate, copy, exhibit,  */
/*  and perform such material.                                             */
/*                                                                         */
/*  The group.c module of CFITSIO was written by Donald G. Jennings of     */
/*  the INTEGRAL Science Data Centre (ISDC) under NASA contract task       */
/*  66002J6. The above copyright laws apply. Copyright guidelines of The   */
/*  University of Geneva might also apply.                                 */

/*  The following routines are designed to create, read, and manipulate    */
/*  FITS Grouping Tables as defined in the FITS Grouping Convention paper  */
/*  by Jennings, Pence, Folk and Schlesinger. The development of the       */
/*  grouping structure was partially funded under the NASA AISRP Program.  */ 
    
#include "fitsio2.h"
#include "group.h"
#include <string.h>
#include <stdlib.h>

/*---------------------------------------------------------------------------
 Change record:

D. Jennings, 18/06/98, version 1.0 of group module delivered to B. Pence for
                       integration into CFITSIO 2.005

-----------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
int ffgtcr(fitsfile *fptr,      /* FITS file pointer                         */
	   char    *grpname,    /* name of the grouping table                */
	   int      grouptype,  /* code specifying the type of
				   grouping table information:
				   GT_ID_ALL_URI  0 ==> defualt (all columns)
				   GT_ID_REF      1 ==> ID by reference
				   GT_ID_POS      2 ==> ID by position
				   GT_ID_ALL      3 ==> ID by ref. and position
				   GT_ID_REF_URI 11 ==> (1) + URI info 
				   GT_ID_POS_URI 12 ==> (2) + URI info       */
	   int      *status    )/* return status code                        */

/* 
   create a grouping table at the end of the current FITS file. This
   function makes the last HDU in the file the CHDU, then calls the
   fits_insert_group() function to actually create the new grouping table.
*/

{
  int hdutype;
  int hdunum;


  if(*status != 0) return(*status);


  *status = fits_get_num_hdus(fptr,&hdunum,status);

  *status = fits_movabs_hdu(fptr,hdunum,&hdutype,status);

  *status = fits_insert_group(fptr,grpname,grouptype,status);

  return(*status);
}

/*---------------------------------------------------------------------------*/
int ffgtis(fitsfile *fptr,      /* FITS file pointer                         */
	   char    *grpname,    /* name of the grouping table                */
	   int      grouptype,  /* code specifying the type of
				   grouping table information:
				   GT_ID_ALL_URI  0 ==> defualt (all columns)
				   GT_ID_REF      1 ==> ID by reference
				   GT_ID_POS      2 ==> ID by position
				   GT_ID_ALL      3 ==> ID by ref. and position
				   GT_ID_REF_URI 11 ==> (1) + URI info 
				   GT_ID_POS_URI 12 ==> (2) + URI info       */
	   int      *status)     /* return status code                       */
	   
/* 
   insert a grouping table just after the current HDU of the current FITS file.
   This is the same as fits_create_group() only it allows the user to select
   the place within the FITS file to add the grouping table.
*/

{

  int tfields  = 0;
  int hdunum   = 0;
  int hdutype  = 0;
  int extver   = 0;
  int i;
  
  long pcount  = 0;

  char *ttype[6];
  char *tform[6];

  char ttypeBuff[102];  
  char tformBuff[54];  

  char  extname[] = "GROUPING";
  char  keyword[FLEN_KEYWORD];
  char  keyvalue[FLEN_VALUE];
  char  comment[FLEN_COMMENT];
    
  do
    {

      /* set up the ttype and tform character buffers */

      for(i = 0; i < 6; ++i)
	{
	  ttype[i] = ttypeBuff+(i*17);
	  tform[i] = tformBuff+(i*9);
	}

      /* define the columns required according to the grouptype parameter */

      *status = ffgtdc(grouptype,0,0,0,0,0,0,ttype,tform,&tfields,status);

      /* create the grouping table using the columns defined above */

      *status = fits_insert_btbl(fptr,0,tfields,ttype,tform,NULL,
				 NULL,pcount,status);

      if(*status != 0) continue;

      /*
	 retrieve the hdu position of the new grouping table for
	 future use
      */

      fits_get_hdu_num(fptr,&hdunum);

      /*
	 add the EXTNAME and EXTVER keywords to the HDU just after the 
	 TFIELDS keyword; for now the EXTVER value is set to 0, it will be 
	 set to the correct value later on
      */

      fits_read_keyword(fptr,"TFIELDS",keyvalue,comment,status);

      fits_insert_key_str(fptr,"EXTNAME",extname,
			  "HDU contains a Grouping Table",status);
      fits_insert_key_lng(fptr,"EXTVER",0,"Grouping Table vers. (this file)",
			  status);

      /* 
	 if the grpname parameter value was defined (Non NULL and non zero
	 length) then add the GRPNAME keyword and value
      */

      if(grpname != NULL && strlen(grpname) > 0)
	fits_insert_key_str(fptr,"GRPNAME",grpname,"Grouping Table name",
			    status);

      /* 
	 add the TNULL keywords and values for each integer column defined;
	 integer null values are zero (0) for the MEMBER_POSITION and 
	 MEMBER_VERSION columns.
      */

      for(i = 0; i < tfields && *status == 0; ++i)
	{	  
	  if(strcasecmp(ttype[i],"MEMBER_POSITION") == 0 ||
	     strcasecmp(ttype[i],"MEMBER_VERSION")  == 0)
	    {
	      sprintf(keyword,"TFORM%d",i+1);
	      *status = fits_read_key_str(fptr,keyword,keyvalue,comment,
					  status);
	 
	      sprintf(keyword,"TNULL%d",i+1);

	      *status = fits_insert_key_lng(fptr,keyword,0,"Column Null Value",
					    status);
	    }
	}

      /*
	 determine the correct EXTVER value for the new grouping table
	 by finding the highest numbered grouping table EXTVER value
	 the currently exists
      */

      for(extver = 1;
	  (fits_movnam_hdu(fptr,ANY_HDU,"GROUPING",extver,status)) == 0; 
	  ++extver);

      if(*status == BAD_HDU_NUM) *status = 0;

      /*
	 move back to the new grouping table HDU and update the EXTVER
	 keyword value
      */

      fits_movabs_hdu(fptr,hdunum,&hdutype,status);

      fits_modify_key_lng(fptr,"EXTVER",extver,"&",status);

    }while(0);


  return(*status);
}

/*---------------------------------------------------------------------------*/
int ffgtch(fitsfile *gfptr,     /* FITS pointer to group                     */
	   int       grouptype, /* code specifying the type of
				   grouping table information:
				   GT_ID_ALL_URI  0 ==> defualt (all columns)
				   GT_ID_REF      1 ==> ID by reference
				   GT_ID_POS      2 ==> ID by position
				   GT_ID_ALL      3 ==> ID by ref. and position
				   GT_ID_REF_URI 11 ==> (1) + URI info 
				   GT_ID_POS_URI 12 ==> (2) + URI info       */
	   int      *status)     /* return status code                       */


/* 
   Change the grouping table structure of the grouping table pointed to by
   gfptr. The grouptype code specifies the new structure of the table. This
   operation only adds or removes grouping table columns, it does not add
   or delete group members (i.e., table rows). If the grouping table already
   has the desired structure then no operations are performed and function   
   simply returns with a (0) success status code. If the requested structure
   change creates new grouping table columns, then the column values for all
   existing members will be filled with the appropriate null values.
*/

{
  int xtensionCol, extnameCol, extverCol, positionCol, locationCol, uriCol;
  int ncols    = 0;
  int colnum   = 0;
  int nrows    = 0;
  int grptype  = 0;
  int i,j;

  long intNull  = 0;
  long tfields  = 0;
  
  char *tform[6];
  char *ttype[6];

  unsigned char  charNull[1] = {"\0"};

  char ttypeBuff[102];  
  char tformBuff[54];  

  char  keyword[FLEN_KEYWORD];
  char  keyvalue[FLEN_VALUE];
  char  comment[FLEN_COMMENT];


  if(*status != 0) return(*status);

  do
    {
      /* set up the ttype and tform character buffers */

      for(i = 0; i < 6; ++i)
	{
	  ttype[i] = ttypeBuff+(i*17);
	  tform[i] = tformBuff+(i*9);
	}

      /* retrieve positions of all Grouping table reserved columns */

      *status = ffgtgc(gfptr,&xtensionCol,&extnameCol,&extverCol,&positionCol,
		       &locationCol,&uriCol,&grptype,status);

      if(*status != 0) continue;

      /* determine the total number of grouping table columns */

      *status = fits_read_key_lng(gfptr,"TFIELDS",&tfields,comment,status);

      /* define grouping table columns to be added to the configuration */

      *status = ffgtdc(grouptype,xtensionCol,extnameCol,extverCol,positionCol,
		       locationCol,uriCol,ttype,tform,&ncols,status);

      /*
	delete any grouping tables columns that exist but do not belong to
	new desired configuration; note that we delete before creating new
	columns for (file size) efficiency reasons
      */

      switch(grouptype)
	{

	case GT_ID_ALL_URI:

	  /* no columns to be deleted in this case */

	  break;

	case GT_ID_REF:

	  if(positionCol != 0) 
	    {
	      *status = fits_delete_col(gfptr,positionCol,status);
	      --tfields;
	      if(uriCol      > positionCol)  --uriCol;
	      if(locationCol > positionCol) --locationCol;
	    }
	  if(uriCol      != 0)
	    { 
	    *status = fits_delete_col(gfptr,uriCol,status);
	      --tfields;
	      if(locationCol > uriCol) --locationCol;
	    }
	  if(locationCol != 0) 
	    *status = fits_delete_col(gfptr,locationCol,status);

	  break;

	case  GT_ID_POS:

	  if(xtensionCol != 0) 
	    {
	      *status = fits_delete_col(gfptr,xtensionCol,status);
	      --tfields;
	      if(extnameCol  > xtensionCol)  --extnameCol;
	      if(extverCol   > xtensionCol)  --extverCol;
	      if(uriCol      > xtensionCol)  --uriCol;
	      if(locationCol > xtensionCol)  --locationCol;
	    }
	  if(extnameCol  != 0) 
	    {
	      *status = fits_delete_col(gfptr,extnameCol,status);
	      --tfields;
	      if(extverCol   > extnameCol)  --extverCol;
	      if(uriCol      > extnameCol)  --uriCol;
	      if(locationCol > extnameCol)  --locationCol;
	    }
	  if(extverCol   != 0)
	    { 
	      *status = fits_delete_col(gfptr,extverCol,status);
	      --tfields;
	      if(uriCol      > extverCol)  --uriCol;
	      if(locationCol > extverCol)  --locationCol;
	    }
	  if(uriCol      != 0)
	    { 
	      *status = fits_delete_col(gfptr,uriCol,status);
	      --tfields;
	      if(locationCol > uriCol)  --locationCol;
	    }
	  if(locationCol != 0)
	    { 
	      *status = fits_delete_col(gfptr,locationCol,status);
	      --tfields;
	    }
	  
	  break;

	case  GT_ID_ALL:

	  if(uriCol      != 0) 
	    {
	      *status = fits_delete_col(gfptr,uriCol,status);
	      --tfields;
	      if(locationCol > uriCol)  --locationCol;
	    }
	  if(locationCol != 0)
	    { 
	      *status = fits_delete_col(gfptr,locationCol,status);
	      --tfields;
	    }

	  break;

	case GT_ID_REF_URI:

	  if(positionCol != 0)
	    { 
	      *status = fits_delete_col(gfptr,positionCol,status);
	      --tfields;
	    }

	  break;

	case  GT_ID_POS_URI:

	  if(xtensionCol != 0) 
	    {
	      *status = fits_delete_col(gfptr,xtensionCol,status);
	      --tfields;
	      if(extnameCol > xtensionCol)  --extnameCol;
	      if(extverCol  > xtensionCol)  --extverCol;
	    }
	  if(extnameCol  != 0)
	    { 
	      *status = fits_delete_col(gfptr,extnameCol,status);
	      --tfields;
	      if(extverCol > extnameCol)  --extverCol;
	    }
	  if(extverCol   != 0)
	    { 
	      *status = fits_delete_col(gfptr,extverCol,status);
	      --tfields;
	    }

	  break;

	default:

	  *status = BAD_OPTION;
	  ffpmsg("Invalid value for grouptype parameter specified (ffgtch)");
	  break;

	}

      /*
	add all the new grouping table columns that were not there
	previously but are called for by the grouptype parameter
      */

      for(i = 0; i < ncols && *status == 0; ++i)
	*status = fits_insert_col(gfptr,tfields+i+1,ttype[i],tform[i],status);

      /* 
	 add the TNULL keywords and values for each new integer column defined;
	 integer null values are zero (0) for the MEMBER_POSITION and 
	 MEMBER_VERSION columns. Insert a null ("/0") into each new string
	 column defined: MEMBER_XTENSION, MEMBER_NAME, MEMBER_URI_TYPE and
	 MEMBER_LOCATION. Note that by convention a null string is the
	 TNULL value for character fields so no TNULL is required.
      */

      for(i = 0; i < ncols && *status == 0; ++i)
	{	  
	  if(strcasecmp(ttype[i],"MEMBER_POSITION") == 0 ||
	     strcasecmp(ttype[i],"MEMBER_VERSION")  == 0)
	    {
	      /* col contains int data; set TNULL and insert 0 for each col */

	      *status = fits_get_colnum(gfptr,CASESEN,ttype[i],&colnum,
					status);
	      
	      sprintf(keyword,"TFORM%d",colnum);

	      *status = fits_read_key_str(gfptr,keyword,keyvalue,comment,
					  status);
	 
	      sprintf(keyword,"TNULL%d",colnum);

	      *status = fits_insert_key_lng(gfptr,keyword,0,
					    "Column Null Value",status);

	      for(j = 1; j <= nrows && *status == 0; ++j)
		*status = fits_write_col_lng(gfptr,colnum,j,1,1,&intNull,
					     status);
	    }
	  else if(strcasecmp(ttype[i],"MEMBER_XTENSION") == 0 ||
		  strcasecmp(ttype[i],"MEMBER_NAME")     == 0 ||
		  strcasecmp(ttype[i],"MEMBER_URI_TYPE") == 0 ||
		  strcasecmp(ttype[i],"MEMBER_LOCATION") == 0)
	    {

	      /* new col contains character data; insert NULLs into each col */

	      *status = fits_get_colnum(gfptr,CASESEN,ttype[i],&colnum,
					status);

	      for(j = 1; j <= nrows && *status == 0; ++j)
	    /* WILL THIS WORK FOR VAR LENTH CHAR COLS??????*/
		*status = fits_write_col_byt(gfptr,colnum,j,1,1,charNull,
					     status);
	    }
	}

    }while(0);

  return(*status);
}

/*---------------------------------------------------------------------------*/
int ffgtrm(fitsfile *gfptr,  /* FITS file pointer to group                   */
	   int       rmopt,  /* code specifying if member
				elements are to be deleted:
				OPT_RM_GPT ==> remove only group table
				OPT_RM_ALL ==> recursively remove members
				and their members (if groups)                */
	   int      *status) /* return status code                           */
	    
/*
  remove a grouping table, and optionally all its members. Any groups 
  containing the grouping table are updated, and all members (if not 
  deleted) have their GRPIDn and GRPLCn keywords updated accordingly. 
  If the (deleted) members are members of another grouping table then those
  tables are also updated. The CHDU of the FITS file pointed to by gfptr must 
  be positioned to the grouping table to be deleted.
*/

{
  int hdutype;

  long i;
  long nmembers = 0;

  HDUtracker HDU;
  

  if(*status != 0) return(*status);

  /*
     remove the grouping table depending upon the rmopt parameter
  */

  switch(rmopt)
    {

    case OPT_RM_GPT:

      /*
	 for this option, the grouping table is deleted, but the member
	 HDUs remain; in this case we only have to remove each member from
	 the grouping table by calling fits_remove_member() with the
	 OPT_RM_ENTRY option
      */

      /* get the number of members contained by this table */

      *status = fits_get_num_members(gfptr,&nmembers,status);

      /* loop over all grouping table members and remove them */

      for(i = nmembers; i > 0 && *status == 0; --i)
	*status = fits_remove_member(gfptr,i,OPT_RM_ENTRY,status);
      
	break;

    case OPT_RM_ALL:

      /*
	for this option the entire Group is deleted -- this includes all
	members and their members (if grouping tables themselves). Call 
	the recursive form of this function to perform the removal.
      */

      /* add the current grouping table to the HDUtracker struct */

      HDU.nHDU = 0;

      *status = fftsad(gfptr,&HDU,NULL,NULL);

      /* call the recursive group remove function */

      *status = ffgtrmr(gfptr,&HDU,status);

      /* free the memory allocated to the HDUtracker struct */

      for(i = 0; i < HDU.nHDU; ++i)
	{
	  free(HDU.filename[i]);
	  free(HDU.newFilename[i]);
	}

      break;

    default:
      
      *status = BAD_OPTION;
      ffpmsg("Invalid value for the rmopt parameter specified (ffgtrm)");
      break;

     }

  /*
     if all went well then unlink and delete the grouping table HDU
  */

  *status = ffgmul(gfptr,0,status);

  *status = fits_delete_hdu(gfptr,&hdutype,status);
      
  return(*status);
}

/*---------------------------------------------------------------------------*/
int ffgtcp(fitsfile *infptr,  /* input FITS file pointer                     */
	   fitsfile *outfptr, /* output FITS file pointer                    */
	   int        cpopt,  /* code specifying copy options:
				OPT_GCP_GPT (0) ==> copy only grouping table
				OPT_GCP_ALL (2) ==> recusrively copy members 
				                    and their members (if 
						    groups)                  */
	   int      *status)  /* return status code                          */

/*
  copy a grouping table, and optionally all its members, to a new FITS file.
  If the cpopt is set to OPT_GCP_GPT (copy grouping table only) then the 
  existing members have their GRPIDn and GRPLCn keywords updated to reflect 
  the existance of the new group, since they now belong to another group. If 
  cpopt is set to OPT_GCP_ALL (copy grouping table and members recursively) 
  then the original members are not updated; the new grouping table is 
  modified to include only the copied member HDUs and not the original members.

  Note that the recursive version of this function, ffgtcpr(), is called
  to perform the group table copy. In the case of cpopt == OPT_GCP_GPT
  ffgtcpr() does not actually use recursion.
*/

{
  int i;

  HDUtracker HDU;


  if(*status != 0) return(*status);

  /* make sure infptr and outfptr are not the same pointer */

  if(infptr == outfptr) *status = IDENTICAL_POINTERS;
  else
    {

      /* initialize the HDUtracker struct */
      
      HDU.nHDU = 0;
      
      *status = fftsad(infptr,&HDU,NULL,NULL);
      
      /* 
	 call the recursive form of this function to copy the grouping table. 
	 If the cpopt is OPT_GCP_GPT then there is actually no recursion
	 performed
      */

      *status = ffgtcpr(infptr,outfptr,cpopt,&HDU,status);
  
      /* free memory allocated for the HDUtracker struct */

      for(i = 0; i < HDU.nHDU; ++i) 
	{
	  free(HDU.filename[i]);
	  free(HDU.newFilename[i]);
	}
    }

  return(*status);
}

/*---------------------------------------------------------------------------*/
int ffgtmg(fitsfile *infptr,  /* FITS file ptr to source grouping table      */
	   fitsfile *outfptr, /* FITS file ptr to target grouping table      */
	   int       mgopt,   /* code specifying merge options:
				 OPT_MRG_COPY (0) ==> copy members to target
				                      group, leaving source 
						      group in place
				 OPT_MRG_MOV  (1) ==> move members to target
				                      group, source group is
						      deleted after merge    */
	   int      *status)   /* return status code                         */
     

/*
  merge two grouping tables by combining their members into a single table. 
  The source grouping table must be the CHDU of the fitsfile pointed to by 
  infptr, and the target grouping table must be the CHDU of the fitsfile to by 
  outfptr. All members of the source grouping table shall be copied to the
  target grouping table. If the mgopt parameter is OPT_MRG_COPY then the source
  grouping table continues to exist after the merge. If the mgopt parameter
  is OPT_MRG_MOV then the source grouping table is deleted after the merge, 
  and all member HDUs are updated accordingly.
*/
{
  long i ;
  long nmembers = 0;

  fitsfile *tmpfptr = NULL;


  if(*status != 0) return(*status);

  do
    {

      *status = fits_get_num_members(infptr,&nmembers,status);

      for(i = 1; i <= nmembers && *status == 0; ++i)
	{
	  *status = fits_open_member(infptr,i,&tmpfptr,status);
	  *status = fits_add_group_member(outfptr,tmpfptr,0,status);

	  if(*status == HDU_ALREADY_MEMBER) *status = 0;

	  if(tmpfptr != NULL)
	    {
	      fits_close_file(tmpfptr,status);
	      tmpfptr = NULL;
	    }
	}

      if(*status != 0) continue;

      if(mgopt == OPT_MRG_MOV) 
	*status = fits_remove_group(infptr,OPT_RM_GPT,status);

    }while(0);

  if(tmpfptr != NULL)  fits_close_file(tmpfptr,status);

  return(*status);
}

/*---------------------------------------------------------------------------*/
int ffgtcm(fitsfile *gfptr,  /* FITS file pointer to grouping table          */
	   int       cmopt,  /* code specifying compact options
				OPT_CMT_MBR      (1) ==> compact only direct 
			                                 members (if groups)
				OPT_CMT_MBR_DEL (11) ==> (1) + delete all 
				                         compacted groups    */
	   int      *status) /* return status code                           */
    
/*
  "Compact" a group pointed to by the FITS file pointer gfptr. This 
  is achieved by flattening the tree structure of a group and its 
  (grouping table) members. All members HDUs of a grouping table which is 
  itself a member of the grouping table gfptr are added to gfptr. Optionally,
  the grouping tables which are "compacted" are deleted. If the grouping 
  table contains no members that are themselves grouping tables then this 
  function performs a NOOP.
*/

{
  long i;
  long nmembers = 0;

  char keyvalue[FLEN_VALUE];
  char comment[FLEN_COMMENT];

  fitsfile *mfptr = NULL;


  if(*status != 0) return(*status);

  do
    {
      if(cmopt != OPT_CMT_MBR && cmopt != OPT_CMT_MBR_DEL)
	{
	  *status = BAD_OPTION;
	  ffpmsg("Invalid value for cmopt parameter specified (ffgtcm)");
	  continue;
	}

      /* reteive the number of grouping table members */

      *status = fits_get_num_members(gfptr,&nmembers,status);

      /*
	loop over all the grouping table members; if the member is a 
	grouping table then merge its members with the parent grouping 
	table 
      */

      for(i = 1; i <= nmembers && *status == 0; ++i)
	{
	  *status = fits_open_member(gfptr,i,&mfptr,status);

	  if(*status != 0) continue;

	  *status = fits_read_key_str(mfptr,"EXTNAME",keyvalue,comment,status);

	  /* if no EXTNAME keyword then cannot be a grouping table */

	  if(*status == KEY_NO_EXIST) 
	    {
	      *status = 0;
	      continue;
	    }
	  prepare_keyvalue(keyvalue);

	  if(*status != 0) continue;

	  /* if EXTNAME == "GROUPING" then process member as grouping table */

	  if(strcasecmp(keyvalue,"GROUPING") == 0)
	    {
	      /* merge the member (grouping table) into the grouping table */

	      *status = fits_merge_groups(mfptr,gfptr,OPT_MRG_COPY,status);

	      *status = fits_close_file(mfptr,status);
	      mfptr   = NULL;

	      /* 
		 remove the member from the grouping table now that all of
		 its members have been transferred; if cmopt is set to
		 OPT_CMT_MBR_DEL then remove and delete the member
	      */

	      if(cmopt == OPT_CMT_MBR)
		*status = fits_remove_member(gfptr,i,OPT_RM_ENTRY,status);
	      else
		*status = fits_remove_member(gfptr,i,OPT_RM_MBR,status);
	    }
	  else
	    {
	      /* not a grouping table; just close the opened member */

	      *status = fits_close_file(mfptr,status);

	      mfptr   = NULL;
	    }
	}

    }while(0);

  return(*status);
}

/*--------------------------------------------------------------------------*/
int ffgtvf(fitsfile *gfptr,       /* FITS file pointer to group             */
	   long     *firstfailed, /* Member ID (if positive) of first failed
				     member HDU verify check or GRPID index
				     (if negitive) of first failed group
				     link verify check.                     */
	   int      *status)      /* return status code                     */

/*
 check the integrity of a grouping table to make sure that all group members 
 are accessible and all the links to other grouping tables are valid. The
 firstfailed parameter returns the member ID of the first member HDU to fail
 verification if positive or the first group link to fail if negative; 
 otherwise firstfailed contains a return value of 0.
*/

{
  long i;
  long nmembers = 0;
  long ngroups  = 0;

  char errstr[FLEN_VALUE];

  fitsfile *fptr = NULL;


  if(*status != 0) return(*status);

  *firstfailed = 0;

  do
    {
      /*
	attempt to open all the members of the grouping table. We stop
	at the first member which cannot be opened (which implies that it
	cannot be located)
      */

      *status = fits_get_num_members(gfptr,&nmembers,status);

      for(i = 1; i <= nmembers && *status == 0; ++i)
	{
	  *status = fits_open_member(gfptr,i,&fptr,status);
	  fits_close_file(fptr,status);
	}

      /*
	if the status is non-zero from the above loop then record the
	member index that caused the error
      */

      if(*status != 0)
	{
	  *firstfailed = i;
	  sprintf(errstr,"Group table verify failed for member %ld (ffgtvf)",
		  i);
	  ffpmsg(errstr);
	  continue;
	}

      /*
	attempt to open all the groups linked to this grouping table. We stop
	at the first group which cannot be opened (which implies that it
	cannot be located)
      */

      *status = fits_get_num_groups(gfptr,&ngroups,status);

      for(i = 1; i <= ngroups && *status == 0; ++i)
	{
	  *status = fits_open_group(gfptr,i,&fptr,status);
	  fits_close_file(fptr,status);
	}

      /*
	if the status from the above loop is non-zero, then record the
	GRPIDn index of the group that caused the failure
      */

      if(*status != 0)
	{
	  *firstfailed = -1*i;
	  sprintf(errstr,
		  "Group table verify failed for GRPID index %ld (ffgtvf)",i);
	  ffpmsg(errstr);
	  continue;
	}

    }while(0);

  return(*status);
}

/*---------------------------------------------------------------------------*/
int ffgtop(fitsfile *mfptr,  /* FITS file pointer to the member HDU          */
	   int       grpid,  /* group ID (GRPIDn index) within member HDU    */
	   fitsfile **gfptr, /* FITS file pointer to grouping table HDU      */
	   int      *status) /* return status code                           */

/*
  open the grouping table that contains the member HDU. The member HDU must
  be the CHDU of the FITS file pointed to by mfptr, and the grouping table
  is identified by the Nth index number of the GRPIDn keywords specified in 
  the member HDU's header. The fitsfile gfptr pointer is positioned with the
  appropriate FITS file with the grouping table as the CHDU. If the group
  grouping table resides in a file other than the member then an attempt
  is first made to open the file readwrite, and failing that readonly.
 
  Note that it is possible for the GRPIDn/GRPLCn keywords in a member 
  header to be non-continuous, e.g., GRPID1, GRPID2, GRPID5, GRPID6. In 
  such cases, the grpid index value specified in the function call shall
  identify the (grpid)th GRPID value. In the above example, if grpid == 3,
  then the group specified by GRPID5 would be opened.
*/
{

  long ngroups   = 0;
  long grpExtver = 0;

  char keyword[FLEN_KEYWORD];
  char keyvalue[FLEN_VALUE];
  char comment[FLEN_COMMENT];


  if(*status != 0) return(*status);

  do
    {

      /*
	make sure that the group ID requested is valid ==> cannot be
	larger than the number of GRPIDn keywords in the member HDU header
      */

      *status = fits_get_num_groups(mfptr,&ngroups,status);

      if(grpid > ngroups)
	{
	  *status = BAD_GROUP_ID;
	  sprintf(comment,
		  "GRPID index %d larger total GRPID keywords %ld (ffgtop)",
		  grpid,ngroups);
	  ffpmsg(comment);
	  continue;
	}

      /*
	find the (grpid)th group that the member HDU belongs to and read
	the value of the GRPID(grpid) keyword; fits_get_num_groups()
	automatically re-enumerates the GRPIDn/GRPLCn keywords to fill in
	any gaps
      */

      sprintf(keyword,"GRPID%d",grpid);

      *status = fits_read_key_lng(mfptr,keyword,&grpExtver,comment,status);

      if(*status != 0) continue;

      /*
	if the value of the GRPIDn keyword is positive then the member is
	in the same FITS file as the grouping table and we only have to
	reopen the current FITS file. Else the member and grouping table
	HDUs reside in different files and another FITS file must be opened
	as specified by the corresponding GRPLCn keyword
      */

      if(grpExtver > 0) 
	*status = fits_reopen_file(mfptr,gfptr,status);

      else if(grpExtver < 0)
	{
	  /* 
	     the grouping table must reside in another FITS file;
	     search for the corresponding GRPLCn keyword 
	  */

	  sprintf(keyword,"GRPLC%d",grpid);
	  *status = fits_read_key_str(mfptr,keyword,keyvalue,comment,status);

	  /* if the GRPLCn keyword was not found then there is a problem */

	  if(*status == KEY_NO_EXIST)
	    {
	      *status = BAD_GROUP_ID;
	      sprintf(comment,"Cannot find GRPLC%d keyword (ffgtop)",grpid);
	      ffpmsg(comment);
	      continue;
	    }
	  prepare_keyvalue(keyvalue);

	  /* open the FITS file containing the grouping table READWRITE */

	  *status = fits_open_file(gfptr,keyvalue,READWRITE,status);

	  /* if READWRITE failed then try opening it READONLY */

	  if(*status != 0)
	    {
	      *status = 0;
	      *status = fits_open_file(gfptr,keyvalue,READONLY,status);
	    }

	  /* set the grpExtver value positive */

	  grpExtver = -1*grpExtver;
	}
      else
	{
	  /* the GRPIDn value must be zero (0) which is undefined */

	  *status = BAD_GROUP_ID;
	  sprintf(comment,"Invalid value of %ld for GRPID%d (ffgtop)",
		  grpExtver,grpid);
	  ffpmsg(comment);
	}

      if(*status != 0) continue;

      /* search for the grouping table in its FITS file */

      *status = fits_movnam_hdu(*gfptr,ANY_HDU,"GROUPING",(int)grpExtver,
				status);

      if(*status != 0) *status = GROUP_NOT_FOUND;

    }while(0);

  if(*status != 0 && *gfptr != NULL) fits_close_file(*gfptr,status);

  return(*status);
}
/*---------------------------------------------------------------------------*/
int ffgtam(fitsfile *gfptr,   /* FITS file pointer to grouping table HDU     */
	   fitsfile *mfptr,   /* FITS file pointer to member HDU             */
	   int       hdupos,  /* member HDU position IF in the same file as
			         the grouping table AND mfptr == NULL        */
	   int      *status)  /* return status code                          */
 
/*
  add a member HDU to an existing grouping table. The fitsfile pointer gfptr
  must be positioned with the grouping table as the CHDU. The member HDU
  may either be identifed with the fitsfile *mfptr (which must be positioned
  to the member HDU) or the hdupos parameter (the HDU number of the member 
  HDU) if both reside in the same FITS file. The hdupos value is only used
  if the mfptr parameter has a value of NULL (0). The new member HDU shall 
  have the appropriate GRPIDn and GRPLCn keywords created in its header.

  Note that if the member HDU to be added to the grouping table is already
  a member of the group then it will not be added a sceond time.
*/

{
  int xtensionCol,extnameCol,extverCol,positionCol,locationCol,uriCol;
  int memberPosition = 0;
  int grptype        = 0;
  int hdutype        = 0;
  int useLocation    = 0;
  int nkeys          = 6;
  int i;

  long memberExtver = 0;
  long groupExtver  = 0;
  long memberID     = 0;
  long nmembers     = 0;
  long ngroups      = 0;

  char memberFileName[FLEN_FILENAME];
  char groupFileName[FLEN_FILENAME];
  char memberHDUtype[FLEN_VALUE];
  char memberExtname[FLEN_VALUE];
  char memberURI[] = "URL";

  char *keys[] = {"GRPNAME","EXTVER","EXTNAME","TFIELDS","GCOUNT","EXTEND"};
  char *tmpPtr[1];

  char keyword[FLEN_KEYWORD];
  char card[FLEN_CARD];

  unsigned char charNull[]  = {"\0"};

  fitsfile *tmpfptr = NULL;

  if(*status != 0) return(*status);

  do
    {
      
      /*
	 if the calling function supplied the HDU position of the member
	 HDU instead of fitsfile pointer then get a fitsfile pointer
      */

      if(mfptr == NULL)
	{
	  *status = fits_reopen_file(gfptr,&tmpfptr,status);
	  *status = fits_movabs_hdu(tmpfptr,hdupos,&hdutype,status);

	  if(*status != 0) continue;
	}
      else
	tmpfptr = mfptr;

      /*
	 determine all the information about the member HDU that will
	 be needed later; note that we establish the default values for
	 all information values that are not explicitly found
      */

      *status = fits_read_key_str(tmpfptr,"XTENSION",memberHDUtype,card,
				  status);

      if(*status == KEY_NO_EXIST) 
	{
	  strcpy(memberHDUtype,"PRIMARY");
	  *status = 0;
	}
      prepare_keyvalue(memberHDUtype);

      *status = fits_read_key_lng(tmpfptr,"EXTVER",&memberExtver,card,status);

      if(*status == KEY_NO_EXIST) 
	{
	  memberExtver = 1;
	  *status      = 0;
	}

      *status = fits_read_key_str(tmpfptr,"EXTNAME",memberExtname,card,
				  status);

      if(*status == KEY_NO_EXIST) 
	{
	  memberExtname[0] = 0;
	  *status          = 0;
	}
      prepare_keyvalue(memberExtname);

      fits_get_hdu_num(tmpfptr,&memberPosition);

      /* retrieve and construct the member HDU's file name */

      *status = fits_file_name(tmpfptr,memberFileName,status);

      if(*status != 0) continue;

      ffgtcn(memberFileName);

      /* retrieve and construct the grouping table's file name */

      *status = fits_file_name(gfptr,groupFileName,status);

      ffgtcn(groupFileName);
      
      /*
	 if the member HDU and grouping table reside in the same FITS file
	 then there is no need to add the location and URI information to
	 the grouping table
      */

      if(strcmp(groupFileName,memberFileName) == 0)
	useLocation = 0;
      else
	useLocation = 1;

      /* retrieve the grouping table's EXTVER value */

      *status = fits_read_key_lng(gfptr,"EXTVER",&groupExtver,card,status);

      /* retrieve the number of group members */

      *status = fits_get_num_members(gfptr,&nmembers,status);
	      
      /*
	 make sure the member HDU is not already an entry in the
	 grouping table before adding it
      */

      *status = ffgmf(gfptr,memberHDUtype,memberExtname,memberExtver,
		      memberPosition,memberFileName,&memberID,status);

      if(*status == MEMBER_NOT_FOUND) *status = 0;
      else if(*status == 0)
	{  
	  *status = HDU_ALREADY_MEMBER;
    ffpmsg("Specified HDU is already a member of the Grouping table (ffgtam)");
	  continue;
	}
      else continue;

      /*
	 if the member HDU is not already recorded in the grouping table
	 then add it 
      */

      /* add a new row to the grouping table */

      *status = fits_insert_rows(gfptr,nmembers,1,status);
      ++nmembers;

      /* retrieve the grouping table column IDs and structure type */

      *status = ffgtgc(gfptr,&xtensionCol,&extnameCol,&extverCol,&positionCol,
		       &locationCol,&uriCol,&grptype,status);

      /* fill in the member HDU data in the new grouping table row */

      *tmpPtr = memberHDUtype; 

      if(xtensionCol != 0)
	fits_write_col_str(gfptr,xtensionCol,nmembers,1,1,tmpPtr,status);

      *tmpPtr = memberExtname; 

      if(extnameCol  != 0)
	{
	  if(strlen(memberExtname) != 0)
	    fits_write_col_str(gfptr,extnameCol,nmembers,1,1,tmpPtr,status);
	  else
	    /* WILL THIS WORK FOR VAR LENTH CHAR COLS??????*/
	    fits_write_col_byt(gfptr,extnameCol,nmembers,1,1,charNull,status);
	}

      if(extverCol   != 0)
	fits_write_col_lng(gfptr,extverCol,nmembers,1,1,&memberExtver,
			   status);

      if(positionCol != 0)
	fits_write_col_lng(gfptr,positionCol,nmembers,1,1,
			   (long *)&memberPosition,status);

      *tmpPtr = memberFileName; 

      if(locationCol != 0)
	{
	  if(useLocation != 0)
	    fits_write_col_str(gfptr,locationCol,nmembers,1,1,tmpPtr,status);
	  else
	    /* WILL THIS WORK FOR VAR LENTH CHAR COLS??????*/
	    fits_write_col_byt(gfptr,locationCol,nmembers,1,1,charNull,status);
	}

      *tmpPtr = memberURI;

      if(uriCol      != 0)
	{
	  if(useLocation != 0)
	    fits_write_col_str(gfptr,uriCol,nmembers,1,1,tmpPtr,status);
	  else
	    /* WILL THIS WORK FOR VAR LENTH CHAR COLS??????*/
	    fits_write_col_byt(gfptr,uriCol,nmembers,1,1,charNull,status);
	}

      /*
	 add GRPIDn/GRPLCn keywords to the member HDU header to link
	 it to the grouing table
      */

      *status = fits_get_num_groups(tmpfptr,&ngroups,status);

      /* position the write pointer to the correct header insert position */

      if(ngroups > 0)
	{
	  /* find the last GRPIDn/GRPLCn keyword in the header */

	  sprintf(keyword,"GRPLC%d",(int)ngroups);
	  *status = fits_read_card(tmpfptr,keyword,card,status);

	  if(*status == KEY_NO_EXIST)
	    {
	      *status = 0;
	      sprintf(keyword,"GRPID%d",(int)ngroups);
	      *status = fits_read_card(tmpfptr,keyword,card,status);
	    }
	}
      else
	{
	  /* no GRPIDn/GRPLCn keywords currently exist in header */

	  for(i = 0, *status = KEY_NO_EXIST; 
	                            i < nkeys && *status == KEY_NO_EXIST; ++i)
	    {
	      *status = 0;
	      *status = fits_read_card(tmpfptr,keys[i],card,status);
	    }

	  /* all else fails: move write pointer to end of header */

	  if(*status == KEY_NO_EXIST)
	    {
	      fits_get_hdrspace(tmpfptr,&nkeys,&i,status);
	      ffgrec(tmpfptr,nkeys,card,status);
	    }

	  /* any other error status then abort */

	  if(*status != 0) continue;
	}
      
      /* increment the number of group links counter for the member HDU */

      ++ngroups;

      /*
	 if the member HDU and grouping table reside in the same FITS file
	 then there is no need to add a GRPLCn keyword
      */

      if(useLocation == 0)
	{
	  sprintf(keyword,"GRPID%d",(int)ngroups);
	  fits_insert_key_lng(tmpfptr,keyword,groupExtver,
			      "EXTVER of Group containing this HDU",status);
	}
      else
	{
	  sprintf(keyword,"GRPID%d",(int)ngroups);
	  fits_insert_key_lng(tmpfptr,keyword,groupExtver*(-1),
			      "EXTVER of Group containing this HDU",status);

	  sprintf(keyword,"GRPLC%d",(int)ngroups);
	  fits_insert_key_str(tmpfptr,keyword,groupFileName,
			      "URL of file containing Group",status);
	}

    }while(0);

  /* close the tmpfptr pointer if it was opened in this function */

  if(mfptr == NULL)
    *status = fits_close_file(tmpfptr,status);

  return(*status);
}

/*---------------------------------------------------------------------------*/
int ffgtnm(fitsfile *gfptr,    /* FITS file pointer to grouping table        */
	   long     *nmembers, /* member count  of the groping table         */
	   int      *status)   /* return status code                         */

/*
  return the number of member HDUs in a grouping table. The fitsfile pointer
  gfptr must be positioned with the grouping table as the CHDU. The number
  of grouping table member HDUs is just the NAXIS2 value of the grouping
  table.
*/

{
  char keyvalue[FLEN_VALUE];
  char comment[FLEN_COMMENT];
  

  if(*status != 0) return(*status);

  *status = fits_read_keyword(gfptr,"EXTNAME",keyvalue,comment,status);
  
  if(*status == KEY_NO_EXIST)
    *status = NOT_GROUP_TABLE;
  else
    {
      prepare_keyvalue(keyvalue);

      if(strcasecmp(keyvalue,"GROUPING") != 0)
	{
	  *status = NOT_GROUP_TABLE;
	  ffpmsg("Specified HDU is not a Grouping table (ffgtnm)");
	}

      *status = fits_read_key_lng(gfptr,"NAXIS2",nmembers,comment,status);
    }

  return(*status);
}

/*--------------------------------------------------------------------------*/
int ffgmng(fitsfile *mfptr,   /* FITS file pointer to member HDU            */
	   long     *ngroups, /* total number of groups linked to HDU       */
	   int      *status)  /* return status code                         */

/*
  return the number of groups to which a HDU belongs, as defined by the number
  of GRPIDn/GRPLCn keyword records that appear in the HDU header. The 
  fitsfile pointer mfptr must be positioned with the member HDU as the CHDU. 
  Each time this function is called, the indicies of the GRPIDn/GRPLCn
  keywords are checked to make sure they are continuous (ie no gaps) and
  are re-enumerated to eliminate gaps if gaps are found to be present.
*/

{
  int offset;
  int index;
  int newIndex;
  int i;
  
  long grpid;

  char *inclist[] = {"GRPID#"};
  char keyword[FLEN_KEYWORD];
  char newKeyword[FLEN_KEYWORD];
  char card[FLEN_CARD];

  if(*status != 0) return(*status);

  *ngroups = 0;

  /* reset the member HDU keyword counter to the beginning */

  *status = ffgrec(mfptr,0,card,status);
  
  /*
    search for the number of GRPIDn keywords in the member HDU header
    and count them with the ngroups variable
  */
  
  while(*status == 0)
    {
      /* read the next GRPIDn keyword in the series */

      *status = fits_find_nextkey(mfptr,inclist,1,NULL,0,card,status);
      
      if(*status != 0) continue;
      
      ++(*ngroups);
    }

  if(*status == KEY_NO_EXIST) *status = 0;
      
  /*
     read each GRPIDn/GRPLCn keyword and adjust their index values so that
     there are no gaps in the index count
  */

  for(index = 1, offset = 0, i = 1; i <= *ngroups && *status == 0; ++index)
    {	  
      sprintf(keyword,"GRPID%d",index);

      /* try to read the next GRPIDn keyword in the series */

      *status = fits_read_key_lng(mfptr,keyword,&grpid,card,status);

      /* if not found then increment the offset counter and continue */

      if(*status == KEY_NO_EXIST) 
	{
	  *status = 0;
	  ++offset;
	}
      else
	{
	  /* 
	     increment the number_keys_found counter and see if the index
	     of the keyword needs to be updated
	  */

	  ++i;

	  if(offset > 0)
	    {
	      /* compute the new index for the GRPIDn/GRPLCn keywords */
	      newIndex = index - offset;

	      /* update the GRPIDn keyword index */

	      sprintf(newKeyword,"GRPID%d",newIndex);
	      fits_modify_name(mfptr,keyword,newKeyword,status);

	      /* If present, update the GRPLCn keyword index */

	      sprintf(keyword,"GRPLC%d",index);
	      sprintf(newKeyword,"GRPLC%d",newIndex);
	      fits_modify_name(mfptr,keyword,newKeyword,status);

	      if(*status == KEY_NO_EXIST) *status = 0;
	    }
	}
    }

  return(*status);
}

/*---------------------------------------------------------------------------*/
int ffgmop(fitsfile *gfptr,  /* FITS file pointer to grouping table          */
	   long      member, /* member ID (row num) within grouping table    */
	   fitsfile **mfptr, /* FITS file pointer to member HDU              */
	   int      *status) /* return status code                           */

/*
  open a grouping table member, returning a pointer to the member's FITS file
  with the CHDU set to the member HDU. The grouping table must be the CHDU of
  the FITS file pointed to by gfptr. The member to open is identified by its
  row number within the grouping table (first row/member == 1).

  Note that if the member resides in a FITS file different from the grouping
  table the member file is first opened readwrite and if this fails then
  it is opened readonly.

*/
{
  int xtensionCol,extnameCol,extverCol,positionCol,locationCol,uriCol;
  int grptype,hdutype;
  int dummy;
  
  long hdupos = 0;
  long extver = 0;

  char  xtension[FLEN_VALUE];
  char  extname[FLEN_VALUE];
  char  location[FLEN_FILENAME];
  char  uri[FLEN_VALUE];
  char  grpLocation[FLEN_FILENAME];
  char  card[FLEN_CARD];
  char  nstr[] = {"\0"};
  char *tmpPtr[1];

  if(*status != 0) return(*status);

  do
    {
      /*
	retrieve the Grouping Convention reserved column positions within
	the grouping table
      */

      *status = ffgtgc(gfptr,&xtensionCol,&extnameCol,&extverCol,&positionCol,
		       &locationCol,&uriCol,&grptype,status);

      if(*status != 0) continue;

      /*
	 extract the member information from grouping table
      */

      tmpPtr[0] = xtension;

      if(xtensionCol != 0)
	{

	  *status = fits_read_col_str(gfptr,xtensionCol,member,1,1,nstr,
				      tmpPtr,&dummy,status);

	  /* convert the xtension string to a hdutype code */

	  if(strcasecmp(xtension,"PRIMARY")       == 0) hdutype = IMAGE_HDU; 
	  else if(strcasecmp(xtension,"IMAGE")    == 0) hdutype = IMAGE_HDU; 
	  else if(strcasecmp(xtension,"TABLE")    == 0) hdutype = ASCII_TBL; 
	  else if(strcasecmp(xtension,"BINTABLE") == 0) hdutype = BINARY_TBL; 
	  else hdutype = ANY_HDU; 
	}

      tmpPtr[0] = extname;

      if(extnameCol  != 0)
	  *status = fits_read_col_str(gfptr,extnameCol,member,1,1,nstr,
				      tmpPtr,&dummy,status);

      if(extverCol   != 0)
	  *status = fits_read_col_lng(gfptr,extverCol,member,1,1,0,
				      (long*)&extver,&dummy,status);

      if(positionCol != 0)
	  *status = fits_read_col_lng(gfptr,positionCol,member,1,1,0,
				      (long*)&hdupos,&dummy,status);

      tmpPtr[0] = location;

      if(locationCol != 0)
	*status = fits_read_col_str(gfptr,locationCol,member,1,1,nstr,
				    tmpPtr,&dummy,status);
      tmpPtr[0] = uri;

      if(uriCol != 0)
	*status = fits_read_col_str(gfptr,uriCol,member,1,1,nstr,
				    tmpPtr,&dummy,status);

      if(*status != 0) continue;

      /* decide what FITS file the member HDU resides in */

      switch(grptype)
	{

	case GT_ID_POS:
	case GT_ID_REF:
	case GT_ID_ALL:

	  /*
	     no location information is given so we must assume that the
	     member HDU resides in the same FITS file as the grouping table
	  */

	  *status = fits_reopen_file(gfptr,mfptr,status);
	  
	  break;

	default:
	  break;

	case GT_ID_REF_URI:
	case GT_ID_POS_URI:
	case GT_ID_ALL_URI:

	  /*
	    The member location column exists; determine if the member 
	    resides in a separate file from the grouping table; if so, 
	    attempt to open it
	  */

	  if(strlen(location) != 0)
	    {

	      /*
		make sure the location specifiation is "URL"; we cannot
		decode any other URI types at this time
	      */

	      if(strcasecmp(uri,"URL") != 0)
		{
		  *status = FILE_NOT_OPENED;
		  sprintf(card,
		  "Cannot open member HDU file with URI type %s (ffgmop)",
			  uri);
		  ffpmsg(card);

		  continue;
		}

	      /*
		The location string for the member is not NULL, so it 
		does not necessecially reside in the same FITS file as the
		grouping table. Compare the location string to the 
		file name that contains the grouping table to see if
		they are the same ==> member and grouping table reside
		in the same file.

		The comparision between member HDU location and grouping
		table location is made by constructing the URL for each
		location, e.g., http://xxx.yyy.zzz/filepath/filename, and
		checking if they are the same string. Note that this 
		comparision is not unambiguous. For example, a file with
		name 'filename' containing the grouping table and the
		file containing the member with full URL 
                'http://xxx.yyy.zzz/AAA/filename' could actually be the
		same FITS file and yet this comparision would not
		reconize it
	      */

	      /* retrieve full input URL of grouping table file and parse */

	      *status = fits_file_name(gfptr,grpLocation,status);

	      ffgtcn(grpLocation);

	      /* parse the member location string and build URL */

	      ffgtcn(location);

	      /* string compare the group and member location URLs */
	      if(strcmp(grpLocation,location) == 0)
		{
		  /* 
		     The member HDU and grouping table reside in the 
		     same FITS file, so just reopen the current FITS
		     file
		  */
		  *status = fits_reopen_file(gfptr,mfptr,status);
		}
	      else
		{
		  /* 
		     The member HDU and grouping table reside in separate
		     FITS files. first try to open the member HDU's FITS 
		     file read/write mode; if that fails then try to
		     open it in readonly mode
		  */
		  *status = fits_open_file(mfptr,location,READWRITE,status);

		  if(*status != 0)
		    {
		      /* now try to open in readonly mode */ 
		      *status = 0;
		      *status = fits_open_file(mfptr,location,READONLY,status);
		    }
		}
	    }
	  else
	    {
	      /*
		 since no location information was given we must assume
		 that the member is in the same FITS file as the grouping
		 table
	      */

	      *status = fits_reopen_file(gfptr,mfptr,status);
	    }

	  break;

	}

      if(*status != 0) continue;

      /*
	 attempt to locate the member HDU within its FITS file as determined
	 and opened above
      */

      switch(grptype)
	{

	case GT_ID_POS:
	case GT_ID_POS_URI:

	  /*
	    try to find the member hdu in the the FITS file pointed to
	    by mfptr based upon its HDU posistion value. Note that is 
	    impossible to verify if the HDU is actually the correct HDU due 
	    to a lack of information.
	  */
	  
	  *status = fits_movabs_hdu(*mfptr,(int)hdupos,&hdutype,status);

	  break;

	case GT_ID_REF:
	case GT_ID_REF_URI:

	  /*
	     try to find the member hdu in the FITS file pointed to
	     by mfptr based upon its XTENSION, EXTNAME and EXTVER keyword 
	     values
	  */

	  *status = fits_movnam_hdu(*mfptr,hdutype,extname,extver,status);

	  if(*status == BAD_HDU_NUM) 
	    {
	      *status = MEMBER_NOT_FOUND;
	      ffpmsg("Cannot find specified member HDU (ffgmop)");
	    }

	  /*
	     if the above function returned without error then the
	     mfptr is pointed to the member HDU
	  */

	  break;

	case GT_ID_ALL:
	case GT_ID_ALL_URI:

	  /*
	     if the member entry has reference information then use it
             (ID by reference is safer than ID by position) else use
	     the position information
	  */

	  if(strlen(xtension) > 0 && strlen(extname) > 0 && extver > 0)
	    {
	      /* valid reference info exists so use it */
	      
	      /* try to find the member hdu in the grouping table's file */

	      *status = fits_movnam_hdu(*mfptr,hdutype,extname,extver,status);

	      if(*status == BAD_HDU_NUM) 
		{
		  *status = MEMBER_NOT_FOUND;
		  ffpmsg("Cannot find specified member HDU (ffgmop)");
		}
	    }
	  else
	      {
		  *status = fits_movabs_hdu(*mfptr,(int)hdupos,&hdutype,
					    status);
		  if(*status == END_OF_FILE) *status = MEMBER_NOT_FOUND;
	      }

	  /*
	     if the above function returned without error then the
	     mfptr is pointed to the member HDU
	  */

	  break;

	default:
	  break;

	}
      
    }while(0);

  if(*status != 0 && *mfptr != NULL) fits_close_file(*mfptr,status);

  return(*status);
}

/*---------------------------------------------------------------------------*/
int ffgmcp(fitsfile *gfptr,  /* FITS file pointer to group                   */
	   fitsfile *mfptr,  /* FITS file pointer to new member
				FITS file                                    */
	   long      member, /* member ID (row num) within grouping table    */
	   int       cpopt,  /* code specifying copy options:
				OPT_MCP_ADD  (0) ==> add copied member to the
 				                     grouping table
				OPT_MCP_NADD (1) ==> do not add member copy to
				                     the grouping table
				OPT_MCP_REPL (2) ==> replace current member
				                     entry with member copy  */
	   int      *status) /* return status code                           */
	   
/*
  copy a member HDU of a grouping table to a new FITS file. The grouping table
  must be the CHDU of the FITS file pointed to by gfptr. The copy of the
  group member shall be appended to the end of the FITS file pointed to by
  mfptr. If the cpopt parameter is set to OPT_MCP_ADD then the copy of the 
  member is added to the grouping table as a new member, if OPT_MCP_NADD 
  then the copied member is not added to the grouping table, and if 
  OPT_MCP_REPL then the copied member is used to replace the original member.
  The copied member HDU also has its EXTVER value updated so that its
  combination of XTENSION, EXTNAME and EXVTER is unique within its new
  FITS file.
*/

{
  int numkeys = 0;
  int keypos  = 0;
  int hdunum  = 0;
  int hdutype = 0;
  int i;
  
  char *incList[] = {"GRPID#","GRPLC#"};
  char  extname[FLEN_VALUE];
  char  card[FLEN_CARD];
  char  comment[FLEN_COMMENT];

  fitsfile *tmpfptr = NULL;


  if(*status != 0) return(*status);

  do
    {
      /* open the member HDU to be copied */

      *status = fits_open_member(gfptr,member,&tmpfptr,status);

      if(*status != 0) continue;

      /*
	if the member is a grouping table then copy it with a call to
	fits_copy_group() using the "copy only the grouping table" option

	if it is not a grouping table then copy the hdu with fits_copy_hdu()
	remove all GRPIDn and GRPLCn keywords, and update the EXTVER keyword
	value
      */

      /* get the member HDU's EXTNAME value */

      *status = fits_read_key_str(tmpfptr,"EXTNAME",extname,comment,status);

      /* if no EXTNAME value was found then set the extname to a null string */

      if(*status == KEY_NO_EXIST) 
	{
	  extname[0] = 0;
	  *status    = 0;
	}
      else if(*status != 0) continue;

      prepare_keyvalue(extname);

      /* if a grouping table then copy with fits_copy_group() */

      if(strcasecmp(extname,"GROUPING") == 0)
	*status = fits_copy_group(tmpfptr,mfptr,OPT_GCP_GPT,status);
      else
	{
	  /* copy the non-grouping table HDU the conventional way */

	  *status = fits_copy_hdu(tmpfptr,mfptr,0,status);

	  ffgrec(mfptr,0,card,status);

	  /* delete all the GRPIDn and GRPLCn keywords in the copied HDU */

	  while(*status == 0)
	    {
	      *status = fits_find_nextkey(mfptr,incList,2,NULL,0,card,status);
	      *status = fits_get_hdrpos(mfptr,&numkeys,&keypos,status);  
	      *status = fits_delete_record(mfptr,keypos-1,status);
	    }

	  if(*status == KEY_NO_EXIST) *status = 0;
	  if(*status != 0) continue;
	}

      /* 
	 if the member HDU does not have an EXTNAME keyword then add one
	 with a default value
      */

      if(strlen(extname) == 0)
	{
	  if(fits_get_hdu_num(tmpfptr,&hdunum) == 1)
	    {
	      strcpy(extname,"PRIMARY");
	      *status = fits_write_key_str(mfptr,"EXTNAME",extname,
					   "HDU was Formerly a Primary Array",
					   status);
	    }
	  else
	    {
	      strcpy(extname,"DEFAULT");
	      *status = fits_write_key_str(mfptr,"EXTNAME",extname,
					   "default EXTNAME set by CFITSIO",
					   status);
	    }
	}

      /* 
	 update the member HDU's EXTVER value (add it if not present)
      */

      fits_get_hdu_num(mfptr,&hdunum);
      fits_get_hdu_type(mfptr,&hdutype,status);

      /* set the EXTVER value to 0 for now */

      *status = fits_modify_key_lng(mfptr,"EXTVER",0,NULL,status);

      /* if the EXTVER keyword was not found then add it */

      if(*status == KEY_NO_EXIST)
	{
	  *status = 0;
	  *status = fits_read_key_str(mfptr,"EXTNAME",extname,comment,
				      status);
	  *status = fits_insert_key_lng(mfptr,"EXTVER",0,
					"Extension version ID",status);
	}

      if(*status != 0) continue;

      /* find the first available EXTVER value for the copied HDU */
 
      for(i = 1; fits_movnam_hdu(mfptr,hdutype,extname,i,status) == 0; ++i);

      *status = 0;

      fits_movabs_hdu(mfptr,hdunum,&hdutype,status);

      /* reset the copied member HDUs EXTVER value */

      *status = fits_modify_key_lng(mfptr,"EXTVER",(long)i,NULL,status);    

      /*
	perform member copy operations that are dependent upon the cpopt
	parameter value
      */

      switch(cpopt)
	{
	case OPT_MCP_ADD:

	  /*
	    add the copied member to the grouping table, leaving the
	    entry for the original member in place
	  */

	  *status = fits_add_group_member(gfptr,mfptr,0,status);

	  break;

	case OPT_MCP_NADD:

	  /*
	    nothing to do for this copy option
	  */

	  break;

	case OPT_MCP_REPL:

	  /*
	    remove the original member from the grouping table and add the
	    copied member in its place
	  */

	  *status = fits_remove_member(gfptr,member,OPT_RM_ENTRY,status);
	  *status = fits_add_group_member(gfptr,mfptr,0,status);

	  break;

	default:

	  *status = BAD_OPTION;
	  ffpmsg("Invalid value specified for the cmopt parameter (ffgmcp)");

	  break;
	}

    }while(0);
      
  if(tmpfptr != NULL) fits_close_file(tmpfptr,status);
  
  return(*status);
}		     

/*---------------------------------------------------------------------------*/
int ffgmtf(fitsfile *infptr,   /* FITS file pointer to source grouping table */
	   fitsfile *outfptr,  /* FITS file pointer to target grouping table */
	   long      member,   /* member ID within source grouping table     */
	   int       tfopt,    /* code specifying transfer opts:
				  OPT_MCP_ADD (0) ==> copy member to dest.
				  OPT_MCP_MOV (3) ==> move member to dest.   */
	   int      *status)   /* return status code                         */

/*
  transfer a group member from one grouping table to another. The source
  grouping table must be the CHDU of the fitsfile pointed to by infptr, and 
  the destination grouping table must be the CHDU of the fitsfile to by 
  outfptr. If the tfopt parameter is OPT_MCP_ADD then the member is made a 
  member of the target group and remains a member of the source group. If
  the tfopt parameter is OPT_MCP_MOV then the member is deleted from the 
  source group after the transfer to the destination group. The member to be
  transfered is identified by its row number within the source grouping table.
*/

{
  fitsfile *mfptr = NULL;


  if(*status != 0) return(*status);

  if(tfopt != OPT_MCP_MOV && tfopt != OPT_MCP_ADD)
    {
      *status = BAD_OPTION;
      ffpmsg("Invalid value specified for the tfopt parameter (ffgmtf)");
    }
  else
    {
      /* open the member of infptr to be transfered */

      *status = fits_open_member(infptr,member,&mfptr,status);
      
      /* add the member to the outfptr grouping table */
      
      *status = fits_add_group_member(outfptr,mfptr,0,status);
      
      /* close the member HDU */
      
      *status = fits_close_file(mfptr,status);
      
      /* 
	 if the tfopt is "move member" then remove it from the infptr 
	 grouping table
      */

      if(tfopt == OPT_MCP_MOV)
	*status = fits_remove_member(infptr,member,OPT_RM_ENTRY,status);
    }
  
  return(*status);
}

/*---------------------------------------------------------------------------*/
int ffgmrm(fitsfile *gfptr,  /* FITS file pointer to group table             */
	   long      member, /* member ID (row num) in the group             */
	   int       rmopt,  /* code specifying the delete option:
				OPT_RM_ENTRY ==> delete the member entry
				OPT_RM_MBR   ==> delete entry and member HDU */
	   int      *status)  /* return status code                          */

/*
  remove a member HDU from a grouping table. The fitsfile pointer gfptr must
  be positioned with the grouping table as the CHDU, and the member to 
  delete is identified by its row number in the table (first member == 1).
  The rmopt parameter determines if the member entry is deleted from the
  grouping table (in which case GRPIDn and GRPLCn keywords in the member 
  HDU's header shall be updated accordingly) or if the member HDU shall itself
  be removed from its FITS file.
*/

{
  int found     = 0;
  int hdutype   = 0;
  int index     = 0;

  long i;
  long ngroups      = 0;
  long nmembers     = 0;
  long groupExtver  = 0;
  long grpid        = 0;

  char groupFileName[FLEN_FILENAME];
  char keyword[FLEN_KEYWORD];
  char grplc[FLEN_VALUE];
  char keyvalue[FLEN_VALUE];
  char card[FLEN_CARD];

  fitsfile *mfptr  = NULL;


  if(*status != 0) return(*status);

  do
    {
      /* open the group member to be deleted */

      *status = fits_open_member(gfptr,member,&mfptr,status);

      /*
	 if the member HDU is to be deleted then call fits_unlink_member()
	 to remove it from all groups to which it belongs (including
	 this one) and then delete it. Note that if the member is a
	 grouping table then we have to recursively call fits_remove_member()
	 for each member of the member before we delete the member itself.
      */

      if(rmopt == OPT_RM_MBR)
	{
	    /* cannot delete a PHDU */
	    if(fits_get_hdu_num(mfptr,&hdutype) == 1)
		{
		    *status = BAD_HDU_NUM;
		    continue;
		}

	  /* determine if the member HDU is itself a grouping table */

	  *status = fits_read_key_str(mfptr,"EXTNAME",keyvalue,card,status);

	  /* if no EXTNAME is found then the HDU cannot be a grouping table */ 

	  if(*status == KEY_NO_EXIST) 
	    {
	      keyvalue[0] = 0;
	      *status = 0;
	    }
	  prepare_keyvalue(keyvalue);

	  /* Any other error is a reason to abort */

	  if(*status != 0) continue;

	  /* if the EXTNAME == GROUPING then the member is a grouping table */
	  
	  if(strcasecmp(keyvalue,"GROUPING") == 0)
	    {
	      /* remove each of the grouping table members */
	      
	      *status = fits_get_num_members(mfptr,&nmembers,status);
	      
	      for(i = nmembers; i > 0 && *status == 0; --i)
		*status = fits_remove_member(mfptr,i,OPT_RM_ENTRY,status);
	      
	      if(*status != 0) continue;
	    }

	  /* unlink the member HDU from all groups that contain it */

	  *status = ffgmul(mfptr,0,status);

	  if(*status != 0) continue;
 
	  /* reset the grouping table HDU struct */

	  fits_set_hdustruc(gfptr,status);

	  /* delete the member HDU */

	  *status = fits_delete_hdu(mfptr,&hdutype,status);
	}
      else if(rmopt == OPT_RM_ENTRY)
	{
	  /* 
	     The member HDU is only to be removed as an entry from this
	     grouping table. Actions are (1) find the GRPIDn/GRPLCn 
	     keywords that link the member to the grouping table, (2)
	     remove the GRPIDn/GRPLCn keyword from the member HDU header
	     and (3) remove the member entry from the grouping table
	  */

	  /* determine the group EXTVER value of the grouping table */

	  *status = fits_read_key_lng(gfptr,"EXTVER",&groupExtver,card,status);

	  /* retrieve input file names for member and grouping table HDUs */

	  *status = fits_file_name(gfptr,groupFileName,status);

	  if(*status != 0) continue;
	  
	  /* construct the grouping table FITS file name */

	  ffgtcn(groupFileName);

	  /*
	     determine the number of groups to which the member HDU belongs
	  */
	  
	  *status = fits_get_num_groups(mfptr,&ngroups,status);

	  /* reset the HDU keyword position counter to the beginning */

	  *status = ffgrec(mfptr,0,card,status);
	  
	  /*
	     loop over all the GRPIDn keywords in the member HDU header and
	     find the appropriate GRPIDn and GRPLCn keywords that identify it
	     as belonging to the group
	  */

	  for(index = 1, found = 0; index <= ngroups && *status == 0 && 
	                                                       !found; ++index)
	    {	  
	      /* read the next GRPIDn keyword in the series */

	      sprintf(keyword,"GRPID%d",index);

	      *status = fits_read_key_lng(mfptr,keyword,&grpid,card,status);
	      if(*status != 0) continue;
	      
	      /* grpid value == group EXTVER value then we have a match */

	      if(grpid > 0)
		{		
		  if(grpid == groupExtver) found = index;
		}
	      else
		{
		  /* have to look at the GRPLCn value to determine a match */

		  sprintf(keyword,"GRPLC%d",index);

		  *status = fits_read_key_str(mfptr,keyword,grplc,card,status);

		  if(*status == KEY_NO_EXIST)
		    {
		      *status = 0;
		      continue;
		    }
		  else if (*status != 0) continue;

		  /* construct the URL for the GRPLCn value */

		  prepare_keyvalue(grplc);
		  ffgtcn(grplc);

		  /*
		     if the absolute value of GRPIDn is equal to the
		     EXTVER value of the grouping table and the derived 
		     file name of the grouping table matches the
		     derived file name from the GRPLCn keyword value
		     then we hava a match
		   */

		  if(strcmp(grplc,groupFileName) == 0 && 
		                 groupExtver == -1*grpid) found = index; 
		}
	    }

	  /*
	     if found == 0 (false) after the above search then we assume 
	     that it is due to an inpromper updating of the GRPIDn and GRPLCn 
	     keywords in the member header ==> nothing to delete in the 
	     header. Else delete the GRPLCn and GRPIDn keywords that identify 
	     the member HDU with the group HDU and re-enumerate the 
	     remaining GRPIDn and GRPLCn keywords
	  */

	  if(found != 0)
	    {
	      sprintf(keyword,"GRPID%d",found);
	      *status = fits_delete_key(mfptr,keyword,status);
	      
	      sprintf(keyword,"GRPLC%d",found);
	      *status = fits_delete_key(mfptr,keyword,status);
	      
	      *status = 0;

	      /* call fits_get_num_groups() to re-enumerate the GRPIDn */

	      *status = fits_get_num_groups(mfptr,&ngroups,status);
	    }

	  /*
	     finally, remove the member entry from the current grouping table
	     pointed to by gfptr
	  */

	  *status = fits_delete_rows(gfptr,member,1,status);
	}
      else
	{
	  *status = BAD_OPTION;
	  ffpmsg("Invalid value specified for the rmopt parameter (ffgmrm)");
	}

    }while(0);

  if(mfptr != NULL) fits_close_file(mfptr,status);

  return(*status);
}

/*---------------------------------------------------------------------------*/
int ffgtgc(fitsfile *gfptr,  /* pointer to the grouping table                */
	   int *xtensionCol, /* column ID of the MEMBER_XTENSION column      */
	   int *extnameCol,  /* column ID of the MEMBER_NAME column          */
	   int *extverCol,   /* column ID of the MEMBER_VERSION column       */
	   int *positionCol, /* column ID of the MEMBER_POSITION column      */
	   int *locationCol, /* column ID of the MEMBER_LOCATION column      */
	   int *uriCol,      /* column ID of the MEMBER_URI_TYPE column      */
	   int *grptype,     /* group structure type code specifying the
				grouping table columns that are defined:
				GT_ID_ALL_URI  (0) ==> all columns defined   
				GT_ID_REF      (1) ==> reference cols only   
				GT_ID_POS      (2) ==> position col only     
				GT_ID_ALL      (3) ==> ref & pos cols        
				GT_ID_REF_URI (11) ==> ref & loc cols        
				GT_ID_POS_URI (12) ==> pos & loc cols        */
	   int *status)      /* return status code                           */
/*
   examine the grouping table pointed to by gfptr and determine the column
   index ID of each possible grouping column. If a column is not found then
   an index of 0 is returned. the grptype parameter returns the structure
   of the grouping table ==> what columns are defined.
*/

{

  char keyvalue[FLEN_VALUE];
  char comment[FLEN_COMMENT];


  if(*status != 0) return(*status);

  do
    {
      /*
	if the HDU does not have an extname of "GROUPING" then it is not
	a grouping table
      */

      *status = fits_read_key_str(gfptr,"EXTNAME",keyvalue,comment,status);
  
      if(*status == KEY_NO_EXIST) 
	{
	  *status = NOT_GROUP_TABLE;
	  ffpmsg("Specified HDU is not a Grouping Table (ffgtgc)");
	}
      if(*status != 0) continue;

      prepare_keyvalue(keyvalue);

      if(strcasecmp(keyvalue,"GROUPING") != 0)
	{
	  *status = NOT_GROUP_TABLE;
	  continue;
	}

      /*
        search for the MEMBER_XTENSION, MEMBER_NAME, MEMBER_VERSION,
	MEMBER_POSITION, MEMBER_LOCATION and MEMBER_URI_TYPE columns
	and determine their column index ID
      */

      *status = fits_get_colnum(gfptr,CASESEN,"MEMBER_XTENSION",xtensionCol,
				status);

      if(*status == COL_NOT_FOUND)
	{
	  *status      = 0;
 	  *xtensionCol = 0;
	}

      if(*status != 0) continue;

      *status = fits_get_colnum(gfptr,CASESEN,"MEMBER_NAME",extnameCol,status);

      if(*status == COL_NOT_FOUND)
	{
	  *status     = 0;
	  *extnameCol = 0;
	}

      if(*status != 0) continue;

      *status = fits_get_colnum(gfptr,CASESEN,"MEMBER_VERSION",extverCol,
				status);

      if(*status == COL_NOT_FOUND)
	{
	  *status    = 0;
	  *extverCol = 0;
	}

      if(*status != 0) continue;

      *status = fits_get_colnum(gfptr,CASESEN,"MEMBER_POSITION",positionCol,
				status);

      if(*status == COL_NOT_FOUND)
	{
	  *status      = 0;
	  *positionCol = 0;
	}

      if(*status != 0) continue;

      *status = fits_get_colnum(gfptr,CASESEN,"MEMBER_LOCATION",locationCol,
				status);

      if(*status == COL_NOT_FOUND)
	{
	  *status      = 0;
	  *locationCol = 0;
	}

      if(*status != 0) continue;

      *status = fits_get_colnum(gfptr,CASESEN,"MEMBER_URI_TYPE",uriCol,
				status);

      if(*status == COL_NOT_FOUND)
	{
	  *status = 0;
	  *uriCol = 0;
	}

      if(*status != 0) continue;

      /*
	 determine the type of grouping table structure used by this
	 grouping table and record it in the grptype parameter
      */

      if(*xtensionCol && *extnameCol && *extverCol && *positionCol &&
	 *locationCol && *uriCol) 
	*grptype = GT_ID_ALL_URI;
      
      else if(*xtensionCol && *extnameCol && *extverCol &&
	      *locationCol && *uriCol) 
	*grptype = GT_ID_REF_URI;

      else if(*xtensionCol && *extnameCol && *extverCol && *positionCol)
	*grptype = GT_ID_ALL;
      
      else if(*xtensionCol && *extnameCol && *extverCol)
	*grptype = GT_ID_REF;
      
      else if(*positionCol && *locationCol && *uriCol) 
	*grptype = GT_ID_POS_URI;
      
      else if(*positionCol)
	*grptype = GT_ID_POS;
      
      else
	*status = NOT_GROUP_TABLE;
      
    }while(0);

  /*
    if the table contained more than one column with a reserved name then
    this cannot be considered a vailid grouping table
  */

  if(*status == COL_NOT_UNIQUE) 
    {
      *status = NOT_GROUP_TABLE;
      ffpmsg("Specified HDU has multipule Group table cols defined (ffgtgc)");
    }

  return(*status);
}

/*****************************************************************************/
int ffgtdc(int   grouptype,     /* code specifying the type of
				   grouping table information:
				   GT_ID_ALL_URI  0 ==> defualt (all columns)
				   GT_ID_REF      1 ==> ID by reference
				   GT_ID_POS      2 ==> ID by position
				   GT_ID_ALL      3 ==> ID by ref. and position
				   GT_ID_REF_URI 11 ==> (1) + URI info 
				   GT_ID_POS_URI 12 ==> (2) + URI info       */
	   int   xtensioncol, /* does MEMBER_XTENSION already exist?         */
	   int   extnamecol,  /* does MEMBER_NAME aleady exist?              */
	   int   extvercol,   /* does MEMBER_VERSION already exist?          */
	   int   positioncol, /* does MEMBER_POSITION already exist?         */
	   int   locationcol, /* does MEMBER_LOCATION already exist?         */
	   int   uricol,      /* does MEMBER_URI_TYPE aleardy exist?         */
	   char *ttype[],     /* array of grouping table column TTYPE names
				 to define (if *col var false)               */
	   char *tform[],     /* array of grouping table column TFORM values
				 to define (if*col variable false)           */
	   int  *ncols,       /* number of TTYPE and TFORM values returned   */
	   int  *status)      /* return status code                          */

/*
  create the TTYPE and TFORM values for the grouping table according to the
  value of the grouptype parameter and the values of the *col flags. The
  resulting TTYPE and TFORM are returned in ttype[] and tform[] respectively.
  The number of TTYPE and TFORMs returned is given by ncols. Both the TTYPE[]
  and TTFORM[] arrays must contain enough pre-allocated strings to hold
  the returned information.
*/

{

  int i = 0;

  char  xtension[]  = "MEMBER_XTENSION";
  char  xtenTform[] = "8A";
  
  char  name[]      = "MEMBER_NAME";
  char  nameTform[] = "32A";

  char  version[]   = "MEMBER_VERSION";
  char  verTform[]  = "1J";
  
  char  position[]  = "MEMBER_POSITION";
  char  posTform[]  = "1J";

  char  URI[]       = "MEMBER_URI_TYPE";
  char  URITform[]  = "3A";

  char  location[]  = "MEMBER_LOCATION";
  char  locTform[]  = "160A";


  if(*status != 0) return(*status);

  switch(grouptype)
    {
      
    case GT_ID_ALL_URI:

      if(xtensioncol == 0)
	{
	  strcpy(ttype[i],xtension);
	  strcpy(tform[i],xtenTform);
	  ++i;
	}
      if(extnamecol == 0)
	{
	  strcpy(ttype[i],name);
	  strcpy(tform[i],nameTform);
	  ++i;
	}
      if(extvercol == 0)
	{
	  strcpy(ttype[i],version);
	  strcpy(tform[i],verTform);
	  ++i;
	}
      if(positioncol == 0)
	{
	  strcpy(ttype[i],position);
	  strcpy(tform[i],posTform);
	  ++i;
	}
      if(locationcol == 0)
	{
	  strcpy(ttype[i],location);
	  strcpy(tform[i],locTform);
	  ++i;
	}
      if(uricol == 0)
	{
	  strcpy(ttype[i],URI);
	  strcpy(tform[i],URITform);
	  ++i;
	}
      break;
      
    case GT_ID_REF:
      
      if(xtensioncol == 0)
	{
	  strcpy(ttype[i],xtension);
	  strcpy(tform[i],xtenTform);
	  ++i;
	}
      if(extnamecol == 0)
	{
	  strcpy(ttype[i],name);
	  strcpy(tform[i],nameTform);
	  ++i;
	}
      if(extvercol == 0)
	{
	  strcpy(ttype[i],version);
	  strcpy(tform[i],verTform);
	  ++i;
	}
      break;
      
    case GT_ID_POS:
      
      if(positioncol == 0)
	{
	  strcpy(ttype[i],position);
	  strcpy(tform[i],posTform);
	  ++i;
	}	  
      break;
      
    case GT_ID_ALL:
      
      if(xtensioncol == 0)
	{
	  strcpy(ttype[i],xtension);
	  strcpy(tform[i],xtenTform);
	  ++i;
	}
      if(extnamecol == 0)
	{
	  strcpy(ttype[i],name);
	  strcpy(tform[i],nameTform);
	  ++i;
	}
      if(extvercol == 0)
	{
	  strcpy(ttype[i],version);
	  strcpy(tform[i],verTform);
	  ++i;
	}
      if(positioncol == 0)
	{
	  strcpy(ttype[i],position);
	  strcpy(tform[i], posTform);
	  ++i;
	}	  
      
      break;
      
    case GT_ID_REF_URI:
      
      if(xtensioncol == 0)
	{
	  strcpy(ttype[i],xtension);
	  strcpy(tform[i],xtenTform);
	  ++i;
	}
      if(extnamecol == 0)
	{
	  strcpy(ttype[i],name);
	  strcpy(tform[i],nameTform);
	  ++i;
	}
      if(extvercol == 0)
	{
	  strcpy(ttype[i],version);
	  strcpy(tform[i],verTform);
	  ++i;
	}
      if(locationcol == 0)
	{
	  strcpy(ttype[i],location);
	  strcpy(tform[i],locTform);
	  ++i;
	}
      if(uricol == 0)
	{
	  strcpy(ttype[i],URI);
	  strcpy(tform[i],URITform);
	  ++i;
	}
      break;
      
    case GT_ID_POS_URI:
      
      if(positioncol == 0)
	{
	  strcpy(ttype[i],position);
	  strcpy(tform[i],posTform);
	  ++i;
	}
      if(locationcol == 0)
	{
	  strcpy(ttype[i],location);
	  strcpy(tform[i],locTform);
	  ++i;
	}
      if(uricol == 0)
	{
	  strcpy(ttype[i],URI);
	  strcpy(tform[i],URITform);
	  ++i;
	}
      break;
      
    default:
      
      *status = BAD_OPTION;
      ffpmsg("Invalid value specified for the grouptype parameter (ffgtdc)");

      break;

    }

  *ncols = i;
  
  return(*status);
}

/*****************************************************************************/
int ffgmul(fitsfile *mfptr,   /* pointer to the grouping table member HDU    */
           int       rmopt,   /* 0 ==> leave GRPIDn/GRPLCn keywords,
				 1 ==> remove GRPIDn/GRPLCn keywords         */
	   int      *status) /* return status code                          */

/*
   examine all the GRPIDn and GRPLCn keywords in the member HDUs header
   and remove the member from the grouping tables referenced; This
   effectively "unlinks" the member from all of its groups. The rmopt 
   specifies if the GRPIDn/GRPLCn keywords are to be removed from the
   member HDUs header after the unlinking.
*/

{
  int memberPosition = 0;

  long index        = 0;
  long ngroups      = 0;
  long memberExtver = 0;
  long memberID     = 0;

  char memberFileName[FLEN_FILENAME];
  char memberHDUtype[FLEN_VALUE];
  char memberExtname[FLEN_VALUE];
  char keyword[FLEN_KEYWORD];
  char card[FLEN_CARD];

  fitsfile *gfptr = NULL;


  if(*status != 0) return(*status);

  do
    {
      /* 
	 determine location parameters of the member HDU; note that
	 default values are supplied if the expected keywords are not
	 found
      */

      *status = fits_read_key_str(mfptr,"XTENSION",memberHDUtype,card,status);

      if(*status == KEY_NO_EXIST) 
	{
	  strcpy(memberHDUtype,"PRIMARY");
	  *status = 0;
	}
      prepare_keyvalue(memberHDUtype);

      *status = fits_read_key_lng(mfptr,"EXTVER",&memberExtver,card,status);

      if(*status == KEY_NO_EXIST) 
	{
	  memberExtver = 1;
	  *status      = 0;
	}

      *status = fits_read_key_str(mfptr,"EXTNAME",memberExtname,card,status);

      if(*status == KEY_NO_EXIST) 
	{
	  memberExtname[0] = 0;
	  *status          = 0;
	}
      prepare_keyvalue(memberExtname);

      fits_get_hdu_num(mfptr,&memberPosition);

      *status = fits_file_name(mfptr,memberFileName,status);

      ffgtcn(memberFileName);

      if(*status != 0) continue;

      /*
	 open each grouping table linked to this HDU and remove the member 
	 from the grouping tables
      */

      *status = fits_get_num_groups(mfptr,&ngroups,status);

      /* loop over each group linked to the member HDU */

      for(index = 1; index <= ngroups && *status == 0; ++index)
	{
	  /* open the (index)th group linked to the member HDU */ 

	  *status = fits_open_group(mfptr,index,&gfptr,status);

	  /* if the group could not be opened then just skip it */

	  if(*status != 0)
	    {
	      *status = 0;
	      continue;
	    }

	  /* try to find the member's row within the grouping table */
	      
	  *status = ffgmf(gfptr,memberHDUtype,memberExtname,memberExtver,
			   memberPosition,memberFileName,&memberID,status);

	  /* if the member was found then delete it from the grouping table */

	  if(*status == 0)
	    *status = fits_delete_rows(gfptr,memberID,1,status);

	  /*
	     continue the loop over all member groups even if an error
	     was generated
	  */

	  *status = 0;

	  /*
	     close the file pointed to by gfptr if it is non NULL to
	     prepare for the next loop iterration
	  */

	  if(gfptr != NULL)
	    {
	      fits_close_file(gfptr,status);
	      gfptr = NULL;
	    }
	}

      if(*status != 0) continue;

      /*
	 if rmopt is non-zero then find and delete the GRPIDn/GRPLCn 
	 keywords from the member HDU header
      */

      if(rmopt != 0)
	{
	  /* delete all the GRPIDn/GRPLCn keywords */

	  for(index = 1; index <= ngroups && *status == 0; ++index)
	    {
	      sprintf(keyword,"GRPID%d",(int)index);
	      fits_delete_key(mfptr,keyword,status);
	      
	      sprintf(keyword,"GRPLC%d",(int)index);
	      fits_delete_key(mfptr,keyword,status);

	      if(*status == KEY_NO_EXIST) *status = 0;
	    }
	}
    }while(0);

  /* make sure the gfptr has been closed */

  if(gfptr != NULL) fits_close_file(gfptr,status);

return(*status);
}

/*--------------------------------------------------------------------------*/
int ffgmf(fitsfile *gfptr, /* pointer to grouping table HDU to search       */
	   char *xtension,  /* XTENSION value for member HDU                */
	   char *extname,   /* EXTNAME value for member HDU                 */
	   int   extver,    /* EXTVER value for member HDU                  */
	   int   position,  /* HDU position value for member HDU            */
	   char *location,  /* FITS file location value for member HDU      */
	   long *member,    /* member HDU ID within group table (if found)  */
	   int  *status)    /* return status code                           */

/*
   try to find the entry for the member HDU defined by the xtension, extname,
   extver, position, and location parameters within the grouping table
   pointed to by gfptr. If the member HDU is found then its ID (row number)
   within the grouping table is returned in the member variable; if not
   found then member is returned with a value of 0 and the status return
   code will be set to MEMBER_NOT_FOUND.

   Note that the member HDU postion information is used to obtain a member
   match only if the grouping table type is GT_ID_POS_URI or GT_ID_POS. This
   is because the position information can become invalid much more
   easily then the reference information for a group member.
*/

{
  int xtensionCol,extnameCol,extverCol,positionCol,locationCol,uriCol;
  int mposition = 0;
  int grptype;
  int dummy;
  int i;

  long nmembers = 0;
  long mextver  = 0;

  char  charBuff[FLEN_FILENAME];
  char  nstr[] = {"\0"};
  char *tmpPtr[1];

  if(*status != 0) return(*status);

  *member = 0;

  tmpPtr[0] = charBuff;

  /*
     retrieve the Grouping Convention reserved column positions within
     the grouping table
  */

  *status = ffgtgc(gfptr,&xtensionCol,&extnameCol,&extverCol,&positionCol,
		   &locationCol,&uriCol,&grptype,status);

  /* retrieve the number of group members */

  *status = fits_get_num_members(gfptr,&nmembers,status);
	      
  /* 
     loop over all grouping table rows until the member HDU is found 
  */

  for(i = 1; i <= nmembers && *status == 0; ++i)
    {
      if(xtensionCol != 0)
	{
	  fits_read_col_str(gfptr,xtensionCol,i,1,1,nstr,tmpPtr,&dummy,status);
	  if(strcasecmp(tmpPtr[0],xtension) != 0) continue;
	}
	  
      if(extnameCol  != 0)
	{
	  fits_read_col_str(gfptr,extnameCol,i,1,1,nstr,tmpPtr,&dummy,status);
	  if(strcasecmp(tmpPtr[0],extname) != 0) continue;
	}
	  
      if(extverCol   != 0)
	{
	  fits_read_col_lng(gfptr,extverCol,i,1,1,0,
			    (long*)&mextver,&dummy,status);
	  if(extver != mextver) continue;
	}
      
      /* note we only use postionCol if we have to */

      if(positionCol != 0 && 
	            (grptype == GT_ID_POS || grptype == GT_ID_POS_URI))
	{
	  fits_read_col_lng(gfptr,positionCol,i,1,1,0,
			    (long*)&mposition,&dummy,status);
	  if(position != mposition) continue;
	}
      
      if(locationCol != 0)
	{
	  fits_read_col_str(gfptr,locationCol,i,1,1,nstr,tmpPtr,&dummy,status);
	  
	  /* if the location value is NULL then use the group table location */

	  if(strlen(tmpPtr[0]) == 0)
	    fits_file_name(gfptr,tmpPtr[0],status);

	  ffgtcn(tmpPtr[0]);

	  if(strcasecmp(tmpPtr[0],location) != 0) continue;
	}

      /* if we made it this far then a match to the member HDU was found */
      
      *member = i;
    }

  /* if a match was not found then set the return status code */

  if(*member == 0 && *status == 0) 
    {
      *status = MEMBER_NOT_FOUND;
      ffpmsg("Cannot find specified member HDU (ffgmf)");
    }

  return(*status);
}

/*--------------------------------------------------------------------------*/
int ffgtrmr(fitsfile   *gfptr,  /* FITS file pointer to group               */
	    HDUtracker *HDU,    /* list of processed HDUs                   */
	    int        *status) /* return status code                       */
	    
/*
  recursively remove a grouping table and all its members. Each member of
  the grouping table pointed to by gfptr it processed. If the member is itself
  a grouping table then ffgtrmr() is recursively called to process all
  of its members. The HDUtracker struct *HDU is used to make sure a member
  is not processed twice, thus avoiding an infinite loop (e.g., a grouping
  table contains itself as a member).
*/

{
  int i;
  int hdutype;

  long nmembers = 0;

  char keyvalue[FLEN_VALUE];
  char comment[FLEN_COMMENT];
  
  fitsfile *mfptr = NULL;


  if(*status != 0) return(*status);

  /* get the number of members contained by this grouping table */

  *status = fits_get_num_members(gfptr,&nmembers,status);

  /* loop over all group members and delete them */

  for(i = nmembers; i > 0 && *status == 0; --i)
    {
      /* open the member HDU */

      *status = fits_open_member(gfptr,i,&mfptr,status);

      /* if the member cannot be opened then just skip it and continue */

      if(*status == MEMBER_NOT_FOUND) 
	{
	  *status = 0;
	  continue;
	}

      /* Any other error is a reason to abort */
      
      if(*status != 0) continue;

      /* add the member HDU to the HDUtracker struct */

      *status = fftsad(mfptr,HDU,NULL,NULL);

      /* status == HDU_ALREADY_TRACKED ==> HDU has already been processed */

      if(*status == HDU_ALREADY_TRACKED) 
	{
	  *status = 0;
	  fits_close_file(mfptr,status);
	  continue;
	}
      else if(*status != 0) continue;

      /* determine if the member HDU is itself a grouping table */

      *status = fits_read_key_str(mfptr,"EXTNAME",keyvalue,comment,status);

      /* if no EXTNAME is found then the HDU cannot be a grouping table */ 

      if(*status == KEY_NO_EXIST) 
	{
	  *status     = 0;
	  keyvalue[0] = 0;
	}
      prepare_keyvalue(keyvalue);

      /* Any other error is a reason to abort */
      
      if(*status != 0) continue;

      /* 
	 if the EXTNAME == GROUPING then the member is a grouping table 
	 and we must call ffgtrmr() to process its members
      */

      if(strcasecmp(keyvalue,"GROUPING") == 0)
	  *status = ffgtrmr(mfptr,HDU,status);  

      /* 
	 unlink all the grouping tables that contain this HDU as a member 
	 and then delete the HDU (if not a PHDU)
      */

      if(fits_get_hdu_num(mfptr,&hdutype) == 1)
	      *status = ffgmul(mfptr,1,status);
      else
	  {
	      *status = ffgmul(mfptr,0,status);
	      *status = fits_delete_hdu(mfptr,&hdutype,status);
	  }

      /* close the fitsfile pointer */

      fits_close_file(mfptr,status);
    }

  return(*status);
}
/*--------------------------------------------------------------------------*/
int ffgtcpr(fitsfile   *infptr,  /* input FITS file pointer                 */
	    fitsfile   *outfptr, /* output FITS file pointer                */
	    int         cpopt,   /* code specifying copy options:
				    OPT_GCP_GPT (0) ==> cp only grouping table
				    OPT_GCP_ALL (2) ==> recusrively copy 
				    members and their members (if groups)   */
	    HDUtracker *HDU,     /* list of already copied HDUs             */
	    int        *status)  /* return status code                      */

/*
  copy a Group to a new FITS file. If the cpopt parameter is set to 
  OPT_GCP_GPT (copy grouping table only) then the existing members have their 
  GRPIDn and GRPLCn keywords updated to reflect the existance of the new group,
  since they now belong to another group. If cpopt is set to OPT_GCP_ALL 
  (copy grouping table and members recursively) then the original members are 
  not updated; the new grouping table is modified to include only the copied 
  member HDUs and not the original members.

  Note that this function is recursive. When copt is OPT_GCP_ALL it will call
  itself whenever a member HDU of the current grouping table is itself a
  grouping table (i.e., EXTNAME = 'GROUPING').
*/

{

  int i;
  int nexclude     = 8;
  int hdutype      = 0;
  int groupHDUnum  = 0;
  int numkeys      = 0;
  int keypos       = 0;
  int newPosition  = 0;

  long nmembers    = 0;
  long tfields     = 0;
  long newTfields  = 0;

  char keyword[FLEN_KEYWORD];
  char keyvalue[FLEN_VALUE];
  char card[FLEN_CARD];

  char *excludeList[] ={"EXTNAME","EXTVER","GRPNAME","GRPID#","GRPLC#",
			"THEAP","TDIM#","T????#"};

  fitsfile *mfptr = NULL;


  if(*status != 0) return(*status);

  do
    {
      /*
	create a new grouping table in the FITS file pointed to by outptr
      */

      *status = fits_get_num_members(infptr,&nmembers,status);

      *status = fits_read_key_str(infptr,"GRPNAME",keyvalue,card,status);

      if(*status == KEY_NO_EXIST)
	{
	  keyvalue[0] = 0;
	  *status     = 0;
	}
      prepare_keyvalue(keyvalue);

      *status = fits_create_group(outfptr,keyvalue,GT_ID_ALL_URI,status);
     
      /* save the new grouping table's HDU position for future use */

      fits_get_hdu_num(outfptr,&groupHDUnum);

      /*
	 copy all auxiliary keyword records from the original grouping table
	 to the new grouping table; we attempt to write the keywords into
	 the same position that they had in the original grouping table
      */

      ffgrec(infptr,8,card,status);

      while(*status == 0)
	{

	  *status = fits_find_nextkey(infptr,NULL,0,excludeList,nexclude,
				      card,status);
	  *status = fits_get_hdrpos(infptr,&numkeys,&keypos,status);
	  
	  *status = fits_insert_record(outfptr,keypos-1,card,status);
	}

      if(*status == KEY_NO_EXIST) 
	*status = 0;
      else if(*status != 0) continue;

      /*
	 search all the columns of the original grouping table and copy
	 those to the new grouping table that were not part of the grouping
	 convention. Note that is legal to have additional columns in a
	 grouping table. Also note that the order of the columns may
	 not be the same in the original and copied grouping table.
      */

      /* retrieve the number of columns in the original and new group tables */

      *status = fits_read_key_lng(infptr,"TFIELDS",&tfields,card,status);
      *status = fits_read_key_lng(outfptr,"TFIELDS",&newTfields,card,status);

      for(i = 1; i <= tfields; ++i)
	{
	  sprintf(keyword,"TTYPE%d",i);
	  *status = fits_read_key_str(infptr,keyword,keyvalue,card,status);
	  
	  if(*status == KEY_NO_EXIST)
	    {
	      *status = 0;
              keyvalue[0] = 0;
	    }
	  prepare_keyvalue(keyvalue);

	  if(strcasecmp(keyvalue,"MEMBER_XTENSION") != 0 &&
	     strcasecmp(keyvalue,"MEMBER_NAME")     != 0 &&
	     strcasecmp(keyvalue,"MEMBER_VERSION")  != 0 &&
	     strcasecmp(keyvalue,"MEMBER_POSITION") != 0 &&
	     strcasecmp(keyvalue,"MEMBER_LOCATION") != 0 &&
	     strcasecmp(keyvalue,"MEMBER_URI_TYPE") != 0   )
	    {
	      *status = fits_copy_col(infptr,outfptr,i,newTfields,1,status);
	      ++newTfields;
	    }
	}

      /* update the HDUtracker struct with the grouping table's new position */
      
      *status = fftsud(infptr,HDU,groupHDUnum,NULL);

      /*
	Now populate the copied grouping table depending upon the 
	copy option parameter value
      */

      switch(cpopt)
	{

	  /*
	    for the "copy grouping table only" option we only have to
	    add the members of the original grouping table to the new
	    grouping table
	  */

	case OPT_GCP_GPT:

	  for(i = 1; i <= nmembers && *status == 0; ++i)
	    {
	      *status = fits_open_member(infptr,i,&mfptr,status);
	      *status = fits_add_group_member(outfptr,mfptr,0,status);

	      fits_close_file(mfptr,status);
	      mfptr = NULL;
	    }

	  break;

	case OPT_GCP_ALL:
      
	  /*
	    for the "copy the entire group" option
 	  */

	  /* loop over all the grouping table members */

	  for(i = 1; i <= nmembers && *status == 0; ++i)
	    {
	      /* open the ith member */

	      *status = fits_open_member(infptr,i,&mfptr,status);

	      /* add it to the HDUtracker struct */

	      *status = fftsad(mfptr,HDU,&newPosition,NULL);

	      /* if already copied then just add the member to the group */

	      if(*status == HDU_ALREADY_TRACKED)
		{
		  *status = 0;
		  *status = fits_add_group_member(outfptr,NULL,newPosition,
						  status);
		  fits_close_file(mfptr,status);
                  mfptr = NULL;
		  continue;
		}
	      else if(*status != 0) continue;

	      /* see if the member is a grouping table */

	      *status = fits_read_key_str(mfptr,"EXTNAME",keyvalue,card,
					  status);

	      if(*status == KEY_NO_EXIST)
		{
		  keyvalue[0] = 0;
		  *status     = 0;
		}
	      prepare_keyvalue(keyvalue);

	      /*
		if the member is a grouping table then copy it and all of
		its members using ffgtcpr(), else copy it using
		fits_copy_member(); the outptr will point to the newly
		copied member upon return from both functions
	      */

	      if(strcasecmp(keyvalue,"GROUPING") == 0)
		*status = ffgtcpr(mfptr,outfptr,OPT_GCP_ALL,HDU,status);
	      else
		*status = fits_copy_member(infptr,outfptr,i,OPT_MCP_NADD,
					   status);

	      /* retrieve the position of the newly copied member */

	      fits_get_hdu_num(outfptr,&newPosition);

	      /* update the HDUtracker struct with member's new position */
	      
	      if(strcasecmp(keyvalue,"GROUPING") != 0)
		*status = fftsud(mfptr,HDU,newPosition,NULL);

	      /* move the outfptr back to the copied grouping table HDU */

	      *status = fits_movabs_hdu(outfptr,groupHDUnum,&hdutype,status);

	      /* add the copied member HDU to the copied grouping table */

	      *status = fits_add_group_member(outfptr,NULL,newPosition,status);

	      /* close the mfptr pointer */

	      fits_close_file(mfptr,status);
	      mfptr = NULL;
	    }

	  break;

	default:
	  
	  *status = BAD_OPTION;
	  ffpmsg("Invalid value specified for cmopt parameter (ffgtcpr)");
	  break;
	}

      /* 
	 reposition the outfptr to the grouping table so that the grouping
	 table is the CHDU upon return to the calling function
      */

      fits_movabs_hdu(outfptr,groupHDUnum,&hdutype,status);

    }while(0);

  if(mfptr != NULL) fits_close_file(mfptr,status);

  return(*status);
}
/*--------------------------------------------------------------------------*/
int fftsad(fitsfile   *mfptr,       /* pointer to an member HDU             */
	   HDUtracker *HDU,         /* pointer to an HDU tracker struct     */
	   int        *newPosition, /* new HDU position of the member HDU   */
	   char       *newFileName) /* file containing member HDU           */

/*
  add an HDU to the HDUtracker struct pointed to by HDU. The HDU is only 
  added if it does not already reside in the HDUtracker. If it already
  resides in the HDUtracker then the new HDU postion and file name are
  returned in  newPosition and newFileName (if != NULL)
*/

{
  int i;
  int hdunum;
  int status = 0;

  char filename[FLEN_FILENAME];

  do
    {
      /* retrieve the HDU's position within the FITS file */

      fits_get_hdu_num(mfptr,&hdunum);
      
      /* retrieve the HDU's file name */
      
      status = fits_file_name(mfptr,filename,&status);
      
      /* parse the file name and construct the "standard" URL for it */
      
      ffgtcn(filename);
      
      /* 
	 examine all the existing HDUs in the HDUtracker an see if this HDU
	 has already been registered
      */

      for(i = 0; 
       i < HDU->nHDU && 
       !(HDU->position[i] == hdunum && strcmp(HDU->filename[i],filename) == 0);
       ++i);

      if(i != HDU->nHDU) 
	{
	  status = HDU_ALREADY_TRACKED;
	  if(newPosition != NULL) *newPosition = HDU->newPosition[i];
	  if(newFileName != NULL) strcpy(newFileName,HDU->newFilename[i]);
	  continue;
	}

      if(HDU->nHDU == MAX_HDU_TRACKER) 
	{
	  status = TOO_MANY_HDUS_TRACKED;
	  continue;
	}

      HDU->filename[i] = (char*) malloc(FLEN_FILENAME * sizeof(char));

      if(HDU->filename[i] == NULL)
	{
	  status = MEMORY_ALLOCATION;
	  continue;
	}

      HDU->newFilename[i] = (char*) malloc(FLEN_FILENAME * sizeof(char));

      if(HDU->newFilename[i] == NULL)
	{
	  status = MEMORY_ALLOCATION;
	  free(HDU->filename[i]);
	  continue;
	}

      HDU->position[i]    = hdunum;
      HDU->newPosition[i] = hdunum;

      strcpy(HDU->filename[i],filename);
      strcpy(HDU->newFilename[i],filename);
 
       ++(HDU->nHDU);

    }while(0);

  return(status);
}
/*--------------------------------------------------------------------------*/
int fftsud(fitsfile   *mfptr,       /* pointer to an member HDU             */
	   HDUtracker *HDU,         /* pointer to an HDU tracker struct     */
	   int         newPosition, /* new HDU position of the member HDU   */
	   char       *newFileName) /* file containing member HDU           */

/*
  update the HDU information in the HDUtracker struct pointed to by HDU. The 
  HDU to update is pointed to by mfptr. If non-zero, the value of newPosition
  is used to update the HDU->newPosition[] value for the mfptr, and if
  non-NULL the newFileName value is used to update the HDU->newFilename[]
  value for mfptr.
*/

{
  int i;
  int hdunum;
  int status = 0;

  char filename[FLEN_FILENAME];


  /* retrieve the HDU's position within the FITS file */
  
  fits_get_hdu_num(mfptr,&hdunum);
  
  /* retrieve the HDU's file name */
  
  fits_file_name(mfptr,filename,&status);
  
  /* parse the file name and construct the "standard" URL for it */
      
  ffgtcn(filename);

  /* 
     examine all the existing HDUs in the HDUtracker an see if this HDU
     has already been registered
  */

  for(i = 0; i < HDU->nHDU && 
      !(HDU->position[i] == hdunum && strcmp(HDU->filename[i],filename) == 0);
      ++i);

  /* if previously registered then change newPosition and newFileName */

  if(i != HDU->nHDU) 
    {
      if(newPosition  != 0) HDU->newPosition[i] = newPosition;
      if(newFileName  != NULL) 
	{
	  /* create the "standard" URL and store in the newFileName */
	  
	  ffgtcn(newFileName);
      
	  strcpy(HDU->newFilename[i],newFileName);
	}
    }
  else
    status = MEMBER_NOT_FOUND;
 
  return(status);
}

/*---------------------------------------------------------------------------*/
void ffgtcn(char *filename) /* FITS file name                                */

/*
  construct a URL from an input FITS file name in place (i.e., in the
  same character string). This function essentially strips off any information
  that might be contained after the filename in the name specification
  for purposes of string comparisions used elsewhere in the grouping table
  module
*/

{
  int status = 0;

  char urltype[FLEN_VALUE]; 
  char infile[FLEN_FILENAME];
  char outfile[FLEN_FILENAME];
  char extspec[FLEN_FILENAME];
  char rowfilter[FLEN_FILENAME];
  char binspec[FLEN_FILENAME];
  char colspec[FLEN_FILENAME];

  /* call fits_parse_input_url() to break the filename into peices */

  fits_parse_input_url(filename,urltype,infile,outfile,extspec,rowfilter,
		       binspec,colspec,&status);

  /* copy the URL specifier to the filename string */

  strcpy(filename,urltype);

  /* copy the machine ID, file path and file name to the filename string */

  strcat(filename,infile);
}

/*---------------------------------------------------------------------------*/
void prepare_keyvalue(char *keyvalue) /* string containing keyword value     */

/*
  strip off all single quote characters "'" and blank spaces from a keyword
  value retrieved via fits_read_key*() routines

  this is necessary so that a standard comparision of keyword values may
  be made
*/

{

  int i;
  int length;

  /*
    strip off any leading or trailing single quotes (`) and (') from
    the keyword value
  */

  length = strlen(keyvalue) - 1;

  if(keyvalue[0] == '\'' && keyvalue[length] == '\'')
    {
      for(i = 0; i < length - 1; ++i) keyvalue[i] = keyvalue[i+1];
      keyvalue[length-1] = 0;
    }
  
  /*
    strip off any trailing blanks from the keyword value; note that if the
    keyvalue consists of nothing but blanks then no blanks are stripped
  */

  length = strlen(keyvalue) - 1;

  for(i = 0; i < length && keyvalue[i] == ' '; ++i);

  if(i != length)
    {
      for(i = length; i >= 0 && keyvalue[i] == ' '; --i) keyvalue[i] = '\0';
    }
}

