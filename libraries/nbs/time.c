/*
*+
*  Name:
*     time.c

*  Purpose:
*     Noticeboard system timing tests.

*  Language:
*     {routine_language}

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     WFL: William Lupton (RGO)
*     {enter_new_authors_here}

*  History:
*     31-MAR-1988 (WFL):
*       Rewrite in portable C from Fortran.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#if HAVE_CONFIG_H
# include <config.h>
#endif

#if HAVE_TIME_H
#  include <time.h>
#endif
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#ifdef vms
#define _chmove(n,sptr,dptr) ots$move3(n,sptr,dptr)
#endif

void report(FILE*, int);

/* in timer.c */
int timer_stime( void );
int timer_utime( void );
void timer_start( void );

#if HAVE_MEMCPY
#define _chmove(n,sptr,dptr) memcpy(dptr,sptr,n)
#endif

int
main ()

{

time_t tim;		/* Current time */
int count,items,item;	/* # iterations, # items, item no */
int inc,check;		/* Control flags */
int old_inc,old_check;	/* Previous values of control flags */
int old_defn_size;     	/* Previous value of max defn size */
int sid,tsid;		/* Static ids */
int id,tid,zid;		/* Item ids */
int maxbytes,actbytes;	/* Max and actual bytes */
int i,j;		/* Counters */
int status;		/* Status variable */
int value[1024];	/* 1024 longwords */
int sink[1024];		/* Data sink */
char name[16];	  	/* Item name */
FILE *file;		/* Output file */

/*	Initialise status to zero.	*/

status = 0;

/*	Determine number of times to iterate in timing loops.	*/

count = 1;
items = 1;
item = 1;
printf ("Enter # iterations, # items, item no: ");
scanf ("%d %d %d",&count,&items,&item);

/*	Determine increment_modify and check_modify flags	*/

inc = 1;
check = 1;
printf ("Enter increment_modify and check_modify flags: ");
scanf ("%d %d",&inc,&check);
nbc_tune ("increment_modify",inc,&old_inc,&status);
nbc_tune ("check_modify",check,&old_check,&status);

/*	Open output file and report date, time and parameters	*/

file = freopen ("time.lis","w",stdout);
if (!file)
	file = stdout;	/* Questionable?	*/
time (&tim);
fprintf (file,"Noticeboard system timing program at %s",ctime(&tim));
fprintf (file,
	"-------------------------------------------------------------\n\n");
fprintf (file,"Number of iterations   = %d\n",count);
fprintf (file,"Number of items        = %d\n",items);
fprintf (file,"Item used for searches = %d\n",item);
fprintf (file,"Increment modify flag  = %d\n",inc);
fprintf (file,"Check modify flag      = %d\n",check);

/*	Test 1 - define, save and find noticeboard.	*/

if (status == 0)
	{
   	fprintf (file,"\nTest 0, define, save and find noticeboard\n");
   	timer_start ();
	nbc_tune ("max_defn_size",100000,&old_defn_size,&status);
	nbc_begin_definition (&sid,&status);
	for (i = 0; i < items; i++)
		{
		sprintf (name,"%6.6d",i);
		if (i == 0)
			j = 4096;
		else
			j = 4;
		nbc_define_primitive (sid,name,"_integer",0,j,&tsid,&status);
		}
	nbc_end_definition ("time","noticeboard_save",&status);
	nbc_restore_noticeboard ("time","time",&status);
	nbc_find_noticeboard ("time",&id,&status);
   	report (file,0);
	}

/*	Find 000000.                    	*/

nbc_find_item (id,"000000",&zid,&status);

/*	Put data into VALUE.	*/

if (status == 0)
	for (i = 0; i <= 1023; i++)
		value[i] = i;

/*	Test 1 - scalar assignment allows relative measures of CPU speed. */

if (status == 0)
	{
   	fprintf (file,"\nTest 1, scalar assignment\n");
   	timer_start ();
   	for (i = 1; i <= count; i++)
   		j = i;
   	report (file,count);
	}

/*	Test 2 - put 4-byte scalar.	*/

if (status == 0)
	{
   	fprintf (file,"\nTest 2, put scalar\n");
   	timer_start ();
   	for (i = 1; i <= count; i++)
   		nbc_put_value (zid,0,4,value,&status);
   	report (file,count);
	}

/*	Test 3 - get 4-byte scalar.	*/

if (status == 0)
	{
   	fprintf (file,"\nTest 3, get scalar\n");
   	timer_start ();
   	for (i = 1; i <= count; i++)
   		nbc_get_value (zid,0,4,value,&j,&status);
   	report (file,count);
	}

/*	Test 4 - move 10 longwords directly	*/

if (status == 0)
	{
   	fprintf (file,"\nTest 4, move 10 longwords directly\n");
   	timer_start ();
   	for (i = 1; i <= count; i++)
   		_chmove (40,value,sink);
   	report (file,count);
	}

/*	Test 5 - put 10 longword array.	*/

if (status == 0)
	{
   	fprintf (file,"\nTest 5, put 10 longword array\n");
   	timer_start ();
   	for (i = 1; i <= count; i++)
   		nbc_put_value (zid,0,40,value,&status);
   	report (file,count);
	}

/*	Test 6 - get 10 longword array.	*/

if (status == 0)
	{
   	fprintf (file,"\nTest 6, get 10 longword array\n");
   	timer_start ();
   	for (i = 1; i <= count; i++)
   		nbc_get_value (zid,0,40,value,&j,&status);
   	report (file,count);
	}

/*	Test 7 - move 1024 longwords directly	*/

if (status == 0)
	{
   	fprintf (file,"\nTest 7, move 1024 longwords directly\n");
   	timer_start ();
   	for (i = 1; i <= count; i++)
   	       	_chmove (4096,value,sink);
   	report (file,count);
	}

/*	Test 8 - put 1024 longword array.	*/

if (status == 0)
	{
   	fprintf (file,"\nTest 8, put 1024 longword array\n");
   	timer_start ();
   	for (i = 1; i <= count; i++)
   		nbc_put_value (zid,0,4096,value,&status);
   	report (file,count);
	}

/*	Test 9 - get 1024 longword array.	*/

if (status == 0)
	{
   	fprintf (file,"\nTest 9, get 1024 longword array\n");
   	timer_start ();
   	for (i = 1; i <= count; i++)
   		nbc_get_value (zid,0,4096,value,&j,&status);
   	report (file,count);
	}

/*	Test 10 - find item'th item by position.	*/

if (status == 0)
	{
   	fprintf (file,"\nTest 10, find %dth item by position\n",item);
   	timer_start ();
   	for (i = 1; i <= count; i++)
   		nbc_find_nth_item (id,item,&tid,&status);
   	report (file,count);
	}

/*	Test 11 - find item'th item by name.	*/

if (status == 0)
	{
   	fprintf (file,"\nTest 11, find %dth item by name\n",item);
	nbc_get_name (tid,name,&status);
   	timer_start ();
   	for (i = 1; i <= count; i++)
   		nbc_find_item (id,name,&tid,&status);
   	report (file,count);
	}

/*	Test 12 - enquire name.	*/

if (status == 0)
	{
   	fprintf (file,"\nTest 12, enquire name\n");
   	timer_start ();
   	for (i = 1; i <= count; i++)
		nbc_get_name (tid,name,&status);
   	report (file,count);
	}

/*	Test 13 - enquire size.	*/

if (status == 0)
	{
   	fprintf (file,"\nTest 13, enquire size\n");
   	timer_start ();
   	for (i = 1; i <= count; i++)
		nbc_get_size (tid,&maxbytes,&actbytes,&status);
   	report (file,count);
	}

/*	Report if any error occurred.	*/

if (status != 0)
	exit (status);

 return EXIT_SUCCESS;
}

/*	Report on elapsed and processor time.	*/

void
report (file,count)
FILE *file;
int count;

{

int cpu;

cpu = timer_stime () + timer_utime ();
if (count < 2)
	fprintf (file,"\tcpu microseconds = %d\n",cpu);
else
  {
  if ( cpu/count < 100 )
	fprintf (file,"\tcpu microseconds per iteration = %f\n",
 ((float)cpu)/((float)count));
  else
	fprintf (file,"\tcpu microseconds per iteration = %d\n",cpu/count);
      }
}
