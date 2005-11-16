#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*+HDSGROUP.C-*/                                       

/* Include files */
#include <ctype.h>               /* Character classification                */
#include <strings.h>             /* String handling functions               */

#include "ems.h"                 /* EMS error reporting routines            */

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

/*==================================*/
/* HDS_LINK - Link locator to group */
/*==================================*/

int
hdsLink(char locator_str[DAT__SZLOC],
        char *group_str,
        int *status)
{
#undef context_name
#undef context_message
#define context_name "HDS_LINK_ERR"
#define context_message\
        "HDS_LINK: Error linking a locator to an HDS group."

   struct DSC               group;

   struct LCP      *lcp;
   struct LCP_DATA *data;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import the group string.        */

   _strcsimp( &group, group_str );

/* Import the locator.  */

   dat1_import_loc( locator_str, DAT__SZLOC, &lcp );
   data = &lcp->data;
   
/* Save the group specification.        */

   _call( hds1_check_group( &group, data->group ) )

   return hds_gl_status;
}

/*=================================*/
/* HDS_FLUSH - Flush locator group */
/*=================================*/

int
hdsFlush( char *group_str,
          int *status)
{
#undef context_name
#undef context_message
#define context_name "HDS_FLUSH_ERR"
#define context_message\
        "HDS_FLUSH: Error flushing an HDS locator group."

   struct DSC group;

   struct LCP *lcp;
   char grpbuf[DAT__SZGRP];
   int again;
   struct LCP *next;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import the group string and validate the group specification.        */

   _strcsimp( &group, group_str );
   _call( hds1_check_group( &group, grpbuf ) )

/* Obtain a pointer to the start of the Working Locator Queue and loop to   */
/* process all the Locator Control Packets in the queue.                    */
   again = ( dat_ga_wlq != NULL );
   for ( lcp = dat_ga_wlq; again; lcp = next )
   {

/* Obtain a pointer to the next queue element (while it still exists) and   */
/* note whether we have reached the end of the queue.                       */
      next = lcp->flink;
      again = ( next != dat_ga_wlq );

/* Identify those LCPs whose group name matches the one supplied.           */
      if ( _cheql( DAT__SZGRP, lcp->data.group, grpbuf ) )
      {

/* If the LCP is not a primary one, then simply defuse it (causing the      */
/* associated locator to become invalid). This only affects the one LCP.    */
         if ( !lcp->primary )
         {
            dau_defuse_lcp( &lcp );
         }

/* Primary LCPs must be annulled, decrementing the container file's         */
/* reference count. If this falls to zero, then all other LCPs in the queue */
/* associated with the same file will also be defused and the file will be  */
/* closed.                                                                  */
         else
         {
            dat1_annul_lcp( &lcp );

/* After an annul operation, we no longer know whether any particular LCP   */
/* remains in the queue. Thus we must return to the head of the queue to    */
/* identify the next LCP to process. Quit if the queue is empty (head of    */
/* queue is null).                                                          */
            next = dat_ga_wlq;
            again = ( next != NULL );
         }
      }
   }

   return hds_gl_status;
}

/*=============================*/
/* HDS_GROUP - Locator group ? */
/*=============================*/

int
hdsGroup(char locator_str[DAT__SZLOC],
         char group_str[DAT__SZGRP+1],
         int *status)
{
#undef context_name
#undef context_message
#define context_name "HDS_GROUP_ERR"
#define context_message\
        "HDS_GROUP: Error enquiring the group membership of an HDS locator."

   struct LCP      *lcp;
   struct LCP_DATA *data;

   char *p;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status     = DAT__OK;

/* Import the locator.  */

   dat1_import_loc( locator_str, DAT__SZLOC, &lcp );
   data = &lcp->data;

/* Copy the group specification from the LCP.   */

   _chmove( DAT__SZGRP, data->group, group_str );

/* Convert to C string */
   group_str[DAT__SZGRP] = '\0';
   if( ( p = strchr( group_str,' ' ) ) != NULL)
      *p = '\0';

   return hds_gl_status;
}

int
hds1_check_group(struct DSC *group,
                 char *buf)
{
/*+
 * CHECK_GROUP - Check group
 *
 * This routine validates the syntax of a 'group' specification. If successful,
 * the contents are formatted - any embedded blanks are removed and all lower-
 * case letters are converted to uppercase.
 *
 * Calling sequence:
 *
 *        CHECK_GROUP(GROUP,BUF)
 *
 * GROUP  is the address of a character string descriptor which points to the
 *        group specification.
 * BUF    is the address of a buffer which is to receive the 'formatted' group
 *        string.
 *
 * Routine value:
 *
 *        DAT__OK    if successful.
 *        DAT__GRPIN if the string does not conform to the syntax of a group
 *                   specification.
 */
 
   char *txt = (char *) group->body;
   short len = group->length;
   int   n   = 0;
   int   i;

/* First clear the destination buffer and then scan through the string,
   continually checking the syntax and converting lower case characters
   to uppercase.        */

   (void) memset( (void *) buf, (int) ' ', (size_t) DAT__SZGRP );
   for (i=0; i<len; i++)
      if (isspace(txt[i]))
                ;
      else if (n >= DAT__SZGRP) return hds_gl_status = DAT__GRPIN;
      else if (!isprint(txt[i])) return hds_gl_status = DAT__GRPIN;
      else
         buf[n++] = toupper(txt[i]);

/* Return with 'group invalid' if the string is empty.  */

   if (n == 0) return hds_gl_status = DAT__GRPIN;

   return hds_gl_status;
}
