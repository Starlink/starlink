/* - AMS_MAC - macro definitions for adam message system */

#define MXQUEUE      32
#define FIXED_NUM_Q      6      /* number of fixed task queues */
#define REC_MXQUEUE      (FIXED_NUM_Q + MESSYS__MXTRANS)
                        /* maximum number of queues */

#define CHARNIL ((char *)0)




/*
*+
*  Name:
*     AMS_CHECKTRANS

*  Language:
*     {routine_language}

*  Algorithm:
*     A macro which checks that a transaction number is legal (between 0
*     and MESSYS__MXTRANS) and that it is being used (transfree[messid]
*     == false) setting status to MESSYS__BADMESS/MESSYS__NOMESS/SAI__OK
*     as appropriate

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
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
#define AMS_checktrans(messid,status)  \
   if ((messid) < 0 || (messid) >= MESSYS__MXTRANS) \
     (*(status)) = MESSYS__BADMESS; \
   else if (transfree[(messid)]  == true) \
     (*(status)) = MESSYS__NOMESS; \
   else (*(status)) = SAI__OK;


/*
*+
*  Name:
*     AMS_CHECKTRANSACTIVE

*  Language:
*     {routine_language}

*  Algorithm:
*     Check that a transaction number is legal (between 0 and
*     MESSYS__MXTRANS) being used, and that the transaction is active
*     (that it has an acknowledge queue) setting *status to
*     MESSYS__BADMESS/MESSYS_NOMESS/SAI__OK as appropriate

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
#define AMS_checktransactive(messid, status) \
   if ( (messid) < 0  || (messid) >= MESSYS__MXTRANS  ) \
     (*(status)) = MESSYS__BADMESS; \
   else if (transfree[(messid)] == true) \
     (*(status)) = MESSYS__NOMESS; \
   else if (t_trans[(messid)].this_task_ack_q == MESSYS__NULL_Q ) \
     (*(status)) = MESSYS__NOMESS; \
   else (*(status)) = SAI__OK;


/*
*+
*  Name:
*     AMS_CHECKPATHOPEN

*  Language:
*     {routine_language}

*  Algorithm:
*     Check that a path is legal (between 0 and MESSYS__MAXPATH) and that
*     it is open (pathfree[path] == false) setting status to
*     MESSYS__BADPATH/ MESSYS__NONEXIST/SAI__OK as appropriate.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
#define AMS_checkpathopen(path,status) \
   if ( (path) < 0 || (path) >= MESSYS__MXPATH ) \
     (*(status)) = MESSYS__BADPATH; \
   else if( pathfree[(path)] == true ) \
     (*(status)) = MESSYS__NONEXIST; \
   else (*(status)) = SAI__OK;


/*
*+
*  Name:
*     AMS_CHECKPATH

*  Language:
*     {routine_language}

*  Algorithm:
*     Check that a path is legal (between 0 and MESSYS__MAXPATH) setting
*     status to MESSYS__BADPATH or SAI__OK

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
#define AMS_checkpath(path,status) \
   if ( (path) < 0 || (path) >= MESSYS__MXPATH ) \
     (*(status)) = MESSYS__BADPATH; \
   else (*(status)) = SAI__OK;
