/*+
 *  Name:
 *     GaiaLocalCatalog

 *  Purpose:
 *     Defines a class for controlling access to a non-tab table
 *     (i.e. CAT and plain ASCII) catalogues.

 *  Description:
 *     This class defines a series of methods that control the
 *     conversion of a "foreign" catalogue into a tab-table stored in
 *     a temporary file.
 *
 *     The actual conversion is performed by an external [incr Tcl]
 *     class GaiaConvertTable, which is provided so that addition
 *     filters etc. can be added without extending the abilities of
 *     this class.

 *  Language:
 *     C++

 *  Copyright:
 *     Copyright (C) 1996-2005 Central Laboratory of the Research Councils
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     Copyright (C) 2008 Science and Techology Facilities Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *  Inherits:
 *     LocalCatalog

 *  Bugs:
 *     Not really a bug in this class, but in the way that it and
 *     LocalCatalog are used. After the initial creation of this
 *     object queries are done in forked processes. So if the
 *     catalogue changes this fact is noted in the forked process and
 *     a conversion is performed, however, no information about this
 *     change is available after the forked process dies. So the
 *     timestamp is not updated and any mapped data are lost.  This is
 *     relatively harmless, but has an efficiency burden as after the
 *     first modification the catalogue is reconverted on occasions
 *     when it isn't really necessary. The simple solution is to avoid
 *     forking processes that use local catalogues (in fact this is
 *     what happens in GAIA).

 *  Authors:
 *     Peter W. Draper (PWD):
 *     {enter_new_authors_here}

 *  History:
 *     28-JUN-1996 (PWD):
 *        Original version, based on LocalCatalog.
 *     28-JAN-2000 (PWD):
 *        Changed to handle FITS extensions.
 *     {enter_changes_here}

 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstdio>
#include <cstdlib>
#include <sys/stat.h>
#include <ctime>
#include <unistd.h>
#include "error.h"
#include "GaiaLocalCatalog.h"
#include "Mem.h"
#include "tcl.h"
#include "util.h"

/**
 *  Constructor - used internally only, public interface uses "open(name)",
 *  "e" is the catalog config entry object for this catalog. Note that
 *  getInfo() in LocalCatalog constructor will be resolved at that level,
 *  during construction, so for known catalogues there will be a
 *  fake open. It would be nice to avoid that.
 */
GaiaLocalCatalog::GaiaLocalCatalog( CatalogInfoEntry *e, Tcl_Interp *interp )
    : LocalCatalog( e ),
      realname_( NULL ),
      modified_( 0 ),
      interp_( interp )
{
    convertTable_[0] = '\0';

    //  If this catalogue is already known, then the url will be set to the
    //  temporary file instead of the actual file (which is stored in
    //  longname), check that longname and url match, if not reset filename_
    //  to longName (was set to url).
    if ( strcmp( e->longName(), filename_ ) != 0 ) {
        free( filename_ ) ;
        filename_ = strdup( e->longName() );
    }

    status_ = getInfo();
}

/**
 *  Constructor for tab-tables that are already converted. In this case the
 *  name of the table and the destination file (of foreign type) are given.
 */
GaiaLocalCatalog::GaiaLocalCatalog( CatalogInfoEntry *e, Tcl_Interp *interp,
                                    const char *in, const char *out )
    : LocalCatalog( e ),
      interp_( interp )
{
    realname_ = strdup ( out );
    if ( filename_ ) {
        free( filename_ );
    }
    filename_ = strdup( in );
    convertTable_[0] = '\0';
    tempstamp_ = modDate( in );
    timestamp_ = modDate( out );
    modified_ = 1;
}

/**
 *  Destructor. Convert catalogue back, if it has been modified and dispose of
 *  temporary file.
 */
GaiaLocalCatalog::~GaiaLocalCatalog()
{
    if ( filename_ && realname_ ) {
        freeCat();

        //  The entry in the list of catalogues for this file will be
        //  wrong. Change the url to point to the real file again, rather
        //  than the temporary one.
        entry_->url( realname_ );
        free( realname_ );
    }

    //  Destroy the conversion object.
    if ( convertTable_[0] != '\0' ) {
        Tcl_VarEval( interp_, "catch {delete object ", convertTable_, "}",
                     (char *) NULL);
    }
}

/**
 *  Static method that checks the validity of the catalogue. Since this is
 *  static and we do not want to convert catalogues if not really necessary,
 *  just check the file extension.
 */
int GaiaLocalCatalog::check_table( const char* filename )
{
    if ( GaiaLocalCatalog::is_foreign( filename ) ) {
        return TCL_OK;
    }
    return TCL_ERROR;
}

/**
 *  If necessary convert the foreign catalogue into a tab-table.
 *
 *  Note the conversion is deemed unnecessary if the variable realname_ is
 *  already defined and the modification time of the input catalogue is
 *  unchanged.
 */
int GaiaLocalCatalog::getInfo()
{
    //  If realname_ exists then a conversion has already been successful. We
    //  will only convert again if the modification time of the main catalogue
    //  is changed.

    if ( realname_ ) {
        time_t curtime = modDate( realname_ );
        if ( difftime( curtime, timestamp_ ) != 0.0 ) {

            //  Main catalogue modified, so dispose of temporary and
            //  regenerate.
            dispose();
            filename_ = realname_;
        }
        else {

            //  Check if temporary file has been modified since it was last
            //  read, if so we need to re-read it.
            curtime = modDate( filename_ );
            if ( difftime( curtime, tempstamp_ ) != 0.0 ) {
                modified_ = 1;
                return readTemp();
            }
            return 0;
        }
    }

    //  No conversion yet, so get on with it.  Note update time of input file,
    //  so we know if it has been modified...
    timestamp_ = modDate( filename_ );

    //  Convert the catalogue into a "tab table" and retain the old and new
    //  names. Note tempnam creates the file in TMPDIR or /tmp.
    realname_ = filename_;
    filename_ = tempnam( (char *) NULL, "gaia" );
    filename_ = strdup( filename_ );
    entry_->url( filename_ );   //  Set the temporary name as the url.
                                //  This is the correct place.
                                //  Leave longname and shortname alone.
    if ( ! convertTo() ) {
        return sys_error( "failed to convert catalogue: ", realname_ );
    }

    //  OK now force the initial read of the temporary file.
    modified_ = 0;
    return readTemp();
}

/**
 *  Free the catalogue by converting the temporary file, if needed and then
 *  deleting it.
 */
int GaiaLocalCatalog::freeCat()
{
    if ( filename_ && realname_ ) {

        //  If intermediary tab table has been modified and the main catalogue
        //  has not, then convert it back to the original file, otherwise just
        //  remove it.
        time_t newtime = modDate( realname_ );
        if ( difftime( newtime, timestamp_ ) == 0.0 ) {

            //  Main catalogue not changed, so check the temporary file.
            if ( modified_ ) {

                //  This is changed, so convert it back.
                convertFrom();
            }
        }

        //  Now dispose of temporary file.
        dispose();
    }
    return 0;
}

/**
 *  Static member to test if a local catalogue is a known foreign catalogue
 *  type. These are the known CAT types and two plain ascii formats (asc and
 *  lis).
 */
int GaiaLocalCatalog::is_foreign( const char *name )
{
    //  Look for last '/'.
    const char *basename = strrchr( name, '/' );
    if ( ! basename ) {
        //  None, so use whole string.
        basename = name;
    }

    //  Search for last '.', note last not first. The tests below only
    //  check for a trailing type.
    const char *type = strrchr( basename, '.' );

    if ( type ) {
        type++;
        //  Check for .gz & .Z, these cannot be foreign.
        if ( strcasecmp( type, "gz" ) == 0 ||
             strcasecmp( type, "Z," ) == 0 ) {
            return 0;
        }

        if ( strncasecmp( type, "fit", 3 )  == 0 ||
             strncasecmp( type, "fits", 4 ) == 0 ||
             strncasecmp( type, "gsc", 3 )  == 0 ||
             strncasecmp( type, "asc", 3 )  == 0 ||
             strncasecmp( type, "lis", 3 )  == 0 ||
             strncasecmp( type, "txt", 3 )  == 0 ||
             strncasecmp( type, "xml", 3 )  == 0 ||
             strncasecmp( type, "vot", 3 )  == 0 ) {
            return 1;
        }
    }
    return 0;
}

/**
 *  Convert the current file "realname_" into a tab table.
 */
int GaiaLocalCatalog::convertTo()
{
    //  If realname_ is defined then attempt to convert it into a tab table.
    if ( realname_ ) {

        //  Make sure we're ok to attempt a conversion.
        if ( ! startConvert() ) {
            cerr << "Failed to start convertTo" << endl;
            return 0;
        }

        //  Convert to a temporary file.
        char buf[1024];
        sprintf( buf, "%s to %s %s", convertTable_, realname_, filename_ );
        if ( Tcl_Eval( interp_, buf ) != TCL_OK ) {
            cerr << buf << endl;
            cerr << "command failed:" << Tcl_GetStringResult(interp_) << endl;
            return 0;
        }
    }

    //  Record the modification time for this file.
    tempstamp_ = modDate( filename_ );
    return 1;
}

/**
 *  Convert temporary tab table into previous filename and type.
 */
int GaiaLocalCatalog::convertFrom()
{
    //  If filename_ is defined then attempt to convert it back.
    if ( filename_ ) {

        //  Make sure we're ok to attempt a conversion.
        if ( ! startConvert() ) {
            return 0;
        }

        //  Convert back to permanent file.
        char buf[1024];
        sprintf( buf, "%s from %s %s", convertTable_, filename_, realname_ );
        if ( Tcl_Eval( interp_, buf ) != TCL_OK ) {
            cerr << "command failed:" << Tcl_GetStringResult(interp_) << endl;
            return 0;
        }
    }
    modified_ = 0;
    return 1;
}

/**
 *  Check for and start the conversion [incr Tcl] class that controls the
 *  conversion process.
 */
int GaiaLocalCatalog::startConvert()
{
    //  Use a [incr Tcl] class GaiaConvertTable to control the conversion
    //  process. This controls the external filters and executes the
    //  conversion command. If a GaiaConvertTable object doesn't exist yet,
    //  then create one. Catch name with the namespace that is current and
    //  record for future use.
    char buf[256];
    if ( convertTable_[0] != '\0' ) {
        sprintf( buf, "info exists \"%s\"", convertTable_ );
        if ( Tcl_Eval( interp_, buf ) != TCL_OK ) {
            convertTable_[0] = '\0';
        }
        else {
            if ( strcmp( Tcl_GetStringResult( interp_ ), "0" ) == 0 ) {
                convertTable_[0] = '\0';
            }
        }
    }
    if ( convertTable_[0] == '\0' ) {
        if (Tcl_Eval( interp_, (char *) "gaia::GaiaConvertTable #auto" ) != TCL_OK) {
            return 0;
        }
        else {
            if ( Tcl_VarEval( interp_, "code ", Tcl_GetStringResult( interp_ ),
                              (char *) NULL ) != TCL_OK ) {
                return 0;
            }
            (void) strncpy( convertTable_, Tcl_GetStringResult( interp_ ),
                            NAMELEN - 1 );
        }
    }
    return 1;
}

/**
 *  Return the modification date of a file (see stat(2)).
 */
time_t GaiaLocalCatalog::modDate( const char *filename )
{
    struct stat buf;
    if ( stat( filename, &buf ) != 0 ) {
        return (time_t) 0;
    }
    return buf.st_mtime;
}

/**
 *  Dispose of the temporary file. No questions asked.
 */
void GaiaLocalCatalog::dispose()
{
    if ( filename_ ) {
        (void) remove( filename_ );
    }
    free( filename_ );
    filename_ = (char *) NULL;
}

/**
 *  Map in the temporary file and re-read its contents.
 */
int GaiaLocalCatalog::readTemp()
{
    if ( filename_ && access( filename_, F_OK ) == 0 ) {

        //  Mmap the file and enter into a TabTable.
        Mem m( filename_ );
        if ( m.status() != 0 ) {
            return TCL_ERROR;
        }

        //  Make a null terminated copy, which will be managed by info_.
        int size = m.size() + 1;
        char* data = (char *) malloc( size );
        if ( ! data ) {
            return fmt_error( "cannot allocate %d bytes for %s", size,
                              filename_ );
        }
        strncpy( data, (char*) m.ptr(), size-1 );
        data[ size-1 ] = '\0';

        //  This will extract any catalog config info from the file's header
        //  (do this before reading data which replaces \n with \0).
        info_.entry( entry_, data );

        //  Read the data.
        if ( info_.init( data, 0, 1 ) != 0 ) {
            return TCL_ERROR;
        }

        //  Copy the comments from table to entry.
        int n = info_.numComments();
        if ( n > 0 ) {
            char* c = NULL;
            int have = 1024;
            int l = 0;
            int used = 0;

            char* com = (char*)malloc( have );
            com[0] = '\0';
            char* p = com;

            for ( int i = 0; i < n; i++ ) {
                info_.getComment( i, c );
                l = strlen( c );
                if ( ( used + l ) >= have ) {
                    have += 1024;
                    com = (char*)realloc( com, have );
                    p = com + used;
                }
                strcpy( p, c );
                used += ( l + 1 );
                p += l;
                if ( i < ( n - 1 ) ) {
                    *p++ = '\n'; //  new line not NULL.
                }
            }
            entry_->comments( com );
            free( com );
        }

        //  This will extract any catalog config info from the file's header.
        info_.entry( entry_, data );

        //  Record modification date at this read.
        tempstamp_ = modDate( filename_ );
        return TCL_OK;
    }
    return TCL_ERROR;
}

/**
 *  If we don't have the info for this catalog, get it and return the status.
 *
 *  Here we also check if the files have been modified. If the real file has
 *  been modified we need to reconvert it. If the temporary file has been
 *  change we just need to reload it.
 */
int GaiaLocalCatalog::checkInfo()
{
    if ( info_.numCols() > 0 ) {

        //  Check the real file.
        time_t newtime = modDate( realname_ );
        if ( difftime( newtime, timestamp_ ) == 0.0 ) {

            //  Not changed, so check the temporary file, since it was last
            //  read.
            newtime = modDate( filename_ );
            if ( difftime( newtime, tempstamp_ ) == 0.0 ) {

                //  Neither are changed, so do nothing.
                return 0;
            }
        }
    }

    //  One of the files is changed, so reconvert or reload as necessary.
    return getInfo();
}

/**
 *  Static member to convert a tab table into a known catalogue type.
 *
 *  Arguments are:
 *
 *     in = name of tab table.
 *     out = name of new catalogue.
 */
int GaiaLocalCatalog::save( CatalogInfoEntry *e, Tcl_Interp *interp,
                            const char *in, const char *out )
{
    GaiaLocalCatalog *temp = new GaiaLocalCatalog( e, interp, in, out );
    if ( temp->convertFrom() ) {
        return TCL_OK;
    }
    return error( "failed to save file:", out );
}
