/*+
 *  Name:
 *     VOTableFunctions

 *  Purpose:
 *     Define VOTable members that require namespace qualification
 *     support through macros. The namespace (votable_11 or votable11_dns)
 *     should be defined using the NS macro before including this file.

 *  Language:
 *     C++ include file.

 *  Copyright:
 *     Copyright (C) 2008 Science and Technology Facilities Council.
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

 *  Authors:
 *     PWD: Peter W. Draper (JAC, Durham University)

 *  History:
 *     07-JUL-2008 (PWD):
 *        Original version.
 *     {enter_changes_here}
 *-
 */

/*  Prototypes for members that are overloaded by namespace qualified
 *  types. */
void votable_enum( NS::VOTABLE& votable, ostream& str );

int votable_count( NS::VOTABLE& votable );

NS::TABLE &votable_get( NS::VOTABLE& votable, int index );

int votable_write( NS::VOTABLE& votable, int index, ofstream& out );

int table_write( const NS::TABLE& table, ofstream& out );

int table_params( const NS::TABLE& table, ofstream& out );

int table_data( const NS::TABLE& table, ofstream& out );

int table_columns( const NS::TABLE& table, ofstream& out, int& id_index,
                   int& ra_index, bool& ra_radians, string& ra_unit,
                   int& dec_index, bool& dec_radians, string& dec_unit  );

void table_stc( const NS::GROUP& group, ofstream& out );

void table_coosys( const NS::COOSYS& coosys, ofstream& out );

int table_nfields( NS::TABLE& table );

void table_description( NS::TABLE& table, string& description );

int data_tabledata( const NS::TABLE& table, const NS::TABLEDATA& tdata,
                    ofstream& out, int& id_index,
                    int& ra_index, bool& ra_radians, string& ra_unit,
                    int& dec_index, bool& dec_radians, string& dec_unit );

int data_binarydata( const NS::TABLE& table, const NS::BINARY& bdata,
                     ofstream& out, int& id_index, int& ra_index,
                     bool& ra_radians, int& dec_index, bool& dec_radians );

int data_fitsdata( const NS::FITS& fdata, ofstream& out, int& id_index,
                   int& ra_index, bool& ra_radians, int& dec_index,
                   bool& dec_radians );

int field_arraysize( const NS::FIELD& field );

int votable_info_value( NS::VOTABLE& votable, const char *namevalue,
                        string &value , string &content );

