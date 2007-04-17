# E.S.O. - VLT project 
# "@(#) $Id: SkyCatHduChooser.tcl,v 1.1.1.1 2006/01/12 16:41:55 abrighto Exp $"
#
# SkyCatHduChooser.tcl - Itcl widget for displaying FITS extensions
# 
# See man page SkyCatHduChooser(n) for a complete description.
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  09/11/98   Created

itk::usual SkyCatHduChooser {}

# This class defines a widget for displaying the HDUs in the current FITS
# image. The user can select a FITS table or image extension to display 
# by clicking on an entry the list or on one of the small images displayed 
# in a table.

itcl::class skycat::SkyCatHduChooser {
    inherit rtd::RtdImageHduChooser


    # constructor: create a toplevel window

    constructor {args} {
	eval itk_initialize $args
    }
    

    # Display the current FITS table

    protected method display_fits_table {name hdu} {
	# build the name from the catalog name and the file base name
	set file [file tail [file rootname [$image_ cget -file]]]
	if {[string first "$file-" $name] == 0} {
	    set filename /tmp/$name
	} else {
	    set filename "/tmp/$file-$name"
	}

	# get the catalog config entry from the $catinfo table
	set entry [get_config_entry_from_fits_table $name $filename]

	# copy the FITS table to a temporary local catalog
	if {[catch {$image_ hdu get $hdu $filename $entry} msg]} {
	    error_dialog $msg
	    return
	}

	# display the catalog
	cat::AstroCat::new_catalog $filename $itk_option(-image) ::skycat::SkySearch
    }


    # Return the catalog config entry for the named FITS table, if
    # available, or a default entry. If the current FITS file contains 
    # an HDU named $catinfo, with an entry for the named catalog ($extname), 
    # then extract and return that entry as a Tcl keyed list.
    
    protected method get_config_entry_from_fits_table {extname filename} {
	set headings [$image_ hdu listheadings]

	# the first part of the catalog config entry is always the same
	set entry {}
	lappend entry [list serv_type local]
	lappend entry [list short_name $extname]
	lappend entry [list long_name $extname]
	lappend entry [list url $filename]

	foreach row [$table_ cget -info] {
	    eval lassign [list $row] $headings
	    if {"$ExtName" == "$catinfo"} {
		# found table
		set headings [$image_ hdu headings $HDU]
		foreach row [$image_ hdu get $HDU] {
		    eval lassign [list $row] $headings
		    if {"$SHORT_NAME" == "$extname"} {
			# found entry
			foreach key $headings {
			    set value [set $key]
			    set key [string tolower $key]
			    if {"[string trim $value]" != ""} {
				lappend entry [list $key $value]
			    }
			}
			return $entry
		    }
		}
		break
	    }
	}
	# no entry found, use default (no plotting)
	foreach i {id_col ra_col dec_col x_col y_col} {
	    lappend entry [list $i -1]
	}
	return $entry
    }

    
    # -- options  --

    # name of the FITS table containing catalog config info
    public variable catinfo "CATINFO"

    # -- protected vars --
    
    # C++ astrocat object use here to access catalog entries
    common astrocat_ [astrocat ::cat::.cataloginfo]
}
