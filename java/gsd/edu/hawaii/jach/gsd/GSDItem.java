package edu.hawaii.jach.gsd;

import java.io.*;
import java.nio.*;
import java.nio.channels.*;
import java.lang.reflect.*;

/**
 * Information relating to a particular item in a GSD file.
 * Currently the data type, dimensionality, units and dimensional
 * units are available as well as the data. Non-scalar data is vectorized.
 * To minimize memory requirements, non-scalar data are read from the GSD
 * file on-demand rather than reading the whole file into memory when
 * the file is opened.
 *
 * Objects are created for each item in a GSD file by the GSDObject
 * constructor. In general, GSDItem objects are usually not instantiated
 * from user code. Whilst the public interface does allow for the attributes
 * stored in the object to be modified, this is usually only useful
 * for object population via GSDObject class since the data retrieved
 * from GSD files are currently read-only and can not be modified on disk.
 *
 * @author  Tim Jenness (JAC)
 * @version $Id$
 */

public class GSDItem {

    // Only the Item needs to know how many bytes are used
    // to represent a string in the data segment of a GSD file.
    private static final int GSD__SZCHAR    = 16;

    // Instance variables that contain information useful
    // for public consumption.
    private int number;
    private String name;
    private String unit;
    private char type;
    private boolean isArray;

    // For array items
    private String[] dimnames;
    private String[] dimunits;
    private int[]    dimensions;
    private int[]    dimnumbers; // The item numbers corresponding to dims

    // This is the actual data.
    // Note that we must cast to the correct type
    // Do not yet know enough java to do this with a single Object
    private Object data;


    // The following information is required to find the
    // item in the byte stream. These have public setter methods
    // but once set (by the GSDObject) can not be retrieved again.

    // This is the contents of the entire GSD file data segment
    // Usually is a MappedByteBuffer.
    // It is identical to the contents attribute found in GSDObject class.
    private ByteBuffer contents;

    // This is the offset into the ByteBuffer, relative to the
    // start of the buffer and not the start of the GSD file
    private int start_byte;

    // The number of byte representing the data in the GSD data segment
    private int nbytes;

    // The actual number of dimensions associated with the data.
    // Useful for stopping loops whilst reading the data in.
    private int ndims;


    // Set methods. Note that start_byte, ndims and contents are here
    // since they are required in order to allow the object to be
    // configured by the GSDObject class. Alternative is to provide
    // a constructor that includes these attributes.

    /**
     * Set the number of the item in the original GSD file.
     *
     */
    public void number( int itemnum ) { this.number = itemnum; };

    /**
     * Set the start byte position for this item in the mapped data buffer.
     * Set by the GSDObject when the data segment is mapped.
     *
     * @param pos Offset position from start of ByteBuffer.
     */
    public void start_byte( int pos ) { this.start_byte = pos; };

    /**
     * Set the number of bytes used to represent the data in the GSD file.
     *
     */
    public void nbytes( int numbytes ) { this.nbytes = numbytes; };

    /**
     * Set the number of dimensions in the data. Zero indicates a scalar.
     *
     */
    public void ndims( int numdims ) { this.ndims = numdims; };

    /**
     * Set the (possibly) mapped ByteBuffer associated with the data
     * array in the GSD file (not this specfic item). Will be null if
     * the file has been unmapped.
     *
     */
    public void contents( ByteBuffer buf ) {this.contents = buf; }

    /**
     * Set the name of the item in the GSD file. GSD item names are
     * always uppercased.
     *
     * @param item_name
     */
    public void name( String item_name ) { 
	this.name = item_name.toUpperCase(); 
    };

    /**
     * Set the unit associated with the data.
     *
     */
    public void unit( String item_unit ) { this.unit = item_unit; };

    /**
     * Set whether the item is an array or scalar. Could also determine
     * this from the ndims() attribute.
     */
    public void isArray( boolean arrok ) { this.isArray = arrok; };

    /**
     * Set the type of the item (as a single character).
     *
     */
    public void type( char dtype ) { this.type = dtype; };

    /**
     * Set the size of each of the data dimensions.
     */
    public void dimensions( int[] dims ) { this.dimensions = dims; };

    /**
     * Set the item numbers associated with each data dimension.
     */
    public void dimnumbers( int[] dimitems ) { this.dimnumbers = dimitems; };

    /**
     * Set the name of each dimensions.
     */
    public void dimnames( String[] dimnam ) { this.dimnames = dimnam; };

    /**
     * Set the units of each dimension.
     */
    public void dimunits( String[] dimunt ) { this.dimunits = dimunt; };

    /**
     * Store the actual data associated with this item.
     */
    public void setData( Object thedata ) { this.data = thedata; };

    // Accessor methods

    // Extra accessors that must be documented
    // Returns the start position of the byte stream. For internal use
    // only. This avoids confusion.
    private int start_byte() { return this.start_byte; }

    // Number of bytes representing the data in the byte buffer
    private int nbytes() { return this.nbytes; }

    /**
     * The number of dimensions in the data. Zero for scalar items.
     * This is also the number of elements in the dimnames(),
     * dimunits() and dimensions() arrays.
     */
    public int ndims() { return this.ndims; }

    /**
     * For array items, each dimension in the data array is described
     * by a corresponding scalar item. This method returns the item numbers
     * associated with each dimension in the data. The item information
     * can then be retrieved by using the GSDObject.itemByNum() method.
     */
    public int[] dimnumbers() { return this.dimnumbers; }

    /**
     * The item number in the original GSD file.
     */
    public int number() { return this.number;}

    /**
     * The name of the item.
     */
    public String name() { return this.name; }

    /**
     * The units associated with the data.
     */
    public String unit() { return this.unit; }

    /**
     * A single character describing the underlying data type.
     * Allowed values are:
     * <dl>
     * <dt>C</dt><dd>Character string(s).</dd>
     * <dt>D</dt><dd>Double precision.</dd>
     * <dt>R</dt><dd>Float</dd>
     * <dt>I</dt><dd>Integer</dd>
     * <dt>W</dt><dd>Short</dd>
     * <dt>B</dt><dd>Byte</dd>
     * <dt>L</dt><dd>Boolean</dd>
     * </dl>
     */
    public char type() { return this.type; }

    /**
     * Indicates whether the item is scalar or an array of values.
     */
    public boolean isArray() { return this.isArray; }

    /**
     * If an array, this is the dimensionality of the data array.
     */
    public int[] dimensions() { return this.dimensions; }

    /**
     * If an array, this is the name of each axis.
     */
    public String[] dimnames() { return this.dimnames; }

    /**
     * If an array, this is the units associated with each axis.
     */
    public String[] dimunits() { return this.dimunits; }

    /**
     * Returns an object representing the data. If the data have not
     * yet been read from file, a read is forced if the GSD file is
     * still open. Returns null if the data can not be read. Scalar
     * data are stored as java.lang.Number objects. Array data
     * are stored in int[], float[], String[] etc but stored in this
     * object as objects.
     
     */
    public Object getData() throws IOException, GSDException { 
	if (this.data == null) {
	    // try to read
	    this.attachItemData();
	}
	return this.data;
    }

    /**
     * Return the number of data elements associated with the item.
     * Calculated from the dimensions and so does not force a read
     * of the item data.
     */
    public int size() {
	int nelm = 1;
	if (this.isArray() ){
	    int[] dims = this.dimensions;
	    for (int i=0; i<this.ndims; i++) {
		nelm *= dims[i];
	    }
	}
        return nelm;
    }

    /**
     * Print the contents of the Item to standard output regardless
     * of the size of the item.
     */
    public void dumpIt() throws IOException, GSDException {
	// Simply call dumpIt with a negative value
	dumpIt( -1 );
    }

    /**
     * Print the contents of the Item to standard output. If the item
     * represents an array item the data values will only be printed
     * if the number of items is less than or equal to the specified maximum.
     *
     * @param maxsize  Only print data arrays if they are smaller than this
     *                 value. A negative value indicates that the length is
     *                 dumped regardless of size.
     */

    public void dumpIt( long maxsize ) throws IOException, GSDException {

	String header = this.name() + "      " + this.unit() +
	    "     " + this.type();

	// Different display depending on array type
	if (this.isArray()) {
	    String size = "" + this.size();
	    header = header + " 1" + "      " +
		"Array size=" + size;
	} else {
	    header = header + " 0" + "      " + this.getData();
	}
	System.out.println(header);
	
	if (this.isArray) {
	    // dimensions
	    int[] dims = this.dimensions;
	    if ( dims != null ) {
		for(int i=0;i<this.dimensions.length;i++){
		    System.out.println(" > " + this.dimnames[i] +
				       " " +  this.dimensions[i] +
				       " [" + this.dimunits[i] + "] ");
		}
	    } else {
		System.out.println(" > Dimensions not defined ");
	    }

	    // Print out the data values 4 per line
	    System.out.println(" Data:");
	    String row = "";
	    int length = this.size();
	    if ((length <= maxsize) || (maxsize < 0)) {
		// Force read of data
		Object arrdata = this.getData();
		if (arrdata != null) {
		    for (int i = 0; i< length; i++) {
			row = row + "   " + Array.get(arrdata,i);
			if ((i+1) % 4 == 0) {
			    System.out.println(row);
			    row = "";
			}
		    }
		    // print out the final row even if it is short
		    if (row.length() > 0) {
			System.out.println(row);
		    }
		} else {
		    System.out.println("Data array not defined");
		}
	    } else {
		System.out.println("Number of values exceeds limit");
	    }
	}

    }

    /**
     * Given a GSDItem, attempt to attach data to it. Nothing is
     * returned, the Item is modified in place. This method is called
     * automatically when the GSDItem.getData() method is invoked if
     * the data has not previously been read from file. 
     */
    private void attachItemData() throws IOException, GSDException {
	// First we need to read the bytes from the file
	// This may not be the most memory efficient approach
	// for really large buffers

	// First make sure we have a buffer
	if (this.contents == null) {
	    return;
	}

	// find out the type
	char type = this.type();

	// Now we need to convert to native number[s]
	if (this.isArray()) {
	    // could do scalars and arrays with the same logic
	    // simply because the number of elements will be one
	    // each time. I need to practice with casting an Object[]
	    // to an Object first.

	    if (type == 'B') {
		this.setData( GSDVAXBytes.tobyteArrayNB( contents,
							 this.start_byte(),
							 this.nbytes() ) );
	    } else if (type == 'L') {
		this.setData( GSDVAXBytes.toboolArrayNB( contents,
							 this.start_byte(),
							 this.nbytes() ) );
	    } else if (type == 'W') {
		this.setData( GSDVAXBytes.toshortArrayNB( contents,
							  this.start_byte(),
							  this.nbytes() ) );
	    } else if (type == 'I') {
		this.setData( GSDVAXBytes.tointArrayNB( contents,
							this.start_byte(),
							this.nbytes() ) );
	    } else if (type == 'R') {
		this.setData( GSDVAXBytes.tofloatArrayNB( contents,
							  this.start_byte(),
							  this.nbytes() ) );

	    } else if (type == 'D') {
		this.setData( GSDVAXBytes.todoubleArrayNB( contents,
							   this.start_byte(),
							   this.nbytes() ) );
	    } else if (type == 'C') {
		this.setData( GSDVAXBytes.tostringArrayNB( contents,
							   this.start_byte(),
							   this.nbytes(),
							   GSD__SZCHAR) );
	    }
	    

	} else {

	    // Then position the buffer offset
	    this.contents.position( this.start_byte() );

	    // a byte array to receive the scalar data
	    // We know this is going to be a small array
	    // so do not need to worry about memory consumption
	    byte[] byteArray = new byte[ this.nbytes() ];

	    // and read in the data
	    contents.get( byteArray );
	
	    // switch on type
	    if (type == 'B') {
		this.setData( new Byte( GSDVAXBytes.tobyte( byteArray ) ) );
	    } else if (type == 'L') {
		this.setData( new Boolean( GSDVAXBytes.tobool( byteArray ) ) );
	    } else if (type == 'W') {
		this.setData( new Short( GSDVAXBytes.toshort( byteArray ) ) );
	    } else if (type == 'I') {
		this.setData( new Integer( GSDVAXBytes.toint( byteArray ) ) );
	    } else if (type == 'R') {
		this.setData( new Float( GSDVAXBytes.tofloat( byteArray ) ) );
	    } else if (type == 'D') {
		this.setData( new Double( GSDVAXBytes.todouble( byteArray ) ));
	    } else if (type == 'C') {
		this.setData( GSDVAXBytes.tostring( byteArray ) );
	    }
	}
    }


}
