package edu.hawaii.jach.gsd;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.NoSuchElementException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.util.Iterator;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Map;
import java.util.SimpleTimeZone;
import java.util.TimeZone;

/**
 *  Read GSD file from disk and map data array.
 *  Data are read from mapped file on demand when requesting the
 *  data from GSDItem object.
 *
 *  Note that Java is big-endian but GSD is written in little-endian
 *  (VAX) format.
 *
 * @author  Tim Jenness (JAC)
 * @version $Id$
 */

public final class GSDObject {

        // Constants
	// Size of GSD numerical data types in bytes
	private static final int GSD__SZBYTE = GSDVAXBytes.VAX__SZBYTE;
	private static final int GSD__SZLOGICAL = GSDVAXBytes.VAX__SZLOGICAL;
	private static final int GSD__SZWORD = GSDVAXBytes.VAX__SZWORD;
	private static final int GSD__SZINTEGER = GSDVAXBytes.VAX__SZINTEGER;
	private static final int GSD__SZREAL = GSDVAXBytes.VAX__SZREAL;
	private static final int GSD__SZDOUBLE = GSDVAXBytes.VAX__SZDOUBLE;

	// These constants denote how much space is used for the different
	// types of strings and what the maximum number of dimensions are
	// allowed for array data. These values are required for the header read.
        // Make GSD__SZUNIT and GSD__SZNAME package-private so that we know the
        // largest possible size
        // of the name and unit for printing without having to look at all the unts.
	private static final int GSD__SZLABEL = 40;
	static final int GSD__SZUNIT = 10;
	static final int GSD__SZNAME = 15;
	private static final int GSD__MAXDIMS = 5;

	private static final char[] GSD__TYPES =
		{ 'B', 'L', 'W', 'I', 'R', 'D', 'C' };

	// The file data itself
	private ByteBuffer contents;

	// Global meta data from the file
	// Should be marked as final but we can not do this
	// because the values are set in a submethod and not a constructor
	// even though that method is called from the constructor.
	private int maxitems;
	private int no_items;
	private String label;
	private float version;
        private File file;
	private int start_data;
	private int end_data;
	private int size;

	// Item information plus lookup table
	private GSDItem items[];
	private Map itemmap;

	/**
	 * Constructs a GSDObject given a File object
	 *
	 * @param file File object corresponding to the name of the GSD file on disk.
	 * @throws FileNotFoundException if the specified file can not be found
	 *         or opened.
	 * @throws IOException if there is an error reading the file contents.
	 * @throws NullPointerException if an item describing an array
	 * dimension has not been read before the array item itself. This
	 * suggests an internal error in the organisation of the file.
	 */

    // Need to do the proper Java thing and also allow
    // GSDObject( File file )
	public GSDObject(File file)
		throws FileNotFoundException, IOException {

    	        String filename = file.getAbsolutePath();

		// Need to open the file
		RandomAccessFile fptr = new RandomAccessFile(filename, "r");

		// Once we have opened the file we simply map
		// it and pass the ByteBuffer to the real constructor

		// First need to get the channel
		FileChannel chan = fptr.getChannel();

		// And the mapped contents. Note that we map the entire file
		ByteBuffer allcontents =
			chan.map(FileChannel.MapMode.READ_ONLY, 0, fptr.length());

		// Close the file
		fptr.close();

		// File read okay so store the filename
		this.file = file;

		// Now parse the buffer
		this.parseBuffer(allcontents);

	}

	/**
	 * Process a ByteBuffer as if it were the contents of a GSD file.
	 * Assumes that the beginning of the buffer corresponds to the
	 * beginning of the GSD file. The buffer is rewound prior to use.
	 *
	 * @param contents ByteBuffer representing the GSD file.
	 * @throws NullPointerException if an item describing an array
	 * dimension has not been read before the array item itself. This
	 * suggests an internal error in the organisation of the buffer.
	 */
	public GSDObject(ByteBuffer contents) {
		// Reset the position of the buffer
		contents.rewind();

		// Immediately farm this off to the internal routine
		this.parseBuffer(contents);
	}

	//  General accessor methods
	/**
	  * The GSD file label.
	  */
	public String getLabel() {
		return this.label;
	}

	/**
	 * The number of items read from the file.
	 */
	public int getNumItems() {
		return this.no_items;
	}

	/**
	 * The version number of this GSD file.
	 */
	public float getGsdVersion() {
		return this.version;
	}


	/**
	 * The File object associated with the GSD file. If this object was constructed
	 * from a ByteBuffer the file object will not be defined.
	 */
	public File getFile() {
		return this.file;
	}

    /**
     * Return the filename associated with this GSD object. If this object was constructed
     * from a ByteBuffer the filename will be set to "<none>".
     */
        public String getFilename() {
	    String filename;
	    try {
		filename = getFile().getName();
	    } catch (NullPointerException e) {
		filename = "<none>";
	    }
	    return filename;
        }


/**
 * Return the date and time of observation as a java.util.Date object.
 * @return the UTC date of observation
 * @throws IllegalStateException if the C3DAT or C3UT items are missing.
 */
    public Date getDateObs() {
        Double dayObj;
        double dechour;

        // Need to get the three items relating to the DATE-OBS
        try {
            GSDItem dayItem = itemByName("C3DAT");
            GSDItem hourItem = itemByName("C3UT");
            GSDItem ut1cItem = itemByName( "C3UT1C");

            dayObj = (Double)dayItem.getData();
            Double hourObj = (Double)hourItem.getData();
            Double ut1cObj = (Double)ut1cItem.getData();

            // correction converted to hours
            double ut1c = ut1cObj.doubleValue() * 24.0;

            // Correct the hour to UTC = UT1 - UT1C
            dechour = hourObj.doubleValue() - ut1c;

        } catch (GSDException e) {
            throw new IllegalStateException("Error extracting date related fields from GSDObject: " + e);
        }
        // Now need to extract the Year month and day
        int year = dayObj.intValue();
        int month = (int)(100 *(dayObj.doubleValue()-(double)year));
        // Handle rounding errors
        int day = (int)( 0.5 + 10000*( (dayObj.doubleValue()) - (double)year) )
                             - 100*month;
        int hour = (int)dechour;
        int min = (int)(60*(dechour - hour));
        int sec = (int)(0.5+3600*(dechour-hour)) - 60*min;

        // correct for 0 offset
        month--;

        // Need the GMT time zone
        String[] ids = TimeZone.getAvailableIDs(0);
        if (ids.length == 0) {
            // Must have one match at least
            throw new IllegalStateException("Error retrieving GMT time zone");
        }

        // Create a TimeZone
        SimpleTimeZone ut = new SimpleTimeZone(0, ids[0]);

        // and a new calendar object for this time zone
        GregorianCalendar cal = new GregorianCalendar(ut);

        // and set the actual date
        cal.set(year, month, day, hour,min , sec);

        return cal.getTime();
    }

	/**
	 * Retrieve all the items associated with this GSD file.
	 * Note that the array returned by this method is indexed in the
	 * normal manner (starting at zero) and that this differs from the
	 * standard numbering of GSD items as used in the itemByNum() method.
	 *
	 * @return GSDItem[] array indexed from zero.
	 */
	public GSDItem[] items() {
		// Make sure we return a copy of the array so that the contents
		// and ordering of the items associated with this object can
		// not be tweaked.
		return (GSDItem[]) this.items.clone();
	}

    /**
     * Return an item iterator. Each time a call to .next() is made the next GSD
     * item is returned. Always starts from item 1.
     *
     */

    public Iterator getItemIterator() {
	return new Iterator () {
		private int itemno = 1;

		public Object next() {
		    if (hasNext()) {
			Object item = itemByNum( itemno );
			itemno++;
			return item;
		    } else {
			throw new NoSuchElementException();
		    }
		}

		/* Return true if we have another item */
		public boolean hasNext() {
		    return itemno <= getNumItems();
		}
		public void remove() {
		    throw new UnsupportedOperationException();
		}
	    };
    }

	/**
	  * Return item contents by GSD item name. Item names are case
	  * insensitive. The name can be either the long name or the short
	  * name.
          *
	  * @param itemname Name of the requested item.
	  * @return a GSDItem object with all the Item information
	  * @throws GSDException if the named item is not present.
	  */
	public GSDItem itemByName(String itemname) throws GSDException {
		itemname = itemname.toUpperCase();
		GSDItem result = (GSDItem) this.itemmap.get(itemname);
		if (result == null) {
			throw new GSDException(
				"Item " + itemname + " is not present in the file");
		}
		return result;
	}

	/**
	 * Return item contents by GSD item number.
	 *
	 * @param itemno The item number (starting at 1, maximum value defined
	 *               by getNumItems)
	 * @return a GSDItem object with all the Item information
	 * @throws IndexOutOfBoundsException  if the requested item number is
	 *         outside the allowed range.
	 */
	public GSDItem itemByNum(int itemno) {
		if (itemno < 1 || itemno > this.getNumItems()) {
			throw new IndexOutOfBoundsException(
				"Requested item, "
					+ itemno
					+ ", is outside the file limits of 1 and "
					+ this.getNumItems());
		}
		GSDItem result = this.items[itemno - 1];
		return result;
	}

	/**
	 * Returns a short summary of the object. The format of this string
	 * is not specified but may include the file name and the number of
	 * data items.
	 */
	public String toString() {
		String datastring;
		try {
			GSDItem maindata = this.itemByName("C13DAT");
			datastring = " Data array size: " + maindata.size() + " elements";
		} catch (GSDException e) {
			datastring = "No C13DAT";
		}
		String result =
			"GSD file: "
				+ this.getFilename()
				+ " Number of items: "
				+ this.getNumItems()
				+ datastring;
		return result;
	}

	/**
	 * Dump the contents of the file as text to standard out
	 * without special formatting. Equivalent to the gsdprint
	 * command.
	 */
	public void print() {
		System.out.println(
			"-----------------------------------------------------");
		System.out.println(" G S D   P R I N T");
		System.out.println(
			"-----------------------------------------------------");
		System.out.println("");
		System.out.println(" Filename       : " + getFilename());
		System.out.println(" GSD version    : " + getGsdVersion());
		System.out.println(" Label          : " + getLabel());
		System.out.println(" Number of items: " + getNumItems());
                System.out.println(" Date of Observation: " + getDateObs());
		System.out.println("");
		System.out.println("");
		System.out.println("");
		System.out.println(
			"Name             Unit       Type    Arr?    Value");
		System.out.println(
			"-----------------------------------------------------");

		// Upper limit for number of data values to print. Should
		// be an argument so that a command line option can be
		// triggered.
		long maxsize = 1024;

		// Loop over each item. Draw a divider if we have just reached
		// an array item
		Iterator all = getItemIterator();

		while ( all.hasNext() ) {
  		        GSDItem item = (GSDItem)all.next();

			// print a divider if we are an array item
			if (item.isArray()) {
				System.out.println(
					"-----------------------------------------------------");
			}

			item.dumpIt(maxsize);

		}
	}

	/* I N T E R N A L   I O   R O U T I N E S */

	private void parseBuffer(ByteBuffer buf) {

		// First read the file header
		this.readFileDesc(buf);

		// Then read the Item descriptor information
		this.readItemDesc(buf);

		// Now map the data array and close the file
		this.attachData(buf);

		// and populate the look up table for fast by-name access
		this.fillItemMap();

	}

	private void readFileDesc(ByteBuffer buf) {
		// Read the first few bytes for the general file description
		// Assumes the buffer is at the start of the GSD contents.

		/**
		   The GSD file descriptor is defined as follows:
		   [See also gsd1.h in struct file_descriptor]

		   float version;      GSD file format version for SPECX
		   int   max_no_items;    Maximum number of items in file ?
		   int   no_items;        Number of items in this file    ?
		   int   str_data;        Start of data area in file - byte number
		   int   end_data;        End of data area - byte number
		   char  comment[40];
		   int size;

		*/

		// For efficiency reasons do a single read and then extract
		// the contents from the byte buffer
		int[] struct = { GSD__SZREAL, // version
			GSD__SZINTEGER, // max_no_items
			GSD__SZINTEGER, // no_items
			GSD__SZINTEGER, // str_data
			GSD__SZINTEGER, // end_data
			GSD__SZLABEL, // comment[40]
			GSD__SZINTEGER // size
		};

		int szstruct = 0;
		for (int i = 0; i < struct.length; i++) {
			szstruct += struct[i];
		}

		// offset into byte buffer
		int offset = 0;

		// position in struct
		int i = 0;

		// Some bytes for the struct
		byte[] byteArray = new byte[szstruct];

		// Read the data
		buf.get(byteArray);

		// Versions
		this.version = GSDVAXBytes.tofloat(byteArray, offset);

		offset += struct[i];
		i++;
		this.maxitems = GSDVAXBytes.toint(byteArray, offset);

		offset += struct[i];
		i++;
		this.no_items = GSDVAXBytes.toint(byteArray, offset);

		offset += struct[i];
		i++;
		this.start_data = GSDVAXBytes.toint(byteArray, offset);

		offset += struct[i];
		i++;
		this.end_data = GSDVAXBytes.toint(byteArray, offset);

		offset += struct[i];
		i++;
		this.label = GSDVAXBytes.tostring(byteArray, offset, GSD__SZLABEL);

		offset += struct[i];
		i++;
		this.size = GSDVAXBytes.toint(byteArray, offset);

		//System.out.println("Read: " + this.no_items +
		//		   " and "  + this.maxitems +
		//		   " and " + this.version +
		//		   " and " + this.label + ";"
		//		   );
		//System.out.println( "Start pos: " + this.start_data +
		//		    " End pos: " + this.end_data +
		//		    " Size: " + this.size);

	};

	/**
	 * For each of the no_items items in the file the next
	 * block of the buffer contains the information associated
	 * with each item but not the data itself
	 * [which is to be extracted later]. Assumes the buffer is at
	 * the correct position.
	 *
	 * @throws NullPointerException if an item describing an array
	 * dimension has not been read before the array item itself.
	 */
	private void readItemDesc(ByteBuffer buf) {

		/* The struct is implemented as follows:
		   char  array;
		   char  name[15];
		   short namelen;
		   char  unit[10];
		   short unitlen;
		   short data_type;
		   int   location;
		   int   length;
		   int   no_dims;
		   int   dimnumbers[5];

		   But we assume there are no extra bytes padding the file
		   for now (a safe assumption since it is unlikely that
		   the VAX will decide to change format on us)
		*/

		// For efficiency reasons do a single read of the item block
		// and then extract the contents from the byte buffer
		int[] struct = { GSD__SZLOGICAL, // array
			GSD__SZNAME, // name[15]
			GSD__SZWORD, // namelen
			GSD__SZUNIT, // unit[10]
			GSD__SZWORD, // unitlen
			GSD__SZWORD, // data_type
			GSD__SZINTEGER, // location
			GSD__SZINTEGER, // length
			GSD__SZINTEGER, // no_dims
			GSD__SZINTEGER * GSD__MAXDIMS // dimnumbers[5]
		};

		int szstruct = 0;
		for (int i = 0; i < struct.length; i++) {
			szstruct += struct[i];
		}

		// Read in a byte array for all the item descriptions
		// There should not be a massive overhead to this since the
		// number of items is always finite (and for JCMT no more than
		// 200 items)
		byte[] byteArray = new byte[szstruct * this.no_items];

		// Somewhere to put the items contents
		items = new GSDItem[this.no_items];

		// Read the buffer in one ago. I assume this is more efficient
		// than using little chunks
		buf.get(byteArray);

		// Loop over each scalar item in turn. We will do the array items
		// with a second pass
		for (int i = 0; i < this.no_items; i++) {

			// recalculate global offset on the basis of index rather than
			// assuming we have started at the beginning
			int offset = (szstruct * i);

			// index into current struct
			int j = 0;

			// isArray item?
			boolean isArray = GSDVAXBytes.tobool(byteArray, offset);

			// Item name and then its length
			offset += struct[j];
			j++;
			String itemname =
				GSDVAXBytes.tostring(byteArray, offset, GSD__SZNAME);

			offset += struct[j];
			j++;
			short namelen = GSDVAXBytes.toshort(byteArray, offset);

			// Extract the substring of the right length
			itemname = itemname.substring(0, namelen);

			// Data units [10 characters] plus length
			offset += struct[j];
			j++;
			String itemunit =
				GSDVAXBytes.tostring(byteArray, offset, GSD__SZUNIT);

			offset += struct[j];
			j++;
			short unitlen = GSDVAXBytes.toshort(byteArray, offset);

			// Must trim the string
			itemunit = itemunit.substring(0, unitlen);

			// Data type
			offset += struct[j];
			j++;
			short type = GSDVAXBytes.toshort(byteArray, offset);

			// curritem.type( GSD__TYPES[type-1] );
			//System.out.println("Data type = " + curritem.type() );

			// Location of this item in the data array
			// corrected for the start position of the GSD data segment
			offset += struct[j];
			j++;
			int start_byte =
				GSDVAXBytes.toint(byteArray, offset) - this.start_data;

			// Number of bytes in data array to read
			offset += struct[j];
			j++;
			int nbytes = GSDVAXBytes.toint(byteArray, offset);

			// Number of dimensions if array
			// Some dimensions can be negative (!) -1
			// Set those to 0 assuming we are scalar
			offset += struct[j];
			j++;
			int ndims = GSDVAXBytes.toint(byteArray, offset);

			//System.out.println("Location: " +  curritem.start_byte() +
			//		       " Length = "  + curritem.nbytes()  +
			//		       " bytes, Ndims  = " + curritem.ndims()
			//		       );

			// This is an array of item numbers corresponding to each
			// dimension in the scalar part of the header. If one of the
			// dimensions says 83 that means the dimension is specified
			// by the contents of scalar item 83.
			offset += struct[j];
			int[] dimnumbers =
				GSDVAXBytes.tointArray(byteArray, offset, GSD__MAXDIMS);

			// Construct an array of items that can be used for the
			// GSDItem constructor.
			// Must force ndims to zero
			if (ndims < 0) {
				ndims = 0;
			}
			GSDItem[] dimitems = new GSDItem[ndims];
			if (isArray) {
				for (int k = 0; k < ndims; k++) {
					// remember to offset by 1
					dimitems[k] = items[dimnumbers[k] - 1];
					if (dimitems[k] == null) {
						throw new NullPointerException(
							"An attempt was made to read a scalar item [#"
								+ dimnumbers[k]
								+ "] that has not been read from the file yet!");
					}
				}
			}

			// and create the actual object
			GSDItem curritem =
				new GSDItem(
					i + 1,
					start_byte,
					nbytes,
					ndims,
					itemname,
					itemunit,
					GSD__TYPES[type - 1],
					dimitems);

			// and place the item in context
			items[i] = curritem;

		}

	}

	// Associated each item with its short and long name.
	private void fillItemMap() {
		// use HashMap for now
		itemmap = new HashMap(getNumItems());

		// Now read in all the items
		for (int i = 0; i < getNumItems(); i++) {
            itemmap.put(items[i].name(), items[i]);
            itemmap.put(items[i].longName(), items[i]);
		}
	}

	// Attach the actual data to each of the individual items
	private void attachData(ByteBuffer buf) {

		// Set the buffer offset
		buf.position(start_data);

		// Get a slice
		contents = buf.slice();

		// And attach this to all the items so that they can
		// read the data themselves
		for (int i = 0; i < getNumItems(); i++) {
			items[i].contents(contents);
		}

	}

}
