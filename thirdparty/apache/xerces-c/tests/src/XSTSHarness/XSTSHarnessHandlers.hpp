/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * $Id: XSTSHarnessHandlers.hpp 677563 2008-07-17 11:51:39Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/sax2/SAX2XMLReader.hpp>
#include <xercesc/sax2/Attributes.hpp>
#include <xercesc/sax2/DefaultHandler.hpp>
#include <xercesc/util/XMLURL.hpp>
#include <xercesc/util/RefVectorOf.hpp>

XERCES_CPP_NAMESPACE_USE

class XSTSErrorHandler : public ErrorHandler
{
public:
    XSTSErrorHandler() : fSawErrors(false) {}

    bool getSawErrors() const
    {
        return fSawErrors;
    }
    const XMLCh* getErrorText()
    {
        return fErrorText.getRawBuffer();
    }

    // -----------------------------------------------------------------------
    //  Handlers for the SAX ErrorHandler interface
    // -----------------------------------------------------------------------
    void warning(const SAXParseException& exc)      {}
    void error(const SAXParseException& exc);
    void fatalError(const SAXParseException& exc);
    void resetErrors()                              { fSawErrors=false; fErrorText.reset(); }

private:
    // -----------------------------------------------------------------------
    //  Private data members
    //
    //  fSawErrors
    //      This is set by the error handlers, and is queryable later to
    //      see if any errors occured.
    // -----------------------------------------------------------------------
    bool            fSawErrors;
    XMLBuffer       fErrorText;
};

typedef enum
{
    unknown,
    invalid,
    valid
} ValidityOutcome;

class XSTSTest
{
public:
    XSTSTest() :
      fXSDNames(1) 
    {
        fTestName[0]=0;
        fExpectedResult=unknown;
        fSkipped=false;
    }

    XMLCh           fTestName[256];
    RefVectorOf<XMLURL> fXSDNames;
    XMLURL          fXMLName;
    ValidityOutcome fExpectedResult;
    XMLURL          fSpecReference;
    bool            fSkipped;
};

class XSTSHarnessHandlers : public DefaultHandler
{
public:
    // -----------------------------------------------------------------------
    //  Constructors and Destructor
    // -----------------------------------------------------------------------
    XSTSHarnessHandlers(const XMLCh* baseURL);
    ~XSTSHarnessHandlers();

    unsigned int getTotalTests() const
    {
        return fTests;
    }
    unsigned int getFailedTests() const
    {
        return fFailures;
    }

    bool getSawErrors() const
    {
        return fSawErrors;
    }

    // -----------------------------------------------------------------------
    //  Handlers for the SAX ContentHandler interface
    // -----------------------------------------------------------------------
    void startElement(const XMLCh* const uri, const XMLCh* const localname, const XMLCh* const qname, const Attributes& attrs);
    void endElement(const XMLCh* const uri, const XMLCh* const localname, const XMLCh* const qname);

    // -----------------------------------------------------------------------
    //  Handlers for the SAX ErrorHandler interface
    // -----------------------------------------------------------------------
	void warning(const SAXParseException& exc);
    void error(const SAXParseException& exc);
    void fatalError(const SAXParseException& exc);
    void resetErrors();

protected:
    void printFile(XMLURL& url);

private:
    // -----------------------------------------------------------------------
    //  Private data members
    //
    //  fSawErrors
    //      This is set by the error handlers, and is queryable later to
    //      see if any errors occured.
    // -----------------------------------------------------------------------
    bool                fSawErrors;
    XSTSTest            fCurrentTest;
    XMLURL              fBaseURL;
    unsigned int        fFailures, fTests;
    SAX2XMLReader*      fParser;
    XSTSErrorHandler    fErrorHandler;
};

