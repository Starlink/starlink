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
 * $Id: XSTSHarnessHandlers.cpp 677563 2008-07-17 11:51:39Z amassari $
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include "XSTSHarness.hpp"
#include <xercesc/sax2/Attributes.hpp>
#include <xercesc/sax/SAXParseException.hpp>
#include <xercesc/sax/SAXException.hpp>
#include <xercesc/validators/common/Grammar.hpp>
#include <xercesc/util/OutOfMemoryException.hpp>
#include <xercesc/util/BinInputStream.hpp>

// ---------------------------------------------------------------------------
//  XSTSHarnessHandlers: Constructors and Destructor
// ---------------------------------------------------------------------------
XSTSHarnessHandlers::XSTSHarnessHandlers(const XMLCh* baseURL) :
    fSawErrors(false),
    fBaseURL(baseURL),
    fFailures(0), 
    fTests(0)
{
    fParser = XMLReaderFactory::createXMLReader();
    fParser->setFeature(XMLUni::fgSAX2CoreNameSpaces, true);
    fParser->setFeature(XMLUni::fgSAX2CoreNameSpacePrefixes, true);
    fParser->setFeature(XMLUni::fgSAX2CoreValidation, true);
    fParser->setFeature(XMLUni::fgXercesSchema, true);
    fParser->setFeature(XMLUni::fgXercesSchemaFullChecking, true);
    fParser->setFeature(XMLUni::fgXercesDynamic, false);
    fParser->setFeature(XMLUni::fgXercesUseCachedGrammarInParse, true);
    fParser->setFeature(XMLUni::fgXercesIdentityConstraintChecking, true);
    fParser->setErrorHandler(&fErrorHandler);
}

XSTSHarnessHandlers::~XSTSHarnessHandlers()
{
    delete fParser;
}

static XMLCh urlW3C[]={ chLatin_h, chLatin_t, chLatin_t, chLatin_p, chColon, chForwardSlash, chForwardSlash, 
                        chLatin_w, chLatin_w, chLatin_w, chPeriod, chLatin_w, chDigit_3, chPeriod, chLatin_o, chLatin_r, chLatin_g, chForwardSlash,
                        chLatin_X, chLatin_M, chLatin_L, chForwardSlash, 
                        chLatin_S, chLatin_c, chLatin_h, chLatin_e, chLatin_m, chLatin_a, chNull };

static XMLCh szTestSuite[]={ chLatin_T, chLatin_e, chLatin_s, chLatin_t, chLatin_S, chLatin_u, chLatin_i, chLatin_t, chLatin_e, chNull };
static XMLCh szTestGroup[]={ chLatin_t, chLatin_e, chLatin_s, chLatin_t, chLatin_G, chLatin_r, chLatin_o, chLatin_u, chLatin_p, chNull };
static XMLCh szSchemaTest[]={ chLatin_s, chLatin_c, chLatin_h, chLatin_e, chLatin_m, chLatin_a, chLatin_T, chLatin_e, chLatin_s, chLatin_t, chNull };
static XMLCh szInstanceTest[]={ chLatin_i, chLatin_n, chLatin_s, chLatin_t, chLatin_a, chLatin_n, chLatin_c, chLatin_e, chLatin_T, chLatin_e, chLatin_s, chLatin_t, chNull };
static XMLCh szDocumentationReference[]={ chLatin_d, chLatin_o, chLatin_c, chLatin_u, chLatin_m, chLatin_e, chLatin_n, chLatin_t, chLatin_a, chLatin_t, chLatin_i, chLatin_o, chLatin_n, 
                                          chLatin_R, chLatin_e, chLatin_f, chLatin_e, chLatin_r, chLatin_e, chLatin_n, chLatin_c, chLatin_e, chNull };
static XMLCh szSchemaDocument[]={ chLatin_s, chLatin_c, chLatin_h, chLatin_e, chLatin_m, chLatin_a, chLatin_D, chLatin_o, chLatin_c, chLatin_u, chLatin_m, chLatin_e, chLatin_n, chLatin_t, chNull };
static XMLCh szInstanceDocument[]={ chLatin_i, chLatin_n, chLatin_s, chLatin_t, chLatin_a, chLatin_n, chLatin_c, chLatin_e, chLatin_D, chLatin_o, chLatin_c, chLatin_u, chLatin_m, chLatin_e, chLatin_n, chLatin_t, chNull };
static XMLCh szExpected[]={ chLatin_e, chLatin_x, chLatin_p, chLatin_e, chLatin_c, chLatin_t, chLatin_e, chLatin_d, chNull };
static XMLCh szValidity[]={ chLatin_v, chLatin_a, chLatin_l, chLatin_i, chLatin_d, chLatin_i, chLatin_t, chLatin_y, chNull };

static XMLCh szXLINK[]={ chLatin_h, chLatin_t, chLatin_t, chLatin_p, chColon, chForwardSlash, chForwardSlash, 
                         chLatin_w, chLatin_w, chLatin_w, chPeriod, chLatin_w, chDigit_3, chPeriod, chLatin_o, chLatin_r, chLatin_g, chForwardSlash,
                         chDigit_1, chDigit_9, chDigit_9, chDigit_9, chForwardSlash, 
                         chLatin_x, chLatin_l, chLatin_i, chLatin_n, chLatin_k, chNull };
static XMLCh szHREF[]={ chLatin_h, chLatin_r, chLatin_e, chLatin_f, chNull };
static XMLCh szNAME[]={ chLatin_n, chLatin_a, chLatin_m, chLatin_e, chNull };
static XMLCh szVALID[]={ chLatin_v, chLatin_a, chLatin_l, chLatin_i, chLatin_d, chNull };
static XMLCh szINVALID[]={ chLatin_i, chLatin_n, chLatin_v, chLatin_a, chLatin_l, chLatin_i, chLatin_d, chNull };


static XMLCh szTestSuite2[]={ chLatin_h, chLatin_t, chLatin_t, chLatin_p, chColon, chForwardSlash, chForwardSlash, 
                        chLatin_w, chLatin_w, chLatin_w, chPeriod, chLatin_w, chDigit_3, chPeriod, chLatin_o, chLatin_r, chLatin_g, chForwardSlash,
                        chLatin_X, chLatin_M, chLatin_L, chForwardSlash, 
                        chDigit_2, chDigit_0, chDigit_0, chDigit_4, chForwardSlash,
                        chLatin_x, chLatin_m, chLatin_l, chDash, chLatin_s, chLatin_c, chLatin_h, chLatin_e, chLatin_m, chLatin_a, chDash, 
                        chLatin_t, chLatin_e, chLatin_s, chLatin_t, chDash, chLatin_s, chLatin_u, chLatin_i, chLatin_t, chLatin_e, chForwardSlash, chNull };

static XMLCh szTestSetRef[]={ chLatin_t, chLatin_e, chLatin_s, chLatin_t, chLatin_S, chLatin_e, chLatin_t, chLatin_R, chLatin_e, chLatin_f, chNull };

static XMLCh dummy[]={ chLatin_f, chLatin_i, chLatin_l, chLatin_e, chColon, chForwardSlash, chForwardSlash, 
                       chLatin_d, chLatin_u, chLatin_m, chLatin_m, chLatin_y, chForwardSlash, chNull };

// ---------------------------------------------------------------------------
//  XSTSHarnessHandlers: Implementation of the SAX DocumentHandler interface
// ---------------------------------------------------------------------------
void XSTSHarnessHandlers::startElement(const XMLCh* const uri
                                   , const XMLCh* const localname
                                   , const XMLCh* const /* qname */
                                   , const Attributes& attrs)
{
    if(XMLString::equals(uri, szTestSuite) || XMLString::equals(uri, szTestSuite2))
    {
        if(XMLString::equals(localname, szTestSetRef))
        {
            XMLURL testSet, backupBase(fBaseURL);
            testSet.setURL(fBaseURL, attrs.getValue(szXLINK, szHREF));

            fBaseURL=testSet;
            SAX2XMLReader* parser = XMLReaderFactory::createXMLReader();
            try
            {
                parser->setFeature(XMLUni::fgSAX2CoreNameSpaces, true);
                parser->setContentHandler(this);
                parser->setErrorHandler(this);
                parser->parse(testSet.getURLText());
            }
            catch (...)
            {
            }
            delete parser;
            fBaseURL=backupBase;

        }
        else if(XMLString::equals(localname, szTestGroup))
        {
            fCurrentTest.fExpectedResult=unknown;
            fCurrentTest.fSpecReference.setURL(urlW3C);
            fCurrentTest.fTestName[0]=0;
            fCurrentTest.fXMLName.setURL(dummy);
            fCurrentTest.fXSDNames.removeAllElements();
            StrX x(attrs.getValue(szNAME));
            const char* groupName=x.localForm();
            if(XMLString::equals(groupName,"isDefault072") ||     // this fails because of an access violation
               XMLString::equals(groupName,"addB194") ||
               XMLString::equals(groupName,"particlesZ033_c") ||
               XMLString::equals(groupName,"particlesZ033_d") ||
               XMLString::equals(groupName,"particlesZ033_e") ||
               XMLString::equals(groupName,"particlesZ033_f") ||
               XMLString::equals(groupName,"particlesZ033_g") ||
               XMLString::equals(groupName,"particlesZ035_a") ||
               XMLString::equals(groupName,"particlesZ036_b1") ||
               XMLString::equals(groupName,"particlesZ036_b2") ||
               XMLString::equals(groupName,"particlesZ036_c") ||
               XMLString::equals(groupName,"wildG032") )
                fCurrentTest.fSkipped=true;
            else
                fCurrentTest.fSkipped=false;
            fParser->resetCachedGrammarPool();
        }
        else if(XMLString::equals(localname, szDocumentationReference))
        {
            const XMLCh* ref=attrs.getValue(szXLINK, szHREF);
            if(ref && *ref)
                fCurrentTest.fSpecReference.setURL(ref);
            else
                fCurrentTest.fSpecReference.setURL(dummy);
        }
        else if(XMLString::equals(localname, szSchemaTest) ||
                XMLString::equals(localname, szInstanceTest))
        {
            XMLString::copyString(fCurrentTest.fTestName, attrs.getValue(szNAME));
        }
        else if(XMLString::equals(localname, szSchemaDocument))
        {
            fCurrentTest.fXSDNames.addElement(new XMLURL(fBaseURL, attrs.getValue(szXLINK, szHREF)));
        }
        else if(XMLString::equals(localname, szInstanceDocument))
        {
            fCurrentTest.fXMLName.setURL(fBaseURL, attrs.getValue(szXLINK, szHREF));
        }
        else if(XMLString::equals(localname, szExpected))
        {
            const XMLCh* validity=attrs.getValue(szValidity);
            if(XMLString::equals(validity, szVALID))
                fCurrentTest.fExpectedResult=valid;
            else if(XMLString::equals(validity, szINVALID))
                fCurrentTest.fExpectedResult=invalid;
            else
                fCurrentTest.fExpectedResult=unknown;
        }
    }
}

void XSTSHarnessHandlers::endElement(const XMLCh* const uri,
	                                 const XMLCh* const localname,
	                                 const XMLCh* const /*qname*/)
{
    if(XMLString::equals(uri, szTestSuite) || XMLString::equals(uri, szTestSuite2))
    {
        if(XMLString::equals(localname, szSchemaTest))
        {
            if(fCurrentTest.fSkipped)
            {
                fTests++;
                fFailures++;
                XERCES_STD_QUALIFIER cout << "Test " << StrX(fCurrentTest.fTestName) << " skipped" << XERCES_STD_QUALIFIER endl;
                return;
            }
            bool success=true, fatalFailure=false;
            try
            {
                fErrorHandler.resetErrors();
                for(unsigned int i=0;i<fCurrentTest.fXSDNames.size();i++)
                {
                    Grammar* grammar=fParser->loadGrammar(fCurrentTest.fXSDNames.elementAt(i)->getURLText(), Grammar::SchemaGrammarType, true);
                    success=(success && (grammar!=NULL));
                }
            }
            catch (const OutOfMemoryException&)
            {
                fatalFailure=true;
                XERCES_STD_QUALIFIER cout << "Test " << StrX(fCurrentTest.fTestName) << " ran out of memory" << XERCES_STD_QUALIFIER endl;
                success=false;
            }
            catch(const XMLException& exc)
            {
                XERCES_STD_QUALIFIER cout << "Test " << StrX(fCurrentTest.fTestName) << " threw " << StrX(exc.getMessage()) << XERCES_STD_QUALIFIER endl;
                success=false;
            }
            catch (...)
            {
                fatalFailure=true;
                XERCES_STD_QUALIFIER cout << "Test " << StrX(fCurrentTest.fTestName) << " crashed" << XERCES_STD_QUALIFIER endl;
                success=false;
                exit(1);
            }
            fTests++;
            if(fatalFailure)
            {
                // skip the rest of the group, as we had problems with the schema itself
                fCurrentTest.fSkipped=true;
                fFailures++;
                for(unsigned int i=0;i<fCurrentTest.fXSDNames.size();i++)
                    printFile(*fCurrentTest.fXSDNames.elementAt(i));
            }
            else
            {
                if(success && !fErrorHandler.getSawErrors())
                {
                    if(fCurrentTest.fExpectedResult!=valid)
                    {
                        // skip the rest of the group, as we had problems with the schema itself
                        fCurrentTest.fSkipped=true;
                        fFailures++;
                        XERCES_STD_QUALIFIER cout << "Test " << StrX(fCurrentTest.fTestName) << " succeeded but was expected to fail" << XERCES_STD_QUALIFIER endl;
                        for(unsigned int i=0;i<fCurrentTest.fXSDNames.size();i++)
                            printFile(*fCurrentTest.fXSDNames.elementAt(i));
                    }
                }
                else
                {
                    if(fCurrentTest.fExpectedResult!=invalid)
                    {
                        // skip the rest of the group, as we had problems with the schema itself
                        fCurrentTest.fSkipped=true;
                        fFailures++;
                        XERCES_STD_QUALIFIER cout << "Test " << StrX(fCurrentTest.fTestName) << " failed but was expected to pass" << XERCES_STD_QUALIFIER endl;
                        XERCES_STD_QUALIFIER cout << "Reported error: " << StrX(fErrorHandler.getErrorText()) << XERCES_STD_QUALIFIER endl;
                        for(unsigned int i=0;i<fCurrentTest.fXSDNames.size();i++)
                            printFile(*fCurrentTest.fXSDNames.elementAt(i));
                    }
                }
            }
        }
        else if(XMLString::equals(localname, szInstanceTest))
        {
            if(fCurrentTest.fSkipped)
            {
                fTests++;
                fFailures++;
                XERCES_STD_QUALIFIER cout << "Test " << StrX(fCurrentTest.fTestName) << " skipped" << XERCES_STD_QUALIFIER endl;
                return;
            }
            bool success=true, fatalFailure=false;
            try
            {
                fErrorHandler.resetErrors();
                fParser->parse(fCurrentTest.fXMLName.getURLText());
            }
            catch (const OutOfMemoryException&)
            {
                fatalFailure=true;
                XERCES_STD_QUALIFIER cout << "Test " << StrX(fCurrentTest.fTestName) << " ran out of memory" << XERCES_STD_QUALIFIER endl;
                success=false;
            }
            catch(const XMLException& exc)
            {
                XERCES_STD_QUALIFIER cout << "Test " << StrX(fCurrentTest.fTestName) << " threw " << StrX(exc.getMessage()) << XERCES_STD_QUALIFIER endl;
                success=false;
            }
            catch (...)
            {
                fatalFailure=true;
                XERCES_STD_QUALIFIER cout << "Test " << StrX(fCurrentTest.fTestName) << " crashed" << XERCES_STD_QUALIFIER endl;
                success=false;
                exit(1);
            }
            fTests++;
            if(fatalFailure)
            {
                fFailures++;
                for(unsigned int i=0;i<fCurrentTest.fXSDNames.size();i++)
                    printFile(*fCurrentTest.fXSDNames.elementAt(i));
                printFile(fCurrentTest.fXMLName);
            }
            else
            {
                if(success && !fErrorHandler.getSawErrors())
                {
                    if(fCurrentTest.fExpectedResult!=valid)
                    {
                        fFailures++;
                        XERCES_STD_QUALIFIER cout << "Test " << StrX(fCurrentTest.fTestName) << " succeeded but was expected to fail" << XERCES_STD_QUALIFIER endl;
                        for(unsigned int i=0;i<fCurrentTest.fXSDNames.size();i++)
                            printFile(*fCurrentTest.fXSDNames.elementAt(i));
                        printFile(fCurrentTest.fXMLName);
                    }
                }
                else
                {
                    if(fCurrentTest.fExpectedResult!=invalid)
                    {
                        fFailures++;
                        XERCES_STD_QUALIFIER cout << "Test " << StrX(fCurrentTest.fTestName) << " failed but was expected to pass" << XERCES_STD_QUALIFIER endl;
                        XERCES_STD_QUALIFIER cout << "Reported error: " << StrX(fErrorHandler.getErrorText()) << XERCES_STD_QUALIFIER endl;
                        for(unsigned int i=0;i<fCurrentTest.fXSDNames.size();i++)
                            printFile(*fCurrentTest.fXSDNames.elementAt(i));
                        printFile(fCurrentTest.fXMLName);
                    }
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
//  XSTSHarnessHandlers: Overrides of the SAX ErrorHandler interface
// ---------------------------------------------------------------------------
void XSTSHarnessHandlers::error(const SAXParseException& e)
{
    fSawErrors = true;
    XERCES_STD_QUALIFIER cout << "\nError at file " << StrX(e.getSystemId())
		 << ", line " << e.getLineNumber()
		 << ", char " << e.getColumnNumber()
         << "\n  Message: " << StrX(e.getMessage()) << XERCES_STD_QUALIFIER endl;
}

void XSTSHarnessHandlers::fatalError(const SAXParseException& e)
{
    fSawErrors = true;
    XERCES_STD_QUALIFIER cout << "\nFatal Error at file " << StrX(e.getSystemId())
		 << ", line " << e.getLineNumber()
		 << ", char " << e.getColumnNumber()
         << "\n  Message: " << StrX(e.getMessage()) << XERCES_STD_QUALIFIER endl;
}

void XSTSHarnessHandlers::warning(const SAXParseException& e)
{
    XERCES_STD_QUALIFIER cout << "\nWarning at file " << StrX(e.getSystemId())
		 << ", line " << e.getLineNumber()
		 << ", char " << e.getColumnNumber()
         << "\n  Message: " << StrX(e.getMessage()) << XERCES_STD_QUALIFIER endl;
}

void XSTSHarnessHandlers::resetErrors()
{
    fSawErrors = false;
}

// ---------------------------------------------------------------------------
//  XSTSHarnessHandlers: Helpers
// ---------------------------------------------------------------------------
void XSTSHarnessHandlers::printFile(XMLURL& url)
{
    if(XMLString::equals(url.getURLText(), dummy))
        return;
    BinInputStream* stream=url.makeNewStream();
    if(stream==NULL)
    {
        XERCES_STD_QUALIFIER cout << "File " << StrX(url.getURLText()) << " is missing" << XERCES_STD_QUALIFIER endl;
        return;
    }
    XERCES_STD_QUALIFIER cout << "Content of file " << StrX(url.getURLText()) << XERCES_STD_QUALIFIER endl;
    XMLByte buffer[256];
    XMLSize_t nRead;
    while((nRead=stream->readBytes(buffer, 255)) >0)
    {
        buffer[nRead]=0;
        // sending data containing \n\r to cout generates \n\n\r, so strip any \r
        XMLSize_t idx=0;
        while(true)
        {
            int cr=XMLString::indexOf((const char*)buffer, '\r', idx);
            if(cr==-1)
                break;
            memmove(&buffer[cr], &buffer[cr+1], XMLString::stringLen((const char*)&buffer[cr+1])+1);
            idx=cr;
            if(buffer[idx]==0)
                break;
        }
        XERCES_STD_QUALIFIER cout << (const char*)buffer;
    }
    XERCES_STD_QUALIFIER cout << XERCES_STD_QUALIFIER endl;
    delete stream;
}

void XSTSErrorHandler::error(const SAXParseException& exc)
{ 
    fSawErrors=true; 
    fErrorText.append(exc.getMessage()); 
    fErrorText.append(chLF); 
}

void XSTSErrorHandler::fatalError(const SAXParseException& exc)
{ 
    fSawErrors=true; 
    fErrorText.append(exc.getMessage()); 
    fErrorText.append(chLF); 
}

