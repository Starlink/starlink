/*=========================================================================

  Program:   CMake - Cross-Platform Makefile Generator
  Module:    $RCSfile: PathDialog.h,v $
  Language:  C++
  Date:      $Date: 2002/10/23 22:03:27 $
  Version:   $Revision: 1.4 $

  Copyright (c) 2002 Kitware, Inc., Insight Consortium.  All rights reserved.
  See Copyright.txt or http://www.cmake.org/HTML/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
//////////////////////////////////////////////////////////////////////////
//PathDialog.h file
//
//Written by Nguyen Tan Hung <tanhung@yahoo.com>
//////////////////////////////////////////////////////////////////////////

#if !defined(AFX_PATHDIALOG_H__0F70BC86_11DB_11D4_B012_0000E8DD8DAA__INCLUDED_)
#define AFX_PATHDIALOG_H__0F70BC86_11DB_11D4_B012_0000E8DD8DAA__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// PathDialog.h : header file
//
#include "shlobj.h"

class CPathDialog;

// CPathDialogSub - intercepts messages from child controls
class CPathDialogSub : public CWnd
{
        friend CPathDialog;
public:
        CPathDialog* m_pPathDialog;
protected:
    afx_msg void OnOK();              // OK button clicked
        afx_msg void OnChangeEditPath();
    DECLARE_MESSAGE_MAP()
private:
};

/////////////////////////////////////////////////////////////////////////////
// CPathDialog dialog

class CPathDialog
{
        friend CPathDialogSub;
// Construction
public:
        CPathDialog(LPCTSTR lpszCaption=NULL,
                LPCTSTR lpszTitle=NULL,
                LPCTSTR lpszInitialPath=NULL, 
                CWnd* pParent = NULL);

        CString GetPathName();
        virtual int DoModal();

        static int Touch(LPCTSTR lpPath, BOOL bValidate=TRUE);
        static int MakeSurePathExists(LPCTSTR lpPath);
        static BOOL IsFileNameValid(LPCTSTR lpFileName);
        static int ConcatPath(LPTSTR lpRoot, LPCTSTR lpMorePath);

private:
        static int CALLBACK BrowseCallbackProc(HWND hwnd,UINT uMsg,LPARAM lParam, LPARAM pData);

        LPCTSTR m_lpszCaption;
        LPCTSTR m_lpszInitialPath;

        TCHAR m_szPathName[MAX_PATH];

        BROWSEINFO m_bi;
        HWND m_hWnd;
        CWnd*   m_pParentWnd;
        BOOL m_bParentDisabled;
        BOOL m_bGetSuccess;

        CPathDialogSub m_PathDialogSub;

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PATHDIALOG_H__0F70BC86_11DB_11D4_B012_0000E8DD8DAA__INCLUDED_)
