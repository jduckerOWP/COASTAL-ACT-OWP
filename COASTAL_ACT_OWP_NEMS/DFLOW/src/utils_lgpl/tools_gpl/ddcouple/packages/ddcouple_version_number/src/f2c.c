#include <string.h>
#if defined(WIN32) || defined (WIN64)
#  include <io.h>
#  include <wtypes.h>
#elif defined (salford32)
#  include <io.h>
#  include <windows.h>
#endif

#if HAVE_CONFIG_H
#   include "config.h"
#endif
#include "version_number.h"
#define CAT(a, b) a ## b
#define FUNC_CAT(a, b) CAT(a, b)

#ifndef max
#   define max(a,b) (((a) > (b)) ? (a) : (b))
#   define min(a,b) (((a) < (b)) ? (a) : (b))
#endif

extern  char * FUNC_CAT( version_getFileVersionString_, MOD_NAME)(void);
extern  char * FUNC_CAT( version_getFullVersionString_, MOD_NAME)(void);
extern  char * FUNC_CAT( version_getCompanyString_, MOD_NAME)(void);
extern  char * FUNC_CAT( version_getVersionNumberString_, MOD_NAME)(void);
extern  char * FUNC_CAT( version_getProgramNameString_, MOD_NAME)(void);
extern  char * FUNC_CAT( version_getShortProgramNameString_, MOD_NAME)(void);
extern  char * FUNC_CAT( version_getSvnRevisionString_, MOD_NAME)(void);
extern  char * FUNC_CAT( version_getFeatureNumberString_, MOD_NAME)(void);


/*
 * FTN_CAPITAL   : dvf6, salford
 * FTN_UNDERSCORE: sgi, sun, cygwin, linux
 * FTN_SMALL     : hp
 */

#if HAVE_CONFIG_H
#   include "config.h"
#   define FTN_CALL  /* nothing */
#   define F90_GETFILEVERSIONSTRING      FUNC_CAT( FC_FUNC(getfileversionstring,GETFILEVERSIONSTRING), F90_MOD_NAME)
#   define F90_GETFULLVERSIONSTRING      FUNC_CAT( FC_FUNC(getfullversionstring,GETFULLVERSIONSTRING), F90_MOD_NAME)
#   define F90_GETCOMPANYSTRING          FUNC_CAT( FC_FUNC(getcompanystring,GETCOMPANYSTRING), F90_MOD_NAME)
#   define F90_GETVERSIONNUMBERSTRING    FUNC_CAT( FC_FUNC(getversionnumberstring,GETVERSIONNUMBERSTRING), F90_MOD_NAME)
#   define F90_GETPROGRAMNAMESTRING      FUNC_CAT( FC_FUNC(getprogramnamestring,GETPROGRAMNAMESTRING), F90_MOD_NAME)
#   define F90_GETSHORTPROGRAMNAMESTRING FUNC_CAT( FC_FUNC(getshortprogramnamestring,GETSHORTPROGRAMNAMESTRING), F90_MOD_NAME)
#   define F90_GETSVNREVISIONSTRING      FUNC_CAT( FC_FUNC(getsvnrevisionstring,GETSVNREVISIONSTRING), F90_MOD_NAME)
#   define F90_GETFEATURENUMBERSTRING    FUNC_CAT( FC_FUNC(getfeaturenumberstring,GETFEATURENUMBERSTRING), F90_MOD_NAME)
#else
// WIN32 or WIN64
#   define FTN_CALL  __cdecl
#   define F90_GETFILEVERSIONSTRING      FUNC_CAT( GETFILEVERSIONSTRING_, MOD_NAME)
#   define F90_GETFULLVERSIONSTRING      FUNC_CAT( GETFULLVERSIONSTRING_, MOD_NAME)
#   define F90_GETCOMPANYSTRING          FUNC_CAT( GETCOMPANYSTRING_, MOD_NAME)
#   define F90_GETVERSIONNUMBERSTRING    FUNC_CAT( GETVERSIONNUMBERSTRING_, MOD_NAME)
#   define F90_GETPROGRAMNAMESTRING      FUNC_CAT( GETPROGRAMNAMESTRING_, MOD_NAME)
#   define F90_GETSHORTPROGRAMNAMESTRING FUNC_CAT( GETSHORTPROGRAMNAMESTRING_, MOD_NAME)
#   define F90_GETSVNREVISIONSTRING      FUNC_CAT( GETSVNREVISIONSTRING_, MOD_NAME)
#   define F90_GETFEATURENUMBERSTRING    FUNC_CAT( GETFEATURENUMBERSTRING_, MOD_NAME)
#endif

/*==========================================================================*/
 void FTN_CALL F90_GETFILEVERSIONSTRING( char * str, int length_str )
{
  char * string;
  int i;
  for (i=0; i<length_str; i++) {str[i] = ' ';}
  string = FUNC_CAT( version_getFileVersionString_, MOD_NAME)();
  i  = min((int) length_str, (int) strlen(string));
  strncpy(str, string, i);
}

/*==========================================================================*/
 void FTN_CALL F90_GETFULLVERSIONSTRING( char * str, int length_str )
{
   char * string;
  int i;
  for (i=0; i<length_str; i++) {str[i] = ' ';}
  string = FUNC_CAT( version_getFullVersionString_, MOD_NAME)();
  i  = min((int) length_str, (int) strlen(string));
  strncpy(str, string, i);
}

/*==========================================================================*/
 void FTN_CALL F90_GETCOMPANYSTRING( char * str, int length_str )
{
  char * string;
  int i;
  for (i=0; i<length_str; i++) {str[i] = ' ';}
  string = FUNC_CAT( version_getCompanyString_, MOD_NAME)();
  i  = min(length_str, (int) strlen(string));
  strncpy(str, string, i);
}

/*==========================================================================*/
 void FTN_CALL F90_GETVERSIONNUMBERSTRING( char * str, int length_str )
{
  char * string;
  int i;
  for (i=0; i<length_str; i++) {str[i] = ' ';}
  string = FUNC_CAT( version_getVersionNumberString_, MOD_NAME)();
  i  = min(length_str, (int) strlen(string));
  strncpy(str, string, i);
}

/*==========================================================================*/
 void FTN_CALL F90_GETPROGRAMNAMESTRING( char * str, int length_str )
{
  char * string;
  int i;
  for (i=0; i<length_str; i++) {str[i] = ' ';}
  string = FUNC_CAT( version_getProgramNameString_, MOD_NAME)();
  i  = min(length_str, (int) strlen(string));
  strncpy(str, string, i);
}

/*==========================================================================*/
 void FTN_CALL F90_GETSHORTPROGRAMNAMESTRING( char * str, int length_str )
{
  char * string;
  int i;
  for (i=0; i<length_str; i++) {str[i] = ' ';}
  string = FUNC_CAT( version_getShortProgramNameString_, MOD_NAME)();
  i  = min(length_str, (int) strlen(string));
  strncpy(str, string, i);
}

/*==========================================================================*/
 void FTN_CALL F90_GETSVNREVISIONSTRING( char * str, int length_str )
{
  char * string;
  int i;
  for (i=0; i<length_str; i++) {str[i] = ' ';}
  string = FUNC_CAT( version_getSvnRevisionString_, MOD_NAME)();
  i  = min(length_str, (int) strlen(string));
  strncpy(str, string, i);
}
/*==========================================================================*/
 void FTN_CALL F90_GETFEATURENUMBERSTRING( char * str, int length_str )
{
  char * string;
  int i;
  for (i=0; i<length_str; i++) {str[i] = ' ';}
  string = FUNC_CAT( version_getFeatureNumberString_, MOD_NAME)();
  i  = min(length_str, (int) strlen(string));
  strncpy(str, string, i);
}
