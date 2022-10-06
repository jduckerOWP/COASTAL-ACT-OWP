#ifndef NEFIS_VERSION
#define NEFIS_VERSION

#define nefis_major "5"
#define nefis_minor "09"
#define nefis_revision "00"
#define nefis_build "141013M"

#define nefis_company "Deltares"
#define nefis_company_url  = "http://www.deltares.nl"
#define nefis_program "NEFIS"

/*=================================================== DO NOT MAKE CHANGES BELOW THIS LINE ===================================================================== */


extern "C" {
    extern char * getfullversionstring_nefis(void);
    extern char * getfileversionstring_nefis(void);
    extern char * getcompanystring_nefis(void);
}

#endif /* NEFIS_VERSION */
