
#include <../src/vec/is/ao/aoimpl.h>

static PetscBool AOPackageInitialized = PETSC_FALSE;

/*@C
  AOFinalizePackage - This function finalizes everything in the AO package. It is called
  from PetscFinalize().

  Level: developer

.keywords: AO, initialize, package
.seealso: PetscInitialize()
@*/
PetscErrorCode  AOFinalizePackage(void)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  ierr = PetscFunctionListDestroy(&AOList);CHKERRQ(ierr);
  AOPackageInitialized = PETSC_FALSE;
  AORegisterAllCalled = PETSC_FALSE;
  PetscFunctionReturn(0);
}

/*@C
  AOInitializePackage - This function initializes everything in the AO package. It is called
  from PetscDLLibraryRegister() when using dynamic libraries, and on the first call to AOCreate().

  Level: developer

.keywords: AO, initialize, package
.seealso: PetscInitialize()
@*/
PetscErrorCode  AOInitializePackage(void)
{
  char           logList[256];
  PetscBool      opt,pkg;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  if (AOPackageInitialized) PetscFunctionReturn(0);
  AOPackageInitialized = PETSC_TRUE;
  /* Register Classes */
  ierr = PetscClassIdRegister("Application Order",&AO_CLASSID);CHKERRQ(ierr);
  /* Register Constructors */
  ierr = AORegisterAll();CHKERRQ(ierr);
  /* Register Events */
  ierr = PetscLogEventRegister("AOPetscToApplication", AO_CLASSID,&AO_PetscToApplication);CHKERRQ(ierr);
  ierr = PetscLogEventRegister("AOApplicationToPetsc", AO_CLASSID,&AO_ApplicationToPetsc);CHKERRQ(ierr);
  /* Process info exclusions */
  ierr = PetscOptionsGetString(NULL,NULL,"-info_exclude",logList,sizeof(logList),&opt);CHKERRQ(ierr);
  if (opt) {
    ierr = PetscStrInList("ao",logList,',',&pkg);CHKERRQ(ierr);
    if (pkg) {ierr = PetscInfoDeactivateClass(AO_CLASSID);CHKERRQ(ierr);}
  }
  /* Process summary exclusions */
  ierr = PetscOptionsGetString(NULL,NULL,"-log_exclude",logList,sizeof(logList),&opt);CHKERRQ(ierr);
  if (opt) {
    ierr = PetscStrInList("ao",logList,',',&pkg);CHKERRQ(ierr);
    if (pkg) {ierr = PetscLogEventExcludeClass(AO_CLASSID);CHKERRQ(ierr);}
  }
  /* Register package finalizer */
  ierr = PetscRegisterFinalize(AOFinalizePackage);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}
