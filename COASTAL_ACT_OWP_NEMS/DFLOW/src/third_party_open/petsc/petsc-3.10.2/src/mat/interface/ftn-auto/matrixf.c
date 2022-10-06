#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* matrix.c */
/* Fortran interface file */

/*
* This file was generated automatically by bfort from the C source
* file.  
 */

#ifdef PETSC_USE_POINTER_CONVERSION
#if defined(__cplusplus)
extern "C" { 
#endif 
extern void *PetscToPointer(void*);
extern int PetscFromPointer(void *);
extern void PetscRmPointer(void*);
#if defined(__cplusplus)
} 
#endif 

#else

#define PetscToPointer(a) (*(PetscFortranAddr *)(a))
#define PetscFromPointer(a) (PetscFortranAddr)(a)
#define PetscRmPointer(a)
#endif

#include "petscmat.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsetrandom_ MATSETRANDOM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsetrandom_ matsetrandom
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matfactorgeterrorzeropivot_ MATFACTORGETERRORZEROPIVOT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matfactorgeterrorzeropivot_ matfactorgeterrorzeropivot
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matfactorgeterror_ MATFACTORGETERROR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matfactorgeterror_ matfactorgeterror
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matfactorclearerror_ MATFACTORCLEARERROR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matfactorclearerror_ matfactorclearerror
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matfindnonzerorows_ MATFINDNONZEROROWS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matfindnonzerorows_ matfindnonzerorows
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matfindzerorows_ MATFINDZEROROWS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matfindzerorows_ matfindzerorows
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetdiagonalblock_ MATGETDIAGONALBLOCK
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetdiagonalblock_ matgetdiagonalblock
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgettrace_ MATGETTRACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgettrace_ matgettrace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matrealpart_ MATREALPART
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matrealpart_ matrealpart
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matimaginarypart_ MATIMAGINARYPART
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matimaginarypart_ matimaginarypart
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmissingdiagonal_ MATMISSINGDIAGONAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmissingdiagonal_ matmissingdiagonal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matconjugate_ MATCONJUGATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matconjugate_ matconjugate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetrowuppertriangular_ MATGETROWUPPERTRIANGULAR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetrowuppertriangular_ matgetrowuppertriangular
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matrestorerowuppertriangular_ MATRESTOREROWUPPERTRIANGULAR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matrestorerowuppertriangular_ matrestorerowuppertriangular
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matresetpreallocation_ MATRESETPREALLOCATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matresetpreallocation_ matresetpreallocation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsetup_ MATSETUP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsetup_ matsetup
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdestroy_ MATDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdestroy_ matdestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsetvaluesrowlocal_ MATSETVALUESROWLOCAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsetvaluesrowlocal_ matsetvaluesrowlocal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsetvaluesrow_ MATSETVALUESROW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsetvaluesrow_ matsetvaluesrow
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsetvaluesstencil_ MATSETVALUESSTENCIL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsetvaluesstencil_ matsetvaluesstencil
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsetvaluesblockedstencil_ MATSETVALUESBLOCKEDSTENCIL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsetvaluesblockedstencil_ matsetvaluesblockedstencil
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsetstencil_ MATSETSTENCIL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsetstencil_ matsetstencil
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetvalues_ MATGETVALUES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetvalues_ matgetvalues
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsetvaluesbatch_ MATSETVALUESBATCH
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsetvaluesbatch_ matsetvaluesbatch
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsetlocaltoglobalmapping_ MATSETLOCALTOGLOBALMAPPING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsetlocaltoglobalmapping_ matsetlocaltoglobalmapping
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetlocaltoglobalmapping_ MATGETLOCALTOGLOBALMAPPING
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetlocaltoglobalmapping_ matgetlocaltoglobalmapping
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetlayouts_ MATGETLAYOUTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetlayouts_ matgetlayouts
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmultdiagonalblock_ MATMULTDIAGONALBLOCK
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmultdiagonalblock_ matmultdiagonalblock
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmult_ MATMULT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmult_ matmult
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmulttranspose_ MATMULTTRANSPOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmulttranspose_ matmulttranspose
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmulthermitiantranspose_ MATMULTHERMITIANTRANSPOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmulthermitiantranspose_ matmulthermitiantranspose
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmultadd_ MATMULTADD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmultadd_ matmultadd
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmulttransposeadd_ MATMULTTRANSPOSEADD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmulttransposeadd_ matmulttransposeadd
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmulthermitiantransposeadd_ MATMULTHERMITIANTRANSPOSEADD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmulthermitiantransposeadd_ matmulthermitiantransposeadd
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmultconstrained_ MATMULTCONSTRAINED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmultconstrained_ matmultconstrained
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmulttransposeconstrained_ MATMULTTRANSPOSECONSTRAINED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmulttransposeconstrained_ matmulttransposeconstrained
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsolve_ MATSOLVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsolve_ matsolve
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmatsolve_ MATMATSOLVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmatsolve_ matmatsolve
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmatsolvetranspose_ MATMATSOLVETRANSPOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmatsolvetranspose_ matmatsolvetranspose
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmattransposesolve_ MATMATTRANSPOSESOLVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmattransposesolve_ matmattransposesolve
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matforwardsolve_ MATFORWARDSOLVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matforwardsolve_ matforwardsolve
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matbackwardsolve_ MATBACKWARDSOLVE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matbackwardsolve_ matbackwardsolve
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsolveadd_ MATSOLVEADD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsolveadd_ matsolveadd
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsolvetranspose_ MATSOLVETRANSPOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsolvetranspose_ matsolvetranspose
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsolvetransposeadd_ MATSOLVETRANSPOSEADD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsolvetransposeadd_ matsolvetransposeadd
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsor_ MATSOR
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsor_ matsor
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcopy_ MATCOPY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcopy_ matcopy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matduplicate_ MATDUPLICATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matduplicate_ matduplicate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetdiagonal_ MATGETDIAGONAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetdiagonal_ matgetdiagonal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetrowsum_ MATGETROWSUM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetrowsum_ matgetrowsum
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define mattranspose_ MATTRANSPOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define mattranspose_ mattranspose
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matistranspose_ MATISTRANSPOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matistranspose_ matistranspose
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define mathermitiantranspose_ MATHERMITIANTRANSPOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define mathermitiantranspose_ mathermitiantranspose
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matishermitiantranspose_ MATISHERMITIANTRANSPOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matishermitiantranspose_ matishermitiantranspose
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matpermute_ MATPERMUTE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matpermute_ matpermute
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matequal_ MATEQUAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matequal_ matequal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdiagonalscale_ MATDIAGONALSCALE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdiagonalscale_ matdiagonalscale
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matscale_ MATSCALE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matscale_ matscale
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matnorm_ MATNORM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matnorm_ matnorm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matassemblybegin_ MATASSEMBLYBEGIN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matassemblybegin_ matassemblybegin
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matassembled_ MATASSEMBLED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matassembled_ matassembled
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matassemblyend_ MATASSEMBLYEND
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matassemblyend_ matassemblyend
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsetoption_ MATSETOPTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsetoption_ matsetoption
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetoption_ MATGETOPTION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetoption_ matgetoption
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matzeroentries_ MATZEROENTRIES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matzeroentries_ matzeroentries
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matzerorowscolumns_ MATZEROROWSCOLUMNS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matzerorowscolumns_ matzerorowscolumns
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matzerorowscolumnsis_ MATZEROROWSCOLUMNSIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matzerorowscolumnsis_ matzerorowscolumnsis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matzerorows_ MATZEROROWS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matzerorows_ matzerorows
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matzerorowsis_ MATZEROROWSIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matzerorowsis_ matzerorowsis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matzerorowsstencil_ MATZEROROWSSTENCIL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matzerorowsstencil_ matzerorowsstencil
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matzerorowscolumnsstencil_ MATZEROROWSCOLUMNSSTENCIL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matzerorowscolumnsstencil_ matzerorowscolumnsstencil
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matzerorowslocalis_ MATZEROROWSLOCALIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matzerorowslocalis_ matzerorowslocalis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matzerorowscolumnslocal_ MATZEROROWSCOLUMNSLOCAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matzerorowscolumnslocal_ matzerorowscolumnslocal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matzerorowscolumnslocalis_ MATZEROROWSCOLUMNSLOCALIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matzerorowscolumnslocalis_ matzerorowscolumnslocalis
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matincreaseoverlap_ MATINCREASEOVERLAP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matincreaseoverlap_ matincreaseoverlap
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matincreaseoverlapsplit_ MATINCREASEOVERLAPSPLIT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matincreaseoverlapsplit_ matincreaseoverlapsplit
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetblocksize_ MATGETBLOCKSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetblocksize_ matgetblocksize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetblocksizes_ MATGETBLOCKSIZES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetblocksizes_ matgetblocksizes
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsetblocksize_ MATSETBLOCKSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsetblocksize_ matsetblocksize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsetvariableblocksizes_ MATSETVARIABLEBLOCKSIZES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsetvariableblocksizes_ matsetvariableblocksizes
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsetblocksizes_ MATSETBLOCKSIZES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsetblocksizes_ matsetblocksizes
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsetblocksizesfrommats_ MATSETBLOCKSIZESFROMMATS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsetblocksizesfrommats_ matsetblocksizesfrommats
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matresidual_ MATRESIDUAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matresidual_ matresidual
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsetunfactored_ MATSETUNFACTORED
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsetunfactored_ matsetunfactored
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreatesubmatrix_ MATCREATESUBMATRIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreatesubmatrix_ matcreatesubmatrix
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstashsetinitialsize_ MATSTASHSETINITIALSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstashsetinitialsize_ matstashsetinitialsize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matinterpolateadd_ MATINTERPOLATEADD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matinterpolateadd_ matinterpolateadd
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matinterpolate_ MATINTERPOLATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matinterpolate_ matinterpolate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matrestrict_ MATRESTRICT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matrestrict_ matrestrict
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetnullspace_ MATGETNULLSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetnullspace_ matgetnullspace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsetnullspace_ MATSETNULLSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsetnullspace_ matsetnullspace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgettransposenullspace_ MATGETTRANSPOSENULLSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgettransposenullspace_ matgettransposenullspace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsettransposenullspace_ MATSETTRANSPOSENULLSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsettransposenullspace_ matsettransposenullspace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsetnearnullspace_ MATSETNEARNULLSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsetnearnullspace_ matsetnearnullspace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetnearnullspace_ MATGETNEARNULLSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetnearnullspace_ matgetnearnullspace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matdiagonalscalelocal_ MATDIAGONALSCALELOCAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matdiagonalscalelocal_ matdiagonalscalelocal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetinertia_ MATGETINERTIA
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetinertia_ matgetinertia
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matissymmetric_ MATISSYMMETRIC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matissymmetric_ matissymmetric
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matishermitian_ MATISHERMITIAN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matishermitian_ matishermitian
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matissymmetricknown_ MATISSYMMETRICKNOWN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matissymmetricknown_ matissymmetricknown
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matishermitianknown_ MATISHERMITIANKNOWN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matishermitianknown_ matishermitianknown
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matisstructurallysymmetric_ MATISSTRUCTURALLYSYMMETRIC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matisstructurallysymmetric_ matisstructurallysymmetric
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matstashgetinfo_ MATSTASHGETINFO
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matstashgetinfo_ matstashgetinfo
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matfactorsetschuris_ MATFACTORSETSCHURIS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matfactorsetschuris_ matfactorsetschuris
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matfactorcreateschurcomplement_ MATFACTORCREATESCHURCOMPLEMENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matfactorcreateschurcomplement_ matfactorcreateschurcomplement
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matfactorgetschurcomplement_ MATFACTORGETSCHURCOMPLEMENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matfactorgetschurcomplement_ matfactorgetschurcomplement
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matfactorrestoreschurcomplement_ MATFACTORRESTORESCHURCOMPLEMENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matfactorrestoreschurcomplement_ matfactorrestoreschurcomplement
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matfactorsolveschurcomplementtranspose_ MATFACTORSOLVESCHURCOMPLEMENTTRANSPOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matfactorsolveschurcomplementtranspose_ matfactorsolveschurcomplementtranspose
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matfactorsolveschurcomplement_ MATFACTORSOLVESCHURCOMPLEMENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matfactorsolveschurcomplement_ matfactorsolveschurcomplement
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matfactorinvertschurcomplement_ MATFACTORINVERTSCHURCOMPLEMENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matfactorinvertschurcomplement_ matfactorinvertschurcomplement
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matfactorfactorizeschurcomplement_ MATFACTORFACTORIZESCHURCOMPLEMENT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matfactorfactorizeschurcomplement_ matfactorfactorizeschurcomplement
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matptap_ MATPTAP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matptap_ matptap
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matptapnumeric_ MATPTAPNUMERIC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matptapnumeric_ matptapnumeric
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matptapsymbolic_ MATPTAPSYMBOLIC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matptapsymbolic_ matptapsymbolic
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matrart_ MATRART
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matrart_ matrart
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matrartnumeric_ MATRARTNUMERIC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matrartnumeric_ matrartnumeric
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matrartsymbolic_ MATRARTSYMBOLIC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matrartsymbolic_ matrartsymbolic
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmatmult_ MATMATMULT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmatmult_ matmatmult
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmatmultsymbolic_ MATMATMULTSYMBOLIC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmatmultsymbolic_ matmatmultsymbolic
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmatmultnumeric_ MATMATMULTNUMERIC
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmatmultnumeric_ matmatmultnumeric
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmattransposemult_ MATMATTRANSPOSEMULT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmattransposemult_ matmattransposemult
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define mattransposematmult_ MATTRANSPOSEMATMULT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define mattransposematmult_ mattransposematmult
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matmatmatmult_ MATMATMATMULT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matmatmatmult_ matmatmatmult
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreateredundantmatrix_ MATCREATEREDUNDANTMATRIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreateredundantmatrix_ matcreateredundantmatrix
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetlocalsubmatrix_ MATGETLOCALSUBMATRIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetlocalsubmatrix_ matgetlocalsubmatrix
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matrestorelocalsubmatrix_ MATRESTORELOCALSUBMATRIX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matrestorelocalsubmatrix_ matrestorelocalsubmatrix
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matfindzerodiagonals_ MATFINDZERODIAGONALS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matfindzerodiagonals_ matfindzerodiagonals
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matfindoffblockdiagonalentries_ MATFINDOFFBLOCKDIAGONALENTRIES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matfindoffblockdiagonalentries_ matfindoffblockdiagonalentries
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matinvertblockdiagonalmat_ MATINVERTBLOCKDIAGONALMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matinvertblockdiagonalmat_ matinvertblockdiagonalmat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgetnonzerostate_ MATGETNONZEROSTATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgetnonzerostate_ matgetnonzerostate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matcreatempimatconcatenateseqmat_ MATCREATEMPIMATCONCATENATESEQMAT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matcreatempimatconcatenateseqmat_ matcreatempimatconcatenateseqmat
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matsubdomainscreatecoalesce_ MATSUBDOMAINSCREATECOALESCE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matsubdomainscreatecoalesce_ matsubdomainscreatecoalesce
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define matgalerkin_ MATGALERKIN
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define matgalerkin_ matgalerkin
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define mathasoperation_ MATHASOPERATION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define mathasoperation_ mathasoperation
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define mathascongruentlayouts_ MATHASCONGRUENTLAYOUTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define mathascongruentlayouts_ mathascongruentlayouts
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  matsetrandom_(Mat x,PetscRandom rctx, int *__ierr){
*__ierr = MatSetRandom(
	(Mat)PetscToPointer((x) ),
	(PetscRandom)PetscToPointer((rctx) ));
}
PETSC_EXTERN void PETSC_STDCALL  matfactorgeterrorzeropivot_(Mat mat,PetscReal *pivot,PetscInt *row, int *__ierr){
*__ierr = MatFactorGetErrorZeroPivot(
	(Mat)PetscToPointer((mat) ),pivot,row);
}
PETSC_EXTERN void PETSC_STDCALL  matfactorgeterror_(Mat mat,MatFactorError *err, int *__ierr){
*__ierr = MatFactorGetError(
	(Mat)PetscToPointer((mat) ),err);
}
PETSC_EXTERN void PETSC_STDCALL  matfactorclearerror_(Mat mat, int *__ierr){
*__ierr = MatFactorClearError(
	(Mat)PetscToPointer((mat) ));
}
PETSC_EXTERN void PETSC_STDCALL  matfindnonzerorows_(Mat mat,IS *keptrows, int *__ierr){
*__ierr = MatFindNonzeroRows(
	(Mat)PetscToPointer((mat) ),keptrows);
}
PETSC_EXTERN void PETSC_STDCALL  matfindzerorows_(Mat mat,IS *zerorows, int *__ierr){
*__ierr = MatFindZeroRows(
	(Mat)PetscToPointer((mat) ),zerorows);
}
PETSC_EXTERN void PETSC_STDCALL  matgetdiagonalblock_(Mat A,Mat *a, int *__ierr){
*__ierr = MatGetDiagonalBlock(
	(Mat)PetscToPointer((A) ),a);
}
PETSC_EXTERN void PETSC_STDCALL  matgettrace_(Mat mat,PetscScalar *trace, int *__ierr){
*__ierr = MatGetTrace(
	(Mat)PetscToPointer((mat) ),trace);
}
PETSC_EXTERN void PETSC_STDCALL  matrealpart_(Mat mat, int *__ierr){
*__ierr = MatRealPart(
	(Mat)PetscToPointer((mat) ));
}
PETSC_EXTERN void PETSC_STDCALL  matimaginarypart_(Mat mat, int *__ierr){
*__ierr = MatImaginaryPart(
	(Mat)PetscToPointer((mat) ));
}
PETSC_EXTERN void PETSC_STDCALL  matmissingdiagonal_(Mat mat,PetscBool *missing,PetscInt *dd, int *__ierr){
*__ierr = MatMissingDiagonal(
	(Mat)PetscToPointer((mat) ),missing,dd);
}
PETSC_EXTERN void PETSC_STDCALL  matconjugate_(Mat mat, int *__ierr){
*__ierr = MatConjugate(
	(Mat)PetscToPointer((mat) ));
}
PETSC_EXTERN void PETSC_STDCALL  matgetrowuppertriangular_(Mat mat, int *__ierr){
*__ierr = MatGetRowUpperTriangular(
	(Mat)PetscToPointer((mat) ));
}
PETSC_EXTERN void PETSC_STDCALL  matrestorerowuppertriangular_(Mat mat, int *__ierr){
*__ierr = MatRestoreRowUpperTriangular(
	(Mat)PetscToPointer((mat) ));
}
PETSC_EXTERN void PETSC_STDCALL  matresetpreallocation_(Mat A, int *__ierr){
*__ierr = MatResetPreallocation(
	(Mat)PetscToPointer((A) ));
}
PETSC_EXTERN void PETSC_STDCALL  matsetup_(Mat A, int *__ierr){
*__ierr = MatSetUp(
	(Mat)PetscToPointer((A) ));
}
PETSC_EXTERN void PETSC_STDCALL  matdestroy_(Mat *A, int *__ierr){
*__ierr = MatDestroy(A);
}
PETSC_EXTERN void PETSC_STDCALL  matsetvaluesrowlocal_(Mat mat,PetscInt *row, PetscScalar v[], int *__ierr){
*__ierr = MatSetValuesRowLocal(
	(Mat)PetscToPointer((mat) ),*row,v);
}
PETSC_EXTERN void PETSC_STDCALL  matsetvaluesrow_(Mat mat,PetscInt *row, PetscScalar v[], int *__ierr){
*__ierr = MatSetValuesRow(
	(Mat)PetscToPointer((mat) ),*row,v);
}
PETSC_EXTERN void PETSC_STDCALL  matsetvaluesstencil_(Mat mat,PetscInt *m, MatStencil idxm[],PetscInt *n, MatStencil idxn[], PetscScalar v[],InsertMode *addv, int *__ierr){
*__ierr = MatSetValuesStencil(
	(Mat)PetscToPointer((mat) ),*m,idxm,*n,idxn,v,*addv);
}
PETSC_EXTERN void PETSC_STDCALL  matsetvaluesblockedstencil_(Mat mat,PetscInt *m, MatStencil idxm[],PetscInt *n, MatStencil idxn[], PetscScalar v[],InsertMode *addv, int *__ierr){
*__ierr = MatSetValuesBlockedStencil(
	(Mat)PetscToPointer((mat) ),*m,idxm,*n,idxn,v,*addv);
}
PETSC_EXTERN void PETSC_STDCALL  matsetstencil_(Mat mat,PetscInt *dim, PetscInt dims[], PetscInt starts[],PetscInt *dof, int *__ierr){
*__ierr = MatSetStencil(
	(Mat)PetscToPointer((mat) ),*dim,dims,starts,*dof);
}
PETSC_EXTERN void PETSC_STDCALL  matgetvalues_(Mat mat,PetscInt *m, PetscInt idxm[],PetscInt *n, PetscInt idxn[],PetscScalar v[], int *__ierr){
*__ierr = MatGetValues(
	(Mat)PetscToPointer((mat) ),*m,idxm,*n,idxn,v);
}
PETSC_EXTERN void PETSC_STDCALL  matsetvaluesbatch_(Mat mat,PetscInt *nb,PetscInt *bs,PetscInt rows[], PetscScalar v[], int *__ierr){
*__ierr = MatSetValuesBatch(
	(Mat)PetscToPointer((mat) ),*nb,*bs,rows,v);
}
PETSC_EXTERN void PETSC_STDCALL  matsetlocaltoglobalmapping_(Mat x,ISLocalToGlobalMapping rmapping,ISLocalToGlobalMapping cmapping, int *__ierr){
*__ierr = MatSetLocalToGlobalMapping(
	(Mat)PetscToPointer((x) ),
	(ISLocalToGlobalMapping)PetscToPointer((rmapping) ),
	(ISLocalToGlobalMapping)PetscToPointer((cmapping) ));
}
PETSC_EXTERN void PETSC_STDCALL  matgetlocaltoglobalmapping_(Mat A,ISLocalToGlobalMapping *rmapping,ISLocalToGlobalMapping *cmapping, int *__ierr){
*__ierr = MatGetLocalToGlobalMapping(
	(Mat)PetscToPointer((A) ),rmapping,cmapping);
}
PETSC_EXTERN void PETSC_STDCALL  matgetlayouts_(Mat A,PetscLayout *rmap,PetscLayout *cmap, int *__ierr){
*__ierr = MatGetLayouts(
	(Mat)PetscToPointer((A) ),rmap,cmap);
}
PETSC_EXTERN void PETSC_STDCALL  matmultdiagonalblock_(Mat mat,Vec x,Vec y, int *__ierr){
*__ierr = MatMultDiagonalBlock(
	(Mat)PetscToPointer((mat) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void PETSC_STDCALL  matmult_(Mat mat,Vec x,Vec y, int *__ierr){
*__ierr = MatMult(
	(Mat)PetscToPointer((mat) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void PETSC_STDCALL  matmulttranspose_(Mat mat,Vec x,Vec y, int *__ierr){
*__ierr = MatMultTranspose(
	(Mat)PetscToPointer((mat) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void PETSC_STDCALL  matmulthermitiantranspose_(Mat mat,Vec x,Vec y, int *__ierr){
*__ierr = MatMultHermitianTranspose(
	(Mat)PetscToPointer((mat) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void PETSC_STDCALL  matmultadd_(Mat mat,Vec v1,Vec v2,Vec v3, int *__ierr){
*__ierr = MatMultAdd(
	(Mat)PetscToPointer((mat) ),
	(Vec)PetscToPointer((v1) ),
	(Vec)PetscToPointer((v2) ),
	(Vec)PetscToPointer((v3) ));
}
PETSC_EXTERN void PETSC_STDCALL  matmulttransposeadd_(Mat mat,Vec v1,Vec v2,Vec v3, int *__ierr){
*__ierr = MatMultTransposeAdd(
	(Mat)PetscToPointer((mat) ),
	(Vec)PetscToPointer((v1) ),
	(Vec)PetscToPointer((v2) ),
	(Vec)PetscToPointer((v3) ));
}
PETSC_EXTERN void PETSC_STDCALL  matmulthermitiantransposeadd_(Mat mat,Vec v1,Vec v2,Vec v3, int *__ierr){
*__ierr = MatMultHermitianTransposeAdd(
	(Mat)PetscToPointer((mat) ),
	(Vec)PetscToPointer((v1) ),
	(Vec)PetscToPointer((v2) ),
	(Vec)PetscToPointer((v3) ));
}
PETSC_EXTERN void PETSC_STDCALL  matmultconstrained_(Mat mat,Vec x,Vec y, int *__ierr){
*__ierr = MatMultConstrained(
	(Mat)PetscToPointer((mat) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void PETSC_STDCALL  matmulttransposeconstrained_(Mat mat,Vec x,Vec y, int *__ierr){
*__ierr = MatMultTransposeConstrained(
	(Mat)PetscToPointer((mat) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void PETSC_STDCALL  matsolve_(Mat mat,Vec b,Vec x, int *__ierr){
*__ierr = MatSolve(
	(Mat)PetscToPointer((mat) ),
	(Vec)PetscToPointer((b) ),
	(Vec)PetscToPointer((x) ));
}
PETSC_EXTERN void PETSC_STDCALL  matmatsolve_(Mat A,Mat B,Mat X, int *__ierr){
*__ierr = MatMatSolve(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),
	(Mat)PetscToPointer((X) ));
}
PETSC_EXTERN void PETSC_STDCALL  matmatsolvetranspose_(Mat A,Mat B,Mat X, int *__ierr){
*__ierr = MatMatSolveTranspose(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),
	(Mat)PetscToPointer((X) ));
}
PETSC_EXTERN void PETSC_STDCALL  matmattransposesolve_(Mat A,Mat Bt,Mat X, int *__ierr){
*__ierr = MatMatTransposeSolve(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((Bt) ),
	(Mat)PetscToPointer((X) ));
}
PETSC_EXTERN void PETSC_STDCALL  matforwardsolve_(Mat mat,Vec b,Vec x, int *__ierr){
*__ierr = MatForwardSolve(
	(Mat)PetscToPointer((mat) ),
	(Vec)PetscToPointer((b) ),
	(Vec)PetscToPointer((x) ));
}
PETSC_EXTERN void PETSC_STDCALL  matbackwardsolve_(Mat mat,Vec b,Vec x, int *__ierr){
*__ierr = MatBackwardSolve(
	(Mat)PetscToPointer((mat) ),
	(Vec)PetscToPointer((b) ),
	(Vec)PetscToPointer((x) ));
}
PETSC_EXTERN void PETSC_STDCALL  matsolveadd_(Mat mat,Vec b,Vec y,Vec x, int *__ierr){
*__ierr = MatSolveAdd(
	(Mat)PetscToPointer((mat) ),
	(Vec)PetscToPointer((b) ),
	(Vec)PetscToPointer((y) ),
	(Vec)PetscToPointer((x) ));
}
PETSC_EXTERN void PETSC_STDCALL  matsolvetranspose_(Mat mat,Vec b,Vec x, int *__ierr){
*__ierr = MatSolveTranspose(
	(Mat)PetscToPointer((mat) ),
	(Vec)PetscToPointer((b) ),
	(Vec)PetscToPointer((x) ));
}
PETSC_EXTERN void PETSC_STDCALL  matsolvetransposeadd_(Mat mat,Vec b,Vec y,Vec x, int *__ierr){
*__ierr = MatSolveTransposeAdd(
	(Mat)PetscToPointer((mat) ),
	(Vec)PetscToPointer((b) ),
	(Vec)PetscToPointer((y) ),
	(Vec)PetscToPointer((x) ));
}
PETSC_EXTERN void PETSC_STDCALL  matsor_(Mat mat,Vec b,PetscReal *omega,MatSORType *flag,PetscReal *shift,PetscInt *its,PetscInt *lits,Vec x, int *__ierr){
*__ierr = MatSOR(
	(Mat)PetscToPointer((mat) ),
	(Vec)PetscToPointer((b) ),*omega,*flag,*shift,*its,*lits,
	(Vec)PetscToPointer((x) ));
}
PETSC_EXTERN void PETSC_STDCALL  matcopy_(Mat A,Mat B,MatStructure *str, int *__ierr){
*__ierr = MatCopy(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),*str);
}
PETSC_EXTERN void PETSC_STDCALL  matduplicate_(Mat mat,MatDuplicateOption *op,Mat *M, int *__ierr){
*__ierr = MatDuplicate(
	(Mat)PetscToPointer((mat) ),*op,M);
}
PETSC_EXTERN void PETSC_STDCALL  matgetdiagonal_(Mat mat,Vec v, int *__ierr){
*__ierr = MatGetDiagonal(
	(Mat)PetscToPointer((mat) ),
	(Vec)PetscToPointer((v) ));
}
PETSC_EXTERN void PETSC_STDCALL  matgetrowsum_(Mat mat,Vec v, int *__ierr){
*__ierr = MatGetRowSum(
	(Mat)PetscToPointer((mat) ),
	(Vec)PetscToPointer((v) ));
}
PETSC_EXTERN void PETSC_STDCALL  mattranspose_(Mat mat,MatReuse *reuse,Mat *B, int *__ierr){
*__ierr = MatTranspose(
	(Mat)PetscToPointer((mat) ),*reuse,B);
}
PETSC_EXTERN void PETSC_STDCALL  matistranspose_(Mat A,Mat B,PetscReal *tol,PetscBool  *flg, int *__ierr){
*__ierr = MatIsTranspose(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),*tol,flg);
}
PETSC_EXTERN void PETSC_STDCALL  mathermitiantranspose_(Mat mat,MatReuse *reuse,Mat *B, int *__ierr){
*__ierr = MatHermitianTranspose(
	(Mat)PetscToPointer((mat) ),*reuse,B);
}
PETSC_EXTERN void PETSC_STDCALL  matishermitiantranspose_(Mat A,Mat B,PetscReal *tol,PetscBool  *flg, int *__ierr){
*__ierr = MatIsHermitianTranspose(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),*tol,flg);
}
PETSC_EXTERN void PETSC_STDCALL  matpermute_(Mat mat,IS row,IS col,Mat *B, int *__ierr){
*__ierr = MatPermute(
	(Mat)PetscToPointer((mat) ),
	(IS)PetscToPointer((row) ),
	(IS)PetscToPointer((col) ),B);
}
PETSC_EXTERN void PETSC_STDCALL  matequal_(Mat A,Mat B,PetscBool  *flg, int *__ierr){
*__ierr = MatEqual(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),flg);
}
PETSC_EXTERN void PETSC_STDCALL  matdiagonalscale_(Mat mat,Vec l,Vec r, int *__ierr){
*__ierr = MatDiagonalScale(
	(Mat)PetscToPointer((mat) ),
	(Vec)PetscToPointer((l) ),
	(Vec)PetscToPointer((r) ));
}
PETSC_EXTERN void PETSC_STDCALL  matscale_(Mat mat,PetscScalar *a, int *__ierr){
*__ierr = MatScale(
	(Mat)PetscToPointer((mat) ),*a);
}
PETSC_EXTERN void PETSC_STDCALL  matnorm_(Mat mat,NormType *type,PetscReal *nrm, int *__ierr){
*__ierr = MatNorm(
	(Mat)PetscToPointer((mat) ),*type,nrm);
}
PETSC_EXTERN void PETSC_STDCALL  matassemblybegin_(Mat mat,MatAssemblyType *type, int *__ierr){
*__ierr = MatAssemblyBegin(
	(Mat)PetscToPointer((mat) ),*type);
}
PETSC_EXTERN void PETSC_STDCALL  matassembled_(Mat mat,PetscBool  *assembled, int *__ierr){
*__ierr = MatAssembled(
	(Mat)PetscToPointer((mat) ),assembled);
}
PETSC_EXTERN void PETSC_STDCALL  matassemblyend_(Mat mat,MatAssemblyType *type, int *__ierr){
*__ierr = MatAssemblyEnd(
	(Mat)PetscToPointer((mat) ),*type);
}
PETSC_EXTERN void PETSC_STDCALL  matsetoption_(Mat mat,MatOption *op,PetscBool *flg, int *__ierr){
*__ierr = MatSetOption(
	(Mat)PetscToPointer((mat) ),*op,*flg);
}
PETSC_EXTERN void PETSC_STDCALL  matgetoption_(Mat mat,MatOption *op,PetscBool *flg, int *__ierr){
*__ierr = MatGetOption(
	(Mat)PetscToPointer((mat) ),*op,flg);
}
PETSC_EXTERN void PETSC_STDCALL  matzeroentries_(Mat mat, int *__ierr){
*__ierr = MatZeroEntries(
	(Mat)PetscToPointer((mat) ));
}
PETSC_EXTERN void PETSC_STDCALL  matzerorowscolumns_(Mat mat,PetscInt *numRows, PetscInt rows[],PetscScalar *diag,Vec x,Vec b, int *__ierr){
*__ierr = MatZeroRowsColumns(
	(Mat)PetscToPointer((mat) ),*numRows,rows,*diag,
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((b) ));
}
PETSC_EXTERN void PETSC_STDCALL  matzerorowscolumnsis_(Mat mat,IS is,PetscScalar *diag,Vec x,Vec b, int *__ierr){
*__ierr = MatZeroRowsColumnsIS(
	(Mat)PetscToPointer((mat) ),
	(IS)PetscToPointer((is) ),*diag,
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((b) ));
}
PETSC_EXTERN void PETSC_STDCALL  matzerorows_(Mat mat,PetscInt *numRows, PetscInt rows[],PetscScalar *diag,Vec x,Vec b, int *__ierr){
*__ierr = MatZeroRows(
	(Mat)PetscToPointer((mat) ),*numRows,rows,*diag,
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((b) ));
}
PETSC_EXTERN void PETSC_STDCALL  matzerorowsis_(Mat mat,IS is,PetscScalar *diag,Vec x,Vec b, int *__ierr){
*__ierr = MatZeroRowsIS(
	(Mat)PetscToPointer((mat) ),
	(IS)PetscToPointer((is) ),*diag,
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((b) ));
}
PETSC_EXTERN void PETSC_STDCALL  matzerorowsstencil_(Mat mat,PetscInt *numRows, MatStencil rows[],PetscScalar *diag,Vec x,Vec b, int *__ierr){
*__ierr = MatZeroRowsStencil(
	(Mat)PetscToPointer((mat) ),*numRows,rows,*diag,
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((b) ));
}
PETSC_EXTERN void PETSC_STDCALL  matzerorowscolumnsstencil_(Mat mat,PetscInt *numRows, MatStencil rows[],PetscScalar *diag,Vec x,Vec b, int *__ierr){
*__ierr = MatZeroRowsColumnsStencil(
	(Mat)PetscToPointer((mat) ),*numRows,rows,*diag,
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((b) ));
}
PETSC_EXTERN void PETSC_STDCALL  matzerorowslocalis_(Mat mat,IS is,PetscScalar *diag,Vec x,Vec b, int *__ierr){
*__ierr = MatZeroRowsLocalIS(
	(Mat)PetscToPointer((mat) ),
	(IS)PetscToPointer((is) ),*diag,
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((b) ));
}
PETSC_EXTERN void PETSC_STDCALL  matzerorowscolumnslocal_(Mat mat,PetscInt *numRows, PetscInt rows[],PetscScalar *diag,Vec x,Vec b, int *__ierr){
*__ierr = MatZeroRowsColumnsLocal(
	(Mat)PetscToPointer((mat) ),*numRows,rows,*diag,
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((b) ));
}
PETSC_EXTERN void PETSC_STDCALL  matzerorowscolumnslocalis_(Mat mat,IS is,PetscScalar *diag,Vec x,Vec b, int *__ierr){
*__ierr = MatZeroRowsColumnsLocalIS(
	(Mat)PetscToPointer((mat) ),
	(IS)PetscToPointer((is) ),*diag,
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((b) ));
}
PETSC_EXTERN void PETSC_STDCALL  matincreaseoverlap_(Mat mat,PetscInt *n,IS is[],PetscInt *ov, int *__ierr){
*__ierr = MatIncreaseOverlap(
	(Mat)PetscToPointer((mat) ),*n,is,*ov);
}
PETSC_EXTERN void PETSC_STDCALL  matincreaseoverlapsplit_(Mat mat,PetscInt *n,IS is[],PetscInt *ov, int *__ierr){
*__ierr = MatIncreaseOverlapSplit(
	(Mat)PetscToPointer((mat) ),*n,is,*ov);
}
PETSC_EXTERN void PETSC_STDCALL  matgetblocksize_(Mat mat,PetscInt *bs, int *__ierr){
*__ierr = MatGetBlockSize(
	(Mat)PetscToPointer((mat) ),bs);
}
PETSC_EXTERN void PETSC_STDCALL  matgetblocksizes_(Mat mat,PetscInt *rbs,PetscInt *cbs, int *__ierr){
*__ierr = MatGetBlockSizes(
	(Mat)PetscToPointer((mat) ),rbs,cbs);
}
PETSC_EXTERN void PETSC_STDCALL  matsetblocksize_(Mat mat,PetscInt *bs, int *__ierr){
*__ierr = MatSetBlockSize(
	(Mat)PetscToPointer((mat) ),*bs);
}
PETSC_EXTERN void PETSC_STDCALL  matsetvariableblocksizes_(Mat mat,PetscInt *nblocks,PetscInt *bsizes, int *__ierr){
*__ierr = MatSetVariableBlockSizes(
	(Mat)PetscToPointer((mat) ),*nblocks,bsizes);
}
PETSC_EXTERN void PETSC_STDCALL  matsetblocksizes_(Mat mat,PetscInt *rbs,PetscInt *cbs, int *__ierr){
*__ierr = MatSetBlockSizes(
	(Mat)PetscToPointer((mat) ),*rbs,*cbs);
}
PETSC_EXTERN void PETSC_STDCALL  matsetblocksizesfrommats_(Mat mat,Mat fromRow,Mat fromCol, int *__ierr){
*__ierr = MatSetBlockSizesFromMats(
	(Mat)PetscToPointer((mat) ),
	(Mat)PetscToPointer((fromRow) ),
	(Mat)PetscToPointer((fromCol) ));
}
PETSC_EXTERN void PETSC_STDCALL  matresidual_(Mat mat,Vec b,Vec x,Vec r, int *__ierr){
*__ierr = MatResidual(
	(Mat)PetscToPointer((mat) ),
	(Vec)PetscToPointer((b) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((r) ));
}
PETSC_EXTERN void PETSC_STDCALL  matsetunfactored_(Mat mat, int *__ierr){
*__ierr = MatSetUnfactored(
	(Mat)PetscToPointer((mat) ));
}
PETSC_EXTERN void PETSC_STDCALL  matcreatesubmatrix_(Mat mat,IS isrow,IS iscol,MatReuse *cll,Mat *newmat, int *__ierr){
*__ierr = MatCreateSubMatrix(
	(Mat)PetscToPointer((mat) ),
	(IS)PetscToPointer((isrow) ),
	(IS)PetscToPointer((iscol) ),*cll,newmat);
}
PETSC_EXTERN void PETSC_STDCALL  matstashsetinitialsize_(Mat mat,PetscInt *size,PetscInt *bsize, int *__ierr){
*__ierr = MatStashSetInitialSize(
	(Mat)PetscToPointer((mat) ),*size,*bsize);
}
PETSC_EXTERN void PETSC_STDCALL  matinterpolateadd_(Mat A,Vec x,Vec y,Vec w, int *__ierr){
*__ierr = MatInterpolateAdd(
	(Mat)PetscToPointer((A) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ),
	(Vec)PetscToPointer((w) ));
}
PETSC_EXTERN void PETSC_STDCALL  matinterpolate_(Mat A,Vec x,Vec y, int *__ierr){
*__ierr = MatInterpolate(
	(Mat)PetscToPointer((A) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void PETSC_STDCALL  matrestrict_(Mat A,Vec x,Vec y, int *__ierr){
*__ierr = MatRestrict(
	(Mat)PetscToPointer((A) ),
	(Vec)PetscToPointer((x) ),
	(Vec)PetscToPointer((y) ));
}
PETSC_EXTERN void PETSC_STDCALL  matgetnullspace_(Mat mat,MatNullSpace *nullsp, int *__ierr){
*__ierr = MatGetNullSpace(
	(Mat)PetscToPointer((mat) ),nullsp);
}
PETSC_EXTERN void PETSC_STDCALL  matsetnullspace_(Mat mat,MatNullSpace nullsp, int *__ierr){
*__ierr = MatSetNullSpace(
	(Mat)PetscToPointer((mat) ),
	(MatNullSpace)PetscToPointer((nullsp) ));
}
PETSC_EXTERN void PETSC_STDCALL  matgettransposenullspace_(Mat mat,MatNullSpace *nullsp, int *__ierr){
*__ierr = MatGetTransposeNullSpace(
	(Mat)PetscToPointer((mat) ),nullsp);
}
PETSC_EXTERN void PETSC_STDCALL  matsettransposenullspace_(Mat mat,MatNullSpace nullsp, int *__ierr){
*__ierr = MatSetTransposeNullSpace(
	(Mat)PetscToPointer((mat) ),
	(MatNullSpace)PetscToPointer((nullsp) ));
}
PETSC_EXTERN void PETSC_STDCALL  matsetnearnullspace_(Mat mat,MatNullSpace nullsp, int *__ierr){
*__ierr = MatSetNearNullSpace(
	(Mat)PetscToPointer((mat) ),
	(MatNullSpace)PetscToPointer((nullsp) ));
}
PETSC_EXTERN void PETSC_STDCALL  matgetnearnullspace_(Mat mat,MatNullSpace *nullsp, int *__ierr){
*__ierr = MatGetNearNullSpace(
	(Mat)PetscToPointer((mat) ),nullsp);
}
PETSC_EXTERN void PETSC_STDCALL  matdiagonalscalelocal_(Mat mat,Vec diag, int *__ierr){
*__ierr = MatDiagonalScaleLocal(
	(Mat)PetscToPointer((mat) ),
	(Vec)PetscToPointer((diag) ));
}
PETSC_EXTERN void PETSC_STDCALL  matgetinertia_(Mat mat,PetscInt *nneg,PetscInt *nzero,PetscInt *npos, int *__ierr){
*__ierr = MatGetInertia(
	(Mat)PetscToPointer((mat) ),nneg,nzero,npos);
}
PETSC_EXTERN void PETSC_STDCALL  matissymmetric_(Mat A,PetscReal *tol,PetscBool  *flg, int *__ierr){
*__ierr = MatIsSymmetric(
	(Mat)PetscToPointer((A) ),*tol,flg);
}
PETSC_EXTERN void PETSC_STDCALL  matishermitian_(Mat A,PetscReal *tol,PetscBool  *flg, int *__ierr){
*__ierr = MatIsHermitian(
	(Mat)PetscToPointer((A) ),*tol,flg);
}
PETSC_EXTERN void PETSC_STDCALL  matissymmetricknown_(Mat A,PetscBool  *set,PetscBool  *flg, int *__ierr){
*__ierr = MatIsSymmetricKnown(
	(Mat)PetscToPointer((A) ),set,flg);
}
PETSC_EXTERN void PETSC_STDCALL  matishermitianknown_(Mat A,PetscBool  *set,PetscBool  *flg, int *__ierr){
*__ierr = MatIsHermitianKnown(
	(Mat)PetscToPointer((A) ),set,flg);
}
PETSC_EXTERN void PETSC_STDCALL  matisstructurallysymmetric_(Mat A,PetscBool  *flg, int *__ierr){
*__ierr = MatIsStructurallySymmetric(
	(Mat)PetscToPointer((A) ),flg);
}
PETSC_EXTERN void PETSC_STDCALL  matstashgetinfo_(Mat mat,PetscInt *nstash,PetscInt *reallocs,PetscInt *bnstash,PetscInt *breallocs, int *__ierr){
*__ierr = MatStashGetInfo(
	(Mat)PetscToPointer((mat) ),nstash,reallocs,bnstash,breallocs);
}
PETSC_EXTERN void PETSC_STDCALL  matfactorsetschuris_(Mat mat,IS is, int *__ierr){
*__ierr = MatFactorSetSchurIS(
	(Mat)PetscToPointer((mat) ),
	(IS)PetscToPointer((is) ));
}
PETSC_EXTERN void PETSC_STDCALL  matfactorcreateschurcomplement_(Mat F,Mat* S,MatFactorSchurStatus* status, int *__ierr){
*__ierr = MatFactorCreateSchurComplement(
	(Mat)PetscToPointer((F) ),S,
	(MatFactorSchurStatus* )PetscToPointer((status) ));
}
PETSC_EXTERN void PETSC_STDCALL  matfactorgetschurcomplement_(Mat F,Mat* S,MatFactorSchurStatus* status, int *__ierr){
*__ierr = MatFactorGetSchurComplement(
	(Mat)PetscToPointer((F) ),S,
	(MatFactorSchurStatus* )PetscToPointer((status) ));
}
PETSC_EXTERN void PETSC_STDCALL  matfactorrestoreschurcomplement_(Mat F,Mat* S,MatFactorSchurStatus *status, int *__ierr){
*__ierr = MatFactorRestoreSchurComplement(
	(Mat)PetscToPointer((F) ),S,*status);
}
PETSC_EXTERN void PETSC_STDCALL  matfactorsolveschurcomplementtranspose_(Mat F,Vec rhs,Vec sol, int *__ierr){
*__ierr = MatFactorSolveSchurComplementTranspose(
	(Mat)PetscToPointer((F) ),
	(Vec)PetscToPointer((rhs) ),
	(Vec)PetscToPointer((sol) ));
}
PETSC_EXTERN void PETSC_STDCALL  matfactorsolveschurcomplement_(Mat F,Vec rhs,Vec sol, int *__ierr){
*__ierr = MatFactorSolveSchurComplement(
	(Mat)PetscToPointer((F) ),
	(Vec)PetscToPointer((rhs) ),
	(Vec)PetscToPointer((sol) ));
}
PETSC_EXTERN void PETSC_STDCALL  matfactorinvertschurcomplement_(Mat F, int *__ierr){
*__ierr = MatFactorInvertSchurComplement(
	(Mat)PetscToPointer((F) ));
}
PETSC_EXTERN void PETSC_STDCALL  matfactorfactorizeschurcomplement_(Mat F, int *__ierr){
*__ierr = MatFactorFactorizeSchurComplement(
	(Mat)PetscToPointer((F) ));
}
PETSC_EXTERN void PETSC_STDCALL  matptap_(Mat A,Mat P,MatReuse *scall,PetscReal *fill,Mat *C, int *__ierr){
*__ierr = MatPtAP(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((P) ),*scall,*fill,C);
}
PETSC_EXTERN void PETSC_STDCALL  matptapnumeric_(Mat A,Mat P,Mat C, int *__ierr){
*__ierr = MatPtAPNumeric(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((P) ),
	(Mat)PetscToPointer((C) ));
}
PETSC_EXTERN void PETSC_STDCALL  matptapsymbolic_(Mat A,Mat P,PetscReal *fill,Mat *C, int *__ierr){
*__ierr = MatPtAPSymbolic(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((P) ),*fill,C);
}
PETSC_EXTERN void PETSC_STDCALL  matrart_(Mat A,Mat R,MatReuse *scall,PetscReal *fill,Mat *C, int *__ierr){
*__ierr = MatRARt(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((R) ),*scall,*fill,C);
}
PETSC_EXTERN void PETSC_STDCALL  matrartnumeric_(Mat A,Mat R,Mat C, int *__ierr){
*__ierr = MatRARtNumeric(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((R) ),
	(Mat)PetscToPointer((C) ));
}
PETSC_EXTERN void PETSC_STDCALL  matrartsymbolic_(Mat A,Mat R,PetscReal *fill,Mat *C, int *__ierr){
*__ierr = MatRARtSymbolic(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((R) ),*fill,C);
}
PETSC_EXTERN void PETSC_STDCALL  matmatmult_(Mat A,Mat B,MatReuse *scall,PetscReal *fill,Mat *C, int *__ierr){
*__ierr = MatMatMult(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),*scall,*fill,C);
}
PETSC_EXTERN void PETSC_STDCALL  matmatmultsymbolic_(Mat A,Mat B,PetscReal *fill,Mat *C, int *__ierr){
*__ierr = MatMatMultSymbolic(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),*fill,C);
}
PETSC_EXTERN void PETSC_STDCALL  matmatmultnumeric_(Mat A,Mat B,Mat C, int *__ierr){
*__ierr = MatMatMultNumeric(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),
	(Mat)PetscToPointer((C) ));
}
PETSC_EXTERN void PETSC_STDCALL  matmattransposemult_(Mat A,Mat B,MatReuse *scall,PetscReal *fill,Mat *C, int *__ierr){
*__ierr = MatMatTransposeMult(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),*scall,*fill,C);
}
PETSC_EXTERN void PETSC_STDCALL  mattransposematmult_(Mat A,Mat B,MatReuse *scall,PetscReal *fill,Mat *C, int *__ierr){
*__ierr = MatTransposeMatMult(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),*scall,*fill,C);
}
PETSC_EXTERN void PETSC_STDCALL  matmatmatmult_(Mat A,Mat B,Mat C,MatReuse *scall,PetscReal *fill,Mat *D, int *__ierr){
*__ierr = MatMatMatMult(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((B) ),
	(Mat)PetscToPointer((C) ),*scall,*fill,D);
}
PETSC_EXTERN void PETSC_STDCALL  matcreateredundantmatrix_(Mat mat,PetscInt *nsubcomm,MPI_Fint * subcomm,MatReuse *reuse,Mat *matredundant, int *__ierr){
*__ierr = MatCreateRedundantMatrix(
	(Mat)PetscToPointer((mat) ),*nsubcomm,
	MPI_Comm_f2c(*(subcomm)),*reuse,matredundant);
}
PETSC_EXTERN void PETSC_STDCALL  matgetlocalsubmatrix_(Mat mat,IS isrow,IS iscol,Mat *submat, int *__ierr){
*__ierr = MatGetLocalSubMatrix(
	(Mat)PetscToPointer((mat) ),
	(IS)PetscToPointer((isrow) ),
	(IS)PetscToPointer((iscol) ),submat);
}
PETSC_EXTERN void PETSC_STDCALL  matrestorelocalsubmatrix_(Mat mat,IS isrow,IS iscol,Mat *submat, int *__ierr){
*__ierr = MatRestoreLocalSubMatrix(
	(Mat)PetscToPointer((mat) ),
	(IS)PetscToPointer((isrow) ),
	(IS)PetscToPointer((iscol) ),submat);
}
PETSC_EXTERN void PETSC_STDCALL  matfindzerodiagonals_(Mat mat,IS *is, int *__ierr){
*__ierr = MatFindZeroDiagonals(
	(Mat)PetscToPointer((mat) ),is);
}
PETSC_EXTERN void PETSC_STDCALL  matfindoffblockdiagonalentries_(Mat mat,IS *is, int *__ierr){
*__ierr = MatFindOffBlockDiagonalEntries(
	(Mat)PetscToPointer((mat) ),is);
}
PETSC_EXTERN void PETSC_STDCALL  matinvertblockdiagonalmat_(Mat A,Mat C, int *__ierr){
*__ierr = MatInvertBlockDiagonalMat(
	(Mat)PetscToPointer((A) ),
	(Mat)PetscToPointer((C) ));
}
PETSC_EXTERN void PETSC_STDCALL  matgetnonzerostate_(Mat mat,PetscObjectState *state, int *__ierr){
*__ierr = MatGetNonzeroState(
	(Mat)PetscToPointer((mat) ),
	(PetscObjectState* )PetscToPointer((state) ));
}
PETSC_EXTERN void PETSC_STDCALL  matcreatempimatconcatenateseqmat_(MPI_Fint * comm,Mat seqmat,PetscInt *n,MatReuse *reuse,Mat *mpimat, int *__ierr){
*__ierr = MatCreateMPIMatConcatenateSeqMat(
	MPI_Comm_f2c(*(comm)),
	(Mat)PetscToPointer((seqmat) ),*n,*reuse,mpimat);
}
PETSC_EXTERN void PETSC_STDCALL  matsubdomainscreatecoalesce_(Mat A,PetscInt *N,PetscInt *n,IS *iss[], int *__ierr){
*__ierr = MatSubdomainsCreateCoalesce(
	(Mat)PetscToPointer((A) ),*N,n,iss);
}
PETSC_EXTERN void PETSC_STDCALL  matgalerkin_(Mat restrct,Mat dA,Mat interpolate,MatReuse *reuse,PetscReal *fill,Mat *A, int *__ierr){
*__ierr = MatGalerkin(
	(Mat)PetscToPointer((restrct) ),
	(Mat)PetscToPointer((dA) ),
	(Mat)PetscToPointer((interpolate) ),*reuse,*fill,A);
}
PETSC_EXTERN void PETSC_STDCALL  mathasoperation_(Mat mat,MatOperation *op,PetscBool *has, int *__ierr){
*__ierr = MatHasOperation(
	(Mat)PetscToPointer((mat) ),*op,has);
}
PETSC_EXTERN void PETSC_STDCALL  mathascongruentlayouts_(Mat mat,PetscBool *cong, int *__ierr){
*__ierr = MatHasCongruentLayouts(
	(Mat)PetscToPointer((mat) ),cong);
}
#if defined(__cplusplus)
}
#endif
