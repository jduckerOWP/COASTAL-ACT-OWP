
# This directory contains SNES example programs for solving systems of
# nonlinear equations.

#CPPFLAGS	 = -I/PETSc3/geodynamics/PetscSimulationsViewers/src
CFLAGS           =
FFLAGS		 =
CPPFLAGS         =
FPPFLAGS         =
LOCDIR		 = src/snes/examples/tutorials/
MANSEC           = SNES
EXAMPLESC	 = ex1.c ex2.c ex3.c  ex5.c ex5s.c ex9.c \
                ex12.c ex14.c ex15.c ex18.c ex19.c ex20.c ex21.c ex22.c \
                ex25.c ex28.c ex30.c ex33.c \
                ex35.c ex42.c ex46.c ex48.c \
                ex56.c ex58.c ex59.c \
                ex62.c ex70.c ex77.c ex78.c \
                ex47cu.cu
EXAMPLESF	 = ex1f.F90 ex5f.F90 ex5f90.F90 ex5f90t.F90 ex5fs.F ex40f90.F90 ex73f90t.F90
EXAMPLESCH	 =
EXAMPLESFH       = ex5f.h
EXAMPLESMATLAB   = ex5m.m  ex61genm.m ex61m.m
DIRS		 = ex10d network

include ${PETSC_DIR}/lib/petsc/conf/variables
include ${PETSC_DIR}/lib/petsc/conf/rules
#
#  The SGI parallelizing compiler generates incorrect code by treating
#  the math functions (such as sqrt and exp) as local variables. The
#  sed below patches this.
#
ex5s: chkopts
	@if [ "${PETSC_ARCH}" != "IRIX64" ]; then echo "Only for PETSC_ARCH of IRIX64"; false ; fi
	-${CC} -pca keep  -WK,-lo=l ${FCONF} ${CFLAGS} -c ex5s.c
	sed "s/, sqrt/ /g"   ex5s.M | sed "s/, exp/ /g"  > ex5s_tmp.c
	-${CC} -mp ${PCC_FLAGS} ${CFLAGS} ${CCPPFLAGS} -c ex5s_tmp.c
	-${FC} -pfa keep -mp -64 ${FC_FLAGS} ${FFLAGS} ${FCPPFLAGS} -c ex5fs.F
	-${CLINKER} -mp -o ex5s ex5s_tmp.o ex5fs.o ${PETSC_SNES_LIB}
	${RM} ex5s.o
#-------------------------------------------------------------------------

#  these tests are used by the makefile in PETSC_DIR for basic tests of the install and should not be removed
testex5f: ex5f.PETSc
	@if [ "${PETSC_WITH_BATCH}" != "" ]; then \
           echo "Running with batch filesystem; to test run src/snes/examples/tutorials/ex5f with" ; \
           echo "your systems batch system"; \
        elif [ "${MPIEXEC}" = "/bin/false" ]; then \
           echo "*mpiexec not found*. Please run src/snes/examples/tutorials/ex5f manually"; \
        elif [ -f ex5f ]; then \
	   ${MPIEXEC} -n 1 ./ex5f > ex5f_1.tmp 2>&1; \
	   if (${DIFF} output/ex5f_1.testout ex5f_1.tmp > /dev/null 2>&1) then \
           echo "Fortran example src/snes/examples/tutorials/ex5f run successfully with 1 MPI process"; \
	   else echo "Possible error running Fortran example src/snes/examples/tutorials/ex5f with 1 MPI process"; \
           echo "See http://www.mcs.anl.gov/petsc/documentation/faq.html";\
           cat ex5f_1.tmp; fi;  \
         ${RM} -f ex5f_1.tmp ;\
         ${MAKE} PETSC_ARCH=${PETSC_ARCH} PETSC_DIR=${PETSC_DIR} ex5f.rm; fi

testex19: ex19.PETSc
	-@if [ "${PETSC_WITH_BATCH}" != "" ]; then \
           echo "Running with batch filesystem; to test run src/snes/examples/tutorials/ex19 with" ; \
           echo "your systems batch system"; \
        elif [ "${MPIEXEC}" = "/bin/false" ]; then \
           echo "*mpiexec not found*. Please run src/snes/examples/tutorials/ex19 manually"; \
	elif [ -f ex19 ]; then \
           ${MPIEXEC} -n 1 ./ex19 -da_refine 3 -pc_type mg -ksp_type fgmres  > ex19_1.tmp 2>&1; \
	   if (${DIFF} output/ex19_1.testout ex19_1.tmp > /dev/null 2>&1) then \
           echo "C/C++ example src/snes/examples/tutorials/ex19 run successfully with 1 MPI process"; \
	   else echo "Possible error running C/C++ src/snes/examples/tutorials/ex19 with 1 MPI process"; \
           echo "See http://www.mcs.anl.gov/petsc/documentation/faq.html";\
           cat ex19_1.tmp; fi; \
	if [ ! "${MPI_IS_MPIUNI}" ]; then \
           ${MPIEXEC} -n 2 ./ex19 -da_refine 3 -pc_type mg -ksp_type fgmres  > ex19_1.tmp 2>&1; \
	   if (${DIFF} output/ex19_1.testout ex19_1.tmp > /dev/null 2>&1) then \
           echo "C/C++ example src/snes/examples/tutorials/ex19 run successfully with 2 MPI processes"; \
	   else echo "Possible error running C/C++ src/snes/examples/tutorials/ex19 with 2 MPI processes"; \
           echo "See http://www.mcs.anl.gov/petsc/documentation/faq.html";\
           cat ex19_1.tmp; fi; fi; \
        ${RM} -f ex19_1.tmp; fi
runex19_hypre:
	-@${MPIEXEC} -n 2 ./ex19 -da_refine 3 -snes_monitor_short -pc_type hypre > ex19_1.tmp 2>&1; \
	   if (${DIFF} output/ex19_hypre.out ex19_1.tmp) then true; \
	   else  printf "${PWD}\nPossible problem with ex19_hypre, diffs above\n=========================================\n"; fi; \
	   ${RM} -f ex19_1.tmp
runex19_ml:
	-@${MPIEXEC} -n 2 ./ex19 -da_refine 3 -snes_monitor_short -pc_type ml > ex19_1.tmp 2>&1; \
	   if (${DIFF} output/ex19_ml.out ex19_1.tmp) then true; \
	   else  printf "${PWD}\nPossible problem with ex19_ml, diffs above\n=========================================\n"; fi; \
           ${RM} -f ex19_1.tmp
runex19_fieldsplit_mumps:
	-@${MPIEXEC} -n 2 ./ex19 -pc_type fieldsplit -pc_fieldsplit_block_size 4 -pc_fieldsplit_type SCHUR -pc_fieldsplit_0_fields 0,1,2 -pc_fieldsplit_1_fields 3 -fieldsplit_0_pc_type lu -fieldsplit_1_pc_type lu -snes_monitor_short -ksp_monitor_short  -fieldsplit_0_pc_factor_mat_solver_type mumps -fieldsplit_1_pc_factor_mat_solver_type mumps > ex19_6.tmp 2>&1; \
	   if (${DIFF} output/ex19_fieldsplit_5.out ex19_6.tmp) then true; \
	   else  printf "${PWD}\nPossible problem with ex19_fieldsplit_fieldsplit_mumps, diffs above\n=========================================\n"; fi; \
           ${RM} -f ex19_6.tmp
runex19_superlu_dist:
	-@${MPIEXEC} -n 1 ./ex19 -da_grid_x 20 -da_grid_y 20 -pc_type lu -pc_factor_mat_solver_type superlu_dist > ex19.tmp 2>&1; \
	   if (${DIFF} output/ex19_superlu.out ex19.tmp) then true; \
	   else  printf "${PWD}\nPossible problem with ex19_superlu_dist, diffs above\n=========================================\n"; fi; \
	   ${RM} -f ex19.tmp

#-------------------------------------------------------------------------

runex12_p4est_convergence_test:
	-@echo "=================="
	-@echo "Refinement level 2"
	-@echo "=================="
	-@${MPIEXEC} -n 4 ./ex12 -quiet -run_type test -interpolate 1 -petscspace_degree 1 -simplex 0 -petscspace_poly_tensor -dm_plex_convert_type p4est -dm_forest_minimum_refinement 2 -dm_forest_initial_refinement 2 -dm_forest_maximum_refinement 4 -dm_p4est_refine_pattern hash
	-@echo "=================="
	-@echo "Refinement level 3"
	-@echo "=================="
	-@${MPIEXEC} -n 4 ./ex12 -quiet -run_type test -interpolate 1 -petscspace_degree 1 -simplex 0 -petscspace_poly_tensor -dm_plex_convert_type p4est -dm_forest_minimum_refinement 3 -dm_forest_initial_refinement 3 -dm_forest_maximum_refinement 5 -dm_p4est_refine_pattern hash
	-@echo "=================="
	-@echo "Refinement level 4"
	-@echo "=================="
	-@${MPIEXEC} -n 4 ./ex12 -quiet -run_type test -interpolate 1 -petscspace_degree 1 -simplex 0 -petscspace_poly_tensor -dm_plex_convert_type p4est -dm_forest_minimum_refinement 4 -dm_forest_initial_refinement 4 -dm_forest_maximum_refinement 6 -dm_p4est_refine_pattern hash
	-@echo "=================="
	-@echo "Refinement level 5"
	-@echo "=================="
	-@${MPIEXEC} -n 4 ./ex12 -quiet -run_type test -interpolate 1 -petscspace_degree 1 -simplex 0 -petscspace_poly_tensor -dm_plex_convert_type p4est -dm_forest_minimum_refinement 5 -dm_forest_initial_refinement 5 -dm_forest_maximum_refinement 7 -dm_p4est_refine_pattern hash

include ${PETSC_DIR}/lib/petsc/conf/test
