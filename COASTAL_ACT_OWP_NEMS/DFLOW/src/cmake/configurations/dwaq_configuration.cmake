project(dwaq)

#
# WAQ
#=============
if(NOT TARGET waq_plugin_wasteload)
    add_subdirectory(${checkout_src_root}/${waq_plugin_wasteload_module} waq_plugin_wasteload)
endif()

if(NOT TARGET waq_utils_c)
    add_subdirectory(${checkout_src_root}/${waq_utils_c_module} waq_utils_c)
endif()

if(NOT TARGET waq_utils_f)
    add_subdirectory(${checkout_src_root}/${waq_utils_f_module} waq_utils_f)
endif()

if(NOT TARGET waq_process)
    add_subdirectory(${checkout_src_root}/${waq_process_module} waq_process)
endif()

if(NOT TARGET waq_kernel)
    add_subdirectory(${checkout_src_root}/${waq_kernel_module} waq_kernel)
endif()

if(NOT TARGET waq_io)
    add_subdirectory(${checkout_src_root}/${waq_io_module} waq_io)
endif()

if(NOT TARGET delwaq)
    add_subdirectory(${checkout_src_root}/${delwaq_module} delwaq)
endif()

if(NOT TARGET delwaq1)
    add_subdirectory(${checkout_src_root}/${delwaq1_module} delwaq1)
endif()

if(NOT TARGET delwaq2)
    add_subdirectory(${checkout_src_root}/${delwaq2_module} delwaq2)
endif()

if(NOT TARGET delwaq_lib_examples)
    add_subdirectory(${checkout_src_root}/${delwaq_lib_examples_module} delwaq_lib_examples)
endif()

#
# WAQ Tools
#=============
# Waqpb
if(NOT TARGET waqpb_export)
    add_subdirectory(${checkout_src_root}/${waqpb_export_module} waqpb_export)
endif()
if(NOT TARGET waqpb_import)
    add_subdirectory(${checkout_src_root}/${waqpb_import_module} waqpb_import)
endif()
if(NOT TARGET waqpb_lib)
    add_subdirectory(${checkout_src_root}/${waqpb_lib_module} waqpb_lib)
endif()
if(NOT TARGET waqmerge_version_number)
    add_subdirectory(${checkout_src_root}/${waqmerge_version_number_module} waqmerge_version_number)
endif()
if(NOT TARGET waqmerge)
    add_subdirectory(${checkout_src_root}/${waqmerge_module} waqmerge)
endif()
if(NOT TARGET ddcouple_version_number)
    add_subdirectory(${checkout_src_root}/${ddcouple_version_number_module} ddcouple_version_number)
endif()
if(NOT TARGET ddcouple)
    add_subdirectory(${checkout_src_root}/${ddcouple_module} ddcouple)
endif()
if(NOT TARGET agrhyd)
    add_subdirectory(${checkout_src_root}/${agrhyd_module} agrhyd)
endif()



#
# PART
#=============
if(NOT TARGET part_data_f)
    add_subdirectory(${checkout_src_root}/${part_data_f_module} part_data_f)
endif()

if(NOT TARGET part_utils_f)
    add_subdirectory(${checkout_src_root}/${part_utils_f_module} part_utils_f)
endif()

if(NOT TARGET part_io_f)
    add_subdirectory(${checkout_src_root}/${part_io_f_module} part_io_f)
endif()

if(NOT TARGET part_kernel_f)
    add_subdirectory(${checkout_src_root}/${part_kernel_f_module} part_kernel_f)
endif()

if(NOT TARGET delpar)
    add_subdirectory(${checkout_src_root}/${delpar_module} delpar)
endif()

#
# Utils
#=============
# Deltares_common
if(NOT TARGET deltares_common) 
    add_subdirectory(${checkout_src_root}/${deltares_common_module} deltares_common)
endif()
if(NOT TARGET deltares_common_c)
    add_subdirectory(${checkout_src_root}/${deltares_common_c_module} deltares_common_c)
endif()

# netcdf
if(WIN32)
    if(NOT TARGET netcdff)
        add_subdirectory(${checkout_src_root}/${netcdf_module} netcdff)
    endif()
endif()

# io_netcdf
if(NOT TARGET io_netcdf)
    add_subdirectory(${checkout_src_root}/${io_netcdf_module} io_netcdf)
endif()

# Nefis
if(NOT TARGET nefis)
    add_subdirectory(${checkout_src_root}/${nefis_module} nefis)
endif()

# delftio
if(NOT TARGET delftio_shm)
    add_subdirectory(${checkout_src_root}/${delftio_shm_module} delftio_shm)
endif()
if(NOT TARGET delftio)
    add_subdirectory(${checkout_src_root}/${delftio_module} delftio)
endif()

# esmfsm
if(NOT TARGET esmfsm_version_number)
    add_subdirectory(${checkout_src_root}/${esmfsm_version_number_module} esmfsm_version_number)
endif()
if(NOT TARGET esmfsm_c)
    add_subdirectory(${checkout_src_root}/${esmfsm_c_module} esmfsm_c)
endif()
if(NOT TARGET esmfsm)
    add_subdirectory(${checkout_src_root}/${esmfsm_module} esmfsm)
endif()

# io_netcdf
if(NOT TARGET io_hyd)
    add_subdirectory(${checkout_src_root}/${io_hyd_module} io_hyd)
endif()


#
# Linux installation
#=============
if(UNIX)
    add_subdirectory(${checkout_src_root}/${install_waq_module} install_waq)
endif()
