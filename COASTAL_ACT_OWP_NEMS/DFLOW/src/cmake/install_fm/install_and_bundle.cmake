set(BUILD_LIBRARIES
   ${CMAKE_INSTALL_PREFIX}/lib/libdflowfm.so
)
set(THIRDPARTY_x64_LIB_FOLDERS
  ${CMAKE_INSTALL_PREFIX}
  ${CMAKE_INSTALL_PREFIX}/bin
  ${CMAKE_INSTALL_PREFIX}/lib
  ${CMAKE_INSTALL_PREFIX}/share
)
function(gp_resolved_file_type_override resolved_file type_var)
  set(${type_var} local PARENT_SCOPE)
endfunction()

function(gp_item_default_embedded_path_override item default_embedded_path_var)
  if(item MATCHES ".so")
    set(path "@executable_path/../lib" PARENT_SCOPE)
    set( overridden 1 PARENT_SCOPE )
  endif()
endfunction(gp_item_default_embedded_path_override)

include(BundleUtilities)

set(BU_CHMOD_BUNDLE_ITEMS 1)

fixup_bundle("${CMAKE_INSTALL_PREFIX}/bin/dflowfm-cli" "${BUILD_LIBRARIES}" "${THIRDPARTY_x64_LIB_FOLDERS}")
fixup_bundle("${CMAKE_INSTALL_PREFIX}/bin/dfmoutput" "${BUILD_LIBRARIES}" "${THIRDPARTY_x64_LIB_FOLDERS}")

execute_process(COMMAND find "${CMAKE_INSTALL_PREFIX}/bin" -type f -exec bash -c "/opt/apps/patchelf/0.12/bin/patchelf --set-rpath '$ORIGIN/../lib' $1" _ {} \; -exec echo "patched rpath of: " {} \;)
execute_process(COMMAND find "${CMAKE_INSTALL_PREFIX}/lib" -type f -exec echo "patched rpath of: "  {} \; -exec bash -c "/opt/apps/patchelf/0.12/bin/patchelf --set-rpath '$ORIGIN' $1" _ {} \;)
execute_process(COMMAND find "${CMAKE_INSTALL_PREFIX}/share" -type f -exec echo "patched rpath of: " {} \; -exec bash -c "/opt/apps/patchelf/0.12/bin/patchelf --set-rpath '$ORIGIN/../lib:$ORIGIN' $1" _ {} \;)
execute_process(COMMAND find "${CMAKE_INSTALL_PREFIX}/lib" -type l -exec echo "remove destination of symlink:" {} \; -exec bash -c "cp --remove-destination $(readlink {}) {};"  {} \; WORKING_DIRECTORY "${CMAKE_INSTALL_PREFIX}/lib" )

file(RENAME ${CMAKE_INSTALL_PREFIX}/bin/dflowfm-cli ${CMAKE_INSTALL_PREFIX}/bin/dflowfm)

