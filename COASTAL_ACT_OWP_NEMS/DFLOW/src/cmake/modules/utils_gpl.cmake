# File to define the utils_gpl components and their corresponding tests
set(utils_gpl_path utils_gpl)

# Trachytopes
set(trachytopes_path "${utils_gpl_path}/trachytopes/packages")
set(trachytopes_kernel_module ${trachytopes_path}/trachytopes_kernel)
set(trachytopes_io_module ${trachytopes_path}/trachytopes_io)

# Flow 1D
set(flow1d_module_path "${utils_gpl_path}/flow1d/packages")
set(flow1d_core_module ${flow1d_module_path}/flow1d_core)
set(flow1d_io_module ${flow1d_module_path}/flow1d_io)
set(flow1d_module ${flow1d_module_path}/flow1d)

# Morphology
set(morphology_module_path "${utils_gpl_path}/morphology/packages")
set(morphology_plugins_c_module "${morphology_module_path}/morphology_plugins_c")
set(morphology_data_module "${morphology_module_path}/morphology_data")
set(morphology_kernel_module "${morphology_module_path}/morphology_kernel")
set(morphology_io_module "${morphology_module_path}/morphology_io")

# Hydrology
set(hydrology_module_path "${utils_gpl_path}/dhydrology/packages")
set(hydrology_kernel_module "${hydrology_module_path}/dhydrology_kernel")
set(hydrology_kernel_io_module "${hydrology_module_path}/dhydrology_io")
