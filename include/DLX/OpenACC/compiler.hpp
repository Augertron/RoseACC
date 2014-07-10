
#ifndef __DLX_COMPILER_OPENACC_HPP__
#define __DLX_COMPILER_OPENACC_HPP__

#include "openacc_spec.hpp"

#include "MDCG/OpenACC/model.hpp"

namespace DLX {

namespace OpenACC {

struct compiler_modules_t {
  MFB::KLT_Driver driver;

  MDCG::ModelBuilder model_builder;
  MDCG::CodeGenerator codegen;
  KLT::Generator<Annotation, Language, Runtime, MFB::KLT_Driver> generator;

  KLT::CG_Config<Annotation, Language, Runtime> cg_config;

  unsigned libopenacc_model;
  unsigned host_data_file_id;
  std::string ocl_kernels_file;
  std::string versions_db_file;

  MDCG::Model::class_t compiler_data_class;

  MDCG::OpenACC::CompilerData::input_t comp_data;

  struct libopenacc_api_t {
    SgFunctionSymbol * push_data_environment;
    SgFunctionSymbol * pop_data_environment;

    SgFunctionSymbol * region_build;
    SgFunctionSymbol * region_execute;
    SgFunctionSymbol * region_free;

    SgFunctionSymbol * get_device_idx;

    SgFunctionSymbol * copyin;
    SgFunctionSymbol * copyin_region;
    SgFunctionSymbol * copyout;
    SgFunctionSymbol * copyout_region;
    SgFunctionSymbol * create;
    SgFunctionSymbol * create_region;
    SgFunctionSymbol * present;
    SgFunctionSymbol * present_region;
    SgFunctionSymbol * present_or_copyin;
    SgFunctionSymbol * present_or_copyin_region;
    SgFunctionSymbol * present_or_copyout;
    SgFunctionSymbol * present_or_copyout_region;
    SgFunctionSymbol * present_or_create;
    SgFunctionSymbol * present_or_create_region;

    MDCG::Model::class_t region_class;

    MDCG::Model::field_t region_param_ptrs;
    MDCG::Model::field_t region_scalar_ptrs;
    MDCG::Model::field_t region_data;
    MDCG::Model::field_t region_loops;
    MDCG::Model::field_t region_distributed_data;
    MDCG::Model::field_t region_devices;

    MDCG::Model::field_t region_data_ptr;
    MDCG::Model::field_t region_data_nbr_elements;
    MDCG::Model::field_t region_data_element_size;
    MDCG::Model::field_t region_data_dominant_dimension;
    MDCG::Model::field_t region_data_nbr_elements_dominant_dimension;

    MDCG::Model::field_t region_loops_lower;
    MDCG::Model::field_t region_loops_upper;
    MDCG::Model::field_t region_loops_stride;

    MDCG::Model::field_t region_devices_device_idx;
    MDCG::Model::field_t region_devices_num_gangs;
    MDCG::Model::field_t region_devices_num_workers;
    MDCG::Model::field_t region_devices_vector_length;
    
  } libopenacc_api;

  void loadOpenaccPrivateAPI();

  compiler_modules_t(
    SgProject * project,
    const std::string & ocl_kernels_file_,
    const std::string & kernels_desc_file_,
    const std::string & versions_db_file_,
    const std::string & openacc_inc_path,
    const std::string & openacc_lib_path,
    const std::string & kernels_dir
  );
};

}

}

#endif /* __DLX_COMPILER_OPENACC_HPP__ */

