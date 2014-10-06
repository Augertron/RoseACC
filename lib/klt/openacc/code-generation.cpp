
#include "KLT/utils.hpp"

#include "KLT/Core/loop-trees.hpp"
#include "KLT/Core/generator.hpp"
#include "KLT/Core/kernel.hpp"
#include "KLT/Core/data.hpp"
#include "KLT/Core/loop-tiler.hpp"
#include "KLT/Core/mfb-klt.hpp"

#include "KLT/OpenACC/language-opencl.hpp"
#include "KLT/OpenACC/runtime-openacc.hpp"
#include "KLT/OpenACC/dlx-openacc.hpp"
#include "KLT/OpenACC/mfb-acc-ocl.hpp"

#include "MFB/Sage/function-declaration.hpp"

#include "sage3basic.h"

namespace KLT {

template <>
unsigned long Generator<
  DLX::KLT_Annotation<DLX::OpenACC::language_t>,
  Language::OpenCL,
  Runtime::OpenACC,
  MFB::KLT_Driver
>::createFile() {
  unsigned long file_id = p_sage_driver.create(boost::filesystem::path(p_file_name));
  p_sage_driver.addPragmaDecl(file_id, "OPENCL EXTENSION cl_khr_fp64: enable");
  return file_id;
}

template <>
SgFunctionParameterList * createParameterList<
  DLX::KLT_Annotation<DLX::OpenACC::language_t>,
  Language::OpenCL,
  Runtime::OpenACC
>(
  Kernel<
    DLX::KLT_Annotation<DLX::OpenACC::language_t>,
    Language::OpenCL,
    Runtime::OpenACC
  > * kernel
) {
  const std::list<SgVariableSymbol *> & params = kernel->getArguments().parameters;
  const std::list<SgVariableSymbol *> & scalars = kernel->getArguments().scalars;
  const std::list<Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> > *> & datas = kernel->getArguments().datas;
  const std::list<Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> > *> & privates = kernel->getArguments().privates;

  std::list<SgVariableSymbol *>::const_iterator it_var_sym;
  std::list<Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> > *>::const_iterator it_data;

  SgFunctionParameterList * result = SageBuilder::buildFunctionParameterList();

  // ******************

  for (it_var_sym = params.begin(); it_var_sym != params.end(); it_var_sym++) {
    SgVariableSymbol * param_sym = *it_var_sym;
    std::string param_name = param_sym->get_name().getString();
    SgType * param_type =  param_sym->get_type();

    result->append_arg(SageBuilder::buildInitializedName("param_" + param_name, param_type, NULL));
  }

  // ******************

  for (it_var_sym = scalars.begin(); it_var_sym != scalars.end(); it_var_sym++) {
    SgVariableSymbol * scalar_sym = *it_var_sym;
    std::string scalar_name = scalar_sym->get_name().getString();
    SgType * scalar_type = scalar_sym->get_type();

    result->append_arg(SageBuilder::buildInitializedName("scalar_" + scalar_name, scalar_type, NULL));
  }

  // ******************

  for (it_data = datas.begin(); it_data != datas.end(); it_data++) {
    Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> > * data = *it_data;
    SgVariableSymbol * data_sym = data->getVariableSymbol();
    std::string data_name = data_sym->get_name().getString();

    SgType * base_type = data->getBaseType();
    SgType * field_type = SageBuilder::buildPointerType(base_type);

    SgModifierType * modif_type = SageBuilder::buildModifierType(field_type);
      modif_type->get_typeModifier().setOpenclGlobal();
    field_type = modif_type;

    result->append_arg(SageBuilder::buildInitializedName("data_" + data_name, field_type, NULL));

    if (data->isDistributed())
      result->append_arg(SageBuilder::buildInitializedName("offset_" + data_name, SageBuilder::buildLongType(), NULL));
  }

  // ******************

  for (it_data = privates.begin(); it_data != privates.end(); it_data++) {
    Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> > * data = *it_data;
    SgVariableSymbol * data_sym = data->getVariableSymbol();
    std::string data_name = data_sym->get_name().getString();

    SgType * base_type = data->getBaseType();
    SgType * field_type = SageBuilder::buildPointerType(base_type);

    SgModifierType * modif_type = SageBuilder::buildModifierType(field_type);
      modif_type->get_typeModifier().setOpenclLocal();
    field_type = modif_type;

    result->append_arg(SageBuilder::buildInitializedName("private_" + data_name, field_type, NULL));
  }

  // ******************

  SgModifierType * context_type = SageBuilder::buildModifierType(
    SageBuilder::buildPointerType(
      Runtime::OpenACC::runtime_device_context_symbol->get_declaration()->get_type()
    )
  );
  context_type->get_typeModifier().setOpenclConstant();

  result->append_arg(SageBuilder::buildInitializedName("context", context_type, NULL));
  
  return result;
}

template <>
SgStatement * generateStatement<
  DLX::KLT_Annotation<DLX::OpenACC::language_t>,
  Language::OpenCL,
  Runtime::OpenACC
> (
  LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::stmt_t * stmt,
  const Kernel<
    DLX::KLT_Annotation<DLX::OpenACC::language_t>, Language::OpenCL, Runtime::OpenACC
  >::local_symbol_maps_t & local_symbol_maps
) {
  SgStatement * result = SageInterface::copyStatement(stmt->statement);

  std::map<SgVariableSymbol *, SgVariableSymbol *>::const_iterator it_sym_to_local;
  std::map<Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> > *, SgVariableSymbol *>::const_iterator it_data_to_local;

  std::map<SgVariableSymbol *, SgVariableSymbol *> data_sym_to_local;
  std::map<SgVariableSymbol *, Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> > *> data_sym_to_data;

  for (it_data_to_local = local_symbol_maps.datas.begin(); it_data_to_local != local_symbol_maps.datas.end(); it_data_to_local++) {
    Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> > * data = it_data_to_local->first;
    SgVariableSymbol * data_sym = it_data_to_local->first->getVariableSymbol();
    SgVariableSymbol * local_sym = it_data_to_local->second;

    data_sym_to_local.insert(std::pair<SgVariableSymbol *, SgVariableSymbol *>(data_sym, local_sym));
    data_sym_to_data.insert(std::pair<SgVariableSymbol *, Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> > *>(data_sym, data));
  }

  for (it_data_to_local = local_symbol_maps.privates.begin(); it_data_to_local != local_symbol_maps.privates.end(); it_data_to_local++) {
    Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> > * data = it_data_to_local->first;
    SgVariableSymbol * data_sym = it_data_to_local->first->getVariableSymbol();
    SgVariableSymbol * local_sym = it_data_to_local->second;

    data_sym_to_local.insert(std::pair<SgVariableSymbol *, SgVariableSymbol *>(data_sym, local_sym));
    data_sym_to_data.insert(std::pair<SgVariableSymbol *, Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> > *>(data_sym, data));
  }

  std::vector<SgVarRefExp *> var_refs = SageInterface::querySubTree<SgVarRefExp>(result);
  std::vector<SgVarRefExp *>::const_iterator it_var_ref;

  for (it_var_ref = var_refs.begin(); it_var_ref != var_refs.end(); it_var_ref++) {
    SgVarRefExp * var_ref = *it_var_ref;
    SgVariableSymbol * var_sym = var_ref->get_symbol();

    std::map<SgVariableSymbol *, Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> > *>::const_iterator it_data_sym_to_data = data_sym_to_data.find(var_sym);
    if (it_data_sym_to_data == data_sym_to_data.end()) continue; // Not a variable reference to a Data

    Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> > * data = it_data_sym_to_data->second;

    SgPntrArrRefExp * arr_ref = isSgPntrArrRefExp(var_ref->get_parent());
    SgPntrArrRefExp * top_arr_ref = NULL;
    std::vector<SgExpression *> subscripts;
    while (arr_ref != NULL) {
      top_arr_ref = arr_ref;
      subscripts.push_back(arr_ref->get_rhs_operand_i());
      arr_ref = isSgPntrArrRefExp(arr_ref->get_parent());
    }
    assert(subscripts.size() == data->getSections().size());

    if (data->isDistributed()) {
      size_t distributed_dimension = data->getDistribution().distributed_dimension;
      assert(distributed_dimension < subscripts.size());

      std::map<Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> > *, SgVariableSymbol *>::const_iterator it_data_offset = local_symbol_maps.data_offsets.find(data);
      assert(it_data_offset != local_symbol_maps.data_offsets.end());

      subscripts[distributed_dimension] = SageBuilder::buildSubtractOp(subscripts[distributed_dimension], SageBuilder::buildVarRefExp(it_data_offset->second));
    }

    assert(top_arr_ref != NULL);
    std::vector<SgExpression *>::const_iterator it_subscript = subscripts.begin();
    SgExpression * subscript = SageInterface::copyExpression(*it_subscript);
    it_subscript++;
    size_t cnt = 1;
    while (it_subscript != subscripts.end()) {
      SgExpression * dim_size = SageInterface::copyExpression(data->getSections()[cnt].size);
      subscript = SageBuilder::buildMultiplyOp(subscript, dim_size);
      subscript = SageBuilder::buildAddOp(subscript, SageInterface::copyExpression(*it_subscript));
      cnt++; it_subscript++;
    }
    SageInterface::replaceExpression(top_arr_ref, SageBuilder::buildPntrArrRefExp(SageInterface::copyExpression(var_ref), subscript), true);
  }

  var_refs = SageInterface::querySubTree<SgVarRefExp>(result);
  for (it_var_ref = var_refs.begin(); it_var_ref != var_refs.end(); it_var_ref++) {
    SgVarRefExp * var_ref = *it_var_ref;
    SgVariableSymbol * var_sym = var_ref->get_symbol();

    SgVariableSymbol * local_sym = NULL;
    it_sym_to_local = local_symbol_maps.parameters.find(var_sym);
    if (it_sym_to_local != local_symbol_maps.parameters.end())
      local_sym = it_sym_to_local->second;

    it_sym_to_local = local_symbol_maps.scalars.find(var_sym);
    if (it_sym_to_local != local_symbol_maps.scalars.end()) {
      assert(local_sym == NULL);

      local_sym = it_sym_to_local->second;
    }

    it_sym_to_local = data_sym_to_local.find(var_sym);
    if (it_sym_to_local != data_sym_to_local.end()) {
      assert(local_sym == NULL);

      local_sym = it_sym_to_local->second;
    }

    it_sym_to_local = local_symbol_maps.iterators.find(var_sym);
    if (it_sym_to_local != local_symbol_maps.iterators.end()) {
      assert(local_sym == NULL);

      local_sym = it_sym_to_local->second;
    }

    if (local_sym != NULL)
      SageInterface::replaceExpression(var_ref, SageBuilder::buildVarRefExp(local_sym), true);
    else {
      /// \todo valid if sym can be found in local scope
    }
  }

  assert(result != NULL);

  return result;
}

SgExpression * translateConstExpression(
  SgExpression * expr,
  const std::map<SgVariableSymbol *, SgVariableSymbol *> & param_to_local,
  const std::map<SgVariableSymbol *, SgVariableSymbol *> & iter_to_local
) {
  SgExpression * result = SageInterface::copyExpression(expr);

  std::map<SgVariableSymbol *, SgVariableSymbol *>::const_iterator it_sym_to_local;

  if (isSgVarRefExp(result)) {
    // Catch an issue when reading looptree from file. In this case, 'expr' may not have a valid parent.
    // If 'expr' is a SgVarRefExp, it causes an assertion to fail in SageInterface::replaceExpression

    SgVarRefExp * var_ref = (SgVarRefExp *)result;

    SgVariableSymbol * var_sym = var_ref->get_symbol();

    SgVariableSymbol * local_sym = NULL;

    it_sym_to_local = param_to_local.find(var_sym);
    if (it_sym_to_local != param_to_local.end())
      local_sym = it_sym_to_local->second;

    it_sym_to_local = iter_to_local.find(var_sym);
    if (it_sym_to_local != iter_to_local.end()) {
      assert(local_sym == NULL); // implies VarRef to a variable symbol which is both parameter and iterator... It does not make sense!

      local_sym = it_sym_to_local->second;
    }

    assert(local_sym != NULL); // implies VarRef to an unknown variable symbol (neither parameter or iterator)

    return SageBuilder::buildVarRefExp(local_sym);
  }
  
  std::vector<SgVarRefExp *> var_refs = SageInterface::querySubTree<SgVarRefExp>(result);
  std::vector<SgVarRefExp *>::const_iterator it_var_ref;
  for (it_var_ref = var_refs.begin(); it_var_ref != var_refs.end(); it_var_ref++) {
    SgVarRefExp * var_ref = *it_var_ref;
    SgVariableSymbol * var_sym = var_ref->get_symbol();

    SgVariableSymbol * local_sym = NULL;

    it_sym_to_local = param_to_local.find(var_sym);
    if (it_sym_to_local != param_to_local.end())
      local_sym = it_sym_to_local->second;

    it_sym_to_local = iter_to_local.find(var_sym);
    if (it_sym_to_local != iter_to_local.end()) {
      assert(local_sym == NULL); // implies VarRef to a variable symbol which is both parameter and iterator... It does not make sense!

      local_sym = it_sym_to_local->second;
    }

    assert(local_sym != NULL); // implies VarRef to an unknown variable symbol (neither parameter or iterator)

    SageInterface::replaceExpression(var_ref, SageBuilder::buildVarRefExp(local_sym), true);
  }

  return result;
}

template <>
Runtime::OpenACC::exec_mode_t changeExecutionMode<
  DLX::KLT_Annotation<DLX::OpenACC::language_t>,
  Language::OpenCL,
  Runtime::OpenACC
> (
  const Runtime::OpenACC::exec_mode_t & exec_mode,
  const Runtime::OpenACC::exec_config_t & exec_cfg,
  LoopTiler<DLX::KLT_Annotation<DLX::OpenACC::language_t>, Language::OpenCL, Runtime::OpenACC>::loop_tiling_t & tiling
) {
  Runtime::OpenACC::exec_mode_t new_exec_mode = exec_mode;
  std::vector<Runtime::OpenACC::tile_desc_t>::iterator it_tile;
  for (it_tile = tiling.tiles.begin(); it_tile != tiling.tiles.end(); it_tile++) {
    switch (it_tile->kind) {
      case Runtime::OpenACC::e_static_tile:
      case Runtime::OpenACC::e_dynamic_tile:
        break;
      case Runtime::OpenACC::e_gang_tile:
        new_exec_mode |=  (1 << (it_tile->param.level));
        break;
      case Runtime::OpenACC::e_worker_tile:
        new_exec_mode |=  (1 << (it_tile->param.level + 3));
        break;
      case Runtime::OpenACC::e_vector_tile:
        new_exec_mode |=  (1 << 7);
        break;
      default:
        assert(false);
    }
  }

  std::cout << "changeExecutionMode(" << exec_mode << ") = " << new_exec_mode << std::endl;

  return new_exec_mode;
}

template <>
void generateSynchronizations<
  DLX::KLT_Annotation<DLX::OpenACC::language_t>,
  Language::OpenCL,
  Runtime::OpenACC
> (
  Runtime::OpenACC::exec_mode_t prev_exec_mode,
  Runtime::OpenACC::exec_mode_t next_exec_mode,
  const Runtime::OpenACC::exec_config_t & exec_cfg,
  SgScopeStatement * scope,
  const Kernel<
    DLX::KLT_Annotation<DLX::OpenACC::language_t>, Language::OpenCL, Runtime::OpenACC
  >::local_symbol_maps_t & local_symbol_maps
) {

  std::cout << "generateSynchronizations(" << prev_exec_mode << ", " << next_exec_mode << ")" << std::endl;

  if (!::KLT::Runtime::are_both_exec_mode<Runtime::OpenACC>(prev_exec_mode, next_exec_mode, OPENACC_EXEC_MODE_GANG_0)) {
    assert(
               ::KLT::Runtime::is_exec_mode<Runtime::OpenACC>(prev_exec_mode, OPENACC_EXEC_MODE_GANG_0)
           && !::KLT::Runtime::is_exec_mode<Runtime::OpenACC>(next_exec_mode, OPENACC_EXEC_MODE_GANG_0)
    );
    /// \todo gang[0] sync
  }
  if (!::KLT::Runtime::are_both_exec_mode<Runtime::OpenACC>(prev_exec_mode, next_exec_mode, OPENACC_EXEC_MODE_GANG_1)) {
    assert(
               ::KLT::Runtime::is_exec_mode<Runtime::OpenACC>(prev_exec_mode, OPENACC_EXEC_MODE_GANG_1)
           && !::KLT::Runtime::is_exec_mode<Runtime::OpenACC>(next_exec_mode, OPENACC_EXEC_MODE_GANG_1)
    );
    /// \todo gang[1] sync
  }
  if (!::KLT::Runtime::are_both_exec_mode<Runtime::OpenACC>(prev_exec_mode, next_exec_mode, OPENACC_EXEC_MODE_GANG_2)) {
    assert(
               ::KLT::Runtime::is_exec_mode<Runtime::OpenACC>(prev_exec_mode, OPENACC_EXEC_MODE_GANG_2)
           && !::KLT::Runtime::is_exec_mode<Runtime::OpenACC>(next_exec_mode, OPENACC_EXEC_MODE_GANG_2)
    );
    /// \todo gang[2] sync
  }
  if (!::KLT::Runtime::are_both_exec_mode<Runtime::OpenACC>(prev_exec_mode, next_exec_mode, OPENACC_EXEC_MODE_WORKER_0)) {
    assert(
               ::KLT::Runtime::is_exec_mode<Runtime::OpenACC>(prev_exec_mode, OPENACC_EXEC_MODE_WORKER_0)
           && !::KLT::Runtime::is_exec_mode<Runtime::OpenACC>(next_exec_mode, OPENACC_EXEC_MODE_WORKER_0)
    );

    SageInterface::appendStatement(SageBuilder::buildFunctionCallStmt(
      SageBuilder::buildFunctionRefExp(Runtime::OpenACC::runtime_device_function_symbols.barrier_workers_symbol),
      SageBuilder::buildExprListExp(SageBuilder::buildVarRefExp(local_symbol_maps.context))
    ), scope); /// acc_barrier_workers('ctx')
  }
  if (!::KLT::Runtime::are_both_exec_mode<Runtime::OpenACC>(prev_exec_mode, next_exec_mode, OPENACC_EXEC_MODE_WORKER_1)) {
    assert(
               ::KLT::Runtime::is_exec_mode<Runtime::OpenACC>(prev_exec_mode, OPENACC_EXEC_MODE_WORKER_1)
           && !::KLT::Runtime::is_exec_mode<Runtime::OpenACC>(next_exec_mode, OPENACC_EXEC_MODE_WORKER_1)
    );

    SageInterface::appendStatement(SageBuilder::buildFunctionCallStmt(
      SageBuilder::buildFunctionRefExp(Runtime::OpenACC::runtime_device_function_symbols.barrier_workers_symbol),
      SageBuilder::buildExprListExp(SageBuilder::buildVarRefExp(local_symbol_maps.context))
    ), scope); /// acc_barrier_workers('ctx')
  }
  if (!::KLT::Runtime::are_both_exec_mode<Runtime::OpenACC>(prev_exec_mode, next_exec_mode, OPENACC_EXEC_MODE_WORKER_2)) {
    assert(
               ::KLT::Runtime::is_exec_mode<Runtime::OpenACC>(prev_exec_mode, OPENACC_EXEC_MODE_WORKER_2)
           && !::KLT::Runtime::is_exec_mode<Runtime::OpenACC>(next_exec_mode, OPENACC_EXEC_MODE_WORKER_2)
    );

    SageInterface::appendStatement(SageBuilder::buildFunctionCallStmt(
      SageBuilder::buildFunctionRefExp(Runtime::OpenACC::runtime_device_function_symbols.barrier_workers_symbol),
      SageBuilder::buildExprListExp(SageBuilder::buildVarRefExp(local_symbol_maps.context))
    ), scope); /// acc_barrier_workers('ctx')
  }
  if (!::KLT::Runtime::are_both_exec_mode<Runtime::OpenACC>(prev_exec_mode, next_exec_mode, OPENACC_EXEC_MODE_VECTOR)) {
    assert(
               ::KLT::Runtime::is_exec_mode<Runtime::OpenACC>(prev_exec_mode, OPENACC_EXEC_MODE_VECTOR)
           && !::KLT::Runtime::is_exec_mode<Runtime::OpenACC>(next_exec_mode, OPENACC_EXEC_MODE_VECTOR)
    );
    /// \todo vector sync
  }
}

template <>
SgScopeStatement * generateExecModeGuards<
  DLX::KLT_Annotation<DLX::OpenACC::language_t>,
  Language::OpenCL,
  Runtime::OpenACC
> (
  Runtime::OpenACC::exec_mode_t exec_mode,
  const Runtime::OpenACC::exec_config_t & exec_cfg,
  SgScopeStatement * scope,
  const Kernel<
    DLX::KLT_Annotation<DLX::OpenACC::language_t>, Language::OpenCL, Runtime::OpenACC
  >::local_symbol_maps_t & local_symbol_maps
) {
  SgExpression * condition = NULL;

  if (exec_cfg.num_workers[0] != 1 && !::KLT::Runtime::is_exec_mode<Runtime::OpenACC>(exec_mode, OPENACC_EXEC_MODE_WORKER_0)) {
    // !worker[0] : only the workers that are master on level 0 (worker_id[0] == 0)
    SgExpression * new_cond = SageBuilder::buildFunctionCallExp(
      SageBuilder::buildFunctionRefExp(Runtime::OpenACC::runtime_device_function_symbols.is_master_worker_lvl_symbol),
      SageBuilder::buildExprListExp(SageBuilder::buildVarRefExp(local_symbol_maps.context), SageBuilder::buildIntVal(0))
    ); /// acc_is_master_worker_lvl(ctx, 0)
    condition = (condition == NULL) ? new_cond : SageBuilder::buildAndOp(condition, new_cond);
  }

  if (exec_cfg.num_workers[1] != 1 && !::KLT::Runtime::is_exec_mode<Runtime::OpenACC>(exec_mode, OPENACC_EXEC_MODE_WORKER_1)) {
    // !worker[1] : only the workers that are master on level 1 (worker_id[1] == 0)
    SgExpression * new_cond = SageBuilder::buildFunctionCallExp(
      SageBuilder::buildFunctionRefExp(Runtime::OpenACC::runtime_device_function_symbols.is_master_worker_lvl_symbol),
      SageBuilder::buildExprListExp(SageBuilder::buildVarRefExp(local_symbol_maps.context), SageBuilder::buildIntVal(1))
    ); /// acc_is_master_worker_lvl(ctx, 1)
    condition = (condition == NULL) ? new_cond : SageBuilder::buildAndOp(condition, new_cond);
  }

  if (exec_cfg.num_workers[2] != 1 && !::KLT::Runtime::is_exec_mode<Runtime::OpenACC>(exec_mode, OPENACC_EXEC_MODE_WORKER_2)) {
    // !worker[2] : only the workers that are master on level 2 (worker_id[2] == 0)
    SgExpression * new_cond = SageBuilder::buildFunctionCallExp(
      SageBuilder::buildFunctionRefExp(Runtime::OpenACC::runtime_device_function_symbols.is_master_worker_lvl_symbol),
      SageBuilder::buildExprListExp(SageBuilder::buildVarRefExp(local_symbol_maps.context), SageBuilder::buildIntVal(2))
    ); /// acc_is_master_worker_lvl(ctx, 2)
    condition = (condition == NULL) ? new_cond : SageBuilder::buildAndOp(condition, new_cond);
  }

  if (condition == NULL) return scope;
  else {
    SgScopeStatement * bb_scope = SageBuilder::buildBasicBlock();
    SageInterface::appendStatement(SageBuilder::buildIfStmt(condition, bb_scope, NULL), scope);
    return bb_scope;
  }
}

template <>
std::pair<SgStatement *, std::vector<SgScopeStatement *> > generateLoops<
  DLX::KLT_Annotation<DLX::OpenACC::language_t>,
  Language::OpenCL,
  Runtime::OpenACC
> (
  LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::loop_t * loop,
  size_t & loop_cnt,
  size_t & tile_cnt,
  std::map<LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::loop_t *, Runtime::OpenACC::a_loop> & loop_descriptors_map,
  LoopTiler<DLX::KLT_Annotation<DLX::OpenACC::language_t>, Language::OpenCL, Runtime::OpenACC>::loop_tiling_t & tiling,
  const Kernel<
    DLX::KLT_Annotation<DLX::OpenACC::language_t>, Language::OpenCL, Runtime::OpenACC
  >::local_symbol_maps_t & local_symbol_maps
) {
  if (tiling.tiles.empty()) {
    std::map<SgVariableSymbol *, SgVariableSymbol *>::const_iterator it_sym_to_local = local_symbol_maps.iterators.find(loop->iterator);
    assert(it_sym_to_local != local_symbol_maps.iterators.end());

    SgExprStatement * init_stmt = SageBuilder::buildExprStatement(SageBuilder::buildAssignOp(
                                    SageBuilder::buildVarRefExp(it_sym_to_local->second),
                                    translateConstExpression(loop->lower_bound, local_symbol_maps.iterators, local_symbol_maps.parameters)
                                  ));
    SgExprStatement * test_stmt  = SageBuilder::buildExprStatement(SageBuilder::buildLessOrEqualOp(
                                     SageBuilder::buildVarRefExp(it_sym_to_local->second),
                                     translateConstExpression(loop->upper_bound, local_symbol_maps.iterators, local_symbol_maps.parameters))
                                   );
    SgExpression * inc_expr = SageBuilder::buildPlusAssignOp(
                                SageBuilder::buildVarRefExp(it_sym_to_local->second),
                                translateConstExpression(loop->stride, local_symbol_maps.iterators, local_symbol_maps.parameters)
                              );
    SgBasicBlock * for_body = SageBuilder::buildBasicBlock();
    SgForStatement * for_stmt = SageBuilder::buildForStatement(init_stmt, test_stmt, inc_expr, for_body);

    return std::pair<SgStatement *, std::vector<SgScopeStatement *> >(for_stmt, std::vector<SgScopeStatement *>(1, for_body));
  }
  else {
    size_t loop_id;

    std::map<LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::loop_t *, Runtime::OpenACC::a_loop>::iterator it_loop_desc = loop_descriptors_map.find(loop);
    if (it_loop_desc == loop_descriptors_map.end()) {
      Runtime::OpenACC::a_loop loop_desc;
        loop_desc.id = loop_cnt++;
        loop_desc.lb = loop->lower_bound;
        loop_desc.ub = loop->upper_bound;
        loop_desc.stride = loop->stride;
        loop_desc.tiles = tiling.tiles;

      loop_id = loop_desc.id;

      it_loop_desc = loop_descriptors_map.insert(
          std::pair<
            LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::loop_t *,
            Runtime::OpenACC::a_loop
          >(loop, loop_desc)
      ).first;
    }
    else
      loop_id = it_loop_desc->second.id;

    std::vector<SgScopeStatement *>::iterator it_scopes;
    std::pair<SgStatement *, std::vector<SgScopeStatement *> > result(NULL, std::vector<SgScopeStatement *>(1, NULL));

    SgExpression * lower_bound = SageBuilder::buildFunctionCallExp(
      SageBuilder::buildFunctionRefExp(Runtime::OpenACC::runtime_device_function_symbols.loop_lower_symbol),
      SageBuilder::buildExprListExp(
        SageBuilder::buildVarRefExp(local_symbol_maps.context),
        SageBuilder::buildIntVal(loop_id)
      )
    ); /// acc_get_loop_lower('ctx', 'loop_id')

    std::vector<Runtime::OpenACC::tile_desc_t>::iterator it_tile;
    for (it_tile = it_loop_desc->second.tiles.begin(); it_tile != it_loop_desc->second.tiles.end(); it_tile++) {
      size_t tile_id = tile_cnt++;
      it_tile->id = tile_id;

      std::vector<SgScopeStatement *> scopes;
      for (it_scopes = result.second.begin(); it_scopes != result.second.end(); it_scopes++) {
        switch (it_tile->kind) {
          case Runtime::OpenACC::e_static_tile:
          case Runtime::OpenACC::e_dynamic_tile:
          {
            SgExpression * upper_bound;/*
            if (it_tile->kind == Runtime::OpenACC::e_static_tile) {
              upper_bound = SageBuilder::buildAddOp(
                SageInterface::copyExpression(lower_bound),
                SageBuilder::buildIntVal(it_tile->param.length)
              ); // 'lower_bound' + 'it_tile->param.length'
            }
            else if (it_tile->kind == Runtime::OpenACC::e_dynamic_tile) {*/
              upper_bound = SageBuilder::buildAddOp(
                SageInterface::copyExpression(lower_bound),
                SageBuilder::buildFunctionCallExp(
                  SageBuilder::buildFunctionRefExp(Runtime::OpenACC::runtime_device_function_symbols.tile_length_symbol),
                  SageBuilder::buildExprListExp(
                    SageBuilder::buildVarRefExp(local_symbol_maps.context),
                    SageBuilder::buildIntVal(tile_id)
                  )
                )
              ); // 'lower_bound' + acc_get_tile_length('ctx', 'tile_id')
            /*}
            else assert(false);*/

            SgExprStatement * init_stmt = SageBuilder::buildExprStatement(SageBuilder::buildAssignOp(
                                            SageBuilder::buildVarRefExp(it_tile->iterator_sym),
                                            SageInterface::copyExpression(lower_bound)
                                          )); // 'tile_iterator' = 'lower_bound'
            SgExprStatement * test_stmt  = SageBuilder::buildExprStatement(
                                             SageBuilder::buildLessThanOp(SageBuilder::buildVarRefExp(it_tile->iterator_sym), upper_bound)
                                           ); // 'tile_iterator' < 'upper_bound'
            SgExpression * inc_expr = SageBuilder::buildPlusAssignOp(
                                        SageBuilder::buildVarRefExp(it_tile->iterator_sym),
                                        SageBuilder::buildFunctionCallExp(
                                          SageBuilder::buildFunctionRefExp(Runtime::OpenACC::runtime_device_function_symbols.tile_stride_symbol),
                                          SageBuilder::buildExprListExp(
                                            SageBuilder::buildVarRefExp(local_symbol_maps.context),
                                            SageBuilder::buildIntVal(tile_id)
                                          )
                                        )
                                      ); // 'tile_iterator' += acc_get_tile_stride('ctx', 'tile_id')

            SgBasicBlock * for_body = SageBuilder::buildBasicBlock();
            SgForStatement * for_stmt = SageBuilder::buildForStatement(init_stmt, test_stmt, inc_expr, for_body);

            if (*it_scopes == NULL) {
              assert(result.first == NULL && result.second.size() == 1);
              result.first = for_stmt;
              scopes.push_back(for_body);
            }
            else {
              assert(result.first != NULL);

              SageInterface::appendStatement(for_stmt, *it_scopes);
              scopes.push_back(for_body);
            }
          
            break;
          }
          case Runtime::OpenACC::e_gang_tile:
          case Runtime::OpenACC::e_worker_tile:
          {
            SgFunctionSymbol * function_symbol;
            if (it_tile->kind == Runtime::OpenACC::e_gang_tile)
              function_symbol = Runtime::OpenACC::runtime_device_function_symbols.gang_iter_symbol;
            else if (it_tile->kind == Runtime::OpenACC::e_worker_tile)
              function_symbol = Runtime::OpenACC::runtime_device_function_symbols.worker_iter_symbol;
            else assert(false);

            SgExprStatement * set_it = SageBuilder::buildExprStatement(SageBuilder::buildAssignOp(
                                         SageBuilder::buildVarRefExp(it_tile->iterator_sym),
                                         SageBuilder::buildFunctionCallExp(
                                           SageBuilder::buildFunctionRefExp(function_symbol),
                                           SageBuilder::buildExprListExp(
                                             SageBuilder::buildVarRefExp(local_symbol_maps.context),
                                             SageBuilder::buildIntVal(tile_id),
                                             SageInterface::copyExpression(lower_bound),
                                             SageBuilder::buildIntVal(it_tile->param.level)
                                           )
                                         )
                                       )); // 'tile_iterator' = acc_gang_iteration('ctx', 'tile_id', 'lower_bound', 'it_tile->param.lvl')

            if (*it_scopes == NULL) {
              assert(result.first == NULL && result.second.size() == 1);
              *it_scopes = SageBuilder::buildBasicBlock();
              result.first = *it_scopes;
            }
            SageInterface::appendStatement(set_it, *it_scopes);

            break;
          }
          case Runtime::OpenACC::e_vector_tile:
            assert(false);
          default:
            assert(false);
        }
      }

      if (!scopes.empty())
        result.second = scopes;

      delete lower_bound;
      lower_bound = SageBuilder::buildVarRefExp(it_tile->iterator_sym);
    }
    delete lower_bound;

    return result;
  }

  assert(false);
}

}

