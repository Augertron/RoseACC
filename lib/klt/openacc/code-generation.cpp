
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
  unsigned long file_id = p_sage_driver.add(boost::filesystem::path(p_file_name));
  p_sage_driver.attachArbitraryText(file_id, "#pragma OPENCL EXTENSION cl_khr_fp64: enable");
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
  unsigned long data_type_modifer_ = SgTypeModifier::e_ocl_global__;

  std::list<SgVariableSymbol *>::const_iterator it_var_sym;
  std::list<Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> > *>::const_iterator it_data;

  SgTypeModifier::type_modifier_enum data_type_modifer = (SgTypeModifier::type_modifier_enum)data_type_modifer_;

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

    switch (data_type_modifer) {
      case SgTypeModifier::e_default:
        break;
      case SgTypeModifier::e_ocl_global__:
      {
        SgModifierType * modif_type = SageBuilder::buildModifierType(field_type);
        modif_type->get_typeModifier().setOpenclGlobal();
        field_type = modif_type;
        break;
      }
      default:
        assert(false);
    }

    result->append_arg(SageBuilder::buildInitializedName("data_" + data_name, field_type, NULL));
  }

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
  >::local_symbol_maps_t & local_symbol_maps,
  bool flatten_array_ref
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

  std::vector<SgVarRefExp *> var_refs = SageInterface::querySubTree<SgVarRefExp>(result);
  std::vector<SgVarRefExp *>::const_iterator it_var_ref;

  if (flatten_array_ref) {
    for (it_var_ref = var_refs.begin(); it_var_ref != var_refs.end(); it_var_ref++) {
      SgVarRefExp * var_ref = *it_var_ref;
      SgVariableSymbol * var_sym = var_ref->get_symbol();

      std::map<SgVariableSymbol *, Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> > *>::const_iterator it_data_sym_to_data = data_sym_to_data.find(var_sym);
      if (it_data_sym_to_data == data_sym_to_data.end()) continue; // Not a variable reference to a Data

      Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> > * data = it_data_sym_to_data->second;

      if (data->getSections().size() <= 1) continue; // No need for flattening

      SgPntrArrRefExp * arr_ref = isSgPntrArrRefExp(var_ref->get_parent());
      SgPntrArrRefExp * top_arr_ref = NULL;
      std::list<SgExpression *> subscripts;
      while (arr_ref != NULL) {
        top_arr_ref = arr_ref;
        subscripts.push_back(arr_ref->get_rhs_operand_i());
        arr_ref = isSgPntrArrRefExp(arr_ref->get_parent());
      }
      assert(top_arr_ref != NULL);
      assert(subscripts.size() == data->getSections().size());

      std::list<SgExpression *>::const_iterator it_subscript;
      SgExpression * subscript = SageInterface::copyExpression(subscripts.front());
      subscripts.pop_front();
      unsigned int cnt = 0;
      for (it_subscript = subscripts.begin(); it_subscript != subscripts.end(); it_subscript++) {
        SgExpression * dim_size = SageInterface::copyExpression(data->getSections()[++cnt].size);
        subscript = SageBuilder::buildMultiplyOp(subscript, dim_size);
        subscript = SageBuilder::buildAddOp(subscript, SageInterface::copyExpression(*it_subscript));
      }

      SageInterface::replaceExpression(top_arr_ref, SageBuilder::buildPntrArrRefExp(SageInterface::copyExpression(var_ref), subscript), true);
    }
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
                                    SageInterface::copyExpression(loop->lower_bound)
                                  ));
    SgExprStatement * test_stmt  = SageBuilder::buildExprStatement(SageBuilder::buildLessThanOp(
                                     SageBuilder::buildVarRefExp(it_sym_to_local->second),
                                     SageInterface::copyExpression(loop->upper_bound))
                                   );
    SgExpression * inc_expr = SageBuilder::buildPlusAssignOp(
                                SageBuilder::buildVarRefExp(it_sym_to_local->second),
                                SageInterface::copyExpression(loop->stride)
                              );
    SgBasicBlock * for_body = SageBuilder::buildBasicBlock();
    SgForStatement * for_stmt = SageBuilder::buildForStatement(init_stmt, test_stmt, inc_expr, for_body);

    return std::pair<SgStatement *, std::vector<SgScopeStatement *> >(for_stmt, std::vector<SgScopeStatement *>(1, for_body));
  }
  else {
    size_t loop_id;

    std::map<LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::loop_t *, Runtime::OpenACC::a_loop>::const_iterator it_loop_desc = loop_descriptors_map.find(loop);
    if (it_loop_desc == loop_descriptors_map.end()) {
      Runtime::OpenACC::a_loop loop_desc;
        loop_desc.id = loop_cnt++;
        loop_desc.lb = loop->lower_bound;
        loop_desc.ub = loop->upper_bound;
        loop_desc.stride = loop->stride;
        loop_desc.tiles = tiling.tiles;

      loop_id = loop_desc.id;

      loop_descriptors_map.insert(
          std::pair<
            LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::loop_t *,
            Runtime::OpenACC::a_loop
          >(loop, loop_desc)
      );
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
    for (it_tile = tiling.tiles.begin(); it_tile != tiling.tiles.end(); it_tile++) {
      size_t tile_id = tile_cnt++;
      it_tile->id = tile_id;

      std::vector<SgScopeStatement *> scopes;
      for (it_scopes = result.second.begin(); it_scopes != result.second.end(); it_scopes++) {
        switch (it_tile->kind) {
          case Runtime::OpenACC::e_static_tile:
          case Runtime::OpenACC::e_dynamic_tile:
          {
            SgExpression * upper_bound;
            if (it_tile->kind == Runtime::OpenACC::e_static_tile) {
              upper_bound = SageBuilder::buildAddOp(
                SageInterface::copyExpression(lower_bound),
                SageBuilder::buildIntVal(it_tile->param.length)
              ); // 'lower_bound' + 'it_tile->param.length'
            }
            else if (it_tile->kind == Runtime::OpenACC::e_dynamic_tile) {
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
            }
            else assert(false);

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

