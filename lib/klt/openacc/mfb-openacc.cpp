
#include "KLT/utils.hpp"

#include "KLT/Core/loop-trees.hpp"
#include "KLT/Core/generator.hpp"
#include "KLT/Core/kernel.hpp"
#include "KLT/Core/data.hpp"
#include "KLT/Core/loop-tiler.hpp"
#include "KLT/Core/mfb-klt.hpp"

#include "KLT/OpenACC/dlx-openacc.hpp"
#include "KLT/OpenACC/language-opencl.hpp"
#include "KLT/OpenACC/runtime-openacc.hpp"
#include "KLT/OpenACC/mfb-acc-ocl.hpp"

#include "MFB/Sage/function-declaration.hpp"

#include "sage3basic.h"

namespace MFB {

KLT<Kernel_OpenCL_OpenACC>::object_desc_t::object_desc_t(
  unsigned id_,
  Kernel_OpenCL_OpenACC * kernel_,
  unsigned long file_id_
) :
  id(id_),
  kernel(kernel_),
  file_id(file_id_),
  tiling()
{}

SgVariableSymbol * getExistingSymbolOrBuildDecl(
  const std::string & name,
  SgType * type,
  SgScopeStatement * scope
) {
  SgVariableSymbol * sym = scope->lookup_variable_symbol(name);
  if (sym == NULL) {
    SgVariableDeclaration * decl = SageBuilder::buildVariableDeclaration(name, type, NULL, scope);
    SageInterface::appendStatement(decl, scope);
    sym = scope->lookup_variable_symbol(name);
  }
  assert(sym != NULL);
  return sym;
}

template <>
SgBasicBlock * createLocalDeclarations<
  DLX::KLT_Annotation<DLX::OpenACC::language_t>,
  ::KLT::Language::OpenCL,
  ::KLT::Runtime::OpenACC
>(
  Driver<Sage> & driver,
  SgFunctionDefinition * kernel_defn,
  ::KLT::Kernel<
    DLX::KLT_Annotation<DLX::OpenACC::language_t>,
    ::KLT::Language::OpenCL,
    ::KLT::Runtime::OpenACC
  >::local_symbol_maps_t & local_symbol_maps,
  const ::KLT::Kernel<
    DLX::KLT_Annotation<DLX::OpenACC::language_t>,
    ::KLT::Language::OpenCL,
    ::KLT::Runtime::OpenACC
  >::arguments_t & arguments,
  const std::map<
    ::KLT::LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::loop_t *,
    ::KLT::LoopTiler<DLX::KLT_Annotation<DLX::OpenACC::language_t>, ::KLT::Language::OpenCL, ::KLT::Runtime::OpenACC>::loop_tiling_t *
  > & loop_tiling
) {
  std::list<SgVariableSymbol *>::const_iterator it_var_sym;
  std::list< ::KLT::Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> > *>::const_iterator it_data;

  std::map<SgVariableSymbol *, SgVariableSymbol *>::const_iterator   it_param_to_field;
  std::map<SgVariableSymbol *, SgVariableSymbol *>::const_iterator   it_scalar_to_field;
  std::map< ::KLT::Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> > *, SgVariableSymbol *>::const_iterator it_data_to_field;

  std::map<
    ::KLT::LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::loop_t *,
    ::KLT::LoopTiler<DLX::KLT_Annotation<DLX::OpenACC::language_t>, ::KLT::Language::OpenCL, ::KLT::Runtime::OpenACC>::loop_tiling_t *
  >::const_iterator it_loop_tiling;
  
  // * Definition *

  SgBasicBlock * kernel_body = kernel_defn->get_body();
  assert(kernel_body != NULL);

  // * Lookup parameter symbols *

  for (it_var_sym = arguments.parameters.begin(); it_var_sym != arguments.parameters.end(); it_var_sym++) {
    SgVariableSymbol * param_sym = *it_var_sym;
    std::string param_name = param_sym->get_name().getString();

    SgVariableSymbol * arg_sym = kernel_defn->lookup_variable_symbol("param_" + param_name);
    assert(arg_sym != NULL);

    local_symbol_maps.parameters.insert(std::pair<SgVariableSymbol *, SgVariableSymbol *>(param_sym, arg_sym));
  }

  // * Lookup scalar symbols *

  for (it_var_sym = arguments.scalars.begin(); it_var_sym != arguments.scalars.end(); it_var_sym++) {
    SgVariableSymbol * scalar_sym = *it_var_sym;
    std::string scalar_name = scalar_sym->get_name().getString();

    SgVariableSymbol * arg_sym = kernel_defn->lookup_variable_symbol("scalar_" + scalar_name);
    assert(arg_sym != NULL);

    local_symbol_maps.scalars.insert(std::pair<SgVariableSymbol *, SgVariableSymbol *>(scalar_sym, arg_sym));
  }

  // * Lookup data symbols *

  for (it_data = arguments.datas.begin(); it_data != arguments.datas.end(); it_data++) {
    ::KLT::Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> > * data = *it_data;
    SgVariableSymbol * data_sym = data->getVariableSymbol();;
    std::string data_name = data_sym->get_name().getString();

    SgVariableSymbol * arg_sym = kernel_defn->lookup_variable_symbol("data_" + data_name);
    assert(arg_sym != NULL);

    local_symbol_maps.datas.insert(
      std::pair< ::KLT::Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> > *, SgVariableSymbol *>(data, arg_sym)
    );
  }

  // * Create iterator *

  for (it_loop_tiling = loop_tiling.begin(); it_loop_tiling != loop_tiling.end(); it_loop_tiling++) {
    ::KLT::LoopTrees<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::loop_t * loop = it_loop_tiling->first;
    ::KLT::LoopTiler<DLX::KLT_Annotation<DLX::OpenACC::language_t>, ::KLT::Language::OpenCL, ::KLT::Runtime::OpenACC>::loop_tiling_t * tiling = it_loop_tiling->second;

    SgVariableSymbol * iter_sym = loop->iterator;
    std::string iter_name = iter_sym->get_name().getString();
    SgType * iter_type = iter_sym->get_type();

    SgVariableSymbol * local_sym = NULL;
    if (tiling->tiles.empty()) {
      local_sym = getExistingSymbolOrBuildDecl("local_it_" + iter_name, iter_type, kernel_body);
    }
    else {
      size_t tile_cnt = 0;
      std::vector< ::KLT::Runtime::OpenACC::tile_desc_t>::iterator it_tile;
      for (it_tile = tiling->tiles.begin(); it_tile != tiling->tiles.end(); it_tile++) {
        std::ostringstream oss;
        oss << "local_it_" << iter_name << "_tile_" << tile_cnt++;
        local_sym = getExistingSymbolOrBuildDecl(oss.str(), iter_type, kernel_body);
        it_tile->iterator_sym = local_sym;
      }
      assert(local_sym != NULL);
    }
    local_symbol_maps.iterators.insert(std::pair<SgVariableSymbol *, SgVariableSymbol *>(iter_sym, local_sym));
  }

  SgVariableSymbol * context_sym = kernel_defn->lookup_variable_symbol("context");
  assert(context_sym != NULL);

  local_symbol_maps.context = context_sym;

  return kernel_body;

}

}

