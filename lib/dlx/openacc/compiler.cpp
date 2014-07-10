/*!
 * 
 * \file lib/openacc/compiler.cpp
 *
 * \author Tristan Vanderbruggen
 *
 */

#include "sage3basic.h"

#include "MFB/Sage/driver.hpp"
#include "MFB/Sage/class-declaration.hpp"

#include "DLX/Core/compiler.hpp"
#include "DLX/OpenACC/language.hpp"
#include "DLX/OpenACC/compiler.hpp"

#include "KLT/Core/loop-trees.hpp"
#include "KLT/Core/loop-tiler.hpp"

#include "DLX/Core/parser.hpp"

namespace DLX {

namespace OpenACC {

/*!
 * \addtogroup grp_dlx_openacc_compiler
 * @{
 */

compiler_modules_t::compiler_modules_t(
  SgProject * project,
  const std::string & ocl_kernels_file_,
  const std::string & kernels_desc_file_,
  const std::string & versions_db_file_,
  const std::string & openacc_inc_path,
  const std::string & openacc_lib_path,
  const std::string & kernels_dir
) :
  driver(project),
  model_builder(driver),
  codegen(driver),
  generator(driver, ocl_kernels_file_),
  cg_config(
    new KLT::LoopMapper<Annotation, Language, Runtime>(),
    new KLT::LoopTiler<Annotation, Language, Runtime>(),
    new KLT::DataFlow<Annotation, Language, Runtime>()
  ),
  libopenacc_model(0),
  host_data_file_id(0),
  ocl_kernels_file(ocl_kernels_file_),
  versions_db_file(versions_db_file_),
  compiler_data_class(NULL),
  comp_data(),
  libopenacc_api()
{
  host_data_file_id = driver.create(boost::filesystem::path(kernels_desc_file_));
    driver.setUnparsedFile(host_data_file_id);
    driver.setCompiledFile(host_data_file_id);

  libopenacc_model = MDCG::OpenACC::readOpenaccModel(model_builder, openacc_inc_path);

  // Get base class for host data generation
  std::set<MDCG::Model::class_t> classes;
  model_builder.get(libopenacc_model).lookup<MDCG::Model::class_t>("acc_compiler_data_t_", classes);
  assert(classes.size() == 1);
  compiler_data_class = *(classes.begin());

  comp_data.openacc_inc_path = SageBuilder::buildStringVal(openacc_inc_path);
  comp_data.openacc_lib_path = SageBuilder::buildStringVal(openacc_lib_path);
  comp_data.kernels_dir = SageBuilder::buildStringVal(kernels_dir);

  // Load libOpenACC API for KLT
  KLT::Runtime::OpenACC::loadAPI(model_builder.get(libopenacc_model));

  // Load libOpenACC API for transformation
  loadOpenaccPrivateAPI();
}

void compiler_modules_t::loadOpenaccPrivateAPI() {
  const MDCG::Model::model_t & model = model_builder.get(libopenacc_model);
  MDCG::Model::function_t func;

  func = model.lookup<MDCG::Model::function_t>("acc_push_data_environment");
  libopenacc_api.push_data_environment = func->node->symbol;
  assert(libopenacc_api.push_data_environment != NULL);

  func = model.lookup<MDCG::Model::function_t>("acc_pop_data_environment");
  libopenacc_api.pop_data_environment = func->node->symbol;
  assert(libopenacc_api.pop_data_environment != NULL);

  func = model.lookup<MDCG::Model::function_t>("acc_build_region");
  libopenacc_api.build_region = func->node->symbol;
  assert(libopenacc_api.build_region != NULL);

  func = model.lookup<MDCG::Model::function_t>("acc_region_execute");
  libopenacc_api.region_execute = func->node->symbol;
  assert(libopenacc_api.region_execute != NULL);

  func = model.lookup<MDCG::Model::function_t>("acc_get_device_idx");
  libopenacc_api.get_device_idx = func->node->symbol;
  assert(libopenacc_api.get_device_idx != NULL);

  func = model.lookup<MDCG::Model::function_t>("acc_copyin");
  libopenacc_api.copyin = func->node->symbol;
  assert(libopenacc_api.copyin != NULL);
  func = model.lookup<MDCG::Model::function_t>("acc_copyin_regions_");
  libopenacc_api.copyin_region = func->node->symbol;
  assert(libopenacc_api.copyin_region != NULL);

  func = model.lookup<MDCG::Model::function_t>("acc_copyout");
  libopenacc_api.copyout = func->node->symbol;
  assert(libopenacc_api.copyout != NULL);
  func = model.lookup<MDCG::Model::function_t>("acc_copyout_regions_");
  libopenacc_api.copyout_region = func->node->symbol;
  assert(libopenacc_api.copyout_region != NULL);

  func = model.lookup<MDCG::Model::function_t>("acc_create");
  libopenacc_api.create = func->node->symbol;
  assert(libopenacc_api.create != NULL);
  func = model.lookup<MDCG::Model::function_t>("acc_create_regions_");
  libopenacc_api.create_region = func->node->symbol;
  assert(libopenacc_api.create_region != NULL);

  func = model.lookup<MDCG::Model::function_t>("acc_present");
  libopenacc_api.present = func->node->symbol;
  assert(libopenacc_api.present != NULL);
  func = model.lookup<MDCG::Model::function_t>("acc_present_regions_");
  libopenacc_api.present_region = func->node->symbol;
  assert(libopenacc_api.present_region != NULL);

  func = model.lookup<MDCG::Model::function_t>("acc_present_or_copyin");
  libopenacc_api.present_or_copyin = func->node->symbol;
  assert(libopenacc_api.present_or_copyin != NULL);
  func = model.lookup<MDCG::Model::function_t>("acc_present_or_copyin_regions_");
  libopenacc_api.present_or_copyin_region = func->node->symbol;
  assert(libopenacc_api.present_or_copyin_region != NULL);

  func = model.lookup<MDCG::Model::function_t>("acc_present_or_copyout");
  libopenacc_api.present_or_copyout = func->node->symbol;
  assert(libopenacc_api.present_or_copyout != NULL);
  func = model.lookup<MDCG::Model::function_t>("acc_present_or_copyout_regions_");
  libopenacc_api.present_or_copyout_region = func->node->symbol;
  assert(libopenacc_api.present_or_copyout_region != NULL);

  func = model.lookup<MDCG::Model::function_t>("acc_present_or_create");
  libopenacc_api.present_or_create = func->node->symbol;
  assert(libopenacc_api.present_or_create != NULL);
  func = model.lookup<MDCG::Model::function_t>("acc_present_or_create_regions_");
  libopenacc_api.present_or_create_region = func->node->symbol;
  assert(libopenacc_api.present_or_create_region != NULL);

  std::set<MDCG::Model::class_t> classes;
  model_builder.get(libopenacc_model).lookup<MDCG::Model::class_t>("acc_region_t_", classes);
  assert(classes.size() == 1);
  libopenacc_api.region_class = *(classes.begin());

  assert(libopenacc_api.region_class->scope->field_children.size() == 6);

  libopenacc_api.region_param_ptrs = libopenacc_api.region_class->scope->field_children[1];
  libopenacc_api.region_scalar_ptrs = libopenacc_api.region_class->scope->field_children[2];
  libopenacc_api.region_data = libopenacc_api.region_class->scope->field_children[3];
  libopenacc_api.region_loops = libopenacc_api.region_class->scope->field_children[4];
  libopenacc_api.region_devices = libopenacc_api.region_class->scope->field_children[5];

  libopenacc_api.region_data = libopenacc_api.region_class->scope->field_children[3];
  assert(libopenacc_api.region_data->node->type != NULL);
  assert(libopenacc_api.region_data->node->type->node->kind == MDCG::Model::node_t<MDCG::Model::e_model_type>::e_pointer_type);
  assert(libopenacc_api.region_data->node->type->node->base_type != NULL);
  assert(libopenacc_api.region_data->node->type->node->base_type->node->kind == MDCG::Model::node_t<MDCG::Model::e_model_type>::e_class_type);
  MDCG::Model::class_t data = libopenacc_api.region_data->node->type->node->base_type->node->base_class;
  assert(data != NULL);
  assert(data->node->symbol->get_name().getString() == "acc_data_t_");

  assert(data->scope->field_children.size() == 5);

  libopenacc_api.region_data_ptr                             = data->scope->field_children[0];
  libopenacc_api.region_data_nbr_elements                    = data->scope->field_children[1];
  libopenacc_api.region_data_element_size                    = data->scope->field_children[2];
  libopenacc_api.region_data_dominant_dimension              = data->scope->field_children[3];
  libopenacc_api.region_data_nbr_elements_dominant_dimension = data->scope->field_children[4];

  assert(libopenacc_api.region_loops->node->type != NULL);
  assert(libopenacc_api.region_loops->node->type->node->kind == MDCG::Model::node_t<MDCG::Model::e_model_type>::e_pointer_type);
  assert(libopenacc_api.region_loops->node->type->node->base_type != NULL);
  assert(libopenacc_api.region_loops->node->type->node->base_type->node->kind == MDCG::Model::node_t<MDCG::Model::e_model_type>::e_class_type);
  MDCG::Model::class_t loops = libopenacc_api.region_loops->node->type->node->base_type->node->base_class;
  assert(loops != NULL);
  assert(loops->node->symbol->get_name().getString() == "acc_loop_t_");

  assert(loops->scope->field_children.size() == 3);

  libopenacc_api.region_loops_lower  = loops->scope->field_children[0];
  libopenacc_api.region_loops_upper  = loops->scope->field_children[1];
  libopenacc_api.region_loops_stride = loops->scope->field_children[2];

  assert(libopenacc_api.region_devices->node->type != NULL);
  assert(libopenacc_api.region_devices->node->type->node->kind == MDCG::Model::node_t<MDCG::Model::e_model_type>::e_pointer_type);
  assert(libopenacc_api.region_devices->node->type->node->base_type != NULL);
  assert(libopenacc_api.region_devices->node->type->node->base_type->node->kind == MDCG::Model::node_t<MDCG::Model::e_model_type>::e_class_type);
  MDCG::Model::class_t region_per_device = libopenacc_api.region_devices->node->type->node->base_type->node->base_class;
  assert(region_per_device != NULL);
  assert(region_per_device->node->symbol->get_name().getString() == "acc_region_per_device_t_");

  assert(region_per_device->scope->field_children.size() == 4);

  libopenacc_api.region_devices_device_idx = region_per_device->scope->field_children[0];
  libopenacc_api.region_devices_num_gangs = region_per_device->scope->field_children[1];
  libopenacc_api.region_devices_num_workers = region_per_device->scope->field_children[2];
  libopenacc_api.region_devices_vector_length = region_per_device->scope->field_children[3];
}

}

namespace Compiler {

void dataFromDataSections(
  const std::vector<Frontend::data_sections_t> & data_sections,
  std::vector<KLT::Data<KLT_Annotation<OpenACC::language_t> > *> & datas,
  std::vector<SgVariableSymbol *> & params
) {
  std::vector<Frontend::data_sections_t>::const_iterator it_data;
  for (it_data = data_sections.begin(); it_data != data_sections.end(); it_data++) {
    KLT::Data<KLT_Annotation<OpenACC::language_t> > * data = new KLT::Data<KLT_Annotation<OpenACC::language_t> >(it_data->first, it_data->second.size());
    assert(data != NULL);
    datas.push_back(data);

    std::vector<Frontend::section_t>::const_iterator it_section;
    for (it_section = it_data->second.begin(); it_section != it_data->second.end(); it_section++) {
      KLT::Data<DLX::KLT_Annotation<DLX::OpenACC::language_t> >::section_t section;
      section.lower_bound = it_section->lower_bound;
      section.size = it_section->size;
      section.stride = it_section->stride;
      data->addSection(section);

      std::vector<SgVarRefExp *>::const_iterator it_var_ref;
      std::vector<SgVarRefExp *> var_refs;

      if (section.lower_bound != NULL) {
        var_refs = SageInterface::querySubTree<SgVarRefExp>(section.lower_bound);
        for (it_var_ref = var_refs.begin(); it_var_ref != var_refs.end(); it_var_ref++)
          params.push_back((*it_var_ref)->get_symbol());
      }
      if (section.size != NULL) {
        var_refs = SageInterface::querySubTree<SgVarRefExp>(section.size);
        for (it_var_ref = var_refs.begin(); it_var_ref != var_refs.end(); it_var_ref++)
          params.push_back((*it_var_ref)->get_symbol());
      }
      if (section.stride != NULL) {
        var_refs = SageInterface::querySubTree<SgVarRefExp>(section.stride);
        for (it_var_ref = var_refs.begin(); it_var_ref != var_refs.end(); it_var_ref++)
          params.push_back((*it_var_ref)->get_symbol());
      }
    }
  }
}

void translateDataSections(
  const std::vector<Frontend::data_sections_t> & data_sections,
  Directives::generic_clause_t<OpenACC::language_t> * clause,
  LoopTrees * loop_tree
) {
  std::vector<KLT::Data<KLT_Annotation<OpenACC::language_t> > *> datas;
  std::vector<SgVariableSymbol *> params;
  dataFromDataSections(data_sections, datas, params);

  std::vector<KLT::Data<KLT_Annotation<OpenACC::language_t> > *>::const_iterator it_data;
  for (it_data = datas.begin(); it_data != datas.end(); it_data++) {
    KLT::Data<KLT_Annotation<OpenACC::language_t> > * data = *it_data;
    assert(data);
    loop_tree->addData(data);
    data->annotations.push_back(KLT_Annotation<OpenACC::language_t>(clause));
  }
  std::vector<SgVariableSymbol *>::const_iterator it_param;
  for (it_param = params.begin(); it_param != params.end(); it_param++)
    loop_tree->addParameter(*it_param);
}

void interpretClauses(
  const std::vector<Directives::generic_clause_t<OpenACC::language_t> *> & clauses,
  LoopTrees * loop_tree
) {
  assert(clauses.size() > 0);

  std::vector<Directives::generic_clause_t<OpenACC::language_t> *>::const_iterator it_clause;
  for (it_clause = clauses.begin(); it_clause != clauses.end(); it_clause++) {
    switch ((*it_clause)->kind) {
      case OpenACC::language_t::e_acc_clause_copy:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_copy> * clause =
                 (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_copy> *)(*it_clause);
        translateDataSections(clause->parameters.data_sections, clause, loop_tree);
        break;
      }
      case OpenACC::language_t::e_acc_clause_copyin:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_copyin> * clause =
                 (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_copyin> *)(*it_clause);
        translateDataSections(clause->parameters.data_sections, clause, loop_tree);
        break;
      }
      case OpenACC::language_t::e_acc_clause_copyout:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_copyout> * clause =
                 (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_copyout> *)(*it_clause);
        translateDataSections(clause->parameters.data_sections, clause, loop_tree);
        break;
      }
      case OpenACC::language_t::e_acc_clause_create:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_create> * clause =
                 (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_create> *)(*it_clause);
        translateDataSections(clause->parameters.data_sections, clause, loop_tree);
        break;
      }
      case OpenACC::language_t::e_acc_clause_present:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present> * clause =
                 (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present> *)(*it_clause);
        translateDataSections(clause->parameters.data_sections, clause, loop_tree);
        break;
      }
      case OpenACC::language_t::e_acc_clause_present_or_copy:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present_or_copy> * clause =
                 (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present_or_copy> *)(*it_clause);
        translateDataSections(clause->parameters.data_sections, clause, loop_tree);
        break;
      }
      case OpenACC::language_t::e_acc_clause_present_or_copyin:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present_or_copyin> * clause =
                 (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present_or_copyin> *)(*it_clause);
        translateDataSections(clause->parameters.data_sections, clause, loop_tree);
        break;
      }
      case OpenACC::language_t::e_acc_clause_present_or_copyout:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present_or_copyout> * clause =
                 (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present_or_copyout> *)(*it_clause);
        translateDataSections(clause->parameters.data_sections, clause, loop_tree);
        break;
      }
      case OpenACC::language_t::e_acc_clause_present_or_create:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present_or_create> * clause =
                 (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present_or_create> *)(*it_clause);
        translateDataSections(clause->parameters.data_sections, clause, loop_tree);
        break;
      }
      case OpenACC::language_t::e_acc_clause_if:
      {
        assert(false); /// \todo
        break;
      }
      case OpenACC::language_t::e_acc_clause_async:
      {
        assert(false); /// \todo
        break;
      }
      case OpenACC::language_t::e_acc_clause_reduction:
      {
        assert(false); /// \todo
        break;
      }
      case OpenACC::language_t::e_acc_clause_deviceptr:
      {
        assert(false); /// \todo
        break;
      }
      case OpenACC::language_t::e_acc_clause_private:
      {
        assert(false); /// \todo
        break;
      }
      case OpenACC::language_t::e_acc_clause_firstprivate:
      {
        assert(false); /// \todo
        break;
      }
      case OpenACC::language_t::e_acc_clause_devices:
      case OpenACC::language_t::e_acc_clause_num_gangs:
      case OpenACC::language_t::e_acc_clause_num_workers:
      case OpenACC::language_t::e_acc_clause_vector_length:
      {
        loop_tree->annotations.push_back(KLT_Annotation<OpenACC::language_t>(*it_clause));
        break;
      }
      default:
        assert(false);
    }
  }
}

LoopTrees::node_t * buildLoopTree(SgStatement * stmt, LoopTrees * loop_tree, std::vector<SgVariableSymbol *> & iterators, std::vector<SgVariableSymbol *> & locals, std::vector<SgVariableSymbol *> & others, std::map<SgForStatement *, LoopTrees::loop_t *> & loop_map);

LoopTrees::block_t * buildLoopTreeBlock(SgStatement * stmt, LoopTrees * loop_tree, std::vector<SgVariableSymbol *> & iterators, std::vector<SgVariableSymbol *> & locals, std::vector<SgVariableSymbol *> & others, std::map<SgForStatement *, LoopTrees::loop_t *> & loop_map) {
  LoopTrees::node_t * child = buildLoopTree(stmt, loop_tree, iterators, locals, others, loop_map);
  assert(child != NULL);
  LoopTrees::block_t * block = dynamic_cast<LoopTrees::block_t *>(child);
  if (block == NULL) {
    block = new LoopTrees::block_t();
    block->children.push_back(child);
  }
  return block;
}

LoopTrees::node_t * buildLoopTree(SgStatement * stmt, LoopTrees * loop_tree, std::vector<SgVariableSymbol *> & iterators, std::vector<SgVariableSymbol *> & locals, std::vector<SgVariableSymbol *> & others, std::map<SgForStatement *, LoopTrees::loop_t *> & loop_map) {
  switch (stmt->variantT()) {
    case V_SgBasicBlock:
    {
      SgBasicBlock * bb = (SgBasicBlock *)stmt;

      LoopTrees::block_t * block = new LoopTrees::block_t();

      std::vector<SgStatement *>::const_iterator it_stmt;
      for (it_stmt = bb->get_statements().begin(); it_stmt != bb->get_statements().end(); it_stmt++)
        if (!isSgPragmaDeclaration(*it_stmt))
          block->children.push_back(buildLoopTree(*it_stmt, loop_tree, iterators, locals, others, loop_map));

      return block;
    }
    case V_SgForStatement:
    {
      SgForStatement * for_stmt = (SgForStatement *)stmt;

      SgVariableSymbol * iterator = NULL;
      SgExpression * lower_bound = NULL;
      SgExpression * upper_bound = NULL;
      SgExpression * stride = NULL;
      assert(SageInterface::getForLoopInformations(for_stmt, iterator, lower_bound, upper_bound, stride));

      iterators.push_back(iterator);
      std::vector<SgVarRefExp *>::const_iterator it_var_ref;
      std::vector<SgVarRefExp *> var_refs;

      var_refs = SageInterface::querySubTree<SgVarRefExp>(for_stmt->get_for_init_stmt());
      for (it_var_ref = var_refs.begin(); it_var_ref != var_refs.end(); it_var_ref++) {
        SgVariableSymbol * sym = (*it_var_ref)->get_symbol();
        if (std::find(iterators.begin(), iterators.end(), sym) == iterators.end())
          loop_tree->addParameter(sym); // in a loop : !iterator => parameter
      }

      var_refs = SageInterface::querySubTree<SgVarRefExp>(for_stmt->get_test());
      for (it_var_ref = var_refs.begin(); it_var_ref != var_refs.end(); it_var_ref++) {
        SgVariableSymbol * sym = (*it_var_ref)->get_symbol();
        if (std::find(iterators.begin(), iterators.end(), sym) == iterators.end())
          loop_tree->addParameter(sym); // in a loop : !iterator => parameter
      }

      var_refs = SageInterface::querySubTree<SgVarRefExp>(for_stmt->get_increment());
      for (it_var_ref = var_refs.begin(); it_var_ref != var_refs.end(); it_var_ref++) {
        SgVariableSymbol * sym = (*it_var_ref)->get_symbol();
        if (std::find(iterators.begin(), iterators.end(), sym) == iterators.end())
          loop_tree->addParameter(sym); // in a loop : !iterator => parameter
      }

      LoopTrees::loop_t * loop = new LoopTrees::loop_t(iterator, lower_bound, upper_bound, stride);

      loop_map.insert(std::pair<SgForStatement *, LoopTrees::loop_t *>(for_stmt, loop));

      loop->block = buildLoopTreeBlock(for_stmt->get_loop_body(), loop_tree, iterators, locals, others, loop_map);

      return loop;
    }
    case V_SgIfStmt:
    {
      SgIfStmt * if_stmt = (SgIfStmt *)stmt;

      SgExprStatement * cond_stmt = isSgExprStatement(if_stmt->get_conditional());
      assert(cond_stmt != NULL);
      SgExpression * cond_expr = cond_stmt->get_expression();
      assert(cond_expr != NULL);

      std::vector<SgVarRefExp *> var_refs = SageInterface::querySubTree<SgVarRefExp>(cond_expr);
      std::vector<SgVarRefExp *>::const_iterator it_var_ref;
      for (it_var_ref = var_refs.begin(); it_var_ref != var_refs.end(); it_var_ref++) {
        SgVariableSymbol * sym = (*it_var_ref)->get_symbol();
        if (std::find(iterators.begin(), iterators.end(), sym) == iterators.end() && std::find(locals.begin(), locals.end(), sym) == locals.end())
          others.push_back(sym); 
      }

      LoopTrees::cond_t * cond = new LoopTrees::cond_t(cond_expr);
      
      cond->block_true = buildLoopTreeBlock(if_stmt->get_true_body(), loop_tree, iterators, locals, others, loop_map);
      cond->block_false = buildLoopTreeBlock(if_stmt->get_false_body(), loop_tree, iterators, locals, others, loop_map);
      
      return cond;
    }
    case V_SgExprStatement:
    {
      SgExprStatement * expr_stmt = (SgExprStatement *)stmt;
      SgExpression * expr = expr_stmt->get_expression();
      assert(expr != NULL);

      std::vector<SgVarRefExp *> var_refs = SageInterface::querySubTree<SgVarRefExp>(expr);
      std::vector<SgVarRefExp *>::const_iterator it_var_ref;
      for (it_var_ref = var_refs.begin(); it_var_ref != var_refs.end(); it_var_ref++) {
        SgVariableSymbol * sym = (*it_var_ref)->get_symbol();
        if (std::find(iterators.begin(), iterators.end(), sym) == iterators.end() && std::find(locals.begin(), locals.end(), sym) == locals.end())
          others.push_back(sym); 
      }

      return new LoopTrees::stmt_t(stmt);
    }
    case V_SgVariableDeclaration:
    {
      SgVariableDeclaration * var_decl = isSgVariableDeclaration(stmt);
      assert(var_decl != NULL);
      SgScopeStatement * scope = var_decl->get_scope();
      assert(scope != NULL);
      const std::vector<SgInitializedName *> & decls = var_decl->get_variables();
      std::vector<SgInitializedName *>::const_iterator it_decl;
      for (it_decl = decls.begin(); it_decl != decls.end(); it_decl++) {
        SgVariableSymbol * var_sym = scope->lookup_variable_symbol((*it_decl)->get_name());
        assert(var_sym != NULL);
        locals.push_back(var_sym);
      }

      std::vector<SgVarRefExp *> var_refs = SageInterface::querySubTree<SgVarRefExp>(stmt);
      std::vector<SgVarRefExp *>::const_iterator it_var_ref;
      for (it_var_ref = var_refs.begin(); it_var_ref != var_refs.end(); it_var_ref++) {
        SgVariableSymbol * sym = (*it_var_ref)->get_symbol();
        if (std::find(iterators.begin(), iterators.end(), sym) == iterators.end() && std::find(locals.begin(), locals.end(), sym) == locals.end())
          others.push_back(sym); 
      }

      return new LoopTrees::stmt_t(stmt);
    }
    default:
      std::cerr << "Unsupported statement : " << stmt->class_name() << " ( " << stmt << " )" << std::endl;
      assert(false);
  }

  assert(false);
  return NULL;
}

void extractLoopTrees(
  const std::vector<Directives::directive_t<OpenACC::language_t> *> & directives,
  std::map<Directives::directive_t<OpenACC::language_t> *, LoopTrees *> & regions,
  std::map<SgForStatement *, LoopTrees::loop_t *> & loop_map
) {
  std::vector<Directives::directive_t<OpenACC::language_t> *>::const_iterator it_directive;
  for (it_directive = directives.begin(); it_directive != directives.end(); it_directive++) {
    Directives::directive_t<OpenACC::language_t> * directive = *it_directive;

    assert(!directive->clause_list.empty());

    switch (directive->construct->kind) {
      case OpenACC::language_t::e_acc_construct_parallel:
      {
        Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_parallel> * construct =
                 (Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_parallel> *)(directive->construct);

        LoopTrees * loop_tree = new LoopTrees();
        regions.insert(std::pair<Directives::directive_t<OpenACC::language_t> *, LoopTrees *>(directive, loop_tree));

        interpretClauses(directive->clause_list, loop_tree);

        SgStatement * region_base = construct->assoc_nodes.parallel_region;
        if (isSgPragmaDeclaration(region_base)) {
          assert(directive->successor_list.size() == 1);
          Directives::directive_t<OpenACC::language_t> * child = directive->successor_list.begin()->second;
          assert(child->construct->kind == OpenACC::language_t::e_acc_construct_loop);
          region_base = ((Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_loop> *)child->construct)->assoc_nodes.for_loop;
        }

       std::vector<SgVariableSymbol *> iterators;
       std::vector<SgVariableSymbol *> locals;
       std::vector<SgVariableSymbol *> others;

        SgBasicBlock * region_bb = isSgBasicBlock(region_base);
        if (region_bb != NULL) {
           std::vector<SgStatement *>::const_iterator it_stmt;
           for (it_stmt = region_bb->get_statements().begin(); it_stmt != region_bb->get_statements().end(); it_stmt++)
             if (!isSgPragmaDeclaration(*it_stmt))
               loop_tree->addTree(buildLoopTree(*it_stmt, loop_tree, iterators, locals, others, loop_map));
        }
        else loop_tree->addTree(buildLoopTree(region_base, loop_tree, iterators, locals, others, loop_map));

        const std::vector<SgVariableSymbol *> & params = loop_tree->getParameters();
        const std::vector<KLT::Data<Annotation> *> & datas_ = loop_tree->getDatas();

        std::vector<SgVariableSymbol *> datas;
        std::vector<KLT::Data<Annotation> *>::const_iterator it_data;
        for (it_data = datas_.begin(); it_data != datas_.end(); it_data++)
          datas.push_back((*it_data)->getVariableSymbol());

        std::vector<SgVariableSymbol *>::const_iterator it_other;
        for (it_other = others.begin(); it_other != others.end(); it_other++)
          if (std::find(params.begin(), params.end(), *it_other) == params.end() && std::find(datas.begin(), datas.end(), *it_other) == datas.end())
            loop_tree->addScalar(*it_other); // Neither iterators or parameters or data

        break;
      }
      case OpenACC::language_t::e_acc_construct_kernel:
      {
        assert(false); /// \todo generate LoopTrees from kernel regions
        break;
      }
      case OpenACC::language_t::e_acc_construct_data:
      case OpenACC::language_t::e_acc_construct_loop:
      case OpenACC::language_t::e_acc_construct_host_data:
      case OpenACC::language_t::e_acc_construct_declare:
      case OpenACC::language_t::e_acc_construct_cache:
      case OpenACC::language_t::e_acc_construct_update:
      case OpenACC::language_t::e_acc_construct_blank:
      default:
        break;
    }
  }
}

struct device_config_t {
  SgExpression * device_kind;
  SgExpression * device_num;

  SgExpression * num_gangs[3];
  SgExpression * num_workers[3];
  SgExpression * vector_length;
};

bool getDevicesConfig(LoopTrees * loop_trees, std::vector<struct device_config_t> & device_configs) {

  assert(device_configs.size() == 0);

  Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_num_gangs> * num_gangs[3] = {NULL, NULL, NULL};
  Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_num_workers> * num_workers[3] = {NULL, NULL, NULL};
  Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_vector_length> * vector_length = NULL;
  Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_devices> * devices = NULL;

  std::vector<KLT_Annotation<DLX::OpenACC::language_t> >::const_iterator it_annotation;
  for (it_annotation = loop_trees->annotations.begin(); it_annotation != loop_trees->annotations.end(); it_annotation++) {
    switch (it_annotation->clause->kind) {
      case OpenACC::language_t::e_acc_clause_num_gangs:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_num_gangs> * tmp = (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_num_gangs> * )it_annotation->clause;
        assert(tmp->parameters.lvl >= 0 && tmp->parameters.lvl <= 2);
        assert(num_gangs[tmp->parameters.lvl] == NULL);
        num_gangs[tmp->parameters.lvl] = tmp;
        break;
      }
      case OpenACC::language_t::e_acc_clause_num_workers:
      {
        Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_num_workers> * tmp = (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_num_workers> * )it_annotation->clause;
        assert(tmp->parameters.lvl >= 0 && tmp->parameters.lvl <= 2);
        assert(num_workers[tmp->parameters.lvl] == NULL);
        num_workers[tmp->parameters.lvl] = tmp;
        break;
      }
      case OpenACC::language_t::e_acc_clause_vector_length:
        assert(vector_length == NULL);
        vector_length = (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_vector_length> *)it_annotation->clause;
        break;
      case OpenACC::language_t::e_acc_clause_devices:
        assert(devices == NULL);
        devices = (Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_devices> *)it_annotation->clause;
        break;
      default:
        break;
    }
  }

  assert(num_gangs[2] == 0 || (num_gangs[0] != NULL && num_gangs[1] != NULL));       //    num_gangs[2]  => num_gangs[0] && num_gangs[1]
  assert(num_gangs[1] == 0 || num_gangs[0] != NULL);                                 //    num_gangs[1]  => num_gangs[0]

  assert(num_workers[2] == 0 || (num_workers[0] != NULL && num_workers[1] != NULL)); //  num_workers[2]  => num_workers[0] && num_workers[1]
  assert(num_workers[1] == 0 || num_workers[0] != NULL);                             //  num_workers[1]  => num_workers[0]

  if (devices != NULL) {
    size_t num_dev = devices->parameters.device_list.size();
    device_configs.resize(num_dev);

    std::vector<std::pair<SgExpression *, SgExpression *> >::const_iterator it_dev;
    size_t cnt_dev = 0;
    for (it_dev = devices->parameters.device_list.begin(); it_dev != devices->parameters.device_list.end(); it_dev++) {
      device_configs[cnt_dev].device_kind = it_dev->first;
      device_configs[cnt_dev].device_num = it_dev->second;

      if (num_gangs[0] != NULL) {
        assert(num_gangs[0]->parameters.exp.size() == num_dev);
        device_configs[cnt_dev].num_gangs[0] = num_gangs[0]->parameters.exp[cnt_dev];
      }
      else device_configs[cnt_dev].num_gangs[0] = SageBuilder::buildIntVal(0);

      if (num_gangs[1] != NULL) {
        assert(num_gangs[1]->parameters.exp.size() == num_dev);
        device_configs[cnt_dev].num_gangs[1] = num_gangs[1]->parameters.exp[cnt_dev];
      }
      else device_configs[cnt_dev].num_gangs[1] = SageBuilder::buildIntVal(0);

      if (num_gangs[2] != NULL) {
        assert(num_gangs[2]->parameters.exp.size() == num_dev);
        device_configs[cnt_dev].num_gangs[2] = num_gangs[2]->parameters.exp[cnt_dev];
      }
      else device_configs[cnt_dev].num_gangs[2] = SageBuilder::buildIntVal(0);

      if (num_workers[0] != NULL) {
        assert(num_workers[0]->parameters.exp.size() == num_dev);
        device_configs[cnt_dev].num_workers[0] = num_workers[0]->parameters.exp[cnt_dev];
      }
      else device_configs[cnt_dev].num_workers[0] = SageBuilder::buildIntVal(0);

      if (num_workers[1] != NULL) {
        assert(num_workers[1]->parameters.exp.size() == num_dev);
        device_configs[cnt_dev].num_workers[1] = num_workers[1]->parameters.exp[cnt_dev];
      }
      else device_configs[cnt_dev].num_workers[1] = SageBuilder::buildIntVal(0);

      if (num_workers[2] != NULL) {
        assert(num_workers[2]->parameters.exp.size() == num_dev);
        device_configs[cnt_dev].num_workers[2] = num_workers[2]->parameters.exp[cnt_dev];
      }
      else device_configs[cnt_dev].num_workers[2] = SageBuilder::buildIntVal(0);

      if (vector_length != NULL) {
        assert(vector_length->parameters.exp.size() == num_dev);
        device_configs[cnt_dev].vector_length = vector_length->parameters.exp[cnt_dev];
      }
      else device_configs[cnt_dev].vector_length = SageBuilder::buildIntVal(0);

      cnt_dev++;
    }
  }
  else {
    device_configs.resize(1);

    device_configs[0].device_kind = NULL;
    device_configs[0].device_num = NULL;

    if (num_gangs[0] != NULL) {
      assert(num_gangs[0]->parameters.exp.size() == 1);
      device_configs[0].num_gangs[0] = num_gangs[0]->parameters.exp[0];
    }
    else device_configs[0].num_gangs[0] = SageBuilder::buildIntVal(0);

    if (num_gangs[1] != NULL) {
      assert(num_gangs[1]->parameters.exp.size() == 1);
      device_configs[0].num_gangs[1] = num_gangs[1]->parameters.exp[0];
    }
    else device_configs[0].num_gangs[1] = SageBuilder::buildIntVal(0);

    if (num_gangs[2] != NULL) {
      assert(num_gangs[2]->parameters.exp.size() == 1);
      device_configs[0].num_gangs[2] = num_gangs[2]->parameters.exp[0];
    }
    else device_configs[0].num_gangs[2] = SageBuilder::buildIntVal(0);

    if (num_workers[0] != NULL) {
      assert(num_workers[0]->parameters.exp.size() == 1);
      device_configs[0].num_workers[0] = num_workers[0]->parameters.exp[0];
    }
    else device_configs[0].num_workers[0] = SageBuilder::buildIntVal(0);

    if (num_workers[1] != NULL) {
      assert(num_workers[1]->parameters.exp.size() == 1);
      device_configs[0].num_workers[1] = num_workers[1]->parameters.exp[0];
    }
    else device_configs[0].num_workers[1] = SageBuilder::buildIntVal(0);

    if (num_workers[2] != NULL) {
      assert(num_workers[2]->parameters.exp.size() == 1);
      device_configs[0].num_workers[2] = num_workers[2]->parameters.exp[0];
    }
    else device_configs[0].num_workers[2] = SageBuilder::buildIntVal(0);

    if (vector_length != NULL) {
      assert(vector_length->parameters.exp.size() == 1);
      device_configs[0].vector_length = vector_length->parameters.exp[0];
    }
    else device_configs[0].vector_length = SageBuilder::buildIntVal(0);

  }

  return true;
}

void genLoopConfig(
  LoopTrees::node_t * node,
  const DLX::OpenACC::compiler_modules_t::libopenacc_api_t & libopenacc_api,
  MFB::KLT_Driver & driver,
  SgScopeStatement * scope,
  size_t & loop_cnt,
  SgVariableSymbol * region_sym
) {
  LoopTrees::loop_t  * loop  = dynamic_cast<LoopTrees::loop_t  *>(node);
  LoopTrees::cond_t  * cond  = dynamic_cast<LoopTrees::cond_t  *>(node);
  LoopTrees::block_t * block = dynamic_cast<LoopTrees::block_t *>(node);
  LoopTrees::stmt_t  * stmt  = dynamic_cast<LoopTrees::stmt_t  *>(node);

  if (loop != NULL) {
    // loop_ref_exp : region->loops['loop_cnt']
    SgExpression * loop_ref_exp = SageBuilder::buildPntrArrRefExp(
      SageBuilder::buildArrowExp(SageBuilder::buildVarRefExp(region_sym), SageBuilder::buildVarRefExp(libopenacc_api.region_loops->node->symbol)),
      SageBuilder::buildIntVal(loop_cnt)
    );
    // region->loops['loop_cnt'].lower = 'loop->lower_bound'
    SageInterface::appendStatement(SageBuilder::buildExprStatement(SageBuilder::buildAssignOp(
      SageBuilder::buildDotExp(
        SageInterface::copyExpression(loop_ref_exp),
        SageBuilder::buildVarRefExp(libopenacc_api.region_loops_lower->node->symbol)
      ),
      SageInterface::copyExpression(loop->lower_bound)
    )), scope);
    // region->loops['loop_cnt'].upper = 'loop->upper_bound'
    SageInterface::appendStatement(SageBuilder::buildExprStatement(SageBuilder::buildAssignOp(
      SageBuilder::buildDotExp(
        SageInterface::copyExpression(loop_ref_exp),
        SageBuilder::buildVarRefExp(libopenacc_api.region_loops_upper->node->symbol)
      ),
      SageInterface::copyExpression(loop->upper_bound)
    )), scope);
    // region->loops['loop_cnt'].stride = 'loop->stride'
    SageInterface::appendStatement(SageBuilder::buildExprStatement(SageBuilder::buildAssignOp(
      SageBuilder::buildDotExp(
        loop_ref_exp,
        SageBuilder::buildVarRefExp(libopenacc_api.region_loops_stride->node->symbol)
      ),
      SageInterface::copyExpression(loop->stride)
    )), scope);

    loop_cnt++;
    genLoopConfig(loop->block, libopenacc_api, driver, scope, loop_cnt, region_sym);
  }
  else if (cond != NULL) {
    genLoopConfig(cond->block_true,  libopenacc_api, driver, scope, loop_cnt, region_sym);
    genLoopConfig(cond->block_false, libopenacc_api, driver, scope, loop_cnt, region_sym);
  }
  else if (block != NULL) {
    std::vector<LoopTrees::node_t *>::const_iterator it_block;
    for (it_block = block->children.begin(); it_block != block->children.end(); it_block++)
      genLoopConfig(*it_block, libopenacc_api, driver, scope, loop_cnt, region_sym);
  }
  else assert(stmt != NULL);
}

SgBasicBlock * buildRegionBlock(
  LoopTrees * loop_trees,
  const DLX::OpenACC::compiler_modules_t::libopenacc_api_t & libopenacc_api,
  MFB::KLT_Driver & driver,
  SgScopeStatement * scope,
  const std::vector<struct device_config_t> & device_configs
) {
  SgBasicBlock * result = SageBuilder::buildBasicBlock();

  // Import symbols in to be used in this file

  driver.useSymbol<SgFunctionDeclaration>(libopenacc_api.push_data_environment, scope);
  driver.useSymbol<SgFunctionDeclaration>(libopenacc_api.pop_data_environment, scope);

  driver.useSymbol<SgFunctionDeclaration>(libopenacc_api.build_region, scope);
  driver.useSymbol<SgFunctionDeclaration>(libopenacc_api.region_execute, scope);

  driver.useSymbol<SgFunctionDeclaration>(libopenacc_api.copyin, scope);
  driver.useSymbol<SgFunctionDeclaration>(libopenacc_api.copyin_region, scope);
  driver.useSymbol<SgFunctionDeclaration>(libopenacc_api.copyout, scope);
  driver.useSymbol<SgFunctionDeclaration>(libopenacc_api.copyout_region, scope);
  driver.useSymbol<SgFunctionDeclaration>(libopenacc_api.create, scope);
  driver.useSymbol<SgFunctionDeclaration>(libopenacc_api.create_region, scope);
  driver.useSymbol<SgFunctionDeclaration>(libopenacc_api.present_or_copyin, scope);
  driver.useSymbol<SgFunctionDeclaration>(libopenacc_api.present_or_copyin_region, scope);
  driver.useSymbol<SgFunctionDeclaration>(libopenacc_api.present_or_copyout, scope);
  driver.useSymbol<SgFunctionDeclaration>(libopenacc_api.present_or_copyout_region, scope);
  driver.useSymbol<SgFunctionDeclaration>(libopenacc_api.present_or_create, scope);
  driver.useSymbol<SgFunctionDeclaration>(libopenacc_api.present_or_create_region, scope);

  driver.useSymbol<SgClassDeclaration>(libopenacc_api.region_class->node->symbol, scope);

  // build decl : acc_region_t region = acc_build_region(0);

  SgInitializer * init = SageBuilder::buildAssignInitializer(
    SageBuilder::buildFunctionCallExp(
      SageBuilder::buildFunctionRefExp(libopenacc_api.build_region),
      SageBuilder::buildExprListExp(SageBuilder::buildIntVal(loop_trees->id))
    )
  );
  /// \todo notify MFB that we use libopenacc_api.region_class->node->symbol
  SgVariableDeclaration * region_decl = SageBuilder::buildVariableDeclaration("region", SageBuilder::buildPointerType(libopenacc_api.region_class->node->symbol->get_type()), init, result);
  SageInterface::appendStatement(region_decl, result);

  SgVariableSymbol * region_sym = SageInterface::getFirstVarSym(region_decl);
  assert(region_sym != NULL);

  std::vector<SgVariableSymbol *>::const_iterator it_var_sym;

  const std::vector<SgVariableSymbol *> & params = loop_trees->getParameters();
  size_t param_cnt = 0;
  for (it_var_sym = params.begin(); it_var_sym != params.end(); it_var_sym++) {
    // param_ptrs_ref_exp : region->param_ptrs
    SgExpression * param_ptrs_ref_exp = SageBuilder::buildArrowExp(SageBuilder::buildVarRefExp(region_sym), SageBuilder::buildVarRefExp(libopenacc_api.region_param_ptrs->node->symbol));
    // region->param_ptrs['param_cnt'] = &('*it_var_sym')
    SageInterface::appendStatement(SageBuilder::buildExprStatement(SageBuilder::buildAssignOp(
      SageBuilder::buildPntrArrRefExp(param_ptrs_ref_exp, SageBuilder::buildIntVal(param_cnt++)),
      SageBuilder::buildAddressOfOp(SageBuilder::buildVarRefExp(*it_var_sym))
    )), result);
  }


  const std::vector<SgVariableSymbol *> & scalars = loop_trees->getScalars();
  size_t scalar_cnt = 0;
  for (it_var_sym = scalars.begin(); it_var_sym != scalars.end(); it_var_sym++) {
    // scalar_ptrs_ref_exp : region->scalar_ptrs
    SgExpression * scalar_ptrs_ref_exp = SageBuilder::buildArrowExp(SageBuilder::buildVarRefExp(region_sym), SageBuilder::buildVarRefExp(libopenacc_api.region_scalar_ptrs->node->symbol));
    // region->scalar_ptrs['scalar_cnt'] = &('*it_var_sym')
    SageInterface::appendStatement(SageBuilder::buildExprStatement(SageBuilder::buildAssignOp(
      SageBuilder::buildPntrArrRefExp(scalar_ptrs_ref_exp, SageBuilder::buildIntVal(scalar_cnt++)),
      SageBuilder::buildAddressOfOp(SageBuilder::buildVarRefExp(*it_var_sym))
    )), result);
  }

  std::vector< ::KLT::Data<KLT_Annotation<OpenACC::language_t> > *>::const_iterator it_data;
  size_t data_cnt = 0;
  const std::vector< ::KLT::Data<KLT_Annotation<OpenACC::language_t> > *> & datas = loop_trees->getDatas();
  for (it_data = datas.begin(); it_data != datas.end(); it_data++) {
    // data_ref_exp : region->data['data_cnt']
    SgExpression * data_ref_exp = SageBuilder::buildPntrArrRefExp(
      SageBuilder::buildArrowExp(SageBuilder::buildVarRefExp(region_sym), SageBuilder::buildVarRefExp(libopenacc_api.region_data->node->symbol)),
      SageBuilder::buildIntVal(data_cnt++)
    );

    // array_ref_exp : ref on base data (go down the dimension for multidimensionnal arrays)
    SgExpression * array_ref_exp = SageBuilder::buildVarRefExp((*it_data)->getVariableSymbol());
    for (size_t section_cnt = 0; section_cnt < (*it_data)->getSections().size(); section_cnt++)
      array_ref_exp = SageBuilder::buildPntrArrRefExp(array_ref_exp, SageInterface::copyExpression((*it_data)->getSections()[section_cnt].lower_bound));
    array_ref_exp = SageBuilder::buildAddressOfOp(array_ref_exp); /// \todo not VALID with sections not starting at zero ?????

    // region->data['data_cnt'].ptr= 'array_ref_exp'
    SageInterface::appendStatement(SageBuilder::buildExprStatement(SageBuilder::buildAssignOp(
      SageBuilder::buildDotExp(SageInterface::copyExpression(data_ref_exp), SageBuilder::buildVarRefExp(libopenacc_api.region_data_ptr->node->symbol)), array_ref_exp
    )), result);

    // nbr_elements_exp : Number of data elements
    SgExpression * nbr_elements_exp = SageInterface::copyExpression((*it_data)->getSections()[0].size);
    for (size_t section_cnt = 1; section_cnt < (*it_data)->getSections().size(); section_cnt++)
      nbr_elements_exp = SageBuilder::buildMultiplyOp(nbr_elements_exp, SageInterface::copyExpression((*it_data)->getSections()[section_cnt].size));

    // region->data['data_cnt'].nbr_elements = 'nbr_elements_exp'
    SageInterface::appendStatement(SageBuilder::buildExprStatement(SageBuilder::buildAssignOp(
      SageBuilder::buildDotExp(SageInterface::copyExpression(data_ref_exp), SageBuilder::buildVarRefExp(libopenacc_api.region_data_nbr_elements->node->symbol)), nbr_elements_exp
    )), result);

    // element_size_exp : Size of one data element
    SgExpression * element_size_exp = SageBuilder::buildSizeOfOp((*it_data)->getBaseType());

    // region->data['data_cnt'].element_size = 'element_size_exp'
    SageInterface::appendStatement(SageBuilder::buildExprStatement(SageBuilder::buildAssignOp(
      SageBuilder::buildDotExp(SageInterface::copyExpression(data_ref_exp), SageBuilder::buildVarRefExp(libopenacc_api.region_data_element_size->node->symbol)), element_size_exp
    )), result);

    size_t dominant_dimension = 0; /// \todo

    // dominant_dimension_exp : 
    SgExpression * dominant_dimension_exp = SageBuilder::buildIntVal(dominant_dimension);

    // region->data['data_cnt'].dominant_dimension = 0
    SageInterface::appendStatement(SageBuilder::buildExprStatement(SageBuilder::buildAssignOp(
      SageBuilder::buildDotExp(SageInterface::copyExpression(data_ref_exp), SageBuilder::buildVarRefExp(libopenacc_api.region_data_dominant_dimension->node->symbol)), dominant_dimension_exp
    )), result);

    // nbr_elements_dominant_dimension_exp : 
    SgExpression * nbr_elements_dominant_dimension_exp = SageInterface::copyExpression((*it_data)->getSections()[dominant_dimension].size);

    // region->data['data_cnt'].nbr_elements_dominant_dimension = 'nbr_elements_dominant_dimension_exp'
    SageInterface::appendStatement(SageBuilder::buildExprStatement(SageBuilder::buildAssignOp(
      SageBuilder::buildDotExp(data_ref_exp, SageBuilder::buildVarRefExp(libopenacc_api.region_data_nbr_elements_dominant_dimension->node->symbol)), nbr_elements_dominant_dimension_exp
    )), result);
  }

  const std::vector<LoopTrees::node_t *> & trees = loop_trees->getTrees();
  std::vector<LoopTrees::node_t *>::const_iterator it_tree;
  size_t loop_cnt = 0;
  for (it_tree = trees.begin(); it_tree != trees.end(); it_tree++)
    genLoopConfig(*it_tree, libopenacc_api, driver, result, loop_cnt, region_sym);

  std::vector<struct device_config_t>::const_iterator it_config;
  size_t device_cnt = 0;
  for (it_config = device_configs.begin(); it_config != device_configs.end(); it_config++) {

    // device_ref_exp : region->devices['device_cnt++']
    SgExpression * device_ref_exp = SageBuilder::buildPntrArrRefExp(
      SageBuilder::buildArrowExp(SageBuilder::buildVarRefExp(region_sym), SageBuilder::buildVarRefExp(libopenacc_api.region_devices->node->symbol)),
      SageBuilder::buildIntVal(device_cnt++)
    );

    // region->devices['device_idx'].device_idx = 'device_idx';
    if (it_config->device_kind != NULL) {
      assert(it_config->device_num != NULL);
      SageInterface::appendStatement(SageBuilder::buildExprStatement(SageBuilder::buildAssignOp(
        SageBuilder::buildDotExp(SageInterface::copyExpression(device_ref_exp), SageBuilder::buildVarRefExp(libopenacc_api.region_devices_device_idx->node->symbol)),
        SageBuilder::buildFunctionCallExp(
          SageBuilder::buildFunctionRefExp(libopenacc_api.get_device_idx),
          SageBuilder::buildExprListExp(SageInterface::copyExpression(it_config->device_kind), SageInterface::copyExpression(it_config->device_num))
        )
      )), result);
    }

    // gang_ref_exp : region->devices['device_idx'].num_gangs
    SgExpression * gang_ref_exp = SageBuilder::buildDotExp(
      SageInterface::copyExpression(device_ref_exp),
      SageBuilder::buildVarRefExp(libopenacc_api.region_devices_num_gangs->node->symbol)
    );
    // region->devices['device_idx'].num_gangs[0] = 'loop_trees->p_num_gangs[0]'
    SageInterface::appendStatement(SageBuilder::buildExprStatement(SageBuilder::buildAssignOp(
      SageBuilder::buildPntrArrRefExp(SageInterface::copyExpression(gang_ref_exp), SageBuilder::buildIntVal(0)),
      it_config->num_gangs[0] != NULL ? SageInterface::copyExpression(it_config->num_gangs[0]) : SageBuilder::buildIntVal(0)
    )), result);
    // region->devices['device_idx'].num_gangs[1] = 'loop_trees->p_num_gangs[1]'
    SageInterface::appendStatement(SageBuilder::buildExprStatement(SageBuilder::buildAssignOp(
      SageBuilder::buildPntrArrRefExp(SageInterface::copyExpression(gang_ref_exp), SageBuilder::buildIntVal(1)),
      it_config->num_gangs[1] != NULL ? SageInterface::copyExpression(it_config->num_gangs[1]) : SageBuilder::buildIntVal(0)
    )), result);
    // region->devices['device_idx'].num_gangs[2] = 'loop_trees->p_num_gangs[2]'
    SageInterface::appendStatement(SageBuilder::buildExprStatement(SageBuilder::buildAssignOp(
      SageBuilder::buildPntrArrRefExp(gang_ref_exp, SageBuilder::buildIntVal(2)),
      it_config->num_gangs[2] != NULL ? SageInterface::copyExpression(it_config->num_gangs[2]) : SageBuilder::buildIntVal(0)
    )), result);

    // worker_ref_exp : region->devices['device_idx'].num_workers
    SgExpression * worker_ref_exp = SageBuilder::buildDotExp(
      SageInterface::copyExpression(device_ref_exp),
      SageBuilder::buildVarRefExp(libopenacc_api.region_devices_num_workers->node->symbol)
    );
    // region->devices['device_idx'].num_workers[0] = 'loop_trees->p_num_workers[0]'
    SageInterface::appendStatement(SageBuilder::buildExprStatement(SageBuilder::buildAssignOp(
      SageBuilder::buildPntrArrRefExp(SageInterface::copyExpression(worker_ref_exp), SageBuilder::buildIntVal(0)),
      it_config->num_workers[0] != NULL ? SageInterface::copyExpression(it_config->num_workers[0]) : SageBuilder::buildIntVal(0)
    )), result);
    // region->devices['device_idx'].num_workers[1] = 'loop_trees->p_num_workers[1]'
    SageInterface::appendStatement(SageBuilder::buildExprStatement(SageBuilder::buildAssignOp(
      SageBuilder::buildPntrArrRefExp(SageInterface::copyExpression(worker_ref_exp), SageBuilder::buildIntVal(1)),
      it_config->num_workers[1] != NULL ? SageInterface::copyExpression(it_config->num_workers[1]) : SageBuilder::buildIntVal(0)
    )), result);
    // region->devices['device_idx'].num_workers[2] = 'loop_trees->p_num_workers[2]'
    SageInterface::appendStatement(SageBuilder::buildExprStatement(SageBuilder::buildAssignOp(
      SageBuilder::buildPntrArrRefExp(worker_ref_exp, SageBuilder::buildIntVal(2)),
      it_config->num_workers[2] != NULL ? SageInterface::copyExpression(it_config->num_workers[2]) : SageBuilder::buildIntVal(0)
    )), result);

    // region->devices['device_idx'].vector_length = 'loop_trees->p_vector_length';
    SageInterface::appendStatement(SageBuilder::buildExprStatement(SageBuilder::buildAssignOp(
      SageBuilder::buildDotExp(device_ref_exp, SageBuilder::buildVarRefExp(libopenacc_api.region_devices_vector_length->node->symbol)),
      it_config->vector_length != NULL ? SageInterface::copyExpression(it_config->vector_length) : SageBuilder::buildIntVal(0)
    )), result);
  }

  // build call : acc_push_data_environment(); /// \todo ? region specific call (create one data env per device)

  SgExprStatement * push_dataenv_call = SageBuilder::buildExprStatement(SageBuilder::buildFunctionCallExp(
      SageBuilder::buildFunctionRefExp(libopenacc_api.push_data_environment), SageBuilder::buildExprListExp()
  ));
  SageInterface::appendStatement(push_dataenv_call, result);

  // data : copyin / create

  for (it_data = datas.begin(); it_data != datas.end(); it_data++) {
    SgFunctionSymbol * func_to_call = NULL;
    std::vector<Annotation>::const_iterator it_annotation;
    for (it_annotation = (*it_data)->annotations.begin(); it_annotation != (*it_data)->annotations.end(); it_annotation++) {
      switch (it_annotation->clause->kind) {
        case OpenACC::language_t::e_acc_clause_copy:
        case OpenACC::language_t::e_acc_clause_copyin:
          assert(func_to_call == NULL);
          func_to_call = libopenacc_api.copyin_region;
          break;
        case OpenACC::language_t::e_acc_clause_copyout:
        case OpenACC::language_t::e_acc_clause_create:
          assert(func_to_call == NULL);
          func_to_call = libopenacc_api.create_region;
          break;
        case OpenACC::language_t::e_acc_clause_present:
          assert(func_to_call == NULL);
          func_to_call = libopenacc_api.present_region;
          break;
        case OpenACC::language_t::e_acc_clause_present_or_copy:
        case OpenACC::language_t::e_acc_clause_present_or_copyin:
          assert(func_to_call == NULL);
          func_to_call = libopenacc_api.present_or_copyin_region;
          break;
        case OpenACC::language_t::e_acc_clause_present_or_create:
        case OpenACC::language_t::e_acc_clause_present_or_copyout:
          assert(func_to_call == NULL);
          func_to_call = libopenacc_api.present_or_create_region;
          break;
        default:
          assert(false);
      }
    }

    assert(func_to_call != NULL);

    SgExpression * array_ref_exp = SageBuilder::buildVarRefExp((*it_data)->getVariableSymbol());
    for (size_t section_cnt = 0; section_cnt < (*it_data)->getSections().size(); section_cnt++)
      array_ref_exp = SageBuilder::buildPntrArrRefExp(array_ref_exp, SageInterface::copyExpression((*it_data)->getSections()[section_cnt].lower_bound));
    array_ref_exp = SageBuilder::buildAddressOfOp(array_ref_exp);
    SgExpression * array_size_exp = SageInterface::copyExpression((*it_data)->getSections()[0].size);
    for (size_t section_cnt = 1; section_cnt < (*it_data)->getSections().size(); section_cnt++)
      array_size_exp = SageBuilder::buildMultiplyOp(array_size_exp, SageInterface::copyExpression((*it_data)->getSections()[section_cnt].size));

    SgExprStatement * call = SageBuilder::buildExprStatement(SageBuilder::buildFunctionCallExp(
      SageBuilder::buildFunctionRefExp(func_to_call),
      SageBuilder::buildExprListExp(
        SageBuilder::buildVarRefExp(region_sym),
        array_ref_exp,
        SageBuilder::buildMultiplyOp(array_size_exp, SageBuilder::buildSizeOfOp((*it_data)->getBaseType()))
      )
    ));
    SageInterface::appendStatement(call, result);
  }

  // build call : acc_region_execute(region);

  SgExprStatement * region_exec_call = SageBuilder::buildExprStatement(SageBuilder::buildFunctionCallExp(
      SageBuilder::buildFunctionRefExp(libopenacc_api.region_execute),
      SageBuilder::buildExprListExp(SageBuilder::buildVarRefExp(region_sym))
  ));
  SageInterface::appendStatement(region_exec_call, result);

  // data : copyout

  for (it_data = datas.begin(); it_data != datas.end(); it_data++) {
    SgFunctionSymbol * func_to_call = NULL;
    std::vector<Annotation>::const_iterator it_annotation;
    for (it_annotation = (*it_data)->annotations.begin(); it_annotation != (*it_data)->annotations.end(); it_annotation++) {
      switch (it_annotation->clause->kind) {
        case OpenACC::language_t::e_acc_clause_copy:
        case OpenACC::language_t::e_acc_clause_copyout:
          assert(func_to_call == NULL);
          func_to_call = libopenacc_api.copyout_region;
          break;
        case OpenACC::language_t::e_acc_clause_present_or_copy:
        case OpenACC::language_t::e_acc_clause_present_or_copyout:
          assert(func_to_call == NULL);
          func_to_call = libopenacc_api.present_or_copyout_region;
          break;
        case OpenACC::language_t::e_acc_clause_copyin:
        case OpenACC::language_t::e_acc_clause_create:
        case OpenACC::language_t::e_acc_clause_present:
        case OpenACC::language_t::e_acc_clause_present_or_copyin:
        case OpenACC::language_t::e_acc_clause_present_or_create:
          break;
        default:
          assert(false);
      }
    }

    if (func_to_call == NULL) continue;

    SgExpression * array_ref_exp = SageBuilder::buildVarRefExp((*it_data)->getVariableSymbol());
    for (size_t section_cnt = 0; section_cnt < (*it_data)->getSections().size(); section_cnt++)
      array_ref_exp = SageBuilder::buildPntrArrRefExp(array_ref_exp, SageInterface::copyExpression((*it_data)->getSections()[section_cnt].lower_bound));
    array_ref_exp = SageBuilder::buildAddressOfOp(array_ref_exp);
    SgExpression * array_size_exp = SageInterface::copyExpression((*it_data)->getSections()[0].size);
    for (size_t section_cnt = 1; section_cnt < (*it_data)->getSections().size(); section_cnt++)
      array_size_exp = SageBuilder::buildMultiplyOp(array_size_exp, SageInterface::copyExpression((*it_data)->getSections()[section_cnt].size));
    array_size_exp = SageBuilder::buildMultiplyOp(array_size_exp, SageBuilder::buildSizeOfOp((*it_data)->getBaseType()));

    SgExprStatement * call = SageBuilder::buildExprStatement(SageBuilder::buildFunctionCallExp(
      SageBuilder::buildFunctionRefExp(func_to_call),
      SageBuilder::buildExprListExp(SageBuilder::buildVarRefExp(region_sym), array_ref_exp, array_size_exp)
    ));
    SageInterface::appendStatement(call, result);
  }

  // build call : acc_pop_data_environment();

  SgExprStatement * pop_dataenv_call = SageBuilder::buildExprStatement(SageBuilder::buildFunctionCallExp(
      SageBuilder::buildFunctionRefExp(libopenacc_api.pop_data_environment), SageBuilder::buildExprListExp()
  ));
  SageInterface::appendStatement(pop_dataenv_call, result);

  return result;
}

template <>
bool Compiler<DLX::OpenACC::language_t, DLX::OpenACC::compiler_modules_t>::compile(
  const Compiler<DLX::OpenACC::language_t, DLX::OpenACC::compiler_modules_t>::directives_ptr_set_t & directives,
  const Compiler<DLX::OpenACC::language_t, DLX::OpenACC::compiler_modules_t>::directives_ptr_set_t & graph_entry,
  const Compiler<DLX::OpenACC::language_t, DLX::OpenACC::compiler_modules_t>::directives_ptr_set_t & graph_final
) {
  assert(compiler_modules.comp_data.regions.empty());

  /// \todo verify that it is correct OpenACC.....

  /// Extract LoopTrees from original code
  std::map<Directives::directive_t<OpenACC::language_t> *, LoopTrees *> regions;
  std::map<SgForStatement *, LoopTrees::loop_t *> loop_map;
  extractLoopTrees(directives, regions, loop_map);

  std::vector<Directives::directive_t<OpenACC::language_t> *>::const_iterator it_directive;
  for (it_directive = directives.begin(); it_directive != directives.end(); it_directive++) {
    Directives::directive_t<OpenACC::language_t> * directive = *it_directive;
    if (directive->construct->kind == OpenACC::language_t::e_acc_construct_loop) {
      Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_loop> * loop_construct =
                 (Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_loop> *)(directive->construct);
      std::map<SgForStatement *, LoopTrees::loop_t *>::const_iterator it_loop = loop_map.find(loop_construct->assoc_nodes.for_loop);
      assert(it_loop != loop_map.end());
      std::vector<Directives::generic_clause_t<OpenACC::language_t> *>::const_iterator it_clause;
      for (it_clause = directive->clause_list.begin(); it_clause != directive->clause_list.end(); it_clause++) 
        it_loop->second->annotations.push_back(KLT_Annotation<OpenACC::language_t>(*it_clause));
    }
  }

  compiler_modules.comp_data.regions.resize(regions.size());

  /// Transforms parallel/kernel directives
  size_t region_cnt = 0;
  std::map<Directives::directive_t<OpenACC::language_t> *, LoopTrees *>::const_iterator it_region;
  for (it_region = regions.begin(); it_region != regions.end(); it_region++, region_cnt++) {
    /// Get devices configuration from LoopTrees annotation
    std::vector<struct device_config_t> device_configs;
    assert(getDevicesConfig(it_region->second, device_configs));

    /// Build associated input for MDCG model
    MDCG::OpenACC::RegionDesc::input_t & input_region = compiler_modules.comp_data.regions[region_cnt];
      input_region.id = it_region->second->id;
      input_region.file = compiler_modules.ocl_kernels_file;
      input_region.loop_tree = it_region->second;
      input_region.num_devices = device_configs.size();

//  input_region.loop_tree->toText(std::cout);

    /// Generate OpenCL kernel (stored in 'input_region.kernel_lists')
    compiler_modules.generator.generate(*(it_region->second), input_region.kernel_lists, compiler_modules.cg_config);

    SgStatement * old_region = NULL;
    if (it_region->first->construct->kind == OpenACC::language_t::e_acc_construct_parallel)
      old_region = ((Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_parallel> *)(it_region->first->construct))->assoc_nodes.parallel_region;
    else if (it_region->first->construct->kind == OpenACC::language_t::e_acc_construct_kernel)
      old_region = ((Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_kernel> *)(it_region->first->construct))->assoc_nodes.kernel_region;
    assert(old_region != NULL);

    if (isSgPragmaDeclaration(old_region)) {
      assert(it_region->first->successor_list.size() == 1);
      Directives::directive_t<OpenACC::language_t> * child = it_region->first->successor_list.begin()->second;
      assert(child->construct->kind == OpenACC::language_t::e_acc_construct_loop);
      old_region = ((Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_loop> *)child->construct)->assoc_nodes.for_loop;
    }

    SgBasicBlock * region_block = buildRegionBlock(
                                    it_region->second,
                                    compiler_modules.libopenacc_api,
                                    compiler_modules.driver,
                                    SageInterface::getScope(old_region),
                                    device_configs
                                  );

    if (it_region->first->construct->kind == OpenACC::language_t::e_acc_construct_parallel)
      ((Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_parallel> *)(it_region->first->construct))->assoc_nodes.parallel_region = region_block;
    else if (it_region->first->construct->kind == OpenACC::language_t::e_acc_construct_kernel)
      ((Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_kernel> *)(it_region->first->construct))->assoc_nodes.kernel_region = region_block;

    SageInterface::replaceStatement(old_region, region_block);
  }

  /// Transforms data directives
  for (it_directive = directives.begin(); it_directive != directives.end(); it_directive++) {
    Directives::directive_t<OpenACC::language_t> * directive = *it_directive;
    if (directive->construct->kind == OpenACC::language_t::e_acc_construct_data) {
      Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_data> * data_construct =
                 (Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_data> *)(directive->construct);

      SgBasicBlock * data_block = isSgBasicBlock(data_construct->assoc_nodes.data_region);
      if (data_block == NULL) {
        assert(isSgPragmaDeclaration(data_construct->assoc_nodes.data_region));
        assert(directive->successor_list.size() == 1);
        assert(directive->successor_list[0].first == DLX::OpenACC::language_t::e_child_scope);
        assert(directive->successor_list[0].second != NULL);
        SgStatement * child_stmt = NULL;
        if (directive->successor_list[0].second->construct->kind == OpenACC::language_t::e_acc_construct_parallel)
          child_stmt = ((Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_parallel> *)(it_region->first->construct))->assoc_nodes.parallel_region;
        else if (directive->successor_list[0].second->construct->kind == OpenACC::language_t::e_acc_construct_kernel)
          child_stmt = ((Directives::construct_t<OpenACC::language_t, OpenACC::language_t::e_acc_construct_kernel> *)(it_region->first->construct))->assoc_nodes.kernel_region;
        else assert(false);
        assert(child_stmt != NULL);
        data_block = SageBuilder::buildBasicBlock(child_stmt);
      }
      assert(data_block != NULL);

      std::vector<Directives::generic_clause_t<OpenACC::language_t> *>::const_iterator it_clause;
      for (it_clause = directive->clause_list.begin(); it_clause != directive->clause_list.end(); it_clause++) {
        SgFunctionSymbol * call_before = NULL;
        SgFunctionSymbol * call_after  = NULL;
        std::vector<KLT::Data<KLT_Annotation<OpenACC::language_t> > *> datas;
        std::vector<SgVariableSymbol *> params;
        switch ((*it_clause)->kind) {
          case OpenACC::language_t::e_acc_clause_copy:
            call_before = compiler_modules.libopenacc_api.copyin;
            call_after  = compiler_modules.libopenacc_api.copyout;
            dataFromDataSections(((Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_copy> * )(*it_clause))->parameters.data_sections, datas, params);
            break;
          case OpenACC::language_t::e_acc_clause_copyin:
            call_before = compiler_modules.libopenacc_api.copyin;
            call_after  = NULL;
            dataFromDataSections(((Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_copyin> * )(*it_clause))->parameters.data_sections, datas, params);
            break;
          case OpenACC::language_t::e_acc_clause_copyout:
            call_before = compiler_modules.libopenacc_api.create;
            call_after  = compiler_modules.libopenacc_api.copyout;
            dataFromDataSections(((Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_copyout> * )(*it_clause))->parameters.data_sections, datas, params);
            break;
          case OpenACC::language_t::e_acc_clause_create:
            call_before = compiler_modules.libopenacc_api.create;
            call_after  = NULL;
            dataFromDataSections(((Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_create> * )(*it_clause))->parameters.data_sections, datas, params);
            break;
          case OpenACC::language_t::e_acc_clause_present:
            call_before = compiler_modules.libopenacc_api.present;
            call_after  = NULL;
            dataFromDataSections(((Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present> * )(*it_clause))->parameters.data_sections, datas, params);
            break;
          case OpenACC::language_t::e_acc_clause_present_or_copy:
            call_before = compiler_modules.libopenacc_api.present_or_copyin;
            call_after  = compiler_modules.libopenacc_api.present_or_copyout;
            dataFromDataSections(((Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present_or_copy> * )(*it_clause))->parameters.data_sections, datas, params);
            break;
          case OpenACC::language_t::e_acc_clause_present_or_copyin:
            call_before = compiler_modules.libopenacc_api.present_or_copyin;
            call_after  = NULL;
            dataFromDataSections(((Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present_or_copyin> * )(*it_clause))->parameters.data_sections, datas, params);
            break;
          case OpenACC::language_t::e_acc_clause_present_or_create:
            call_before = compiler_modules.libopenacc_api.present_or_create;
            call_after  = NULL;
            dataFromDataSections(((Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present_or_create> * )(*it_clause))->parameters.data_sections, datas, params);
            break;
          case OpenACC::language_t::e_acc_clause_present_or_copyout:
            call_before = compiler_modules.libopenacc_api.present_or_create;
            call_after  = compiler_modules.libopenacc_api.present_or_copyout;
            dataFromDataSections(((Directives::clause_t<OpenACC::language_t, OpenACC::language_t::e_acc_clause_present_or_copyout> * )(*it_clause))->parameters.data_sections, datas, params);
            break;
          default:
            assert(false);
        }

        std::vector<KLT::Data<KLT_Annotation<OpenACC::language_t> > *>::const_iterator it_data;
        for (it_data = datas.begin(); it_data != datas.end(); it_data++) {
          SgExpression * array_ref_exp = SageBuilder::buildVarRefExp((*it_data)->getVariableSymbol());
          for (size_t section_cnt = 0; section_cnt < (*it_data)->getSections().size(); section_cnt++)
            array_ref_exp = SageBuilder::buildPntrArrRefExp(array_ref_exp, SageInterface::copyExpression((*it_data)->getSections()[section_cnt].lower_bound));
          array_ref_exp = SageBuilder::buildAddressOfOp(array_ref_exp);
          SgExpression * array_size_exp = SageInterface::copyExpression((*it_data)->getSections()[0].size);
          for (size_t section_cnt = 1; section_cnt < (*it_data)->getSections().size(); section_cnt++)
            array_size_exp = SageBuilder::buildMultiplyOp(array_size_exp, SageInterface::copyExpression((*it_data)->getSections()[section_cnt].size));
          array_size_exp = SageBuilder::buildMultiplyOp(array_size_exp, SageBuilder::buildSizeOfOp((*it_data)->getBaseType()));

          if (call_before != NULL) {
            SgExprStatement * call = SageBuilder::buildExprStatement(SageBuilder::buildFunctionCallExp(
              SageBuilder::buildFunctionRefExp(call_before),
              SageBuilder::buildExprListExp(array_ref_exp, array_size_exp)
            ));
            if (call_after != NULL) {
              array_ref_exp = SageInterface::copyExpression(array_ref_exp);
              array_size_exp = SageInterface::copyExpression(array_size_exp);
            }
            SageInterface::prependStatement(call, data_block);
          }
          if (call_after != NULL) {
            SgExprStatement * call = SageBuilder::buildExprStatement(SageBuilder::buildFunctionCallExp(
              SageBuilder::buildFunctionRefExp(call_after),
              SageBuilder::buildExprListExp(array_ref_exp, array_size_exp)
            ));
            SageInterface::appendStatement(call, data_block);
          }
        }
      }
    }
  }

  // Removes all OpenACC pragma
  std::vector<SgPragmaDeclaration * > pragma_decls = SageInterface::querySubTree<SgPragmaDeclaration>(compiler_modules.driver.project);
  std::vector<SgPragmaDeclaration * >::iterator it_pragma_decl;
  for (it_pragma_decl = pragma_decls.begin(); it_pragma_decl != pragma_decls.end(); it_pragma_decl++) {
    std::string directive_string = (*it_pragma_decl)->get_pragma()->get_pragma();
    if (::DLX::Frontend::consume_label(directive_string, OpenACC::language_t::language_label))
      SageInterface::removeStatement(*it_pragma_decl);
  }

  return true;
}

/** @} */
}

}

