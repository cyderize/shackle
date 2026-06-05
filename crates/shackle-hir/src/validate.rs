//! Final validation step for HIR representation.
//!
//! This module contains miscellaneous validation steps which require the whole
//! program HIR, and can't be done on a per-item basis.
//!
//! - Check for illegal overloading/duplicate definitions
//! - Check for multiple definitions of variables
//! - Check for multiple solve items

use std::collections::hash_map::Entry;

use shackle_utils::hash::Map;
use shackle_diagnostics::{
	AdditionalSolveItem, ConstructorAlreadyDefined, DuplicateAssignment, DuplicateConstructor,
	DuplicateFunction, FunctionAlreadyDefined, IllegalOverload, IllegalOverloading,
	MultipleAssignments, MultipleSolveItems,
};
use shackle_ty::{FunctionEntry, OverloadingError};

use crate::{
	Db, GlobalScope, Item, PatternTy,
	diagnostics::Errors,
	ids::{ExpressionRef, NodeRef},
	lower::lower_models,
};

/// Validate HIR
pub fn validate_hir<'db>(db: &'db dyn Db) {
	log::info!("Validating HIR");
	// Validate overloading
	for (_, ps) in GlobalScope::functions(db) {
		let mut overloads = Vec::new();
		let mut annotation_constructors = Vec::new();
		let mut enum_constructors = Vec::new();
		for p in ps.iter() {
			let signature = p.item(db).signature(db);
			match &signature.patterns[&p.pattern(db)] {
				PatternTy::Function(f) | PatternTy::AnnotationDestructure(f) => {
					overloads.push((*p, *f.clone()));
				}
				PatternTy::AnnotationConstructor(f) => {
					if annotation_constructors.is_empty() {
						overloads.push((*p, *f.clone()));
					}
					annotation_constructors.push(*p);
				}
				PatternTy::EnumConstructor(ecs) => {
					if enum_constructors.is_empty() {
						overloads.extend(ecs.iter().map(|f| (*p, f.constructor.clone())));
					}
					enum_constructors.push(*p);
				}
				PatternTy::EnumDestructure(fs) => {
					overloads.extend(fs.iter().map(|f| (*p, f.clone())));
				}
				_ => unreachable!(),
			}
		}
		if annotation_constructors.len() > 1 {
			let mut iter = annotation_constructors.into_iter();
			let first = iter.next().unwrap();
			let name = first.identifier(db).unwrap();
			let (src, span) = first.source_span(db);
			let others = iter
				.map(|c| {
					let (src, span) = c.source_span(db);
					let help = format!(
						"Try removing this item or use the functional syntax 'function ann: {}(..) = ..'.",
						name.pretty_print(db)
					);
					DuplicateConstructor { help, src, span }
				})
				.collect();
			Errors::add(db, ConstructorAlreadyDefined { src, span, others });
		}
		if enum_constructors.len() > 1 {
			let mut iter = enum_constructors.into_iter();
			let first = iter.next().unwrap();
			let (src, span) = first.source_span(db);
			let others = iter
				.map(|c| {
					let (src, span) = c.source_span(db);
					let help = "Try removing this enum constructor.".to_owned();
					DuplicateConstructor { help, src, span }
				})
				.collect();
			Errors::add(db, ConstructorAlreadyDefined { src, span, others });
		}
		let errors = FunctionEntry::check_overloading(db, overloads);

		for e in errors.iter() {
			match e {
				OverloadingError::FunctionAlreadyDefined {
					first: (first_pat, first_fn),
					others,
				} => {
					let name = first_pat.identifier(db).unwrap();
					let signature = first_fn.overload.pretty_print_call_signature(db, name);
					let (src, span) = first_pat.source_span(db);
					Errors::add(
						db,
						FunctionAlreadyDefined {
							src,
							span,
							signature,
							others: others
								.iter()
								.map(|(p, _)| {
									let (src, span) = p.source_span(db);
									DuplicateFunction { src, span }
								})
								.collect(),
						},
					);
				}
				OverloadingError::IncompatibleReturnType {
					first: (first_pat, _),
					others,
				} => {
					let (src, span) = first_pat.source_span(db);
					Errors::add(
						db,
						IllegalOverloading {
							src,
							span,
							others: others
								.iter()
								.map(|(p, _)| {
									let (src, span) = p.source_span(db);
									IllegalOverload { src, span }
								})
								.collect(),
						},
					)
				}
			}
		}
	}

	// Check for multiple assignments to variables
	let mut assignments: Map<_, Vec<NodeRef<'db>>> = Map::default();
	for model in lower_models(db).iter() {
		for it in model.items(db).iter() {
			match it {
				Item::Assignment(item) => {
					let a = item.assignment(db);
					let types = it.types(db);
					if let Some(p) = types.name_resolution(a.assignee) {
						match assignments.entry(p) {
							Entry::Occupied(mut e) => {
								e.get_mut().push((*it).into());
							}
							Entry::Vacant(e) => {
								let mut v = Vec::new();
								let resolved_item = p.item(db);
								if let Item::Declaration(d) = resolved_item
									&& let Some(def) = d.declaration(db).definition
								{
									v.push(
										ExpressionRef::new(db, resolved_item, def)
											.into_entity(db)
											.into(),
									);
								}
								v.push((*it).into());
								let _ = e.insert(v);
							}
						}
					}
				}
				Item::EnumAssignment(item) => {
					let a = item.enum_assignment(db);
					let types = it.types(db);
					if let Some(p) = types.name_resolution(a.assignee) {
						match assignments.entry(p) {
							Entry::Occupied(mut e) => {
								e.get_mut().push((*it).into());
							}
							Entry::Vacant(e) => {
								let mut v = Vec::new();
								let resolved_item = p.item(db);
								if let Item::Enumeration(e) = resolved_item
									&& e.enumeration(db).definition.is_some()
								{
									v.push(p.into_entity(db).into());
								}
								v.push((*it).into());
								let _ = e.insert(v);
							}
						}
					}
				}
				_ => (),
			}
		}
	}
	for (p, asgs) in assignments {
		if asgs.len() > 1 {
			let variable = p.identifier(db).unwrap().pretty_print(db);
			let mut asgs = asgs.into_iter();
			let (src, span) = asgs.next().unwrap().source_span(db);
			let others = asgs
				.map(|i| {
					let (src, span) = i.source_span(db);
					DuplicateAssignment { src, span }
				})
				.collect();
			Errors::add(
				db,
				MultipleAssignments {
					src,
					span,
					variable,
					others,
				},
			)
		}
	}

	// Check for multiple solve items
	let mut solve_items = Vec::new();
	for m in lower_models(db).iter() {
		for it in m.items(db).iter() {
			if let Item::Solve(_) = it {
				solve_items.push(*it);
			}
		}
	}
	if solve_items.len() > 1 {
		let mut iter = solve_items.into_iter();
		let first = iter.next().unwrap();
		let (src, span) = NodeRef::from(first).source_span(db);
		Errors::add(
			db,
			MultipleSolveItems {
				src,
				span,
				others: iter
					.map(|i| {
						let (src, span) = NodeRef::from(i).source_span(db);
						AdditionalSolveItem { src, span }
					})
					.collect(),
			},
		);
	}
}
