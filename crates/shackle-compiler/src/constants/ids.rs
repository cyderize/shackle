use crate::hir::{db::Hir, Identifier};

macro_rules! id_registry {
	($struct:ident, $($tail:tt)*) => {
		id_registry!(@def $struct ($($tail)*) ());
		id_registry!(@imp $struct db ($($tail)*) ());
	};

	(@def $struct:ident ($($name:ident $(:$value:expr)?)?) ($($rest:tt)*)) => {
		/// Registry for common identifiers
		#[derive(Clone, Debug, PartialEq, Eq)]
		pub struct $struct {
			$($rest)*
			$(
				#[allow(missing_docs)]
				pub $name: Identifier,
			)?
		}
	};
	(@def $struct:ident ($name:ident $(:$value:expr)?, $($todo:tt)*) ($($rest:tt)*)) => {
		id_registry!(@def $struct ($($todo)*) (
			$($rest)*
			#[allow(missing_docs)]
			pub $name: Identifier,
		));
	};

	(@imp $struct:ident $db:ident ($($name:ident)?) ($($rest:tt)*)) => {
		impl $struct {
			/// Create a new identifier registry
			pub fn new($db: &dyn Hir) -> Self {
				Self {
					$($rest)*
					$(
						$name: Identifier::new(stringify!($name), $db),
					)?
				}
			}
		}
	};
	(@imp $struct:ident $db:ident ($name:ident, $($todo:tt)*) ($($rest:tt)*)) => {
		id_registry!(@imp $struct $db ($($todo)*) (
			$($rest)*
			$name: Identifier::new(stringify!($name), $db),
		));
	};


	(@imp $struct:ident $db:ident ($name:ident: $value:expr) ($($rest:tt)*)) => {
		impl $struct {
			/// Create a new identifier registry
			pub fn new($db: &dyn Hir) -> Self {
				Self {
					$($rest)*
					$name: Identifier::new($value, $db)
				}
			}
		}
	};
	(@imp $struct:ident $db:ident ($name:ident: $value:expr, $($todo:tt)*) ($($rest:tt)*)) => {
		id_registry!(@imp $struct $db ($($todo)*) (
			$($rest)*
			$name: Identifier::new($value, $db),
		));
	};
}

#[allow(unused_imports)] // TODO
pub(crate) use id_registry;

id_registry!(
	Builtins,
	mzn_get_parameter,
	mzn_forall_par,
	mzn_forall_var,
	mzn_exists_par,
	mzn_exists_var,
	mzn_indexed_array,
	mzn_element_internal,
	mzn_slice_internal,
	mzn_array_to_set,
	mzn_array_plus_plus,
	mzn_array_length,
	mzn_index_sets_agree,
	mzn_index_sets,
	mzn_array_xd,
	mzn_array_kd,
	mzn_compute_div_bounds,
	mzn_compute_mod_bounds,
	mzn_compute_float_div_bounds,
	mzn_compute_pow_bounds,
	mzn_normal,
	mzn_uniform_float,
	mzn_uniform_int,
	mzn_poisson,
	mzn_gamma,
	mzn_weibull,
	mzn_exponential,
	mzn_lognormal,
	mzn_chisquared,
	mzn_cauchy,
	mzn_fdistribution,
	mzn_tdistribution,
	mzn_discrete_distribution,
	mzn_bernoulli,
	mzn_binomial,
	mzn_add_warning,
	mzn_trace_exp,
	mzn_trace_to_section,
	mzn_logstream_to_string,
	mzn_abort,
	mzn_internal_check_debug_mode,
	mzn_lb,
	mzn_ub,
	mzn_lb_array,
	mzn_ub_array,
	mzn_dom,
	mzn_dom_array,
	mzn_dom_bounds_array,
	mzn_has_bounds,
	mzn_has_ub_set,
	mzn_is_fixed,
	mzn_fix,
	mzn_has_ann,
	mzn_annotate,
	mzn_is_same,
	mzn_compiler_version,
	mzn_string_plus_plus,
	mzn_string_length,
	mzn_concat,
	mzn_join,
	mzn_lt_par,
	mzn_lt_var,
	mzn_le_par,
	mzn_le_var,
	mzn_ne_par,
	mzn_ne_var,
	mzn_eq_par,
	mzn_eq_var,
	mzn_and_par,
	mzn_and_var,
	mzn_or_par,
	mzn_or_var,
	mzn_implies_par,
	mzn_implies_var,
	mzn_xor_par,
	mzn_xor_var,
	mzn_not_par,
	mzn_not_var,
	mzn_xorall,
	mzn_iffall,
	mzn_clause_var,
	mzn_clause_par,
	mzn_sort,
	mzn_sort_by,
	mzn_show,
	mzn_show_dzn,
	mzn_show_dzn_id,
	mzn_show_checker_output,
	mzn_show_json,
	mzn_format,
	mzn_format_justify_string,
	mzn_output_to_section,
	mzn_output_to_json_section,
	mzn_set_range,
	mzn_in_par,
	mzn_in_var,
	mzn_subset_par,
	mzn_subset_var,
	mzn_superset_par,
	mzn_superset_var,
	mzn_union_par,
	mzn_union_var,
	mzn_intersect_par,
	mzn_intersect_var,
	mzn_diff_par,
	mzn_diff_var,
	mzn_symdiff_par,
	mzn_symdiff_var,
	mzn_set_to_ranges,
	mzn_ceil,
	mzn_floor,
	mzn_round,
	mzn_set_to_array,
	mzn_add_par,
	mzn_add_var,
	mzn_sub_par,
	mzn_sub_var,
	mzn_times_par,
	mzn_times_var,
	mzn_pow,
	mzn_neg_par,
	mzn_neg_var,
	mzn_div_int,
	mzn_mod,
	mzn_div_float,
	mzn_sum_par,
	mzn_sum_var,
	mzn_product_par,
	mzn_product_var,
	mzn_minimum,
	mzn_maximum,
	mzn_min_array,
	mzn_max_array,
	mzn_arg_min,
	mzn_arg_max,
	mzn_abs,
	mzn_sqrt,
	mzn_exp,
	mzn_ln,
	mzn_log10,
	mzn_sin,
	mzn_cos,
	mzn_tan,
	mzn_asin,
	mzn_acos,
	mzn_atan,
	mzn_sinh,
	mzn_cosh,
	mzn_tanh,
	mzn_asinh,
	mzn_acosh,
	mzn_atanh,
);

id_registry!(
	Annotations,
	annotated_expression,
	output_only,
	shackle_type,
	empty_annotation,
	mzn_var_where_clause,
	promise_total,
	output,
	no_output,
	mzn_inline,
	mzn_inline_call_by_name,
	mzn_unreachable,
);

id_registry!(
	Literals,
	empty_string: "",
	return_value: "<return value>",
	default,
);

id_registry!(
	Names,
	objective: "_objective",
);

id_registry!(
	Functions,
	mzn_builtin,
	forall,
	exists,
	sum,
	show,
	show_json: "showJSON",
	show_dzn: "showDzn",
	join,
	plus_plus: "++",
	concat,
	and: "/\\",
	times: "*",
	lb,
	ub,
	implies: "->",
	array2set,
	in_: "in",
	array_xd: "arrayXd",
	mzn_start_indexed_array,
	array_access: "[]",
	index_set,
	mzn_infinite_range,
	set2iter,
	symmetry_breaking_constraint,
	redundant_constraint,
	erase_enum,
	mzn_erase_index_sets,
	enum_of,
	to_enum,
	mzn_to_enum,
	occurs,
	deopt,
	minus: "-",
	eq: "=",
	set2array,
	dot_dot: "..",
	fix,
	is_fixed,
	annotate,
	index_sets,
	mzn_get_enum,
	mzn_defining_set,
	mzn_construct_enum,
	mzn_destruct_enum,
	mzn_show_enum,
	default,
	mzn_construct_opt,
	mzn_destruct_opt,
	mzn_opt_domain,
	mzn_opt_channel,
	mzn_domain_constraint,
	mzn_check_index_set,
	mzn_show_array_access,
	mzn_show_tuple_access,
	mzn_show_record_access,
	mzn_opt_bool,
	mzn_construct_partial,
	mzn_destruct_partial,
	mzn_array_access_valid,
	mzn_array_access_known_valid,
	if_then_else,
);

/// Registry for common identifiers
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IdentifierRegistry {
	/// Interpreter builtins
	pub builtins: Builtins,
	/// Annotations
	pub annotations: Annotations,
	/// Literals for strings
	pub literals: Literals,
	/// Names of variables
	pub names: Names,
	/// Non-builtin functions (or compiler erased functions)
	pub functions: Functions,
}

impl IdentifierRegistry {
	/// Create a new identifier registry
	pub fn new(db: &dyn Hir) -> Self {
		Self {
			builtins: Builtins::new(db),
			annotations: Annotations::new(db),
			literals: Literals::new(db),
			names: Names::new(db),
			functions: Functions::new(db),
		}
	}
}
