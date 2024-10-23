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
	forall,
	exists,
	mzn_indexed_array,
	mzn_element_internal,
	mzn_slice_internal,
	array2set,
	plus_plus: "++",
	length,
	index_sets_agree,
	index_sets,
	array_xd: "arrayXd",
	mzn_array_kd,
	compute_div_bounds,
	compute_mod_bounds,
	compute_float_div_bounds,
	compute_pow_bounds,
	normal,
	uniform,
	poisson,
	gamma,
	weibull,
	exponential,
	lognormal,
	chisquared,
	cauchy,
	fdistribution,
	tdistribution,
	discrete_distribution,
	bernoulli,
	binomial,
	mzn_add_warning,
	trace,
	trace_exp,
	trace_dbg,
	trace_to_section,
	trace_stdout,
	trace_logstream,
	logstream_to_string,
	abort,
	mzn_internal_check_debug_mode,
	lb,
	ub,
	lb_array,
	ub_array,
	dom,
	dom_array,
	dom_bounds_array,
	has_bounds,
	has_ub_set,
	is_fixed,
	fix,
	has_ann,
	annotate,
	is_same,
	mzn_compiler_version,
	string_length,
	concat,
	join,
	lt: "<",
	le: "<=",
	gt: ">",
	ge: ">=",
	ne: "!=",
	eq: "",
	and: "/\\",
	or: "\\/",
	implies: "->",
	reverse_implies: "<-",
	iff: "<->",
	xor,
	not,
	xorall,
	iffall,
	clause,
	sort,
	sort_by,
	show,
	show_dzn: "showDzn",
	show_dzn_id: "showDznId",
	show_checker_output: "showCheckerOutput",
	show_json: "showJSON",
	format,
	format_justify_string,
	output_to_section,
	output_to_json_section,
	dot_dot: "..",
	in_: "in",
	subset,
	superset,
	union_: "union",
	intersect,
	diff,
	symdiff,
	set2ranges,
	ceil,
	floor,
	round,
	set2array,
	plus: "+",
	minus: "-",
	times: "*",
	pow,
	div,
	mod_: "mod",
	float_div: "/",
	sum,
	product,
	min,
	max,
	arg_min,
	arg_max,
	abs,
	sqrt,
	exp,
	ln,
	log10,
	sin,
	cos,
	tan,
	asin,
	acos,
	atan,
	sinh,
	cosh,
	tanh,
	asinh,
	acosh,
	atanh,
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
