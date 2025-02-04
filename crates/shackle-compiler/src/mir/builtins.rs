//! Interpreter builtins
//!

use super::{db::Mir, Value};
use crate::hir::Identifier;

/// An interpreter builtin
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Builtin {
	/// Get the value of a top-level input parameter
	GetParameter(Box<Value>),

	/// `forall` with an array of par booleans
	ForAllPar(Box<Value>),
	/// `forall` with an array of var booleans
	ForAllVar(Box<Value>),

	/// `exists` with an array of par booleans
	ExistsPar(Box<Value>),
	/// `exists` with an array of var booleans
	ExistsVar(Box<Value>),
	/// Transform list of tuples of index and value into indexed array
	IndexedArray(Box<Value>),
	/// Array access with par index
	ArrayAccess {
		/// The array to access
		array: Box<Value>,
		/// The indices
		indices: Box<Value>,
	},
	/// Array slicing
	ArraySlice {
		/// The array to slice
		array: Box<Value>,
		/// The indices
		slices: Box<Value>,
	},
	/// Coerce array to set
	ArrayToSet(Box<Value>),
	/// Array concatenation
	ArrayPlusPlus {
		/// Left-hand side array
		left: Box<Value>,
		/// Right-hand side array
		right: Box<Value>,
	},
	/// Array length
	ArrayLength(Box<Value>),
	/// Array index sets agree
	IndexSetsAgree {
		/// Left-hand side array
		left: Box<Value>,
		/// Right-hand side array
		right: Box<Value>,
	},

	/// Get array index sets
	IndexSets(Box<Value>),
	/// Copy index sets
	ArrayXd {
		/// Array from which to take the index sets
		source: Box<Value>,
		/// Array to coerce
		array: Box<Value>,
	},
	/// Change array to use the given index sets
	ArrayKd {
		/// Tuple of index sets
		index_sets: Box<Value>,
		/// The array to coerce
		array: Box<Value>,
	},

	/// Compute bounds for integer division
	ComputeDivBounds {
		/// Dividend
		dividend: Box<Value>,
		/// Divisor
		divisor: Box<Value>,
	},
	/// Compute bounds for mod
	ComputeModBounds {
		/// Dividend
		dividend: Box<Value>,
		/// Divisor
		divisor: Box<Value>,
	},
	/// Compute bounds for float division
	ComputeFloatDivBounds {
		/// Dividend
		dividend: Box<Value>,
		/// Divisor
		divisor: Box<Value>,
	},
	/// Compute bounds for pow
	ComputePowBounds {
		/// Base
		base: Box<Value>,
		/// Exponent
		exponent: Box<Value>,
	},

	// mzn_increment_counter
	/// Sample from normal distribution
	Normal {
		/// Mean
		mean: Box<Value>,
		/// Standard deviation
		std: Box<Value>,
	},
	/// Sample from uniform distribution
	UniformFloat {
		/// Lower bound
		lower: Box<Value>,
		/// Upper bound
		upper: Box<Value>,
	},
	/// Sample from uniform distribution
	UniformInt {
		/// Lower bound
		lower: Box<Value>,
		/// Upper bound
		upper: Box<Value>,
	},
	/// Sample from Poisson distribution
	Poisson {
		/// Mean
		mean: Box<Value>,
	},
	/// Sample from gamma distribution
	Gamma {
		/// Alpha
		alpha: Box<Value>,
		/// Beta
		beta: Box<Value>,
	},
	/// Sample from weibull distribution
	Weibull {
		/// Shape
		shape: Box<Value>,
		/// Scale
		scale: Box<Value>,
	},
	/// Sample from exponential distribution
	Exponential {
		/// Lambda
		lambda: Box<Value>,
	},
	/// Sample from lognormal distribution
	LogNormal {
		/// Mean
		mean: Box<Value>,
		/// Standard deviation
		std: Box<Value>,
	},
	/// Sample from chi-squared distribution
	ChiSquared {
		/// Degree of freedom
		dof: Box<Value>,
	},
	/// Sample from Cauchy distribution
	Cauchy {
		/// Mean
		mean: Box<Value>,
		/// Scale
		scale: Box<Value>,
	},
	/// Sample from Fisher-Snedecor F-distribution
	FDistribution {
		/// First degree of freedom
		dof1: Box<Value>,
		/// Second degree of freedom
		dof2: Box<Value>,
	},
	/// Sample from student's T-distribution
	TDistribution {
		/// Sample size
		size: Box<Value>,
	},
	/// Sample from discrete distribution
	DiscreteDistribution {
		/// Weights
		weights: Box<Value>,
	},
	/// Sample from Bernoulli distribution
	Bernoulli {
		/// Probability
		prob: Box<Value>,
	},
	/// Sample from binomial distribution
	Binomial {
		/// Number of trials
		n: Box<Value>,
		/// Probability
		p: Box<Value>,
	},

	/// Emit a warning
	Warn(Box<Value>),

	/// Trace expression
	TraceExp(Box<Value>),
	/// Trace to section
	TraceToSection {
		/// The section
		section: Box<Value>,
		/// The message
		message: Box<Value>,
		/// Whether or not the string is JSON
		json: Box<Value>,
	},
	/// Output log stream as string
	LogStreamToString,
	/// Abort
	Abort(Box<Value>),
	/// Check if in debug mode
	CheckDebugMode,

	/// Lower bound
	LowerBound(Box<Value>),
	/// Upper bound
	UpperBound(Box<Value>),
	/// Min of lower bounds in array
	LowerBoundArray(Box<Value>),
	/// Max of upper bounds in array
	UpperBoundArray(Box<Value>),
	/// Get domain
	Domain(Box<Value>),
	/// Union of domains of array elements
	DomainArray(Box<Value>),
	/// Approximation of union of domains of array elements
	DomainBoundsArray(Box<Value>),

	/// Whether variable has declared, finite bounds
	HasBounds(Box<Value>),
	/// Whether set has declared, finite upper bound
	HasUpperBoundSet(Box<Value>),

	/// Whether the variable is fixed
	IsFixed(Box<Value>),
	/// Fix the value or abort
	Fix(Box<Value>),

	/// Whether or not a variable has an annotation
	HasAnn {
		/// The variable
		variable: Box<Value>,
		/// The annotation
		ann: Box<Value>,
	},
	/// Annotate a declaration
	Annotate {
		/// The variable
		variable: Box<Value>,
		/// The annotation
		ann: Box<Value>,
	},

	/// Whether these are the same variable
	IsSame {
		/// The variable
		variable: Box<Value>,
		/// The other variable
		other: Box<Value>,
	},

	/// Compiler version
	CompilerVersion,

	/// Concatenate two strings
	StringPlusPlus {
		/// Left-hand side string
		left: Box<Value>,
		/// Right-hand side string
		right: Box<Value>,
	},
	/// Get length of string
	StringLength(Box<Value>),
	/// Concatenate an array of strings
	StringConcat(Box<Value>),
	/// Join an array of strings by a delimiter
	StringJoin {
		/// The delimiter
		delimiter: Box<Value>,
		/// Array of strings
		strings: Box<Value>,
	},

	/// Par less than
	LessThanPar {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Var less than
	LessThanVar {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Par less than or equal to
	LessThanEqPar {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Var less than or equal to
	LessThanEqVar {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Par not equal
	NotEqualPar {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Var not equal
	NotEqualVar {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Par equality
	EqualPar {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Var equality
	EqualVar {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Par conjunction
	AndPar {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Var conjunction
	AndVar {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Par disjunction
	OrPar {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Var disjunction
	OrVar {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Par implication
	ImplicationPar {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Var implication
	ImplicationVar {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Par not
	NotPar(Box<Value>),
	/// Var not
	NotVar(Box<Value>),
	/// Xorall
	XorAll(Box<Value>),
	/// Iffall
	IffAll(Box<Value>),
	/// Par version of clause
	ClausePar {
		/// Literals
		pos: Box<Value>,
		/// Negated literals
		neg: Box<Value>,
	},
	/// Var version of clause
	ClauseVar {
		/// Literals
		pos: Box<Value>,
		/// Negated literals
		neg: Box<Value>,
	},

	/// Sort an array
	Sort(Box<Value>),
	/// Sort an array by values in another array
	SortBy {
		/// Array to sort
		array: Box<Value>,
		/// Values to sort by
		sort_by: Box<Value>,
	},

	/// Show an expression
	Show(Box<Value>),
	/// Show in DZN format
	ShowDzn(Box<Value>),
	/// Show a string as a DZN identifier
	ShowDznId(Box<Value>),
	/// Show checker output
	ShowCheckerOutput,
	/// Show in JSON format
	ShowJson(Box<Value>),

	/// Format value as string
	Format {
		/// Width
		width: Box<Value>,
		/// Precision for floats
		precision: Box<Value>,
		/// Value to format
		value: Box<Value>,
	},
	/// Format justified string
	FormatJustifyString {
		/// The width
		width: Box<Value>,
		/// The text to format
		text: Box<Value>,
	},

	/// Output to a section
	OutputToSection {
		/// Section
		section: Box<Value>,
		/// Value
		string: Box<Value>,
	},
	/// Output to a JSON section
	OutputToJsonSection {
		/// Section
		section: Box<Value>,
		/// Value
		value: Box<Value>,
	},

	/// Create range
	SetRange {
		/// Min
		min: Box<Value>,
		/// Max
		max: Box<Value>,
	},
	/// Test if value is in value in set
	SetInPar {
		/// The value
		value: Box<Value>,
		/// The set
		set: Box<Value>,
	},
	/// Test if value is in value in set
	SetInVar {
		/// The value
		value: Box<Value>,
		/// The set
		set: Box<Value>,
	},
	/// Test if set is subset
	SetSubsetPar {
		/// Left-hand side set
		left: Box<Value>,
		/// Right-hand side set
		right: Box<Value>,
	},
	/// Test if set is subset
	SetSubsetVar {
		/// Left-hand side set
		left: Box<Value>,
		/// Right-hand side set
		right: Box<Value>,
	},
	/// Par set union
	SetUnionPar {
		/// Left-hand side set
		left: Box<Value>,
		/// Right-hand side set
		right: Box<Value>,
	},
	/// Var set union
	SetUnionVar {
		/// Left-hand side set
		left: Box<Value>,
		/// Right-hand side set
		right: Box<Value>,
	},
	/// Par set intersection
	SetIntersectPar {
		/// Left-hand side set
		left: Box<Value>,
		/// Right-hand side set
		right: Box<Value>,
	},
	/// Var set intersection
	SetIntersectVar {
		/// Left-hand side set
		left: Box<Value>,
		/// Right-hand side set
		right: Box<Value>,
	},
	/// Par set difference
	SetDiffPar {
		/// Left-hand side set
		left: Box<Value>,
		/// Right-hand side set
		right: Box<Value>,
	},
	/// Var set difference
	SetDiffVar {
		/// Left-hand side set
		left: Box<Value>,
		/// Right-hand side set
		right: Box<Value>,
	},
	/// Par set symmetric difference
	SetSymDiffPar {
		/// Left-hand side set
		left: Box<Value>,
		/// Right-hand side set
		right: Box<Value>,
	},
	/// Var set symmetric difference
	SetSymDiffVar {
		/// Left-hand side set
		left: Box<Value>,
		/// Right-hand side set
		right: Box<Value>,
	},

	/// Set to ranges
	SetToRanges(Box<Value>),

	/// Ceiling
	Ceil(Box<Value>),
	/// Floor
	Floor(Box<Value>),
	/// Round
	Round(Box<Value>),
	/// Set to array
	SetToArray(Box<Value>),

	/// Par addition
	AddPar {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Var addition
	AddVar {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},

	/// Par subtraction
	SubPar {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Var subtraction
	SubVar {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},

	/// Par multiplication
	TimesPar {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Var multiplication
	TimesVar {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},

	/// Raise number to power
	Pow {
		/// Base
		base: Box<Value>,
		/// Exponent
		exponent: Box<Value>,
	},

	/// Par negation
	NegPar(Box<Value>),
	/// Var negation
	NegVar(Box<Value>),

	/// Integer (truncating) division
	DivInt {
		/// Left-hand side
		dividend: Box<Value>,
		/// Right-hand side
		divisor: Box<Value>,
	},
	/// Modulo
	Mod {
		/// Left-hand side
		dividend: Box<Value>,
		/// Right-hand side
		divisor: Box<Value>,
	},
	/// Floating point division
	DivFloat {
		/// Left-hand side
		dividend: Box<Value>,
		/// Right-hand side
		divisor: Box<Value>,
	},

	/// Par summation
	SumPar(Box<Value>),
	/// Var summation
	SumVar(Box<Value>),

	/// Par product
	ProductPar(Box<Value>),
	/// Var product
	ProductVar(Box<Value>),

	/// Minimum of two values
	Minimum {
		/// First argument
		left: Box<Value>,
		/// Second argument
		right: Box<Value>,
	},
	/// Minimum of two values
	Maximum {
		/// First argument
		left: Box<Value>,
		/// Second argument
		right: Box<Value>,
	},
	/// Minimum value of array
	MinArray(Box<Value>),
	/// Maximum value of array
	MaxArray(Box<Value>),

	/// Index of minimum value in array
	ArgMin(Box<Value>),
	/// Index of maximum value in array
	ArgMax(Box<Value>),

	/// Absolute value
	Abs(Box<Value>),
	/// Square root
	Sqrt(Box<Value>),
	/// Exponentiation
	Exp(Box<Value>),
	/// Natural logarithm
	Ln(Box<Value>),
	/// Base 10 logarithm
	Log10(Box<Value>),

	/// Sine function
	Sin(Box<Value>),
	/// Cosine function
	Cos(Box<Value>),
	/// Tangent function
	Tan(Box<Value>),
	/// Arcsine function
	ASin(Box<Value>),
	/// Arccosine function
	ACos(Box<Value>),
	/// Arctangent function
	ATan(Box<Value>),
	/// Hyperbolic sine function
	SinH(Box<Value>),
	/// Hyperbolic cosine function
	CosH(Box<Value>),
	/// Hyperbolic tangent function
	TanH(Box<Value>),
	/// Hyperbolic arcsine function
	ASinH(Box<Value>),
	/// Hyperbolic arccosine function
	ACosH(Box<Value>),
	/// Hyperbolic arctangent function
	ATanH(Box<Value>),
}

impl Builtin {
	/// Get the name of the builtin (used for printing as MiniZinc)
	pub fn name(&self, db: &dyn Mir) -> Identifier {
		let ids = db.identifier_registry();
		match self {
			Builtin::GetParameter(_) => ids.builtins.mzn_get_parameter,
			Builtin::ForAllPar(_) => ids.builtins.mzn_forall_par,
			Builtin::ForAllVar(_) => ids.builtins.mzn_forall_var,
			Builtin::ExistsPar(_) => ids.builtins.mzn_exists_par,
			Builtin::ExistsVar(_) => ids.builtins.mzn_exists_var,
			Builtin::IndexedArray(_) => ids.builtins.mzn_indexed_array,
			Builtin::ArrayAccess { .. } => ids.builtins.mzn_element_internal,
			Builtin::ArraySlice { .. } => ids.builtins.mzn_slice_internal,
			Builtin::ArrayToSet(_) => ids.builtins.mzn_array_to_set,
			Builtin::ArrayPlusPlus { .. } => ids.builtins.mzn_array_plus_plus,
			Builtin::ArrayLength(_) => ids.builtins.mzn_array_length,
			Builtin::IndexSetsAgree { .. } => ids.builtins.mzn_index_sets_agree,
			Builtin::IndexSets(_) => ids.builtins.mzn_index_sets,
			Builtin::ArrayXd { .. } => ids.builtins.mzn_array_xd,
			Builtin::ArrayKd { .. } => ids.builtins.mzn_array_kd,
			Builtin::ComputeDivBounds { .. } => ids.builtins.mzn_compute_div_bounds,
			Builtin::ComputeModBounds { .. } => ids.builtins.mzn_compute_mod_bounds,
			Builtin::ComputeFloatDivBounds { .. } => ids.builtins.mzn_compute_float_div_bounds,
			Builtin::ComputePowBounds { .. } => ids.builtins.mzn_compute_pow_bounds,
			Builtin::Normal { .. } => ids.builtins.mzn_normal,
			Builtin::UniformFloat { .. } => ids.builtins.mzn_uniform_float,
			Builtin::UniformInt { .. } => ids.builtins.mzn_uniform_int,
			Builtin::Poisson { .. } => ids.builtins.mzn_poisson,
			Builtin::Gamma { .. } => ids.builtins.mzn_gamma,
			Builtin::Weibull { .. } => ids.builtins.mzn_weibull,
			Builtin::Exponential { .. } => ids.builtins.mzn_exponential,
			Builtin::LogNormal { .. } => ids.builtins.mzn_lognormal,
			Builtin::ChiSquared { .. } => ids.builtins.mzn_chisquared,
			Builtin::Cauchy { .. } => ids.builtins.mzn_cauchy,
			Builtin::FDistribution { .. } => ids.builtins.mzn_fdistribution,
			Builtin::TDistribution { .. } => ids.builtins.mzn_tdistribution,
			Builtin::DiscreteDistribution { .. } => ids.builtins.mzn_discrete_distribution,
			Builtin::Bernoulli { .. } => ids.builtins.mzn_bernoulli,
			Builtin::Binomial { .. } => ids.builtins.mzn_binomial,
			Builtin::Warn(_) => ids.builtins.mzn_add_warning,
			Builtin::TraceExp(_) => ids.builtins.mzn_trace_exp,
			Builtin::TraceToSection { .. } => ids.builtins.mzn_trace_to_section,
			Builtin::LogStreamToString => ids.builtins.mzn_logstream_to_string,
			Builtin::Abort(_) => ids.builtins.mzn_abort,
			Builtin::CheckDebugMode => ids.builtins.mzn_internal_check_debug_mode,
			Builtin::LowerBound(_) => ids.builtins.mzn_lb,
			Builtin::UpperBound(_) => ids.builtins.mzn_ub,
			Builtin::LowerBoundArray(_) => ids.builtins.mzn_lb_array,
			Builtin::UpperBoundArray(_) => ids.builtins.mzn_ub_array,
			Builtin::Domain(_) => ids.builtins.mzn_dom,
			Builtin::DomainArray(_) => ids.builtins.mzn_dom_array,
			Builtin::DomainBoundsArray(_) => ids.builtins.mzn_dom_bounds_array,
			Builtin::HasBounds(_) => ids.builtins.mzn_has_bounds,
			Builtin::HasUpperBoundSet(_) => ids.builtins.mzn_has_ub_set,
			Builtin::IsFixed(_) => ids.builtins.mzn_is_fixed,
			Builtin::Fix(_) => ids.builtins.mzn_fix,
			Builtin::HasAnn { .. } => ids.builtins.mzn_has_ann,
			Builtin::Annotate { .. } => ids.builtins.mzn_annotate,
			Builtin::IsSame { .. } => ids.builtins.mzn_is_same,
			Builtin::CompilerVersion => ids.builtins.mzn_compiler_version,
			Builtin::StringPlusPlus { .. } => ids.builtins.mzn_string_plus_plus,
			Builtin::StringLength(_) => ids.builtins.mzn_string_length,
			Builtin::StringConcat(_) => ids.builtins.mzn_concat,
			Builtin::StringJoin { .. } => ids.builtins.mzn_join,
			Builtin::LessThanPar { .. } => ids.builtins.mzn_lt_par,
			Builtin::LessThanVar { .. } => ids.builtins.mzn_lt_var,
			Builtin::LessThanEqPar { .. } => ids.builtins.mzn_le_par,
			Builtin::LessThanEqVar { .. } => ids.builtins.mzn_le_var,
			Builtin::NotEqualPar { .. } => ids.builtins.mzn_ne_par,
			Builtin::NotEqualVar { .. } => ids.builtins.mzn_ne_var,
			Builtin::EqualPar { .. } => ids.builtins.mzn_eq_par,
			Builtin::EqualVar { .. } => ids.builtins.mzn_eq_var,
			Builtin::AndPar { .. } => ids.builtins.mzn_and_par,
			Builtin::AndVar { .. } => ids.builtins.mzn_and_var,
			Builtin::OrPar { .. } => ids.builtins.mzn_or_par,
			Builtin::OrVar { .. } => ids.builtins.mzn_or_var,
			Builtin::ImplicationPar { .. } => ids.builtins.mzn_implies_par,
			Builtin::ImplicationVar { .. } => ids.builtins.mzn_implies_var,
			Builtin::NotPar(_) => ids.builtins.mzn_not_par,
			Builtin::NotVar(_) => ids.builtins.mzn_not_var,
			Builtin::XorAll(_) => ids.builtins.mzn_xorall,
			Builtin::IffAll(_) => ids.builtins.mzn_iffall,
			Builtin::ClausePar { .. } => ids.builtins.mzn_clause_par,
			Builtin::ClauseVar { .. } => ids.builtins.mzn_clause_var,
			Builtin::Sort(_) => ids.builtins.mzn_sort,
			Builtin::SortBy { .. } => ids.builtins.mzn_sort_by,
			Builtin::Show(_) => ids.builtins.mzn_show,
			Builtin::ShowDzn(_) => ids.builtins.mzn_show_dzn,
			Builtin::ShowDznId(_) => ids.builtins.mzn_show_dzn_id,
			Builtin::ShowCheckerOutput => ids.builtins.mzn_show_checker_output,
			Builtin::ShowJson(_) => ids.builtins.mzn_show_json,
			Builtin::Format { .. } => ids.builtins.mzn_format,
			Builtin::FormatJustifyString { .. } => ids.builtins.mzn_format_justify_string,
			Builtin::OutputToSection { .. } => ids.builtins.mzn_output_to_section,
			Builtin::OutputToJsonSection { .. } => ids.builtins.mzn_output_to_json_section,
			Builtin::SetRange { .. } => ids.builtins.mzn_set_range,
			Builtin::SetInPar { .. } => ids.builtins.mzn_in_par,
			Builtin::SetInVar { .. } => ids.builtins.mzn_in_var,
			Builtin::SetSubsetPar { .. } => ids.builtins.mzn_subset_par,
			Builtin::SetSubsetVar { .. } => ids.builtins.mzn_subset_var,
			Builtin::SetUnionPar { .. } => ids.builtins.mzn_union_par,
			Builtin::SetUnionVar { .. } => ids.builtins.mzn_union_var,
			Builtin::SetIntersectPar { .. } => ids.builtins.mzn_intersect_par,
			Builtin::SetIntersectVar { .. } => ids.builtins.mzn_intersect_var,
			Builtin::SetDiffPar { .. } => ids.builtins.mzn_diff_par,
			Builtin::SetDiffVar { .. } => ids.builtins.mzn_diff_var,
			Builtin::SetSymDiffPar { .. } => ids.builtins.mzn_symdiff_par,
			Builtin::SetSymDiffVar { .. } => ids.builtins.mzn_symdiff_var,
			Builtin::SetToRanges(_) => ids.builtins.mzn_set_to_ranges,
			Builtin::Ceil(_) => ids.builtins.mzn_ceil,
			Builtin::Floor(_) => ids.builtins.mzn_floor,
			Builtin::Round(_) => ids.builtins.mzn_round,
			Builtin::SetToArray(_) => ids.builtins.mzn_set_to_array,
			Builtin::AddPar { .. } => ids.builtins.mzn_add_par,
			Builtin::AddVar { .. } => ids.builtins.mzn_add_var,
			Builtin::SubPar { .. } => ids.builtins.mzn_sub_par,
			Builtin::SubVar { .. } => ids.builtins.mzn_sub_var,
			Builtin::TimesPar { .. } => ids.builtins.mzn_times_par,
			Builtin::TimesVar { .. } => ids.builtins.mzn_times_var,
			Builtin::Pow { .. } => ids.builtins.mzn_pow,
			Builtin::NegPar(_) => ids.builtins.mzn_neg_par,
			Builtin::NegVar(_) => ids.builtins.mzn_neg_var,
			Builtin::DivInt { .. } => ids.builtins.mzn_div_int,
			Builtin::Mod { .. } => ids.builtins.mzn_mod,
			Builtin::DivFloat { .. } => ids.builtins.mzn_div_float,
			Builtin::SumPar(_) => ids.builtins.mzn_sum_par,
			Builtin::SumVar(_) => ids.builtins.mzn_sum_var,
			Builtin::ProductPar(_) => ids.builtins.mzn_product_par,
			Builtin::ProductVar(_) => ids.builtins.mzn_product_var,
			Builtin::Minimum { .. } => ids.builtins.mzn_minimum,
			Builtin::Maximum { .. } => ids.builtins.mzn_maximum,
			Builtin::MinArray(_) => ids.builtins.mzn_min_array,
			Builtin::MaxArray(_) => ids.builtins.mzn_max_array,
			Builtin::ArgMin(_) => ids.builtins.mzn_arg_min,
			Builtin::ArgMax(_) => ids.builtins.mzn_arg_max,
			Builtin::Abs(_) => ids.builtins.mzn_abs,
			Builtin::Sqrt(_) => ids.builtins.mzn_sqrt,
			Builtin::Exp(_) => ids.builtins.mzn_exp,
			Builtin::Ln(_) => ids.builtins.mzn_ln,
			Builtin::Log10(_) => ids.builtins.mzn_log10,
			Builtin::Sin(_) => ids.builtins.mzn_sin,
			Builtin::Cos(_) => ids.builtins.mzn_cos,
			Builtin::Tan(_) => ids.builtins.mzn_tan,
			Builtin::ASin(_) => ids.builtins.mzn_asin,
			Builtin::ACos(_) => ids.builtins.mzn_acos,
			Builtin::ATan(_) => ids.builtins.mzn_atan,
			Builtin::SinH(_) => ids.builtins.mzn_sinh,
			Builtin::CosH(_) => ids.builtins.mzn_cosh,
			Builtin::TanH(_) => ids.builtins.mzn_tanh,
			Builtin::ASinH(_) => ids.builtins.mzn_asinh,
			Builtin::ACosH(_) => ids.builtins.mzn_acosh,
			Builtin::ATanH(_) => ids.builtins.mzn_tanh,
		}
	}

	/// Get arguments (used for printing as MiniZinc)
	pub fn arguments<'a>(&'a self) -> Vec<&'a Value> {
		match self {
			Builtin::Format {
				width: v1,
				precision: v2,
				value: v3,
			}
			| Builtin::TraceToSection {
				section: v1,
				message: v2,
				json: v3,
			} => vec![v1, v2, v3],
			Builtin::ArrayAccess {
				array: v1,
				indices: v2,
			}
			| Builtin::ArraySlice {
				array: v1,
				slices: v2,
			}
			| Builtin::ArrayPlusPlus {
				left: v1,
				right: v2,
			}
			| Builtin::IndexSetsAgree {
				left: v1,
				right: v2,
			}
			| Builtin::ArrayXd {
				source: v1,
				array: v2,
			}
			| Builtin::ArrayKd {
				index_sets: v1,
				array: v2,
			}
			| Builtin::ComputeDivBounds {
				dividend: v1,
				divisor: v2,
			}
			| Builtin::ComputeModBounds {
				dividend: v1,
				divisor: v2,
			}
			| Builtin::ComputeFloatDivBounds {
				dividend: v1,
				divisor: v2,
			}
			| Builtin::ComputePowBounds {
				base: v1,
				exponent: v2,
			}
			| Builtin::Normal { mean: v1, std: v2 }
			| Builtin::UniformFloat {
				lower: v1,
				upper: v2,
			}
			| Builtin::UniformInt {
				lower: v1,
				upper: v2,
			}
			| Builtin::Gamma {
				alpha: v1,
				beta: v2,
			}
			| Builtin::Weibull {
				shape: v1,
				scale: v2,
			}
			| Builtin::LogNormal { mean: v1, std: v2 }
			| Builtin::Cauchy {
				mean: v1,
				scale: v2,
			}
			| Builtin::FDistribution { dof1: v1, dof2: v2 }
			| Builtin::Binomial { n: v1, p: v2 }
			| Builtin::HasAnn {
				variable: v1,
				ann: v2,
			}
			| Builtin::Annotate {
				variable: v1,
				ann: v2,
			}
			| Builtin::IsSame {
				variable: v1,
				other: v2,
			}
			| Builtin::StringPlusPlus {
				left: v1,
				right: v2,
			}
			| Builtin::StringJoin {
				delimiter: v1,
				strings: v2,
			}
			| Builtin::LessThanPar {
				left: v1,
				right: v2,
			}
			| Builtin::LessThanVar {
				left: v1,
				right: v2,
			}
			| Builtin::LessThanEqPar {
				left: v1,
				right: v2,
			}
			| Builtin::LessThanEqVar {
				left: v1,
				right: v2,
			}
			| Builtin::NotEqualPar {
				left: v1,
				right: v2,
			}
			| Builtin::NotEqualVar {
				left: v1,
				right: v2,
			}
			| Builtin::EqualPar {
				left: v1,
				right: v2,
			}
			| Builtin::EqualVar {
				left: v1,
				right: v2,
			}
			| Builtin::AndPar {
				left: v1,
				right: v2,
			}
			| Builtin::AndVar {
				left: v1,
				right: v2,
			}
			| Builtin::OrPar {
				left: v1,
				right: v2,
			}
			| Builtin::OrVar {
				left: v1,
				right: v2,
			}
			| Builtin::ImplicationPar {
				left: v1,
				right: v2,
			}
			| Builtin::ImplicationVar {
				left: v1,
				right: v2,
			}
			| Builtin::ClausePar { pos: v1, neg: v2 }
			| Builtin::ClauseVar { pos: v1, neg: v2 }
			| Builtin::SortBy {
				array: v1,
				sort_by: v2,
			}
			| Builtin::FormatJustifyString {
				width: v1,
				text: v2,
			}
			| Builtin::OutputToSection {
				section: v1,
				string: v2,
			}
			| Builtin::OutputToJsonSection {
				section: v1,
				value: v2,
			}
			| Builtin::SetRange { min: v1, max: v2 }
			| Builtin::SetInPar { value: v1, set: v2 }
			| Builtin::SetInVar { value: v1, set: v2 }
			| Builtin::SetSubsetPar {
				left: v1,
				right: v2,
			}
			| Builtin::SetSubsetVar {
				left: v1,
				right: v2,
			}
			| Builtin::SetUnionPar {
				left: v1,
				right: v2,
			}
			| Builtin::SetUnionVar {
				left: v1,
				right: v2,
			}
			| Builtin::SetIntersectPar {
				left: v1,
				right: v2,
			}
			| Builtin::SetIntersectVar {
				left: v1,
				right: v2,
			}
			| Builtin::SetDiffPar {
				left: v1,
				right: v2,
			}
			| Builtin::SetDiffVar {
				left: v1,
				right: v2,
			}
			| Builtin::SetSymDiffPar {
				left: v1,
				right: v2,
			}
			| Builtin::SetSymDiffVar {
				left: v1,
				right: v2,
			}
			| Builtin::AddPar {
				left: v1,
				right: v2,
			}
			| Builtin::AddVar {
				left: v1,
				right: v2,
			}
			| Builtin::SubPar {
				left: v1,
				right: v2,
			}
			| Builtin::SubVar {
				left: v1,
				right: v2,
			}
			| Builtin::TimesPar {
				left: v1,
				right: v2,
			}
			| Builtin::TimesVar {
				left: v1,
				right: v2,
			}
			| Builtin::Pow {
				base: v1,
				exponent: v2,
			}
			| Builtin::DivInt {
				dividend: v1,
				divisor: v2,
			}
			| Builtin::Mod {
				dividend: v1,
				divisor: v2,
			}
			| Builtin::DivFloat {
				dividend: v1,
				divisor: v2,
			}
			| Builtin::Minimum {
				left: v1,
				right: v2,
			}
			| Builtin::Maximum {
				left: v1,
				right: v2,
			} => vec![v1, v2],
			Builtin::GetParameter(value)
			| Builtin::IndexedArray(value)
			| Builtin::ForAllPar(value)
			| Builtin::ForAllVar(value)
			| Builtin::ExistsPar(value)
			| Builtin::ExistsVar(value)
			| Builtin::ArrayLength(value)
			| Builtin::IndexSets(value)
			| Builtin::TraceExp(value)
			| Builtin::Warn(value)
			| Builtin::Abort(value)
			| Builtin::LowerBound(value)
			| Builtin::UpperBound(value)
			| Builtin::LowerBoundArray(value)
			| Builtin::UpperBoundArray(value)
			| Builtin::Domain(value)
			| Builtin::DomainArray(value)
			| Builtin::DomainBoundsArray(value)
			| Builtin::HasBounds(value)
			| Builtin::HasUpperBoundSet(value)
			| Builtin::IsFixed(value)
			| Builtin::Fix(value)
			| Builtin::StringLength(value)
			| Builtin::StringConcat(value)
			| Builtin::NotPar(value)
			| Builtin::NotVar(value)
			| Builtin::XorAll(value)
			| Builtin::IffAll(value)
			| Builtin::Sort(value)
			| Builtin::Show(value)
			| Builtin::ShowDzn(value)
			| Builtin::ShowDznId(value)
			| Builtin::ShowJson(value)
			| Builtin::SetToRanges(value)
			| Builtin::Ceil(value)
			| Builtin::Floor(value)
			| Builtin::Round(value)
			| Builtin::SetToArray(value)
			| Builtin::ArrayToSet(value)
			| Builtin::NegPar(value)
			| Builtin::NegVar(value)
			| Builtin::SumPar(value)
			| Builtin::SumVar(value)
			| Builtin::ProductPar(value)
			| Builtin::ProductVar(value)
			| Builtin::MinArray(value)
			| Builtin::MaxArray(value)
			| Builtin::ArgMin(value)
			| Builtin::ArgMax(value)
			| Builtin::Abs(value)
			| Builtin::Sqrt(value)
			| Builtin::Exp(value)
			| Builtin::Ln(value)
			| Builtin::Log10(value)
			| Builtin::Sin(value)
			| Builtin::Cos(value)
			| Builtin::Tan(value)
			| Builtin::ASin(value)
			| Builtin::ACos(value)
			| Builtin::ATan(value)
			| Builtin::SinH(value)
			| Builtin::CosH(value)
			| Builtin::TanH(value)
			| Builtin::ASinH(value)
			| Builtin::ACosH(value)
			| Builtin::ATanH(value)
			| Builtin::Poisson { mean: value }
			| Builtin::Exponential { lambda: value }
			| Builtin::ChiSquared { dof: value }
			| Builtin::TDistribution { size: value }
			| Builtin::DiscreteDistribution { weights: value }
			| Builtin::Bernoulli { prob: value } => vec![value],
			Builtin::LogStreamToString
			| Builtin::CheckDebugMode
			| Builtin::CompilerVersion
			| Builtin::ShowCheckerOutput => vec![],
		}
	}
}
