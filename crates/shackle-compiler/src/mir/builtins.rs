//! Interpreter builtins
//!

use super::Value;

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
	Uniform {
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

	/// Trace
	Trace {
		/// The message to print
		message: Box<Value>,
		/// The value to return
		value: Box<Value>,
	},
	/// Trace expression
	TraceExp(Box<Value>),
	/// Trace debug
	TraceDbg(Box<Value>),
	/// Trace to section
	TraceToSection {
		/// The section
		section: Box<Value>,
		/// The message
		message: Box<Value>,
		/// Whether or not the string is JSON
		json: Box<Value>,
	},
	/// Trace to standard output
	TraceStdout {
		/// The message
		message: Box<Value>,
		/// The value to return
		value: Box<Value>,
	},
	/// Trace to log stream
	TraceLogStream {
		/// The message
		message: Box<Value>,
		/// The value to return
		value: Box<Value>,
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

	/// Less than
	LessThan {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Less than or equal to
	LessThanEq {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Greater than
	GreaterThan {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Greater than or equal to
	GreaterThanEq {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Not equal
	NotEqual {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Equality
	Equal {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},

	/// And
	And {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Or
	Or {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Implication
	Implication {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Reverse implication
	ReverseImplication {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// If and only if
	IfAndOnlyIf {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Exclusive or
	Xor {
		/// Left-hand side
		left: Box<Value>,
		/// Right-hand side
		right: Box<Value>,
	},
	/// Not
	Not(Box<Value>),
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
		/// Value to format
		value: Box<Value>,
	},
	/// Format value as string
	FormatWithPrecision {
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
	/// Test if set is a superset
	SetSupersetPar {
		/// Left-hand side set
		left: Box<Value>,
		/// Right-hand side set
		right: Box<Value>,
	},
	/// Test if set is a superset
	SetSupersetVar {
		/// Left-hand side set
		left: Box<Value>,
		/// Right-hand side set
		right: Box<Value>,
	},
	/// Set union
	SetUnion {
		/// Left-hand side set
		left: Box<Value>,
		/// Right-hand side set
		right: Box<Value>,
	},
	/// Set intersection
	SetIntersect {
		/// Left-hand side set
		left: Box<Value>,
		/// Right-hand side set
		right: Box<Value>,
	},
	/// Set difference
	SetDiff {
		/// Left-hand side set
		left: Box<Value>,
		/// Right-hand side set
		right: Box<Value>,
	},
	/// Set symmetric difference
	SetSymDiff {
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
	Set2Array(Box<Value>),

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

	/// Negation
	Neg(Box<Value>),

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
	pub fn name(&self) -> &'static str {
		match self {
			Builtin::GetParameter(_) => "mzn_get_parameter",
			Builtin::ForAllPar(_) | Builtin::ForAllVar(_) => "forall",
			Builtin::ExistsPar(_) | Builtin::ExistsVar(_) => "exists",
			Builtin::IndexedArray(_) => "mzn_indexed_array",
			Builtin::ArrayAccess { .. } => "mzn_element_internal",
			Builtin::ArraySlice { .. } => "mzn_slice_internal",
			Builtin::ArrayPlusPlus { .. } | Builtin::StringPlusPlus { .. } => "'++'",
			Builtin::ArrayLength(_) => "length",
			Builtin::IndexSetsAgree { .. } => "index_sets_agree",
			Builtin::IndexSets(_) => "index_sets",
			Builtin::ArrayXd { .. } => "arrayXd",
			Builtin::ArrayKd { .. } => "mzn_array_kd",
			Builtin::ComputeDivBounds { .. } => "compute_div_bounds",
			Builtin::ComputeModBounds { .. } => "compute_mod_bounds",
			Builtin::ComputeFloatDivBounds { .. } => "compute_float_div_bounds",
			Builtin::ComputePowBounds { .. } => "compute_pow_bounds",
			Builtin::Normal { .. } => "normal",
			Builtin::Uniform { .. } => "uniform",
			Builtin::Poisson { .. } => "poisson",
			Builtin::Gamma { .. } => "gamma",
			Builtin::Weibull { .. } => "weibull",
			Builtin::Exponential { .. } => "exponential",
			Builtin::LogNormal { .. } => "lognormal",
			Builtin::ChiSquared { .. } => "chisquared",
			Builtin::Cauchy { .. } => "cauchy",
			Builtin::FDistribution { .. } => "fdistribution",
			Builtin::TDistribution { .. } => "tdistribution",
			Builtin::DiscreteDistribution { .. } => "discrete_distribution",
			Builtin::Bernoulli { .. } => "bernoulli",
			Builtin::Binomial { .. } => "binomial",
			Builtin::Warn(_) => "mzn_add_warning",
			Builtin::Trace { .. } => "trace",
			Builtin::TraceExp { .. } => "trace_exp",
			Builtin::TraceDbg { .. } => "trace_dbg",
			Builtin::TraceToSection { .. } => "trace_to_section",
			Builtin::TraceStdout { .. } => "trace_stdout",
			Builtin::TraceLogStream { .. } => "trace_logstream",
			Builtin::LogStreamToString => "logstream_to_string",
			Builtin::Abort(_) => "abort",
			Builtin::CheckDebugMode => "mzn_internal_check_debug_mode",
			Builtin::LowerBound(_) => "lb",
			Builtin::UpperBound(_) => "ub",
			Builtin::LowerBoundArray(_) => "lb_array",
			Builtin::UpperBoundArray(_) => "ub_array",
			Builtin::Domain(_) => "dom",
			Builtin::DomainArray(_) => "dom_array",
			Builtin::DomainBoundsArray(_) => "dom_bounds_array",
			Builtin::HasBounds(_) => "has_bounds",
			Builtin::HasUpperBoundSet(_) => "has_ub_set",
			Builtin::IsFixed(_) => "is_fixed",
			Builtin::Fix(_) => "fix",
			Builtin::HasAnn { .. } => "has_ann",
			Builtin::Annotate { .. } => "annotate",
			Builtin::IsSame { .. } => "is_same",
			Builtin::CompilerVersion => "mzn_compiler_version",
			Builtin::StringLength(_) => "string_length",
			Builtin::StringConcat(_) => "concat",
			Builtin::StringJoin { .. } => "join",
			Builtin::LessThan { .. } => "'<'",
			Builtin::LessThanEq { .. } => "'<='",
			Builtin::GreaterThan { .. } => "'>'",
			Builtin::GreaterThanEq { .. } => "'>='",
			Builtin::NotEqual { .. } => "'!='",
			Builtin::Equal { .. } => "'='",
			Builtin::And { .. } => "'/\\'",
			Builtin::Or { .. } => "'\\/'",
			Builtin::Implication { .. } => "'->'",
			Builtin::ReverseImplication { .. } => "'<-'",
			Builtin::IfAndOnlyIf { .. } => "'<->'",
			Builtin::Xor { .. } => "'xor'",
			Builtin::Not(_) => "not",
			Builtin::XorAll(_) => "xorall",
			Builtin::IffAll(_) => "iffall",
			Builtin::ClausePar { .. } | Builtin::ClauseVar { .. } => "clause",
			Builtin::Sort(_) => "sort",
			Builtin::SortBy { .. } => "sort_by",
			Builtin::Show(_) => "show",
			Builtin::ShowDzn(_) => "showDzn",
			Builtin::ShowDznId(_) => "showDznId",
			Builtin::ShowCheckerOutput => "showCheckerOutput",
			Builtin::ShowJson(_) => "showJSON",
			Builtin::Format { .. } | Builtin::FormatWithPrecision { .. } => "format",
			Builtin::FormatJustifyString { .. } => "format_justify_string",
			Builtin::OutputToSection { .. } => "output_to_section",
			Builtin::OutputToJsonSection { .. } => "output_to_json_section",
			Builtin::SetRange { .. } => "'..'",
			Builtin::SetInPar { .. } | Builtin::SetInVar { .. } => "'in'",
			Builtin::SetSubsetPar { .. } | Builtin::SetSubsetVar { .. } => "'subset'",
			Builtin::SetSupersetPar { .. } | Builtin::SetSupersetVar { .. } => "'superset'",
			Builtin::SetUnion { .. } => "'union'",
			Builtin::SetIntersect { .. } => "'intersect'",
			Builtin::SetDiff { .. } => "'diff'",
			Builtin::SetSymDiff { .. } => "'symdiff'",
			Builtin::SetToRanges(_) => "set2ranges",
			Builtin::Ceil(_) => "ceil",
			Builtin::Floor(_) => "floor",
			Builtin::Round(_) => "round",
			Builtin::Set2Array(_) => "set2array",
			Builtin::AddPar { .. } | Builtin::AddVar { .. } => "'+'",
			Builtin::SubPar { .. } | Builtin::SubVar { .. } | Builtin::Neg(_) => "'-'",
			Builtin::TimesPar { .. } | Builtin::TimesVar { .. } => "'*'",
			Builtin::Pow { .. } => "pow",
			Builtin::DivInt { .. } => "'div'",
			Builtin::Mod { .. } => "'mod'",
			Builtin::DivFloat { .. } => "'/'",
			Builtin::SumPar(_) | Builtin::SumVar(_) => "sum",
			Builtin::ProductPar(_) | Builtin::ProductVar(_) => "product",
			Builtin::Minimum { .. } | Builtin::MinArray(_) => "min",
			Builtin::Maximum { .. } | Builtin::MaxArray(_) => "max",
			Builtin::ArgMin(_) => "arg_min",
			Builtin::ArgMax(_) => "arg_max",
			Builtin::Abs(_) => "abs",
			Builtin::Sqrt(_) => "sqrt",
			Builtin::Exp(_) => "exp",
			Builtin::Ln(_) => "ln",
			Builtin::Log10(_) => "log10",
			Builtin::Sin(_) => "sin",
			Builtin::Cos(_) => "cos",
			Builtin::Tan(_) => "tan",
			Builtin::ASin(_) => "asin",
			Builtin::ACos(_) => "acos",
			Builtin::ATan(_) => "atan",
			Builtin::SinH(_) => "sinh",
			Builtin::CosH(_) => "cosh",
			Builtin::TanH(_) => "tanh",
			Builtin::ASinH(_) => "asinh",
			Builtin::ACosH(_) => "acosh",
			Builtin::ATanH(_) => "atanh",
		}
	}

	/// Get arguments (used for printing as MiniZinc)
	pub fn arguments<'a>(&'a self) -> Vec<&'a Value> {
		match self {
			Builtin::FormatWithPrecision {
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
			| Builtin::Uniform {
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
			| Builtin::Trace {
				message: v1,
				value: v2,
			}
			| Builtin::TraceStdout {
				message: v1,
				value: v2,
			}
			| Builtin::TraceLogStream {
				message: v1,
				value: v2,
			}
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
			| Builtin::LessThan {
				left: v1,
				right: v2,
			}
			| Builtin::LessThanEq {
				left: v1,
				right: v2,
			}
			| Builtin::GreaterThan {
				left: v1,
				right: v2,
			}
			| Builtin::GreaterThanEq {
				left: v1,
				right: v2,
			}
			| Builtin::NotEqual {
				left: v1,
				right: v2,
			}
			| Builtin::Equal {
				left: v1,
				right: v2,
			}
			| Builtin::And {
				left: v1,
				right: v2,
			}
			| Builtin::Or {
				left: v1,
				right: v2,
			}
			| Builtin::Implication {
				left: v1,
				right: v2,
			}
			| Builtin::ReverseImplication {
				left: v1,
				right: v2,
			}
			| Builtin::IfAndOnlyIf {
				left: v1,
				right: v2,
			}
			| Builtin::Xor {
				left: v1,
				right: v2,
			}
			| Builtin::ClausePar { pos: v1, neg: v2 }
			| Builtin::ClauseVar { pos: v1, neg: v2 }
			| Builtin::SortBy {
				array: v1,
				sort_by: v2,
			}
			| Builtin::Format {
				width: v1,
				value: v2,
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
			| Builtin::SetSupersetPar {
				left: v1,
				right: v2,
			}
			| Builtin::SetSupersetVar {
				left: v1,
				right: v2,
			}
			| Builtin::SetUnion {
				left: v1,
				right: v2,
			}
			| Builtin::SetIntersect {
				left: v1,
				right: v2,
			}
			| Builtin::SetDiff {
				left: v1,
				right: v2,
			}
			| Builtin::SetSymDiff {
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
			| Builtin::TraceDbg(value)
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
			| Builtin::Not(value)
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
			| Builtin::Set2Array(value)
			| Builtin::Neg(value)
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
