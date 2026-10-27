//! Interpreter builtins
//!
//! This module defines the Builtin enum which is parameterized by the type of its arguments
//! allowing use in different levels of IR.
//!
//! The MIR non-generic Builtin type is defined in lib.rs.

use shackle_mir_derive::MirBuiltin;

/// An interpreter builtin with arguments of type `T`.
#[derive(Clone, Debug, Hash, PartialEq, Eq, MirBuiltin)]
pub enum Builtin<T> {
	/// Fresh boolean variable
	#[name(mzn_var_bool)]
	VarBool,
	/// Fresh integer variable
	#[name(mzn_var_int)]
	VarInt {
		/// The domain of the variable
		domain: T,
	},
	/// Fresh float variable
	#[name(mzn_var_float)]
	VarFloat {
		/// The domain of the variable
		domain: T,
	},
	/// Fresh set of integer variable
	#[name(mzn_var_set_of_int)]
	VarSetOfInt {
		/// The domain of the variable
		domain: T,
	},

	/// Transform list of tuples of index and value into indexed array
	#[name(mzn_indexed_array)]
	IndexedArray(T),
	/// Array access with par index
	#[name(mzn_element_internal)]
	ArrayAccess {
		/// The array to access
		array: T,
		/// The indices
		indices: T,
	},
	/// Array slicing
	#[name(mzn_slice_internal)]
	ArraySlice {
		/// The array to slice
		array: T,
		/// The indices
		slices: T,
	},
	/// Coerce array to set
	#[name(mzn_array2set)]
	ArrayToSet(T),
	/// Array length
	#[name(length)]
	ArrayLength(T),
	/// Get array index sets
	#[name(index_sets)]
	IndexSets(T),
	/// Copy index sets
	#[name(array_xd)]
	ArrayXd {
		/// Array from which to take the index sets
		source: T,
		/// Array to coerce
		array: T,
	},
	/// Change array to use the given index sets
	#[name(mzn_array_kd)]
	ArrayKd {
		/// Tuple of index sets
		index_sets: T,
		/// The array to coerce
		array: T,
	},

	// mzn_increment_counter
	/// Sample from normal distribution
	#[name(normal)]
	Normal {
		/// Mean
		mean: T,
		/// Standard deviation
		std: T,
	},
	/// Sample from uniform distribution
	#[name(uniform_internal_float)]
	UniformFloat {
		/// Lower bound
		lower: T,
		/// Upper bound
		upper: T,
	},
	/// Sample from uniform distribution
	#[name(uniform_internal_int)]
	UniformInt {
		/// Lower bound
		lower: T,
		/// Upper bound
		upper: T,
	},
	/// Sample from Poisson distribution
	#[name(poisson)]
	Poisson {
		/// Mean
		mean: T,
	},
	/// Sample from gamma distribution
	#[name(gamma)]
	Gamma {
		/// Alpha
		alpha: T,
		/// Beta
		beta: T,
	},
	/// Sample from weibull distribution
	#[name(weibull)]
	Weibull {
		/// Shape
		shape: T,
		/// Scale
		scale: T,
	},
	/// Sample from exponential distribution
	#[name(exponential)]
	Exponential {
		/// Lambda
		lambda: T,
	},
	/// Sample from lognormal distribution
	#[name(lognormal)]
	LogNormal {
		/// Mean
		mean: T,
		/// Standard deviation
		std: T,
	},
	/// Sample from chi-squared distribution
	#[name(chisquared)]
	ChiSquared {
		/// Degree of freedom
		dof: T,
	},
	/// Sample from Cauchy distribution
	#[name(cauchy)]
	Cauchy {
		/// Mean
		mean: T,
		/// Scale
		scale: T,
	},
	/// Sample from Fisher-Snedecor F-distribution
	#[name(fdistribution)]
	FDistribution {
		/// First degree of freedom
		dof1: T,
		/// Second degree of freedom
		dof2: T,
	},
	/// Sample from student's T-distribution
	#[name(tdistribution)]
	TDistribution {
		/// Sample size
		size: T,
	},
	/// Sample from discrete distribution
	#[name(discrete_distribution)]
	DiscreteDistribution {
		/// Weights
		weights: T,
	},
	/// Sample from Bernoulli distribution
	#[name(bernoulli)]
	Bernoulli {
		/// Probability
		prob: T,
	},
	/// Sample from binomial distribution
	#[name(binomial)]
	Binomial {
		/// Number of trials
		n: T,
		/// Probability
		p: T,
	},

	/// Emit a warning
	#[name(mzn_add_warning)]
	Warn(T),

	/// Trace expression
	#[name(trace_exp)]
	TraceExp(T),
	/// Trace to section
	#[name(mzn_trace_to_section)]
	TraceToSection {
		/// The section
		section: T,
		/// The message
		message: T,
		/// Whether or not the string is JSON
		json: T,
	},
	/// Output log stream as string
	#[name(logstream_to_string)]
	LogStreamToString,
	/// Abort
	#[name(abort)]
	Abort(T),
	/// Check if in debug mode
	#[name(mzn_internal_check_debug_mode)]
	CheckDebugMode,

	/// Lower bound
	#[name(lb)]
	LowerBound(T),
	/// Upper bound
	#[name(ub)]
	UpperBound(T),
	/// Min of lower bounds in array
	#[name(lb_array)]
	LowerBoundArray(T),
	/// Max of upper bounds in array
	#[name(ub_array)]
	UpperBoundArray(T),
	/// Get domain
	#[name(dom)]
	Domain(T),
	/// Union of domains of array elements
	#[name(dom_array)]
	DomainArray(T),
	/// Approximation of union of domains of array elements
	#[name(dom_bounds_array)]
	DomainBoundsArray(T),

	/// Whether variable has declared, finite bounds
	#[name(has_bounds)]
	HasBounds(T),
	/// Whether set has declared, finite upper bound
	#[name(has_ub_set)]
	HasUpperBoundSet(T),

	/// Whether the variable is fixed
	#[name(is_fixed)]
	IsFixed(T),
	/// Fix the value or abort
	#[name(fix)]
	Fix(T),

	/// Whether or not a variable has an annotation
	#[name(has_ann)]
	HasAnn {
		/// The variable
		variable: T,
		/// The annotation
		ann: T,
	},
	/// Annotate a declaration
	#[name(annotate)]
	Annotate {
		/// The variable
		variable: T,
		/// The annotation
		ann: T,
	},

	/// Whether these are the same variable
	#[name(is_same)]
	IsSame {
		/// The variable
		variable: T,
		/// The other variable
		other: T,
	},

	/// Compiler version
	#[name(mzn_compiler_version)]
	CompilerVersion,

	/// Concatenate two strings
	#[name(plus_plus)]
	StringPlusPlus {
		/// Left-hand side string
		left: T,
		/// Right-hand side string
		right: T,
	},
	/// Get length of string
	#[name(string_length)]
	StringLength(T),
	/// Par less than
	#[name(lt)]
	LessThan {
		/// Left-hand side
		left: T,
		/// Right-hand side
		right: T,
	},
	/// Par equality
	#[name(eq)]
	Equal {
		/// Left-hand side
		left: T,
		/// Right-hand side
		right: T,
	},

	/// Sort an array
	#[name(sort)]
	Sort(T),
	/// Sort an array by values in another array
	#[name(sort_by)]
	SortBy {
		/// Array to sort
		array: T,
		/// Values to sort by
		sort_by: T,
	},

	/// Show an expression
	#[name(show_internal)]
	Show(T),
	/// Show in DZN format
	#[name(show_dzn)]
	ShowDzn(T),
	/// Show a string as a DZN identifier
	#[name(show_dzn_id)]
	ShowDznId(T),
	/// Show checker output
	#[name(show_checker_output)]
	ShowCheckerOutput,
	/// Show in JSON format
	#[name(show_json_internal)]
	ShowJson(T),

	/// Format value as string
	#[name(format)]
	Format {
		/// Width
		width: T,
		/// Precision for floats
		precision: T,
		/// Value<'db> to format
		value: T,
	},
	/// Format value as string using the default precision
	#[name(format)]
	FormatDefault {
		/// Width
		width: T,
		/// Value to format
		value: T,
	},
	/// Format justified string
	#[name(format_justify_string)]
	FormatJustifyString {
		/// The width
		width: T,
		/// The text to format
		text: T,
	},
	/// Format justified string with a precision
	#[name(format_justify_string)]
	FormatJustifyStringWithPrecision {
		/// The width
		width: T,
		/// The precision
		precision: T,
		/// The text to format
		text: T,
	},

	/// Output to a section
	#[name(output_to_section)]
	OutputToSection {
		/// Section
		section: T,
		/// Value<'db>
		string: T,
	},
	/// Output to a JSON section
	#[name(output_to_json_section)]
	OutputToJsonSection {
		/// Section
		section: T,
		/// Value<'db>
		value: T,
	},
	/// Create integer range
	#[name(range_internal_int)]
	SetRangeInt {
		/// Min
		min: T,
		/// Max
		max: T,
	},
	/// Create float range
	#[name(range_internal_float)]
	SetRangeFloat {
		/// Min
		min: T,
		/// Max
		max: T,
	},

	/// Integer set to ranges
	#[name(set_to_ranges_internal_int)]
	SetToRangesInt(T),
	/// Float set to ranges
	#[name(set_to_ranges_internal_float)]
	SetToRangesFloat(T),

	/// Cardinality of a par set
	#[name(card)]
	SetCard(T),
	/// Union of two par int sets
	#[name(union_internal_int)]
	SetUnionInt {
		/// Left-hand side
		left: T,
		/// Right-hand side
		right: T,
	},
	/// Union of two par float sets
	#[name(union_internal_float)]
	SetUnionFloat {
		/// Left-hand side
		left: T,
		/// Right-hand side
		right: T,
	},
	/// Intersection of two par int sets
	#[name(intersect_internal_int)]
	SetIntersectInt {
		/// Left-hand side
		left: T,
		/// Right-hand side
		right: T,
	},
	/// Union of two par float sets
	#[name(intersect_internal_float)]
	SetIntersectFloat {
		/// Left-hand side
		left: T,
		/// Right-hand side
		right: T,
	},
	/// Difference of two par int sets
	#[name(diff_internal_int)]
	SetDiffInt {
		/// Left-hand side
		left: T,
		/// Right-hand side
		right: T,
	},
	/// Difference of two par float sets
	#[name(diff_internal_float)]
	SetDiffFloat {
		/// Left-hand side
		left: T,
		/// Right-hand side
		right: T,
	},

	/// Ceiling
	#[name(ceil)]
	Ceil(T),
	/// Floor
	#[name(floor)]
	Floor(T),
	/// Round
	#[name(round)]
	Round(T),
	/// Set to array
	#[name(set2array)]
	SetToArray(T),

	/// Par addition
	#[name(plus)]
	Add {
		/// Left-hand side
		left: T,
		/// Right-hand side
		right: T,
	},

	/// Par subtraction
	#[name(minus)]
	Sub {
		/// Left-hand side
		left: T,
		/// Right-hand side
		right: T,
	},

	/// Par multiplication
	#[name(times)]
	Times {
		/// Left-hand side
		left: T,
		/// Right-hand side
		right: T,
	},

	/// Raise integer to power
	#[name(pow_internal_int)]
	PowInt {
		/// Base
		base: T,
		/// Exponent
		exponent: T,
	},
	/// Raise float to power
	#[name(pow_internal_float)]
	PowFloat {
		/// Base
		base: T,
		/// Exponent
		exponent: T,
	},

	/// Par negation
	#[name(minus)]
	Neg(T),

	/// Integer (truncating) division
	#[name(div_internal_int)]
	DivInt {
		/// Left-hand side
		dividend: T,
		/// Right-hand side
		divisor: T,
	},
	/// Floating point division
	#[name(div_internal_float)]
	DivFloat {
		/// Left-hand side
		dividend: T,
		/// Right-hand side
		divisor: T,
	},

	/// Minimum value of array
	#[name(min_internal_array)]
	MinArray(T),
	/// Minimum value of set
	#[name(min_internal_set)]
	MinSet(T),
	/// Maximum value of array
	#[name(max_internal_array)]
	MaxArray(T),
	/// Maximum value of set
	#[name(max_internal_set)]
	MaxSet(T),
	/// Square root
	#[name(sqrt)]
	Sqrt(T),
	/// Exponentiation
	#[name(exp)]
	Exp(T),
	/// Natural logarithm
	#[name(ln)]
	Ln(T),
	/// Base 10 logarithm
	#[name(log10)]
	Log10(T),

	/// Sine function
	#[name(sin)]
	Sin(T),
	/// Cosine function
	#[name(cos)]
	Cos(T),
	/// Tangent function
	#[name(tan)]
	Tan(T),
	/// Arcsine function
	#[name(asin)]
	ASin(T),
	/// Arccosine function
	#[name(acos)]
	ACos(T),
	/// Arctangent function
	#[name(atan)]
	ATan(T),
	/// Hyperbolic sine function
	#[name(sinh)]
	SinH(T),
	/// Hyperbolic cosine function
	#[name(cosh)]
	CosH(T),
	/// Hyperbolic tangent function
	#[name(tanh)]
	TanH(T),
	/// Hyperbolic arcsine function
	#[name(asinh)]
	ASinH(T),
	/// Hyperbolic arccosine function
	#[name(acosh)]
	ACosH(T),
	/// Hyperbolic arctangent function
	#[name(atanh)]
	ATanH(T),
}

#[cfg(test)]
mod tests {
	use shackle_hir::{
		CompilerDatabase, Identifier, IntegerLiteral, builtins::builtins,
		constants::IdentifierRegistry,
	};
	use shackle_thir::source::Origin;
	use shackle_ty::registry::TypeRegistry;

	use super::Builtin;
	use crate::{LiteralData, Value};

	#[test]
	fn new_only_consumes_arguments_for_a_matching_builtin() {
		let db = CompilerDatabase::default();
		let ids = IdentifierRegistry::lookup(&db);
		let tys = TypeRegistry::lookup(&db);
		let argument = Value::new(
			LiteralData::Integer(IntegerLiteral(1)),
			tys.par_int,
			Origin::Introduced("builtin test"),
		);
		let mut arguments = vec![argument.clone()];
		assert!(Builtin::new(&db, Identifier::new(&db, "not_a_builtin"), &mut arguments).is_none());
		assert_eq!(arguments, vec![argument]);

		let builtin = Builtin::new(&db, ids.functions.show_internal, &mut arguments);
		assert!(matches!(builtin, Some(Builtin::Show(_))));
		assert!(arguments.is_empty());
	}

	#[test]
	fn map_preserves_builtin_and_argument_order() {
		let builtin = Builtin::Add { left: 1, right: 2 };
		let mut visited = Vec::new();
		assert_eq!(
			builtin.map(|value| {
				visited.push(*value);
				value * 10
			}),
			Builtin::Add {
				left: 10,
				right: 20
			}
		);
		assert_eq!(visited, [1, 2]);
		assert_eq!(builtin, Builtin::Add { left: 1, right: 2 });
		assert_eq!(
			Builtin::<i32>::VarBool.map(|value| value * 10),
			Builtin::VarBool
		);
	}

	#[test]
	fn builtin_names_and_arities_are_unique() {
		let db = CompilerDatabase::default();
		let signatures = crate::Builtin::signatures(&db);
		for (index, signature) in signatures.iter().copied().enumerate() {
			for other in signatures[index + 1..].iter().copied() {
				assert_ne!(
					signature, other,
					"two MIR builtins have the same registry key"
				);
			}
		}
	}

	#[test]
	fn uses_redefined_internal_builtin_names() {
		let db = CompilerDatabase::default();
		let ids = IdentifierRegistry::lookup(&db);
		let signatures = crate::Builtin::signatures(&db);
		let cases = [
			(ids.functions.string_length, 1),
			(ids.functions.range_internal_int, 2),
			(ids.functions.range_internal_float, 2),
			(ids.functions.uniform_internal_int, 2),
			(ids.functions.uniform_internal_float, 2),
			(ids.functions.div_internal_int, 2),
			(ids.functions.div_internal_float, 2),
			(ids.functions.pow_internal_int, 2),
			(ids.functions.pow_internal_float, 2),
		];
		for signature in cases {
			assert!(signatures.contains(&signature));
		}
	}

	#[test]
	fn registry_entries_exist_in_hir_builtins() {
		let db = CompilerDatabase::default();
		let ids = IdentifierRegistry::lookup(&db);
		let signatures = builtins(&db)
			.iter()
			.map(|item| {
				let function = item.function(&db);
				let name = function.data()[function.pattern].identifier().unwrap();
				(name, function.parameters.len())
			})
			.collect::<Vec<_>>();

		for key in crate::Builtin::signatures(&db) {
			if key.0 == ids.functions.mzn_var_bool
				|| key.0 == ids.functions.mzn_var_int
				|| key.0 == ids.functions.mzn_var_set_of_int
				|| key.0 == ids.functions.mzn_var_float
			{
				continue;
			}
			assert!(
				signatures.contains(&key),
				"MIR builtin has no matching HIR builtin {} with arity {}",
				key.0.lookup(&db),
				key.1
			);
		}

		assert!(crate::Builtin::signatures(&db).contains(&(ids.functions.show_internal, 1)));
	}
}
