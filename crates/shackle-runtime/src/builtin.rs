//! Interpreter builtins

use itertools::Itertools;
use rand::rng;
use rand_distr::{
	Bernoulli, Binomial, Cauchy, ChiSquared, Distribution, Exp, FisherF, Gamma, LogNormal, Normal,
	Poisson, StudentT, Uniform, Weibull, weighted::WeightedIndex,
};
use rangelist::IntervalIterator;

use crate::{
	FloatVal, IntVal, Interpreter, Value,
	error::{ArithmeticError, FloatSetCoercionError, InvalidIndexSet, ValueError},
	value::{DataView, INT_SET_EMPTY, INT_SET_INF},
};

/// Builtin call with evaluated arguments
pub type Builtin<'db> = shackle_ir::Builtin<'db, Value>;

/// Evaluate a builtin call
pub fn eval_builtin<'db>(interpreter: &mut Interpreter<'_, 'db>, builtin: Builtin<'db>) -> Value {
	macro_rules! float_math {
		($value:expr, $operation:expr) => {
			float_math($value, $operation).unwrap_or_else(|e| {
				interpreter.abort(&format!("Arithmetic error: {}", e.reason));
				0.into()
			})
		};
	}
	macro_rules! sample {
		($distribution:expr) => {
			$distribution.unwrap_or_else(|e: ValueError| {
				interpreter.abort(&format!("Invalid distribution parameters: {}", e.reason));
				0.into()
			})
		};
	}

	match builtin {
		Builtin::Abort(message) => {
			interpreter.abort(&format!("Abort: {}", message.as_str()));
			true.into()
		}
		Builtin::VarBool => interpreter.new_var_bool(),
		Builtin::VarInt { domain } => interpreter.new_var_int(domain),
		Builtin::VarFloat { domain } => interpreter.new_var_float(domain),
		Builtin::VarSetOfInt { domain } => interpreter.new_var_set_of_int(domain),
		Builtin::BoolToInt(v) => v, // Same representation, so no-op
		Builtin::IntToFloat(v) => int_to_float(v).unwrap_or_else(|e| {
			interpreter.abort(&format!("Float set coercion error: {}", e.reason));
			0.into()
		}),
		Builtin::Annotate { variable, ann } => true.into(),
		Builtin::Add { left, right } => plus(left, right).unwrap_or_else(|e| {
			interpreter.abort(&format!("Arithmetic error: {}", e.reason));
			0.into()
		}),
		Builtin::Sub { left, right } => minus(left, right).unwrap_or_else(|e| {
			interpreter.abort(&format!("Arithmetic error: {}", e.reason));
			0.into()
		}),
		Builtin::Neg(value) => negate(value),
		Builtin::Times { left, right } => times(left, right).unwrap_or_else(|e| {
			interpreter.abort(&format!("Arithmetic error: {}", e.reason));
			0.into()
		}),
		Builtin::DivInt { dividend, divisor } => int_div(dividend, divisor).unwrap_or_else(|e| {
			interpreter.abort(&format!("Arithmetic error: {}", e.reason));
			0.into()
		}),
		Builtin::DivFloat { dividend, divisor } => {
			float_div(dividend, divisor).unwrap_or_else(|e| {
				interpreter.abort(&format!("Arithmetic error: {}", e.reason));
				0.into()
			})
		}
		Builtin::SetUnionInt { left, right } => int_set_union(left, right),
		Builtin::SetIntersectInt { left, right } => int_set_intersection(left, right),
		Builtin::SetDiffInt { left, right } => int_set_difference(left, right),
		Builtin::SetUnionFloat { left, right } => {
			float_set_union(left, right).unwrap_or_else(|e| {
				interpreter.abort(&format!("Float set coercion error: {}", e.reason));
				0.into()
			})
		}
		Builtin::SetIntersectFloat { left, right } => float_set_intersection(left, right)
			.unwrap_or_else(|e| {
				interpreter.abort(&format!("Float set coercion error: {}", e.reason));
				0.into()
			}),
		Builtin::IndexedArray(_) => todo!("Builtin::IndexedArray"),
		Builtin::ArrayAccess { array, indices } => {
			array_access(array, indices).unwrap_or_else(|e| {
				interpreter.abort(&format!("Arithmetic error: {}", e.reason));
				0.into()
			})
		}
		Builtin::ArraySlice { array, slices } => array_slice(array, slices),
		Builtin::ArrayToIntSet(x) => array_to_int_set(x),
		Builtin::ArrayToFloatSet(x) => array_to_float_set(x),
		Builtin::ArrayToDiscreteSet(x) => array_to_discrete_set(x),
		Builtin::ArrayLength(x) => array_len(x),
		Builtin::IndexSets(x) => index_sets(x),
		Builtin::ArrayXd { source, array } => array_xd(source, array),
		Builtin::ArrayKd { index_sets, array } => array_kd(index_sets, array).unwrap_or_else(|e| {
			interpreter.abort(&format!("Invalid index set: {}", e.reason));
			0.into()
		}),
		Builtin::Normal { mean, std } => {
			sample!(sample_float(Normal::new(float_arg(mean), float_arg(std))))
		}
		Builtin::UniformFloat { lower, upper } => {
			sample!(sample_float(Uniform::new(
				float_arg(lower),
				float_arg(upper)
			)))
		}
		Builtin::UniformInt { lower, upper } => sample!(uniform_int(lower, upper)),
		Builtin::Poisson { mean } => sample!(sample_float(Poisson::new(float_arg(mean)))),
		Builtin::Gamma { alpha, beta } => {
			sample!(sample_float(Gamma::new(float_arg(alpha), float_arg(beta))))
		}
		Builtin::Weibull { shape, scale } => sample!(sample_float(Weibull::new(
			float_arg(scale),
			float_arg(shape)
		))),
		Builtin::Exponential { lambda } => sample!(sample_float(Exp::new(float_arg(lambda)))),
		Builtin::LogNormal { mean, std } => sample!(sample_float(LogNormal::new(
			float_arg(mean),
			float_arg(std)
		))),
		Builtin::ChiSquared { dof } => sample!(sample_float(ChiSquared::new(float_arg(dof)))),
		Builtin::Cauchy { mean, scale } => {
			sample!(sample_float(Cauchy::new(float_arg(mean), float_arg(scale))))
		}
		Builtin::FDistribution { dof1, dof2 } => {
			sample!(sample_float(FisherF::new(float_arg(dof1), float_arg(dof2))))
		}
		Builtin::TDistribution { size } => sample!(sample_float(StudentT::new(float_arg(size)))),
		Builtin::DiscreteDistribution { weights } => sample!(discrete_distribution(weights)),
		Builtin::Bernoulli { prob } => sample!(sample_bool(Bernoulli::new(float_arg(prob)))),
		Builtin::Binomial { n, p } => sample!(binomial(n, p)),
		Builtin::Warn(message) => {
			eprintln!("Warning: {}", message.as_str());
			true.into()
		}
		Builtin::TraceExp(_) => todo!("Builtin::TraceExp"),
		Builtin::TraceToSection { .. } => todo!("Builtin::TraceToSection"),
		Builtin::LogStreamToString => todo!("Builtin::LogStreamToString"),
		Builtin::CheckDebugMode => todo!("Builtin::CheckDebugMode"),
		Builtin::LowerBound(x) => lower_bound(x),
		Builtin::UpperBound(x) => upper_bound(x),
		Builtin::LowerBoundArray(x) => lower_bound_array(x),
		Builtin::UpperBoundArray(x) => upper_bound_array(x),
		Builtin::Domain(x) => domain(x),
		Builtin::DomainArray(_) => todo!("Builtin::DomainArray"),
		Builtin::DomainBoundsArray(_) => todo!("Builtin::DomainBoundsArray"),
		Builtin::HasBounds(_) => todo!("Builtin::HasBounds"),
		Builtin::HasUpperBoundSet(_) => todo!("Builtin::HasUpperBoundSet"),
		Builtin::IsFixed(x) => is_fixed(x).into(),
		Builtin::Fix(x) => fix(x).unwrap_or_else(|e| {
			interpreter.abort(&format!("Value error: {}", e.reason));
			0.into()
		}),
		Builtin::HasAnn { .. } => todo!("Builtin::HasAnn"),
		Builtin::IsSame { variable, other } => (variable == other).into(),
		Builtin::CompilerVersion => compiler_version(),
		Builtin::StringPlusPlus { left, right } => string_concat(left, right),
		Builtin::StringLength(x) => string_len(x),
		Builtin::LessThan { left, right } => (left < right).into(),
		Builtin::Equal { left, right } => (left == right).into(),
		Builtin::Sort(x) => array_sort(x),
		Builtin::SortBy { array, sort_by } => array_sort_by(array, sort_by).unwrap_or_else(|e| {
			interpreter.abort(&format!("Invalid value: {}", e.reason));
			0.into()
		}),
		Builtin::Show(_) => todo!("Builtin::Show"),
		Builtin::ShowDzn(_) => todo!("Builtin::ShowDzn"),
		Builtin::ShowDznId(_) => todo!("Builtin::ShowDznId"),
		Builtin::ShowCheckerOutput => todo!("Builtin::ShowCheckerOutput"),
		Builtin::ShowJson(_) => todo!("Builtin::ShowJson"),
		Builtin::Format { .. } => todo!("Builtin::Format"),
		Builtin::FormatDefault { .. } => todo!("Builtin::FormatDefault"),
		Builtin::FormatJustifyString { .. } => todo!("Builtin::FormatJustifyString"),
		Builtin::FormatJustifyStringWithPrecision { .. } => {
			todo!("Builtin::FormatJustifyStringWithPrecision")
		}
		Builtin::OutputToSection { .. } => todo!("Builtin::OutputToSection"),
		Builtin::OutputToJsonSection { .. } => todo!("Builtin::OutputToJsonSection"),
		Builtin::SetRangeInt { min, max } => range_int(min, max),
		Builtin::SetRangeFloat { min, max } => range_float(min, max),
		Builtin::SetToRangesInt(x) => set_to_ranges_int(x),
		Builtin::SetToRangesFloat(x) => set_to_ranges_float(x).unwrap_or_else(|e| {
			interpreter.abort(&format!("Float set coercion error: {}", e.reason));
			0.into()
		}),
		Builtin::SetCard(x) => card(x),
		Builtin::SetDiffFloat { left, right } => {
			float_set_difference(left, right).unwrap_or_else(|e| {
				interpreter.abort(&format!("Float set coercion error: {}", e.reason));
				0.into()
			})
		}
		Builtin::Ceil(x) => ceil(x).unwrap_or_else(|e| {
			interpreter.abort(&format!("Arithmetic error: {}", e.reason));
			0.into()
		}),
		Builtin::Floor(x) => floor(x).unwrap_or_else(|e| {
			interpreter.abort(&format!("Arithmetic error: {}", e.reason));
			0.into()
		}),
		Builtin::Round(x) => round(x).unwrap_or_else(|e| {
			interpreter.abort(&format!("Arithmetic error: {}", e.reason));
			0.into()
		}),
		Builtin::SetToArray(x) => set_to_array(x).unwrap_or_else(|e| {
			interpreter.abort(&format!("Value error: {}", e.reason));
			0.into()
		}),
		Builtin::PowInt { base, exponent } => int_pow(base, exponent).unwrap_or_else(|e| {
			interpreter.abort(&format!("Arithmetic error: {}", e.reason));
			0.into()
		}),
		Builtin::PowFloat { base, exponent } => float_pow(base, exponent).unwrap_or_else(|e| {
			interpreter.abort(&format!("Arithmetic error: {}", e.reason));
			0.into()
		}),
		Builtin::MinArray(x) => min(x),
		Builtin::MinSet(x) => min(x),
		Builtin::MaxArray(x) => max(x),
		Builtin::MaxSet(x) => max(x),
		Builtin::Sqrt(x) => sqrt(x).unwrap_or_else(|e| {
			interpreter.abort(&format!("Arithmetic error: {}", e.reason));
			0.into()
		}),
		Builtin::Exp(x) => float_math!(x, FloatVal::exp),
		Builtin::Ln(x) => float_math!(x, FloatVal::ln),
		Builtin::Log10(x) => float_math!(x, FloatVal::log10),
		Builtin::Sin(x) => float_math!(x, FloatVal::sin),
		Builtin::Cos(x) => float_math!(x, FloatVal::cos),
		Builtin::Tan(x) => float_math!(x, FloatVal::tan),
		Builtin::ASin(x) => float_math!(x, FloatVal::asin),
		Builtin::ACos(x) => float_math!(x, FloatVal::acos),
		Builtin::ATan(x) => float_math!(x, FloatVal::atan),
		Builtin::SinH(x) => float_math!(x, FloatVal::sinh),
		Builtin::CosH(x) => float_math!(x, FloatVal::cosh),
		Builtin::TanH(x) => float_math!(x, FloatVal::tanh),
		Builtin::ASinH(x) => float_math!(x, FloatVal::asinh),
		Builtin::ACosH(x) => float_math!(x, FloatVal::acosh),
		Builtin::ATanH(x) => float_math!(x, FloatVal::atanh),
	}
}

const INVALID_DISTRIBUTION: &str = "invalid distribution parameters";

fn float_arg(value: Value) -> f64 {
	value.as_float().into()
}

fn int_arg(value: Value) -> Result<i64, ValueError> {
	match value.as_int() {
		IntVal::Int(value) => Ok(value),
		IntVal::InfPos | IntVal::InfNeg => Err(ValueError {
			reason: INVALID_DISTRIBUTION,
		}),
	}
}

fn sample_float<D, E>(distribution: Result<D, E>) -> Result<Value, ValueError>
where
	D: Distribution<f64>,
{
	distribution
		.map(|distribution| distribution.sample(&mut rng()).into())
		.map_err(|_| ValueError {
			reason: INVALID_DISTRIBUTION,
		})
}

fn sample_bool<E>(distribution: Result<Bernoulli, E>) -> Result<Value, ValueError> {
	distribution
		.map(|distribution| distribution.sample(&mut rng()).into())
		.map_err(|_| ValueError {
			reason: INVALID_DISTRIBUTION,
		})
}

fn uniform_int(lower: Value, upper: Value) -> Result<Value, ValueError> {
	let distribution =
		Uniform::new_inclusive(int_arg(lower)?, int_arg(upper)?).map_err(|_| ValueError {
			reason: INVALID_DISTRIBUTION,
		})?;
	Ok(distribution.sample(&mut rng()).into())
}

fn binomial(n: Value, p: Value) -> Result<Value, ValueError> {
	let n = int_arg(n)?.try_into().map_err(|_| ValueError {
		reason: INVALID_DISTRIBUTION,
	})?;
	let distribution = Binomial::new(n, float_arg(p)).map_err(|_| ValueError {
		reason: INVALID_DISTRIBUTION,
	})?;
	let sample: u64 = distribution.sample(&mut rng());
	let sample: i64 = sample.try_into().map_err(|_| ValueError {
		reason: INVALID_DISTRIBUTION,
	})?;
	Ok(sample.into())
}

fn discrete_distribution(weights: Value) -> Result<Value, ValueError> {
	let weights = weights.as_seq();
	if weights.dims() != 1 {
		return Err(ValueError {
			reason: INVALID_DISTRIBUTION,
		});
	}
	let (lower, _) = weights.dim(1);
	let weights: Vec<f64> = weights
		.iter()
		.map(|weight| float_arg(weight.clone()))
		.collect();
	let distribution = WeightedIndex::new(weights).map_err(|_| ValueError {
		reason: INVALID_DISTRIBUTION,
	})?;
	let index: i64 = distribution
		.sample(&mut rng())
		.try_into()
		.map_err(|_| ValueError {
			reason: INVALID_DISTRIBUTION,
		})?;
	lower.checked_add(index).map(Value::from).ok_or(ValueError {
		reason: INVALID_DISTRIBUTION,
	})
}

fn int_to_float(x: Value) -> Result<Value, FloatSetCoercionError> {
	match x.deref() {
		DataView::Int(v) => Ok(FloatVal::from(v).into()),
		DataView::IntSet(s) if s.is_finite() => Ok(Value::from_iter(s.values().map(|v| {
			let f = FloatVal::from(v);
			f..=f
		}))),
		DataView::IntSet(_) => Err(FloatSetCoercionError {
			reason: INFINITE_INT_SET_COERCION,
		}),
		_ => panic!("Expected int or int set, found {:?}", x),
	}
}

fn plus(a: Value, b: Value) -> Result<Value, ArithmeticError> {
	if matches!((a.deref(), b.deref()), (DataView::Int(_), DataView::Int(_))) {
		(a.as_int() + b.as_int()).map(Value::from)
	} else {
		(a.as_float() + b.as_float()).map(Value::from)
	}
}

fn minus(a: Value, b: Value) -> Result<Value, ArithmeticError> {
	if matches!((a.deref(), b.deref()), (DataView::Int(_), DataView::Int(_))) {
		(a.as_int() - b.as_int()).map(Value::from)
	} else {
		(a.as_float() - b.as_float()).map(Value::from)
	}
}

fn negate(a: Value) -> Value {
	match a.deref() {
		DataView::Int(x) => (-x).into(),
		DataView::Float(x) => (-x).into(),
		_ => unreachable!(),
	}
}

fn times(a: Value, b: Value) -> Result<Value, ArithmeticError> {
	if matches!((a.deref(), b.deref()), (DataView::Int(_), DataView::Int(_))) {
		(a.as_int() * b.as_int()).map(Value::from)
	} else {
		(a.as_float() * b.as_float()).map(Value::from)
	}
}

fn int_div(a: Value, b: Value) -> Result<Value, ArithmeticError> {
	(a.as_int() / b.as_int()).map(Value::from)
}

fn float_div(a: Value, b: Value) -> Result<Value, ArithmeticError> {
	(a.as_float() / b.as_float()).map(Value::from)
}

fn int_set_union(a: Value, b: Value) -> Value {
	a.as_int_set().union(&b.as_int_set())
}

fn int_set_intersection(a: Value, b: Value) -> Value {
	a.as_int_set().intersect(&b.as_int_set())
}

fn int_set_difference(a: Value, b: Value) -> Value {
	a.as_int_set().diff(&b.as_int_set())
}

const INFINITE_INT_SET_COERCION: &str = "cannot coerce an infinite integer set to a float set";

fn float_set_union(a: Value, b: Value) -> Result<Value, FloatSetCoercionError> {
	if let (DataView::IntSet(x), DataView::IntSet(y)) = (a.deref(), b.deref()) {
		return Ok(x.union(&y));
	}
	ensure_finite_int_set_coercions(&a, &b)?;
	let x = a.as_float_set();
	let y = b.as_float_set();
	Ok(x.union(&y))
}

fn float_set_intersection(a: Value, b: Value) -> Result<Value, FloatSetCoercionError> {
	if let (DataView::IntSet(x), DataView::IntSet(y)) = (a.deref(), b.deref()) {
		return Ok(x.intersect(&y));
	}
	ensure_finite_int_set_coercions(&a, &b)?;
	let x = a.as_float_set();
	let y = b.as_float_set();
	Ok(x.intersect(&y))
}

fn float_set_difference(a: Value, b: Value) -> Result<Value, FloatSetCoercionError> {
	if let (DataView::IntSet(x), DataView::IntSet(y)) = (a.deref(), b.deref()) {
		return Ok(x.diff(&y));
	}
	ensure_finite_int_set_coercions(&a, &b)?;
	let x = a.as_float_set();
	let y = b.as_float_set();
	Ok(x.diff(&y))
}

fn ensure_finite_int_set_coercions(a: &Value, b: &Value) -> Result<(), FloatSetCoercionError> {
	for value in [a, b] {
		if let DataView::IntSet(set) = value.deref()
			&& !set.is_finite()
		{
			return Err(FloatSetCoercionError {
				reason: INFINITE_INT_SET_COERCION,
			});
		}
	}
	Ok(())
}

fn floor(a: Value) -> Result<Value, ArithmeticError> {
	if let DataView::Int(_) = a.deref() {
		return Ok(a.clone());
	}
	a.as_float().floor().map(Value::from)
}

fn ceil(a: Value) -> Result<Value, ArithmeticError> {
	if let DataView::Int(_) = a.deref() {
		return Ok(a.clone());
	}
	a.as_float().ceil().map(Value::from)
}

fn round(a: Value) -> Result<Value, ArithmeticError> {
	if let DataView::Int(_) = a.deref() {
		return Ok(a.clone());
	}
	a.as_float().round().map(Value::from)
}

fn sqrt(x: Value) -> Result<Value, ArithmeticError> {
	x.as_float().sqrt().map(Value::from)
}

fn float_math(
	x: Value,
	operation: fn(&FloatVal) -> Result<FloatVal, ArithmeticError>,
) -> Result<Value, ArithmeticError> {
	let x = x.as_float();
	operation(&x).map(Value::from)
}

fn int_pow(a: Value, b: Value) -> Result<Value, ArithmeticError> {
	a.as_int().pow(b.as_int()).map(Value::from)
}

fn float_pow(a: Value, b: Value) -> Result<Value, ArithmeticError> {
	a.as_float().pow(b.as_float()).map(Value::from)
}

fn array_access(array: Value, index: Value) -> Result<Value, ArithmeticError> {
	let indices: Vec<i64> = match index.deref() {
		DataView::Int(_) => vec![index.as_int().try_into()?],
		DataView::Seq(indices) => indices
			.iter()
			.map(|v| v.as_int().try_into())
			.collect::<Result<Vec<_>, _>>()?,
		_ => panic!("expected value to be an integer literal or array of integer literals"),
	};
	Ok(array.as_seq()[&indices].clone())
}

fn min(x: Value) -> Value {
	match x.deref() {
		DataView::Seq(seq) => seq.iter().min().unwrap().clone(),
		DataView::IntSet(set) => set.lower_bound().into(),
		DataView::FloatSet(set) => set.lower_bound().into(),
		DataView::Set(set) => set.min_value().unwrap(),
		_ => panic!("invalid value"),
	}
}

fn max(x: Value) -> Value {
	match x.deref() {
		DataView::Seq(seq) => seq.iter().max().unwrap().clone(),
		DataView::IntSet(set) => set.upper_bound().into(),
		DataView::FloatSet(set) => set.upper_bound().into(),
		DataView::Set(set) => set.max_value().unwrap(),
		_ => panic!("invalid value"),
	}
}

fn range_int(a: Value, b: Value) -> Value {
	Value::from_iter([a.as_int()..=b.as_int()])
}

fn range_float(a: Value, b: Value) -> Value {
	Value::from_iter([a.as_float()..=b.as_float()])
}

fn card(x: Value) -> Value {
	match x.deref() {
		DataView::IntSet(set) => set.card().into(),
		DataView::FloatSet(set) => set.card().into(),
		DataView::Set(set) => set.card().into(),
		_ => panic!("invalid value"),
	}
}

fn compiler_version() -> Value {
	let version = env!("CARGO_PKG_VERSION");
	version.into()
}

fn string_len(x: Value) -> Value {
	(x.as_str().len() as i64).into()
}

fn string_concat(x: Value, y: Value) -> Value {
	(&format!("{}{}", x.as_str(), y.as_str())[..]).into()
}

fn array_len(x: Value) -> Value {
	(x.as_seq().len() as i64).into()
}

fn index_sets(x: Value) -> Value {
	let seq = x.as_seq();
	Value::from_iter((1..=seq.dims()).map(|i| {
		let (a, b) = seq.dim(i);
		Value::from_iter([IntVal::from(a)..=IntVal::from(b)])
	}))
}

fn array_sort(x: Value) -> Value {
	let seq = x.as_seq();
	Value::from_iter(seq.iter().cloned().sorted()).with_dim(seq.dims_iter())
}

fn array_sort_by(array: Value, sort_by: Value) -> Result<Value, ValueError> {
	let array_seq = array.as_seq();
	let sort_by_seq = sort_by.as_seq();
	if array_seq.dims() != sort_by_seq.dims() {
		return Err(ValueError {
			reason: "array and sort_by must have the same dimensions",
		});
	}
	if array_seq.len() != sort_by_seq.len() {
		return Err(ValueError {
			reason: "array and sort_by must have the same length",
		});
	}
	if array_seq
		.dims_iter()
		.zip(sort_by_seq.dims_iter())
		.any(|(a, b)| a != b)
	{
		return Err(ValueError {
			reason: "array and sort_by must have the same index sets",
		});
	}
	let mut pairs = array_seq
		.indices()
		.map(|i| (array_seq[&i].clone(), sort_by_seq[&i].clone()))
		.collect::<Vec<_>>();
	pairs.sort_by(|a, b| a.1.cmp(&b.1));
	let sorted_array = pairs.into_iter().map(|(a, _)| a).collect::<Vec<_>>();
	Ok(Value::from_iter(sorted_array).with_dim(array_seq.dims_iter()))
}

fn array_to_int_set(x: Value) -> Value {
	Value::from_iter(x.as_seq().iter().cloned().map(|v| {
		let i = v.as_int();
		i..=i
	}))
}

fn array_to_float_set(x: Value) -> Value {
	Value::from_iter(x.as_seq().iter().cloned().map(|v| {
		let f = v.as_float();
		f..=f
	}))
}

fn array_to_discrete_set(x: Value) -> Value {
	Value::new_discrete_set(x.as_seq().iter().cloned())
}

fn array_xd(source: Value, array: Value) -> Value {
	array.with_dim(source.as_seq().dims_iter())
}

fn array_kd(index_sets: Value, array: Value) -> Result<Value, InvalidIndexSet> {
	let index_sets_seq = index_sets.as_seq();
	let mut count = 1usize;
	let mut dims = Vec::with_capacity(index_sets_seq.len());
	for v in index_sets_seq.iter() {
		let s = v.as_int_set();
		if !s.is_finite() {
			return Err(InvalidIndexSet {
				reason: "Infinite index set not allowed",
			});
		}
		if !s.is_contiguous() {
			return Err(InvalidIndexSet {
				reason: "Non-contiguous index set not allowed",
			});
		}
		let min: i64 = s.lower_bound().try_into().unwrap();
		let max: i64 = s.upper_bound().try_into().unwrap();
		dims.push((min, max));
		count *= (max - min + 1) as usize;
	}

	if count != array.as_seq().len() {
		return Err(InvalidIndexSet {
			reason: "Array length does not match index set size",
		});
	}

	Ok(array.with_dim(dims))
}

fn set_to_array(x: Value) -> Result<Value, ValueError> {
	match x.deref() {
		DataView::IntSet(set) if set.is_finite() => {
			Ok(Value::from_iter(set.values().map(Value::from)))
		}
		DataView::FloatSet(set) if set.is_finite() => {
			Ok(Value::from_iter(set.values().map(Value::from)))
		}
		DataView::IntSet(_) | DataView::FloatSet(_) => Err(ValueError {
			reason: "cannot convert infinite set to array",
		}),
		DataView::Set(set) => Ok(Value::from_iter(set.values().cloned())),
		_ => panic!("invalid value"),
	}
}

fn set_to_ranges_int(x: Value) -> Value {
	let set = x.as_int_set();
	Value::from_iter(
		set.intervals()
			.flat_map(|r| [Value::from(*r.start()), Value::from(*r.end())]),
	)
}

fn set_to_ranges_float(x: Value) -> Result<Value, FloatSetCoercionError> {
	if let DataView::IntSet(set) = x.deref()
		&& !set.is_finite()
	{
		return Err(FloatSetCoercionError {
			reason: INFINITE_INT_SET_COERCION,
		});
	}
	let set = x.as_float_set();
	Ok(Value::from_iter(set.intervals().flat_map(|r| {
		[Value::from(*r.start()), Value::from(*r.end())]
	})))
}

fn lower_bound(x: Value) -> Value {
	match x.deref() {
		DataView::BoolVar(v) => match v.domain() {
			Some(true) => return true.into(),
			_ => return false.into(),
		},
		DataView::IntVar(v) => {
			return v
				.domain()
				.map(|d| d.as_int_set().lower_bound())
				.unwrap_or_else(|| IntVal::InfNeg)
				.into();
		}
		DataView::FloatVar(v) => {
			return v
				.domain()
				.map(|d| d.as_float_set().lower_bound())
				.unwrap_or_else(|| FloatVal::NEG_INFINITY)
				.into();
		}
		DataView::IntSetVar(_) => return INT_SET_EMPTY.clone(),
		_ => (),
	}
	x
}

fn lower_bound_array(x: Value) -> Value {
	let seq = x.as_seq();
	seq.iter().cloned().map(lower_bound).min().unwrap()
}

fn upper_bound(x: Value) -> Value {
	match x.deref() {
		DataView::BoolVar(v) => match v.domain() {
			Some(true) => return true.into(),
			_ => return false.into(),
		},
		DataView::IntVar(v) => {
			return v
				.domain()
				.map(|d| d.as_int_set().upper_bound())
				.unwrap_or_else(|| IntVal::InfPos)
				.into();
		}
		DataView::FloatVar(v) => {
			return v
				.domain()
				.map(|d| d.as_float_set().upper_bound())
				.unwrap_or_else(|| FloatVal::INFINITY)
				.into();
		}
		DataView::IntSetVar(v) => {
			return v
				.domain()
				.map(|d| Value::from_iter(d.as_int_set().intervals()))
				.unwrap_or_else(|| INT_SET_INF.clone());
		}
		_ => (),
	}
	x
}

fn upper_bound_array(x: Value) -> Value {
	let seq = x.as_seq();
	seq.iter().cloned().map(upper_bound).max().unwrap()
}

fn domain(x: Value) -> Value {
	match x.deref() {
		DataView::BoolVar(v) => match v.domain() {
			Some(b) => {
				let value = IntVal::from(b as i64);
				return Value::from_iter([value..=value]);
			}
			_ => return Value::from_iter([IntVal::from(0)..=IntVal::from(1)]),
		},
		DataView::IntVar(v) => {
			return v.domain().unwrap_or_else(|| INT_SET_INF.clone());
		}
		DataView::FloatVar(v) => {
			return v.domain().unwrap_or_else(|| INT_SET_INF.clone());
		}
		DataView::IntSetVar(v) => {
			return v.domain().unwrap_or_else(|| INT_SET_INF.clone());
		}
		_ => (),
	}
	x
}

fn is_fixed(x: Value) -> bool {
	match x.deref() {
		DataView::BoolVar(v) => v.is_fixed(),
		DataView::IntVar(v) => v.is_fixed(),
		DataView::FloatVar(v) => v.is_fixed(),
		DataView::IntSetVar(v) => v.is_fixed(),
		DataView::Seq(s) => s.iter().all(|v| is_fixed(v.clone())),
		_ => true,
	}
}

fn fix(x: Value) -> Result<Value, ValueError> {
	match x.deref() {
		DataView::BoolVar(v) => return v.fix(),
		DataView::IntVar(v) => return v.fix(),
		DataView::FloatVar(v) => return v.fix(),
		DataView::IntSetVar(v) => return v.fix(),
		DataView::Seq(s) => {
			return Ok(Value::from_iter(
				s.iter()
					.cloned()
					.map(fix)
					.collect::<Result<Vec<_>, _>>()?
					.into_iter(),
			));
		}
		_ => (),
	}
	Ok(x)
}

fn array_slice(array: Value, slices: Value) -> Value {
	let array_seq = array.as_seq();
	let select_idxs = slices
		.as_seq()
		.iter()
		.map(|s| {
			let r = s.as_int_set();
			let a: i64 = r.lower_bound().try_into().unwrap();
			let b: i64 = r.upper_bound().try_into().unwrap();
			(a, b)
		})
		.collect::<Vec<_>>();
	array.slice(select_idxs, [(1, array_seq.len() as i64)])
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn uniform_int_includes_both_bounds() {
		let sample: i64 = uniform_int(3.into(), 3.into()).unwrap().try_into().unwrap();
		assert_eq!(sample, 3);
	}

	#[test]
	fn binomial_handles_degenerate_probability() {
		let sample: i64 = binomial(10.into(), 0.0.into()).unwrap().try_into().unwrap();
		assert_eq!(sample, 0);
	}

	#[test]
	fn discrete_distribution_returns_the_array_index() {
		let weights = Value::from_iter([Value::from(0.0), Value::from(1.0)]).with_dim([(4, 5)]);
		let sample: i64 = discrete_distribution(weights).unwrap().try_into().unwrap();
		assert_eq!(sample, 5);
	}
}
