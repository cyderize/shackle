function bool: enum2int(bool: x) :: mzn_internal_representation;
function var bool: enum2int(var bool: x) :: mzn_internal_representation;

function opt bool: enum2int(opt bool: x) :: mzn_internal_representation;
function var opt bool: enum2int(var opt bool: x) :: mzn_internal_representation;

function opt set of bool: enum2int(opt set of bool: x) :: mzn_internal_representation;
function set of bool: enum2int(set of bool: x) :: mzn_internal_representation;
function var set of bool: enum2int(var set of bool: x) :: mzn_internal_representation;

function array [$X] of bool: enum2int(array [$X] of bool: x) :: mzn_internal_representation;
function array [$X] of var bool: enum2int(array [$X] of var bool: x) :: mzn_internal_representation;

function array [$X] of opt bool: enum2int(array [$X] of opt bool: x) :: mzn_internal_representation;
function array [$X] of var opt bool: enum2int(array [$X] of var opt bool: x)
  :: mzn_internal_representation;

function array [$X] of opt set of bool: enum2int(array [$X] of opt set of bool: x)
  :: mzn_internal_representation;
function array [$X] of set of bool: enum2int(array [$X] of set of bool: x)
  :: mzn_internal_representation;
function array [$X] of var set of bool: enum2int(array [$X] of var set of bool: x)
  :: mzn_internal_representation;
