<?php
namespace kingfisher\cinip;

/*
  The intention is that any exception class defined in this
  namespace will implement the `Exception' interface.
  (Currently we have only one exception class.)

  This idea came from
  a document entitled "Proposal for Exceptions in ZF2",
  available here:

    http://framework.zend.com/wiki/display/ZFDEV2/Proposal+for+Exceptions+in+ZF2
*/
interface Exception {}
class CinipException extends \Exception implements Exception {}

class parser {
  const subroutine = 1;
  const constant = 2;
  const operator = 3;
  const variable = 4;
  const number = 5;
  const scientific_number = 6;
  const non_scientific_number = 7;
  const string = 8;

  /*
  const subroutine = 'subroutine';
  const constant = 'constant';
  const operator = 'operator';
  const variable = 'variable';
  const number = 'number';
  const scientific_number = 'scientific_number';
  const non_scientific_number = 'non_scientific_number';
  const string = 'string';
  */

  const no_enhancement = 0;
  const memoize = 1;

  function __construct($scale = 60, $perf_mode = self::memoize) {
    $this->scale = $scale;
    $this->perf_mode = $perf_mode;
    $this->saved_code = array();
  }

  static function get_func() {
    $args = array_map('intval', func_get_args());
    return function ($code) use ($args) {
      $tmp = new \ReflectionClass(__CLASS__);
      $parser = $tmp->newInstanceArgs($args);
      return $parser->to_bc($code);
    };
  }

  function to_bc($code) {
    switch($this->perf_mode) {
      case self::no_enhancement:
	return 'return ' .
	  $this->get_translator($code)->get_translation() . ';';
      break;

      case self::memoize:
	if(!array_key_exists($code, $this->saved_code)) {
	  $this->saved_code[$code] = 'return ' .
	    $this->get_translator($code)->get_translation() . ';';
	}

	return $this->saved_code[$code];
      break;
    }
  }

  function get_scale() {
    return $this->scale;
  }

  function get_perf_mode() {
    return $this->perf_mode;
  }

  private function get_translator($code) {
    $this->lexer = new lexer($code);
    $top_node = $this->top();
    $translator = new translator($top_node, $this->scale);
    return $translator;
  }

  private function top() {
    $result = $this->expr_or_comparison();

    if($this->lexer->tokens_remain()) {
      throw exception_with_context("parse error: " .
	"parser has finished but unconsumed tokens remain", $this->lexer);
    }

    return $result;
  }

  private function expr_or_comparison() {
    $e = $this->expr();

    if($this->lexer->tokens_remain() && $this->on_comparison_operator()) {
      $token = $this->lexer->cur_token();
      $this->lexer->advance();
      return new node(self::operator, $token, array($e, $this->expr()));
    } else
      return $e;
  }

  private function expr() {
    $result = $this->term();

    while($this->lexer->tokens_remain() && $this->on_term_operator()) {
      $token = $this->lexer->cur_token();
      $this->lexer->advance();
      $result =
	new node(self::operator, $token, array($result, $this->term()));
    }

    return $result;
  }

  private function term() {
    $result = $this->factor();

    while($this->lexer->tokens_remain() && $this->on_factor_operator()) {
      $token = $this->lexer->cur_token();
      $this->lexer->advance();
      $result =
	new node(self::operator, $token, array($result, $this->factor()));
    }

    return $result;
  }

  private function factor() {
    $type = $this->lexer->cur_type();

    if($type == lexer::plus || $type == lexer::minus) {
      $token = $this->lexer->cur_token();
      $this->lexer->advance();
      return
	new node(self::operator, $token, array($this->signless_factor()));
    } else
      return $this->signless_factor();
  }

  private function signless_factor() {
    $type = $this->lexer->cur_type();
    $token = $this->lexer->cur_token();

    switch($type) {
      case lexer::variable:
	$this->lexer->advance();
	return new node(self::variable, $token, array());
      break;

      case lexer::non_scientific_number:
	$this->lexer->advance();
	return new node(self::non_scientific_number, $token, array());
      break;

      case lexer::scientific_number:
	$this->lexer->advance();
	return new node(self::scientific_number, $token, array());
      break;

      case lexer::single_quoted_string:
      case lexer::double_quoted_string:
	$this->lexer->advance();
	return new node(self::string, $token, array());
      break;

      case lexer::identifier:
	$this->lexer->advance();

	if($this->lexer->tokens_remain() &&
	  $this->lexer->cur_token() == '(') {

	  $this->lexer->eat('(');

	  $args = $this->arglist();
	  $num_args = count($args);

	  switch($token) {
	    case 'pow':
	      if($num_args != 2)
		throw exception_with_context(
		  "`pow' should have 2 arguments, but actually " .
		  "has $num_args", $this->lexer);
	    break;

	    case 'sqrt':
	      if($num_args != 1)
		throw exception_with_context(
		  "`sqrt' should have 1 argument, but actually " .
		  "has $num_args", $this->lexer);
	    break;
	  }

	  $node = new node(self::subroutine, $token, $args);
	  $this->lexer->eat(')');
	} else {
	  $node = new node(self::constant, $token, array());
	}
	return $node;
      break;

      case lexer::left_bracket:
	$this->lexer->advance();
	$node = $this->expr_or_comparison();
	$this->lexer->eat(')');
	return $node;
      break;

      default:
	throw exception_with_context('failed to read a signless factor',
	  $this->lexer);
    }
  }

  private function arglist() {
    $args = array();

    if($this->lexer->cur_type() != lexer::right_bracket) {
      array_push($args, $this->expr_or_comparison());

      while($this->lexer->cur_type() != lexer::right_bracket) {
	$this->lexer->eat(',');
	array_push($args, $this->expr_or_comparison());
      }
    }

    return $args;
  }

  private function on_term_operator() {
    return in_array($this->lexer->cur_type(),
      array(lexer::plus, lexer::minus), TRUE);
  }

  private function on_factor_operator() {
    return in_array($this->lexer->cur_type(),
      array(lexer::slash, lexer::star, lexer::percent), TRUE);
  }

  private function on_comparison_operator() {
    return in_array($this->lexer->cur_type(),
      array(lexer::less_than_or_equal, lexer::less_than,
	lexer::greater_than_or_equal, lexer::greater_than,
	  lexer::equal_to, lexer::not_equal_to), TRUE);
  }
}

class translator {
  function __construct(node $node, $scale) {
    $this->scale = $scale;
    $this->translation = $this->generate_translation($node);
  }

  function get_translation() {
    return $this->translation;
  }

  private function generate_translation($node) {
    $retain_precision =
      '\\' . ltrim(__CLASS__, '\\') . '::retain_precision';
    #
    # the call to ltrim is probably unnecessary, but won't hurt.

    $arity = count($node->args);

    switch($node->head_type) {
      case parser::non_scientific_number:
	return "'" . $node->head . "'";
      break;

      case parser::scientific_number:
	return "'" . expand_scientific_notation($node->head) . "'";
      break;

      case parser::string:
	return $node->head;
      break;

      case parser::variable:
      case parser::constant:
	return "$retain_precision($node->head)";
      break;

      case parser::subroutine:
	$name = $node->head;
	$args_translated =
	  array_map(array($this, 'generate_translation'), $node->args);

	switch($name) {
	  case 'pow':
	    return
	      ('\\' . ltrim(__CLASS__, '\\') . '::use_appropriate_pow') .
	      "($args_translated[0], $args_translated[1], $this->scale)";
	  break;

	  case 'sqrt':
	    return "bcsqrt($args_translated[0], $this->scale)";
	  break;

	  default:
	    $args = implode(', ', $args_translated);
	    return "$retain_precision($name($args))";
	}
      break;

      case parser::operator:
	$operator = $node->head;

	if($arity == 2) {
	  $arith_func = array(
	    '+' => 'bcadd',
	    '-' => 'bcsub',
	    '*' => 'bcmul',
	    '/' => 'bcdiv',

	    # "%" intentionally omitted
	  );

	  $comparison_operators = array(
	    '<', '<=', '>=', '>', '==', '!=',
	  );

	  $arg_1_translated = $this->generate_translation($node->args[0]);
	  $arg_2_translated = $this->generate_translation($node->args[1]);

	  if(array_key_exists($operator, $arith_func)) {
	    return
	      "{$arith_func[$operator]}($arg_1_translated, " .
		"$arg_2_translated, $this->scale)";
	  } elseif($operator == '%') {
	    return "bcmod($arg_1_translated, $arg_2_translated)";
	  } elseif(in_array($operator, $comparison_operators)) {
	    return "(bccomp($arg_1_translated, " .
	      "$arg_2_translated, $this->scale) $operator 0)";
	  } else {
	    throw new
	      CinipException("unknown operator of arity 2: $operator");
	  }
	} elseif($arity == 1 && $operator == '+') {
	    return $this->generate_translation($node->args[0]);
	} elseif($arity == 1 && $operator == '-') {
	  return
	    '("-" . ' .  $this->generate_translation($node->args[0]) . ')';
	} else {
	  throw new
	    CinipException("unknown operator of arity $arity: $operator");
	}
      break;
    }
  }

  static function use_appropriate_pow($base, $exponent, $scale) {
    if(preg_match('@\..*[^0]@', $exponent)) {
      return self::retain_precision(pow($base, $exponent));
    } else
      return bcpow($base, $exponent, $scale);
  }

  static function retain_precision($number) {
    if(is_float($number)) {
      if(!is_finite($number)) {
	if(is_nan($number)) {
	  throw new CinipException("NaN passed to " . __METHOD__);
	} elseif(is_infinite($number)) {
	  throw new CinipException("invalid value passed to " .
	    __METHOD__ . ": $number");
	} else {
	  # this should never happen

	  throw new CinipException("floating-point number of unknown " .
	    "type passed to " . __METHOD__ . ": $number");
	}
      }

      if($number < 0) {
	$sign_prefix = '-';
	$number = -$number;
      } else {
	$sign_prefix = '';
      }

      $integer_part_shifted = floor($number);
      $fractional_part_shifted = $number - $integer_part_shifted;

      if($integer_part_shifted != 0) {
	for($exp1 = 0; float_represents_integer($integer_part_shifted);
	  $exp1++, $integer_part_shifted /= 2) {}

	$exp1--;
	$integer_part_shifted *= 2;
      } else {
	$exp1 = 0;
      }

      for($exp2 = 0; !float_represents_integer($fractional_part_shifted);
	$exp2++, $fractional_part_shifted *= 2) {}

      /*
	using `log' should greatly speed up the calculation of
	$integer_part_shifted and $fractional_part_shifted.
      */

      $saved_precision = ini_get('precision');
      ini_set('precision', 16);
      /*<
	enough to give an exact representation of 2**53-1, which
	is the highest value that $integer_part_shifted can have.
      */
      $int_shifted_str = strval($integer_part_shifted);
      $frac_shifted_str = strval($fractional_part_shifted);
      ini_set('precision', $saved_precision);
      /*<
	strval sometimes returns a value in scientific notation.  we are
	assuming that this will not happen if PHP's `precision' setting
	is high enough to allow the number to be represented without loss
	of precision, but i don't see any statement to this effect in
	the PHP documentation.	however, a little program which supports
	this belief is the following:

	  $x = floatval(123456789);

	  foreach (array(8, 9) as $precision) {
	    ini_set("precision", $precision);
	    var_dump(strval($x));
	  }
      */

      assert(preg_match('@^\d+\z@', $int_shifted_str));
      assert(preg_match('@^\d+\z@', $frac_shifted_str));

      assert(floatval($int_shifted_str) === $integer_part_shifted);
      assert(floatval($frac_shifted_str) === $fractional_part_shifted);

      return
	$sign_prefix .
	bcmul(
	  bcadd(
	    bcmul($int_shifted_str, bcpow(2, $exp1 + $exp2)),
	    $frac_shifted_str
	  ),
	  bcpow(2, -$exp2, $exp2),
	  $exp2
	);
    } else
      return strval($number);
  }

  static function test_retain_precision() {
    $saved_precision = ini_get('precision');
    ini_set('precision', 50);
    $scale = 2000;

    if(1) {
      for($i = -1074; $i <= 200; $i++) {
	$x = pow(2, $i);
	$s1 = self::retain_precision($x);
	$s2 = bcpow(2, $i, $scale);
	$s2 = remove_needless_trailing_zeroes($s2);
	if($s1 !== $s2) {
	  print "Problem at $i\n$s1\n$s2\n";
	}
      }
    }

    srand(1110131);
    for($i = 1; $i <= 1000; $i++) {
      $n = '1';
      for($j = 1; $j <= 4; $j++) {
	$n .= sprintf('%013b', rand(0, 0x1fff));
      }
      $n = bindec($n);
      $exp = 200 - rand(0, 400);
      $sign = rand(0, 1)? -1: 1;
      $x = $sign * $n * pow(2, $exp);

      $s1 = self::retain_precision($x);
      $s2 = bcmul($sign, bcmul($n, bcpow(2, $exp, $scale), $scale), $scale);
      $s2 = remove_needless_trailing_zeroes($s2);

      if($s1 != $s2) {
	print "Problem: $n $exp $x $s1 $s2\n";
      }
    }

    ini_set('precision', $saved_precision);
  }
}

class node {
  function __construct($head_type, $head, array $args) {
    $this->head_type = $head_type;
    $this->head = $head;
    $this->args = $args;
  }

  # used in development, and for debugging
  #
  function pr() {
    $this->pr_aux(0);
  }

  private function pr_aux($indent) {
    print str_repeat(' ', $indent) . "$this->head ($this->head_type)\n";
    foreach ($this->args as $arg) {
      if(!is_object($arg)) {
	var_dump($arg);
      }
      $arg->pr_aux($indent + 2);
    }
  }
}

class lexer {
  const whitespace = 1;
  const scientific_number = 2;
  const non_scientific_number = 3;
  const double_quoted_string = 4;
  const single_quoted_string = 5;
  const identifier = 6;
  const variable = 7;
  const plus = 8;
  const minus = 9;
  const slash = 10;
  const star = 11;
  const percent = 12;
  const left_bracket = 13;
  const right_bracket = 14;
  const comma = 15;
  const less_than_or_equal = 16;
  const less_than = 17;
  const greater_than_or_equal = 18;
  const greater_than = 19;
  const equal_to = 20;
  const not_equal_to = 21;

  /*
  const whitespace = 'whitespace';
  const scientific_number = 'scientific_number';
  const non_scientific_number = 'non_scientific_number';
  const double_quoted_string = 'double_quoted_string';
  const single_quoted_string = 'single_quoted_string';
  const identifier = 'identifier';
  const variable = 'variable';
  const plus = 'plus';
  const minus = 'minus';
  const slash = 'slash';
  const star = 'star';
  const percent = 'percent';
  const left_bracket = 'left_bracket';
  const right_bracket = 'right_bracket';
  const comma = 'comma';
  const less_than_or_equal = 'less_than_or_equal';
  const less_than = 'less_than';
  const greater_than_or_equal = 'greater_than_or_equal';
  const greater_than = 'greater_than';
  */

  function __construct($input) {
    $regex = '@
      (?:
	(?<whitespace>\s+) |
	(?<scientific_number>\d(?:\.\d+)?[Ee][-+]?\d+) |

	#> the order of the alternatives is important.
	#
	# it is also important that this come after the
	# expression for scientific numbers.
	#
	(?<non_scientific_number>\d*\.\d+|\d+\.?) |

	(?<double_quoted_string>"(?:[^\\\\"]|\\\\.)*") |
	(?<single_quoted_string>\'(?:[^\\\\\']|\\\\.)*\') |
	(?<identifier>[A-Za-z_][A-Za-z_0-9]*) |
	(?<variable>\$[A-Za-z_][A-Za-z_0-9]*) |

	# order is important here
	#
	(?<less_than_or_equal><=) |
	(?<less_than><) |
	(?<greater_than_or_equal>>=) |
	(?<greater_than>>) |

	(?<not_equal_to>!=) |
	(?<equal_to>==) |
	(?<minus>-) |
	(?<plus>\+) |
	(?<star>\*) |
	(?<slash>/) |
	(?<percent>%) |
	(?<left_bracket>\() |
	(?<right_bracket>\)) |
	(?<comma>,)
      )
    @xs';
    /*
      the `s' modifier is needed to match strings containing
      a backslash followed by a newline.
    */

    preg_match_all($regex, $input, $info_about_matches,
      PREG_SET_ORDER | PREG_OFFSET_CAPTURE);

    $context_len = 100;   # this is really a sort of local constant
    $expected_pos = 0;
    unset($err_msg);

    foreach ($info_about_matches as $match_info) {
      assert($match_info[0][1] >= $expected_pos);

      if($match_info[0][1] != $expected_pos) {
	$err_msg = "A non-token begins at offset $expected_pos.\n";

	if($expected_pos > 0) {
	  if($expected_pos >= $context_len) {
	    $start = $expected_pos - $context_len;
	    $len = $context_len;
	  } else {
	    $start = 0;
	    $len = $expected_pos;
	  }

	  $err_msg .= "Context before offset $expected_pos:\n<" .
	    substr($input, $start, $len) . ">\n";
	}

	$err_msg .= "Text starting at offset $expected_pos:\n<" .
	  substr($input, $expected_pos, $context_len) . ">\n";

	break;
      } else {
	$expected_pos += strlen($match_info[0][0]);
      }
    }

    if(!isset($err_msg) && $expected_pos < strlen($input)) {
      if($expected_pos == 0) {
	$err_msg = "No tokens found in input";
      } else {
	if($expected_pos >= $context_len) {
	  $start = $expected_pos - $context_len;
	  $len = $context_len;
	} else {
	  $start = 0;
	  $len = $expected_pos;
	}

	$err_msg =
	  "No tokens found after offset $expected_pos.\n" .
	  "Context before offset $expected_pos:\n<" .
	    substr($input, $start, $len) . ">\n" .
	  "Text starting at offset $expected_pos:\n<" .
	    substr($input, $expected_pos, $context_len) . ">\n";
      }
    }

    if(isset($err_msg)) {
      throw new CinipException($err_msg);
    }

    $this->token_info = array();

    foreach ($info_about_matches as $match_info) {
      $num_found = 0;
      #< this variable exists purely for the assertion below

      foreach (array_keys($match_info) as $key) {
	if(is_string($key) && is_string($match_info[$key][0]) &&
	  $match_info[$key][0] != "") {

	  $num_found++;

	  $o = new \stdClass;
	  $o->type = constant("self::$key");
	  $o->token = $match_info[$key][0];
	  array_push($this->token_info, $o);
	}
      }

      assert($num_found == 1);
    }

    $this->position = 0;
    $this->skip_whitespace();
  }

  function cur_type() {
    if(!$this->tokens_remain()) {
      throw new CinipException('no more tokens available');
    }
    return $this->token_info[$this->position]->type;
  }

  function cur_token() {
    if(!$this->tokens_remain()) {
      throw new CinipException('no more tokens available');
    }
    return $this->token_info[$this->position]->token;
  }

  function advance() {
    if($this->position < count($this->token_info)) {
      $this->position++;
      $this->skip_whitespace();
    }
  }

  function eat($token) {
    if(!$this->tokens_remain()) {
      throw new CinipException('no more tokens available');
    } elseif(strval($this->cur_token()) === strval($token)) {
      $this->advance();
    } else {
      $cur_token = $this->cur_token();
      throw exception_with_context(
	"cannot eat <$token>, current token is <$cur_token>", $this);
    }
  }

  function tokens_remain() {
    return $this->position < count($this->token_info);
  }

  function pr() {
    foreach ($this->token_info as $info)
      printf("%23s  %s\n", $info->type, $info->token);
  }

  function get_recent_context() {
    $pos = min($this->position, count($this->token_info) - 1);

    $context = '';
    $num_non_whitespace_tokens = 0;
    $num_chars = 0;

    for(; $pos >= 0 && ($num_non_whitespace_tokens < 16 ||
      $num_chars < 100); $pos--) {

      $info = $this->token_info[$pos];

      if($info->type != self::whitespace)
	$num_non_whitespace_tokens++;

      $num_chars += strlen($info->token);

      $context = $info->token . $context;
    }

    return $context;
  }

  static function test_get_recent_context() {
    $c = __CLASS__;

    $input = '';
    for($i = 100; $i <= 999; $i++)
      $input .= "\$v$i + +";

    $lexer = new $c($input);

    for($i = 0; $i < 30; $i++)
      $lexer->advance();

    var_dump($lexer->get_recent_context());
  }

  private function skip_whitespace() {
    /*
      possibly, a loop is overly defensive, because we shouldn't have
      adjacent whitespace tokens.
    */
    for(
      $pos = &$this->position;
      $pos < count($this->token_info) &&
	$this->token_info[$pos]->type === self::whitespace;
      $pos++) {
    }
  }
}

function exception_with_context($message, lexer $lexer) {
  $extra = $lexer->tokens_remain()? ' (including current token)': '';
  $new_message =
    $message . ":\nrecent context$extra:\n" .
    $lexer->get_recent_context() . "\n";
  return new CinipException($new_message);
}
  
const SCIENTIFIC_NOTATION_CAPTURING_REGEX =
  '@^([-+]?)(\d)(?:\.(\d+))?[Ee]([-+]?)(\d+)@';

function expand_scientific_notation($x) {
  assert(preg_match(SCIENTIFIC_NOTATION_CAPTURING_REGEX,
    $x, $matches));

  list(, $leading_sign, $digit1, $remaining_digits,
    $exponent_sign, $exponent) = $matches;

  if($leading_sign === '+')
    $leading_sign = '';

  if($remaining_digits == '')
    $remaining_digits = '0';

  if($exponent_sign == '')
    $exponent_sign = '+';

  if($exponent == 0) {
    return "$leading_sign$digit1.$remaining_digits";
  } elseif($exponent_sign == '+') {
    $len = strlen($remaining_digits);

    if($len <= $exponent) {
      return "$leading_sign$digit1$remaining_digits" .
	str_repeat('0', $exponent - $len);
    } else {
      /*
	this might produce a number ending in a period, but
	bcmath can cope with that.
      */
      return $leading_sign . $digit1 .
	substr($remaining_digits, 0, $exponent) .
	'.' .  substr($remaining_digits, $exponent);
    }
  } else {
    return $leading_sign . '.' . str_repeat('0', $exponent - 1) .
      "$digit1$remaining_digits";
  }
}

function test_expand_scientific_notation() {
  $correct_answer = array(
    '1e9' => '10' . '0000' . '0000',
    '-1.23456789e4' => '-12345.6789',
    '4.44e-8' => '.000' . '0000' . '444',
    '5.9e0' => '5.9',
    '-1.23456789e+10' => '-12345678900',
    '-1.234e-9' => '-.000000001234',
    '+1.234e-9' => '.000000001234',
    '-9.8e-1' => '-.98',
  );

  $max_len = max(array_map('strlen', array_keys($correct_answer)));

  foreach ($correct_answer as $input => $correct) {
    $result = expand_scientific_notation($input);
    if($result !== $correct)
      printf("%{$max_len}s  %s\n", $input, $result);
  }
}

function float_represents_integer($x) {
  if($x < 0)
    $x = -$x;
  return $x == floor($x);
}

function remove_needless_trailing_zeroes($s) {
  return preg_replace('@(\.\d*[1-9])0+\z@', '$1', $s);
}
