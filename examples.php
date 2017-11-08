<?php
require_once "cinip.php";

use kingfisher\cinip;

birthday_problem();

num_terms_three_ways();

/*
  solves the Birthday Problem.  (The Birthday Problem is explained at
  http://mathworld.wolfram.com/BirthdayProblem.html.)
*/
function birthday_problem() {
  $year_days = 365;

  $f = cinip\parser::get_func(30);

  $numerator = 1;
  $denominator = 1;

  for($num_people = 1; $num_people <= 365; $num_people++) {
    $numerator =
      eval($f('$numerator * ($year_days - $num_people + 1)'));
    $denominator = eval($f('$denominator * $year_days'));

    if(eval($f('$numerator * 2 < $denominator'))) {
      break;
    }
  }

  $probability = eval($f('1 - $numerator / $denominator'));

  $probability = rtrim($probability, '0');

  printf("Number of people: %s.\nProbability: approximately %s.\n\n",
    $num_people, $probability);
}

/*
  calculates, using 3 different methods, the smallest integer
  m such that

          1     1     1     1             1
    4  <  -  +  -  +  -  +  -  +  ...  +  -
          1     2     3     4             m
*/
function num_terms_three_ways() {
  $n = 4;
  num_terms_native($n);
  num_terms_more_accurate($n);
  num_terms_perfect($n);
}

function num_terms_native($x) {
  $sum = 0;

  for($n = 1; $sum <= $x; $n++) {
    $sum += 1 / $n;
  }

  print "Number of terms needed: " . ($n - 1) . "\n";
}

/*
  more accurate than num_terms_native, but too slow to be useful
*/
function num_terms_more_accurate($x) {
  $f = cinip\parser::get_func(1000);

  $sum = 0;

  for($n = 1; eval($f('$sum <= $x')); $n++) {
    $sum = eval($f('$sum + 1 / $n'));
  }

  print "Number of terms needed: " . ($n - 1) . "\n";
}

/*
  perfectly accurate, but too slow to be useful
*/
function num_terms_perfect($x) {
  $f = cinip\parser::get_func(0);

  $numerator = 0;
  $denominator = 1;

  for($n = 1; eval($f('$numerator <= $x * $denominator')); $n++) {
    /*
      we are simply using the following mathematical identity:

        a     1     a * n + b
        -  +  -  =  ---------
        b     n       b * n
    */
    $numerator = eval($f('$numerator * $n + $denominator'));
    $denominator = eval($f('$denominator * $n'));
  }

  print "Number of terms needed: " . ($n - 1) . "\n";
}
