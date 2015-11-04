function plus(x, y) {
 return x + y;
}

function addOne(x) {
  return plus(x, 1);
}

function appendOne(x) {
  return plus(x, "1");
}

//var a = plus(1, 2);
//var b = plus("1", 2);


var r_addOne = addOne(1);
var r_appendOne = appendOne("1");

/**
 * NOTE:
 * Intersection can not be considered
 * because the analyzer merges the results
 * But: per matching contract one merge and one extra block
 */
