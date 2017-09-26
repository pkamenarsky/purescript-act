exports.jsFunFromString = function(str) {
  return new Function("return " + str)();
}

exports.applyJSFun = function(f) {
  return function(arr) {
    for (i = 0; i < arr.length; i++) {
      f = f(arr[i]);
    }
    return f;
  }
}
