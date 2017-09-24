exports.trace = function (x) {
  return function (a) {
    // node only recurses two levels into an object before printing
    // "[object]" for further objects when using console.log()
    console.log(x);
    return a;
  };
};

exports.traceAny = function (x) {
  return function (k) {
    // node only recurses two levels into an object before printing
    // "[object]" for further objects when using console.log()
    console.log(x);
    return k({});
  };
};

exports.stringify = function (x) {
  return JSON.stringify(x, undefined, 2);
};
