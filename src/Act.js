exports.traceAny = function (x) {
  return function (k) {
    // node only recurses two levels into an object before printing
    // "[object]" for further objects when using console.log()
    console.log(x);
    return k({});
  };
};
