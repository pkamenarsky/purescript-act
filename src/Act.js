exports.traceAny = function (x) {
  return function (k) {
    // node only recurses two levels into an object before printing
    // "[object]" for further objects when using console.log()
    console.log(x);
    return k({});
  };
};

let static_ptr = 0;
const static_ptr_table = {};

exports.static_ = function(a) {
  static_ptr_table[static_ptr] = a;
  return static_ptr++;
}

exports.derefStatic_ = function(ptr) {
  return static_ptr_table[ptr];
}
