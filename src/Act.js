exports.traceAny = function (x) {
  return function (k) {
    // node only recurses two levels into an object before printing
    // "[object]" for further objects when using console.log()
    console.log(x);
    return k({});
  };
};

// https://stackoverflow.com/a/7616484
function hashCode() {
  var hash = 0, i, chr;
  if (this.length === 0) return hash;
  for (i = 0; i < this.length; i++) {
    chr   = this.charCodeAt(i);
    hash  = ((hash << 5) - hash) + chr;
    hash |= 0; // Convert to 32bit integer
  }
  return hash;
};

var static_ptr_table = {};

exports.static_ = function(a) {
  var hash = hashCode(a.toString());
  static_ptr_table[hash] = a;
  return hash;
}

exports.derefStatic_ = function(ptr) {
  return static_ptr_table[ptr];
}
