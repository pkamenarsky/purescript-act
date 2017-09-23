exports.traceAny = function (x) {
  return function (k) {
    // node only recurses two levels into an object before printing
    // "[object]" for further objects when using console.log()
    console.log(x);
    return k({});
  };
};

// https://stackoverflow.com/a/7616484
function hashCode(str) {
  var hash = 0, i, chr;
  if (str.length === 0) return hash;
  for (i = 0; i < str.length; i++) {
    chr   = str.charCodeAt(i);
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

exports.dragStart = function(f) {
  var mousemoveListener = function(e) {
    e.preventDefault ();
    f(1)(e)();
  }

  var mouseupListener = function(e) {
    document.removeEventListener ('mouseup', mouseupListener, true);
    document.removeEventListener ('mousemove', mousemoveListener, true);
    e.preventDefault ();
    f(2)(e)();
  }

  return function() {
    document.addEventListener ('mouseup', mouseupListener, true);
    document.addEventListener ('mousemove', mousemoveListener, true);
  }
}

exports.persistEvent = function(e) {
  return function() {
    e.persist();
  }
}

exports.elementDataForXY = function(field) {
  return function(x) {
    return function(y) {
      return function() {
        const el = document.elementFromPoint(x, y); 
        if (el) {
          return el.dataset[field];
        }
        else
          return "";
      }
    }
  }
}
