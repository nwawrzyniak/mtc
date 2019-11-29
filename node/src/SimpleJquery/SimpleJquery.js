//module SimpleJquery.SimpleJquery
'use strict';

exports.trigger = function(evt) {
  return function(ob) {
    return function() {
      ob.trigger(evt);
    }
  }
}

exports.getKeycode = function(evt) {
  return function() {
    return evt.which;
  }
}

exports.isShiftDown = function(evt) {
  return function() {
    return evt.shiftKey;
  }
}
exports.serialize = function(ob) {
  return function() {
    return ob.serialize();
  }
}

exports._ajax = function(settings) {
  return function() {
    return JQuery.ajax(settings)
  }
}
