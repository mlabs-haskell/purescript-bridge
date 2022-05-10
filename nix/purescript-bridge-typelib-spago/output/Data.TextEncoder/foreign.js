"use strict";

exports.encodeImpl = function (utfLabel, str) {
  var encoder = new TextEncoder(utfLabel);

  return encoder.encode(str);
};
