"use strict";

exports.decodeImpl = function (Left, Right, utfLabel, buffer) {
  var result;
  var decoder = new TextDecoder(utfLabel);

  try {
    result = Right(decoder.decode(buffer));
  }
  catch (error) {
    result = Left(error);
  }

  return result;
};
